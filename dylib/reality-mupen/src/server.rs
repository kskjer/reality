use futures_util::future::FutureExt;
use futures_util::future::TryFutureExt;
use hyper::service::{make_service_fn, service_fn};
use hyper::{header, Body, Method, Request, Response};
use reality_mips::isa::{self, Pc};

use std::convert::{Infallible, TryInto};
use std::fmt::Write;

use std::net::SocketAddr;
use std::sync::{Arc, Barrier};
use std::thread::JoinHandle;

use hyper::header::HeaderValue;
use reality_mips::memory::MemoryMapFrom;
use reality_mupen_core::emu::{self, Emu};
use reality_mupen_core::modes::Modes;
use std::collections::BTreeMap;
use std::{mem, slice, thread};
use tokio::sync::mpsc::error::TryRecvError;
use tokio::{runtime, sync::mpsc, sync::oneshot};

type MagicSender = mpsc::Sender<oneshot::Sender<LoanedEmu>>;

async fn http_handler(
    req: Request<Body>,
    mut q: MagicSender,
) -> Result<Response<Body>, Infallible> {
    let mut cx = get_emu_cx(&mut q).await.unwrap();
    let (emu, modes) = cx.borrow_mut();

    let mut res = Response::new(match (req.method(), req.uri().path()) {
        (&Method::GET, "/threads") => {
            let json = serde_json::to_string(&modes.threads).unwrap();

            json.into()
        }
        (&Method::GET, "/threads/pretty") => modes
            .threads
            .set
            .iter()
            .enumerate()
            .fold(String::new(), |mut acc, (i, thread)| {
                writeln!(acc, "Thread {}:", i).unwrap();

                for (level, f) in thread.calls.iter().rev().enumerate() {
                    writeln!(acc, "  #{} {:?}", level, f).unwrap();
                }

                writeln!(acc).unwrap();

                acc
            })
            .into(),
        (&Method::POST, "/diff/push") => serde_json::to_string(&modes.insn_diff.push())
            .unwrap()
            .into(),
        (&Method::POST, "/diff/pop") => serde_json::to_string(&modes.insn_diff.pop())
            .unwrap()
            .into(),
        (&Method::POST, "/diff/reset") => {
            modes.insn_diff = Default::default();

            "".into()
        }
        (&Method::GET, "/diff/delta") => modes
            .insn_diff
            .delta()
            .map(|i| {
                i.map(|pc| format!("{}: {}", pc, isa::disassemble(emu.read_insn(pc)).at(pc)))
                    .take(200)
                    .collect::<Vec<_>>()
                    .join("\n")
            })
            .unwrap_or_default()
            .into(),
        (&Method::POST, "/offset/push") => {
            serde_json::to_string(&modes.offset.push()).unwrap().into()
        }
        (&Method::POST, "/offset/pop") => {
            serde_json::to_string(&modes.offset.pop()).unwrap().into()
        }
        (&Method::POST, "/offset/reset") => {
            modes.offset = Default::default();

            "".into()
        }
        (&Method::GET, "/offset/delta") => serde_json::to_string_pretty(
            &modes
                .offset
                .delta()
                .into_iter()
                .flat_map(|x| x.into_iter())
                .map(|d| d.to_string())
                .collect::<Vec<_>>(),
        )
        .unwrap()
        .into(),
        (&Method::GET, "/offset/delta-all") => serde_json::to_string_pretty(
            &modes
                .offset
                .delta_all()
                .into_iter()
                .flat_map(|x| x.into_iter())
                .map(|d| d.to_string())
                .collect::<Vec<_>>(),
        )
        .unwrap()
        .into(),
        (&Method::GET, "/dma") => serde_json::to_string_pretty(
            &modes
                .dma
                .dma_seen
                .iter()
                .map(|(k, v)| (format!("{:08X}", k), v))
                .collect::<BTreeMap<_, _>>(),
        )
        .expect("JSON")
        .into(),
        (&Method::GET, "/dma/contiguous") => serde_json::to_string_pretty(
            &modes
                .dma
                .dma_seen
                .iter()
                .fold(Vec::new(), |mut acc, &cur| {
                    if let Some((a, s)) = acc.pop() {
                        let mut need_new = false;

                        acc.push(if a + s == cur.0 {
                            (a, s + cur.1)
                        } else {
                            need_new = true;

                            (a, s)
                        });

                        if need_new {
                            acc.push(cur);
                        }
                    } else {
                        acc.push(cur);
                    }

                    acc
                })
                .into_iter()
                .map(|(k, v)| (format!("{:08X}", k), v))
                .collect::<BTreeMap<_, _>>(),
        )
        .expect("JSON")
        .into(),
        (&Method::GET, "/dma/largest") => serde_json::to_string_pretty(
            &modes
                .dma
                .dma_seen
                .iter()
                .map(|(k, v)| (*v, *k))
                .collect::<BTreeMap<_, _>>()
                .iter()
                .rev()
                .map(|(v, k)| format!("{:08X} - {:08X} ({} bytes)", k, k + v, v))
                .collect::<Vec<_>>(),
        )
        .expect("JSON")
        .into(),
        (&Method::GET, x) if x.starts_with("/fn/") => {
            use reality_mips::block;

            let tgt: Pc = u32::from_str_radix(x.split('/').last().expect("bad PC (split)"), 16)
                .expect("bad PC (parse)")
                .try_into()
                .expect("PC new fail");

            // hacky, but mupen's RAM is in LE
            let ram = unsafe { slice::from_raw_parts(emu.ram, emu::MEM_SIZE_WORDS) }
                .iter()
                .fold(Vec::new(), |mut acc, cur| {
                    acc.extend_from_slice(&cur.to_be_bytes());

                    acc
                });

            let map = ram.map_to(Pc::MEM_START);

            let x = format!("{}", block::trace(tgt, &map).expect("Trace").display(&map));

            x.into()
        }
        _ => "Hello, World".into(),
    });

    res.headers_mut().insert(
        header::CONTENT_TYPE,
        HeaderValue::from_static("application/json"),
    );

    Ok(res)
}

pub struct Server {
    #[allow(dead_code)]
    runtime: JoinHandle<()>,
    quit_signal: Option<oneshot::Sender<()>>,
    rx_cx_req: mpsc::Receiver<oneshot::Sender<LoanedEmu>>,
}

// Warning: make sure the returned value doesn't live too long, as its drop method is used to send
// the value to others, if multiple were requested at the same time.
async fn get_emu_cx(queue: &mut MagicSender) -> Result<LoanedEmu, oneshot::error::RecvError> {
    let (tx, rx) = oneshot::channel();

    queue
        .send(tx)
        .await
        .unwrap_or_else(|_| panic!("Error getting emulator context"));

    rx.await
}

async fn server_main(quit_rx: oneshot::Receiver<()>, tx_cx_req: MagicSender) {
    println!("now running on a worker thread");

    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));

    let make_svc = make_service_fn(|_conn| {
        let tx_cx_req = tx_cx_req.clone();

        async move {
            let tx_cx_req = tx_cx_req.clone();

            Ok::<_, Infallible>(service_fn(move |r| http_handler(r, tx_cx_req.clone())))
        }
    });

    // And construct the `Server` like normal...
    let server = hyper::Server::bind(&addr).serve(make_svc);

    // And now add a graceful shutdown signal...
    let graceful = server
        .with_graceful_shutdown(quit_rx.map(|_| ()))
        .map_err(|err| panic!("server error: {:?}", err));

    graceful.await.expect("Server error");

    println!("got quit signal");
}

impl Server {
    pub fn start() -> Self {
        let (tx, rx) = oneshot::channel::<()>();
        let (tx_cx_req, rx_cx_req) = mpsc::channel(16);

        Server {
            runtime: thread::spawn(|| {
                let mut rt = runtime::Builder::new()
                    .basic_scheduler()
                    .enable_all()
                    .build()
                    .expect("Couldn't start tokio runtime");

                rt.block_on(server_main(rx, tx_cx_req));

                println!("Tokio thread exited.");
            }),
            quit_signal: Some(tx),
            rx_cx_req,
        }
    }

    pub fn lend(&mut self, emu: &mut Emu, modes: &mut Modes) {
        let mut pending = Vec::new();

        loop {
            match self.rx_cx_req.try_recv() {
                Ok(x) => pending.push(x),
                Err(TryRecvError::Empty) => break,
                Err(e) => panic!("mpsc failed: {:?}", e),
            }
        }

        if pending.len() == 0 {
            return;
        }

        let signal = Arc::new(Barrier::new(2));

        let forwarded = unsafe {
            LoanedEmu {
                emu: Some(Box::from_raw(emu as *mut Emu)),
                modes: Some(Box::from_raw(modes as *mut Modes)),
                todo: Some(pending),
                signal: Some(Arc::clone(&signal)),
                depth: 0,
            }
        }
        .dispatch();

        if forwarded {
            signal.wait();
        }
    }
}

unsafe impl Send for LoanedEmu {}

struct LoanedEmu {
    emu: Option<Box<Emu>>,
    modes: Option<Box<Modes>>,
    todo: Option<Vec<oneshot::Sender<LoanedEmu>>>,
    signal: Option<Arc<Barrier>>,
    depth: usize,
}

impl LoanedEmu {
    fn next(mut self, delta: isize) -> Self {
        self.depth = (self.depth as isize + delta) as usize;

        self
    }

    pub fn borrow_mut(&mut self) -> (&mut Emu, &mut Modes) {
        (self.emu.as_mut().unwrap(), self.modes.as_mut().unwrap())
    }

    #[inline(never)]
    pub fn dispatch(mut self) -> bool {
        if let Some(x) = self.todo.as_mut().unwrap().pop() {
            self = match x.send(self.next(1)) {
                Ok(()) => return true,
                Err(x) => x.next(-1),
            };

            // If an incoming req is aborted, this send can fail
        }

        if self.emu.is_some() && self.modes.is_some() {
            let (_todo, signal) = (
                mem::replace(&mut self.todo, None),
                mem::replace(&mut self.signal, None),
            );

            if self.depth > 0 {
                signal.unwrap().wait();
            }

            mem::forget(self);
        }

        false
    }
}

impl Drop for LoanedEmu {
    fn drop(&mut self) {
        let original = mem::replace(
            self,
            LoanedEmu {
                emu: None,
                modes: None,
                todo: None,
                signal: None,
                depth: 0,
            },
        );

        original.dispatch();
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        self.quit_signal.take().unwrap().send(()).unwrap();
    }
}
