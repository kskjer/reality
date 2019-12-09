use std::env;

pub struct Config {
    pub show_dma: bool,
}

impl Default for Config {
    fn default() -> Self {
        let base = Config { show_dma: false };

        env::vars().fold(base, |mut base, (k, v)| {
            match (k.as_ref(), v.as_ref()) {
                ("REALITY_DMA", "1") => base.show_dma = true,
                _ => {}
            }

            base
        })
    }
}
