use crate::graph::scc;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum SccNodeType<V> {
    Node,
    Leaf,
    Break(V), // breaks out of the component/loop
    InfiniteLoop,
}

#[derive(PartialEq)]
pub enum SccNode<V> {
    Node(V, SccNodeType<V>),
    Scc(Vec<SccNode<V>>),
}

impl<V: fmt::Debug> fmt::Debug for SccNode<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            SccNode::Node(v, t) => write!(f, "Node({:?}, {:?})", v, t),
            SccNode::Scc(v) => write!(f, "SccNode({:#?})", v),
        }
    }
}

pub fn derive_scc_nodes<V, E, I>(
    vertices: impl Iterator<Item = V>,
    node_to_idx: &impl Fn(V) -> Result<usize, E>,
    successors: &impl Fn(V) -> I,
) -> Result<Vec<SccNode<V>>, E>
where
    V: Copy + PartialEq + std::fmt::Debug,
    I: Iterator<Item = V>,
{
    derive_scc_nodes_from_components(
        &scc(vertices, node_to_idx, successors)?,
        node_to_idx,
        successors,
    )
}

pub fn derive_scc_nodes_from_components<V, E, I>(
    components: &Vec<Vec<V>>,
    node_to_idx: &impl Fn(V) -> Result<usize, E>,
    successors: &impl Fn(V) -> I,
) -> Result<Vec<SccNode<V>>, E>
where
    V: Copy + PartialEq + std::fmt::Debug,
    I: Iterator<Item = V>,
{
    let mut nodes = Vec::new();

    //println!("recurse components {:?}", components);

    for c in components.iter().rev() {
        let first = c.last().unwrap();

        //println!("doing {:?}", c);

        match c {
            // Single block loop
            c if c.len() == 1 && successors(c[0]).any(|v| v == c[0]) => {
                nodes.push(SccNode::Scc(vec![SccNode::Node(
                    c[0],
                    successors(c[0])
                        .filter(|&v| v != c[0])
                        .nth(0)
                        .map(|v| SccNodeType::Break(v))
                        .unwrap_or_else(|| {
                            if successors(c[0]).count() == 1 {
                                SccNodeType::InfiniteLoop
                            } else {
                                dbg!(successors(c[0]).collect::<Vec<_>>());
                                dbg!(c);
                                panic!("Don't know what to do. Should be infinite loop here?");
                            }
                        }),
                )]))
            }
            // Multi-block loop
            c if c.len() > 1 => {
                let scc = scc(
                    c.iter().rev().copied(),
                    node_to_idx,
                    &|v| successors(v).filter(|v| c.contains(v) && v != first), //.inspect(move |x| println!(" -- successor for {:?} is {:?}", v, x))
                )?;

                nodes.push(SccNode::Scc(derive_scc_nodes_from_components(
                    &scc,
                    node_to_idx,
                    successors,
                )?));
            }
            c => {
                let v = c[0];
                let ours = components
                    .iter()
                    .flat_map(|c| c.iter().copied())
                    .collect::<Vec<_>>();
                let breaks = successors(v).filter(|v| !ours.contains(v)).nth(0);

                //println!("@ {:?}, successors: {:?}, self {:?}, ours {:?}", v, successors(v).collect::<Vec<_>>(), components, ours);

                nodes.push(SccNode::Node(
                    v,
                    match (v, breaks) {
                        (_, Some(brk)) => SccNodeType::Break(brk),
                        (v, _) if !successors(v).any(|_| true) => SccNodeType::Leaf,
                        _ => SccNodeType::Node,
                    },
                ));
            }
        }
    }

    Ok(nodes)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scc_nodes() {
        use super::SccNodeType::*;

        let graph = [
            vec![1],
            // -- loop start
            vec![2],
            // -- inner loop start
            vec![3],
            vec![4],
            vec![2, 5],
            // -- inner loop end
            vec![6],
            vec![1, 7],
            // -- loop end
            vec![8],
            vec![8, 9], // single block loop
            vec![],
        ];

        /*panic!("\n{:#?}\n", derive_scc_nodes(
            graph.iter().enumerate().map(|(i, _)| i),
            &|v| -> Result<usize, ()> { Ok(v) },
            &|v| graph[v].iter().copied()
        ));*/

        assert_eq!(
            derive_scc_nodes(
                graph.iter().enumerate().map(|(i, _)| i),
                &|v| -> Result<usize, ()> { Ok(v) },
                &|v| graph[v].iter().copied()
            ),
            Result::<_, ()>::Ok(vec![
                SccNode::Node(0, Node,),
                SccNode::Scc(vec![
                    SccNode::Node(1, Node,),
                    SccNode::Scc(vec![
                        SccNode::Node(2, Node,),
                        SccNode::Node(3, Node,),
                        SccNode::Node(4, Break(5,),),
                    ],),
                    SccNode::Node(5, Node,),
                    SccNode::Node(6, Break(7,),),
                ],),
                SccNode::Node(7, Node,),
                SccNode::Scc(vec![SccNode::Node(8, Break(9,),),],),
                SccNode::Node(9, Leaf,),
            ],)
        );
    }
}
