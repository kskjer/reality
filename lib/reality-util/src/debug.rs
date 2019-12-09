use std::collections::BTreeMap;
use std::fmt::{Debug, Write};

pub fn dump_map<K, V>(
    map: &BTreeMap<K, V>,
    depth: usize,
    map_val: impl Fn(&V, usize) -> String,
) -> String
where
    K: Debug,
{
    let mut output = String::new();

    writeln!(output, "{{").unwrap();

    for (k, v) in map {
        writeln!(
            output,
            "{:<width$}{:?}: {}",
            "",
            k,
            map_val(v, depth + 1).trim_end(),
            width = (depth + 1) * 4
        )
        .unwrap();
    }

    writeln!(output, "{:<width$}}}", "", width = depth * 4).unwrap();

    output
}

#[cfg(test)]
mod test {
    #[test]
    fn test_map_display() {
        use super::*;

        let map = [
            (1, &[(1, 2), (2, 3), (3, 4)][..]),
            (2, &[(5, 6), (7, 8)][..]),
        ]
        .iter()
        .copied()
        .map(|(k, items)| (k, items.iter().copied().collect::<BTreeMap<_, _>>()))
        .collect::<BTreeMap<_, _>>();

        assert_eq!(
            "{\n    1: {\n        1: 2\n        2: 3\n        3: 4\n    }\n    2: {\n        5: 6\n        7: 8\n    }\n}\n".to_owned(),
            dump_map(&map, 0, |v, depth| dump_map(v, depth, |v, _| v.to_string()))
        );
    }
}
