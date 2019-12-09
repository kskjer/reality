use std::borrow::Borrow;
use std::collections::BTreeMap;

pub trait IterGroupBy {
    type Value;

    fn group_by<K, M, N, O>(self, mapper: M, value_mapper: N) -> BTreeMap<K, Vec<O>>
    where
        M: Fn(&Self::Value) -> K,
        N: Fn(Self::Value) -> O,
        K: PartialEq + Eq + PartialOrd + Ord;
}

pub trait Slice32Tobytes {
    fn to_be_bytes(self) -> Vec<u8>;
}

impl<T: Iterator> IterGroupBy for T {
    type Value = T::Item;

    fn group_by<K, M, N, O>(self, mapper: M, value_mapper: N) -> BTreeMap<K, Vec<O>>
    where
        M: Fn(&Self::Value) -> K,
        N: Fn(Self::Value) -> O,
        K: PartialEq + Eq + PartialOrd + Ord,
    {
        self.fold(BTreeMap::new(), |mut acc, cur| {
            acc.entry(mapper(&cur))
                .or_insert_with(|| Vec::new())
                .push(value_mapper(cur));

            acc
        })
    }
}

impl<T: Borrow<[u32]>> Slice32Tobytes for T {
    fn to_be_bytes(self) -> Vec<u8> {
        let v = self.borrow();

        v.iter()
            .enumerate()
            .fold(vec![0; v.len() * 4], |mut acc, (i, cur)| {
                let bytes = cur.to_be_bytes();

                for (j, b) in bytes.iter().enumerate() {
                    acc[i * 4 + j] = *b;
                }

                acc
            })
    }
}

#[cfg(test)]
mod tests {
    use crate::IterGroupBy;

    #[test]
    fn test_group_by() {
        let input = vec![(1, 0), (1, 1), (2, 0), (3, 0), (4, 0), (4, 1)];

        let grouping = input.into_iter().group_by(|(a, _)| *a, |(_, b)| b);

        assert_eq!(grouping[&1], &[0, 1]);
        assert_eq!(grouping[&2], &[0]);
        assert_eq!(grouping[&3], &[0]);
        assert_eq!(grouping[&4], &[0, 1]);
    }
}
