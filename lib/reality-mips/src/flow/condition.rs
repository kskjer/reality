use std::fmt::{Debug, Error, Formatter};
use std::ops;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Condition<V> {
    Equal(V, V),
    NotEqual(V, V),
    GreaterThan(V, V),
    GreaterThanOrEqualTo(V, V),
    LessThan(V, V),
    LessThanOrEqualTo(V, V),
}

impl<V> Condition<V> {
    pub fn map_lr<T>(&self, mapper: impl Fn(&V) -> T) -> (T, T) {
        use Condition::*;

        match self {
            Equal(l, r)
            | NotEqual(l, r)
            | GreaterThan(l, r)
            | GreaterThanOrEqualTo(l, r)
            | LessThan(l, r)
            | LessThanOrEqualTo(l, r) => (mapper(l), mapper(r)),
        }
    }
}

impl<V> ops::Not for Condition<V> {
    type Output = Self;

    fn not(self) -> Self::Output {
        use Condition::*;

        match self {
            Equal(l, r) => NotEqual(l, r),
            NotEqual(l, r) => Equal(l, r),
            GreaterThan(l, r) => LessThanOrEqualTo(l, r),
            GreaterThanOrEqualTo(l, r) => LessThan(l, r),
            LessThan(l, r) => GreaterThanOrEqualTo(l, r),
            LessThanOrEqualTo(l, r) => GreaterThan(l, r),
        }
    }
}

impl<V: Debug + Copy> Debug for Condition<V> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        use Condition::*;

        let (l, r) = self.map_lr(|x| *x);

        write!(
            f,
            "`{:?} {} {:?}`",
            l,
            match self {
                Equal(_, _) => "==",
                NotEqual(_, _) => "!=",
                GreaterThan(_, _) => ">",
                GreaterThanOrEqualTo(_, _) => ">=",
                LessThan(_, _) => "<",
                LessThanOrEqualTo(_, _) => "<=",
            },
            r
        )
    }
}

// As soon as we hit the "not taken" portion of a branch, we can remove the condition.
// Yes?
// No, we combine the active conditions as we do with the Values when creating a Phi.
