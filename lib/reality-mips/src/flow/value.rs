use crate::flow::location::Location;
use crate::flow::value_expr::ValueExpr;
use std::fmt::{Debug, Error, Formatter};

#[derive(Copy, Clone)]
pub struct Value<V>(pub(super) Location<V>, pub(super) ValueExpr<V>);

impl<V: Debug> Debug for Value<V> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "Value{:?}", (&self.0, &self.1))
    }
}
