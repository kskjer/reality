use crate::flow::value::Value;
use crate::impl_counter;
use std::convert::TryFrom;
use std::fmt::{Display, Error, Formatter};
use std::num::NonZeroU16;
use std::ops::Index;
use std::{fmt, u16};

impl_counter!(ValueId);

#[derive(Debug)]
pub struct VecValueStore<V>(Vec<Value<V>>);
pub struct DisplayVecValueStore<'a>(&'a VecValueStore<ValueId>);

pub trait ValueStore<V>: Index<V, Output = Value<V>> {
    type Id;
    type Error;

    fn push(&mut self, value: Value<Self::Id>) -> Result<Self::Id, Self::Error>;
}

#[derive(Debug)]
pub enum ValueStoreError {
    IdOverflow(usize),
}

impl<V> VecValueStore<V> {
    pub fn new() -> Self {
        VecValueStore(Vec::new())
    }
}

impl VecValueStore<ValueId> {
    pub fn last_id(&self) -> Result<Option<ValueId>, ValueStoreError> {
        Ok(if self.0.len() > 0 {
            Some(ValueId(convert_id(self.0.len())?))
        } else {
            None
        })
    }

    pub fn iter_from<'a>(
        &'a self,
        index: ValueId,
    ) -> impl Iterator<Item = &'a Value<ValueId>> + '_ {
        let index: usize = index.get().into();

        self.0[index..].iter()
    }

    pub fn display(&self) -> DisplayVecValueStore {
        DisplayVecValueStore(self)
    }
}

impl<'a> Display for DisplayVecValueStore<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        writeln!(f, "[")?;

        for (i, v) in (self.0).0.iter().enumerate() {
            writeln!(f, "    {}: {:?},", i + 1, v)?;
        }

        writeln!(f, "]")
    }
}

impl Index<ValueId> for VecValueStore<ValueId> {
    type Output = Value<ValueId>;

    fn index(&self, index: ValueId) -> &Self::Output {
        // Maybe we could use the unsafe variant here, since the collection is append only.
        // But you could still use it to erroneously index into other collections.
        &self.0[index.0.get() as usize - 1]
    }
}

impl ValueStore<ValueId> for VecValueStore<ValueId> {
    type Id = ValueId;
    type Error = ValueStoreError;

    fn push(&mut self, value: Value<Self::Id>) -> Result<Self::Id, Self::Error> {
        let new_id = ValueId({
            let desired = self.0.len() + 1;

            convert_id(desired)?
        });

        self.0.push(value);

        Ok(new_id)
    }
}

fn convert_id(desired: usize) -> Result<NonZeroU16, ValueStoreError> {
    u16::try_from(desired)
        .ok()
        .and_then(NonZeroU16::new)
        .ok_or(ValueStoreError::IdOverflow(desired))
}
