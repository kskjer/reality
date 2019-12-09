#[macro_export]
macro_rules! impl_counter {
    ($name:tt) => {
        #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
        pub struct $name(pub(super) NonZeroU16);

        impl $name {
            #[allow(dead_code)]
            pub const MIN: $name = $name(unsafe { NonZeroU16::new_unchecked(1) });
            #[allow(dead_code)]
            pub const MAX: $name = $name(unsafe { NonZeroU16::new_unchecked(u16::MAX) });

            #[allow(dead_code)]
            pub fn next(self) -> Self {
                Self(NonZeroU16::new(self.0.get().checked_add(1).unwrap()).unwrap())
            }

            #[allow(dead_code)]
            pub fn get(self) -> u16 {
                self.0.get()
            }

            #[allow(dead_code)]
            pub fn new(val: usize) -> Option<$name> {
                use std::convert::TryFrom;

                u16::try_from(val)
                    .ok()
                    .and_then(|v| NonZeroU16::new(v))
                    .map(|v| $name(v))
            }

            #[allow(dead_code)]
            pub fn next_n(self, n: u16) -> impl Iterator<Item = $name> {
                ((self.0).get()..(self.0.get() + n)).map(|v| $name(NonZeroU16::new(v).unwrap()))
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
                write!(f, "{}", self.0)
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
                write!(f, concat!(stringify!($name), "({})"), self.0)
            }
        }
    };
}
