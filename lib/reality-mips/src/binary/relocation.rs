#[derive(Copy, Clone, Debug)]
pub enum RelocationType {
    Hi16,
    Lo16,
    R32,
    R26,
}

#[derive(Debug)]
pub struct Relocation<S>(pub(crate) RelocationType, pub(crate) S);
