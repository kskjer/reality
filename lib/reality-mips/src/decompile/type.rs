pub enum Type {
    PtrTo(Box<Type>),
    // maybe we can forget the offset here eventually if we have full knowledge
    // of all the fields and their sizes, since it should add up to the same.
    Composite(Vec<(Type, u32)>), 
    Function(Vec<Type>, Type),
    Void,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
}