use crate::block::BlockId;
use crate::flow::ComponentId;

#[allow(dead_code)]
#[derive(Copy, Clone, Debug)]
pub enum ValueExpr<V> {
    Input,    // Input / Unknown
    Constant, // Derive value from Location

    IfElse(BlockId, V, V),
    //IfMany(Vec<(V, Condition<V>)>),
    ComponentOutput(ComponentId),
    FunctionOutput(V),

    Add(V, V),
    Sub(V, V),

    MulHi(V, V),
    MulLo(V, V),

    DivRemainder(V, V),
    DivQuotient(V, V),

    ShiftLeftLogical(V, V),
    ShiftRightArithmetic(V, V),
    BitOr(V, V),
    BitAnd(V, V),

    SetOnLessThan(V, V),
}
