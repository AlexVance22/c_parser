use super::Type;


#[derive(Debug, Clone, PartialEq, Default)]
pub enum Expression {
    #[default]
    Empty,

    Assign(Box<Expression>, Box<Expression>),

    Neg(Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Mod(Box<Expression>, Box<Expression>),

    LogNot(Box<Expression>),
    LogAnd(Box<Expression>, Box<Expression>),
    LogOr (Box<Expression>, Box<Expression>),

    BitNot(Box<Expression>),
    BitAnd(Box<Expression>, Box<Expression>),
    BitOr (Box<Expression>, Box<Expression>),
    BitXor(Box<Expression>, Box<Expression>),

    BitShl(Box<Expression>, Box<Expression>),
    BitShr(Box<Expression>, Box<Expression>),

    Eq(Box<Expression>, Box<Expression>),
    Ne(Box<Expression>, Box<Expression>),
    Lt(Box<Expression>, Box<Expression>),
    Gt(Box<Expression>, Box<Expression>),
    Le(Box<Expression>, Box<Expression>),
    Ge(Box<Expression>, Box<Expression>),

    PostInc(Box<Expression>),
    PostDec(Box<Expression>),
    PreInc(Box<Expression>),
    PreDec(Box<Expression>),

    Deref(Box<Expression>),
    AddOf(Box<Expression>),

    Member(Box<Expression>, Box<Expression>),
    ArrGet(Box<Expression>, Box<Expression>),
    FnCall(String, Vec<Expression>),

    Sizeof(Box<Expression>),
    Owned(Type),

    Identifier(String),
    BoolLit(bool),
    CharLit(char),
    IntLit(i64),
    FltLit(f64),
    StrLit(String),
    // ArrLit(Vec<Expression>),
    StructLit(String, Vec<(String, Expression)>),
}

