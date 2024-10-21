pub mod span;
mod types;
mod expr;
mod write;

pub use types::*;
pub use expr::*;


#[derive(Debug, Clone, PartialEq, Default)]
pub struct VarDecl {
    pub ty: Type,
    pub id: String,
    pub val: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct ExprBlock {
    pub expr: Expression,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct ForLoop {
    pub e1: Box<Statement>,
    pub e2: Expression,
    pub e3: Expression,
    pub body: Option<Block>,
}


pub type Block = Vec<Statement>;


#[derive(Debug, Clone, PartialEq, Default)]
pub enum Statement {
    #[default]
    Empty,
    VarDecl(VarDecl),
    While(ExprBlock),
    Switch(ExprBlock),
    If(ExprBlock),
    Elif(ExprBlock),
    Else(Option<Block>),
    For(ForLoop),
    Break,
    Continue,
    Case(Expression),
    Def,
    Return(Expression),
    Expression(Expression),
    Scope(Block),
}


#[derive(Debug, Clone, PartialEq, Default)]
pub struct TypeDef {
    pub ty: Type,
    pub id: String,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Function {
    pub name: String,
    pub params: Vec<(Type, String)>,
    pub rettype: Type,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: String,
    pub members: Option<Vec<(Type, String)>>,
}


#[derive(Debug, Clone, PartialEq)]
pub enum GlobalItem {
    TypeDef(TypeDef),
    GlobalVarDecl(VarDecl),
    Function(Function),
    Struct(Struct),
}


pub type Ast = Vec<GlobalItem>;

