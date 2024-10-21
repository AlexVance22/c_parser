use crate::lexer::Token;


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Flat (TypeBase),
    PtrTo{ is_const: bool, inner: Box<Type> },
    RefTo{ is_const: bool, inner: Box<Type> },
}

impl Type {
    pub fn new() -> TypeBaseBuilder {
        TypeBaseBuilder::default()
    }
}

impl Default for Type {
    fn default() -> Self {
        Self::Flat(TypeBase::default())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TypeBase {
    pub is_const: bool,
    pub value: FlatType,
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum TypeKeyword {
    Void     = 1,
    Const    = 1 << 1,
    Signed   = 1 << 2,
    Unsigned = 1 << 3,
    Short    = 1 << 4,
    Long     = 1 << 5,
    Bool     = 1 << 6,
    Char     = 1 << 7,
    Int      = 1 << 8,
    Float    = 1 << 9,
    Double   = 1 << 10,
}

impl std::ops::BitOr for TypeKeyword {
    type Output = u32;
    fn bitor(self, rhs: Self) -> Self::Output {
        self as u32 | rhs as u32
    }
}
impl std::ops::BitOr<u32> for TypeKeyword {
    type Output = u32;
    fn bitor(self, rhs: u32) -> Self::Output {
        self as u32 | rhs
    }
}
impl std::ops::BitOr<TypeKeyword> for u32 {
    type Output = u32;
    fn bitor(self, rhs: TypeKeyword) -> Self::Output {
        self | rhs as u32
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Primitive {
    #[default]
    Void,
    Bool,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FlatType {
    Primitive(Primitive),
    Custom(String),
}

impl Default for FlatType {
    fn default() -> Self {
        Self::Primitive(Primitive::Void)
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TypeBaseBuilder {
    kw: u32,
    custom: Option<String>,
}

impl TypeBaseBuilder {
    fn has(&self, kw: TypeKeyword) -> bool {
        (self.kw & kw as u32) > 0
    }
    fn has_any(&self, kw: u32) -> bool {
        (self.kw & kw) > 0
    }

    pub fn type_info(&mut self, tok: Token) -> &mut Self {
        match tok {
            Token::Void     => self.kw |= TypeKeyword::Void as u32,
            Token::Const    => self.kw |= TypeKeyword::Const as u32,
            Token::Unsigned => self.kw |= TypeKeyword::Unsigned as u32,
            Token::Short    => self.kw |= TypeKeyword::Short as u32,
            Token::Long     => self.kw |= TypeKeyword::Long as u32,
            Token::Bool     => self.kw |= TypeKeyword::Bool as u32,
            Token::Char     => self.kw |= TypeKeyword::Char as u32,
            Token::Int      => self.kw |= TypeKeyword::Int as u32,
            Token::Float    => self.kw |= TypeKeyword::Float as u32,
            Token::Double   => self.kw |= TypeKeyword::Double as u32,
            Token::Identifier(id) => self.custom = Some(id),
            _ => unreachable!()
        }
        self
    }

    pub fn lock(self) -> Option<TypePtrBuilder> {
        use TypeKeyword::*;

        let is_const = self.has(TypeKeyword::Const);
        let unsigned = self.has(TypeKeyword::Unsigned);

        if let Some(id) = self.custom {
            if self.kw == 0 || self.kw == 2 {
                Some(TypePtrBuilder::Flat(TypeBase{ is_const, value: FlatType::Custom(id) }))
            } else {
                None
            }
        } else if ((self.has_any(Short|Long)) && (self.has_any(Void|Bool|Char|Float|Double))) ||
                (unsigned && (self.has_any(Signed|Bool|Float|Double))) {
            None
        } else if self.has(TypeKeyword::Short) {
            Some(TypePtrBuilder::Flat(TypeBase{ is_const, value: FlatType::Primitive(if unsigned { Primitive::U16 } else { Primitive::I16 }) }))
        } else if self.has(TypeKeyword::Long) {
            Some(TypePtrBuilder::Flat(TypeBase{ is_const, value: FlatType::Primitive(if unsigned { Primitive::U64 } else { Primitive::I64 }) }))
        } else if self.has(TypeKeyword::Void) {
            Some(TypePtrBuilder::Flat(TypeBase{ is_const, value: FlatType::Primitive(Primitive::Void) }))
        } else if self.has(TypeKeyword::Bool) {
            Some(TypePtrBuilder::Flat(TypeBase{ is_const, value: FlatType::Primitive(Primitive::Bool) }))
        } else if self.has(TypeKeyword::Char) {
            Some(TypePtrBuilder::Flat(TypeBase{ is_const, value: FlatType::Primitive(if unsigned { Primitive::U8 } else { Primitive::I8 }) }))
        } else if self.has(TypeKeyword::Int) {
            Some(TypePtrBuilder::Flat(TypeBase{ is_const, value: FlatType::Primitive(if unsigned { Primitive::U32 } else { Primitive::I32 }) }))
        } else if self.has(TypeKeyword::Float) {
            Some(TypePtrBuilder::Flat(TypeBase{ is_const, value: FlatType::Primitive(Primitive::F32) }))
        } else if self.has(TypeKeyword::Double) {
            Some(TypePtrBuilder::Flat(TypeBase{ is_const, value: FlatType::Primitive(Primitive::F64) }))
        } else {
            None
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypePtrBuilder {
    Flat (TypeBase),
    PtrTo{ is_const: bool, inner: Box<Type> },
    RefTo{ is_const: bool, inner: Box<Type> },
}

impl TypePtrBuilder {
    pub fn ptr_status(&mut self, tok: Token) -> &mut Self {
        let old = std::mem::take(self).wrap();

        match tok {
            Token::Mul    => *self = Self::PtrTo{ is_const: false, inner: Box::new(old) },
            Token::BitAnd => *self = Self::RefTo{ is_const: false, inner: Box::new(old) },
            Token::Const  => match self {
                Self::Flat ( tb )           => tb.is_const = true,
                Self::PtrTo{ is_const, .. } => *is_const = true,
                Self::RefTo{ is_const, .. } => *is_const = true,
            }
            _ => unreachable!(),
        }
        self
    }

    fn wrap(self) -> Type {
        match self {
            Self::Flat ( tb ) => Type::Flat(tb),
            Self::PtrTo{ is_const, inner } => Type::PtrTo{ is_const, inner },
            Self::RefTo{ is_const, inner } => Type::RefTo{ is_const, inner },
        }
    }

    pub fn build(self) -> Option<Type> {
        Some(self.wrap())
    }
}


impl Default for TypePtrBuilder {
    fn default() -> Self {
        Self::Flat(TypeBase::default())
    }
}

