use std::fmt::Display;
use super::*;


impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "void"),
            Self::Bool => write!(f, "bool"),
            Self::I8   => write!(f, "char"),
            Self::I16  => write!(f, "short"),
            Self::I32  => write!(f, "int"),
            Self::I64  => write!(f, "long long"),
            Self::U8   => write!(f, "unsigned char"),
            Self::U16  => write!(f, "unsigned short"),
            Self::U32  => write!(f, "unsigned int"),
            Self::U64  => write!(f, "unsigned long long"),
            Self::F32  => write!(f, "float"),
            Self::F64  => write!(f, "double"),
        }
    }
}

impl Display for FlatType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primitive(p) => write!(f, "{}", p),
            Self::Custom(c)    => write!(f, "{}", c),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Flat(tb) => {
                if tb.is_const { write!(f, "const ")?; }
                write!(f, "{}", tb.value)?;
            }
            Self::PtrTo{ is_const, inner } => {
                write!(f, "{}*", inner)?;
                if *is_const { write!(f, " const")?; }
            }
            Self::RefTo{ is_const, inner } => {
                write!(f, "{}*", inner)?;
                if *is_const { write!(f, " const")?; }
            }
        }
        Ok(())
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Empty            => write!(f, ""),
            Self::Assign(lhs, rhs) => write!(f, "{} = {}", lhs, rhs),
            Self::Eq(lhs, rhs)     => write!(f, "({} == {})", lhs, rhs),
            Self::Ne(lhs, rhs)     => write!(f, "({} != {})", lhs, rhs),
            Self::Lt(lhs, rhs)     => write!(f, "({} < {})", lhs, rhs),
            Self::Gt(lhs, rhs)     => write!(f, "({} > {})", lhs, rhs),
            Self::Le(lhs, rhs)     => write!(f, "({} <= {})", lhs, rhs),
            Self::Ge(lhs, rhs)     => write!(f, "({} >= {})", lhs, rhs),
            Self::BitShl(lhs, rhs) => write!(f, "({} << {})", lhs, rhs),
            Self::BitShr(lhs, rhs) => write!(f, "({} >> {})", lhs, rhs),
            Self::BitOr (lhs, rhs) => write!(f, "({} | {})", lhs, rhs),
            Self::BitAnd(lhs, rhs) => write!(f, "({} & {})", lhs, rhs),
            Self::BitXor(lhs, rhs) => write!(f, "({} ^ {})", lhs, rhs),
            Self::BitNot(lhs)      => write!(f, "(~{})", lhs),
            Self::LogAnd(lhs, rhs) => write!(f, "({} && {})", lhs, rhs),
            Self::LogOr (lhs, rhs) => write!(f, "({} || {})", lhs, rhs),
            Self::LogNot(lhs)      => write!(f, "(!{})", lhs),
            Self::Add(lhs, rhs)    => write!(f, "({} + {})", lhs, rhs),
            Self::Sub(lhs, rhs)    => write!(f, "({} - {})", lhs, rhs),
            Self::Mul(lhs, rhs)    => write!(f, "({} * {})", lhs, rhs),
            Self::Div(lhs, rhs)    => write!(f, "({} / {})", lhs, rhs),
            Self::Mod(lhs, rhs)    => write!(f, "({} % {})", lhs, rhs),
            Self::Neg(lhs)         => write!(f, "(-{})", lhs),
            Self::PreInc(lhs)      => write!(f, "(++{})", lhs),
            Self::PreDec(lhs)      => write!(f, "(--{})", lhs),
            Self::PostInc(lhs)     => write!(f, "({}++)", lhs),
            Self::PostDec(lhs)     => write!(f, "({}--)", lhs),
            Self::Deref(lhs)       => write!(f, "(*{})", lhs),
            Self::AddOf(lhs)       => write!(f, "(&{})", lhs),
            Self::Member(lhs, rhs) => write!(f, "{}.{}", lhs, rhs),
            Self::ArrGet(lhs, rhs) => write!(f, "{}[{}]", lhs, rhs),
            Self::FnCall(lhs, rhs) => {
                write!(f, "{}(", lhs)?;
                let mut iter = rhs.iter();
                if let Some(p) = iter.next() {
                    write!(f, "{}", p)?;
                }
                for p in iter {
                    write!(f, ", {}", p)?;
                }
                write!(f, ")")
            }
            Self::Sizeof(lhs)      => write!(f, "(sizeof({}))", lhs),
            Self::Owned(lhs)       => write!(f, "(({}*)malloc(sizeof({})))", lhs, lhs),
            Self::Identifier(lhs)  => write!(f, "{}", lhs),
            Self::BoolLit(lhs)     => write!(f, "{}", lhs),
            Self::CharLit(lhs)     => write!(f, "'{}'", lhs),
            Self::IntLit(lhs)      => write!(f, "{}", lhs),
            Self::FltLit(lhs)      => write!(f, "\"{}\"", lhs),
            Self::StrLit(lhs)      => write!(f, "{}", lhs),
            // Self::ArrLit(lhs)      => {
            //     write!(f, "{{ ")?;
            //     let mut iter = lhs.iter();
            //     if let Some(p) = iter.next() {
            //         write!(f, "{}", p)?;
            //     }
            //     for p in iter {
            //         write!(f, ", {}", p)?;
            //     }
            //     write!(f, " }}")
            // }
            Self::StructLit(lhs, rhs) => {
                write!(f, "({}){{", lhs)?;
                for (s, p) in rhs {
                    write!(f, ".{} = {}, ", s, p)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl Display for VarDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.ty, self.id)?;
        if let Some(val) = &self.val {
            write!(f, " = {}", val)?;
        } else {
            write!(f, "")?;
        }
        Ok(())
    }
}

impl Display for ExprBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.expr)?;
        if let Some(body) = &self.body {
            writeln!(f, " {{")?;
            for s in body {
                writeln!(f, "{}", s)?;
            }
            write!(f, "}}")?;
        } else {
            write!(f, ";")?;
        }
        Ok(())
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Empty => write!(f, "")?,
            Self::VarDecl(v) => write!(f, "{};", v)?,
            Self::While(cond) => write!(f, "while {}", cond)?,
            Self::Switch(cond) => write!(f, "switch {}", cond)?,
            Self::If(cond) => write!(f, "if {}", cond)?,
            Self::Elif(cond) => write!(f, "else if {}", cond)?,
            Self::Else(body) => {
                write!(f, "else")?;
                if let Some(body) = body {
                    writeln!(f, " {{")?;
                    for s in body {
                        writeln!(f, "{}", s)?;
                    }
                    write!(f, "}}")?;
                } else {
                    write!(f, ";")?;
                }
            }
            Self::For(forloop) => {
                write!(f, "for ({}; {}; {})", forloop.e1, forloop.e2, forloop.e3)?;
                if let Some(body) = &forloop.body {
                    writeln!(f, " {{")?;
                    for s in body {
                        writeln!(f, "{}", s)?;
                    }
                    write!(f, "}}")?;
                } else {
                    write!(f, ";")?;
                }
            }
            Self::Return(expr) => writeln!(f, "return {};", expr)?,
            Self::Break => writeln!(f, "break;")?,
            Self::Continue => writeln!(f, "continue;")?,
            Self::Case(expr) => write!(f, "case {}:", expr)?,
            Self::Def => write!(f, "default:")?,
            Self::Expression(expr) => write!(f, "{};", expr)?,
            Self::Scope(body) => {
                writeln!(f, " {{")?;
                for s in body {
                    writeln!(f, "{};", s)?;
                }
                write!(f, "}}")?;
            }
        }
        Ok(())
    }
}

impl Display for GlobalItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeDef(td) => writeln!(f, "typedef {} {};", td.ty, td.id),
            Self::GlobalVarDecl(decl) => writeln!(f, "{};", decl),
            Self::Struct(strct) => writeln!(f, "{}", strct),
            Self::Function(func) => writeln!(f, "{}", func),
        }
    }
}

impl Display for Struct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct")
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}(", self.rettype, self.name)?;
        let mut iter = self.params.iter();
        if let Some((t, n)) = iter.next() {
            write!(f, "{} {}", t, n)?;
        }
        for (t, n) in iter {
            write!(f, ", {} {}", t, n)?;
        }
        write!(f, ")")?;
        if let Some(block) = &self.body {
            writeln!(f, " {{")?;
            for s in block {
                writeln!(f, "    {}", s)?;
            }
            writeln!(f, "}}")?;
        } else {
            writeln!(f, ";")?;
        }
        Ok(())
    }
}

