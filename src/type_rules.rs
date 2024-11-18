enum ConstantType {
    None,
    Bool,
    Str,
    Bytes,
    Int,
    Tuple,
    Float,
    Complex,
    Ellipsis,
}

pub struct Heuristic {
    typename: ConstantType,
    score: f64,
}

impl Debug for Heuristic {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Heuristic")
            .field("typename", &self.typename)
            .field("score", &self.score)
            .finish()
    }
}

impl Debug for ConstantType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ConstantType::None => write!(f, "None"),
            ConstantType::Bool => write!(f, "Bool"),
            ConstantType::Str => write!(f, "Str"),
            ConstantType::Float => write!(f, "Float"),
            ConstantType::Int => write!(f, "Int"),
            ConstantType::Tuple => write!(f, "Tuple"),
            ConstantType::Ellipsis => write!(f, "Ellipsis"),
            ConstantType::Bytes => write!(f, "Bytes"),
            ConstantType::Complex => write!(f, "Complex"),
        }
    }
}
