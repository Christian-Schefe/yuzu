use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeHint {
    Number,
    Integer,
    Bool,
    String,
    Null,
    Array(Option<Box<TypeHint>>),
    Object(HashMap<String, TypeHint>),
    Class(String),
    Function {
        parameters: Vec<TypeHint>,
        return_type: Box<TypeHint>,
    },
    ClassInstance(String),
    Any,
    Union(Vec<TypeHint>),
}

impl TypeHint {
    pub fn union(left: TypeHint, right: TypeHint) -> TypeHint {
        if left == right {
            return left;
        }
        match (left, right) {
            (TypeHint::Union(mut types), TypeHint::Union(other_types)) => {
                types.extend(other_types);
                TypeHint::Union(types)
            }
            (TypeHint::Union(mut types), other) | (other, TypeHint::Union(mut types)) => {
                types.push(other);
                TypeHint::Union(types)
            }
            (left, right) => TypeHint::Union(vec![left, right]),
        }
    }
}
