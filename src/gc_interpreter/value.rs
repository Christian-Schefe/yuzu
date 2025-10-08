use gc_arena::{
    Collect, Gc, Mutation, StaticCollect,
    lock::{GcRefLock, RefLock},
};
use num_bigint::BigInt;
use num_traits::{FromPrimitive, cast::ToPrimitive};
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use crate::{
    ModulePath,
    gc_interpreter::{
        LocatedExpression, MyRoot,
        exception::{cannot_assign_to_constant, cyclic_static_initialization, undefined_variable},
        interpret,
    },
    location::{Located, Location},
};

#[derive(Clone, Collect, Debug)]
#[collect(no_drop)]
pub enum Value<'a> {
    Number(f64),
    Integer(IntVariant),
    Bool(bool),
    String(Gc<'a, StringVariant<'a>>),
    Null,
    Array(GcRefLock<'a, Vec<Value<'a>>>),
    Object(GcRefLock<'a, HashMap<String, Value<'a>>>),
    ClassInstance(GcRefLock<'a, ClassInstanceValue<'a>>),
    Function(GcRefLock<'a, FunctionValue<'a>>),
    Class(GcRefLock<'a, ClassValue<'a>>),
    Resource(GcRefLock<'a, Box<dyn Resource>>),
    Buffer(GcRefLock<'a, Vec<u8>>),
    TypedSlice {
        buffer: GcRefLock<'a, Vec<u8>>,
        start: usize,
        length: usize,
        buffer_type: TypedBufferType,
    },
}

#[derive(Clone, Collect, Debug)]
#[collect(no_drop)]
pub enum IntVariant {
    Small(i64),
    Big(StaticCollect<num_bigint::BigInt>),
}

impl Display for IntVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntVariant::Small(i) => write!(f, "{}", i),
            IntVariant::Big(b) => write!(f, "{}", **b),
        }
    }
}

impl PartialEq for IntVariant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (IntVariant::Small(a), IntVariant::Small(b)) => a == b,
            (IntVariant::Small(a), IntVariant::Big(b)) => num_bigint::BigInt::from(*a) == **b,
            (IntVariant::Big(a), IntVariant::Small(b)) => **a == num_bigint::BigInt::from(*b),
            (IntVariant::Big(a), IntVariant::Big(b)) => **a == **b,
        }
    }
}

impl IntVariant {
    pub fn from_digit_string(s: &str) -> Self {
        if let Ok(i) = s.parse::<i64>() {
            IntVariant::Small(i)
        } else if let Ok(b) = s.parse::<num_bigint::BigInt>() {
            IntVariant::Big(StaticCollect(b))
        } else {
            panic!("Invalid integer string: {}", s);
        }
    }
    pub fn as_big(&self) -> num_bigint::BigInt {
        match self {
            IntVariant::Small(i) => num_bigint::BigInt::from(*i),
            IntVariant::Big(b) => b.0.clone(),
        }
    }
    pub fn to_f64(&self) -> f64 {
        match self {
            IntVariant::Small(i) => *i as f64,
            IntVariant::Big(b) => b.to_f64().expect("This can't fail"),
        }
    }
    pub fn try_to_usize(&self) -> Option<usize> {
        match self {
            IntVariant::Small(i) => (*i).to_usize(),
            IntVariant::Big(b) => b.to_usize(),
        }
    }
    pub fn from_small(i: impl Into<i64>) -> Self {
        IntVariant::Small(i.into())
    }
    pub fn from_u64(u: u64) -> Self {
        if let Some(i) = u.to_i64() {
            IntVariant::Small(i)
        } else {
            IntVariant::Big(StaticCollect(num_bigint::BigInt::from(u)))
        }
    }
    pub fn try_cast<T>(&self) -> Option<T>
    where
        T: FromPrimitive,
    {
        match self {
            IntVariant::Small(i) => FromPrimitive::from_i64(*i),
            IntVariant::Big(b) => b
                .to_owned()
                .to_i64()
                .and_then(|i| FromPrimitive::from_i64(i)),
        }
    }
    pub fn try_cast_u64(&self) -> Option<u64> {
        match self {
            IntVariant::Small(i) => FromPrimitive::from_i64(*i),
            IntVariant::Big(b) => b.to_owned().to_u64(),
        }
    }
    pub fn add(self, other: IntVariant) -> IntVariant {
        match (self, other) {
            (IntVariant::Small(a), IntVariant::Small(b)) => {
                if let Some(sum) = a.checked_add(b) {
                    IntVariant::Small(sum)
                } else {
                    IntVariant::Big(StaticCollect(
                        num_bigint::BigInt::from(a) + num_bigint::BigInt::from(b),
                    ))
                }
            }
            (IntVariant::Small(a), IntVariant::Big(b)) => {
                IntVariant::Big(StaticCollect(num_bigint::BigInt::from(a) + b.0))
            }
            (IntVariant::Big(a), IntVariant::Small(b)) => {
                IntVariant::Big(StaticCollect(a.0 + num_bigint::BigInt::from(b)))
            }
            (IntVariant::Big(a), IntVariant::Big(b)) => IntVariant::Big(StaticCollect(a.0 + b.0)),
        }
    }
    pub fn sub(self, other: IntVariant) -> IntVariant {
        match (self, other) {
            (IntVariant::Small(a), IntVariant::Small(b)) => {
                if let Some(diff) = a.checked_sub(b) {
                    IntVariant::Small(diff)
                } else {
                    IntVariant::Big(StaticCollect(
                        num_bigint::BigInt::from(a) - num_bigint::BigInt::from(b),
                    ))
                }
            }
            (IntVariant::Small(a), IntVariant::Big(b)) => {
                IntVariant::Big(StaticCollect(num_bigint::BigInt::from(a) - b.0))
            }
            (IntVariant::Big(a), IntVariant::Small(b)) => {
                IntVariant::Big(StaticCollect(a.0 - num_bigint::BigInt::from(b)))
            }
            (IntVariant::Big(a), IntVariant::Big(b)) => IntVariant::Big(StaticCollect(a.0 - b.0)),
        }
    }
    pub fn mul(self, other: IntVariant) -> IntVariant {
        match (self, other) {
            (IntVariant::Small(a), IntVariant::Small(b)) => {
                if let Some(prod) = a.checked_mul(b) {
                    IntVariant::Small(prod)
                } else {
                    IntVariant::Big(StaticCollect(
                        num_bigint::BigInt::from(a) * num_bigint::BigInt::from(b),
                    ))
                }
            }
            (IntVariant::Small(a), IntVariant::Big(b)) => {
                IntVariant::Big(StaticCollect(num_bigint::BigInt::from(a) * b.0))
            }
            (IntVariant::Big(a), IntVariant::Small(b)) => {
                IntVariant::Big(StaticCollect(a.0 * num_bigint::BigInt::from(b)))
            }
            (IntVariant::Big(a), IntVariant::Big(b)) => IntVariant::Big(StaticCollect(a.0 * b.0)),
        }
    }
    pub fn div(self, other: IntVariant) -> Option<IntVariant> {
        match (self, other) {
            (IntVariant::Small(a), IntVariant::Small(b)) => {
                if b == 0 {
                    return None;
                }
                Some(IntVariant::Small(a / b))
            }
            (IntVariant::Small(a), IntVariant::Big(b)) => {
                if b.0 == BigInt::ZERO {
                    return None;
                }
                Some(IntVariant::Big(StaticCollect(
                    num_bigint::BigInt::from(a) / b.0,
                )))
            }
            (IntVariant::Big(a), IntVariant::Small(b)) => {
                if b == 0 {
                    return None;
                }
                Some(IntVariant::Big(StaticCollect(
                    a.0 / num_bigint::BigInt::from(b),
                )))
            }
            (IntVariant::Big(a), IntVariant::Big(b)) => {
                if b.0 == BigInt::ZERO {
                    return None;
                }
                Some(IntVariant::Big(StaticCollect(a.0 / b.0)))
            }
        }
    }
    pub fn rem(self, other: IntVariant) -> Option<IntVariant> {
        match (self, other) {
            (IntVariant::Small(a), IntVariant::Small(b)) => {
                if b == 0 {
                    return None;
                }
                Some(IntVariant::Small(a % b))
            }
            (IntVariant::Small(a), IntVariant::Big(b)) => {
                if b.0 == BigInt::ZERO {
                    return None;
                }
                Some(IntVariant::Big(StaticCollect(
                    num_bigint::BigInt::from(a) % b.0,
                )))
            }
            (IntVariant::Big(a), IntVariant::Small(b)) => {
                if b == 0 {
                    return None;
                }
                Some(IntVariant::Big(StaticCollect(
                    a.0 % num_bigint::BigInt::from(b),
                )))
            }
            (IntVariant::Big(a), IntVariant::Big(b)) => {
                if b.0 == BigInt::ZERO {
                    return None;
                }
                Some(IntVariant::Big(StaticCollect(a.0 % b.0)))
            }
        }
    }
    pub fn neg(self) -> IntVariant {
        match self {
            IntVariant::Small(a) => {
                if let Some(neg) = a.checked_neg() {
                    IntVariant::Small(neg)
                } else {
                    IntVariant::Big(StaticCollect(-num_bigint::BigInt::from(a)))
                }
            }
            IntVariant::Big(a) => IntVariant::Big(StaticCollect(-a.0)),
        }
    }
    pub fn invert(self) -> IntVariant {
        match self {
            IntVariant::Small(a) => IntVariant::Small(!a),
            IntVariant::Big(a) => IntVariant::Big(StaticCollect(!a.0)),
        }
    }
    pub fn less(&self, other: &Self) -> bool {
        match (self, other) {
            (IntVariant::Small(a), IntVariant::Small(b)) => a < b,
            (IntVariant::Small(a), IntVariant::Big(b)) => num_bigint::BigInt::from(*a) < **b,
            (IntVariant::Big(a), IntVariant::Small(b)) => **a < num_bigint::BigInt::from(*b),
            (IntVariant::Big(a), IntVariant::Big(b)) => **a < **b,
        }
    }

    pub fn less_equal(&self, other: &Self) -> bool {
        match (self, other) {
            (IntVariant::Small(a), IntVariant::Small(b)) => a <= b,
            (IntVariant::Small(a), IntVariant::Big(b)) => num_bigint::BigInt::from(*a) <= **b,
            (IntVariant::Big(a), IntVariant::Small(b)) => **a <= num_bigint::BigInt::from(*b),
            (IntVariant::Big(a), IntVariant::Big(b)) => **a <= **b,
        }
    }

    pub fn greater(&self, other: &Self) -> bool {
        match (self, other) {
            (IntVariant::Small(a), IntVariant::Small(b)) => a > b,
            (IntVariant::Small(a), IntVariant::Big(b)) => num_bigint::BigInt::from(*a) > **b,
            (IntVariant::Big(a), IntVariant::Small(b)) => **a > num_bigint::BigInt::from(*b),
            (IntVariant::Big(a), IntVariant::Big(b)) => **a > **b,
        }
    }

    pub fn greater_equal(&self, other: &Self) -> bool {
        match (self, other) {
            (IntVariant::Small(a), IntVariant::Small(b)) => a >= b,
            (IntVariant::Small(a), IntVariant::Big(b)) => num_bigint::BigInt::from(*a) >= **b,
            (IntVariant::Big(a), IntVariant::Small(b)) => **a >= num_bigint::BigInt::from(*b),
            (IntVariant::Big(a), IntVariant::Big(b)) => **a >= **b,
        }
    }
}

#[derive(Clone, Collect)]
#[collect(no_drop)]
pub enum StringVariant<'a> {
    Ascii(Vec<u8>),
    Utf16(Vec<u16>),
    Utf32(Vec<char>),
    Slice {
        original: Gc<'a, StringVariant<'a>>,
        start: usize,
        length: usize,
    },
}

impl Debug for StringVariant<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StringVariant::Ascii(_) => write!(f, "Ascii({})", self.to_string()),
            StringVariant::Utf16(_) => write!(f, "Utf16({})", self.to_string()),
            StringVariant::Utf32(_) => write!(f, "Utf32({})", self.to_string()),
            StringVariant::Slice {
                original,
                start,
                length,
            } => write!(f, "Slice({}, {}, {})", original.to_string(), start, length),
        }
    }
}

impl<'a> StringVariant<'a> {
    pub fn to_string(&self) -> String {
        match self {
            StringVariant::Ascii(v) => v.iter().map(|&b| b as char).collect(),
            StringVariant::Utf16(v) => String::from_utf16_lossy(v),
            StringVariant::Utf32(v) => v.iter().collect(),
            StringVariant::Slice {
                original,
                start,
                length,
            } => {
                let end = start + length;
                match &**original {
                    StringVariant::Ascii(v) => v[*start..end.min(v.len())]
                        .iter()
                        .map(|&b| b as char)
                        .collect(),
                    StringVariant::Utf16(v) => {
                        String::from_utf16_lossy(&v[*start..end.min(v.len())])
                    }
                    StringVariant::Utf32(v) => v[*start..end.min(v.len())].iter().collect(),
                    StringVariant::Slice { .. } => panic!("Nested slices are not allowed"),
                }
            }
        }
    }
    pub fn len(&self) -> usize {
        match self {
            StringVariant::Ascii(v) => v.len(),
            StringVariant::Utf16(v) => v.len(),
            StringVariant::Utf32(v) => v.len(),
            StringVariant::Slice { length, .. } => *length,
        }
    }
    pub fn from_string(s: &str) -> Self {
        let highest_codepoint = s.chars().map(|c| c as u32).max().unwrap_or(0);
        if highest_codepoint <= 0x7F {
            StringVariant::Ascii(s.bytes().collect())
        } else if highest_codepoint <= 0xFFFF {
            StringVariant::Utf16(s.encode_utf16().collect())
        } else {
            StringVariant::Utf32(s.chars().collect())
        }
    }
    pub fn slice(
        mc: &Mutation<'a>,
        original: Gc<'a, StringVariant<'a>>,
        start: usize,
        length: usize,
    ) -> Option<Gc<'a, StringVariant<'a>>> {
        if start + length > original.len() {
            return None;
        }
        if length == original.len() {
            return Some(original);
        }
        if let StringVariant::Slice {
            original: inner,
            start: inner_start,
            length: _,
        } = &*original
        {
            Some(Gc::new(
                mc,
                StringVariant::Slice {
                    original: inner.clone(),
                    start: inner_start + start,
                    length,
                },
            ))
        } else {
            Some(Gc::new(
                mc,
                StringVariant::Slice {
                    original,
                    start,
                    length,
                },
            ))
        }
    }
}

#[derive(Clone, Collect, Debug)]
#[collect(no_drop)]
pub enum TypedBufferType {
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float32,
    Float64,
}

impl TypedBufferType {
    pub fn byte_size(&self) -> usize {
        match self {
            TypedBufferType::Int8 | TypedBufferType::Uint8 => 1,
            TypedBufferType::Int16 | TypedBufferType::Uint16 => 2,
            TypedBufferType::Int32 | TypedBufferType::Uint32 | TypedBufferType::Float32 => 4,
            TypedBufferType::Int64 | TypedBufferType::Uint64 | TypedBufferType::Float64 => 8,
        }
    }
    pub fn try_read_value<'a>(&self, buffer: &[u8]) -> Option<Value<'a>> {
        match self {
            TypedBufferType::Int8 => Some(Value::Integer(IntVariant::from_small(buffer[0]))),
            TypedBufferType::Int16 => {
                if buffer.len() < 2 {
                    return None;
                }
                let mut arr = [0u8; 2];
                arr.copy_from_slice(&buffer[..2]);
                Some(Value::Integer(IntVariant::from_small(i16::from_le_bytes(
                    arr,
                ))))
            }
            TypedBufferType::Int32 => {
                if buffer.len() < 4 {
                    return None;
                }
                let mut arr = [0u8; 4];
                arr.copy_from_slice(&buffer[..4]);
                Some(Value::Integer(IntVariant::from_small(i32::from_le_bytes(
                    arr,
                ))))
            }
            TypedBufferType::Int64 => {
                if buffer.len() < 8 {
                    return None;
                }
                let mut arr = [0u8; 8];
                arr.copy_from_slice(&buffer[..8]);
                Some(Value::Integer(IntVariant::from_small(i64::from_le_bytes(
                    arr,
                ))))
            }
            TypedBufferType::Uint8 => Some(Value::Integer(IntVariant::from_small(buffer[0]))),
            TypedBufferType::Uint16 => {
                if buffer.len() < 2 {
                    return None;
                }
                let mut arr = [0u8; 2];
                arr.copy_from_slice(&buffer[..2]);
                Some(Value::Integer(IntVariant::from_small(u16::from_le_bytes(
                    arr,
                ))))
            }
            TypedBufferType::Uint32 => {
                if buffer.len() < 4 {
                    return None;
                }
                let mut arr = [0u8; 4];
                arr.copy_from_slice(&buffer[..4]);
                Some(Value::Integer(IntVariant::from_small(u32::from_le_bytes(
                    arr,
                ))))
            }
            TypedBufferType::Uint64 => {
                if buffer.len() < 8 {
                    return None;
                }
                let mut arr = [0u8; 8];
                arr.copy_from_slice(&buffer[..8]);
                Some(Value::Integer(IntVariant::from_u64(u64::from_le_bytes(
                    arr,
                ))))
            }
            TypedBufferType::Float32 => {
                if buffer.len() < 4 {
                    return None;
                }
                let mut arr = [0u8; 4];
                arr.copy_from_slice(&buffer[..4]);
                Some(Value::Number(f32::from_le_bytes(arr) as f64))
            }
            TypedBufferType::Float64 => {
                if buffer.len() < 8 {
                    return None;
                }
                let mut arr = [0u8; 8];
                arr.copy_from_slice(&buffer[..8]);
                Some(Value::Number(f64::from_le_bytes(arr)))
            }
        }
    }
    pub fn try_write_value<'a>(&self, buffer: &mut [u8], value: &Value<'a>) -> Option<()> {
        match value {
            Value::Integer(i) => {
                match self {
                    TypedBufferType::Int8 => {
                        buffer[0] = i.try_cast()?;
                    }
                    TypedBufferType::Int16 => {
                        let bytes = (i.try_cast::<i16>()?).to_le_bytes();
                        buffer[..2].copy_from_slice(&bytes);
                    }
                    TypedBufferType::Int32 => {
                        let bytes = (i.try_cast::<i32>()?).to_le_bytes();
                        buffer[..4].copy_from_slice(&bytes);
                    }
                    TypedBufferType::Int64 => {
                        let bytes = (i.try_cast::<i64>()?).to_le_bytes();
                        buffer[..8].copy_from_slice(&bytes);
                    }
                    TypedBufferType::Uint8 => {
                        buffer[0] = i.try_cast::<u8>()?;
                    }
                    TypedBufferType::Uint16 => {
                        let bytes = (i.try_cast::<u16>()?).to_le_bytes();
                        buffer[..2].copy_from_slice(&bytes);
                    }
                    TypedBufferType::Uint32 => {
                        let bytes = (i.try_cast::<u32>()?).to_le_bytes();
                        buffer[..4].copy_from_slice(&bytes);
                    }
                    TypedBufferType::Uint64 => {
                        let bytes = (i.try_cast_u64()?).to_le_bytes();
                        buffer[..8].copy_from_slice(&bytes);
                    }
                    _ => return None,
                };
                return Some(());
            }
            Value::Number(f) => {
                let f = *f;
                match self {
                    TypedBufferType::Float32 => {
                        let bytes = (f as f32).to_le_bytes();
                        buffer[..4].copy_from_slice(&bytes);
                    }
                    TypedBufferType::Float64 => {
                        let bytes = (f as f64).to_le_bytes();
                        buffer[..8].copy_from_slice(&bytes);
                    }
                    _ => return None,
                };
                return Some(());
            }
            _ => return None,
        }
    }
}

impl PartialEq for Value<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Integer(a), Value::Number(b)) => a.to_f64() == *b,
            (Value::Number(a), Value::Integer(b)) => *a == b.to_f64(),

            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => match (&**a, &**b) {
                (StringVariant::Ascii(a), StringVariant::Ascii(b)) => a == b,
                (StringVariant::Utf16(a), StringVariant::Utf16(b)) => a == b,
                (StringVariant::Utf32(a), StringVariant::Utf32(b)) => a == b,
                _ => false,
            },
            (Value::Null, Value::Null) => true,
            (Value::Array(a), Value::Array(b)) => Gc::ptr_eq(*a, *b),
            (Value::Object(a), Value::Object(b)) => Gc::ptr_eq(*a, *b),
            (Value::Function(a), Value::Function(b)) => Gc::ptr_eq(*a, *b),
            (Value::Class(a), Value::Class(b)) => Gc::ptr_eq(*a, *b),
            (Value::Buffer(a), Value::Buffer(b)) => Gc::ptr_eq(*a, *b),
            (Value::Resource(a), Value::Resource(b)) => Gc::ptr_eq(*a, *b),
            _ => false,
        }
    }
}

pub trait Resource: Collect {
    fn close(&mut self) -> Result<(), String>;
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, String>;
    fn write(&mut self, buf: &[u8]) -> Result<usize, String>;
}

impl std::fmt::Debug for dyn Resource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<resource>")
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub enum FunctionValue<'a> {
    Function {
        parameters: Vec<String>,
        body: Gc<'a, StaticCollect<LocatedExpression>>,
        env: Gc<'a, Environment<'a>>,
    },
    Builtin {
        func: StaticCollect<
            Box<
                dyn for<'b> Fn(
                    &Mutation<'b>,
                    &MyRoot<'b>,
                    Vec<Value<'b>>,
                    &Location,
                    Gc<'b, Environment<'b>>,
                ) -> Result<Value<'b>, LocatedError<'b>>,
            >,
        >,
    },
}

impl std::fmt::Debug for FunctionValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionValue::Function { parameters, .. } => {
                write!(f, "<function ({})>", parameters.join(", "))
            }
            FunctionValue::Builtin { .. } => write!(f, "<builtin function>"),
        }
    }
}

#[derive(Collect, Debug)]
#[collect(no_drop)]
pub struct ClassInstanceValue<'a> {
    pub fields: HashMap<String, Value<'a>>,
    pub class: GcRefLock<'a, ClassValue<'a>>,
}

#[derive(Collect, Debug)]
#[collect(no_drop)]
pub struct ClassValue<'a> {
    pub instance_fields: Vec<(String, Gc<'a, StaticCollect<LocatedExpression>>)>,
    pub constructor: Option<GcRefLock<'a, FunctionValue<'a>>>,
    pub static_fields: HashMap<String, StaticValue<'a>>,
    pub methods: HashMap<String, GcRefLock<'a, FunctionValue<'a>>>,
    pub static_methods: HashMap<String, GcRefLock<'a, FunctionValue<'a>>>,
    pub parent: Option<GcRefLock<'a, ClassValue<'a>>>,
}

#[derive(Collect, Debug, Clone)]
#[collect(no_drop)]
pub enum StaticValue<'a> {
    Uninitialized(Gc<'a, StaticCollect<LocatedExpression>>),
    BeingInitialized,
    Initialized(Value<'a>),
}

#[derive(Clone, Collect, Debug)]
#[collect(no_drop)]
pub struct Environment<'a> {
    values: GcRefLock<'a, HashMap<String, EnvValue<'a>>>,
    parent: Option<Gc<'a, Environment<'a>>>,
}

#[derive(Clone, Collect, Debug)]
#[collect(no_drop)]
pub enum EnvValue<'a> {
    Value(Value<'a>),
    ConstValue(Value<'a>),
    Static(StaticValue<'a>),
}

impl<'a> Environment<'a> {
    pub fn new_global(mc: &Mutation<'a>) -> Self {
        Self {
            values: Gc::new(mc, RefLock::new(HashMap::new())),
            parent: None,
        }
    }

    pub fn transfer(
        mc: &Mutation<'a>,
        source: Gc<'a, Environment<'a>>,
        target: Gc<'a, Environment<'a>>,
    ) {
        for (k, v) in source.values.borrow().iter() {
            match v {
                EnvValue::Value(val) => target.define(mc, k, val.clone()),
                EnvValue::ConstValue(val) => target.define_const(mc, k, val.clone()),
                EnvValue::Static(s) => target.define_static(mc, k, s.clone()),
            };
        }
    }

    pub fn new(mutation: &Mutation<'a>, parent: Gc<'a, Environment<'a>>) -> Self {
        Self {
            values: Gc::new(mutation, RefLock::new(HashMap::new())),
            parent: Some(parent),
        }
    }

    pub fn get(
        env: Gc<'a, Environment<'a>>,
        mc: &Mutation<'a>,
        root: &MyRoot<'a>,
        name: &str,
        location: &Location,
    ) -> Result<Value<'a>, LocatedError<'a>> {
        let mut values = env.values.borrow_mut(mc);
        if let Some(v) = values.get_mut(name) {
            match v {
                EnvValue::Value(val) | EnvValue::ConstValue(val) => Ok(val.clone()),
                EnvValue::Static(s) => match s {
                    StaticValue::Initialized(val) => Ok(val.clone()),
                    StaticValue::BeingInitialized => {
                        cyclic_static_initialization(mc, root, name, location)
                    }
                    StaticValue::Uninitialized(expr) => {
                        let expr = *expr;
                        *s = StaticValue::BeingInitialized;
                        drop(values);
                        let val = interpret(mc, root, expr.as_ref(), env)?;
                        let mut values = env.values.borrow_mut(mc);
                        let Some(v) = values.get_mut(name) else {
                            unreachable!()
                        };
                        let EnvValue::Static(s) = v else {
                            unreachable!()
                        };
                        *s = StaticValue::Initialized(val.clone());
                        Ok(val)
                    }
                },
            }
        } else if let Some(parent) = env.parent {
            Environment::get(parent, mc, root, name, location)
        } else {
            drop(values);
            undefined_variable(mc, root, name, location)
        }
    }

    pub fn exists(&self, name: &str) -> bool {
        if self.values.borrow().contains_key(name) {
            true
        } else if let Some(parent) = &self.parent {
            parent.exists(name)
        } else {
            false
        }
    }

    pub fn get_simple(&self, name: &str) -> Option<Value<'a>> {
        if let Some(v) = self.values.borrow().get(name) {
            match v {
                EnvValue::Value(val) | EnvValue::ConstValue(val) => Some(val.clone()),
                EnvValue::Static(s) => match s {
                    StaticValue::Initialized(val) => Some(val.clone()),
                    _ => None,
                },
            }
        } else if let Some(parent) = &self.parent {
            parent.get_simple(name)
        } else {
            None
        }
    }

    pub fn set(
        env: Gc<'a, Environment<'a>>,
        mc: &Mutation<'a>,
        root: &MyRoot<'a>,
        name: &str,
        value: Value<'a>,
        location: &Location,
    ) -> Result<(), LocatedError<'a>> {
        if let Some(EnvValue::Value(v)) = env.values.borrow_mut(mc).get_mut(name) {
            *v = value;
            Ok(())
        } else if let Some(_) = env.values.borrow_mut(mc).get_mut(name) {
            cannot_assign_to_constant(mc, root, name, location)
        } else if let Some(parent) = &env.parent {
            Environment::set(*parent, mc, root, name, value, location)
        } else {
            undefined_variable(mc, root, name, location)
        }
    }

    pub fn import_from(
        &self,
        mc: &Mutation<'a>,
        from_env: Gc<'a, Environment<'a>>,
        name: &str,
    ) -> bool {
        let from_ref = from_env.values.borrow();
        if let Some(val) = from_ref.get(name) {
            match val {
                EnvValue::Value(v) | EnvValue::ConstValue(v) => {
                    self.define_const(mc, name, v.clone())
                }
                EnvValue::Static(s) => self.define_static(mc, name, s.clone()),
            }
        } else {
            false
        }
    }

    pub fn define(&self, mc: &Mutation<'a>, name: &str, value: Value<'a>) -> bool {
        let mut map = self.values.borrow_mut(mc);
        if map.contains_key(name) {
            return false;
        }
        map.insert(name.to_string(), EnvValue::Value(value));
        true
    }

    pub fn define_const(&self, mc: &Mutation<'a>, name: &str, value: Value<'a>) -> bool {
        let mut map = self.values.borrow_mut(mc);
        if map.contains_key(name) {
            return false;
        }
        map.insert(name.to_string(), EnvValue::ConstValue(value));
        true
    }

    pub fn define_static(&self, mc: &Mutation<'a>, name: &str, value: StaticValue<'a>) -> bool {
        let mut map = self.values.borrow_mut(mc);
        if map.contains_key(name) {
            return false;
        }
        map.insert(name.to_string(), EnvValue::Static(value));
        true
    }
}

impl<'a> Value<'a> {
    pub fn get_type(&self) -> &'static str {
        match self {
            Value::Number(_) => "Number",
            Value::Integer(_) => "Integer",
            Value::Bool(_) => "Bool",
            Value::String(_) => "String",
            Value::Null => "Null",
            Value::Array(_) => "Array",
            Value::Object(_) => "Object",
            Value::ClassInstance(_) => "ClassInstance",
            Value::Function(_) => "Function",
            Value::Class(_) => "Class",
            Value::Resource(_) => "Resource",
            Value::Buffer(_) => "Buffer",
            Value::TypedSlice { .. } => "TypedSlice",
        }
    }
}

pub fn variable_to_string(var: &Value) -> String {
    match var {
        Value::Number(n) => n.to_string(),
        Value::Integer(i) => i.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::String(s) => s.to_string(),
        Value::Null => "null".to_string(),
        Value::Resource(_) => "<resource>".to_string(),
        Value::Object(entries) => {
            let entries = entries
                .borrow()
                .iter()
                .map(|(k, v)| format!("{}: {}", k, variable_to_string(v)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{}}}", entries)
        }
        Value::ClassInstance(class_instance) => {
            let class_instance = class_instance.borrow();
            let entries = class_instance
                .fields
                .iter()
                .map(|(k, v)| format!("{}: {}", k, variable_to_string(v)))
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "<instance of {} {{{}}}>",
                variable_to_string(&Value::Class(class_instance.class.clone())),
                entries
            )
        }
        Value::Array(elements) => {
            let elements = elements
                .borrow()
                .iter()
                .map(|e| variable_to_string(e))
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{}]", elements)
        }
        Value::Function(f) => match &*f.borrow() {
            FunctionValue::Function { parameters, .. } => {
                format!("<function ({})>", parameters.join(", "))
            }
            FunctionValue::Builtin { .. } => format!("<builtin function>"),
        },
        Value::Class(p) => {
            let static_fields = p
                .borrow()
                .static_fields
                .iter()
                .map(|(k, v)| {
                    format!(
                        "{}: {}",
                        k,
                        match v {
                            StaticValue::Uninitialized(_) => "<uninitialized>".to_string(),
                            StaticValue::BeingInitialized => "<being initialized>".to_string(),
                            StaticValue::Initialized(v) => variable_to_string(v),
                        }
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            let static_methods = p
                .borrow()
                .static_methods
                .iter()
                .map(|(k, v)| format!("{}: {}", k, variable_to_string(&Value::Function(*v))))
                .collect::<Vec<_>>()
                .join(", ");
            let methods = p
                .borrow()
                .methods
                .iter()
                .map(|(k, v)| format!("{}: {}", k, variable_to_string(&Value::Function(*v))))
                .collect::<Vec<_>>()
                .join(", ");
            let instance_fields = p
                .borrow()
                .instance_fields
                .iter()
                .map(|(k, _)| format!("{}: <initializer>", k))
                .collect::<Vec<_>>()
                .join(", ");
            let constructor = p.borrow().constructor.map_or("none".to_string(), |c| {
                variable_to_string(&Value::Function(c))
            });
            let entries = format!(
                "static fields: {{{}}}, static methods: {{{}}}, constructor: {}, fields: {{{}}}, methods: {{{}}}",
                static_fields, static_methods, constructor, instance_fields, methods
            );

            let parent = if let Some(parent) = &p.borrow().parent {
                format!(
                    " extends {}",
                    variable_to_string(&Value::Class(parent.clone()))
                )
            } else {
                "".to_string()
            };
            format!("<prototype {{{}}}{}>", entries, parent)
        }
        Value::Buffer(buf) => {
            let buf = buf.borrow();
            let display = if buf.len() > 10 {
                let mut s = buf[..10]
                    .iter()
                    .map(|b| format!("{:02x}", b))
                    .collect::<Vec<_>>()
                    .join(" ");
                s.push_str(" ...");
                s
            } else {
                buf.iter()
                    .map(|b| format!("{:02x}", b))
                    .collect::<Vec<_>>()
                    .join(" ")
            };
            format!("<buffer [{}]>", display)
        }
        Value::TypedSlice {
            buffer,
            start,
            length,
            buffer_type,
        } => {
            let buf = buffer.borrow();
            let end = start + length;
            let display = if *length > 10 {
                let mut s = buf[*start..(*start + 10).min(buf.len())]
                    .iter()
                    .map(|b| format!("{:02x}", b))
                    .collect::<Vec<_>>()
                    .join(" ");
                s.push_str(" ...");
                s
            } else {
                buf[*start..end.min(buf.len())]
                    .iter()
                    .map(|b| format!("{:02x}", b))
                    .collect::<Vec<_>>()
                    .join(" ")
            };
            format!("<typed slice of {:?} [{}]>", buffer_type, display)
        }
    }
}

pub type LocatedError<'a> = Located<Value<'a>>;

#[derive(Collect)]
#[collect(no_drop)]
pub struct ValueClasses<'a> {
    pub number: GcRefLock<'a, ClassValue<'a>>,
    pub string: GcRefLock<'a, ClassValue<'a>>,
    pub array: GcRefLock<'a, ClassValue<'a>>,
    pub object: GcRefLock<'a, ClassValue<'a>>,
    pub function: GcRefLock<'a, ClassValue<'a>>,
    pub class_instance: GcRefLock<'a, ClassValue<'a>>,
    pub integer: GcRefLock<'a, ClassValue<'a>>,
    pub bool: GcRefLock<'a, ClassValue<'a>>,
    pub null: GcRefLock<'a, ClassValue<'a>>,
    pub class: GcRefLock<'a, ClassValue<'a>>,
    pub exception: GcRefLock<'a, ClassValue<'a>>,
    pub resource: GcRefLock<'a, ClassValue<'a>>,
    pub buffer: GcRefLock<'a, ClassValue<'a>>,
    pub typed_slice: GcRefLock<'a, ClassValue<'a>>,
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct ModuleTree<'a> {
    pub env: Gc<'a, Environment<'a>>,
    children: HashMap<String, GcRefLock<'a, ModuleTree<'a>>>,
}

impl<'a> ModuleTree<'a> {
    pub fn new(mc: &Mutation<'a>) -> Self {
        Self {
            env: Gc::new(mc, Environment::new_global(mc)),
            children: HashMap::new(),
        }
    }
    pub fn get_or_insert(
        mc: &Mutation<'a>,
        tree: GcRefLock<'a, ModuleTree<'a>>,
        module_path: &ModulePath,
    ) -> GcRefLock<'a, ModuleTree<'a>> {
        let mut current = tree;
        for segment in &module_path.path {
            current = *current
                .borrow_mut(mc)
                .children
                .entry(segment.clone())
                .or_insert_with(|| Gc::new(mc, RefLock::new(ModuleTree::new(mc))))
        }
        current
    }
    pub fn get(
        tree: GcRefLock<'a, ModuleTree<'a>>,
        module_path: &ModulePath,
    ) -> Option<GcRefLock<'a, ModuleTree<'a>>> {
        let mut current = tree;
        for segment in &module_path.path {
            if let Some(child) = current.borrow().children.get(segment) {
                current = *child;
            } else {
                return None;
            }
        }
        Some(current)
    }
}
