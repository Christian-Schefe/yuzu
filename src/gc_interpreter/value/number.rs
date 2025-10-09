use std::fmt::Display;

use gc_arena::{Collect, StaticCollect};
use num_bigint::BigInt;
use num_traits::{FromPrimitive, ToPrimitive};

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
