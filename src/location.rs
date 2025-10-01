use crate::parser::Extra;

#[derive(Clone, Debug)]
pub struct Location {
    pub span: core::ops::Range<usize>,
    pub module: String,
}

impl Location {
    pub fn new(range: core::ops::Range<usize>, module: String) -> Self {
        Self {
            span: range,
            module,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Located<T> {
    pub data: T,
    pub location: Location,
}

pub trait HasLocation {
    fn location(&self) -> &Location;
}

impl<T> HasLocation for Located<T> {
    fn location(&self) -> &Location {
        &self.location
    }
}

impl HasLocation for Location {
    fn location(&self) -> &Location {
        self
    }
}

impl<T> HasLocation for &T
where
    T: HasLocation,
{
    fn location(&self) -> &Location {
        (*self).location()
    }
}

impl HasLocation for Extra<Location> {
    fn location(&self) -> &Location {
        &self.extra
    }
}

impl<T> HasLocation for Box<T>
where
    T: HasLocation,
{
    fn location(&self) -> &Location {
        self.as_ref().location()
    }
}
