pub struct LineIndex {
    line_starts: Vec<usize>,
}

impl LineIndex {
    pub fn new(source: &str) -> Self {
        let mut line_starts = vec![0];
        for (i, c) in source.char_indices() {
            if c == '\n' {
                line_starts.push(i + 1);
            }
        }
        Self { line_starts }
    }

    pub fn line_col(&self, offset: usize) -> (usize, usize) {
        let line = match self.line_starts.binary_search(&offset) {
            Ok(line) => line,
            Err(next_line) => next_line - 1,
        };

        let line_start = self.line_starts[line];
        let column = offset - line_start;

        (line + 1, column + 1)
    }
}

#[derive(Clone, Debug)]
pub struct Position {
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Debug)]
pub struct Location {
    pub start: Position,
    pub end: Position,
    pub module: String,
    pub file_path: String,
}

impl Location {
    pub fn new(start: Position, end: Position, module: String, file_path: String) -> Self {
        Self {
            start,
            end,
            module,
            file_path,
        }
    }
    pub fn span(&self) -> core::ops::Range<usize> {
        self.start.index..self.end.index
    }
    pub fn update_position(&mut self, line_index: &LineIndex) {
        let start_pos = line_index.line_col(self.start.index);
        let end_pos = line_index.line_col(self.end.index);
        self.start.line = start_pos.0;
        self.start.column = start_pos.1;
        self.end.line = end_pos.0;
        self.end.column = end_pos.1;
    }
}

#[derive(Clone, Debug)]
pub struct Located<T> {
    pub data: T,
    pub location: Location,
}

impl<T> Located<T> {
    pub fn new(data: T, location: Location) -> Self {
        Self { data, location }
    }
}

pub fn located<T>(data: T, location: impl HasLocation) -> Located<T> {
    Located {
        data,
        location: location.location().clone(),
    }
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

impl<T> HasLocation for Box<T>
where
    T: HasLocation,
{
    fn location(&self) -> &Location {
        self.as_ref().location()
    }
}
