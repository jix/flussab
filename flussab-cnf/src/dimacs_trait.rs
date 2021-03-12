/// Type that can be used to represent literals and variables in DIMACS CNF and related file
/// formats.
pub trait Dimacs: Copy + Eq {
    /// Largest literal / variable supported.
    const MAX_DIMACS: isize;

    /// Returns a literal / variable that is encoded as the passed integer in the DIMACS CNF format.
    ///
    /// If the value is out of range, e.g. negative for a variable, this may panic or return an
    /// arbitrary value.
    fn from_dimacs(value: isize) -> Self;

    /// Returns the integer that is used to encode this literal / variable in the DIMACS CNF format.
    fn dimacs(self) -> isize;
}

impl Dimacs for isize {
    const MAX_DIMACS: isize = isize::MAX;

    fn from_dimacs(value: isize) -> Self {
        value
    }

    fn dimacs(self) -> isize {
        self
    }
}

impl Dimacs for i64 {
    #[cfg(any(
        target_pointer_width = "64",
        target_pointer_width = "32",
        target_pointer_width = "16"
    ))]
    const MAX_DIMACS: isize = isize::MAX;

    fn from_dimacs(value: isize) -> Self {
        value as i64
    }

    fn dimacs(self) -> isize {
        self as isize
    }
}

impl Dimacs for i32 {
    #[cfg(target_pointer_width = "64")]
    const MAX_DIMACS: isize = i32::MAX as isize;

    #[cfg(any(target_pointer_width = "32", target_pointer_width = "16"))]
    const MAX_DIMACS: isize = isize::MAX;

    fn from_dimacs(value: isize) -> Self {
        value as i32
    }

    fn dimacs(self) -> isize {
        self as isize
    }
}

impl Dimacs for i16 {
    const MAX_DIMACS: isize = i16::MAX as isize;

    fn from_dimacs(value: isize) -> Self {
        value as i16
    }

    fn dimacs(self) -> isize {
        self as isize
    }
}

impl Dimacs for i8 {
    const MAX_DIMACS: isize = i8::MAX as isize;

    fn from_dimacs(value: isize) -> Self {
        value as i8
    }

    fn dimacs(self) -> isize {
        self as isize
    }
}
