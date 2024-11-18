//! Types for representing BTOR2.
use std::{io::Write, mem::take, num::NonZeroU64};

use bstr::BStr;
use flussab::DeferredWriter;

/// A single non-empty line of a BTOR2 file.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Line<'a> {
    /// A comment line.
    Comment(&'a BStr),
    /// A BTOR2 node.
    Node(Node<'a>),
}

impl<'a> Line<'a> {
    pub(crate) fn has_comment(&self) -> bool {
        match self {
            Line::Comment(_) => true,
            Line::Node(node) => node.comment.is_some(),
        }
    }

    pub(crate) fn update_comment(&mut self, comment: &'a BStr) {
        match self {
            Line::Comment(line_comment) => *line_comment = comment,
            Line::Node(node) => node.comment = Some(comment),
        }
    }

    pub(crate) fn update_bufs(&mut self, constant: &'a str, symbol: &'a BStr, nodes: &'a [NodeId]) {
        if let Line::Node(node) = self {
            if let Some(node_symbol) = &mut node.symbol {
                *node_symbol = symbol
            }
            match &mut node.variant {
                NodeVariant::Value(Value {
                    variant:
                        ValueVariant::Const(
                            Const::Binary(BinaryConst(value_constant))
                            | Const::Hex(HexConst(value_constant))
                            | Const::Decimal(DecimalConst(value_constant)),
                        ),
                    ..
                }) => *value_constant = constant,
                NodeVariant::Output(Output::Justice(conditions)) => {
                    *conditions = nodes;
                }
                _ => (),
            }
        }
    }
}

/// Refers to a BTOR2 node.
///
/// Note that this is used for sort nodes, value nodes, state updates nodes and output nodes as they
/// share a single id space in BTOR2.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct NodeId(pub NonZeroU64);

impl NodeId {
    /// Returns the node with the given id.
    ///
    /// This panics when the id is zero.
    pub fn new(id: u64) -> Self {
        Self(NonZeroU64::new(id).unwrap())
    }
}

/// A BTOR2 node.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Node<'a> {
    /// The node id of this node.
    pub id: NodeId,
    /// The definition of this node.
    pub variant: NodeVariant<'a>,
    /// An optional name for this node.
    pub symbol: Option<&'a BStr>,
    /// An optional comment following the node definition.
    pub comment: Option<&'a BStr>,
}

/// The definition of a BTOR2 node.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum NodeVariant<'a> {
    /// The definition of a sort.
    Sort(Sort),
    /// The definition of a value.
    Value(Value<'a>),
    /// The definition of a state assignment.
    Assignment(Assignment),
    /// The definition of an output.
    Output(Output<'a>),
}

/// A BTOR2 sort.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Sort {
    /// A bit-vector sort of a given positive width.
    BitVec(NonZeroU64),
    /// An array sort with a given domain and codomain.
    Array(Array),
}

impl Sort {
    /// Returns a bit-vector sort of the given width.
    ///
    /// Panics when the given width is zero.
    pub fn bit_vec(width: u64) -> Self {
        Self::BitVec(NonZeroU64::new(width).unwrap())
    }
}

/// An array sort with a given domain and codomain.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Array(pub NodeId, pub NodeId);

/// A BTOR2 Value with of a known sort.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Value<'a> {
    /// The sort of the value.
    pub sort: NodeId,
    /// The definition of the value.
    pub variant: ValueVariant<'a>,
}

/// A BTOR2 Value.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ValueVariant<'a> {
    /// A constant value.
    Const(Const<'a>),
    /// An input value.
    Input,
    /// A state value.
    State,
    /// A value defined by an operation.
    Op(Op),
}

/// A value defined by an operation.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Op {
    /// A value resulting from applying an unary operation to a previously defined value.
    Unary(UnaryOp, NodeId),
    /// A value resulting from applying a binary operation to two previously defined values.
    Binary(BinaryOp, [NodeId; 2]),
    /// A value resulting from applying a ternary operation to three previously defined values.
    Ternary(TernaryOp, [NodeId; 3]),
}

/// A unary BTOR2 operation.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum UnaryOp {
    /// Unsigned width extension.
    Uext(u64),
    /// Signed width extension.
    Sext(u64),
    /// Bit-vector slicing.
    Slice(u64, u64),
    /// Bitwise negation.
    Not,
    /// Addition of constant 1.
    Inc,
    /// Subtraction of constant 1.
    Dec,
    /// Arithmetic negation.
    Neg,
    /// And-reduction.
    Redand,
    /// Or-reduction.
    Redor,
    /// Xor-reduction.
    Redxor,
}

/// A binary BTOR2 operation.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum BinaryOp {
    /// Boolean biimplication.
    Iff,
    /// Boolean implicaqtion.
    Implies,
    /// Equality.
    Eq,
    /// Inequality.
    Neq,
    /// Unsigned greater-than comparison.
    Ugt,
    /// Signed greater-than comparison.
    Sgt,
    /// Unsigned greater-than-or-equal-to comparison.
    Ugte,
    /// Signed greater-than-or-equal-to comparison.
    Sgte,
    /// Unsigned less-than comparison.
    Ult,
    /// Signed less-than comparison.
    Slt,
    /// Unsigned less-than-or-equal-to comparison.
    Ulte,
    /// Signed less-than-or-equal-to comparison.
    Slte,
    /// Bit-wise and.
    And,
    /// Bit-wise not-and.
    Nand,
    /// Bit-wise not-or.
    Nor,
    /// Bit-wise or.
    Or,
    /// Bit-wise not-xor.
    Xnor,
    /// Bit-wise xor.
    Xor,
    /// Left rotation.
    Rol,
    /// right rotation.
    Ror,
    /// Left shift.
    Sll,
    /// Arithmetic right shift.
    Sra,
    /// Logical right shift.
    Srl,
    /// Addition.
    Add,
    /// Multiplication.
    Mul,
    /// Unsigned division.
    Udiv,
    /// Signed division.
    Sdiv,
    /// Signed modulo.
    Smod,
    /// Unsigned remainder.
    Urem,
    /// Signed remainder.
    Srem,
    /// Subtraction.
    Sub,
    /// Overflow check for unsigned addition.
    Uaddo,
    /// Overflow check for signed addition.
    Saddo,
    /// Overflow check for signed division.
    Sdivo,
    /// Overflow check for unsigned multiplication.
    Umulo,
    /// Overflow check for signed multiplication.
    Smulo,
    /// Overflow check for unsigned subtraction.
    Usubo,
    /// Overflow check for signed subtraction.
    Ssubo,
    /// Bit-vector concatenation.
    Concat,
    /// Array lookup.
    Read,
}

/// A ternary BTOR2 operation.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum TernaryOp {
    /// If-then-else.
    Ite,
    /// Array update.
    Write,
}

/// A BTOR2 constant.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Const<'a> {
    /// A constant represented in binary.
    Binary(BinaryConst<'a>),
    /// A constant represented in decimal.
    Decimal(DecimalConst<'a>),
    /// A constant represented in hexadecimal.
    Hex(HexConst<'a>),
    /// Constant one.
    One,
    /// Constant negative one, an all-ones bit-vector.
    Ones,
    /// Constant zero.
    Zero,
}

/// Error of a string to bit-vector constant representation conversion.
#[derive(Debug)]
pub enum InvalidConstError {
    /// The string contains a character that is not a valid digit for the target representation.
    InvalidDigit(char),
    /// The string is empty.
    Empty,
}

/// A string of binary digits.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct BinaryConst<'a>(pub(crate) &'a str);

impl std::fmt::Display for BinaryConst<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

impl<'a> TryFrom<&'a str> for BinaryConst<'a> {
    type Error = InvalidConstError;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        if value.is_empty() {
            return Err(InvalidConstError::Empty);
        }
        for c in value.chars() {
            if !matches!(c, '0' | '1') {
                return Err(InvalidConstError::InvalidDigit(c));
            }
        }
        Ok(BinaryConst(value))
    }
}

/// A string of hex digits.
///
/// This can contain lowercase and uppercase digits
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HexConst<'a>(pub(crate) &'a str);

impl std::fmt::Display for HexConst<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

impl<'a> TryFrom<&'a str> for HexConst<'a> {
    type Error = InvalidConstError;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        if value.is_empty() {
            return Err(InvalidConstError::Empty);
        }
        for c in value.chars() {
            if !c.is_ascii_hexdigit() {
                return Err(InvalidConstError::InvalidDigit(c));
            }
        }
        Ok(HexConst(value))
    }
}

/// A string of decimal digits, optionally prefixed with a minus sign.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct DecimalConst<'a>(pub(crate) &'a str);

impl std::fmt::Display for DecimalConst<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
    }
}

impl<'a> TryFrom<&'a str> for DecimalConst<'a> {
    type Error = InvalidConstError;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        if value.is_empty() {
            return Err(InvalidConstError::Empty);
        }
        let mut first = true;
        for c in value.chars() {
            if !(take(&mut first) && c == '-' || c.is_ascii_hexdigit()) {
                return Err(InvalidConstError::InvalidDigit(c));
            }
        }
        Ok(DecimalConst(value))
    }
}

/// Whether a state assignment is the initial assignment or the state update.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum AssignmentKind {
    /// The assignment assigns an initial state value.
    Init,
    /// The assignment assigns the next state value.
    Next,
}

/// A BTOR2 state assignemnt.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Assignment {
    /// The state to assign to.
    pub state: NodeId,
    /// The common sort of the state and assigned value.
    pub sort: NodeId,
    /// When to perform the assignment.
    pub kind: AssignmentKind,
    /// The value to assign.
    pub value: NodeId,
}

/// A BTOR2 output.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Output<'a> {
    /// An output consisting of a single value.
    SingleValue(SingleValueOutput),
    /// An output representing a negated liveness property.
    Justice(&'a [NodeId]),
}

/// The meaning of a single value output.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum SingleValueOutputKind {
    /// A ciruit output.
    Output,
    /// An output representing a bad state property.
    Bad,
    /// An output representing an invariant constraint.
    Constraint,
    /// An output representing a fairness constraint.
    Fair,
}

/// A BTOR2 output consisting of a single value.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct SingleValueOutput {
    /// The meaning of this output.
    pub kind: SingleValueOutputKind,
    /// The value to output.
    pub value: NodeId,
}

impl std::fmt::Display for Line<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut target = vec![];
        {
            let mut writer = DeferredWriter::from_write(&mut target);
            self.write_into_unterminated(&mut writer);
            writer.flush().unwrap();
        }
        std::fmt::Display::fmt(&String::from_utf8_lossy(&target), f)
    }
}

impl Line<'_> {
    /// Writes the BTOR2 line into a [`DeferredWriter`], includes the terminating newline.
    pub fn write_into(&self, target: &mut DeferredWriter) {
        self.write_into_unterminated(target);
        target.write_all_defer_err(b"\n");
    }

    /// Writes the BTOR2 line into a [`DeferredWriter`], omits the terminating newline.
    pub fn write_into_unterminated(&self, target: &mut DeferredWriter) {
        match self {
            Line::Comment(comment) => {
                target.write_all_defer_err(b";");
                target.write_all_defer_err(comment);
            }
            Line::Node(node) => {
                node.write_into(target);
            }
        }
    }
}

impl NodeId {
    fn write_into(&self, target: &mut DeferredWriter) {
        flussab::write::text::ascii_digits(target, self.0.get());
    }
}

impl Node<'_> {
    fn write_into(&self, target: &mut DeferredWriter) {
        self.id.write_into(target);
        target.write_all_defer_err(b" ");
        self.variant.write_into(target);
        if let Some(symbol) = self.symbol {
            target.write_all_defer_err(b" ");
            target.write_all_defer_err(symbol);
        }
        if let Some(comment) = self.comment {
            target.write_all_defer_err(b" ;");
            target.write_all_defer_err(comment);
        }
    }
}

impl NodeVariant<'_> {
    fn write_into(&self, target: &mut DeferredWriter) {
        match self {
            NodeVariant::Sort(sort) => {
                sort.write_into(target);
            }
            NodeVariant::Value(value) => {
                value.write_into(target);
            }
            NodeVariant::Assignment(assignment) => {
                assignment.write_into(target);
            }
            NodeVariant::Output(output) => {
                output.write_into(target);
            }
        }
    }
}

impl Sort {
    fn write_into(&self, target: &mut DeferredWriter) {
        match *self {
            Sort::BitVec(width) => {
                target.write_all_defer_err(b"sort bitvec ");
                flussab::write::text::ascii_digits(target, width.get());
            }
            Sort::Array(Array(domain, codomain)) => {
                target.write_all_defer_err(b"sort array ");
                domain.write_into(target);
                target.write_all_defer_err(b" ");
                codomain.write_into(target);
            }
        }
    }
}

impl Value<'_> {
    fn write_into(&self, target: &mut DeferredWriter) {
        match &self.variant {
            ValueVariant::Const(Const::Binary(const_value)) => {
                target.write_all_defer_err(b"const ");
                self.sort.write_into(target);
                target.write_all_defer_err(b" ");
                target.write_all_defer_err(const_value.0.as_bytes());
            }
            ValueVariant::Const(Const::Hex(const_value)) => {
                target.write_all_defer_err(b"consth ");
                self.sort.write_into(target);
                target.write_all_defer_err(b" ");
                target.write_all_defer_err(const_value.0.as_bytes());
            }
            ValueVariant::Const(Const::Decimal(const_value)) => {
                target.write_all_defer_err(b"constd ");
                self.sort.write_into(target);
                target.write_all_defer_err(b" ");
                target.write_all_defer_err(const_value.0.as_bytes());
            }
            ValueVariant::Const(Const::One) => {
                target.write_all_defer_err(b"one ");
                self.sort.write_into(target);
            }
            ValueVariant::Const(Const::Ones) => {
                target.write_all_defer_err(b"ones ");
                self.sort.write_into(target);
            }
            ValueVariant::Const(Const::Zero) => {
                target.write_all_defer_err(b"zero ");
                self.sort.write_into(target);
            }
            ValueVariant::Input => {
                target.write_all_defer_err(b"input ");
                self.sort.write_into(target);
            }
            ValueVariant::State => {
                target.write_all_defer_err(b"state ");
                self.sort.write_into(target);
            }
            &ValueVariant::Op(Op::Unary(op, a0)) => {
                target.write_all_defer_err(op.name().as_bytes());
                target.write_all_defer_err(b" ");
                self.sort.write_into(target);
                target.write_all_defer_err(b" ");
                a0.write_into(target);
                op.write_indices_into(target);
            }
            &ValueVariant::Op(Op::Binary(op, [a0, a1])) => {
                target.write_all_defer_err(op.name().as_bytes());
                target.write_all_defer_err(b" ");
                self.sort.write_into(target);
                target.write_all_defer_err(b" ");
                a0.write_into(target);
                target.write_all_defer_err(b" ");
                a1.write_into(target);
            }
            &ValueVariant::Op(Op::Ternary(op, [a0, a1, a2])) => {
                target.write_all_defer_err(op.name().as_bytes());
                target.write_all_defer_err(b" ");
                self.sort.write_into(target);
                target.write_all_defer_err(b" ");
                a0.write_into(target);
                target.write_all_defer_err(b" ");
                a1.write_into(target);
                target.write_all_defer_err(b" ");
                a2.write_into(target);
            }
        }
    }
}

impl UnaryOp {
    fn name(&self) -> &'static str {
        match self {
            UnaryOp::Uext(_) => "uext",
            UnaryOp::Sext(_) => "sext",
            UnaryOp::Slice(_, _) => "slice",
            UnaryOp::Not => "not",
            UnaryOp::Inc => "inc",
            UnaryOp::Dec => "dec",
            UnaryOp::Neg => "neg",
            UnaryOp::Redand => "redand",
            UnaryOp::Redor => "redor",
            UnaryOp::Redxor => "redxor",
        }
    }

    fn write_indices_into(&self, target: &mut DeferredWriter) {
        match self {
            &UnaryOp::Uext(width) | &UnaryOp::Sext(width) => {
                target.write_all_defer_err(b" ");
                flussab::write::text::ascii_digits(target, width);
            }
            &UnaryOp::Slice(upper, lower) => {
                target.write_all_defer_err(b" ");
                flussab::write::text::ascii_digits(target, upper);
                target.write_all_defer_err(b" ");
                flussab::write::text::ascii_digits(target, lower);
            }
            _ => (),
        }
    }
}

impl BinaryOp {
    fn name(&self) -> &'static str {
        match self {
            BinaryOp::Iff => "iff",
            BinaryOp::Implies => "implies",
            BinaryOp::Eq => "eq",
            BinaryOp::Neq => "neq",
            BinaryOp::Ugt => "ugt",
            BinaryOp::Sgt => "sgt",
            BinaryOp::Ugte => "ugte",
            BinaryOp::Sgte => "sgte",
            BinaryOp::Ult => "ult",
            BinaryOp::Slt => "slt",
            BinaryOp::Ulte => "ulte",
            BinaryOp::Slte => "slte",
            BinaryOp::And => "and",
            BinaryOp::Nand => "nand",
            BinaryOp::Nor => "nor",
            BinaryOp::Or => "or",
            BinaryOp::Xnor => "xnor",
            BinaryOp::Xor => "xor",
            BinaryOp::Rol => "rol",
            BinaryOp::Ror => "ror",
            BinaryOp::Sll => "sll",
            BinaryOp::Sra => "sra",
            BinaryOp::Srl => "srl",
            BinaryOp::Add => "add",
            BinaryOp::Mul => "mul",
            BinaryOp::Udiv => "udiv",
            BinaryOp::Sdiv => "sdiv",
            BinaryOp::Smod => "smod",
            BinaryOp::Urem => "urem",
            BinaryOp::Srem => "srem",
            BinaryOp::Sub => "sub",
            BinaryOp::Uaddo => "uaddo",
            BinaryOp::Saddo => "saddo",
            BinaryOp::Sdivo => "sdivo",
            BinaryOp::Umulo => "umulo",
            BinaryOp::Smulo => "smulo",
            BinaryOp::Usubo => "usubo",
            BinaryOp::Ssubo => "ssubo",
            BinaryOp::Concat => "concat",
            BinaryOp::Read => "read",
        }
    }
}

impl TernaryOp {
    fn name(&self) -> &'static str {
        match self {
            TernaryOp::Ite => "ite",
            TernaryOp::Write => "write",
        }
    }
}

impl Assignment {
    fn write_into(&self, target: &mut DeferredWriter) {
        match self.kind {
            AssignmentKind::Init => target.write_all_defer_err(b"init "),
            AssignmentKind::Next => target.write_all_defer_err(b"next "),
        }
        self.sort.write_into(target);
        target.write_all_defer_err(b" ");
        self.state.write_into(target);
        target.write_all_defer_err(b" ");
        self.value.write_into(target);
    }
}

impl Output<'_> {
    fn write_into(&self, target: &mut DeferredWriter) {
        match self {
            Output::SingleValue(simple) => simple.write_into(target),
            &Output::Justice(nodes) => {
                target.write_all_defer_err(b"justice ");
                flussab::write::text::ascii_digits(target, nodes.len());
                for node in nodes {
                    target.write_all_defer_err(b" ");
                    node.write_into(target);
                }
            }
        }
    }
}

impl SingleValueOutput {
    fn write_into(&self, target: &mut DeferredWriter) {
        match self.kind {
            SingleValueOutputKind::Output => target.write_all_defer_err(b"output "),
            SingleValueOutputKind::Bad => target.write_all_defer_err(b"bad "),
            SingleValueOutputKind::Constraint => target.write_all_defer_err(b"constraint "),
            SingleValueOutputKind::Fair => target.write_all_defer_err(b"fair "),
        }
        self.value.write_into(target);
    }
}
