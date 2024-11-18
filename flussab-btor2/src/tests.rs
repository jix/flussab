use core::str;
use std::io::Write;

use flussab::DeferredWriter;

use crate::btor2::{
    Array, Assignment, AssignmentKind, BinaryConst, BinaryOp, Const, DecimalConst, HexConst, Line,
    Node, NodeId, NodeVariant, Op, Output, SingleValueOutput, SingleValueOutputKind, Sort,
    TernaryOp, UnaryOp, Value, ValueVariant,
};
use crate::{Config, Parser};

#[test]
fn test_everything() {
    let mut lines = vec![];
    let lines = &mut lines;

    let line = |lines: &mut Vec<_>, v| lines.push(v);
    let node = |lines: &mut Vec<_>, id: u64, variant, symbol, comment| {
        line(
            lines,
            Line::Node(Node {
                id: NodeId::new(id),
                variant,
                symbol,
                comment,
            }),
        )
    };
    let sort =
        |lines: &mut Vec<_>, id: u64, sort| node(lines, id, NodeVariant::Sort(sort), None, None);
    let val = |lines: &mut Vec<_>, id: u64, sort: u64, variant| {
        node(
            lines,
            id,
            NodeVariant::Value(Value {
                sort: NodeId::new(sort),
                variant,
            }),
            None,
            None,
        )
    };
    let cons = |lines: &mut Vec<_>, id: u64, sort: u64, constant| {
        val(lines, id, sort, ValueVariant::Const(constant))
    };
    let op1 = |lines: &mut Vec<_>, id: u64, sort: u64, op, a0| {
        val(
            lines,
            id,
            sort,
            ValueVariant::Op(Op::Unary(op, NodeId::new(a0))),
        )
    };
    let op2 = |lines: &mut Vec<_>, id: u64, sort: u64, op, a0, a1| {
        val(
            lines,
            id,
            sort,
            ValueVariant::Op(Op::Binary(op, [a0, a1].map(NodeId::new))),
        )
    };
    let op3 = |lines: &mut Vec<_>, id: u64, sort: u64, op, a0, a1, a2| {
        val(
            lines,
            id,
            sort,
            ValueVariant::Op(Op::Ternary(op, [a0, a1, a2].map(NodeId::new))),
        )
    };
    let assign = |lines: &mut Vec<_>, id: u64, sort: u64, state: u64, value: u64, kind| {
        node(
            lines,
            id,
            NodeVariant::Assignment(Assignment {
                state: NodeId::new(state),
                sort: NodeId::new(sort),
                kind,
                value: NodeId::new(value),
            }),
            None,
            None,
        )
    };
    let output = |lines: &mut Vec<_>, id: u64, value: u64, kind| {
        node(
            lines,
            id,
            NodeVariant::Output(Output::SingleValue(SingleValueOutput {
                kind,
                value: NodeId::new(value),
            })),
            None,
            None,
        )
    };

    line(lines, Line::Comment("a comment".into()));
    sort(lines, 1, Sort::bit_vec(1));
    sort(lines, 2, Sort::bit_vec(10));
    sort(lines, 3, Sort::Array(Array(NodeId::new(1), NodeId::new(2))));
    cons(lines, 4, 2, Const::Binary(BinaryConst("0101010101")));
    cons(lines, 5, 2, Const::Decimal(DecimalConst("19")));
    cons(lines, 6, 2, Const::Decimal(DecimalConst("019")));
    cons(lines, 7, 2, Const::Decimal(DecimalConst("-90")));
    cons(lines, 8, 2, Const::Decimal(DecimalConst("-")));
    cons(lines, 9, 2, Const::Hex(HexConst::try_from("fF").unwrap()));
    cons(lines, 10, 1, Const::One);
    cons(lines, 11, 2, Const::Ones);
    cons(lines, 12, 1, Const::Zero);
    val(lines, 13, 2, ValueVariant::Input);
    val(lines, 14, 3, ValueVariant::State);
    op1(lines, 15, 2, UnaryOp::Sext(9), 10);
    op1(lines, 16, 2, UnaryOp::Uext(9), 10);
    op1(lines, 17, 1, UnaryOp::Slice(0, 0), 13);
    op1(lines, 18, 2, UnaryOp::Not, 13);
    op1(lines, 19, 2, UnaryOp::Inc, 13);
    op1(lines, 20, 2, UnaryOp::Dec, 13);
    op1(lines, 21, 2, UnaryOp::Neg, 13);
    op1(lines, 22, 1, UnaryOp::Redand, 13);
    op1(lines, 23, 1, UnaryOp::Redor, 13);
    op1(lines, 24, 1, UnaryOp::Redxor, 13);
    op2(lines, 25, 1, BinaryOp::Iff, 23, 24);
    op2(lines, 26, 1, BinaryOp::Implies, 23, 24);
    op2(lines, 27, 1, BinaryOp::Eq, 14, 14);
    op2(lines, 28, 1, BinaryOp::Neq, 14, 14);
    op2(lines, 29, 1, BinaryOp::Sgt, 13, 13);
    op2(lines, 30, 1, BinaryOp::Ugte, 13, 13);
    op2(lines, 31, 1, BinaryOp::Sgte, 13, 13);
    op2(lines, 32, 1, BinaryOp::Ult, 13, 13);
    op2(lines, 33, 1, BinaryOp::Slt, 13, 13);
    op2(lines, 34, 1, BinaryOp::Ulte, 13, 13);
    op2(lines, 35, 1, BinaryOp::Slte, 13, 13);
    op2(lines, 36, 2, BinaryOp::And, 13, 13);
    op2(lines, 37, 2, BinaryOp::Nand, 13, 13);
    op2(lines, 38, 2, BinaryOp::Nor, 13, 13);
    op2(lines, 39, 2, BinaryOp::Or, 13, 13);
    op2(lines, 40, 2, BinaryOp::Xnor, 13, 13);
    op2(lines, 41, 2, BinaryOp::Xor, 13, 13);
    op2(lines, 42, 2, BinaryOp::Rol, 13, 13);
    op2(lines, 43, 2, BinaryOp::Ror, 13, 13);
    op2(lines, 44, 2, BinaryOp::Sll, 13, 13);
    op2(lines, 45, 2, BinaryOp::Sra, 13, 13);
    op2(lines, 46, 2, BinaryOp::Srl, 13, 13);
    op2(lines, 47, 2, BinaryOp::Add, 13, 13);
    op2(lines, 48, 2, BinaryOp::Mul, 13, 13);
    op2(lines, 49, 2, BinaryOp::Udiv, 13, 13);
    op2(lines, 50, 2, BinaryOp::Sdiv, 13, 13);
    op2(lines, 51, 2, BinaryOp::Smod, 13, 13);
    op2(lines, 52, 2, BinaryOp::Urem, 13, 13);
    op2(lines, 53, 2, BinaryOp::Srem, 13, 13);
    op2(lines, 54, 2, BinaryOp::Sub, 13, 13);
    op2(lines, 55, 1, BinaryOp::Uaddo, 13, 13);
    op2(lines, 56, 1, BinaryOp::Saddo, 13, 13);
    op2(lines, 57, 1, BinaryOp::Sdivo, 13, 13);
    op2(lines, 58, 1, BinaryOp::Umulo, 13, 13);
    op2(lines, 59, 1, BinaryOp::Smulo, 13, 13);
    op2(lines, 60, 1, BinaryOp::Usubo, 13, 13);
    op2(lines, 61, 1, BinaryOp::Ssubo, 13, 13);
    sort(lines, 62, Sort::bit_vec(2));
    op2(lines, 63, 62, BinaryOp::Concat, 27, 28);
    op2(lines, 64, 2, BinaryOp::Read, 14, 17);
    op3(lines, 65, 3, TernaryOp::Write, 14, 17, 13);
    op3(lines, 66, 2, TernaryOp::Ite, 17, 13, 40);
    assign(lines, 67, 3, 14, 65, AssignmentKind::Next);
    val(lines, 68, 2, ValueVariant::State);
    assign(lines, 69, 2, 68, 11, AssignmentKind::Init);
    output(lines, 70, 64, SingleValueOutputKind::Output);
    output(lines, 71, 55, SingleValueOutputKind::Bad);
    output(lines, 72, 55, SingleValueOutputKind::Constraint);
    output(lines, 73, 55, SingleValueOutputKind::Fair);
    let justice = [55, 56, 57].map(NodeId::new);
    node(
        lines,
        74,
        NodeVariant::Output(Output::Justice(&justice)),
        Some("justice".into()),
        Some("justice property".into()),
    );

    let mut buf = vec![];
    {
        let mut writer = DeferredWriter::from_write(&mut buf);
        for line in &*lines {
            line.write_into(&mut writer);
        }
        writer.flush().unwrap();
    }

    println!("{}", str::from_utf8(&buf).unwrap());

    let mut parser = Parser::from_read(buf.as_slice(), Config::default()).unwrap();

    for line in &*lines {
        assert_eq!(parser.next_line().unwrap(), Some(*line));
    }
    assert!(parser.next_line().unwrap().is_none());
}
