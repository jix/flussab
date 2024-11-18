use std::{
    borrow::Borrow,
    io::{BufReader, Read},
};

use bstr::BString;
use flussab::{text::LineReader, DeferredReader, Parsed};

use crate::{
    btor2::{
        Array, Assignment, BinaryConst, Const, DecimalConst, HexConst, Line, Node, NodeId,
        NodeVariant, Op, Output, SingleValueOutput, Sort, UnaryOp, Value, ValueVariant,
    },
    error::ParseError,
    token::{self, NodeToken, NodeValueToken},
};

/// Configuration options for BTOR2 parsing.
#[derive(Default)]
#[non_exhaustive]
pub struct Config {}

/// A BTOR2 parser.
pub struct Parser<'a> {
    reader: LineReader<'a>,
    node_buf: Vec<NodeId>,
    const_buf: String,
    symbol_buf: BString,
}

impl<'a> Parser<'a> {
    /// Creates a parser reading from a [`LineReader`].
    pub fn new(reader: LineReader<'a>, config: Config) -> Result<Self, ParseError> {
        let Config {} = config;
        Ok(Self {
            reader,
            node_buf: Default::default(),
            const_buf: Default::default(),
            symbol_buf: Default::default(),
        })
    }

    /// Creates a parser reading from a [`BufReader`].
    pub fn from_buf_reader(
        buf_reader: BufReader<impl Read + 'a>,
        config: Config,
    ) -> Result<Self, ParseError> {
        Self::new(
            LineReader::new(DeferredReader::from_buf_reader(buf_reader)),
            config,
        )
    }

    /// Creates a parser reading from a [`Read`] instance.
    ///
    /// If the [`Read`] instance is a [`BufReader`], it is better to use
    /// [`from_buf_reader`][Self::from_buf_reader] to avoid unnecessary double buffering of the
    /// data.
    pub fn from_read(read: impl Read + 'a, config: Config) -> Result<Self, ParseError> {
        Self::new(LineReader::new(DeferredReader::from_read(read)), config)
    }

    /// Creates a parser reading from a boxed [`Read`] instance.
    ///
    /// If the [`Read`] instance is a [`BufReader`], it is better to use
    /// [`from_buf_reader`][Self::from_buf_reader] to avoid unnecessary double buffering of the
    /// data.
    #[inline(never)]
    pub fn from_boxed_dyn_read(
        read: Box<dyn Read + 'a>,
        config: Config,
    ) -> Result<Self, ParseError> {
        Self::new(
            LineReader::new(DeferredReader::from_boxed_dyn_read(read)),
            config,
        )
    }
    fn try_node(&mut self) -> Parsed<Node<'static>, ParseError> {
        token::node_id(&mut self.reader).and_then(|node_id| {
            token::required_space(&mut self.reader)?;
            let node_token = token::node_token(&mut self.reader)
                .or_give_up(|| token::unexpected(&mut self.reader, "node keyword"))?;
            let variant = match node_token {
                NodeToken::Sort => NodeVariant::Sort({
                    token::required_space(&mut self.reader)?;
                    match token::sort_token(&mut self.reader)
                        .or_give_up(|| token::unexpected(&mut self.reader, "sort keyword"))?
                    {
                        token::SortToken::Bitvec => {
                            token::required_space(&mut self.reader)?;
                            let width =
                                token::required_positive_int(&mut self.reader, "bit width")?;
                            Sort::BitVec(width)
                        }
                        token::SortToken::Array => {
                            token::required_space(&mut self.reader)?;
                            let domain = token::required_sort_id(&mut self.reader)?;
                            token::required_space(&mut self.reader)?;
                            let codomain = token::required_sort_id(&mut self.reader)?;

                            Sort::Array(Array(domain, codomain))
                        }
                    }
                }),
                NodeToken::Assignment(kind) => {
                    token::required_space(&mut self.reader)?;
                    let sort = token::required_sort_id(&mut self.reader)?;
                    token::required_space(&mut self.reader)?;
                    let state = token::required_node_id(&mut self.reader)?;
                    token::required_space(&mut self.reader)?;
                    let value = token::required_node_id(&mut self.reader)?;

                    NodeVariant::Assignment(Assignment {
                        state,
                        sort,
                        kind,
                        value,
                    })
                }
                NodeToken::Output(kind) => {
                    token::required_space(&mut self.reader)?;
                    let value = token::required_node_id(&mut self.reader)?;
                    NodeVariant::Output(Output::SingleValue(SingleValueOutput { kind, value }))
                }
                NodeToken::Justice => {
                    token::required_space(&mut self.reader)?;
                    let count =
                        token::required_positive_int(&mut self.reader, "condition count")?.get();
                    self.node_buf.clear();
                    for _ in 0..count {
                        token::required_space(&mut self.reader)?;
                        let condition = token::required_node_id(&mut self.reader)?;
                        self.node_buf.push(condition);
                    }
                    NodeVariant::Output(Output::Justice(&[]))
                }
                NodeToken::Value(value_token) => NodeVariant::Value({
                    token::required_space(&mut self.reader)?;
                    let sort = token::required_sort_id(&mut self.reader)?;

                    let variant = match value_token {
                        NodeValueToken::Const => {
                            token::required_space(&mut self.reader)?;
                            self.const_buf.clear();
                            self.const_buf
                                .push_str(token::required_binary_constant(&mut self.reader)?);

                            ValueVariant::Const(Const::Binary(BinaryConst("")))
                        }
                        NodeValueToken::Constd => {
                            token::required_space(&mut self.reader)?;
                            self.const_buf.clear();
                            self.const_buf
                                .push_str(token::required_decimal_constant(&mut self.reader)?);

                            ValueVariant::Const(Const::Decimal(DecimalConst("")))
                        }
                        NodeValueToken::Consth => {
                            token::required_space(&mut self.reader)?;
                            self.const_buf.clear();
                            self.const_buf
                                .push_str(token::required_hex_constant(&mut self.reader)?);

                            ValueVariant::Const(Const::Hex(HexConst("")))
                        }
                        NodeValueToken::Ones => ValueVariant::Const(Const::Ones),
                        NodeValueToken::One => ValueVariant::Const(Const::One),
                        NodeValueToken::Zero => ValueVariant::Const(Const::Zero),

                        NodeValueToken::Input => ValueVariant::Input,
                        NodeValueToken::State => ValueVariant::State,
                        NodeValueToken::ExtOp(ext_op_token) => {
                            token::required_space(&mut self.reader)?;
                            let a0 = token::required_node_id(&mut self.reader)?;
                            token::required_space(&mut self.reader)?;
                            let pad =
                                token::required_nonnegative_int(&mut self.reader, "pad width")?;

                            ValueVariant::Op(Op::Unary(ext_op_token.unary_op(pad), a0))
                        }
                        NodeValueToken::Slice => {
                            token::required_space(&mut self.reader)?;
                            let a0 = token::required_node_id(&mut self.reader)?;
                            token::required_space(&mut self.reader)?;
                            let u = token::required_nonnegative_int(&mut self.reader, "bit index")?;
                            token::required_space(&mut self.reader)?;
                            let l = token::required_nonnegative_int(&mut self.reader, "bit index")?;

                            ValueVariant::Op(Op::Unary(UnaryOp::Slice(u, l), a0))
                        }

                        NodeValueToken::UnaryOp(unary_op_token) => {
                            token::required_space(&mut self.reader)?;
                            let a0 = token::required_node_id(&mut self.reader)?;

                            ValueVariant::Op(Op::Unary(unary_op_token.unary_op(), a0))
                        }

                        NodeValueToken::BinaryOp(binary_op) => {
                            token::required_space(&mut self.reader)?;
                            let a0 = token::required_node_id(&mut self.reader)?;
                            token::required_space(&mut self.reader)?;
                            let a1 = token::required_node_id(&mut self.reader)?;
                            ValueVariant::Op(Op::Binary(binary_op, [a0, a1]))
                        }

                        NodeValueToken::TernaryOp(ternary_op) => {
                            token::required_space(&mut self.reader)?;
                            let a0 = token::required_node_id(&mut self.reader)?;
                            token::required_space(&mut self.reader)?;
                            let a1 = token::required_node_id(&mut self.reader)?;
                            token::required_space(&mut self.reader)?;
                            let a2 = token::required_node_id(&mut self.reader)?;
                            ValueVariant::Op(Op::Ternary(ternary_op, [a0, a1, a2]))
                        }
                    };

                    Value { sort, variant }
                }),
            };

            let (symbol, comment) = token::space(&mut self.reader)
                .and_then(|_| {
                    token::comment_start(&mut self.reader)
                        .map(|_| (None, Some("".into())))
                        .or_parse(|| {
                            token::symbol_name(&mut self.reader)
                                .map(|symbol| {
                                    self.symbol_buf.clear();
                                    self.symbol_buf.extend_from_slice(symbol);
                                    Some("".into())
                                })
                                .and_then(|symbol| {
                                    token::space(&mut self.reader)
                                        .and_then(|_| {
                                            token::comment_start(&mut self.reader)
                                                .map(|_| (symbol, Some("".into())))
                                                .or_give_up(|| {
                                                    token::unexpected(&mut self.reader, "comment")
                                                })
                                        })
                                        .or_parse(|| {
                                            token::newline(&mut self.reader)
                                                .and_then(|_| Ok((symbol, None)))
                                        })
                                        .or_give_up(|| {
                                            token::unexpected(
                                                &mut self.reader,
                                                concat!(
                                                    "end of line or ",
                                                    "a space character followed by a comment"
                                                ),
                                            )
                                        })
                                })
                        })
                        .or_give_up(|| {
                            token::unexpected(&mut self.reader, "symbol name or comment")
                        })
                })
                .or_parse(|| token::newline(&mut self.reader).and_then(|_| Ok((None, None))))
                .or_give_up(|| {
                    token::unexpected(
                        &mut self.reader,
                        "end of line, a space character followed by a symbol name or comment",
                    )
                })?;

            Ok(Node {
                id: node_id,
                variant,
                symbol,
                comment,
            })
        })
    }

    fn try_comment(&mut self) -> Parsed<(), ParseError> {
        token::comment_start(&mut self.reader)
    }

    /// Parses the next line of the BTOR2 file.
    ///
    /// Returns `Ok(None)` when reaching the end of the file.
    pub fn next_line(&mut self) -> Result<Option<Line>, ParseError> {
        token::skip_whitespace(&mut self.reader);

        let Some(mut node) = self
            .try_node()
            .map(Line::Node)
            .or_parse(|| self.try_comment().map(|()| Line::Comment("".into())))
            .map(Some)
            .or_parse(|| token::eof(&mut self.reader).map(|_| None))
            .or_give_up(|| token::unexpected(&mut self.reader, "btor2 line"))?
        else {
            self.reader.reader.check_io_error()?;
            return Ok(None);
        };

        if node.has_comment() {
            node.update_comment(token::comment_body(&mut self.reader));
        }

        node.update_bufs(&self.const_buf, self.symbol_buf.borrow(), &self.node_buf);

        Ok(Some(node))
    }
}
