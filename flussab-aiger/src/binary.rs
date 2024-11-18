use std::{
    borrow::Cow,
    io::{BufReader, Read},
    marker::PhantomData,
};

use flussab::{text::LineReader, DeferredReader, DeferredWriter, Parsed::Fallthrough};

use crate::{
    aig::{OrderedAig, OrderedAndGate, OrderedLatch, Symbol, SymbolTarget},
    token::{self, eof, unexpected},
    Lit, ParseError,
};

#[derive(Default)]
#[non_exhaustive]
pub struct Config {}

#[derive(Clone, Debug)]
pub struct Header {
    pub max_var_index: usize,
    pub input_count: usize,
    pub latch_count: usize,
    pub output_count: usize,
    pub and_gate_count: usize,
    pub bad_state_property_count: usize,
    pub invariant_constraint_count: usize,
    pub justice_property_count: usize,
    pub fairness_constraint_count: usize,
}

impl Header {
    fn parse<L: Lit>(reader: &mut LineReader) -> Result<Self, ParseError> {
        token::fixed(reader, b"aig")
            .or_give_up(|| token::unexpected(reader, "binary AIGER header \"aig\""))?;

        token::required_space(reader)?;
        let max_var_index = token::header_field(
            reader,
            "maximum variable index",
            (L::MAX_CODE - 1) / 2,
            true,
        )?;

        let mut limit = max_var_index;

        token::required_space(reader)?;
        let input_count = token::header_field(reader, "input count", limit, false)?;

        limit -= input_count;

        token::required_space(reader)?;
        let latch_count = token::header_field(reader, "latch count", limit, false)?;

        limit -= latch_count;

        token::required_space(reader)?;
        let output_count = token::header_field(reader, "output count", usize::MAX, true)?;

        token::required_space(reader)?;
        let and_gate_count = token::header_field(reader, "and gate count", limit, false)?;

        let mut bad_state_property_count = 0;
        let mut invariant_constraint_count = 0;
        let mut justice_property_count = 0;
        let mut fairness_constraint_count = 0;

        #[allow(clippy::never_loop)]
        loop {
            if !token::required_newline_or_space(reader)? {
                break;
            }

            bad_state_property_count =
                token::header_field(reader, "bad state property count", limit, false)?;

            if !token::required_newline_or_space(reader)? {
                break;
            }
            invariant_constraint_count =
                token::header_field(reader, "invariant constraint count", limit, false)?;

            if !token::required_newline_or_space(reader)? {
                break;
            }
            justice_property_count =
                token::header_field(reader, "justice property count", limit, false)?;

            if !token::required_newline_or_space(reader)? {
                break;
            }
            fairness_constraint_count =
                token::header_field(reader, "fairness constraint count", limit, false)?;

            token::required_newline(reader)?;
            break;
        }

        Ok(Header {
            max_var_index,
            input_count,
            latch_count,
            output_count,
            and_gate_count,
            bad_state_property_count,
            invariant_constraint_count,
            justice_property_count,
            fairness_constraint_count,
        })
    }
}

/// Parser for the ASCII version of the AIGER file format.
pub struct Parser<'a, L> {
    reader: LineReader<'a>,
    header: Header,
    max_lit: usize,
    code: usize,
    _lit_builder: std::marker::PhantomData<L>,
}

impl<'a, L> Parser<'a, L>
where
    L: Lit,
{
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

    /// Creates a parser reading from a [`LineReader`].
    pub fn new(mut reader: LineReader<'a>, _config: Config) -> Result<Self, ParseError> {
        let header = Header::parse::<L>(&mut reader)?;

        Ok(Self {
            reader,
            max_lit: header.max_var_index * 2 + 1,
            code: (header.input_count + 1) * 2,
            header,
            _lit_builder: std::marker::PhantomData,
        })
    }

    pub fn latches(self) -> Result<ParseLatches<'a, L>, ParseError> {
        Ok(ParseLatches {
            latches_left: self.header.latch_count,
            parser: self,
        })
    }

    pub fn header(&self) -> &Header {
        &self.header
    }

    pub fn parse(self) -> Result<OrderedAig<L>, ParseError> {
        let mut aig = OrderedAig {
            max_var_index: self.header.max_var_index,
            input_count: self.header.input_count,
            ..OrderedAig::default()
        };

        aig.latches.reserve(self.header.latch_count);
        aig.outputs.reserve(self.header.output_count);
        aig.bad_state_properties
            .reserve(self.header.bad_state_property_count);
        aig.invariant_constraints
            .reserve(self.header.invariant_constraint_count);
        aig.justice_properties = (0..self.header.justice_property_count)
            .map(|_| vec![])
            .collect();
        aig.fairness_constraints
            .reserve(self.header.fairness_constraint_count);
        aig.and_gates.reserve(self.header.and_gate_count);

        let justice_property_count = self.header.justice_property_count;

        let mut aag_reader = self.latches()?;
        while let Some(latch) = aag_reader.next_latch()? {
            aig.latches.push(latch);
        }

        let mut aag_reader = aag_reader.outputs()?;
        while let Some(output) = aag_reader.next_output()? {
            aig.outputs.push(output);
        }

        let mut aag_reader = aag_reader.bad_state_properties()?;
        while let Some(bad_state_property) = aag_reader.next_bad_state_property()? {
            aig.bad_state_properties.push(bad_state_property);
        }

        let mut aag_reader = aag_reader.invariant_constraints()?;
        while let Some(invariant_constraint) = aag_reader.next_invariant_constraint()? {
            aig.invariant_constraints.push(invariant_constraint);
        }

        let mut justice_property_sizes = Vec::with_capacity(justice_property_count);

        let mut aag_reader = aag_reader.justice_properties()?;
        while let Some(justice_property_size) = aag_reader.next_justice_property_size()? {
            justice_property_sizes.push(justice_property_size);
        }

        let mut justice_property = 0;

        let mut aag_reader = aag_reader.justice_property_local_fairness_constraints()?;
        while let Some(justice_property_local_fairness_constraint) =
            aag_reader.next_justice_property_local_fairness_constraint()?
        {
            while aig.justice_properties[justice_property].len()
                == justice_property_sizes[justice_property]
            {
                justice_property += 1;
            }
            aig.justice_properties[justice_property]
                .push(justice_property_local_fairness_constraint);
        }

        let mut aag_reader = aag_reader.fairness_constraints()?;
        while let Some(fairness_constraint) = aag_reader.next_fairness_constraint()? {
            aig.fairness_constraints.push(fairness_constraint);
        }

        let mut aag_reader = aag_reader.and_gates()?;
        while let Some(and_gate) = aag_reader.next_and_gate()? {
            aig.and_gates.push(and_gate);
        }

        let mut aag_reader = aag_reader.symbols()?;
        while let Some(symbol) = aag_reader.next_symbol()? {
            aig.symbols.push(symbol.into_owned_name());
        }

        if let Some(comment) = aag_reader.comment()? {
            aig.comment = Some(comment.to_owned());
        }

        Ok(aig)
    }
}

pub struct ParseInputs<'a, L> {
    parser: Parser<'a, L>,
    inputs_left: usize,
}

impl<'a, L> std::ops::Deref for ParseInputs<'a, L> {
    type Target = Parser<'a, L>;

    fn deref(&self) -> &Self::Target {
        &self.parser
    }
}

impl<'a, L> ParseInputs<'a, L>
where
    L: Lit,
{
    pub fn next_input(&mut self) -> Result<Option<L>, ParseError> {
        if self.inputs_left == 0 {
            return Ok(None);
        }
        self.inputs_left -= 1;
        let lit = L::from_code(token::lit(
            &mut self.parser.reader,
            "input literal",
            self.parser.max_lit,
            true,
        )?);
        token::required_newline(&mut self.parser.reader)?;
        Ok(Some(lit))
    }

    pub fn latches(mut self) -> Result<ParseLatches<'a, L>, ParseError> {
        while self.inputs_left != 0 {
            self.next_input()?;
        }

        Ok(ParseLatches {
            latches_left: self.parser.header.latch_count,
            parser: self.parser,
        })
    }
}
pub struct ParseLatches<'a, L> {
    parser: Parser<'a, L>,
    latches_left: usize,
}

impl<'a, L> ParseLatches<'a, L>
where
    L: Lit,
{
    pub fn next_latch(&mut self) -> Result<Option<OrderedLatch<L>>, ParseError> {
        if self.latches_left == 0 {
            return Ok(None);
        }
        self.latches_left -= 1;

        let next_state = L::from_code(token::lit(
            &mut self.parser.reader,
            "latch next state literal",
            self.parser.max_lit,
            false,
        )?);

        let mut initialization = Some(false);

        if token::required_newline_or_space(&mut self.parser.reader)? {
            let initialization_code = token::lit(
                &mut self.parser.reader,
                "latch initialization literal",
                self.parser.max_lit,
                false,
            )?;

            if initialization_code < 2 {
                initialization = Some(initialization_code != 0);
            } else if initialization_code == self.parser.code {
                initialization = None;
            } else {
                return Err(token::invalid_initialization(
                    &mut self.parser.reader,
                    initialization_code,
                    self.parser.code,
                ));
            }

            token::required_newline(&mut self.parser.reader)?;
        }
        self.parser.code += 2;
        Ok(Some(OrderedLatch {
            next_state,
            initialization,
        }))
    }

    pub fn outputs(mut self) -> Result<ParseOutputs<'a, L>, ParseError> {
        while self.latches_left != 0 {
            self.next_latch()?;
        }

        Ok(ParseOutputs {
            outputs_left: self.parser.header.output_count,
            parser: self.parser,
        })
    }
}

pub struct ParseOutputs<'a, L> {
    parser: Parser<'a, L>,
    outputs_left: usize,
}

impl<'a, L> ParseOutputs<'a, L>
where
    L: Lit,
{
    pub fn next_output(&mut self) -> Result<Option<L>, ParseError> {
        if self.outputs_left == 0 {
            return Ok(None);
        }
        self.outputs_left -= 1;
        let lit = L::from_code(token::lit(
            &mut self.parser.reader,
            "output literal",
            self.parser.max_lit,
            false,
        )?);
        token::required_newline(&mut self.parser.reader)?;
        Ok(Some(lit))
    }

    pub fn bad_state_properties(mut self) -> Result<ParseBadStateProperties<'a, L>, ParseError> {
        while self.outputs_left != 0 {
            self.next_output()?;
        }

        Ok(ParseBadStateProperties {
            bad_left: self.parser.header.bad_state_property_count,
            parser: self.parser,
        })
    }
}

pub struct ParseBadStateProperties<'a, L> {
    parser: Parser<'a, L>,
    bad_left: usize,
}

impl<'a, L> ParseBadStateProperties<'a, L>
where
    L: Lit,
{
    pub fn next_bad_state_property(&mut self) -> Result<Option<L>, ParseError> {
        if self.bad_left == 0 {
            return Ok(None);
        }
        self.bad_left -= 1;
        let lit = L::from_code(token::lit(
            &mut self.parser.reader,
            "bad state property literal",
            self.parser.max_lit,
            false,
        )?);
        token::required_newline(&mut self.parser.reader)?;
        Ok(Some(lit))
    }

    pub fn invariant_constraints(mut self) -> Result<ParseInvariantConstraints<'a, L>, ParseError> {
        while self.bad_left != 0 {
            self.next_bad_state_property()?;
        }

        Ok(ParseInvariantConstraints {
            constraints_left: self.parser.header.invariant_constraint_count,
            parser: self.parser,
        })
    }
}

pub struct ParseInvariantConstraints<'a, L> {
    parser: Parser<'a, L>,
    constraints_left: usize,
}

impl<'a, L> ParseInvariantConstraints<'a, L>
where
    L: Lit,
{
    pub fn next_invariant_constraint(&mut self) -> Result<Option<L>, ParseError> {
        if self.constraints_left == 0 {
            return Ok(None);
        }
        self.constraints_left -= 1;
        let lit = L::from_code(token::lit(
            &mut self.parser.reader,
            "invariant constraint literal",
            self.parser.max_lit,
            false,
        )?);
        token::required_newline(&mut self.parser.reader)?;
        Ok(Some(lit))
    }

    pub fn justice_properties(mut self) -> Result<ParseJusticePropertySizes<'a, L>, ParseError> {
        while self.constraints_left != 0 {
            self.next_invariant_constraint()?;
        }

        Ok(ParseJusticePropertySizes {
            justice_left: self.parser.header.justice_property_count,
            parser: self.parser,
            total_local_fairness_count: 0,
        })
    }
}

pub struct ParseJusticePropertySizes<'a, L: Lit> {
    parser: Parser<'a, L>,
    justice_left: usize,
    total_local_fairness_count: usize,
}

impl<'a, L> ParseJusticePropertySizes<'a, L>
where
    L: Lit,
{
    pub fn next_justice_property_size(&mut self) -> Result<Option<usize>, ParseError> {
        if self.justice_left == 0 {
            return Ok(None);
        }
        self.justice_left -= 1;
        let count = token::header_field(
            &mut self.parser.reader,
            "justice property local fairness constraint count",
            usize::MAX - self.total_local_fairness_count,
            true,
        )?;
        token::required_newline(&mut self.parser.reader)?;

        self.total_local_fairness_count += count;

        Ok(Some(count))
    }

    pub fn justice_property_local_fairness_constraints(
        mut self,
    ) -> Result<ParseJusticePropertyLocalFairnessConstraints<'a, L>, ParseError> {
        while self.justice_left != 0 {
            self.next_justice_property_size()?;
        }

        Ok(ParseJusticePropertyLocalFairnessConstraints {
            local_fairness_left: self.total_local_fairness_count,
            parser: self.parser,
        })
    }
}

pub struct ParseJusticePropertyLocalFairnessConstraints<'a, L: Lit> {
    parser: Parser<'a, L>,
    local_fairness_left: usize,
}

impl<'a, L> ParseJusticePropertyLocalFairnessConstraints<'a, L>
where
    L: Lit,
{
    pub fn next_justice_property_local_fairness_constraint(
        &mut self,
    ) -> Result<Option<L>, ParseError> {
        if self.local_fairness_left == 0 {
            return Ok(None);
        }
        self.local_fairness_left -= 1;
        let lit = L::from_code(token::lit(
            &mut self.parser.reader,
            "justice property local fairness constraint literal",
            self.parser.max_lit,
            false,
        )?);
        token::required_newline(&mut self.parser.reader)?;

        Ok(Some(lit))
    }

    pub fn fairness_constraints(mut self) -> Result<ParseFairnessConstraints<'a, L>, ParseError> {
        while self.local_fairness_left != 0 {
            self.next_justice_property_local_fairness_constraint()?;
        }

        Ok(ParseFairnessConstraints {
            fairness_left: self.parser.header.fairness_constraint_count,
            parser: self.parser,
        })
    }
}

pub struct ParseFairnessConstraints<'a, L> {
    parser: Parser<'a, L>,
    fairness_left: usize,
}

impl<'a, L> ParseFairnessConstraints<'a, L>
where
    L: Lit,
{
    pub fn next_fairness_constraint(&mut self) -> Result<Option<L>, ParseError> {
        if self.fairness_left == 0 {
            return Ok(None);
        }
        self.fairness_left -= 1;
        let lit = L::from_code(token::lit(
            &mut self.parser.reader,
            "fairness constraint literal",
            self.parser.max_lit,
            false,
        )?);
        token::required_newline(&mut self.parser.reader)?;
        Ok(Some(lit))
    }

    pub fn and_gates(mut self) -> Result<ParseAndGates<'a, L>, ParseError> {
        while self.fairness_left != 0 {
            self.next_fairness_constraint()?;
        }

        Ok(ParseAndGates {
            ands_left: self.parser.header.and_gate_count,
            parser: self.parser,
        })
    }
}

pub struct ParseAndGates<'a, L> {
    parser: Parser<'a, L>,
    ands_left: usize,
}

impl<'a, L> ParseAndGates<'a, L>
where
    L: Lit,
{
    pub fn next_and_gate(&mut self) -> Result<Option<OrderedAndGate<L>>, ParseError> {
        if self.ands_left == 0 {
            return Ok(None);
        }

        self.ands_left -= 1;

        let output_code = self.parser.code;

        let input_code_0: usize = token::delta_code(
            &mut self.parser.reader,
            output_code,
            "first input",
            "output code",
        )?;

        let input_code_1: usize = token::delta_code(
            &mut self.parser.reader,
            input_code_0,
            "second input",
            "first input code",
        )?;

        self.parser.code += 2;
        Ok(Some(OrderedAndGate {
            inputs: [L::from_code(input_code_0), L::from_code(input_code_1)],
        }))
    }

    pub fn symbols(mut self) -> Result<ParseSymbols<'a, L>, ParseError> {
        while self.ands_left != 0 {
            self.next_and_gate()?;
        }

        Ok(ParseSymbols {
            parser: self.parser,
        })
    }
}

pub struct ParseSymbols<'a, L> {
    parser: Parser<'a, L>,
}

impl<L> ParseSymbols<'_, L>
where
    L: Lit,
{
    pub fn next_symbol(&mut self) -> Result<Option<Symbol>, ParseError> {
        let input = &mut self.parser.reader;
        let target = if self.parser.header.input_count > 0 {
            token::fixed(input, b"i")
        } else {
            Fallthrough
        }
        .and_then(|_| {
            token::symbol_index(input, "input index", self.parser.header.input_count - 1)
                .map(SymbolTarget::Input)
        })
        .or_parse(|| {
            if self.parser.header.output_count > 0 {
                token::fixed(input, b"o")
            } else {
                Fallthrough
            }
            .and_then(|_| {
                token::symbol_index(input, "output index", self.parser.header.output_count - 1)
                    .map(SymbolTarget::Output)
            })
        })
        .or_parse(|| {
            if self.parser.header.latch_count > 0 {
                token::fixed(input, b"l")
            } else {
                Fallthrough
            }
            .and_then(|_| {
                token::symbol_index(input, "latch index", self.parser.header.latch_count - 1)
                    .map(SymbolTarget::Latch)
            })
        })
        .or_parse(|| {
            if self.parser.header.bad_state_property_count > 0 {
                token::fixed(input, b"b")
            } else {
                Fallthrough
            }
            .and_then(|_| {
                token::symbol_index(
                    input,
                    "bad state property index",
                    self.parser.header.latch_count - 1,
                )
                .map(SymbolTarget::BadStateProperty)
            })
        })
        .or_parse(|| {
            if self.parser.header.invariant_constraint_count > 0 {
                token::fixed_not_eol(input, b"c")
            } else {
                Fallthrough
            }
            .and_then(|_| {
                token::symbol_index(
                    input,
                    "invariant constraint index or newline", // could be a comment
                    self.parser.header.latch_count - 1,
                )
                .map(SymbolTarget::InvariantConstraint)
            })
        })
        .or_parse(|| {
            if self.parser.header.justice_property_count > 0 {
                token::fixed(input, b"j")
            } else {
                Fallthrough
            }
            .and_then(|_| {
                token::symbol_index(
                    input,
                    "justice property index",
                    self.parser.header.latch_count - 1,
                )
                .map(SymbolTarget::JusticeProperty)
            })
        })
        .or_parse(|| {
            if self.parser.header.fairness_constraint_count > 0 {
                token::fixed(input, b"f")
            } else {
                Fallthrough
            }
            .and_then(|_| {
                token::symbol_index(
                    input,
                    "fairness constraint index",
                    self.parser.header.latch_count - 1,
                )
                .map(SymbolTarget::FairnessConstraint)
            })
        })
        .optional()?;

        if let Some(target) = target {
            token::required_space(input)?;
            let name = token::remaining_line_content(input)?;

            Ok(Some(Symbol {
                target,
                name: Cow::Borrowed(name),
            }))
        } else {
            Ok(None)
        }
    }

    pub fn comment(&mut self) -> Result<Option<&str>, ParseError> {
        while self.next_symbol()?.is_some() {}
        let input = &mut self.parser.reader;

        if token::fixed(input, b"c").optional()?.is_some() {
            token::required_newline(input)?;
            let comment = token::remaining_file_content(input)?;

            Ok(Some(comment))
        } else {
            eof(input).or_give_up(|| unexpected(input, "symbol, comment or end of file"))?;

            Ok(None)
        }
    }
}

pub struct Writer<'a, L> {
    pub writer: DeferredWriter<'a>,
    pub code: usize,
    codec: PhantomData<L>,
}

impl<'a, L> std::ops::Deref for Writer<'a, L> {
    type Target = DeferredWriter<'a>;

    fn deref(&self) -> &Self::Target {
        &self.writer
    }
}

impl<L> std::ops::DerefMut for Writer<'_, L> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.writer
    }
}

impl<'a, L> Writer<'a, L>
where
    L: Lit,
{
    pub fn new(writer: DeferredWriter<'a>) -> Self {
        Self {
            writer,
            code: 0,
            codec: PhantomData,
        }
    }

    pub fn write_header(&mut self, header: &Header) {
        let fields = [
            header.max_var_index,
            header.input_count,
            header.latch_count,
            header.output_count,
            header.and_gate_count,
            header.bad_state_property_count,
            header.invariant_constraint_count,
            header.justice_property_count,
            header.fairness_constraint_count,
        ];

        self.code = (header.input_count + 1) * 2;

        let mut fields = fields.as_slice();

        while let Some((0, rest)) = fields.split_last() {
            if rest.len() >= 5 {
                fields = rest;
            } else {
                break;
            }
        }

        self.writer.write_all_defer_err(b"aig");

        for &field in fields {
            self.writer.write_all_defer_err(b" ");
            flussab::write::text::ascii_digits(&mut self.writer, field);
        }
        self.writer.write_all_defer_err(b"\n");
    }

    pub fn write_lit(&mut self, lit: L) {
        flussab::write::text::ascii_digits(&mut self.writer, lit.code());
        self.writer.write_all_defer_err(b"\n");
    }

    pub fn write_latch(&mut self, latch: OrderedLatch<L>) {
        flussab::write::text::ascii_digits(&mut self.writer, latch.next_state.code());

        match latch.initialization {
            Some(true) => self.writer.write_all_defer_err(b" 1\n"),
            Some(false) => self.writer.write_all_defer_err(b"\n"),
            None => {
                self.writer.write_all_defer_err(b" ");
                flussab::write::text::ascii_digits(&mut self.writer, self.code);
                self.writer.write_all_defer_err(b"\n");
            }
        }
        self.code += 2;
    }

    pub fn write_count(&mut self, count: usize) {
        flussab::write::text::ascii_digits(&mut self.writer, count);
        self.writer.write_all_defer_err(b"\n");
    }

    pub fn write_and_gate(&mut self, mut and_gate: OrderedAndGate<L>) {
        if and_gate.inputs[0].code() < and_gate.inputs[1].code() {
            and_gate.inputs.swap(0, 1);
        }
        let code_0 = and_gate.inputs[0].code();
        let code_1 = and_gate.inputs[1].code();
        assert!(code_0 <= self.code);
        let delta_0 = self.code - code_0;
        let delta_1 = code_0 - code_1;

        self.write_binary_uint(delta_0);
        self.write_binary_uint(delta_1);
        self.code += 2;
    }

    fn write_binary_uint(&mut self, mut code: usize) {
        // TODO optimize
        let mut bytes = [0u8; (usize::BITS as usize + 6) / 7];
        let mut len = 0;
        loop {
            bytes[len] = (code as u8) | 0x80;
            code >>= 7;

            len += 1;
            if code == 0 {
                break;
            }
        }

        bytes[len - 1] &= 0x7f;

        self.writer.write_all_defer_err(&bytes[..len]);
    }

    pub fn write_symbol(&mut self, symbol: &Symbol) {
        let (prefix, index) = match symbol.target {
            SymbolTarget::Input(index) => (b"i", index),
            SymbolTarget::Output(index) => (b"o", index),
            SymbolTarget::Latch(index) => (b"l", index),
            SymbolTarget::BadStateProperty(index) => (b"b", index),
            SymbolTarget::InvariantConstraint(index) => (b"c", index),
            SymbolTarget::JusticeProperty(index) => (b"j", index),
            SymbolTarget::FairnessConstraint(index) => (b"f", index),
        };

        self.writer.write_all_defer_err(prefix);
        flussab::write::text::ascii_digits(&mut self.writer, index);
        self.writer.write_all_defer_err(b" ");
        self.writer.write_all_defer_err(symbol.name.as_bytes());
        self.writer.write_all_defer_err(b"\n");
    }

    pub fn write_comment(&mut self, comment: &str) {
        self.writer.write_all_defer_err(b"c\n");
        self.writer.write_all_defer_err(comment.as_bytes());
        self.writer.write_all_defer_err(b"\n");
    }

    pub fn write_ordered_aig(&mut self, aig: &OrderedAig<L>) {
        self.write_header(&Header {
            max_var_index: aig.max_var_index,
            input_count: aig.input_count,
            latch_count: aig.latches.len(),
            output_count: aig.outputs.len(),
            and_gate_count: aig.and_gates.len(),
            bad_state_property_count: aig.bad_state_properties.len(),
            invariant_constraint_count: aig.invariant_constraints.len(),
            justice_property_count: aig.justice_properties.len(),
            fairness_constraint_count: aig.fairness_constraints.len(),
        });

        for &latch in &aig.latches {
            self.write_latch(latch);
        }

        for &output in &aig.outputs {
            self.write_lit(output);
        }

        for &bad_state_property in &aig.bad_state_properties {
            self.write_lit(bad_state_property);
        }

        for &invariant_constraint in &aig.invariant_constraints {
            self.write_lit(invariant_constraint);
        }

        for justice_property in &aig.justice_properties {
            self.write_count(justice_property.len());
        }

        for justice_property in &aig.justice_properties {
            for &justice_property_local_fairness_constraint in justice_property {
                self.write_lit(justice_property_local_fairness_constraint);
            }
        }

        for &fairness_constraint in &aig.fairness_constraints {
            self.write_lit(fairness_constraint);
        }

        for &and_gate in &aig.and_gates {
            self.write_and_gate(and_gate);
        }

        for symbol in &aig.symbols {
            self.write_symbol(symbol);
        }

        if let Some(comment) = &aig.comment {
            self.write_comment(comment);
        }
    }
}
