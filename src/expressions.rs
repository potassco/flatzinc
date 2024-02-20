use std::str;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_while_m_n},
    character::complete::{char, multispace1},
    combinator::{map, map_opt, map_res, opt, value, verify},
    error::{FromExternalError, ParseError},
    multi::{fold_many0, many0, separated_list0, separated_list1},
    sequence::{delimited, preceded},
    IResult,
};

use crate::{
    comments::space_or_comment0,
    primitive_literals::{
        bool_literal, float_literal, identifier, int_literal, var_par_identifier,
    },
};

pub type Annotations = Vec<Annotation>;

pub fn annotations<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Annotations, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, annos) = many0(annotation1)(input)?;
    Ok((input, annos))
}

fn annotation1<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Annotation, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = tag("::")(input)?;
    let (input, _) = space_or_comment0(input)?;
    annotation(input)
}

#[derive(PartialEq, Clone, Debug)]
pub struct Annotation {
    pub id: String,
    pub expressions: Vec<AnnExpr>,
}

// <annotation> ::= <identifier>
//                | <identifier> "(" <ann-expr> "," ... ")"
fn annotation<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Annotation, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, id) = identifier(input)?;
    let (input, we) = opt(char('('))(input)?;
    if we.is_some() {
        let (input, expressions_what) = separated_list1(char(','), ann_expr)(input)?;
        let (input, _) = char(')')(input)?;
        Ok((
            input,
            Annotation {
                id,
                expressions: expressions_what,
            },
        ))
    } else {
        let (input, _) = space_or_comment0(input)?;
        Ok((
            input,
            Annotation {
                id,
                expressions: vec![],
            },
        ))
    }
}

// <ann_expr> ::= <expr>
//              | <string_literal>
//              | "[" <annotation> "," ... "]"
#[derive(PartialEq, Clone, Debug)]
pub enum AnnExpr {
    Annotations(Annotations),
    String(String),
    Expr(Expr),
}

fn ann_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AnnExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, expr) = alt((ann_non_array_expr, ae_annotations))(input)?;
    Ok((input, expr))
}

fn ae_annotations<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AnnExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, res) = separated_list1(char(','), annotation)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, AnnExpr::Annotations(res)))
}

// ann_non_array_expr ::=
//       FZ_BOOL_LIT
//     | FZ_INT_LIT
//     | FZ_FLOAT_LIT
//     | set_literal
//     | var_par_id /* variable, possibly array */
//     | var_par_id '[' ann_non_array_expr ']' /* array access */
//     | FZ_STRING_LIT
fn ann_non_array_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AnnExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, expr) = alt((ae_expr, string_lit))(input)?;
    Ok((input, expr))
}

fn ae_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AnnExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, expr) = expr(input)?;
    Ok((input, AnnExpr::Expr(expr)))
}

//
// The code for parsing strings including strings with escape characters is from
// the nom example at
// https://github.com/Geal/nom/blob/ea483e5a81d04dda28ee0159902ede7fc0563f89/examples/string.rs
// The original code is under the MIT license Copyright (c) 2014-2019 Geoffroy Couprie
// https://github.com/Geal/nom/blob/ea483e5a81d04dda28ee0159902ede7fc0563f89/LICENSE
//

/// Parse a unicode sequence, of the form u{XXXX}, where XXXX is 1 to 6
/// hexadecimal numerals. We will combine this later with parse_escaped_char
/// to parse sequences like \u{00AC}.
fn parse_unicode<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());

    let parse_delimited_hex = preceded(char('u'), delimited(char('{'), parse_hex, char('}')));

    let parse_u32 = map_res(parse_delimited_hex, move |hex| u32::from_str_radix(hex, 16));

    map_opt(parse_u32, std::char::from_u32)(input)
}

/// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
fn parse_escaped_char<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    preceded(
        char('\\'),
        alt((
            parse_unicode,
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\u{08}', char('b')),
            value('\u{0C}', char('f')),
            value('\\', char('\\')),
            value('/', char('/')),
            value('"', char('"')),
        )),
    )(input)
}

/// Parse a backslash, followed by any amount of whitespace. This is used later
/// to discard any escaped whitespace.
fn parse_escaped_whitespace<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, &'a str, E> {
    preceded(char('\\'), multispace1)(input)
}

/// Parse a non-empty block of text that doesn't include \ or "
fn parse_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    let not_quote_slash = is_not("\"\\");

    verify(not_quote_slash, |s: &str| !s.is_empty())(input)
}

/// A string fragment contains a fragment of a string being parsed: either
/// a non-empty Literal (a series of non-escaped characters), a single
/// parsed escaped character, or a block of escaped whitespace.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
    Literal(&'a str),
    EscapedChar(char),
    EscapedWS,
}

/// Combine parse_literal, parse_escaped_whitespace, and parse_escaped_char
/// into a StringFragment.
fn parse_fragment<'a, E>(input: &'a str) -> IResult<&'a str, StringFragment<'a>, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    alt((
        map(parse_literal, StringFragment::Literal),
        map(parse_escaped_char, StringFragment::EscapedChar),
        value(StringFragment::EscapedWS, parse_escaped_whitespace),
    ))(input)
}

/// Parse a string literal, including escaped characters such as \n and \".
pub fn string_lit<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AnnExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let build_string = fold_many0(parse_fragment, String::new, |mut string, fragment| {
        match fragment {
            StringFragment::Literal(s) => string.push_str(s),
            StringFragment::EscapedChar(c) => string.push(c),
            StringFragment::EscapedWS => {}
        }
        string
    });
    let (input, string) = delimited(char('"'), build_string, char('"'))(input)?;
    Ok((input, AnnExpr::String(string)))
}

#[test]
fn test_string_lit() {
    use nom::error::VerboseError;
    assert_eq!(
        string_lit::<VerboseError<&str>>("\"bla\""),
        Ok(("", AnnExpr::String("bla".to_string())))
    );
}

#[test]
fn test_string_lit_escaped_characters() {
    use nom::error::VerboseError;
    assert_eq!(
        string_lit::<VerboseError<&str>>(r#""escaped\"characters\ntest\u{0021}\u{01c3}""#),
        Ok((
            "",
            AnnExpr::String("escaped\"characters\ntest!ǃ".to_string())
        ))
    );
}

#[derive(PartialEq, Clone, Debug)]
pub enum BoolExpr {
    Bool(bool),
    VarParIdentifier(String),
}

pub fn bool_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BoolExpr, E> {
    let (input, expr) = alt((be_bool_literal, be_var_par_identifier))(input)?;
    Ok((input, expr))
}

fn be_bool_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BoolExpr, E> {
    let (input, expr) = bool_literal(input)?;
    Ok((input, BoolExpr::Bool(expr)))
}

fn be_var_par_identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BoolExpr, E> {
    let (input, id) = var_par_identifier(input)?;
    Ok((input, BoolExpr::VarParIdentifier(id)))
}

impl TryFrom<Expr> for BoolExpr {
    type Error = ();

    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::VarParIdentifier(id) => Ok(Self::VarParIdentifier(id)),
            Expr::Bool(value) => Ok(Self::Bool(value)),
            _ => Err(()),
        }
    }
}

impl From<BoolExpr> for Expr {
    fn from(expr: BoolExpr) -> Self {
        match expr {
            BoolExpr::Bool(value) => Self::Bool(value),
            BoolExpr::VarParIdentifier(id) => self::Expr::VarParIdentifier(id),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum IntExpr {
    Int(i128),
    VarParIdentifier(String),
}

pub fn int_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, IntExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = space_or_comment0(input)?;
    let (input, expr) = alt((ie_int_literal, ie_var_par_identifier))(input)?;
    Ok((input, expr))
}

fn ie_int_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, IntExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, expr) = int_literal(input)?;
    Ok((input, IntExpr::Int(expr)))
}

fn ie_var_par_identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, IntExpr, E> {
    let (input, id) = var_par_identifier(input)?;
    Ok((input, IntExpr::VarParIdentifier(id)))
}

impl TryFrom<Expr> for IntExpr {
    type Error = ();

    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::VarParIdentifier(id) => Ok(Self::VarParIdentifier(id)),
            Expr::Int(value) => Ok(Self::Int(value)),
            _ => Err(()),
        }
    }
}

impl From<IntExpr> for Expr {
    fn from(expr: IntExpr) -> Self {
        match expr {
            IntExpr::Int(value) => Self::Int(value),
            IntExpr::VarParIdentifier(id) => self::Expr::VarParIdentifier(id),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum FloatExpr {
    Float(f64),
    VarParIdentifier(String),
}

pub fn float_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, FloatExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, expr) = alt((fe_float_literal, fe_var_par_identifier))(input)?;
    Ok((input, expr))
}

fn fe_float_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, FloatExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, expr) = float_literal(input)?;
    Ok((input, FloatExpr::Float(expr)))
}

fn fe_var_par_identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, FloatExpr, E> {
    let (input, id) = var_par_identifier(input)?;
    Ok((input, FloatExpr::VarParIdentifier(id)))
}

impl TryFrom<Expr> for FloatExpr {
    type Error = ();

    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::VarParIdentifier(id) => Ok(Self::VarParIdentifier(id)),
            Expr::Float(value) => Ok(Self::Float(value)),
            _ => Err(()),
        }
    }
}

impl From<FloatExpr> for Expr {
    fn from(expr: FloatExpr) -> Self {
        match expr {
            FloatExpr::Float(value) => Self::Float(value),
            FloatExpr::VarParIdentifier(id) => self::Expr::VarParIdentifier(id),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum SetExpr {
    Set(SetLiteralExpr),
    VarParIdentifier(String),
}

pub fn set_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, expr) = alt((se_set_literal_expr, se_var_par_identifier))(input)?;
    Ok((input, expr))
}

fn se_set_literal_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, sl) = set_literal_expr(input)?;
    Ok((input, SetExpr::Set(sl)))
}

fn se_var_par_identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SetExpr, E> {
    let (input, id) = var_par_identifier(input)?;
    Ok((input, SetExpr::VarParIdentifier(id)))
}

impl TryFrom<Expr> for SetExpr {
    type Error = ();

    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::VarParIdentifier(id) => Ok(Self::VarParIdentifier(id)),
            Expr::Set(value) => Ok(Self::Set(value)),
            _ => Err(()),
        }
    }
}

impl From<SetExpr> for Expr {
    fn from(expr: SetExpr) -> Self {
        match expr {
            SetExpr::Set(value) => Self::Set(value),
            SetExpr::VarParIdentifier(id) => self::Expr::VarParIdentifier(id),
        }
    }
}

#[test]
fn test_expr() {
    use nom::error::VerboseError;
    assert_eq!(
        expr::<VerboseError<&str>>("1..2"),
        Ok((
            "",
            Expr::Set(SetLiteralExpr::IntInRange(IntExpr::Int(1), IntExpr::Int(2)))
        ))
    );
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    VarParIdentifier(String),
    Bool(bool),
    Int(i128),
    Float(f64),
    Set(SetLiteralExpr),
    ArrayOfBool(Vec<BoolExpr>),
    ArrayOfInt(Vec<IntExpr>),
    ArrayOfFloat(Vec<FloatExpr>),
    ArrayOfSet(Vec<SetExpr>),
}

pub fn expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = space_or_comment0(input)?;
    let (input, expr) = alt((
        e_var_par_identifier,
        e_bool_expr,
        e_set_expr,
        e_float_expr,
        e_int_expr,
        e_array_of_bool_expr,
        e_array_of_int_expr,
        e_array_of_float_expr,
        e_array_of_set_expr,
    ))(input)?;
    Ok((input, expr))
}

fn e_var_par_identifier<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, id) = var_par_identifier(input)?;
    Ok((input, Expr::VarParIdentifier(id)))
}

fn e_bool_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, b) = bool_literal(input)?;
    Ok((input, Expr::Bool(b)))
}

fn e_int_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, int) = int_literal(input)?;
    Ok((input, Expr::Int(int)))
}

fn e_float_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, float) = float_literal(input)?;
    Ok((input, Expr::Float(float)))
}

fn e_set_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, set) = set_literal_expr(input)?;
    Ok((input, Expr::Set(set)))
}

fn e_array_of_bool_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, v) = array_of_bool_expr_literal(input)?;
    Ok((input, Expr::ArrayOfBool(v)))
}

fn e_array_of_int_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, v) = array_of_int_expr_literal(input)?;
    Ok((input, Expr::ArrayOfInt(v)))
}

fn e_array_of_float_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, v) = array_of_float_expr_literal(input)?;
    Ok((input, Expr::ArrayOfFloat(v)))
}

fn e_array_of_set_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, v) = array_of_set_expr_literal(input)?;
    Ok((input, Expr::ArrayOfSet(v)))
}

#[derive(PartialEq, Clone, Debug)]
pub enum SetLiteralExpr {
    IntInRange(IntExpr, IntExpr),
    BoundedFloat(FloatExpr, FloatExpr),
    SetFloats(Vec<FloatExpr>),
    SetInts(Vec<IntExpr>),
}

fn set_literal_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SetLiteralExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, sl) = alt((
        sle_int_in_range,
        sle_bounded_float,
        sle_set_of_ints,
        sle_set_of_floats,
    ))(input)?;
    Ok((input, sl))
}

fn sle_int_in_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SetLiteralExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, lb) = int_expr(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ub) = int_expr(input)?;
    Ok((input, SetLiteralExpr::IntInRange(lb, ub)))
}

fn sle_bounded_float<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SetLiteralExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, lb) = float_expr(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ub) = float_expr(input)?;
    Ok((input, SetLiteralExpr::BoundedFloat(lb, ub)))
}

// "{" <int-expr> "," ... "}"
fn sle_set_of_ints<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SetLiteralExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = char('{')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), int_expr)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, SetLiteralExpr::SetInts(v)))
}

// "{" <float-expr> "," ... "}"
fn sle_set_of_floats<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SetLiteralExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = char('{')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), float_expr)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, SetLiteralExpr::SetFloats(v)))
}

#[derive(PartialEq, Clone, Debug)]
pub enum SetLiteral {
    IntRange(i128, i128),
    BoundedFloat(f64, f64),
    SetFloats(Vec<f64>),
    SetInts(Vec<i128>),
}

pub fn set_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E>
where
    E: ParseError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, sl) = alt((
        sl_int_range,
        sl_bounded_float,
        sl_set_of_ints,
        sl_set_of_floats,
    ))(input)?;
    Ok((input, sl))
}

fn sl_int_range<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, lb) = int_literal(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ub) = int_literal(input)?;
    Ok((input, SetLiteral::IntRange(lb, ub)))
}

fn sl_bounded_float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, lb) = float_literal(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ub) = float_literal(input)?;
    Ok((input, SetLiteral::BoundedFloat(lb, ub)))
}

// "{" <int-literal> "," ... "}"
fn sl_set_of_ints<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = char('{')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), int_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, SetLiteral::SetInts(v)))
}

// "{" <float-literal> "," ... "}"
fn sl_set_of_floats<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = char('{')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), float_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, SetLiteral::SetFloats(v)))
}

#[derive(PartialEq, Clone, Debug)]
pub enum ArrayOfBoolExpr {
    Array(Vec<BoolExpr>),
    VarParIdentifier(String),
}

pub fn array_of_bool_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ArrayOfBoolExpr, E> {
    let (input, id) = opt(var_par_identifier)(input)?;
    if let Some(id) = id {
        Ok((input, ArrayOfBoolExpr::VarParIdentifier(id)))
    } else {
        let (input, v) = array_of_bool_expr_literal(input)?;
        Ok((input, ArrayOfBoolExpr::Array(v)))
    }
}

fn array_of_bool_expr_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<BoolExpr>, E> {
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), bool_expr)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, v))
}

pub fn array_of_bool_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<bool>, E> {
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, al) = separated_list0(char(','), bool_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
}

impl TryFrom<Expr> for ArrayOfBoolExpr {
    type Error = ();

    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::VarParIdentifier(id) => Ok(Self::VarParIdentifier(id)),
            Expr::ArrayOfBool(value) => Ok(Self::Array(value)),
            _ => Err(()),
        }
    }
}

impl From<ArrayOfBoolExpr> for Expr {
    fn from(expr: ArrayOfBoolExpr) -> Self {
        match expr {
            ArrayOfBoolExpr::Array(value) => Self::ArrayOfBool(value),
            ArrayOfBoolExpr::VarParIdentifier(id) => self::Expr::VarParIdentifier(id),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum ArrayOfIntExpr {
    Array(Vec<IntExpr>),
    VarParIdentifier(String),
}

pub fn array_of_int_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ArrayOfIntExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, id) = opt(var_par_identifier)(input)?;
    if let Some(id) = id {
        Ok((input, ArrayOfIntExpr::VarParIdentifier(id)))
    } else {
        let (input, v) = array_of_int_expr_literal(input)?;
        Ok((input, ArrayOfIntExpr::Array(v)))
    }
}

fn array_of_int_expr_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<IntExpr>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), int_expr)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, v))
}

pub fn array_of_int_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<i128>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, al) = separated_list0(char(','), int_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
}

impl TryFrom<Expr> for ArrayOfIntExpr {
    type Error = ();

    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::VarParIdentifier(id) => Ok(Self::VarParIdentifier(id)),
            Expr::ArrayOfInt(value) => Ok(Self::Array(value)),
            _ => Err(()),
        }
    }
}

impl From<ArrayOfIntExpr> for Expr {
    fn from(expr: ArrayOfIntExpr) -> Self {
        match expr {
            ArrayOfIntExpr::Array(value) => Self::ArrayOfInt(value),
            ArrayOfIntExpr::VarParIdentifier(id) => self::Expr::VarParIdentifier(id),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum ArrayOfFloatExpr {
    Array(Vec<FloatExpr>),
    VarParIdentifier(String),
}

pub fn array_of_float_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ArrayOfFloatExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, id) = opt(var_par_identifier)(input)?;
    if let Some(id) = id {
        Ok((input, ArrayOfFloatExpr::VarParIdentifier(id)))
    } else {
        let (input, v) = array_of_float_expr_literal(input)?;
        Ok((input, ArrayOfFloatExpr::Array(v)))
    }
}

fn array_of_float_expr_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<FloatExpr>, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), float_expr)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, v))
}

pub fn array_of_float_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<f64>, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, al) = separated_list0(char(','), float_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
}

impl TryFrom<Expr> for ArrayOfFloatExpr {
    type Error = ();

    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::VarParIdentifier(id) => Ok(Self::VarParIdentifier(id)),
            Expr::ArrayOfFloat(value) => Ok(Self::Array(value)),
            _ => Err(()),
        }
    }
}

impl From<ArrayOfFloatExpr> for Expr {
    fn from(expr: ArrayOfFloatExpr) -> Self {
        match expr {
            ArrayOfFloatExpr::Array(value) => Self::ArrayOfFloat(value),
            ArrayOfFloatExpr::VarParIdentifier(id) => self::Expr::VarParIdentifier(id),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum ArrayOfSetExpr {
    Array(Vec<SetExpr>),
    VarParIdentifier(String),
}

pub fn array_of_set_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ArrayOfSetExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, id) = opt(var_par_identifier)(input)?;
    if let Some(id) = id {
        Ok((input, ArrayOfSetExpr::VarParIdentifier(id)))
    } else {
        let (input, v) = array_of_set_expr_literal(input)?;
        Ok((input, ArrayOfSetExpr::Array(v)))
    }
}

fn array_of_set_expr_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<SetExpr>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), set_expr)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, v))
}

pub fn array_of_set_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<SetLiteral>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, al) = separated_list0(char(','), set_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
}

impl TryFrom<Expr> for ArrayOfSetExpr {
    type Error = ();

    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::VarParIdentifier(id) => Ok(Self::VarParIdentifier(id)),
            Expr::ArrayOfSet(value) => Ok(Self::Array(value)),
            _ => Err(()),
        }
    }
}

impl From<ArrayOfSetExpr> for Expr {
    fn from(expr: ArrayOfSetExpr) -> Self {
        match expr {
            ArrayOfSetExpr::Array(value) => Self::ArrayOfSet(value),
            ArrayOfSetExpr::VarParIdentifier(id) => self::Expr::VarParIdentifier(id),
        }
    }
}
