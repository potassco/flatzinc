use winnow::{
    ascii::multispace1,
    combinator::{alt, delimited, fold_repeat, opt, preceded, repeat, separated},
    error::{FromExternalError, ParserError},
    token::{take_till1, take_while},
    PResult, Parser,
};

use crate::{
    comments::space_or_comment0,
    primitive_literals::{
        bool_literal, float_literal, identifier, int_literal, var_par_identifier,
    },
};

pub type Annotations = Vec<Annotation>;

pub fn annotations<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Annotations, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    repeat(0.., annotation1).parse_next(input)
}

fn annotation1<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Annotation, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    "::".parse_next(input)?;
    space_or_comment0(input)?;
    annotation(input)
}

#[derive(PartialEq, Clone, Debug)]
pub struct Annotation {
    pub id: String,
    pub expressions: Vec<AnnExpr>,
}

// <annotation> ::= <identifier>
//                | <identifier> "(" <ann-expr> "," ... ")"
fn annotation<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Annotation, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let id = identifier(input)?;
    let we = opt('(').parse_next(input)?;
    if we.is_some() {
        let expressions_what = separated(1.., ann_expr, ',').parse_next(input)?;
        ')'.parse_next(input)?;
        Ok(Annotation {
            id,
            expressions: expressions_what,
        })
    } else {
        space_or_comment0(input)?;
        Ok(Annotation {
            id,
            expressions: vec![],
        })
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

fn ann_expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<AnnExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    alt((ann_non_array_expr, ae_annotations)).parse_next(input)
}

fn ae_annotations<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<AnnExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    '['.parse_next(input)?;
    space_or_comment0(input)?;
    let res = separated(1.., annotation, ',').parse_next(input)?;
    space_or_comment0(input)?;
    ']'.parse_next(input)?;
    Ok(AnnExpr::Annotations(res))
}

// ann_non_array_expr ::=
//       FZ_BOOL_LIT
//     | FZ_INT_LIT
//     | FZ_FLOAT_LIT
//     | set_literal
//     | var_par_id /* variable, possibly array */
//     | var_par_id '[' ann_non_array_expr ']' /* array access */
//     | FZ_STRING_LIT
fn ann_non_array_expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<AnnExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    alt((ae_expr, string_lit)).parse_next(input)
}

fn ae_expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<AnnExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let expr = expr(input)?;
    Ok(AnnExpr::Expr(expr))
}

//
// The code for parsing strings including strings with escape characters is from
// the nom example at
// https://github.com/Geal/nom/blob/ea483e5a81d04dda28ee0159902ede7fc0563f89/examples/string.rs
// The original code is under the MIT license Copyright (c) 2014-2019 Geoffroy Couprie
// https://github.com/Geal/nom/blob/ea483e5a81d04dda28ee0159902ede7fc0563f89/LICENSE
//

/// Parse a unicode sequence, of the form `u{XXXX}`, where XXXX is 1 to 6
/// hexadecimal numerals. We will combine this later with [`parse_escaped_char`]
/// to parse sequences like `\u{00AC}`.
fn parse_unicode<'a, E>(input: &mut &'a str) -> PResult<char, E>
where
    E: ParserError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    let parse_hex = take_while(1..6, |c: char| c.is_ascii_hexdigit());

    let parse_delimited_hex = preceded('u', delimited('{', parse_hex, '}'));

    if let Some(hex) = opt(parse_delimited_hex).parse_next(input)? {
        if let Ok(u) = u32::from_str_radix(hex, 16) {
            if let Some(x) = std::char::from_u32(u) {
                Ok(x)
            } else {
                Err(ParserError::from_error_kind(
                    input,
                    winnow::error::ErrorKind::Assert,
                ))
            }
        } else {
            Err(ParserError::from_error_kind(
                input,
                winnow::error::ErrorKind::Assert,
            ))
        }
    } else {
        Err(ParserError::from_error_kind(
            input,
            winnow::error::ErrorKind::Assert,
        ))
    }
}

/// Parse an escaped character: `\n`, `\t`, `\r`, `\u{00AC}`, etc.
fn parse_escaped_char<'a, E>(input: &mut &'a str) -> PResult<StringFragment<'a>, E>
where
    E: ParserError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    let c = preceded(
        '\\',
        alt((
            parse_unicode,
            'n'.value('\n'),
            'r'.value('\r'),
            't'.value('\t'),
            'b'.value('\u{08}'),
            'f'.value('\u{0C}'),
            '\\',
            '/',
            '"',
        )),
    )
    .parse_next(input)?;

    Ok(StringFragment::EscapedChar(c))
}

/// Parse a backslash, followed by any amount of whitespace. This is used later
/// to discard any escaped whitespace.
fn parse_escaped_whitespace<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<StringFragment<'a>, E> {
    preceded('\\', multispace1).parse_next(input)?;
    Ok(StringFragment::EscapedWS)
}

/// Parse a non-empty block of text that doesn't include `\` or `"`
fn parse_literal<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<StringFragment<'a>, E> {
    let c = take_till1(['\"', '\\']).parse_next(input)?;
    Ok(StringFragment::Literal(c))
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

/// Combine [`parse_literal`], [`parse_escaped_whitespace`], and [`parse_escaped_char`]
/// into a [`StringFragment`].
fn parse_fragment<'a, E>(input: &mut &'a str) -> PResult<StringFragment<'a>, E>
where
    E: ParserError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    alt((
        parse_literal,
        parse_escaped_char,
        parse_escaped_whitespace.value(StringFragment::EscapedWS),
    ))
    .parse_next(input)
}

/// Parse a string literal, including escaped characters such as `\n` and `\"`.
pub fn string_lit<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<AnnExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let build_string = fold_repeat(0.., parse_fragment, String::new, |mut string, fragment| {
        match fragment {
            StringFragment::Literal(s) => string.push_str(s),
            StringFragment::EscapedChar(c) => string.push(c),
            StringFragment::EscapedWS => {}
        }
        string
    });
    let string = delimited('"', build_string, '"').parse_next(input)?;
    Ok(AnnExpr::String(string))
}
#[test]
fn test_string_lit() {
    use winnow::error::ContextError;
    let mut input = "\"bla\"";
    assert_eq!(
        string_lit::<ContextError<&str>>(&mut input),
        Ok(AnnExpr::String("bla".to_string()))
    );
}
#[test]
fn test_string_lit_escaped_characters() {
    use winnow::error::ContextError;
    let mut input = r#""escaped\"characters\ntest\u{0021}\u{01c3}""#;
    assert_eq!(
        string_lit::<ContextError<&str>>(&mut input),
        Ok(AnnExpr::String("escaped\"characters\ntest!ǃ".to_string()))
    );
}

#[derive(PartialEq, Clone, Debug)]
pub enum BoolExpr {
    Bool(bool),
    VarParIdentifier(String),
}

pub fn bool_expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<BoolExpr, E> {
    alt((be_bool_literal, be_var_par_identifier)).parse_next(input)
}
#[test]
fn test_bool_expr() {
    use winnow::error::ContextError;
    let mut input = "true);";
    assert_eq!(
        bool_expr::<ContextError<&str>>(&mut input),
        Ok(BoolExpr::Bool(true))
    );
}

fn be_bool_literal<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<BoolExpr, E> {
    let expr = bool_literal(input)?;
    Ok(BoolExpr::Bool(expr))
}
#[test]
fn test_bool_literal() {
    use winnow::error::ContextError;
    let mut input = "true);";
    assert_eq!(
        be_bool_literal::<ContextError<&str>>(&mut input),
        Ok(BoolExpr::Bool(true))
    );
    assert_eq!(input, ");");
}

fn be_var_par_identifier<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<BoolExpr, E> {
    let id = var_par_identifier(input)?;
    Ok(BoolExpr::VarParIdentifier(id))
}

#[derive(PartialEq, Clone, Debug)]
pub enum IntExpr {
    Int(i128),
    VarParIdentifier(String),
}

pub fn int_expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<IntExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    space_or_comment0(input)?;
    let expr = alt((ie_int_literal, ie_var_par_identifier)).parse_next(input)?;
    Ok(expr)
}

fn ie_int_literal<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<IntExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let expr = int_literal(input)?;
    Ok(IntExpr::Int(expr))
}

fn ie_var_par_identifier<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<IntExpr, E> {
    let id = var_par_identifier(input)?;
    Ok(IntExpr::VarParIdentifier(id))
}

#[derive(PartialEq, Clone, Debug)]
pub enum FloatExpr {
    Float(f64),
    VarParIdentifier(String),
}

pub fn float_expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<FloatExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    alt((fe_float_literal, fe_var_par_identifier)).parse_next(input)
}

fn fe_float_literal<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<FloatExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let expr = float_literal(input)?;
    Ok(FloatExpr::Float(expr))
}

fn fe_var_par_identifier<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<FloatExpr, E> {
    let id = var_par_identifier(input)?;
    Ok(FloatExpr::VarParIdentifier(id))
}

#[derive(PartialEq, Clone, Debug)]
pub enum SetExpr {
    Set(SetLiteralExpr),
    VarParIdentifier(String),
}

pub fn set_expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<SetExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    alt((se_set_literal_expr, se_var_par_identifier)).parse_next(input)
}

fn se_set_literal_expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<SetExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let sl = set_literal_expr(input)?;
    Ok(SetExpr::Set(sl))
}

fn se_var_par_identifier<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<SetExpr, E> {
    let id = var_par_identifier(input)?;
    Ok(SetExpr::VarParIdentifier(id))
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

pub fn expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Expr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    space_or_comment0(input)?;
    let expr = alt((
        e_var_par_identifier,
        e_bool_expr,
        e_set_expr,
        e_float_expr,
        e_int_expr,
        e_array_of_bool_expr,
        e_array_of_int_expr,
        e_array_of_float_expr,
        e_array_of_set_expr,
    ))
    .parse_next(input)?;
    Ok(expr)
}
#[test]
fn test_expr() {
    use winnow::error::ContextError;
    let mut input = "1..2";
    assert_eq!(
        expr::<ContextError<&str>>(&mut input),
        Ok(Expr::Set(SetLiteralExpr::IntInRange(
            IntExpr::Int(1),
            IntExpr::Int(2)
        )))
    );
}

fn e_var_par_identifier<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Expr, E> {
    let id = var_par_identifier(input)?;
    Ok(Expr::VarParIdentifier(id))
}

fn e_bool_expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Expr, E> {
    let b = bool_literal(input)?;
    Ok(Expr::Bool(b))
}

fn e_int_expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Expr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let int = int_literal(input)?;
    Ok(Expr::Int(int))
}

fn e_float_expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Expr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let float = float_literal(input)?;
    Ok(Expr::Float(float))
}

fn e_set_expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Expr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let set = set_literal_expr(input)?;
    Ok(Expr::Set(set))
}

fn e_array_of_bool_expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Expr, E> {
    let v = array_of_bool_expr_literal(input)?;
    Ok(Expr::ArrayOfBool(v))
}

fn e_array_of_int_expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Expr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let v = array_of_int_expr_literal(input)?;
    Ok(Expr::ArrayOfInt(v))
}

fn e_array_of_float_expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Expr, E>
where
    E: ParserError<&'a str> + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let v = array_of_float_expr_literal(input)?;
    Ok(Expr::ArrayOfFloat(v))
}

fn e_array_of_set_expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Expr, E>
where
    E: ParserError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let v = array_of_set_expr_literal(input)?;
    Ok(Expr::ArrayOfSet(v))
}

#[derive(PartialEq, Clone, Debug)]
pub enum SetLiteralExpr {
    IntInRange(IntExpr, IntExpr),
    BoundedFloat(FloatExpr, FloatExpr),
    SetFloats(Vec<FloatExpr>),
    SetInts(Vec<IntExpr>),
}

fn set_literal_expr<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<SetLiteralExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    alt((
        sle_int_in_range,
        sle_bounded_float,
        sle_set_of_ints,
        sle_set_of_floats,
    ))
    .parse_next(input)
}

fn sle_int_in_range<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<SetLiteralExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let lb = int_expr(input)?;
    space_or_comment0(input)?;
    "..".parse_next(input)?;
    space_or_comment0(input)?;
    let ub = int_expr(input)?;
    Ok(SetLiteralExpr::IntInRange(lb, ub))
}

fn sle_bounded_float<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<SetLiteralExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let lb = float_expr(input)?;
    space_or_comment0(input)?;
    "..".parse_next(input)?;
    space_or_comment0(input)?;
    let ub = float_expr(input)?;
    Ok(SetLiteralExpr::BoundedFloat(lb, ub))
}

// "{" <int-expr> "," ... "}"
fn sle_set_of_ints<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<SetLiteralExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    '{'.parse_next(input)?;
    space_or_comment0(input)?;
    let v = separated(0.., int_expr, ',').parse_next(input)?;
    space_or_comment0(input)?;
    '}'.parse_next(input)?;
    Ok(SetLiteralExpr::SetInts(v))
}

// "{" <float-expr> "," ... "}"
fn sle_set_of_floats<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<SetLiteralExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    '{'.parse_next(input)?;
    space_or_comment0(input)?;
    let v = separated(0.., float_expr, ',').parse_next(input)?;
    space_or_comment0(input)?;
    '}'.parse_next(input)?;
    Ok(SetLiteralExpr::SetFloats(v))
}

#[derive(PartialEq, Clone, Debug)]
pub enum SetLiteral {
    IntRange(i128, i128),
    BoundedFloat(f64, f64),
    SetFloats(Vec<f64>),
    SetInts(Vec<i128>),
}

pub fn set_literal<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<SetLiteral, E>
where
    E: ParserError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    alt((
        sl_int_range,
        sl_bounded_float,
        sl_set_of_ints,
        sl_set_of_floats,
    ))
    .parse_next(input)
}

fn sl_int_range<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<SetLiteral, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let lb = int_literal(input)?;
    space_or_comment0(input)?;
    "..".parse_next(input)?;
    space_or_comment0(input)?;
    let ub = int_literal(input)?;
    Ok(SetLiteral::IntRange(lb, ub))
}

fn sl_bounded_float<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<SetLiteral, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let lb = float_literal(input)?;
    space_or_comment0(input)?;
    "..".parse_next(input)?;
    space_or_comment0(input)?;
    let ub = float_literal(input)?;
    Ok(SetLiteral::BoundedFloat(lb, ub))
}

// "{" <int-literal> "," ... "}"
fn sl_set_of_ints<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<SetLiteral, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    '{'.parse_next(input)?;
    space_or_comment0(input)?;
    let v = separated(0.., int_literal, ',').parse_next(input)?;
    space_or_comment0(input)?;
    '}'.parse_next(input)?;
    Ok(SetLiteral::SetInts(v))
}

// "{" <float-literal> "," ... "}"
fn sl_set_of_floats<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<SetLiteral, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    '{'.parse_next(input)?;
    space_or_comment0(input)?;
    let v = separated(0.., float_literal, ',').parse_next(input)?;
    space_or_comment0(input)?;
    '}'.parse_next(input)?;
    Ok(SetLiteral::SetFloats(v))
}

#[derive(PartialEq, Clone, Debug)]
pub enum ArrayOfBoolExpr {
    Array(Vec<BoolExpr>),
    VarParIdentifier(String),
}

pub fn array_of_bool_expr<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<ArrayOfBoolExpr, E> {
    let id = opt(var_par_identifier).parse_next(input)?;
    if let Some(id) = id {
        Ok(ArrayOfBoolExpr::VarParIdentifier(id))
    } else {
        let v = array_of_bool_expr_literal(input)?;
        Ok(ArrayOfBoolExpr::Array(v))
    }
}

fn array_of_bool_expr_literal<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<Vec<BoolExpr>, E> {
    '['.parse_next(input)?;
    space_or_comment0(input)?;
    let v = separated(0.., bool_expr, ',').parse_next(input)?;
    space_or_comment0(input)?;
    ']'.parse_next(input)?;
    Ok(v)
}

pub fn array_of_bool_literal<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<Vec<bool>, E> {
    '['.parse_next(input)?;
    space_or_comment0(input)?;
    let al = separated(0.., bool_literal, ',').parse_next(input)?;
    space_or_comment0(input)?;
    ']'.parse_next(input)?;
    Ok(al)
}

#[derive(PartialEq, Clone, Debug)]
pub enum ArrayOfIntExpr {
    Array(Vec<IntExpr>),
    VarParIdentifier(String),
}

pub fn array_of_int_expr<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<ArrayOfIntExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let id = opt(var_par_identifier).parse_next(input)?;
    if let Some(id) = id {
        Ok(ArrayOfIntExpr::VarParIdentifier(id))
    } else {
        let v = array_of_int_expr_literal(input)?;
        Ok(ArrayOfIntExpr::Array(v))
    }
}

fn array_of_int_expr_literal<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<Vec<IntExpr>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    '['.parse_next(input)?;
    space_or_comment0(input)?;
    let v = separated(0.., int_expr, ',').parse_next(input)?;
    space_or_comment0(input)?;
    ']'.parse_next(input)?;
    Ok(v)
}

pub fn array_of_int_literal<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<Vec<i128>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    '['.parse_next(input)?;
    space_or_comment0(input)?;
    let al = separated(0.., int_literal, ',').parse_next(input)?;
    space_or_comment0(input)?;
    ']'.parse_next(input)?;
    Ok(al)
}

#[derive(PartialEq, Clone, Debug)]
pub enum ArrayOfFloatExpr {
    Array(Vec<FloatExpr>),
    VarParIdentifier(String),
}

pub fn array_of_float_expr<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<ArrayOfFloatExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let id = opt(var_par_identifier).parse_next(input)?;
    if let Some(id) = id {
        Ok(ArrayOfFloatExpr::VarParIdentifier(id))
    } else {
        let v = array_of_float_expr_literal(input)?;
        Ok(ArrayOfFloatExpr::Array(v))
    }
}

fn array_of_float_expr_literal<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<Vec<FloatExpr>, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    '['.parse_next(input)?;
    space_or_comment0(input)?;
    let v = separated(0.., float_expr, ',').parse_next(input)?;
    space_or_comment0(input)?;
    ']'.parse_next(input)?;
    Ok(v)
}

pub fn array_of_float_literal<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<Vec<f64>, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    '['.parse_next(input)?;
    space_or_comment0(input)?;
    let al = separated(0.., float_literal, ',').parse_next(input)?;
    space_or_comment0(input)?;
    ']'.parse_next(input)?;
    Ok(al)
}

#[derive(PartialEq, Clone, Debug)]
pub enum ArrayOfSetExpr {
    Array(Vec<SetExpr>),
    VarParIdentifier(String),
}

pub fn array_of_set_expr<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<ArrayOfSetExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let id = opt(var_par_identifier).parse_next(input)?;
    if let Some(id) = id {
        Ok(ArrayOfSetExpr::VarParIdentifier(id))
    } else {
        let v = array_of_set_expr_literal(input)?;
        Ok(ArrayOfSetExpr::Array(v))
    }
}

fn array_of_set_expr_literal<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<Vec<SetExpr>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    '['.parse_next(input)?;
    space_or_comment0(input)?;
    let v = separated(0.., set_expr, ',').parse_next(input)?;
    space_or_comment0(input)?;
    ']'.parse_next(input)?;
    Ok(v)
}

pub fn array_of_set_literal<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<Vec<SetLiteral>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    '['.parse_next(input)?;
    space_or_comment0(input)?;
    let al = separated(0.., set_literal, ',').parse_next(input)?;
    space_or_comment0(input)?;
    ']'.parse_next(input)?;
    Ok(al)
}
