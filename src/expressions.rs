use std::str;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::complete::char;
use nom::combinator::opt;
use nom::multi::{many0, separated_list0, separated_list1};

use crate::{comments, primitive_literals, FromExternalError, IResult, ParseError};

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
    let (input, _) = comments::space_or_comment0(input)?;
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
    let (input, id) = primitive_literals::identifier(input)?;
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
        let (input, _) = comments::space_or_comment0(input)?;
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
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, res) = separated_list1(char(','), annotation)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
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

// TODO: implement support for escaped characters in string literals
pub fn string_lit<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AnnExpr, E> {
    let (input, _) = char('"')(input)?;
    let (input, string) = take_while(is_valid)(input)?;
    let (input, _) = char('"')(input)?;
    Ok((input, AnnExpr::String(string.to_string())))
}

fn is_valid(c: char) -> bool {
    !matches!(c, '"')
}

#[test]
fn test_string_lit() {
    use nom::error::VerboseError;
    assert_eq!(
        string_lit::<VerboseError<&str>>("\"bla\""),
        Ok(("", AnnExpr::String("bla".to_string())))
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
    let (input, expr) = primitive_literals::bool_literal(input)?;
    Ok((input, BoolExpr::Bool(expr)))
}

fn be_var_par_identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BoolExpr, E> {
    let (input, id) = primitive_literals::var_par_identifier(input)?;
    Ok((input, BoolExpr::VarParIdentifier(id)))
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
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, expr) = alt((ie_int_literal, ie_var_par_identifier))(input)?;
    Ok((input, expr))
}

fn ie_int_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, IntExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, expr) = primitive_literals::int_literal(input)?;
    Ok((input, IntExpr::Int(expr)))
}

fn ie_var_par_identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, IntExpr, E> {
    let (input, id) = primitive_literals::var_par_identifier(input)?;
    Ok((input, IntExpr::VarParIdentifier(id)))
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
    let (input, expr) = primitive_literals::float_literal(input)?;
    Ok((input, FloatExpr::Float(expr)))
}

fn fe_var_par_identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, FloatExpr, E> {
    let (input, id) = primitive_literals::var_par_identifier(input)?;
    Ok((input, FloatExpr::VarParIdentifier(id)))
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
    let (input, id) = primitive_literals::var_par_identifier(input)?;
    Ok((input, SetExpr::VarParIdentifier(id)))
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
    let (input, _) = comments::space_or_comment0(input)?;
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
    let (input, id) = primitive_literals::var_par_identifier(input)?;
    Ok((input, Expr::VarParIdentifier(id)))
}

fn e_bool_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, b) = primitive_literals::bool_literal(input)?;
    Ok((input, Expr::Bool(b)))
}

fn e_int_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, int) = primitive_literals::int_literal(input)?;
    Ok((input, Expr::Int(int)))
}

fn e_float_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, float) = primitive_literals::float_literal(input)?;
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
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
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
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
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
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), int_expr)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
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
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), float_expr)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
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
    let (input, lb) = primitive_literals::int_literal(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, ub) = primitive_literals::int_literal(input)?;
    Ok((input, SetLiteral::IntRange(lb, ub)))
}

fn sl_bounded_float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, lb) = primitive_literals::float_literal(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, ub) = primitive_literals::float_literal(input)?;
    Ok((input, SetLiteral::BoundedFloat(lb, ub)))
}

// "{" <int-literal> "," ... "}"
fn sl_set_of_ints<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = char('{')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), primitive_literals::int_literal)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, SetLiteral::SetInts(v)))
}

// "{" <float-literal> "," ... "}"
fn sl_set_of_floats<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = char('{')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), primitive_literals::float_literal)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
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
    let (input, id) = opt(primitive_literals::var_par_identifier)(input)?;
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
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list1(char(','), bool_expr)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, v))
}

pub fn array_of_bool_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<bool>, E> {
    let (input, _) = char('[')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, al) = separated_list1(char(','), primitive_literals::bool_literal)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
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
    let (input, id) = opt(primitive_literals::var_par_identifier)(input)?;
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
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list1(char(','), int_expr)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
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
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, al) = separated_list1(char(','), primitive_literals::int_literal)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
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
    let (input, id) = opt(primitive_literals::var_par_identifier)(input)?;
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
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list1(char(','), float_expr)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
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
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, al) = separated_list1(char(','), primitive_literals::float_literal)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
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
    let (input, id) = opt(primitive_literals::var_par_identifier)(input)?;
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
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list1(char(','), set_expr)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
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
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, al) = separated_list1(char(','), set_literal)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
}
