use winnow::{
    combinator::alt,
    combinator::separated,
    error::{FromExternalError, ParserError},
    PResult, Parser,
};

use crate::{
    basic_types::{basic_type, BasicType},
    comments::{space_or_comment0, space_or_comment1},
    primitive_literals::{float_literal, index_set, int_literal, IndexSet},
};

#[derive(PartialEq, Clone, Debug)]
pub enum VarType {
    BasicVarType(BasicVarType),
    Array {
        ix: IndexSet,
        var_type: BasicVarType,
    },
}

pub fn var_type<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<VarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    alt((vt_basic_var_type, array_var_type)).parse_next(input)
}

fn vt_basic_var_type<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<VarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let vt = basic_var_type(input)?;
    Ok(VarType::BasicVarType(vt))
}

fn array_var_type<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<VarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    "array".parse_next(input)?;
    space_or_comment0(input)?;
    '['.parse_next(input)?;
    space_or_comment0(input)?;
    let ix = index_set(input)?;
    space_or_comment0(input)?;
    ']'.parse_next(input)?;
    space_or_comment1(input)?;
    "of".parse_next(input)?;
    space_or_comment1(input)?;
    let var_type = basic_var_type(input)?;
    Ok(VarType::Array { ix, var_type })
}

#[derive(PartialEq, Clone, Debug)]
pub enum BasicVarType {
    BasicType(BasicType),
    IntInRange(i128, i128),
    IntInSet(Vec<i128>),
    BoundedFloat(f64, f64),
    SubSetOfIntSet(Vec<i128>),
    SubSetOfIntRange(i128, i128),
}

pub fn basic_var_type<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    space_or_comment0(input)?;
    "var".parse_next(input)?;
    space_or_comment1(input)?;
    let vt = alt((
        bvt_basic_type,
        bvt_int_in_range,
        bvt_int_in_set,
        bvt_bounded_float,
        bvt_subset_of_int_set,
        bvt_subset_of_int_range,
    ))
    .parse_next(input)?;
    Ok(vt)
}

fn bvt_basic_type<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<BasicVarType, E> {
    let bt = basic_type(input)?;
    Ok(BasicVarType::BasicType(bt))
}

fn bvt_int_in_range<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (lb, ub) = int_in_range(input)?;
    Ok(BasicVarType::IntInRange(lb, ub))
}

fn bvt_int_in_set<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let set = int_in_set(input)?;
    Ok(BasicVarType::IntInSet(set))
}

fn bvt_bounded_float<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (lb, ub) = bounded_float(input)?;
    Ok(BasicVarType::BoundedFloat(lb, ub))
}

fn bvt_subset_of_int_range<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (lb, ub) = subset_of_int_range(input)?;
    Ok(BasicVarType::SubSetOfIntRange(lb, ub))
}

fn bvt_subset_of_int_set<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let set = subset_of_int_set(input)?;
    Ok(BasicVarType::SubSetOfIntSet(set))
}

pub fn int_in_range<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<(i128, i128), E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let lb = int_literal(input)?;
    space_or_comment0(input)?;
    "..".parse_next(input)?;
    space_or_comment0(input)?;
    let ub = int_literal(input)?;
    Ok((lb, ub))
}

pub fn bounded_float<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<(f64, f64), E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let lb = float_literal(input)?;
    space_or_comment0(input)?;
    "..".parse_next(input)?;
    space_or_comment0(input)?;
    let ub = float_literal(input)?;
    Ok((lb, ub))
}

// "{" <float-literal> "," ... "}"
pub fn float_in_set<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Vec<f64>, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    '{'.parse_next(input)?;
    space_or_comment0(input)?;
    let v = separated(0.., float_literal, ',').parse_next(input)?;
    space_or_comment0(input)?;
    '}'.parse_next(input)?;
    Ok(v)
}

// "set" "of" <int_literal> ".." <int_literal>
pub fn subset_of_int_range<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<(i128, i128), E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    "set".parse_next(input)?;
    space_or_comment1(input)?;
    "of".parse_next(input)?;
    space_or_comment1(input)?;
    let lb = int_literal(input)?;
    space_or_comment0(input)?;
    "..".parse_next(input)?;
    space_or_comment0(input)?;
    let ub = int_literal(input)?;
    Ok((lb, ub))
}

// "set" "of" "{" [ <int-literal> "," ... ] "}"
pub fn subset_of_int_set<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Vec<i128>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    "set".parse_next(input)?;
    space_or_comment1(input)?;
    "of".parse_next(input)?;
    space_or_comment1(input)?;
    '{'.parse_next(input)?;
    space_or_comment0(input)?;
    let v = separated(0.., int_literal, ',').parse_next(input)?;
    space_or_comment0(input)?;
    '}'.parse_next(input)?;
    Ok(v)
}

// "{" <int-literal> "," ... "}"
pub fn int_in_set<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Vec<i128>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    '{'.parse_next(input)?;
    space_or_comment0(input)?;
    let v = separated(0.., int_literal, ',').parse_next(input)?;
    space_or_comment0(input)?;
    '}'.parse_next(input)?;
    Ok(v)
}
