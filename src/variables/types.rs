use std::str;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::char,
    error::{FromExternalError, ParseError},
    multi::separated_list0,
    IResult,
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

pub fn var_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, VarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, var_type) = alt((vt_basic_var_type, array_var_type))(input)?;
    Ok((input, var_type))
}

fn vt_basic_var_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, VarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, vt) = basic_var_type(input)?;
    Ok((input, VarType::BasicVarType(vt)))
}

fn array_var_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, VarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = tag("array")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ix) = index_set(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, var_type) = basic_var_type(input)?;
    Ok((input, VarType::Array { ix, var_type }))
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

pub fn basic_var_type<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("var")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, vt) = alt((
        bvt_basic_type,
        bvt_int_in_range,
        bvt_int_in_set,
        bvt_bounded_float,
        bvt_subset_of_int_set,
        bvt_subset_of_int_range,
    ))(input)?;
    Ok((input, vt))
}

fn bvt_basic_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicVarType, E> {
    let (input, bt) = basic_type(input)?;
    Ok((input, BasicVarType::BasicType(bt)))
}

fn bvt_int_in_range<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, (lb, ub)) = int_in_range(input)?;
    Ok((input, BasicVarType::IntInRange(lb, ub)))
}

fn bvt_int_in_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, set) = int_in_set(input)?;
    Ok((input, BasicVarType::IntInSet(set)))
}

fn bvt_bounded_float<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, (lb, ub)) = bounded_float(input)?;
    Ok((input, BasicVarType::BoundedFloat(lb, ub)))
}

fn bvt_subset_of_int_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, (lb, ub)) = subset_of_int_range(input)?;
    Ok((input, BasicVarType::SubSetOfIntRange(lb, ub)))
}

fn bvt_subset_of_int_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, set) = subset_of_int_set(input)?;
    Ok((input, BasicVarType::SubSetOfIntSet(set)))
}

pub fn int_in_range<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (i128, i128), E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, lb) = int_literal(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ub) = int_literal(input)?;
    Ok((input, (lb, ub)))
}

pub fn bounded_float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (f64, f64), E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, lb) = float_literal(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ub) = float_literal(input)?;
    Ok((input, (lb, ub)))
}

// "{" <float-literal> "," ... "}"
pub fn float_in_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Vec<f64>, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = char('{')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), float_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, v))
}

// "set" "of" <int_literal> ".." <int_literal>
pub fn subset_of_int_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, (i128, i128), E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _tag) = tag("set")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, lb) = int_literal(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ub) = int_literal(input)?;
    Ok((input, (lb, ub)))
}

// "set" "of" "{" [ <int-literal> "," ... ] "}"
pub fn subset_of_int_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<i128>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _tag) = tag("set of {")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), int_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("}")(input)?;
    Ok((input, v))
}

// "{" <int-literal> "," ... "}"
pub fn int_in_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Vec<i128>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = char('{')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), int_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, v))
}
