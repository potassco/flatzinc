use std::str;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::char;

use crate::statements::parameter_types::BasicParType;
use crate::statements::variable_types::BasicVarType;
use crate::statements::{parameter_types, variable_types};
use crate::{comments, statements, FromExternalError, IResult, ParseError};

#[derive(PartialEq, Clone, Debug)]
pub enum BasicPredParType {
    BasicParType(BasicParType),
    BasicVarType(BasicVarType),
    VarSetOfInt,
    IntInRange(i128, i128),
    IntInSet(Vec<i128>),
    BoundedFloat(f64, f64),
    FloatInSet(Vec<f64>),
    SubSetOfIntSet(Vec<i128>),
    SubSetOfIntRange(i128, i128),
}

pub fn basic_pred_par_type<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, bppt) = alt((
        bppt_basic_par_type,
        bppt_basic_var_type,
        bppt_var_set_of_int,
        bppt_int_in_range,
        bppt_int_in_set,
        bppt_bounded_float,
        bppt_float_in_set,
        bppt_subset_of_int_set,
        bppt_subset_of_int_range,
    ))(input)?;
    Ok((input, bppt))
}

fn bppt_basic_par_type<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E> {
    let (input, bpt) = parameter_types::basic_par_type(input)?;
    Ok((input, BasicPredParType::BasicParType(bpt)))
}

fn bppt_basic_var_type<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, bvt) = variable_types::basic_var_type(input)?;
    Ok((input, BasicPredParType::BasicVarType(bvt)))
}

fn bppt_var_set_of_int<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E> {
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = tag("var")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _) = tag("set")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _) = tag("of")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _) = tag("int")(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    Ok((input, BasicPredParType::VarSetOfInt))
}

fn bppt_int_in_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, (lb, ub)) = variable_types::int_in_range(input)?;
    Ok((input, BasicPredParType::IntInRange(lb, ub)))
}

fn bppt_int_in_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, set) = variable_types::int_in_set(input)?;
    Ok((input, BasicPredParType::IntInSet(set)))
}

fn bppt_bounded_float<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, (lb, ub)) = variable_types::bounded_float(input)?;
    Ok((input, BasicPredParType::BoundedFloat(lb, ub)))
}

fn bppt_float_in_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, set) = variable_types::float_in_set(input)?;
    Ok((input, BasicPredParType::FloatInSet(set)))
}

fn bppt_subset_of_int_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, (lb, ub)) = variable_types::subset_of_int_range(input)?;
    Ok((input, BasicPredParType::SubSetOfIntRange(lb, ub)))
}

fn bppt_subset_of_int_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, set) = variable_types::subset_of_int_set(input)?;
    Ok((input, BasicPredParType::SubSetOfIntSet(set)))
}

#[derive(PartialEq, Clone, Debug)]
pub enum PredParType {
    Basic(BasicPredParType),
    Array {
        ix: PredIndexSet,
        par_type: BasicPredParType,
    },
}

pub fn pred_par_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, PredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, ppt) = alt((ppt_basic_pred_par_type, array_of_pred_index_set))(input)?;
    Ok((input, ppt))
}

fn ppt_basic_pred_par_type<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, PredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, bppt) = basic_pred_par_type(input)?;
    Ok((input, PredParType::Basic(bppt)))
}

fn array_of_pred_index_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, PredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _tag) = tag("array")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _) = char('[')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, ix) = pred_index_set(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, par_type) = basic_pred_par_type(input)?;
    Ok((input, PredParType::Array { ix, par_type }))
}

#[derive(PartialEq, Clone, Debug)]
pub enum PredIndexSet {
    IndexSet(i128),
    Int,
}

fn pred_index_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, PredIndexSet, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, index_set) = alt((pis_int, pis_index_set))(input)?;
    Ok((input, index_set))
}

fn pis_int<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, PredIndexSet, E> {
    let (input, _tag) = tag("int")(input)?;
    Ok((input, PredIndexSet::Int))
}

fn pis_index_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, PredIndexSet, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, ix) = statements::index_set(input)?;
    Ok((input, PredIndexSet::IndexSet(ix.0)))
}
