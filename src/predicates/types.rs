use winnow::{
    combinator::alt,
    error::{FromExternalError, ParserError},
    token::tag,
    PResult, Parser,
};

use crate::{
    comments::{space_or_comment0, space_or_comment1},
    parameters::types::{basic_par_type, BasicParType},
    primitive_literals::index_set,
    variables::types::{
        basic_var_type, bounded_float, float_in_set, int_in_range, int_in_set, subset_of_int_range,
        subset_of_int_set, BasicVarType,
    },
};

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

pub fn basic_pred_par_type<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    alt((
        bppt_basic_par_type,
        bppt_basic_var_type,
        bppt_var_set_of_int,
        bppt_int_in_range,
        bppt_int_in_set,
        bppt_bounded_float,
        bppt_float_in_set,
        bppt_subset_of_int_set,
        bppt_subset_of_int_range,
    ))
    .parse_next(input)
}
#[test]
fn test_basic_pred_par_type() {
    use crate::predicates::types;
    use crate::predicates::types::BasicPredParType;
    use winnow::error::ContextError;
    let mut input = "var set of int";
    assert_eq!(
        types::basic_pred_par_type::<ContextError<&str>>(&mut input),
        Ok(BasicPredParType::VarSetOfInt)
    );
}
fn bppt_basic_par_type<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<BasicPredParType, E> {
    let bpt = basic_par_type(input)?;
    Ok(BasicPredParType::BasicParType(bpt))
}

fn bppt_basic_var_type<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let bvt = basic_var_type(input)?;
    Ok(BasicPredParType::BasicVarType(bvt))
}

fn bppt_var_set_of_int<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<BasicPredParType, E> {
    space_or_comment0(input)?;
    tag("var").parse_next(input)?;
    space_or_comment1(input)?;
    tag("set").parse_next(input)?;
    space_or_comment1(input)?;
    tag("of").parse_next(input)?;
    space_or_comment1(input)?;
    tag("int").parse_next(input)?;
    space_or_comment0(input)?;
    Ok(BasicPredParType::VarSetOfInt)
}

fn bppt_int_in_range<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (lb, ub) = int_in_range(input)?;
    Ok(BasicPredParType::IntInRange(lb, ub))
}

fn bppt_int_in_set<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let set = int_in_set(input)?;
    Ok(BasicPredParType::IntInSet(set))
}

fn bppt_bounded_float<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (lb, ub) = bounded_float(input)?;
    Ok(BasicPredParType::BoundedFloat(lb, ub))
}

fn bppt_float_in_set<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let set = float_in_set(input)?;
    Ok(BasicPredParType::FloatInSet(set))
}

fn bppt_subset_of_int_range<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (lb, ub) = subset_of_int_range(input)?;
    Ok(BasicPredParType::SubSetOfIntRange(lb, ub))
}

fn bppt_subset_of_int_set<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let set = subset_of_int_set(input)?;
    Ok(BasicPredParType::SubSetOfIntSet(set))
}

#[derive(PartialEq, Clone, Debug)]
pub enum PredParType {
    Basic(BasicPredParType),
    Array {
        ix: PredIndexSet,
        par_type: BasicPredParType,
    },
}

pub fn pred_par_type<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<PredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    alt((ppt_basic_pred_par_type, array_of_pred_index_set)).parse_next(input)
}
#[test]
fn test_pred_par_type_range() {
    use crate::predicates::types;
    use winnow::error::ContextError;
    let mut input = "1..3";
    assert_eq!(
        types::pred_par_type::<ContextError<&str>>(&mut input),
        Ok(types::PredParType::Basic(
            types::BasicPredParType::IntInRange(1, 3)
        ))
    );
}
#[test]
fn test_pred_par_type_2() {
    use crate::predicates::types;
    use winnow::error::ContextError;
    let mut input = "array [1..1] of var set of int";
    assert_eq!(
        pred_par_type::<ContextError<&str>>(&mut input),
        Ok(PredParType::Array {
            ix: types::PredIndexSet::IndexSet(1),
            par_type: types::BasicPredParType::VarSetOfInt,
        },)
    );
}
#[test]
fn test_pred_par_type_3() {
    use crate::predicates::types;
    use winnow::error::ContextError;
    let mut input = "var set of int";
    assert_eq!(
        types::pred_par_type::<ContextError<&str>>(&mut input),
        Ok(types::PredParType::Basic(
            types::BasicPredParType::VarSetOfInt
        ))
    );
}
fn ppt_basic_pred_par_type<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<PredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let bppt = basic_pred_par_type(input)?;
    Ok(PredParType::Basic(bppt))
}

fn array_of_pred_index_set<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<PredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    space_or_comment0(input)?;
    tag("array").parse_next(input)?;
    space_or_comment1(input)?;
    '['.parse_next(input)?;
    space_or_comment0(input)?;
    let ix = pred_index_set(input)?;
    space_or_comment0(input)?;
    ']'.parse_next(input)?;
    space_or_comment1(input)?;
    tag("of").parse_next(input)?;
    space_or_comment1(input)?;
    let par_type = basic_pred_par_type(input)?;
    Ok(PredParType::Array { ix, par_type })
}

#[derive(PartialEq, Clone, Debug)]
pub enum PredIndexSet {
    IndexSet(i128),
    Int,
}

fn pred_index_set<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<PredIndexSet, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    alt((pis_int, pis_index_set)).parse_next(input)
}

fn pis_int<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<PredIndexSet, E> {
    tag("int").parse_next(input)?;
    Ok(PredIndexSet::Int)
}

fn pis_index_set<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<PredIndexSet, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let ix = index_set(input)?;
    Ok(PredIndexSet::IndexSet(ix.0))
}
