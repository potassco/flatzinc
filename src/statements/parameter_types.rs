use std::str;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::char;

use crate::statements::basic_types::BasicType;
use crate::statements::{basic_types, IndexSet};
use crate::{comments, statements, FromExternalError, IResult, ParseError};

#[derive(PartialEq, Clone, Debug)]
pub enum BasicParType {
    BasicType(BasicType),
    SetOfInt,
}

pub fn basic_par_type<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicParType, E> {
    let (input, bpt) = alt((bpt_basic_type, bpt_set_of_int))(input)?;
    Ok((input, bpt))
}

fn bpt_basic_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicParType, E> {
    let (input, bt) = basic_types::basic_type(input)?;
    Ok((input, BasicParType::BasicType(bt)))
}

// "set" "of" "int"
// Moved this be a basic-var-type basic-par-type
fn bpt_set_of_int<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicParType, E> {
    let (input, _tag) = tag("set")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _tag) = tag("int")(input)?;
    Ok((input, BasicParType::SetOfInt))
}

#[derive(PartialEq, Clone, Debug)]
pub enum ParType {
    BasicParType(BasicParType),
    Array {
        ix: IndexSet,
        par_type: BasicParType,
    },
}

#[test]
fn test_par_type() {
    use crate::statements::IndexSet;
    use nom::error::VerboseError;
    assert_eq!(
        par_type::<VerboseError<&str>>("array [1..3] of float"),
        Ok((
            "",
            ParType::Array {
                ix: IndexSet(3),
                par_type: BasicParType::BasicType(BasicType::Float)
            }
        ))
    );
}

pub fn par_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, ParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, par_type) = alt((pt_basic_par_type, array_par_type))(input)?;
    Ok((input, par_type))
}

fn pt_basic_par_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, ParType, E> {
    let (input, pt) = basic_par_type(input)?;
    Ok((input, ParType::BasicParType(pt)))
}

fn array_par_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, ParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = tag("array")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _) = char('[')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, int) = statements::index_set(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, par_type) = basic_par_type(input)?;
    Ok((
        input,
        ParType::Array {
            ix: IndexSet(int),
            par_type,
        },
    ))
}

#[test]
fn test_basic_pred_par_type() {
    use crate::statements::predicate_types;
    use crate::statements::predicate_types::BasicPredParType;
    use nom::error::VerboseError;
    assert_eq!(
        predicate_types::basic_pred_par_type::<VerboseError<&str>>("var set of int"),
        Ok(("", BasicPredParType::VarSetOfInt))
    );
}

#[test]
fn test_pred_par_type_range() {
    use crate::statements::predicate_types;
    use nom::error::VerboseError;
    assert_eq!(
        predicate_types::pred_par_type::<VerboseError<&str>>("1..3"),
        Ok((
            "",
            predicate_types::PredParType::Basic(predicate_types::BasicPredParType::IntInRange(
                1, 3
            ))
        ))
    );
}

#[test]
fn test_pred_par_type_2() {
    use crate::statements::predicate_types;
    use nom::error::VerboseError;
    assert_eq!(
        predicate_types::pred_par_type::<VerboseError<&str>>("array [1..1] of var set of int"),
        Ok((
            "",
            predicate_types::PredParType::Array {
                ix: predicate_types::PredIndexSet::IndexSet(1),
                par_type: predicate_types::BasicPredParType::VarSetOfInt,
            },
        ))
    );
}

#[test]
fn test_pred_par_type_3() {
    use crate::statements::predicate_types;
    use nom::error::VerboseError;
    assert_eq!(
        predicate_types::pred_par_type::<VerboseError<&str>>("var set of int"),
        Ok((
            "",
            predicate_types::PredParType::Basic(predicate_types::BasicPredParType::VarSetOfInt)
        ))
    );
}

#[test]
fn test_pred_par_type_ident_pair() {
    use crate::statements::predicate_declarations;
    use crate::statements::predicate_types;
    use nom::error::VerboseError;
    assert_eq!(
        predicate_declarations::pred_par_type_ident_pair::<VerboseError<&str>>("var set of int: g"),
        Ok((
            "",
            (
                predicate_types::PredParType::Basic(predicate_types::BasicPredParType::VarSetOfInt),
                "g".to_string()
            )
        ))
    );
}
