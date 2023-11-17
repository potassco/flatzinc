use winnow::{
    combinator::alt,
    error::{FromExternalError, ParserError},
    token::tag,
    PResult, Parser,
};

use crate::{
    basic_types::{basic_type, BasicType},
    comments::{space_or_comment0, space_or_comment1},
    primitive_literals::{index_set, IndexSet},
};

#[derive(PartialEq, Clone, Debug)]
pub enum BasicParType {
    BasicType(BasicType),
    SetOfInt,
}

pub fn basic_par_type<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<BasicParType, E> {
    alt((bpt_basic_type, bpt_set_of_int)).parse_next(input)
}

fn bpt_basic_type<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<BasicParType, E> {
    let bt = basic_type(input)?;
    Ok(BasicParType::BasicType(bt))
}

// "set" "of" "int"
// Moved this be a basic-var-type basic-par-type
fn bpt_set_of_int<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<BasicParType, E> {
    tag("set").parse_next(input)?;
    space_or_comment1(input)?;
    tag("of").parse_next(input)?;
    space_or_comment1(input)?;
    tag("int").parse_next(input)?;
    Ok(BasicParType::SetOfInt)
}

#[derive(PartialEq, Clone, Debug)]
pub enum ParType {
    BasicParType(BasicParType),
    Array {
        ix: IndexSet,
        par_type: BasicParType,
    },
}

pub fn par_type<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<ParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    alt((pt_basic_par_type, array_par_type)).parse_next(input)
}
#[test]
fn test_par_type() {
    use crate::IndexSet;
    use winnow::error::ContextError;
    let mut input = "array [1..3] of float";
    assert_eq!(
        par_type::<ContextError<&str>>(&mut input),
        Ok(ParType::Array {
            ix: IndexSet(3),
            par_type: BasicParType::BasicType(BasicType::Float)
        })
    );
}

fn pt_basic_par_type<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<ParType, E> {
    let pt = basic_par_type(input)?;
    Ok(ParType::BasicParType(pt))
}

fn array_par_type<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<ParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    tag("array").parse_next(input)?;
    space_or_comment1(input)?;
    '['.parse_next(input)?;
    space_or_comment0(input)?;
    let ix = index_set(input)?;
    space_or_comment0(input)?;
    ']'.parse_next(input)?;
    space_or_comment1(input)?;
    tag("of").parse_next(input)?;
    space_or_comment1(input)?;
    let par_type = basic_par_type(input)?;
    Ok(ParType::Array { ix, par_type })
}
