use std::str;

use nom::{branch::alt, bytes::complete::tag, error::ParseError, IResult};

#[derive(PartialEq, Clone, Debug)]
pub enum BasicType {
    Bool,
    Int,
    Float,
}

pub fn basic_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicType, E> {
    let (input, bt) = alt((bool, float, int))(input)?;
    Ok((input, bt))
}

fn bool<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicType, E> {
    let (input, _tag) = tag("bool")(input)?;
    Ok((input, BasicType::Bool))
}

fn int<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicType, E> {
    let (input, _tag) = tag("int")(input)?;
    Ok((input, BasicType::Int))
}

fn float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicType, E> {
    let (input, _tag) = tag("float")(input)?;
    Ok((input, BasicType::Float))
}
