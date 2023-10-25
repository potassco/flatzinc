use winnow::{combinator::alt, error::ParserError, token::tag, PResult, Parser};

#[derive(PartialEq, Clone, Debug)]
pub enum BasicType {
    Bool,
    Int,
    Float,
}

pub fn basic_type<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<BasicType, E> {
    let bt = alt((bool, float, int)).parse_next(input)?;
    Ok(bt)
}

fn bool<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<BasicType, E> {
    tag("bool").parse_next(input)?;
    Ok(BasicType::Bool)
}

fn int<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<BasicType, E> {
    tag("int").parse_next(input)?;
    Ok(BasicType::Int)
}

fn float<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<BasicType, E> {
    tag("float").parse_next(input)?;
    Ok(BasicType::Float)
}
