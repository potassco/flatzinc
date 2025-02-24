use winnow::{ModalResult, Parser, combinator::alt, error::ParserError};

/// Enum representing different basic types.
#[derive(PartialEq, Clone, Debug)]
pub enum BasicType {
    Bool,
    Int,
    Float,
}

/// Parses a basic type from the input string.
pub fn basic_type<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> ModalResult<BasicType, E> {
    let bt = alt((bool, float, int)).parse_next(input)?;
    Ok(bt)
}

fn bool<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> ModalResult<BasicType, E> {
    "bool".parse_next(input)?;
    Ok(BasicType::Bool)
}

fn int<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> ModalResult<BasicType, E> {
    "int".parse_next(input)?;
    Ok(BasicType::Int)
}

fn float<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> ModalResult<BasicType, E> {
    "float".parse_next(input)?;
    Ok(BasicType::Float)
}
