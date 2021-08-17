use std::str;

use nom::branch::alt;
use nom::bytes::complete::take_till;
use nom::character::complete::{char, multispace0, multispace1};
use nom::combinator::opt;

use crate::statements::Stmt;
use crate::{IResult, ParseError};

// white space or comments
pub fn space_or_comment0<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, &'a str, E> {
    let (input, s) = alt((comment, multispace0))(input)?;
    Ok((input, s))
}

pub fn space_or_comment1<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, &'a str, E> {
    let (input, s) = alt((comment, multispace1))(input)?;
    Ok((input, s))
}

#[test]
fn test_comment() {
    use nom::error::VerboseError;
    assert_eq!(
        comment::<VerboseError<&str>>("% Comments can have anyth!ng in it really <3"),
        Ok(("", " Comments can have anyth!ng in it really <3".into()))
    );
}

fn comment<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    let (input, _) = multispace0(input)?;
    let (input, _) = char('%')(input)?;
    let (input, string) = take_till(|c| c == '\n')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = opt(comment)(input)?;
    Ok((input, string))
}

pub fn space_or_comment<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Stmt, E> {
    let (input, s) = space_or_comment0(input)?;
    Ok((input, Stmt::Comment(s.into())))
}
