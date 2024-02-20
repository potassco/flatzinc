use winnow::{
    ascii::{multispace0, multispace1},
    combinator::{alt, opt},
    error::ParserError,
    token::take_till,
    PResult, Parser,
};

use crate::statements::Stmt;

pub fn space_or_comment<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Stmt, E> {
    let s = space_or_comment0(input)?;
    Ok(Stmt::Comment(s.into()))
}
pub fn space_or_comment0<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<&'a str, E> {
    alt((comment, multispace0)).parse_next(input)
}
pub fn space_or_comment1<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<&'a str, E> {
    alt((comment, multispace1)).parse_next(input)
}
fn comment<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<&'a str, E> {
    multispace0.parse_next(input)?;
    '%'.parse_next(input)?;
    let string = take_till(0.., |c| c == '\n').parse_next(input)?;
    multispace0.parse_next(input)?;
    opt(comment).parse_next(input)?;
    Ok(string)
}
#[test]
fn test_comment() {
    use winnow::error::ContextError;
    let mut input = "% Comments can have anyth!ng in it really <3";
    assert_eq!(
        comment::<ContextError<&str>>(&mut input),
        Ok(" Comments can have anyth!ng in it really <3".into())
    );
}
#[test]
fn test_comment2() {
    use winnow::error::ContextError;
    let mut input = "5 % Comments can have anyth!ng in it really <3";
    let res = comment::<ContextError<&str>>(&mut input);
    assert!(res.is_err());
}
