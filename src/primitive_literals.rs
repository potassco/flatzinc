use winnow::{
    combinator::{alt, opt},
    error::{ErrorKind, FromExternalError, ParserError},
    token::{one_of, tag, take_while},
    PResult, Parser,
};

use crate::comments::space_or_comment0;

pub fn identifier<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<String, E> {
    let first = one_of([
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
        's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
        'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    ])
    .parse_next(input)?;
    let rest = take_while(0.., is_identifier_rest).parse_next(input)?;
    let combine = format!("{}{}", first, rest);
    // check for reserved key words
    if is_reserved_key_word(&combine) {
        Err(winnow::error::ErrMode::Backtrack(
            ParserError::from_error_kind(input, ErrorKind::Token),
        ))
    } else {
        Ok(combine)
    }
}

pub fn var_par_identifier<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<String, E> {
    let first = one_of([
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
        's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
        'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '_',
    ])
    .parse_next(input)?;

    let rest = take_while(0.., is_identifier_rest).parse_next(input)?;
    let combine = format!("{}{}", first, rest);
    // check for reserved key words
    if is_reserved_key_word(&combine) {
        Err(winnow::error::ErrMode::Backtrack(
            ParserError::from_error_kind(input, ErrorKind::Token),
        ))
    } else {
        Ok(combine)
    }
}

fn is_reserved_key_word(string: &str) -> bool {
    matches!(
        string,
        "annotation"
            | "any"
            | "array"
            | "bool"
            | "case"
            | "constraint"
            | "diff"
            | "div"
            | "else"
            | "elseif"
            | "endif"
            | "enum"
            | "false"
            | "float"
            | "function"
            | "if"
            | "in"
            | "include"
            | "int"
            | "intersect"
            | "let"
            | "list"
            | "maximize"
            | "minimize"
            | "mod"
            | "not"
            | "of"
            | "satisfy"
            | "subset"
            | "superset"
            | "output"
            | "par"
            | "predicate"
            | "record"
            | "set"
            | "solve"
            | "string"
            | "symdiff"
            | "test"
            | "then"
            | "true"
            | "tuple"
            | "union"
            | "type"
            | "var"
            | "where"
            | "xor"
    )
}

fn is_identifier_rest(c: char) -> bool {
    matches!(
        c,
        'a' | 'b'
            | 'c'
            | 'd'
            | 'e'
            | 'f'
            | 'g'
            | 'h'
            | 'i'
            | 'j'
            | 'k'
            | 'l'
            | 'm'
            | 'n'
            | 'o'
            | 'p'
            | 'q'
            | 'r'
            | 's'
            | 't'
            | 'u'
            | 'v'
            | 'w'
            | 'x'
            | 'y'
            | 'z'
            | 'A'
            | 'B'
            | 'C'
            | 'D'
            | 'E'
            | 'F'
            | 'G'
            | 'H'
            | 'I'
            | 'J'
            | 'K'
            | 'L'
            | 'M'
            | 'N'
            | 'O'
            | 'P'
            | 'Q'
            | 'R'
            | 'S'
            | 'T'
            | 'U'
            | 'V'
            | 'W'
            | 'X'
            | 'Y'
            | 'Z'
            | '_'
            | '0'
            | '1'
            | '2'
            | '3'
            | '4'
            | '5'
            | '6'
            | '7'
            | '8'
            | '9'
    )
}

pub fn bool_literal<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<bool, E> {
    alt((tag("true").value(true), tag("false").value(false))).parse_next(input)
}

pub fn int_literal<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<i128, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    crate::comments::space_or_comment0(input)?;
    alt((decimal, hexadecimal, octal)).parse_next(input)
}
#[test]
fn test_int_literal() {
    use winnow::error::ContextError;
    let mut input = "1";
    assert_eq!(int_literal::<ContextError<&str>>(&mut input), Ok(1));
}

fn decimal<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<i128, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let negation = opt('-').parse_next(input)?;
    let int = take_while(1.., is_dec_digit).parse_next(input)?;
    let int = int
        .parse::<i128>()
        .map_err(|e| winnow::error::ErrMode::from_external_error(input, ErrorKind::Verify, e))?;

    if negation.is_some() {
        Ok(-int)
    } else {
        Ok(int)
    }
}
#[test]
fn test_decimal() {
    use winnow::error::ContextError;
    let mut input = "170141183460469231731687303715884105727";
    assert_eq!(
        decimal::<ContextError<&str>>(&mut input),
        Ok(170141183460469231731687303715884105727)
    );
}
#[test]
fn test_decimal2() {
    use winnow::error::ContextError;
    let mut input = "-170141183460469231731687303715884105727";
    assert_eq!(
        decimal::<ContextError<&str>>(&mut input),
        Ok(-170141183460469231731687303715884105727)
    );
}
#[test]
fn test_decimal3() {
    use winnow::error::ContextError;
    let mut input = "170141183460469231731687303715884105728";
    //Should fail because of overflow
    assert!(decimal::<ContextError<&str>>(&mut input).is_err());
}

fn hexadecimal<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<i128, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let negation = opt('-').parse_next(input)?;
    "0x".parse_next(input)?;
    let int = take_while(1.., is_hex_digit).parse_next(input)?;
    let int = i128::from_str_radix(int, 16)
        .map_err(|e| winnow::error::ErrMode::from_external_error(input, ErrorKind::Verify, e))?;

    if negation.is_some() {
        Ok(-int)
    } else {
        Ok(int)
    }
}
#[test]
fn test_hex() {
    use winnow::error::ContextError;
    let mut input = "-0x2f";
    assert_eq!(hexadecimal::<ContextError<&str>>(&mut input), Ok(-47));
}

fn octal<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<i128, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let negation = opt('-').parse_next(input)?;
    "0o".parse_next(input)?;
    let int = take_while(1.., is_oct_digit).parse_next(input)?;
    let int = i128::from_str_radix(int, 8)
        .map_err(|e| winnow::error::ErrMode::from_external_error(input, ErrorKind::Verify, e))?;
    if negation.is_some() {
        Ok(-int)
    } else {
        Ok(int)
    }
}
#[test]
fn test_oct() {
    use winnow::error::ContextError;
    let mut input = "0o200000000000000000000000000000000001";
    assert_eq!(
        octal::<ContextError<&str>>(&mut input),
        Ok(81129638414606681695789005144065)
    );
}

fn is_hex_digit(c: char) -> bool {
    c.is_ascii_hexdigit()
}

fn is_oct_digit(c: char) -> bool {
    c.is_digit(8)
}

fn is_dec_digit(c: char) -> bool {
    c.is_ascii_digit()
}

pub fn float_literal<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<f64, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    crate::comments::space_or_comment0(input)?;
    fz_float(input)
}
#[test]
fn test_float_literal() {
    use winnow::error::ContextError;
    //TODO should return error
    // float_literal::<ContextError<&str>>("5")
    let mut input = "023.21";
    assert_eq!(float_literal::<ContextError<&str>>(&mut input), Ok(023.21));
    let mut input = "0023.21E-098";
    assert_eq!(
        float_literal::<ContextError<&str>>(&mut input),
        Ok(0023.21E-098)
    );
    let mut input = "0023.21e+098";
    assert_eq!(
        float_literal::<ContextError<&str>>(&mut input),
        Ok(0023.21e+098)
    );
    let mut input = "002e+098";
    assert_eq!(
        float_literal::<ContextError<&str>>(&mut input),
        Ok(002e+098)
    );
    let mut input = "0.21";
    assert_eq!(float_literal::<ContextError<&str>>(&mut input), Ok(0.21));
    let mut input = "1.0,";
    assert_eq!(float_literal::<ContextError<&str>>(&mut input), Ok(1.0));

    let mut input = "0.000000000000000000000000000000007609999999000000000000000000000000760999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999900000000000000000000000764DD4DDDDDDDDD%
    ";
    assert_eq!(
        float_literal::<ContextError<&str>>(&mut input),
        Ok(0.000000000000000000000000000000007609999999)
    );
}

fn fz_float<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<f64, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let mut fl = alt((fz_float1, fz_float2)).parse_next(input)?;
    winnow::ascii::float.parse_next(&mut fl)
}

fn fz_float1<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<&'a str, E> {
    let sign = opt('-');

    let pre = take_while(1.., is_dec_digit);
    let post = take_while(1.., is_dec_digit);
    let rest = opt(bpart);

    (sign, pre, '.', post, rest).recognize().parse_next(input)
}

fn fz_float2<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<&'a str, E> {
    let sign = opt('-');
    let digits = take_while(1.., is_dec_digit);
    let e = alt(('e', 'E'));
    let sign2 = opt(alt(("-", "+")));
    let digits2 = take_while(1.., is_dec_digit);
    (sign, digits, e, sign2, digits2)
        .recognize()
        .parse_next(input)
}

fn bpart<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<String, E> {
    let e = alt(('e', 'E')).parse_next(input)?;
    let sign = opt(alt(("-", "+"))).parse_next(input)?;
    let digits = take_while(1.., is_dec_digit).parse_next(input)?;
    if let Some(sign) = sign {
        Ok(format!("{}{}{}", e, sign, digits))
    } else {
        Ok(format!("{}{}", e, digits))
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct IndexSet(pub i128);

pub fn index_set<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<IndexSet, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    '1'.parse_next(input)?;
    space_or_comment0(input)?;
    "..".parse_next(input)?;
    space_or_comment0(input)?;
    let int = int_literal.parse_next(input)?;
    Ok(IndexSet(int))
}
