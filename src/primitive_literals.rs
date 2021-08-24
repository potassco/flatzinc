use std::str;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, one_of},
    combinator::{map_res, opt, value},
    error::{ErrorKind, FromExternalError, ParseError},
    IResult,
};

pub fn identifier<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, String, E> {
    let (input, first) = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")(input)?;
    let (input, rest) = take_while(is_identifier_rest)(input)?;
    let combine = format!("{}{}", first, rest);
    // check for reserved key words
    if is_reserved_key_word(&combine) {
        Err(crate::Err::Error(ParseError::from_error_kind(
            input,
            ErrorKind::IsA,
        )))
    } else {
        Ok((input, combine))
    }
}

pub fn var_par_identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, String, E> {
    let (input, first) = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")(input)?;
    let (input, rest) = take_while(is_identifier_rest)(input)?;
    let combine = format!("{}{}", first, rest);
    // check for reserved key words
    if is_reserved_key_word(&combine) {
        Err(crate::Err::Error(ParseError::from_error_kind(
            input,
            ErrorKind::IsA,
        )))
    } else {
        Ok((input, combine))
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
    //one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789")(input.into()) {
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

pub fn bool_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, bool, E> {
    alt((value(true, tag("true")), value(false, tag("false"))))(input)
}

pub fn int_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i128, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = crate::comments::space_or_comment0(input)?;
    let (input, int) = alt((decimal, hexadecimal, octal))(input)?;
    Ok((input, int as i128))
}

fn decimal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i128, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, negation) = opt(char('-'))(input)?;
    let (input, int) = map_res(take_while1(is_dec_digit), from_dec)(input)?;
    if negation.is_some() {
        Ok((input, -(int as i128)))
    } else {
        Ok((input, int as i128))
    }
}

#[test]
fn test_hex() {
    use nom::error::VerboseError;
    assert_eq!(hexadecimal::<VerboseError<&str>>("-0x2f"), Ok(("", -47)));
}

fn hexadecimal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i128, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, negation) = opt(char('-'))(input)?;
    let (input, _tag) = tag("0x")(input)?;
    let (input, int) = map_res(take_while1(is_hex_digit), from_hex)(input)?;
    if negation.is_some() {
        Ok((input, -(int as i128)))
    } else {
        Ok((input, int as i128))
    }
}

#[test]
fn test_oct() {
    use nom::error::VerboseError;
    assert_eq!(octal::<VerboseError<&str>>("0o21"), Ok(("", 17)));
}

fn octal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i128, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, negation) = opt(char('-'))(input)?;
    let (input, _tag) = tag("0o")(input)?;
    let (input, int) = map_res(take_while1(is_oct_digit), from_oct)(input)?;
    if negation.is_some() {
        Ok((input, -(int as i128)))
    } else {
        Ok((input, int as i128))
    }
}

fn from_hex(input: &str) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input, 16)
}

fn is_hex_digit(c: char) -> bool {
    c.is_digit(16)
}

fn from_oct(input: &str) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input, 8)
}

fn is_oct_digit(c: char) -> bool {
    c.is_digit(8)
}

fn from_dec(input: &str) -> Result<u128, std::num::ParseIntError> {
    input.parse::<u128>()
}

fn from_float(input: &str) -> Result<f64, std::num::ParseFloatError> {
    input.parse::<f64>()
}

fn is_dec_digit(c: char) -> bool {
    c.is_digit(10)
}

#[test]
fn test_float() {
    use nom::error::VerboseError;
    //TODO should return error
    // float_literal::<VerboseError<&str>>("5")
    assert_eq!(
        float_literal::<VerboseError<&str>>("023.21"),
        Ok(("", 023.21))
    );
    assert_eq!(
        float_literal::<VerboseError<&str>>("0023.21E-098"),
        Ok(("", 0023.21E-098))
    );
    assert_eq!(
        float_literal::<VerboseError<&str>>("0023.21e+098"),
        Ok(("", 0023.21e+098))
    );
    assert_eq!(
        float_literal::<VerboseError<&str>>("002e+098"),
        Ok(("", 002e+098))
    );
    assert_eq!(float_literal::<VerboseError<&str>>("0.21"), Ok(("", 0.21)));
    assert_eq!(float_literal::<VerboseError<&str>>("1.0,"), Ok((",", 1.0)));

    assert_eq!(float_literal::<VerboseError<&str>>("0.000000000000000000000000000000007609999999000000000000000000000000760999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999900000000000000000000000764DD4DDDDDDDDD%
    "), Err(crate::Err::Error(VerboseError { errors: vec![("0.000000000000000000000000000000007609999999000000000000000000000000760999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999900000000000000000000000764DD4DDDDDDDDD%\n    ", nom::error::VerboseErrorKind::Nom(nom::error::ErrorKind::MapRes))] })));
}

pub fn float_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, f64, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = crate::comments::space_or_comment0(input)?;
    let (input, f) = fz_float(input)?;
    Ok((input, f))
}

fn fz_float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, f64, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input2, fl) = map_res(alt((fz_float1, fz_float2)), from_float)(input)?;
    Ok((input2, fl))
}

fn fz_float1<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    let (input1, sign) = opt(char('-'))(input)?;
    let (input2, a) = take_while1(is_dec_digit)(input1)?;
    let (input3, _) = char('.')(input2)?;
    let (input4, b) = take_while1(is_dec_digit)(input3)?;
    let (input5, rest) = opt(bpart)(input4)?;
    let len = if sign.is_some() {
        if let Some(rest) = rest {
            1 + a.len() + 1 + b.len() + rest.len()
        } else {
            1 + a.len() + 1 + b.len()
        }
    } else if let Some(rest) = rest {
        a.len() + 1 + b.len() + rest.len()
    } else {
        a.len() + 1 + b.len()
    };
    Ok((input5, &input[..len]))
}

fn fz_float2<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    let (input1, sign) = opt(char('-'))(input)?;
    let (input2, digits) = take_while1(is_dec_digit)(input1)?;
    let (input3, rest) = bpart(input2)?;
    let len = if sign.is_some() {
        1 + digits.len() + rest.len()
    } else {
        digits.len() + rest.len()
    };
    Ok((input3, &input[..len]))
}

fn bpart<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, String, E> {
    let (input, e) = alt((tag("e"), tag("E")))(input)?;
    let (input, sign) = opt(alt((tag("-"), tag("+"))))(input)?;
    let (input, digits) = take_while1(is_dec_digit)(input)?;
    if let Some(sign) = sign {
        Ok((input, format!("{}{}{}", e, sign, digits)))
    } else {
        Ok((input, format!("{}{}", e, digits)))
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct IndexSet(pub i128);

pub fn index_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, IndexSet, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = char('1')(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, int) = int_literal(input)?;
    Ok((input, IndexSet(int)))
}
