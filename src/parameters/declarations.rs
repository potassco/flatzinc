use std::str;

use nom::character::complete::char;

use crate::basic_types::BasicType;
use crate::expressions::SetLiteral;
use crate::parameters::types;
use crate::parameters::types::{BasicParType, ParType};
use crate::primitive_literals::IndexSet;
use crate::{comments, expressions, primitive_literals, FromExternalError, IResult, ParseError};

#[derive(PartialEq, Clone, Debug)]
pub enum ParDeclItem {
    Bool {
        id: String,
        bool: bool,
    },
    Int {
        id: String,
        int: i128,
    },
    Float {
        id: String,
        float: f64,
    },
    SetOfInt {
        id: String,
        set_literal: SetLiteral,
    },
    ArrayOfBool {
        ix: IndexSet,
        id: String,
        v: Vec<bool>,
    },
    ArrayOfInt {
        ix: IndexSet,
        id: String,
        v: Vec<i128>,
    },
    ArrayOfFloat {
        ix: IndexSet,
        id: String,
        v: Vec<f64>,
    },
    ArrayOfSet {
        ix: IndexSet,
        id: String,
        v: Vec<SetLiteral>,
    },
}

pub fn par_decl_item<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, ParDeclItem, E>
where
    E: ParseError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, ptype) = types::par_type(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, id) = primitive_literals::var_par_identifier(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    match ptype {
        ParType::BasicParType(bpt) => match bpt {
            BasicParType::BasicType(bt) => match bt {
                BasicType::Bool => {
                    let (input, bool) = primitive_literals::bool_literal(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    Ok((input, ParDeclItem::Bool { id, bool }))
                }
                BasicType::Int => {
                    let (input, int) = primitive_literals::int_literal(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    Ok((input, ParDeclItem::Int { id, int }))
                }
                BasicType::Float => {
                    let (input, float) = primitive_literals::float_literal(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    Ok((input, ParDeclItem::Float { id, float }))
                }
            },
            BasicParType::SetOfInt => {
                let (input, set_literal) = expressions::set_literal(input)?;
                let (input, _) = comments::space_or_comment0(input)?;
                let (input, _) = char(';')(input)?;
                let (input, _) = comments::space_or_comment0(input)?;
                Ok((input, ParDeclItem::SetOfInt { id, set_literal }))
            }
        },
        ParType::Array { ix, par_type } => match par_type {
            BasicParType::BasicType(bt) => match bt {
                BasicType::Bool => {
                    let (input, v) = expressions::array_of_bool_literal(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    Ok((input, ParDeclItem::ArrayOfBool { ix, id, v }))
                }
                BasicType::Int => {
                    let (input, v) = expressions::array_of_int_literal(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    Ok((input, ParDeclItem::ArrayOfInt { ix, id, v }))
                }
                BasicType::Float => {
                    let (input, v) = expressions::array_of_float_literal(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    Ok((input, ParDeclItem::ArrayOfFloat { ix, id, v }))
                }
            },
            BasicParType::SetOfInt => {
                let (input, v) = expressions::array_of_set_literal(input)?;
                let (input, _) = comments::space_or_comment0(input)?;
                let (input, _) = char(';')(input)?;
                let (input, _) = comments::space_or_comment0(input)?;
                Ok((input, ParDeclItem::ArrayOfSet { ix, id, v }))
            }
        },
    }
}

#[test]
fn test_par_decl_item_1() {
    use crate::primitive_literals::IndexSet;
    use nom::error::VerboseError;
    assert_eq!(
        par_decl_item::<VerboseError<&str>>("array [1..3] of float: X_139 = [1.0,1.0,1.0];"),
        Ok((
            "",
            ParDeclItem::ArrayOfFloat {
                ix: IndexSet(3),
                id: "X_139".to_string(),
                v: vec![1.0, 1.0, 1.0]
            }
        ))
    );
}

#[test]
#[should_panic]
fn test_par_decl_item_2() {
    use nom::error::VerboseError;
    use std::str;
    par_decl_item::<VerboseError<&str>>("bool : b2 = b1;").unwrap();
}

#[test]
fn test_par_decl_item_3() {
    use crate::expressions::SetLiteral;
    use crate::primitive_literals::IndexSet;
    use nom::error::VerboseError;
    assert_eq!(
        par_decl_item::<VerboseError<&str>>("array [1..3] of set of int : h = [{42,17},1..5,{}];"),
        Ok((
            "",
            ParDeclItem::ArrayOfSet {
                ix: IndexSet(3),
                id: "h".to_string(),
                v: vec![
                    SetLiteral::SetInts(vec![42, 17]),
                    SetLiteral::IntRange(1, 5),
                    SetLiteral::SetInts(vec![])
                ]
            }
        ))
    );
}
