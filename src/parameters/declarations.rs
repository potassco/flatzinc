use winnow::{
    error::{FromExternalError, ParserError},
    PResult, Parser,
};

use crate::{
    basic_types::BasicType,
    comments::space_or_comment0,
    expressions::{
        array_of_bool_literal, array_of_float_literal, array_of_int_literal, array_of_set_literal,
        set_literal, SetLiteral,
    },
    parameters::types::{par_type, BasicParType, ParType},
    primitive_literals::{bool_literal, float_literal, int_literal, var_par_identifier, IndexSet},
};

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

pub fn par_decl_item<'a, E>(input: &mut &'a str) -> PResult<ParDeclItem, E>
where
    E: ParserError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    space_or_comment0(input)?;
    let ptype = par_type(input)?;
    space_or_comment0(input)?;
    ':'.parse_next(input)?;
    space_or_comment0(input)?;
    let id = var_par_identifier(input)?;
    space_or_comment0(input)?;
    '='.parse_next(input)?;
    space_or_comment0(input)?;
    match ptype {
        ParType::BasicParType(bpt) => match bpt {
            BasicParType::BasicType(bt) => match bt {
                BasicType::Bool => {
                    let bool = bool_literal(input)?;
                    space_or_comment0(input)?;
                    ';'.parse_next(input)?;
                    space_or_comment0(input)?;
                    Ok(ParDeclItem::Bool { id, bool })
                }
                BasicType::Int => {
                    let int = int_literal(input)?;
                    space_or_comment0(input)?;
                    ';'.parse_next(input)?;
                    space_or_comment0(input)?;
                    Ok(ParDeclItem::Int { id, int })
                }
                BasicType::Float => {
                    let float = float_literal(input)?;
                    space_or_comment0(input)?;
                    ';'.parse_next(input)?;
                    space_or_comment0(input)?;
                    Ok(ParDeclItem::Float { id, float })
                }
            },
            BasicParType::SetOfInt => {
                let set_literal = set_literal(input)?;
                space_or_comment0(input)?;
                ';'.parse_next(input)?;
                space_or_comment0(input)?;
                Ok(ParDeclItem::SetOfInt { id, set_literal })
            }
        },
        ParType::Array { ix, par_type } => match par_type {
            BasicParType::BasicType(bt) => match bt {
                BasicType::Bool => {
                    let v = array_of_bool_literal(input)?;
                    space_or_comment0(input)?;
                    ';'.parse_next(input)?;
                    space_or_comment0(input)?;
                    Ok(ParDeclItem::ArrayOfBool { ix, id, v })
                }
                BasicType::Int => {
                    let v = array_of_int_literal(input)?;
                    space_or_comment0(input)?;
                    ';'.parse_next(input)?;
                    space_or_comment0(input)?;
                    Ok(ParDeclItem::ArrayOfInt { ix, id, v })
                }
                BasicType::Float => {
                    let v = array_of_float_literal(input)?;
                    space_or_comment0(input)?;
                    ';'.parse_next(input)?;
                    space_or_comment0(input)?;
                    Ok(ParDeclItem::ArrayOfFloat { ix, id, v })
                }
            },
            BasicParType::SetOfInt => {
                let v = array_of_set_literal(input)?;
                space_or_comment0(input)?;
                ';'.parse_next(input)?;
                space_or_comment0(input)?;
                Ok(ParDeclItem::ArrayOfSet { ix, id, v })
            }
        },
    }
}
#[test]
fn test_par_decl_item_1() {
    use crate::IndexSet;
    use winnow::error::ContextError;
    let mut input = "array [1..3] of  float: X_139 = [1.0,1.0,1.0];";
    assert_eq!(
        par_decl_item::<ContextError>(&mut input),
        Ok(ParDeclItem::ArrayOfFloat {
            ix: IndexSet(3),
            id: "X_139".to_string(),
            v: vec![1.0, 1.0, 1.0]
        })
    );
}
#[test]
#[should_panic]
fn test_par_decl_item_2() {
    use winnow::error::ContextError;
    let mut input = "bool : b2 = b1;";
    par_decl_item::<ContextError>(&mut input).unwrap();
}
#[test]
fn test_par_decl_item_3() {
    use crate::IndexSet;
    use crate::SetLiteral;
    use winnow::error::ContextError;
    let mut input = "array [1..3] of set of int : h = [{42,17},1..5,{}];";
    assert_eq!(
        par_decl_item::<ContextError>(&mut input),
        Ok(ParDeclItem::ArrayOfSet {
            ix: IndexSet(3),
            id: "h".to_string(),
            v: vec![
                SetLiteral::SetInts(vec![42, 17]),
                SetLiteral::IntRange(1, 5),
                SetLiteral::SetInts(vec![])
            ]
        })
    );
}
