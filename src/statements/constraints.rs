use std::str;

use nom::bytes::complete::tag;
use nom::character::complete::char;
use nom::multi::separated_list1;

use crate::expressions::{Annotation, Expr};
use crate::{comments, expressions, primitive_literals, FromExternalError, IResult, ParseError};

#[derive(PartialEq, Clone, Debug)]
pub struct ConstraintItem {
    pub id: String,
    pub exprs: Vec<Expr>,
    pub annos: Vec<Annotation>,
}

pub fn constraint_item<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ConstraintItem, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _tag) = tag("constraint")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, id) = primitive_literals::identifier(input)?;
    let (input, _) = char('(')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, exprs) = separated_list1(char(','), expressions::expr)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(')')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, annos) = expressions::annotations(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(';')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    Ok((input, ConstraintItem { id, exprs, annos }))
}

#[test]
fn test_constraint_item() {
    use crate::expressions::{AnnExpr, Annotation, Expr, IntExpr, SetLiteralExpr};
    use alloc::str;
    use nom::error::VerboseError;
    assert_eq!(
        constraint_item::<VerboseError<&str>>(
            "constraint set_in_reif(X_26,1..2,X_52):: defines_var(X_52);"
        ),
        Ok((
            "",
            ConstraintItem {
                id: "set_in_reif".to_string(),
                exprs: vec![
                    Expr::VarParIdentifier("X_26".to_string()),
                    Expr::Set(SetLiteralExpr::IntInRange(IntExpr::Int(1), IntExpr::Int(2))),
                    Expr::VarParIdentifier("X_52".to_string())
                ],
                annos: vec![Annotation {
                    id: "defines_var".to_string(),
                    expressions: vec![AnnExpr::Expr(Expr::VarParIdentifier("X_52".to_string()))]
                }]
            }
        ))
    );
    assert_eq!(
        constraint_item::<VerboseError<&str>>("constraint array_var_int_element(INT01, w, 2);"),
        Ok((
            "",
            ConstraintItem {
                id: "array_var_int_element".to_string(),
                exprs: vec![
                    Expr::VarParIdentifier("INT01".to_string()),
                    Expr::VarParIdentifier("w".to_string()),
                    Expr::Int(2)
                ],
                annos: vec![]
            }
        ))
    );
    assert_eq!(
        constraint_item::<VerboseError<&str>>("constraint array_var_int_element(INT01, w, 2.0);"),
        Ok((
            "",
            ConstraintItem {
                id: "array_var_int_element".to_string(),
                exprs: vec![
                    Expr::VarParIdentifier("INT01".to_string()),
                    Expr::VarParIdentifier("w".to_string()),
                    Expr::Float(2.0)
                ],
                annos: vec![]
            }
        ))
    );
}

#[test]
fn test_constraint_item_2() {
    use crate::expressions::{Expr, IntExpr};
    use alloc::str;
    use nom::error::VerboseError;
    assert_eq!(
        constraint_item::<VerboseError<&str>>("constraint int_lin_eq([-1, 1], [INT01, p], -3);"),
        Ok((
            "",
            ConstraintItem {
                id: "int_lin_eq".to_string(),
                exprs: vec![
                    Expr::ArrayOfInt(vec![IntExpr::Int(-1), IntExpr::Int(1)]),
                    Expr::ArrayOfInt(vec![
                        IntExpr::VarParIdentifier("INT01".to_string()),
                        IntExpr::VarParIdentifier("p".to_string())
                    ]),
                    Expr::Int(-3)
                ],
                annos: vec![]
            }
        ))
    );
}

#[test]
fn test_constraint_item_3() {
    use crate::expressions::{BoolExpr, Expr};
    use alloc::str;
    use nom::error::VerboseError;
    assert_eq!(
        constraint_item::<VerboseError<&str>>(
            "constraint float_lin_eq(X_139,[X_27,X_28,X_29],1.0);"
        ),
        Ok((
            "",
            ConstraintItem {
                id: "float_lin_eq".to_string(),
                exprs: vec![
                    Expr::VarParIdentifier("X_139".to_string()),
                    Expr::ArrayOfBool(vec![
                        BoolExpr::VarParIdentifier("X_27".to_string()),
                        BoolExpr::VarParIdentifier("X_28".to_string()),
                        BoolExpr::VarParIdentifier("X_29".to_string()),
                    ]),
                    Expr::Float(1.0)
                ],
                annos: vec![]
            }
        ))
    );
}

#[test]
fn test_constraint_item_4() {
    use crate::expressions::{BoolExpr, Expr};
    use alloc::str;
    use nom::error::VerboseError;
    assert_eq!(
        constraint_item::<VerboseError<&str>>("constraint array_bool_or([X_43,X_44],true);"),
        Ok((
            "",
            ConstraintItem {
                id: "array_bool_or".to_string(),
                exprs: vec![
                    Expr::ArrayOfBool(vec![
                        BoolExpr::VarParIdentifier("X_43".to_string()),
                        BoolExpr::VarParIdentifier("X_44".to_string()),
                    ]),
                    Expr::Bool(true)
                ],
                annos: vec![]
            }
        ))
    );
}
