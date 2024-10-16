use crate::{
    comments::{separator, space_or_comment0, space_or_comment1},
    expressions::{annotations, expr, Annotation, Expr},
    primitive_literals::identifier,
};
use winnow::{
    combinator::{cut_err, separated},
    error::{AddContext, FromExternalError, ParserError, StrContext},
    PResult, Parser,
};

#[derive(PartialEq, Clone, Debug)]
pub struct ConstraintItem {
    pub id: String,
    pub exprs: Vec<Expr>,
    pub annos: Vec<Annotation>,
}

pub fn constraint_item<'a, E>(input: &mut &'a str) -> PResult<ConstraintItem, E>
where
    E: ParserError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>
        + AddContext<&'a str, StrContext>,
{
    space_or_comment0(input)?;
    "constraint".parse_next(input)?;
    cut_err(constraint_tail.context(StrContext::Label("Error while parsing constraint")))
        .parse_next(input)
}
pub fn constraint_tail<'a, E>(input: &mut &'a str) -> PResult<ConstraintItem, E>
where
    E: ParserError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    space_or_comment1(input)?;
    let id = identifier(input)?;
    '('.parse_next(input)?;
    space_or_comment0(input)?;
    let exprs = separated(1.., expr, separator).parse_next(input)?;
    space_or_comment0(input)?;
    ')'.parse_next(input)?;
    space_or_comment0(input)?;
    let annos = annotations(input)?;
    space_or_comment0(input)?;
    ';'.parse_next(input)?;
    space_or_comment0(input)?;
    Ok(ConstraintItem { id, exprs, annos })
}
#[test]
fn test_constraint_item_1() {
    use crate::{AnnExpr, Annotation, Expr, IntExpr, SetLiteralExpr};
    use winnow::error::ContextError;
    let mut input = "constraint set_in_reif(X_26,1..2,X_52):: defines_var(X_52);";
    assert_eq!(
        constraint_item::<ContextError>(&mut input),
        Ok(ConstraintItem {
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
        })
    );
    let mut input = "constraint array_var_int_element(INT01, w, 2);";
    assert_eq!(
        constraint_item::<ContextError>(&mut input),
        Ok(ConstraintItem {
            id: "array_var_int_element".to_string(),
            exprs: vec![
                Expr::VarParIdentifier("INT01".to_string()),
                Expr::VarParIdentifier("w".to_string()),
                Expr::Int(2)
            ],
            annos: vec![]
        })
    );
    let mut input = "constraint array_var_int_element(INT01, w, 2.0);";
    assert_eq!(
        constraint_item::<ContextError>(&mut input),
        Ok(ConstraintItem {
            id: "array_var_int_element".to_string(),
            exprs: vec![
                Expr::VarParIdentifier("INT01".to_string()),
                Expr::VarParIdentifier("w".to_string()),
                Expr::Float(2.0)
            ],
            annos: vec![]
        })
    );
}
#[test]
fn test_constraint_item_2() {
    use crate::{Expr, IntExpr};
    use winnow::error::ContextError;
    let mut input = "constraint int_lin_eq([-1, 1], [INT01, p], -3);";
    assert_eq!(
        constraint_item::<ContextError>(&mut input),
        Ok(ConstraintItem {
            id: "int_lin_eq".to_string(),
            exprs: vec![
                Expr::ArrayOfInt(vec![IntExpr::Int(-1), IntExpr::Int(1)]),
                Expr::ArrayOfBool(vec![
                    crate::BoolExpr::VarParIdentifier("INT01".to_string()),
                    crate::BoolExpr::VarParIdentifier("p".to_string())
                ]),
                Expr::Int(-3)
            ],
            annos: vec![]
        })
    );
}
#[test]
fn test_constraint_item_3() {
    use crate::{BoolExpr, Expr};
    use winnow::error::ContextError;
    let mut input = "constraint float_lin_eq(X_139,[X_27,X_28,X_29],1.0);";
    assert_eq!(
        constraint_item::<ContextError>(&mut input),
        Ok(ConstraintItem {
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
        })
    );
}
#[test]
fn test_constraint_item_4() {
    use crate::{BoolExpr, Expr};
    use winnow::error::ContextError;
    let mut input = "constraint array_bool_or([X_43,X_44],true);";
    assert_eq!(
        constraint_item::<ContextError>(&mut input),
        Ok(ConstraintItem {
            id: "array_bool_or".to_string(),
            exprs: vec![
                Expr::ArrayOfBool(vec![
                    BoolExpr::VarParIdentifier("X_43".to_string()),
                    BoolExpr::VarParIdentifier("X_44".to_string()),
                ]),
                Expr::Bool(true)
            ],
            annos: vec![]
        })
    );
}
#[test]
fn test_constraint_item_5() {
    use crate::{BoolExpr, Expr};
    use winnow::error::ContextError;
    let mut input = "constraint bool_clause([],[X_81,X_77]);";
    assert_eq!(
        constraint_item::<ContextError>(&mut input),
        Ok(ConstraintItem {
            id: "bool_clause".to_string(),
            exprs: vec![
                Expr::ArrayOfBool(vec![]),
                Expr::ArrayOfBool(vec![
                    BoolExpr::VarParIdentifier("X_81".to_string()),
                    BoolExpr::VarParIdentifier("X_77".to_string())
                ])
            ],
            annos: vec![]
        })
    );
}
#[test]
fn test_constraint_item_6() {
    use winnow::error::ContextError;
    let mut input = "constraintX int_lin_le_reif(X_INTRODUCED_22_,,-2,X_INTRODUCED_58_):: defines_var(X_INTRODUCED_58_);";
    let res = constraint_item::<ContextError>(&mut input);
    assert!(res.is_err());
    assert_eq!("Parsing Failure: ContextError { context: [Label(\"Error while parsing constraint\")], cause: None }", res.unwrap_err().to_string());
}
