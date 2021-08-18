use std::str;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::char;

use crate::expressions::{Annotations, BoolExpr, FloatExpr, IntExpr, SetExpr};
use crate::{comments, expressions, FromExternalError, IResult, ParseError};

#[derive(PartialEq, Clone, Debug)]
pub struct SolveItem {
    pub goal: Goal,
    pub annotations: Annotations,
}

pub fn solve_item<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SolveItem, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = tag("solve")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, annotations) = expressions::annotations(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, goal) = alt((
        satisfy,
        optimize_bool,
        optimize_int,
        optimize_float,
        optimize_set,
    ))(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(';')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    Ok((input, SolveItem { goal, annotations }))
}

#[derive(PartialEq, Clone, Debug)]
pub enum Goal {
    Satisfy,
    OptimizeBool(OptimizationType, BoolExpr),
    OptimizeInt(OptimizationType, IntExpr),
    OptimizeFloat(OptimizationType, FloatExpr),
    OptimizeSet(OptimizationType, SetExpr),
}

#[derive(PartialEq, Clone, Debug)]
pub enum OptimizationType {
    Minimize,
    Maximize,
}

pub fn satisfy<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Goal, E> {
    let (input, _) = tag("satisfy")(input)?;
    Ok((input, Goal::Satisfy))
}

fn opt_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, OptimizationType, E> {
    alt((minimize, maximize))(input)
}

fn minimize<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, OptimizationType, E> {
    let (input, _) = tag("minimize")(input)?;
    Ok((input, OptimizationType::Minimize))
}

fn maximize<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, OptimizationType, E> {
    let (input, _) = tag("maximize")(input)?;
    Ok((input, OptimizationType::Maximize))
}

pub fn optimize_bool<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Goal, E> {
    let (input, opt_type) = opt_type(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, be) = expressions::bool_expr(input)?;
    Ok((input, Goal::OptimizeBool(opt_type, be)))
}

pub fn optimize_int<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Goal, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, opt_type) = opt_type(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, be) = expressions::int_expr(input)?;
    Ok((input, Goal::OptimizeInt(opt_type, be)))
}

pub fn optimize_float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Goal, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, opt_type) = opt_type(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, be) = expressions::float_expr(input)?;
    Ok((input, Goal::OptimizeFloat(opt_type, be)))
}

pub fn optimize_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Goal, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, opt_type) = opt_type(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, be) = expressions::set_expr(input)?;
    Ok((input, Goal::OptimizeSet(opt_type, be)))
}

#[test]
fn test_solve_item() {
    use crate::expressions::{AnnExpr, Annotation, Expr};
    use crate::solve_items::{Goal, OptimizationType};
    use nom::error::VerboseError;
    assert_eq!(
        solve_item::<VerboseError<&str>>(
            "solve :: int_search(X_59,input_order,indomain_min,complete) minimize X_24;"
        ),
        Ok((
            "",
            SolveItem {
                goal: Goal::OptimizeBool(
                    OptimizationType::Minimize,
                    BoolExpr::VarParIdentifier("X_24".to_string())
                ),
                annotations: vec![Annotation {
                    id: "int_search".to_string(),
                    expressions: vec![
                        AnnExpr::Expr(Expr::VarParIdentifier("X_59".to_string())),
                        AnnExpr::Expr(Expr::VarParIdentifier("input_order".to_string())),
                        AnnExpr::Expr(Expr::VarParIdentifier("indomain_min".to_string())),
                        AnnExpr::Expr(Expr::VarParIdentifier("complete".to_string()))
                    ]
                }]
            }
        ))
    );
}
