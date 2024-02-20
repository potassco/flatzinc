use winnow::{
    combinator::alt,
    error::{FromExternalError, ParserError},
    token::tag,
    PResult, Parser,
};

use crate::{
    comments::{space_or_comment0, space_or_comment1},
    expressions::{
        annotations, bool_expr, float_expr, int_expr, set_expr, Annotations, BoolExpr, FloatExpr,
        IntExpr, SetExpr,
    },
};

#[derive(PartialEq, Clone, Debug)]
pub struct SolveItem {
    pub goal: Goal,
    pub annotations: Annotations,
}

pub fn solve_item<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<SolveItem, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    space_or_comment0(input)?;
    tag("solve").parse_next(input)?;
    space_or_comment1(input)?;
    let annotations = annotations(input)?;
    space_or_comment0(input)?;
    let goal = alt((
        satisfy,
        optimize_bool,
        optimize_int,
        optimize_float,
        optimize_set,
    ))
    .parse_next(input)?;
    space_or_comment0(input)?;
    ';'.parse_next(input)?;
    space_or_comment0(input)?;
    Ok(SolveItem { goal, annotations })
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

pub fn satisfy<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Goal, E> {
    tag("satisfy").parse_next(input)?;
    Ok(Goal::Satisfy)
}

fn opt_type<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<OptimizationType, E> {
    alt((minimize, maximize)).parse_next(input)
}

fn minimize<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<OptimizationType, E> {
    tag("minimize").parse_next(input)?;
    Ok(OptimizationType::Minimize)
}

fn maximize<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<OptimizationType, E> {
    tag("maximize").parse_next(input)?;
    Ok(OptimizationType::Maximize)
}

pub fn optimize_bool<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Goal, E> {
    let opt_type = opt_type(input)?;
    space_or_comment1(input)?;
    let be = bool_expr(input)?;
    Ok(Goal::OptimizeBool(opt_type, be))
}

pub fn optimize_int<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Goal, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let opt_type = opt_type(input)?;
    space_or_comment1(input)?;
    let be = int_expr(input)?;
    Ok(Goal::OptimizeInt(opt_type, be))
}

pub fn optimize_float<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Goal, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let opt_type = opt_type(input)?;
    space_or_comment1(input)?;
    let be = float_expr(input)?;
    Ok(Goal::OptimizeFloat(opt_type, be))
}

pub fn optimize_set<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Goal, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let opt_type = opt_type(input)?;
    space_or_comment1(input)?;
    let be = set_expr(input)?;
    Ok(Goal::OptimizeSet(opt_type, be))
}

#[test]
fn test_solve_item() {
    use crate::solve_items::{Goal, OptimizationType};
    use crate::{AnnExpr, Annotation, Expr};
    use winnow::error::ContextError;
    let mut input = "solve :: int_search(X_59,input_order,indomain_min,complete) minimize X_24;";
    assert_eq!(
        solve_item::<ContextError<&str>>(&mut input),
        Ok(SolveItem {
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
        })
    );
}
