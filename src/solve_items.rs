use winnow::{
    ModalResult, Parser,
    combinator::{alt, cut_err},
    error::{AddContext, FromExternalError, ParserError, StrContext},
};

use crate::{
    comments::{space_or_comment0, space_or_comment1},
    expressions::{
        Annotations, BoolExpr, FloatExpr, IntExpr, SetExpr, annotations, bool_expr, float_expr,
        int_expr, set_expr,
    },
};

/// A solve item in a FlatZinc model.
#[derive(PartialEq, Clone, Debug)]
pub struct SolveItem {
    /// The goal of the solve item (satisfy or optimize).
    pub goal: Goal,
    /// The annotations associated with the solve item.
    pub annotations: Annotations,
}

/// Parses a solve item from the input string.
pub fn solve_item<'a, E>(input: &mut &'a str) -> ModalResult<SolveItem, E>
where
    E: ParserError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>
        + AddContext<&'a str, StrContext>,
{
    space_or_comment0(input)?;
    "solve".parse_next(input)?;
    cut_err(solve_item_tail.context(StrContext::Label("Error while parsing solve statement")))
        .parse_next(input)
}

/// Parses the tail of a solve item from the input string.
pub fn solve_item_tail<'a, E>(input: &mut &'a str) -> ModalResult<SolveItem, E>
where
    E: ParserError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
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

#[test]
fn test_solve_item() {
    use crate::solve_items::{Goal, OptimizationType};
    use crate::{AnnExpr, Annotation, Expr};
    use winnow::error::ContextError;
    let mut input = "solve :: int_search(X_59,input_order,indomain_min,complete) minimize X_24;";
    assert_eq!(
        solve_item::<ContextError>(&mut input),
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

/// The different kinds of solve goals in a FlatZinc model.
#[derive(PartialEq, Clone, Debug)]
pub enum Goal {
    /// A satisfaction problem.
    Satisfy,
    /// Optimize a boolean expression.
    OptimizeBool(OptimizationType, BoolExpr),
    /// Optimize an integer expression.
    OptimizeInt(OptimizationType, IntExpr),
    /// Optimize a float expression.
    OptimizeFloat(OptimizationType, FloatExpr),
    /// Optimize a set expression.
    OptimizeSet(OptimizationType, SetExpr),
}

/// The type of optimization goal.
#[derive(PartialEq, Clone, Debug)]
pub enum OptimizationType {
    /// Minimize the objective.
    Minimize,
    /// Maximize the objective.
    Maximize,
}

/// Parses a satisfy goal from the input string.
pub fn satisfy<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> ModalResult<Goal, E> {
    "satisfy".parse_next(input)?;
    Ok(Goal::Satisfy)
}

fn opt_type<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> ModalResult<OptimizationType, E> {
    alt((minimize, maximize)).parse_next(input)
}

fn minimize<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> ModalResult<OptimizationType, E> {
    "minimize".parse_next(input)?;
    Ok(OptimizationType::Minimize)
}

fn maximize<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> ModalResult<OptimizationType, E> {
    "maximize".parse_next(input)?;
    Ok(OptimizationType::Maximize)
}

/// Parses an optimize bool goal from the input string.
pub fn optimize_bool<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> ModalResult<Goal, E> {
    let opt_type = opt_type(input)?;
    space_or_comment1(input)?;
    let be = bool_expr(input)?;
    Ok(Goal::OptimizeBool(opt_type, be))
}

/// Parses an optimize int goal from the input string.
pub fn optimize_int<'a, E>(input: &mut &'a str) -> ModalResult<Goal, E>
where
    E: ParserError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    let opt_type = opt_type(input)?;
    space_or_comment1(input)?;
    let be = int_expr(input)?;
    Ok(Goal::OptimizeInt(opt_type, be))
}

/// Parses an optimize float goal from the input string.
pub fn optimize_float<'a, E>(input: &mut &'a str) -> ModalResult<Goal, E>
where
    E: ParserError<&'a str> + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let opt_type = opt_type(input)?;
    space_or_comment1(input)?;
    let be = float_expr(input)?;
    Ok(Goal::OptimizeFloat(opt_type, be))
}

/// Parses an optimize set goal from the input string.
pub fn optimize_set<'a, E>(input: &mut &'a str) -> ModalResult<Goal, E>
where
    E: ParserError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let opt_type = opt_type(input)?;
    space_or_comment1(input)?;
    let be = set_expr(input)?;
    Ok(Goal::OptimizeSet(opt_type, be))
}
