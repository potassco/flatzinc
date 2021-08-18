use std::str;

use nom::branch::alt;

use crate::constraints::ConstraintItem;
use crate::parameters::declarations as parameter_declarations;
use crate::parameters::declarations::ParDeclItem;
use crate::predicates::declarations as predicate_declarations;
use crate::predicates::declarations::PredicateItem;
use crate::solve_items::SolveItem;
use crate::variables::declarations as variable_declarations;
use crate::variables::declarations::VarDeclItem;
use crate::{comments, constraints, solve_items, FromExternalError, IResult, ParseError};
use nom::combinator::all_consuming;

#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    Comment(String),
    Predicate(PredicateItem),
    Parameter(ParDeclItem),
    Variable(VarDeclItem),
    Constraint(ConstraintItem),
    SolveItem(SolveItem),
}

pub fn statement<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Stmt, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, res) = all_consuming(alt((
        stmt_predicate,
        stmt_parameter,
        stmt_variable,
        stmt_constraint,
        stmt_solve_item,
        comments::space_or_comment,
    )))(input)?;
    Ok((input, res))
}

fn stmt_predicate<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Stmt, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, item) = predicate_declarations::predicate_item(input)?;
    Ok((input, Stmt::Predicate(item)))
}

fn stmt_parameter<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Stmt, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, item) = parameter_declarations::par_decl_item(input)?;
    Ok((input, Stmt::Parameter(item)))
}

fn stmt_variable<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Stmt, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, item) = variable_declarations::var_decl_item(input)?;
    Ok((input, Stmt::Variable(item)))
}

fn stmt_constraint<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Stmt, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, item) = constraints::constraint_item(input)?;
    Ok((input, Stmt::Constraint(item)))
}

fn stmt_solve_item<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Stmt, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, item) = solve_items::solve_item(input)?;
    Ok((input, Stmt::SolveItem(item)))
}
