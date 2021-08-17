use std::str;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::char;
use nom::combinator::all_consuming;

use constraints::ConstraintItem;
use parameter_declarations::ParDeclItem;
use predicate_declarations::PredicateItem;
use solve_items::SolveItem;
use variable_declarations::VarDeclItem;

use crate::{comments, primitive_literals, FromExternalError, IResult, ParseError};

mod basic_types;
mod constraints;
mod parameter_types;
mod parameter_declarations;
mod predicate_declarations;
mod predicate_types;
mod solve_items;
mod variable_declarations;
mod variable_types;

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

#[derive(PartialEq, Clone, Debug)]
pub struct IndexSet(pub i128);

fn index_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i128, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = char('1')(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, int) = primitive_literals::int_literal(input)?;
    Ok((input, int))
}
