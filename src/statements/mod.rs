use winnow::{
    combinator::{alt, eof},
    error::{FromExternalError, ParserError},
    PResult, Parser,
};

use crate::{
    comments::space_or_comment,
    constraints::{constraint_item, ConstraintItem},
    parameters::declarations as parameter_declarations,
    parameters::declarations::ParDeclItem,
    predicates::declarations as predicate_declarations,
    predicates::declarations::PredicateItem,
    solve_items::{solve_item, SolveItem},
    variables::declarations as variable_declarations,
    variables::declarations::VarDeclItem,
};

#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    Comment(String),
    Predicate(PredicateItem),
    Parameter(ParDeclItem),
    Variable(VarDeclItem),
    Constraint(ConstraintItem),
    SolveItem(SolveItem),
}

pub fn statement<'a, E: ParserError<&'a str>, I>() -> impl Parser<&'a str, Stmt, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    alt((
        stmt_predicate,
        stmt_parameter,
        stmt_variable,
        stmt_constraint,
        stmt_solve_item,
        space_or_comment,
    ))
}

fn stmt_predicate<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Stmt, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let item = predicate_declarations::predicate_item(input)?;
    Ok(Stmt::Predicate(item))
}

fn stmt_parameter<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Stmt, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let item = parameter_declarations::par_decl_item(input)?;
    Ok(Stmt::Parameter(item))
}

fn stmt_variable<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Stmt, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let item = variable_declarations::var_decl_item(input)?;
    Ok(Stmt::Variable(item))
}

fn stmt_constraint<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Stmt, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let item = constraint_item(input)?;
    Ok(Stmt::Constraint(item))
}

fn stmt_solve_item<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<Stmt, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let item = solve_item(input)?;
    Ok(Stmt::SolveItem(item))
}
