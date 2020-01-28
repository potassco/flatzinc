use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_while1},
    character::complete::{char, one_of, alphanumeric0},
    combinator::{map_res, not, opt},
    error::ParseError,
    multi::{count, many0, many_till, separated_list},
    Err, IResult,
};

#[derive(PartialEq, Clone, Debug)]
pub struct Model {
    pub predicate_items: Vec<PredicateItem>,
    pub par_decl_item: Vec<u32>,
    pub var_decl_item: Vec<u32>,
    pub constraint_item: Vec<u32>,
}
pub fn model<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Model, E> {
    let (input, _nl) = char('\n')(input)?;
    let par_decl_item = vec![];
    let var_decl_item = vec![];
    let constraint_item = vec![];
    let (input, predicate_items) = many0(predicate_item)(input)?;
    Ok((input, Model { predicate_items, par_decl_item, var_decl_item, constraint_item }))
}
#[derive(PartialEq, Clone, Debug)]
pub struct PredicateItem {
    ident: String,
    parameters : Vec<(PredParamType,String)>
}
pub fn predicate_item<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, PredicateItem, E> {
    let (input, _tag) = tag("predicate")(input)?;
    let (input, _space) = char(' ')(input)?;
    let (input, ident) = identifier(input)?;
    let (input, _space) = char('(')(input)?;
    let (input, parameters) = separated_list(tag(","), pred_param_type_ident_pair)(input)?;
    let (input, _space) = char(')')(input)?;
    let (input, _semi) = char(';')(input)?;
    Ok((input, PredicateItem{ident,parameters}))
}
#[derive(PartialEq, Clone, Debug)]
pub struct ConstraintItem {
    ident: String,
    parameters : Vec<(PredParamType,String)>
}
// pub fn constraint_item<'a, E: ParseError<&'a str>>(
//     input: &'a str,
// ) -> IResult<&'a str, ConstraintItem, E> {
//     let (input, _tag) = tag("constraint")(input)?;
//     let (input, _space) = char(' ')(input)?;
//     let (input, ident) = identifier(input)?;
//     let (input, _space) = char('(')(input)?;
//     let (input, parameters) = separated_list(tag(","), expr)(input)?;
//     let (input, _space) = char(')')(input)?;
//     let (input, ident) = annotations(input)?;
//     let (input, _semi) = char(';')(input)?;
//     Ok((input, ConstraintItem{ident,parameters}))
// }

pub fn pred_param_type_ident_pair<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, (PredParamType,String), E> {
    let (input, pred_param_type) = pred_param_type(input)?;
    let (input, _space) = char(':')(input)?;
    let (input, ident) = identifier(input)?;
    Ok((input,(pred_param_type,ident)))
}
#[derive(PartialEq, Clone, Debug)]
pub struct PredParamType;
pub fn pred_param_type<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, PredParamType, E> {
    // TODO
    Ok((input,PredParamType))
}

pub enum BasicParType {
    Bool,
    Int,
    Float,
    SetOfInt
}
pub fn basic_par_type<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicParType, E> {
    let (input, string) = alt((tag("bool"), tag("int"), tag("float"), tag("set of int")))(input)?;
    match string {
        "bool" => Ok((input,BasicParType::Bool)),
        "int" => Ok((input,BasicParType::Int)),
        "float" => Ok((input,BasicParType::Float)),
        "set of int" => Ok((input,BasicParType::SetOfInt)),
        x => panic!("unmatched basic par type {}", x),
    }
}
pub fn identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, String, E> {
    // TODO accept underscore
    let (input, first) = one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ")(input)?;
    let (input, rest) =  alphanumeric0(input)?;
    Ok((input, format!("{}{}", first, rest)))
}
