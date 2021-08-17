use std::str;

// use nom::number::complete::{hex_u32, u128};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::char,
    combinator::{all_consuming, opt},
    multi::{many0, separated_list0, separated_list1},
};
pub use nom::{
    error::{convert_error, ErrorKind, FromExternalError, ParseError, VerboseError},
    Err, IResult,
};

mod comments;
mod primitive_literals;

// impl<I, E> FromExternalError<I, E> for std::num::ParseIntError {
//     /// Create a new error from an input position and an external error
//     fn from_external_error(input: I, kind: ErrorKind, _e: E) -> Self {
//         ParseIntError { input, code: kind }
//     }
// }

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
        predicate,
        parameter,
        variable,
        constraint,
        solve_item,
        comments::space_or_comment,
    )))(input)?;
    Ok((input, res))
}

fn predicate<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Stmt, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, item) = predicate_item(input)?;
    Ok((input, Stmt::Predicate(item)))
}
fn parameter<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Stmt, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, item) = par_decl_item(input)?;
    Ok((input, Stmt::Parameter(item)))
}
fn variable<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Stmt, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, item) = var_decl_item(input)?;
    Ok((input, Stmt::Variable(item)))
}
fn constraint<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Stmt, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, item) = constraint_item(input)?;
    Ok((input, Stmt::Constraint(item)))
}
fn solve_item<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Stmt, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, item) = solve_item_2(input)?;
    Ok((input, Stmt::SolveItem(item)))
}
#[test]
fn test_predicate_item() {
    use nom::error::VerboseError;
    assert_eq!(
        predicate_item::<VerboseError<&str>>("predicate float_03({1.0,3.3}:c);"),
        Ok((
            "",
            PredicateItem {
                id: "float_03".to_string(),
                parameters: vec![(
                    PredParType::Basic(BasicPredParType::FloatInSet(vec![1.0, 3.3])),
                    "c".to_string()
                )]
            }
        ))
    );
}
#[test]
#[should_panic]
fn test_predicate_item_2() {
    predicate_item::<VerboseError<&str>>("predicate float_01(set of float:c);").unwrap();
}
#[derive(PartialEq, Clone, Debug)]
pub struct PredicateItem {
    pub id: String,
    pub parameters: Vec<(PredParType, String)>,
}
pub fn predicate_item<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, PredicateItem, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = tag("predicate")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, id) = primitive_literals::identifier(input)?;
    let (input, _) = char('(')(input)?;
    let (input, parameters) = separated_list1(char(','), pred_par_type_ident_pair)(input)?;
    let (input, _) = char(')')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(';')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    Ok((input, PredicateItem { id, parameters }))
}
fn pred_par_type_ident_pair<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, (PredParType, String), E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, pred_par_type) = pred_par_type(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, ident) = primitive_literals::identifier(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    Ok((input, (pred_par_type, ident)))
}
#[derive(PartialEq, Clone, Debug)]
pub enum BasicParType {
    BasicType(BasicType),
    SetOfInt,
}
fn basic_par_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicParType, E> {
    let (input, bpt) = alt((bpt_basic_type, bpt_set_of_int))(input)?;
    Ok((input, bpt))
}
fn bpt_basic_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicParType, E> {
    let (input, bt) = basic_type(input)?;
    Ok((input, BasicParType::BasicType(bt)))
}
// "set" "of" "int"
// Moved this be a basic-var-type basic-par-type
fn bpt_set_of_int<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicParType, E> {
    let (input, _tag) = tag("set")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _tag) = tag("int")(input)?;
    Ok((input, BasicParType::SetOfInt))
}
#[derive(PartialEq, Clone, Debug)]
pub enum VarType {
    BasicVarType(BasicVarType),
    Array {
        ix: IndexSet,
        var_type: BasicVarType,
    },
}
fn var_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, VarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, var_type) = alt((vt_basic_var_type, array_var_type))(input)?;
    Ok((input, var_type))
}
fn vt_basic_var_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, VarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, vt) = basic_var_type(input)?;
    Ok((input, VarType::BasicVarType(vt)))
}
fn array_var_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, VarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = tag("array")(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char('[')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, int) = index_set(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, var_type) = basic_var_type(input)?;
    Ok((
        input,
        VarType::Array {
            ix: IndexSet(int),
            var_type,
        },
    ))
}
#[derive(PartialEq, Clone, Debug)]
pub enum ParType {
    BasicParType(BasicParType),
    Array {
        ix: IndexSet,
        par_type: BasicParType,
    },
}
#[test]
fn test_par_type() {
    use nom::error::VerboseError;
    assert_eq!(
        par_type::<VerboseError<&str>>("array [1..3] of float"),
        Ok((
            "",
            ParType::Array {
                ix: IndexSet(3),
                par_type: BasicParType::BasicType(BasicType::Float)
            }
        ))
    );
}
fn par_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, ParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, par_type) = alt((pt_basic_par_type, array_par_type))(input)?;
    Ok((input, par_type))
}
fn pt_basic_par_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, ParType, E> {
    let (input, pt) = basic_par_type(input)?;
    Ok((input, ParType::BasicParType(pt)))
}
fn array_par_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, ParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = tag("array")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _) = char('[')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, int) = index_set(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, par_type) = basic_par_type(input)?;
    Ok((
        input,
        ParType::Array {
            ix: IndexSet(int),
            par_type,
        },
    ))
}
#[derive(PartialEq, Clone, Debug)]
pub enum BasicType {
    Bool,
    Int,
    Float,
}
fn basic_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicType, E> {
    let (input, bt) = alt((bool, float, int))(input)?;
    Ok((input, bt))
}
fn bool<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicType, E> {
    let (input, _tag) = tag("bool")(input)?;
    Ok((input, BasicType::Bool))
}
fn int<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicType, E> {
    let (input, _tag) = tag("int")(input)?;
    Ok((input, BasicType::Int))
}
fn float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicType, E> {
    let (input, _tag) = tag("float")(input)?;
    Ok((input, BasicType::Float))
}
#[derive(PartialEq, Clone, Debug)]
pub enum BasicVarType {
    BasicType(BasicType),
    IntInRange(i128, i128),
    IntInSet(Vec<i128>),
    BoundedFloat(f64, f64),
    SubSetOfIntSet(Vec<i128>),
    SubSetOfIntRange(i128, i128),
}
fn basic_var_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _tag) = tag("var")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, vt) = alt((
        bvt_basic_type,
        bvt_int_in_range,
        bvt_int_in_set,
        bvt_bounded_float,
        bvt_subset_of_int_set,
        bvt_subset_of_int_range,
    ))(input)?;
    Ok((input, vt))
}
fn bvt_basic_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicVarType, E> {
    let (input, bt) = basic_type(input)?;
    Ok((input, BasicVarType::BasicType(bt)))
}
fn bvt_int_in_range<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, (lb, ub)) = int_in_range(input)?;
    Ok((input, BasicVarType::IntInRange(lb, ub)))
}
fn bvt_int_in_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, set) = int_in_set(input)?;
    Ok((input, BasicVarType::IntInSet(set)))
}
fn bvt_bounded_float<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, (lb, ub)) = bounded_float(input)?;
    Ok((input, BasicVarType::BoundedFloat(lb, ub)))
}
fn bvt_subset_of_int_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, (lb, ub)) = subset_of_int_range(input)?;
    Ok((input, BasicVarType::SubSetOfIntRange(lb, ub)))
}
fn bvt_subset_of_int_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicVarType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, set) = subset_of_int_set(input)?;
    Ok((input, BasicVarType::SubSetOfIntSet(set)))
}
fn int_in_range<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (i128, i128), E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, lb) = primitive_literals::int_literal(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, ub) = primitive_literals::int_literal(input)?;
    Ok((input, (lb, ub)))
}
fn bounded_float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (f64, f64), E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, lb) = primitive_literals::float_literal(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, ub) = primitive_literals::float_literal(input)?;
    Ok((input, (lb, ub)))
}
// "{" <float-literal> "," ... "}"
fn float_in_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Vec<f64>, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = char('{')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), primitive_literals::float_literal)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, v))
}
// "set" "of" <int_literal> ".." <int_literal>
fn subset_of_int_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, (i128, i128), E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _tag) = tag("set")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, lb) = primitive_literals::int_literal(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, ub) = primitive_literals::int_literal(input)?;
    Ok((input, (lb, ub)))
}
// "set" "of" "{" [ <int-literal> "," ... ] "}"
fn subset_of_int_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Vec<i128>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _tag) = tag("set of {")(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), primitive_literals::int_literal)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _tag) = tag("}")(input)?;
    Ok((input, v))
}
// "{" <int-literal> "," ... "}"
fn int_in_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Vec<i128>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = char('{')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), primitive_literals::int_literal)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, v))
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
#[derive(PartialEq, Clone, Debug)]
pub enum BasicPredParType {
    BasicParType(BasicParType),
    BasicVarType(BasicVarType),
    VarSetOfInt,
    IntInRange(i128, i128),
    IntInSet(Vec<i128>),
    BoundedFloat(f64, f64),
    FloatInSet(Vec<f64>),
    SubSetOfIntSet(Vec<i128>),
    SubSetOfIntRange(i128, i128),
}
fn basic_pred_par_type<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, bppt) = alt((
        bppt_basic_par_type,
        bppt_basic_var_type,
        bppt_var_set_of_int,
        bppt_int_in_range,
        bppt_int_in_set,
        bppt_bounded_float,
        bppt_float_in_set,
        bppt_subset_of_int_set,
        bppt_subset_of_int_range,
    ))(input)?;
    Ok((input, bppt))
}
fn bppt_basic_par_type<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E> {
    let (input, bpt) = basic_par_type(input)?;
    Ok((input, BasicPredParType::BasicParType(bpt)))
}
fn bppt_basic_var_type<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, bvt) = basic_var_type(input)?;
    Ok((input, BasicPredParType::BasicVarType(bvt)))
}
fn bppt_var_set_of_int<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E> {
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = tag("var")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _) = tag("set")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _) = tag("of")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _) = tag("int")(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    Ok((input, BasicPredParType::VarSetOfInt))
}
fn bppt_int_in_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, (lb, ub)) = int_in_range(input)?;
    Ok((input, BasicPredParType::IntInRange(lb, ub)))
}
fn bppt_int_in_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, set) = int_in_set(input)?;
    Ok((input, BasicPredParType::IntInSet(set)))
}
fn bppt_bounded_float<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, (lb, ub)) = bounded_float(input)?;
    Ok((input, BasicPredParType::BoundedFloat(lb, ub)))
}
fn bppt_float_in_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, set) = float_in_set(input)?;
    Ok((input, BasicPredParType::FloatInSet(set)))
}
fn bppt_subset_of_int_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, (lb, ub)) = subset_of_int_range(input)?;
    Ok((input, BasicPredParType::SubSetOfIntRange(lb, ub)))
}
fn bppt_subset_of_int_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, set) = subset_of_int_set(input)?;
    Ok((input, BasicPredParType::SubSetOfIntSet(set)))
}
#[derive(PartialEq, Clone, Debug)]
pub enum PredParType {
    Basic(BasicPredParType),
    Array {
        ix: PredIndexSet,
        par_type: BasicPredParType,
    },
}
fn pred_par_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, PredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, ppt) = alt((ppt_basic_pred_par_type, array_of_pred_index_set))(input)?;
    Ok((input, ppt))
}
fn ppt_basic_pred_par_type<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, PredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, bppt) = basic_pred_par_type(input)?;
    Ok((input, PredParType::Basic(bppt)))
}
fn array_of_pred_index_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, PredParType, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _tag) = tag("array")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _) = char('[')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, ix) = pred_index_set(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, par_type) = basic_pred_par_type(input)?;
    Ok((input, PredParType::Array { ix, par_type }))
}
#[derive(PartialEq, Clone, Debug)]
pub enum PredIndexSet {
    IndexSet(i128),
    Int,
}
fn pred_index_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, PredIndexSet, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, index_set) = alt((pis_int, pis_index_set))(input)?;
    Ok((input, index_set))
}
fn pis_int<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, PredIndexSet, E> {
    let (input, _tag) = tag("int")(input)?;
    Ok((input, PredIndexSet::Int))
}
fn pis_index_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, PredIndexSet, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, int) = index_set(input)?;
    Ok((input, PredIndexSet::IndexSet(int)))
}
#[derive(PartialEq, Clone, Debug)]
pub enum BoolExpr {
    Bool(bool),
    VarParIdentifier(String),
}
fn bool_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BoolExpr, E> {
    let (input, expr) = alt((be_bool_literal, be_var_par_identifier))(input)?;
    Ok((input, expr))
}
fn be_bool_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BoolExpr, E> {
    let (input, expr) = primitive_literals::bool_literal(input)?;
    Ok((input, BoolExpr::Bool(expr)))
}
fn be_var_par_identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BoolExpr, E> {
    let (input, id) = primitive_literals::var_par_identifier(input)?;
    Ok((input, BoolExpr::VarParIdentifier(id)))
}
#[derive(PartialEq, Clone, Debug)]
pub enum IntExpr {
    Int(i128),
    VarParIdentifier(String),
}
fn int_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, IntExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, expr) = alt((ie_int_literal, ie_var_par_identifier))(input)?;
    Ok((input, expr))
}
fn ie_int_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, IntExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, expr) = primitive_literals::int_literal(input)?;
    Ok((input, IntExpr::Int(expr)))
}
fn ie_var_par_identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, IntExpr, E> {
    let (input, id) = primitive_literals::var_par_identifier(input)?;
    Ok((input, IntExpr::VarParIdentifier(id)))
}
#[derive(PartialEq, Clone, Debug)]
pub enum FloatExpr {
    Float(f64),
    VarParIdentifier(String),
}
fn float_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, FloatExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, expr) = alt((fe_float_literal, fe_var_par_identifier))(input)?;
    Ok((input, expr))
}
fn fe_float_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, FloatExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, expr) = primitive_literals::float_literal(input)?;
    Ok((input, FloatExpr::Float(expr)))
}
fn fe_var_par_identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, FloatExpr, E> {
    let (input, id) = primitive_literals::var_par_identifier(input)?;
    Ok((input, FloatExpr::VarParIdentifier(id)))
}
#[derive(PartialEq, Clone, Debug)]
pub enum SetExpr {
    Set(SetLiteralExpr),
    VarParIdentifier(String),
}
fn set_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, expr) = alt((se_set_literal_expr, se_var_par_identifier))(input)?;
    Ok((input, expr))
}
fn se_set_literal_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, sl) = set_literal_expr(input)?;
    Ok((input, SetExpr::Set(sl)))
}
fn se_var_par_identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SetExpr, E> {
    let (input, id) = primitive_literals::var_par_identifier(input)?;
    Ok((input, SetExpr::VarParIdentifier(id)))
}
#[test]
fn test_expr() {
    use nom::error::VerboseError;
    assert_eq!(
        expr::<VerboseError<&str>>("1..2"),
        Ok((
            "",
            Expr::Set(SetLiteralExpr::IntInRange(IntExpr::Int(1), IntExpr::Int(2)))
        ))
    );
}
#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    VarParIdentifier(String),
    Bool(bool),
    Int(i128),
    Float(f64),
    Set(SetLiteralExpr),
    ArrayOfBool(Vec<BoolExpr>),
    ArrayOfInt(Vec<IntExpr>),
    ArrayOfFloat(Vec<FloatExpr>),
    ArrayOfSet(Vec<SetExpr>),
}
fn expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, expr) = alt((
        e_var_par_identifier,
        e_bool_expr,
        e_set_expr,
        e_float_expr,
        e_int_expr,
        e_array_of_bool_expr,
        e_array_of_int_expr,
        e_array_of_float_expr,
        e_array_of_set_expr,
    ))(input)?;
    Ok((input, expr))
}
fn e_var_par_identifier<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, id) = primitive_literals::var_par_identifier(input)?;
    Ok((input, Expr::VarParIdentifier(id)))
}
fn e_bool_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, b) = primitive_literals::bool_literal(input)?;
    Ok((input, Expr::Bool(b)))
}
fn e_int_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, int) = primitive_literals::int_literal(input)?;
    Ok((input, Expr::Int(int)))
}
fn e_float_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, float) = primitive_literals::float_literal(input)?;
    Ok((input, Expr::Float(float)))
}
fn e_set_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, set) = set_literal_expr(input)?;
    Ok((input, Expr::Set(set)))
}
fn e_array_of_bool_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, v) = array_of_bool_expr_literal(input)?;
    Ok((input, Expr::ArrayOfBool(v)))
}
fn e_array_of_int_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, v) = array_of_int_expr_literal(input)?;
    Ok((input, Expr::ArrayOfInt(v)))
}
fn e_array_of_float_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, v) = array_of_float_expr_literal(input)?;
    Ok((input, Expr::ArrayOfFloat(v)))
}
fn e_array_of_set_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E>
where
    E: ParseError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, v) = array_of_set_expr_literal(input)?;
    Ok((input, Expr::ArrayOfSet(v)))
}
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
#[test]
fn test_par_decl_item_1() {
    use nom::error::VerboseError;
    assert_eq!(
        par_decl_item::<VerboseError<&str>>("array [1..3] of float: X_139 = [1.0,1.0,1.0];"),
        Ok((
            "",
            ParDeclItem::ArrayOfFloat {
                ix: IndexSet(3),
                id: "X_139".to_string(),
                v: vec![1.0, 1.0, 1.0]
            }
        ))
    );
}
#[test]
#[should_panic]
fn test_par_decl_item_2() {
    use nom::error::VerboseError;
    par_decl_item::<VerboseError<&str>>("bool : b2 = b1;").unwrap();
}
#[test]
fn test_par_decl_item_3() {
    use nom::error::VerboseError;
    assert_eq!(
        par_decl_item::<VerboseError<&str>>("array [1..3] of set of int : h = [{42,17},1..5,{}];"),
        Ok((
            "",
            ParDeclItem::ArrayOfSet {
                ix: IndexSet(3),
                id: "h".to_string(),
                v: vec![
                    SetLiteral::SetInts(vec![42, 17]),
                    SetLiteral::IntRange(1, 5),
                    SetLiteral::SetInts(vec![])
                ]
            }
        ))
    );
}
#[test]
fn pred_par_type_2() {
    use nom::error::VerboseError;
    assert_eq!(
        pred_par_type::<VerboseError<&str>>("1..3"),
        Ok(("", PredParType::Basic(BasicPredParType::IntInRange(1, 3))))
    );
}
#[test]
fn test_pred_par_type() {
    use nom::error::VerboseError;
    assert_eq!(
        pred_par_type::<VerboseError<&str>>("1..3"),
        Ok(("", PredParType::Basic(BasicPredParType::IntInRange(1, 3))))
    );
}
pub fn par_decl_item<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, ParDeclItem, E>
where
    E: ParseError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, ptype) = par_type(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, id) = primitive_literals::var_par_identifier(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    match ptype {
        ParType::BasicParType(bpt) => match bpt {
            BasicParType::BasicType(bt) => match bt {
                BasicType::Bool => {
                    let (input, bool) = primitive_literals::bool_literal(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    Ok((input, ParDeclItem::Bool { id, bool }))
                }
                BasicType::Int => {
                    let (input, int) = primitive_literals::int_literal(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    Ok((input, ParDeclItem::Int { id, int }))
                }
                BasicType::Float => {
                    let (input, float) = primitive_literals::float_literal(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    Ok((input, ParDeclItem::Float { id, float }))
                }
            },
            BasicParType::SetOfInt => {
                let (input, set_literal) = set_literal(input)?;
                let (input, _) = comments::space_or_comment0(input)?;
                let (input, _) = char(';')(input)?;
                let (input, _) = comments::space_or_comment0(input)?;
                Ok((input, ParDeclItem::SetOfInt { id, set_literal }))
            }
        },
        ParType::Array { ix, par_type } => match par_type {
            BasicParType::BasicType(bt) => match bt {
                BasicType::Bool => {
                    let (input, v) = array_of_bool_literal(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    Ok((input, ParDeclItem::ArrayOfBool { ix, id, v }))
                }
                BasicType::Int => {
                    let (input, v) = array_of_int_literal(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    Ok((input, ParDeclItem::ArrayOfInt { ix, id, v }))
                }
                BasicType::Float => {
                    let (input, v) = array_of_float_literal(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = comments::space_or_comment0(input)?;
                    Ok((input, ParDeclItem::ArrayOfFloat { ix, id, v }))
                }
            },
            BasicParType::SetOfInt => {
                let (input, v) = array_of_set_literal(input)?;
                let (input, _) = comments::space_or_comment0(input)?;
                let (input, _) = char(';')(input)?;
                let (input, _) = comments::space_or_comment0(input)?;
                Ok((input, ParDeclItem::ArrayOfSet { ix, id, v }))
            }
        },
    }
}
#[test]
fn test_pred_par_type_2() {
    use nom::error::VerboseError;
    assert_eq!(
        pred_par_type::<VerboseError<&str>>("array [1..1] of var set of int"),
        Ok((
            "",
            PredParType::Array {
                ix: PredIndexSet::IndexSet(1),
                par_type: BasicPredParType::VarSetOfInt,
            },
        ))
    );
}
#[test]
fn test_basic_pred_par_type() {
    use nom::error::VerboseError;
    assert_eq!(
        basic_pred_par_type::<VerboseError<&str>>("var set of int"),
        Ok(("", BasicPredParType::VarSetOfInt))
    );
}
#[test]
fn test_var_decl_item_1() {
    use nom::error::VerboseError;
    assert_eq!(
        var_decl_item::<VerboseError<&str>>(
            "array [1..1] of var set of 1..10: sets:: output_array([1..1]) = [X_0];"
        ),
        Ok((
            "",
            VarDeclItem::ArrayOfSubSetOfIntRange {
                ix: IndexSet(1),
                id: "sets".to_string(),
                annos: vec![Annotation {
                    id: "output_array".to_string(),
                    expressions: vec![AnnExpr::Expr(Expr::ArrayOfSet(vec![SetExpr::Set(
                        SetLiteralExpr::IntInRange(IntExpr::Int(1), IntExpr::Int(1))
                    )]))]
                }],
                lb: 1,
                ub: 10,
                array_expr: Some(ArrayOfSetExpr::Array(vec![SetExpr::VarParIdentifier(
                    "X_0".to_owned()
                )]))
            }
        ))
    );
}
#[test]
fn test_var_decl_item_2() {
    assert_eq!(
        var_decl_item::<VerboseError<&str>>("array [1..5] of var 0..3: w =X_32;"),
        Ok((
            "",
            VarDeclItem::ArrayOfIntInRange {
                id: "w".to_string(),
                ix: IndexSet(5),
                lb: 0,
                ub: 3,
                array_expr: Some(ArrayOfIntExpr::VarParIdentifier("X_32".to_string())),
                annos: vec![],
            }
        ))
    );
}
#[test]
fn test_var_decl_item_3() {
    assert_eq!(
        var_decl_item::<VerboseError<&str>>("array [1..5] of var {1,2,3}: w;"),
        Ok((
            "",
            VarDeclItem::ArrayOfIntInSet {
                id: "w".to_string(),
                ix: IndexSet(5),
                set: vec![1, 2, 3],
                array_expr: None,
                annos: vec![],
            }
        ))
    );
}
#[test]
fn test_var_decl_item_4() {
    assert_eq!(
        var_decl_item::<VerboseError<&str>>("array [1..5] of var 0..3: w;"),
        Ok((
            "",
            VarDeclItem::ArrayOfIntInRange {
                id: "w".to_string(),
                ix: IndexSet(5),
                lb: 0,
                ub: 3,
                array_expr: None,
                annos: vec![],
            }
        ))
    );
    assert_eq!(
        var_decl_item::<VerboseError<&str>>("var 1..101: objective :: output_var = X_2586;"),
        Ok((
            "",
            VarDeclItem::IntInRange {
                id: "objective".to_string(),
                lb: 1,
                ub: 101,
                expr: Some(IntExpr::VarParIdentifier("X_2586".to_string())),
                annos: vec![Annotation {
                    id: "output_var".to_string(),
                    expressions: vec![]
                }],
            }
        ))
    );
}
#[test]
fn test_var_decl_item_5() {
    use nom::error::VerboseError;
    assert_eq!(
        var_decl_item::<VerboseError<&str>>(
            "array [1..3] of var set of 17..42: h = [{42,17},23..X,{}];"
        ),
        Ok((
            "",
            VarDeclItem::ArrayOfSubSetOfIntRange {
                lb: 17,
                ub: 42,
                annos: vec![],
                ix: IndexSet(3),
                id: "h".to_string(),
                array_expr: Some(ArrayOfSetExpr::Array(vec![
                    SetExpr::Set(SetLiteralExpr::SetInts(vec![
                        IntExpr::Int(42),
                        IntExpr::Int(17)
                    ])),
                    SetExpr::Set(SetLiteralExpr::IntInRange(
                        IntExpr::Int(23),
                        IntExpr::VarParIdentifier("X".to_string())
                    )),
                    SetExpr::Set(SetLiteralExpr::SetInts(vec![])),
                ])),
            }
        ))
    );
}

#[test]
fn test_pred_par_type_3() {
    assert_eq!(
        pred_par_type::<VerboseError<&str>>("var set of int"),
        Ok(("", PredParType::Basic(BasicPredParType::VarSetOfInt)))
    );
}
#[test]
fn test_pred_par_type_ident_pair() {
    assert_eq!(
        pred_par_type_ident_pair::<VerboseError<&str>>("var set of int: g"),
        Ok((
            "",
            (
                PredParType::Basic(BasicPredParType::VarSetOfInt),
                "g".to_string()
            )
        ))
    );
}
#[derive(PartialEq, Clone, Debug)]
pub enum VarDeclItem {
    Bool {
        id: String,
        expr: Option<BoolExpr>,
        annos: Annotations,
    },
    Int {
        id: String,
        expr: Option<IntExpr>,
        annos: Annotations,
    },
    IntInRange {
        id: String,
        lb: i128,
        ub: i128,
        expr: Option<IntExpr>,
        annos: Annotations,
    },
    IntInSet {
        id: String,
        set: Vec<i128>,
        expr: Option<IntExpr>,
        annos: Annotations,
    },
    Float {
        id: String,
        expr: Option<FloatExpr>,
        annos: Annotations,
    },
    BoundedFloat {
        id: String,
        lb: f64,
        ub: f64,
        expr: Option<FloatExpr>,
        annos: Annotations,
    },
    SetOfInt {
        id: String,
        expr: Option<SetExpr>,
        annos: Annotations,
    },
    SubSetOfIntSet {
        id: String,
        set: Vec<i128>,
        expr: Option<SetExpr>,
        annos: Annotations,
    },
    SubSetOfIntRange {
        id: String,
        lb: i128,
        ub: i128,
        expr: Option<SetExpr>,
        annos: Annotations,
    },
    ArrayOfBool {
        ix: IndexSet,
        id: String,
        annos: Annotations,
        array_expr: Option<ArrayOfBoolExpr>,
    },
    ArrayOfInt {
        ix: IndexSet,
        id: String,
        annos: Annotations,
        array_expr: Option<ArrayOfIntExpr>,
    },
    ArrayOfIntInRange {
        lb: i128,
        ub: i128,
        ix: IndexSet,
        id: String,
        annos: Annotations,
        array_expr: Option<ArrayOfIntExpr>,
    },
    ArrayOfIntInSet {
        set: Vec<i128>,
        ix: IndexSet,
        id: String,
        annos: Annotations,
        array_expr: Option<ArrayOfIntExpr>,
    },
    ArrayOfFloat {
        ix: IndexSet,
        id: String,
        annos: Annotations,
        array_expr: Option<ArrayOfFloatExpr>,
    },
    ArrayOfBoundedFloat {
        lb: f64,
        ub: f64,
        ix: IndexSet,
        id: String,
        annos: Annotations,
        array_expr: Option<ArrayOfFloatExpr>,
    },
    ArrayOfSet {
        ix: IndexSet,
        id: String,
        annos: Annotations,
        array_expr: Option<ArrayOfSetExpr>,
    },
    // array [int] of set of 1..3
    ArrayOfSubSetOfIntRange {
        ub: i128,
        lb: i128,
        ix: IndexSet,
        id: String,
        annos: Annotations,
        array_expr: Option<ArrayOfSetExpr>,
    },
    // array [int] of set of {1,2,3} //TODO: not in the specs
    ArrayOfSubSetOfIntSet {
        set: Vec<i128>,
        ix: IndexSet,
        id: String,
        annos: Annotations,
        array_expr: Option<ArrayOfSetExpr>,
    },
}
pub fn var_decl_item<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, VarDeclItem, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, item) = vdi_var(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(';')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    Ok((input, item))
}
fn vdi_var<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, VarDeclItem, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, vt) = var_type(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, id) = primitive_literals::var_par_identifier(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, annos) = annotations(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, assign) = opt(char('='))(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    match vt {
        VarType::BasicVarType(bvt) => match bvt {
            BasicVarType::BasicType(BasicType::Bool) => {
                let (input, expr) = if assign.is_some() {
                    let (input, expr) = bool_expr(input)?;
                    (input, Some(expr))
                } else {
                    (input, None)
                };
                Ok((input, VarDeclItem::Bool { id, annos, expr }))
            }
            BasicVarType::BasicType(BasicType::Int) => {
                let (input, expr) = if assign.is_some() {
                    let (input, expr) = int_expr(input)?;
                    (input, Some(expr))
                } else {
                    (input, None)
                };
                Ok((input, VarDeclItem::Int { id, annos, expr }))
            }
            BasicVarType::BasicType(BasicType::Float) => {
                let (input, expr) = if assign.is_some() {
                    let (input, expr) = float_expr(input)?;
                    (input, Some(expr))
                } else {
                    (input, None)
                };
                Ok((input, VarDeclItem::Float { id, annos, expr }))
            }
            BasicVarType::IntInRange(lb, ub) => {
                let (input, expr) = if assign.is_some() {
                    let (input, expr) = int_expr(input)?;
                    (input, Some(expr))
                } else {
                    (input, None)
                };
                Ok((
                    input,
                    VarDeclItem::IntInRange {
                        id,
                        lb,
                        ub,
                        expr,
                        annos,
                    },
                ))
            }
            BasicVarType::IntInSet(set) => {
                let (input, expr) = if assign.is_some() {
                    let (input, expr) = int_expr(input)?;
                    (input, Some(expr))
                } else {
                    (input, None)
                };
                Ok((
                    input,
                    VarDeclItem::IntInSet {
                        id,
                        set,
                        expr,
                        annos,
                    },
                ))
            }
            BasicVarType::BoundedFloat(lb, ub) => {
                let (input, expr) = if assign.is_some() {
                    let (input, expr) = float_expr(input)?;
                    (input, Some(expr))
                } else {
                    (input, None)
                };
                Ok((
                    input,
                    VarDeclItem::BoundedFloat {
                        id,
                        lb,
                        ub,
                        expr,
                        annos,
                    },
                ))
            }
            BasicVarType::SubSetOfIntRange(lb, ub) => {
                let (input, expr) = if assign.is_some() {
                    let (input, expr) = set_expr(input)?;
                    (input, Some(expr))
                } else {
                    (input, None)
                };
                Ok((
                    input,
                    VarDeclItem::SubSetOfIntRange {
                        id,
                        lb,
                        ub,
                        expr,
                        annos,
                    },
                ))
            }
            BasicVarType::SubSetOfIntSet(set) => {
                let (input, expr) = if assign.is_some() {
                    let (input, expr) = set_expr(input)?;
                    (input, Some(expr))
                } else {
                    (input, None)
                };
                Ok((
                    input,
                    VarDeclItem::SubSetOfIntSet {
                        id,
                        set,
                        expr,
                        annos,
                    },
                ))
            }
        },
        VarType::Array { ix, var_type } => match var_type {
            BasicVarType::BasicType(bt) => match bt {
                BasicType::Bool => {
                    let (input, array_expr) = if assign.is_some() {
                        let (input, expr) = array_of_bool_expr(input)?;
                        (input, Some(expr))
                    } else {
                        (input, None)
                    };
                    Ok((
                        input,
                        VarDeclItem::ArrayOfBool {
                            ix,
                            id,
                            annos,
                            array_expr,
                        },
                    ))
                }
                BasicType::Int => {
                    let (input, array_expr) = if assign.is_some() {
                        let (input, expr) = array_of_int_expr(input)?;
                        (input, Some(expr))
                    } else {
                        (input, None)
                    };
                    Ok((
                        input,
                        VarDeclItem::ArrayOfInt {
                            ix,
                            id,
                            annos,
                            array_expr,
                        },
                    ))
                }
                BasicType::Float => {
                    let (input, array_expr) = if assign.is_some() {
                        let (input, expr) = array_of_float_expr(input)?;
                        (input, Some(expr))
                    } else {
                        (input, None)
                    };
                    Ok((
                        input,
                        VarDeclItem::ArrayOfFloat {
                            ix,
                            id,
                            annos,
                            array_expr,
                        },
                    ))
                }
            },
            BasicVarType::IntInRange(lb, ub) => {
                let (input, array_expr) = if assign.is_some() {
                    let (input, expr) = array_of_int_expr(input)?;
                    (input, Some(expr))
                } else {
                    (input, None)
                };
                Ok((
                    input,
                    VarDeclItem::ArrayOfIntInRange {
                        lb,
                        ub,
                        ix,
                        id,
                        annos,
                        array_expr,
                    },
                ))
            }
            BasicVarType::IntInSet(set) => {
                let (input, array_expr) = if assign.is_some() {
                    let (input, expr) = array_of_int_expr(input)?;
                    (input, Some(expr))
                } else {
                    (input, None)
                };
                Ok((
                    input,
                    VarDeclItem::ArrayOfIntInSet {
                        set,
                        ix,
                        id,
                        annos,
                        array_expr,
                    },
                ))
            }
            BasicVarType::BoundedFloat(lb, ub) => {
                let (input, array_expr) = if assign.is_some() {
                    let (input, expr) = array_of_float_expr(input)?;
                    (input, Some(expr))
                } else {
                    (input, None)
                };
                Ok((
                    input,
                    VarDeclItem::ArrayOfBoundedFloat {
                        lb,
                        ub,
                        ix,
                        id,
                        annos,
                        array_expr,
                    },
                ))
            }
            BasicVarType::SubSetOfIntRange(lb, ub) => {
                let (input, array_expr) = if assign.is_some() {
                    let (input, expr) = array_of_set_expr(input)?;
                    (input, Some(expr))
                } else {
                    (input, None)
                };
                Ok((
                    input,
                    VarDeclItem::ArrayOfSubSetOfIntRange {
                        lb,
                        ub,
                        ix,
                        id,
                        annos,
                        array_expr,
                    },
                ))
            }
            BasicVarType::SubSetOfIntSet(set) => {
                let (input, array_expr) = if assign.is_some() {
                    let (input, expr) = array_of_set_expr(input)?;
                    (input, Some(expr))
                } else {
                    (input, None)
                };
                Ok((
                    input,
                    VarDeclItem::ArrayOfSubSetOfIntSet {
                        set,
                        ix,
                        id,
                        annos,
                        array_expr,
                    },
                ))
            }
        },
    }
}
#[test]
fn test_constraint_item() {
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
    let (input, exprs) = separated_list1(char(','), expr)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(')')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, annos) = annotations(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(';')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    Ok((input, ConstraintItem { id, exprs, annos }))
}
#[test]
fn test_solve_item() {
    use nom::error::VerboseError;
    assert_eq!(
        solve_item_2::<VerboseError<&str>>(
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
#[derive(PartialEq, Clone, Debug)]
pub struct SolveItem {
    pub goal: Goal,
    pub annotations: Annotations,
}
pub fn solve_item_2<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SolveItem, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = tag("solve")(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, annotations) = annotations(input)?;
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
fn satisfy<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Goal, E> {
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
fn optimize_bool<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Goal, E> {
    let (input, opt_type) = opt_type(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, be) = bool_expr(input)?;
    Ok((input, Goal::OptimizeBool(opt_type, be)))
}
fn optimize_int<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Goal, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, opt_type) = opt_type(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, be) = int_expr(input)?;
    Ok((input, Goal::OptimizeInt(opt_type, be)))
}
fn optimize_float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Goal, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, opt_type) = opt_type(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, be) = float_expr(input)?;
    Ok((input, Goal::OptimizeFloat(opt_type, be)))
}
fn optimize_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Goal, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, opt_type) = opt_type(input)?;
    let (input, _) = comments::space_or_comment1(input)?;
    let (input, be) = set_expr(input)?;
    Ok((input, Goal::OptimizeSet(opt_type, be)))
}
type Annotations = Vec<Annotation>;
fn annotations<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Annotations, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, annos) = many0(annotation1)(input)?;
    Ok((input, annos))
}
fn annotation1<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Annotation, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = tag("::")(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    annotation(input)
}
#[derive(PartialEq, Clone, Debug)]
pub struct Annotation {
    pub id: String,
    pub expressions: Vec<AnnExpr>,
}
// <annotation> ::= <identifier>
//                | <identifier> "(" <ann-expr> "," ... ")"
fn annotation<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Annotation, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, id) = primitive_literals::identifier(input)?;
    let (input, we) = opt(char('('))(input)?;
    if we.is_some() {
        let (input, expressions_what) = separated_list1(char(','), ann_expr)(input)?;
        let (input, _) = char(')')(input)?;
        Ok((
            input,
            Annotation {
                id,
                expressions: expressions_what,
            },
        ))
    } else {
        let (input, _) = comments::space_or_comment0(input)?;
        Ok((
            input,
            Annotation {
                id,
                expressions: vec![],
            },
        ))
    }
}
// <ann_expr> ::= <expr>
//              | <string_literal>
//              | "[" <annotation> "," ... "]"
#[derive(PartialEq, Clone, Debug)]
pub enum AnnExpr {
    Annotations(Annotations),
    String(String),
    Expr(Expr),
}
fn ann_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AnnExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, expr) = alt((ann_non_array_expr, ae_annotations))(input)?;
    Ok((input, expr))
}
fn ae_annotations<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AnnExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = char('[')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, res) = separated_list1(char(','), annotation)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, AnnExpr::Annotations(res)))
}
// ann_non_array_expr ::=
//       FZ_BOOL_LIT
//     | FZ_INT_LIT
//     | FZ_FLOAT_LIT
//     | set_literal
//     | var_par_id /* variable, possibly array */
//     | var_par_id '[' ann_non_array_expr ']' /* array access */
//     | FZ_STRING_LIT
fn ann_non_array_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AnnExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, expr) = alt((ae_expr, primitive_literals::string_lit))(input)?;
    Ok((input, expr))
}
fn ae_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AnnExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, expr) = expr(input)?;
    Ok((input, AnnExpr::Expr(expr)))
}
#[derive(PartialEq, Clone, Debug)]
pub enum SetLiteralExpr {
    IntInRange(IntExpr, IntExpr),
    BoundedFloat(FloatExpr, FloatExpr),
    SetFloats(Vec<FloatExpr>),
    SetInts(Vec<IntExpr>),
}
fn set_literal_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SetLiteralExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, sl) = alt((
        sle_int_in_range,
        sle_bounded_float,
        sle_set_of_ints,
        sle_set_of_floats,
    ))(input)?;
    Ok((input, sl))
}
fn sle_int_in_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SetLiteralExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, lb) = int_expr(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, ub) = int_expr(input)?;
    Ok((input, SetLiteralExpr::IntInRange(lb, ub)))
}
fn sle_bounded_float<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SetLiteralExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, lb) = float_expr(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, ub) = float_expr(input)?;
    Ok((input, SetLiteralExpr::BoundedFloat(lb, ub)))
}
// "{" <int-expr> "," ... "}"
fn sle_set_of_ints<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SetLiteralExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = char('{')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), int_expr)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, SetLiteralExpr::SetInts(v)))
}
// "{" <float-expr> "," ... "}"
fn sle_set_of_floats<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SetLiteralExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = char('{')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), float_expr)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, SetLiteralExpr::SetFloats(v)))
}
#[derive(PartialEq, Clone, Debug)]
pub enum SetLiteral {
    IntRange(i128, i128),
    BoundedFloat(f64, f64),
    SetFloats(Vec<f64>),
    SetInts(Vec<i128>),
}
fn set_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E>
where
    E: ParseError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, sl) = alt((
        sl_int_range,
        sl_bounded_float,
        sl_set_of_ints,
        sl_set_of_floats,
    ))(input)?;
    Ok((input, sl))
}
fn sl_int_range<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, lb) = primitive_literals::int_literal(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, ub) = primitive_literals::int_literal(input)?;
    Ok((input, SetLiteral::IntRange(lb, ub)))
}
fn sl_bounded_float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, lb) = primitive_literals::float_literal(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, ub) = primitive_literals::float_literal(input)?;
    Ok((input, SetLiteral::BoundedFloat(lb, ub)))
}
// "{" <int-literal> "," ... "}"
fn sl_set_of_ints<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = char('{')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), primitive_literals::int_literal)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, SetLiteral::SetInts(v)))
}
// "{" <float-literal> "," ... "}"
fn sl_set_of_floats<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = char('{')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list0(char(','), primitive_literals::float_literal)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, SetLiteral::SetFloats(v)))
}
#[derive(PartialEq, Clone, Debug)]
pub enum ArrayOfBoolExpr {
    Array(Vec<BoolExpr>),
    VarParIdentifier(String),
}
fn array_of_bool_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ArrayOfBoolExpr, E> {
    let (input, id) = opt(primitive_literals::var_par_identifier)(input)?;
    if let Some(id) = id {
        Ok((input, ArrayOfBoolExpr::VarParIdentifier(id)))
    } else {
        let (input, v) = array_of_bool_expr_literal(input)?;
        Ok((input, ArrayOfBoolExpr::Array(v)))
    }
}
fn array_of_bool_expr_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<BoolExpr>, E> {
    let (input, _) = char('[')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list1(char(','), bool_expr)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, v))
}
fn array_of_bool_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<bool>, E> {
    let (input, _) = char('[')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, al) = separated_list1(char(','), primitive_literals::bool_literal)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
}
#[derive(PartialEq, Clone, Debug)]
pub enum ArrayOfIntExpr {
    Array(Vec<IntExpr>),
    VarParIdentifier(String),
}
fn array_of_int_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ArrayOfIntExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, id) = opt(primitive_literals::var_par_identifier)(input)?;
    if let Some(id) = id {
        Ok((input, ArrayOfIntExpr::VarParIdentifier(id)))
    } else {
        let (input, v) = array_of_int_expr_literal(input)?;
        Ok((input, ArrayOfIntExpr::Array(v)))
    }
}
fn array_of_int_expr_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<IntExpr>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = char('[')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list1(char(','), int_expr)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, v))
}
fn array_of_int_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<i128>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (input, _) = char('[')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, al) = separated_list1(char(','), primitive_literals::int_literal)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
}
#[derive(PartialEq, Clone, Debug)]
pub enum ArrayOfFloatExpr {
    Array(Vec<FloatExpr>),
    VarParIdentifier(String),
}
fn array_of_float_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ArrayOfFloatExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, id) = opt(primitive_literals::var_par_identifier)(input)?;
    if let Some(id) = id {
        Ok((input, ArrayOfFloatExpr::VarParIdentifier(id)))
    } else {
        let (input, v) = array_of_float_expr_literal(input)?;
        Ok((input, ArrayOfFloatExpr::Array(v)))
    }
}
fn array_of_float_expr_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<FloatExpr>, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = char('[')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list1(char(','), float_expr)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, v))
}
fn array_of_float_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<f64>, E>
where
    E: FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = char('[')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, al) = separated_list1(char(','), primitive_literals::float_literal)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
}
#[derive(PartialEq, Clone, Debug)]
pub enum ArrayOfSetExpr {
    Array(Vec<SetExpr>),
    VarParIdentifier(String),
}
fn array_of_set_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ArrayOfSetExpr, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, id) = opt(primitive_literals::var_par_identifier)(input)?;
    if let Some(id) = id {
        Ok((input, ArrayOfSetExpr::VarParIdentifier(id)))
    } else {
        let (input, v) = array_of_set_expr_literal(input)?;
        Ok((input, ArrayOfSetExpr::Array(v)))
    }
}
fn array_of_set_expr_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<SetExpr>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = char('[')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, v) = separated_list1(char(','), set_expr)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, v))
}
fn array_of_set_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<SetLiteral>, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = char('[')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, al) = separated_list1(char(','), set_literal)(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
}
