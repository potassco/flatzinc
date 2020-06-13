use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_while, take_while1},
    character::complete::{char, multispace0, multispace1, one_of},
    combinator::{all_consuming, map_res, opt},
    multi::{many0, separated_list},
};
pub use nom::{
    error::{convert_error, ErrorKind, ParseError, VerboseError},
    Err, IResult,
};
#[derive(PartialEq, Clone, Debug)]
pub enum FzStmt {
    Comment(String),
    Predicate(PredicateItem),
    Parameter(ParDeclItem),
    Variable(VarDeclItem),
    Constraint(ConstraintItem),
    SolveItem(SolveItem),
}
pub fn fz_statement<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, FzStmt, E> {
    let (input, res) = all_consuming(alt((
        fz_predicate,
        fz_parameter,
        fz_variable,
        fz_constraint,
        fz_solve_item,
        fz_space_or_comment0,
    )))(input)?;
    Ok((input, res))
}
fn fz_space_or_comment0<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, FzStmt, E> {
    let (input, s) = space_or_comment0(input)?;
    Ok((input, FzStmt::Comment(s.into())))
}
fn fz_predicate<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, FzStmt, E> {
    let (input, item) = predicate_item(input)?;
    Ok((input, FzStmt::Predicate(item)))
}
fn fz_parameter<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, FzStmt, E> {
    let (input, item) = par_decl_item(input)?;
    Ok((input, FzStmt::Parameter(item)))
}
fn fz_variable<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, FzStmt, E> {
    let (input, item) = var_decl_item(input)?;
    Ok((input, FzStmt::Variable(item)))
}
fn fz_constraint<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, FzStmt, E> {
    let (input, item) = constraint_item(input)?;
    Ok((input, FzStmt::Constraint(item)))
}
fn fz_solve_item<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, FzStmt, E> {
    let (input, item) = solve_item(input)?;
    Ok((input, FzStmt::SolveItem(item)))
}
#[derive(PartialEq, Clone, Debug)]
pub struct Model {
    pub predicate_items: Vec<PredicateItem>,
    pub par_decl_items: Vec<ParDeclItem>,
    pub var_decl_items: Vec<VarDeclItem>,
    pub constraint_items: Vec<ConstraintItem>,
    pub solve_item: SolveItem,
}
pub fn model<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Model, E> {
    let (input, predicate_items) = many0(predicate_item)(input)?;
    let (input, par_decl_items) = many0(par_decl_item)(input)?;
    let (input, var_decl_items) = many0(var_decl_item)(input)?;
    let (input, constraint_items) = many0(constraint_item)(input)?;
    let (input, solve_item) = solve_item(input)?;
    Ok((
        input,
        Model {
            predicate_items,
            par_decl_items,
            var_decl_items,
            constraint_items,
            solve_item,
        },
    ))
}
#[derive(PartialEq, Clone, Debug)]
pub struct PredicateItem {
    pub id: String,
    pub parameters: Vec<(PredParType, String)>,
}
pub fn predicate_item<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, PredicateItem, E> {
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = tag("predicate")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, id) = identifier(input)?;
    let (input, _) = char('(')(input)?;
    let (input, parameters) = separated_list(char(','), pred_par_type_ident_pair)(input)?;
    let (input, _) = char(')')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(';')(input)?;
    let (input, _) = space_or_comment0(input)?;
    Ok((input, PredicateItem { id, parameters }))
}
fn pred_par_type_ident_pair<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, (PredParType, String), E> {
    let (input, _) = space_or_comment0(input)?;
    let (input, pred_par_type) = pred_par_type(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ident) = identifier(input)?;
    let (input, _) = space_or_comment0(input)?;
    Ok((input, (pred_par_type, ident)))
}
#[derive(PartialEq, Clone, Debug)]
pub enum BasicParType {
    Bool,
    Int,
    Float,
    SetOfInt,
}
fn basic_par_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicParType, E> {
    let (input, bpt) = alt((bpt_bool, bpt_int, bpt_float, bpt_set_of_int))(input)?;
    Ok((input, bpt))
}

fn bpt_bool<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicParType, E> {
    let (input, _tag) = tag("bool")(input)?;
    Ok((input, BasicParType::Bool))
}
fn bpt_int<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicParType, E> {
    let (input, _tag) = tag("int")(input)?;
    Ok((input, BasicParType::Int))
}
fn bpt_float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicParType, E> {
    let (input, _tag) = tag("float")(input)?;
    Ok((input, BasicParType::Float))
}
// "var" "set" "of" "int"
// Moved this be a basic-var-type basic-par-type
fn bpt_set_of_int<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicParType, E> {
    let (input, _tag) = tag("set")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, _tag) = tag("int")(input)?;
    Ok((input, BasicParType::SetOfInt))
}
#[derive(PartialEq, Clone, Debug)]
pub enum ParType {
    Bool,
    Int,
    Float,
    SetOfInt,
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
                par_type: BasicParType::Float
            }
        ))
    );
}
fn par_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, ParType, E> {
    let (input, par_type) = alt((pt_bool, pt_int, pt_float, pt_set_of_int, array_par_type))(input)?;
    Ok((input, par_type))
}

fn pt_bool<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, ParType, E> {
    let (input, _tag) = tag("bool")(input)?;
    Ok((input, ParType::Bool))
}
fn pt_int<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, ParType, E> {
    let (input, _tag) = tag("int")(input)?;
    Ok((input, ParType::Int))
}
fn pt_float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, ParType, E> {
    let (input, _tag) = tag("float")(input)?;
    Ok((input, ParType::Float))
}
// "var" "set" "of" "int"
fn pt_set_of_int<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, ParType, E> {
    let (input, _tag) = tag("set")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, _tag) = tag("int")(input)?;
    Ok((input, ParType::SetOfInt))
}
fn array_par_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, ParType, E> {
    let (input, _) = tag("array")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, int) = index_set(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = space_or_comment1(input)?;
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
pub enum BasicVarType {
    Bool,
    Int,
    Float,
    IntInRange(i128, i128),
    IntInSet(Vec<i128>), // possibly empty
    FloatInRange(f64, f64),
    SetOfIntInSet(Vec<i128>),
    SetOfIntInRange(i128, i128),

    SetOfInt, // added var_set_of_int from basic_pred_par_type TODO: move back
}
fn basic_var_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicVarType, E> {
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("var")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, bvt) = alt((
        bvt_basic_par_type,
        bvt_int_in_range,
        bvt_int_in_set,
        bvt_float_in_range,
        bvt_subset_of_int_in_set,
        bvt_subset_of_int_in_range,
    ))(input)?;
    Ok((input, bvt))
}
fn bvt_basic_par_type<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicVarType, E> {
    let (input, bpt) = basic_par_type(input)?;
    match bpt {
        BasicParType::Bool => Ok((input, BasicVarType::Bool)),
        BasicParType::Int => Ok((input, BasicVarType::Int)),
        BasicParType::Float => Ok((input, BasicVarType::Float)),
        BasicParType::SetOfInt => Ok((input, BasicVarType::SetOfInt)),
    }
}
fn bvt_int_in_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicVarType, E> {
    let (input, (lb, ub)) = int_in_range(input)?;
    Ok((input, BasicVarType::IntInRange(lb, ub)))
}
fn bvt_int_in_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicVarType, E> {
    let (input, set) = int_in_set(input)?;
    Ok((input, BasicVarType::IntInSet(set)))
}
fn bvt_float_in_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicVarType, E> {
    let (input, (lb, ub)) = float_in_range(input)?;
    Ok((input, BasicVarType::FloatInRange(lb, ub)))
}
fn bvt_subset_of_int_in_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicVarType, E> {
    let (input, (lb, ub)) = subset_of_int_in_range(input)?;
    Ok((input, BasicVarType::SetOfIntInRange(lb, ub)))
}
fn bvt_subset_of_int_in_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicVarType, E> {
    let (input, set) = subset_of_int_in_set(input)?;
    Ok((input, BasicVarType::SetOfIntInSet(set)))
}
fn int_in_range<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (i128, i128), E> {
    let (input, lb) = int_literal(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ub) = int_literal(input)?;
    Ok((input, (lb, ub)))
}
fn float_in_range<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (f64, f64), E> {
    let (input, lb) = float_literal(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ub) = float_literal(input)?;
    Ok((input, (lb, ub)))
}
// "set" "of" <int_literal> ".." <int_literal>
fn subset_of_int_in_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, (i128, i128), E> {
    let (input, _tag) = tag("set")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, lb) = int_literal(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ub) = int_literal(input)?;
    Ok((input, (lb, ub)))
}
// "set" "of" "{" [ <int-literal> "," ... ] "}"
fn subset_of_int_in_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<i128>, E> {
    let (input, _tag) = tag("set of {")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list(char(','), int_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("}")(input)?;
    Ok((input, v))
}
// "{" <int-literal> "," ... "}"
fn int_in_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Vec<i128>, E> {
    let (input, _) = char('{')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list(char(','), int_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, v))
}
#[derive(PartialEq, Clone, Debug)]
pub struct IndexSet(pub i128);
fn index_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i128, E> {
    let (input, _) = char('1')(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, int) = int_literal(input)?;
    Ok((input, int))
}
#[derive(PartialEq, Clone, Debug)]
pub enum BasicPredParType {
    BasicParType(BasicParType),
    BasicVarType(BasicVarType),
    IntInRange(i128, i128),
    IntInSet(Vec<i128>), // possibly empty
    FloatInRange(f64, f64),
    SetOfIntInSet(Vec<i128>),
    SetOfIntInRange(i128, i128),
}
fn basic_pred_par_type<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E> {
    let (input, bppt) = alt((
        bppt_basic_par_type,
        bppt_basic_var_type,
        bppt_int_in_range,
        bppt_int_in_set,
        bppt_float_in_range,
        bppt_subset_of_int_in_set,
        bppt_subset_of_int_in_range,
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
) -> IResult<&'a str, BasicPredParType, E> {
    let (input, bvt) = basic_var_type(input)?;
    Ok((input, BasicPredParType::BasicVarType(bvt)))
}
fn bppt_int_in_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E> {
    let (input, (lb, ub)) = int_in_range(input)?;
    Ok((input, BasicPredParType::IntInRange(lb, ub)))
}
fn bppt_int_in_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E> {
    let (input, set) = int_in_set(input)?;
    Ok((input, BasicPredParType::IntInSet(set)))
}
fn bppt_float_in_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E> {
    let (input, (lb, ub)) = float_in_range(input)?;
    Ok((input, BasicPredParType::FloatInRange(lb, ub)))
}
fn bppt_subset_of_int_in_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E> {
    let (input, (lb, ub)) = subset_of_int_in_range(input)?;
    Ok((input, BasicPredParType::SetOfIntInRange(lb, ub)))
}
fn bppt_subset_of_int_in_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E> {
    let (input, set) = subset_of_int_in_set(input)?;
    Ok((input, BasicPredParType::SetOfIntInSet(set)))
}
#[derive(PartialEq, Clone, Debug)]
pub enum PredParType {
    Basic(BasicPredParType),
    Array {
        ix: PredIndexSet,
        par_type: BasicPredParType,
    },
}
fn pred_par_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, PredParType, E> {
    let (input, ppt) = alt((ppt_basic_pred_par_type, array_of_pred_index_set))(input)?;
    Ok((input, ppt))
}
fn ppt_basic_pred_par_type<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, PredParType, E> {
    let (input, bppt) = basic_pred_par_type(input)?;
    Ok((input, PredParType::Basic(bppt)))
}
fn array_of_pred_index_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, PredParType, E> {
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("array")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ix) = pred_index_set(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, par_type) = basic_pred_par_type(input)?;
    Ok((input, PredParType::Array { ix, par_type }))
}
#[derive(PartialEq, Clone, Debug)]
pub enum PredIndexSet {
    IndexSet(i128),
    Int,
}
fn pred_index_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, PredIndexSet, E> {
    let (input, index_set) = alt((pis_int, pis_index_set))(input)?;
    Ok((input, index_set))
}
fn pis_int<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, PredIndexSet, E> {
    let (input, _tag) = tag("int")(input)?;
    Ok((input, PredIndexSet::Int))
}
fn pis_index_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, PredIndexSet, E> {
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
    let (input, expr) = bool_literal(input)?;
    Ok((input, BoolExpr::Bool(expr)))
}
fn be_var_par_identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BoolExpr, E> {
    let (input, id) = var_par_identifier(input)?;
    Ok((input, BoolExpr::VarParIdentifier(id)))
}
#[derive(PartialEq, Clone, Debug)]
pub enum IntExpr {
    Int(i128),
    VarParIdentifier(String),
}
fn int_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, IntExpr, E> {
    let (input, _) = space_or_comment0(input)?;
    let (input, expr) = alt((ie_int_literal, ie_var_par_identifier))(input)?;
    Ok((input, expr))
}
fn ie_int_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, IntExpr, E> {
    let (input, expr) = int_literal(input)?;
    Ok((input, IntExpr::Int(expr)))
}
fn ie_var_par_identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, IntExpr, E> {
    let (input, id) = var_par_identifier(input)?;
    Ok((input, IntExpr::VarParIdentifier(id)))
}
#[derive(PartialEq, Clone, Debug)]
pub enum FloatExpr {
    Float(f64),
    VarParIdentifier(String),
}
fn float_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, FloatExpr, E> {
    let (input, expr) = alt((fe_float_literal, fe_var_par_identifier))(input)?;
    Ok((input, expr))
}
fn fe_float_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, FloatExpr, E> {
    let (input, expr) = float_literal(input)?;
    Ok((input, FloatExpr::Float(expr)))
}
fn fe_var_par_identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, FloatExpr, E> {
    let (input, id) = var_par_identifier(input)?;
    Ok((input, FloatExpr::VarParIdentifier(id)))
}
#[derive(PartialEq, Clone, Debug)]
pub enum SetExpr {
    Set(SetLiteral),
    VarParIdentifier(String),
}
fn set_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetExpr, E> {
    let (input, expr) = alt((se_set_literal, se_var_par_identifier))(input)?;
    Ok((input, expr))
}
fn se_set_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetExpr, E> {
    let (input, sl) = set_literal(input)?;
    Ok((input, SetExpr::Set(sl)))
}
fn se_var_par_identifier<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SetExpr, E> {
    let (input, id) = var_par_identifier(input)?;
    Ok((input, SetExpr::VarParIdentifier(id)))
}
#[test]
fn test_expr() {
    use nom::error::VerboseError;
    assert_eq!(
        expr::<VerboseError<&str>>("1..2"),
        Ok(("", Expr::Set(SetLiteral::IntRange(1, 2))))
    );
}
#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    VarParIdentifier(String),
    Bool(bool),
    Int(i128),
    Float(f64),
    Set(SetLiteral),
    ArrayOfBool(Vec<BoolExpr>),
    ArrayOfInt(Vec<IntExpr>),
    ArrayOfFloat(Vec<FloatExpr>),
    ArrayOfSet(Vec<SetExpr>),
}
fn expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, _) = space_or_comment0(input)?;
    let (input, expr) = alt((
        e_var_par_identifier,
        e_bool_literal,
        e_set_literal,
        e_float_literal,
        e_int_literal,
        e_array_of_bool_expr,
        e_array_of_int_expr,
        e_array_of_float_expr,
        e_array_of_set_expr,
    ))(input)?;
    Ok((input, expr))
}
fn e_var_par_identifier<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, id) = var_par_identifier(input)?;
    Ok((input, Expr::VarParIdentifier(id)))
}
fn e_bool_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, b) = bool_literal(input)?;
    Ok((input, Expr::Bool(b)))
}
fn e_int_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, int) = int_literal(input)?;
    Ok((input, Expr::Int(int)))
}
fn e_float_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, float) = float_literal(input)?;
    Ok((input, Expr::Float(float)))
}
fn e_set_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, sl) = set_literal(input)?;
    Ok((input, Expr::Set(sl)))
}
fn e_array_of_bool_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, array_literal) = array_of_bool_expr(input)?;
    Ok((input, Expr::ArrayOfBool(array_literal)))
}
fn e_array_of_int_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, array_literal) = array_of_int_expr(input)?;
    Ok((input, Expr::ArrayOfInt(array_literal)))
}
fn e_array_of_float_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, array_literal) = array_of_float_expr(input)?;
    Ok((input, Expr::ArrayOfFloat(array_literal)))
}
fn e_array_of_set_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, array_literal) = array_of_set_expr(input)?;
    Ok((input, Expr::ArrayOfSet(array_literal)))
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
        expr: Vec<bool>,
    },
    ArrayOfInt {
        ix: IndexSet,
        id: String,
        expr: Vec<i128>,
    },
    ArrayOfFloat {
        ix: IndexSet,
        id: String,
        expr: Vec<f64>,
    },
    ArrayOfSet {
        ix: IndexSet,
        id: String,
        expr: Vec<SetLiteral>,
    },
}
#[test]
fn test_par_decl_item() {
    use nom::error::VerboseError;
    assert_eq!(
        par_decl_item::<VerboseError<&str>>("array [1..3] of float: X_139 = [1.0,1.0,1.0];"),
        Ok((
            "",
            ParDeclItem::ArrayOfFloat {
                ix: IndexSet(3),
                id: "X_139".to_string(),
                expr: vec![1.0, 1.0, 1.0]
            }
        ))
    );
}
pub fn par_decl_item<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ParDeclItem, E> {
    let (input, _) = space_or_comment0(input)?;
    let (input, ptype) = par_type(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, id) = var_par_identifier(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = space_or_comment0(input)?;
    match ptype {
        ParType::Array { ix, par_type } => match par_type {
            BasicParType::Bool => {
                let (input, expr) = array_of_bool_literal(input)?;
                let (input, _) = space_or_comment0(input)?;
                let (input, _) = char(';')(input)?;
                let (input, _) = space_or_comment0(input)?;
                Ok((input, ParDeclItem::ArrayOfBool { ix, id, expr }))
            }
            BasicParType::Int => {
                let (input, expr) = array_of_int_literal(input)?;
                let (input, _) = space_or_comment0(input)?;
                let (input, _) = char(';')(input)?;
                let (input, _) = space_or_comment0(input)?;
                Ok((input, ParDeclItem::ArrayOfInt { ix, id, expr }))
            }
            BasicParType::Float => {
                let (input, expr) = array_of_float_literal(input)?;
                let (input, _) = space_or_comment0(input)?;
                let (input, _) = char(';')(input)?;
                let (input, _) = space_or_comment0(input)?;
                Ok((input, ParDeclItem::ArrayOfFloat { ix, id, expr }))
            }
            BasicParType::SetOfInt => {
                let (input, expr) = array_of_set_literal(input)?;
                let (input, _) = space_or_comment0(input)?;
                let (input, _) = char(';')(input)?;
                let (input, _) = space_or_comment0(input)?;
                Ok((input, ParDeclItem::ArrayOfSet { ix, id, expr }))
            }
        },
        ParType::Bool => {
            let (input, bool) = bool_literal(input)?;
            let (input, _) = space_or_comment0(input)?;
            let (input, _) = char(';')(input)?;
            let (input, _) = space_or_comment0(input)?;
            Ok((input, ParDeclItem::Bool { id, bool }))
        }
        ParType::Int => {
            let (input, int) = int_literal(input)?;
            let (input, _) = space_or_comment0(input)?;
            let (input, _) = char(';')(input)?;
            let (input, _) = space_or_comment0(input)?;
            Ok((input, ParDeclItem::Int { id, int }))
        }
        ParType::Float => {
            let (input, float) = float_literal(input)?;
            let (input, _) = space_or_comment0(input)?;
            let (input, _) = char(';')(input)?;
            let (input, _) = space_or_comment0(input)?;
            Ok((input, ParDeclItem::Float { id, float }))
        }
        ParType::SetOfInt => {
            let (input, set_literal) = set_literal(input)?;
            let (input, _) = space_or_comment0(input)?;
            let (input, _) = char(';')(input)?;
            let (input, _) = space_or_comment0(input)?;
            Ok((input, ParDeclItem::SetOfInt { id, set_literal }))
        }
    }
}
#[test]
fn test_var_decl_item() {
    use nom::error::VerboseError;
    assert_eq!(
        var_decl_item::<VerboseError<&str>>(
            "array [1..1] of var set of int: sets:: output_array([1..1]) = [X_0];"
        ),
        Ok((
            "",
            VarDeclItem::ArrayOfSet {
                ix: IndexSet(1),
                id: "sets".to_string(),
                annos: vec![Annotation::Id {
                    id: "output_array".to_string(),
                    expressions: vec![AnnExpr::Expr(Expr::ArrayOfSet(vec![SetExpr::Set(
                        SetLiteral::IntRange(1, 1)
                    )]))]
                }],
                array_literal: vec![SetExpr::VarParIdentifier("X_0".to_owned())]
            }
        ))
    );
    assert_eq!(
        var_decl_item::<VerboseError<&str>>("array[1..5] of var 0..3: w;"),
        Ok((
            "",
            VarDeclItem::ArrayOfIntInRange {
                id: "w".to_string(),
                ix: IndexSet(5),
                lb: 0,
                ub: 3,
                array_literal: vec![],
                annos: vec![],
            }
        ))
    );
    assert_eq!(
        var_decl_item::<VerboseError<&str>>("var 1..101: objective = X_2586;"),
        Ok((
            "",
            VarDeclItem::IntInRange {
                id: "objective".to_string(),
                lb: 1,
                ub: 101,
                int: Some(IntExpr::VarParIdentifier("X_2586".to_string())),
                annos: vec![],
            }
        ))
    );
}
#[test]
fn test_var_decl_item_2() {
    assert_eq!(
        var_decl_item::<VerboseError<&str>>("var set of int: g = { 1, 2, 3};"),
        Ok((
            "",
            VarDeclItem::SetOfInt {
                id: "g".to_string(),
                expr: Some(SetExpr::Set(SetLiteral::SetInts(vec![1, 2, 3]))),
                annos: vec![],
            }
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
    Float {
        id: String,
        expr: Option<FloatExpr>,
        annos: Annotations,
    },
    IntInRange {
        id: String,
        lb: i128,
        ub: i128,
        int: Option<IntExpr>,
        annos: Annotations,
    },
    IntInSet {
        id: String,
        set: Vec<i128>, // possibly empty
        int: Option<IntExpr>,
        annos: Annotations,
    },
    FloatInRange {
        id: String,
        lb: f64,
        ub: f64,
        float: Option<FloatExpr>,
        annos: Annotations,
    },
    SetOfInt {
        id: String,
        expr: Option<SetExpr>,
        annos: Annotations,
    },
    SetOfIntInSet {
        id: String,
        set: Vec<i128>,
        expr: Option<SetExpr>,
        annos: Annotations,
    },
    SetOfIntInRange {
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
        array_literal: Vec<BoolExpr>,
    },
    ArrayOfInt {
        ix: IndexSet,
        id: String,
        annos: Annotations,
        array_literal: Vec<IntExpr>,
    },
    ArrayOfIntInRange {
        lb: i128,
        ub: i128,
        ix: IndexSet,
        id: String,
        annos: Annotations,
        array_literal: Vec<IntExpr>,
    },
    ArrayOfIntInSet {
        set: Vec<i128>,
        ix: IndexSet,
        id: String,
        annos: Annotations,
        array_literal: Vec<IntExpr>,
    },
    ArrayOfFloat {
        ix: IndexSet,
        id: String,
        annos: Annotations,
        array_literal: Vec<FloatExpr>,
    },
    ArrayOfFloatInRange {
        lb: f64,
        ub: f64,
        ix: IndexSet,
        id: String,
        annos: Annotations,
        array_literal: Vec<FloatExpr>,
    },
    ArrayOfSet {
        ix: IndexSet,
        id: String,
        annos: Annotations,
        array_literal: Vec<SetExpr>,
    },
    ArrayOfSetOfIntInRange {
        ub: i128,
        lb: i128,
        ix: IndexSet,
        id: String,
        annos: Annotations,
        array_literal: Vec<SetExpr>,
    },
    ArrayOfSetOfIntInSet {
        set: Vec<i128>,
        ix: IndexSet,
        id: String,
        annos: Annotations,
        array_literal: Vec<SetExpr>,
    },
}
pub fn var_decl_item<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, VarDeclItem, E> {
    let (input, _) = space_or_comment0(input)?;
    let (input, item) = alt((
        vdi_basic_var_with_assignment,
        vdi_basic_var_without_assignment,
        vdi_array_with_assignment,
        vdi_array_without_assignment,
    ))(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(';')(input)?;
    let (input, _) = space_or_comment0(input)?;
    Ok((input, item))
}
fn vdi_basic_var_with_assignment<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, VarDeclItem, E> {
    let (input, var_type) = basic_var_type(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, id) = var_par_identifier(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, annos) = annotations(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = space_or_comment0(input)?;
    match var_type {
        BasicVarType::Bool => {
            let (input, bool) = bool_expr(input)?;
            Ok((
                input,
                VarDeclItem::Bool {
                    id,
                    annos,
                    expr: Some(bool),
                },
            ))
        }
        BasicVarType::Int => {
            let (input, int) = int_expr(input)?;
            Ok((
                input,
                VarDeclItem::Int {
                    id,
                    annos,
                    expr: Some(int),
                },
            ))
        }
        BasicVarType::Float => {
            let (input, float) = float_expr(input)?;
            Ok((
                input,
                VarDeclItem::Float {
                    id,
                    annos,
                    expr: Some(float),
                },
            ))
        }
        BasicVarType::SetOfInt => {
            let (input, expr) = set_expr(input)?;
            Ok((
                input,
                VarDeclItem::SetOfInt {
                    id,
                    annos,
                    expr: Some(expr),
                },
            ))
        }
        BasicVarType::IntInRange(lb, ub) => {
            let (input, expr) = int_expr(input)?;
            Ok((
                input,
                VarDeclItem::IntInRange {
                    id,
                    lb,
                    ub,
                    int: Some(expr),
                    annos,
                },
            ))
        }
        BasicVarType::IntInSet(set) => {
            let (input, expr) = int_expr(input)?;
            Ok((
                input,
                VarDeclItem::IntInSet {
                    id,
                    set,
                    int: Some(expr),
                    annos,
                },
            ))
        }
        BasicVarType::FloatInRange(lb, ub) => {
            let (input, float) = float_expr(input)?;
            Ok((
                input,
                VarDeclItem::FloatInRange {
                    id,
                    lb,
                    ub,
                    float: Some(float),
                    annos,
                },
            ))
        }
        BasicVarType::SetOfIntInRange(lb, ub) => {
            let (input, sl) = set_expr(input)?;
            Ok((
                input,
                VarDeclItem::SetOfIntInRange {
                    id,
                    lb,
                    ub,
                    expr: Some(sl),
                    annos,
                },
            ))
        }
        BasicVarType::SetOfIntInSet(set) => {
            let (input, sl) = set_expr(input)?;
            Ok((
                input,
                VarDeclItem::SetOfIntInSet {
                    id,
                    set,
                    expr: Some(sl),
                    annos,
                },
            ))
        }
    }
}
fn vdi_basic_var_without_assignment<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, VarDeclItem, E> {
    let (input, var_type) = basic_var_type(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, id) = var_par_identifier(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, annos) = annotations(input)?;
    let (input, _) = space_or_comment0(input)?;
    match var_type {
        BasicVarType::Bool => Ok((
            input,
            VarDeclItem::Bool {
                id,
                annos,
                expr: None,
            },
        )),
        BasicVarType::Int => Ok((
            input,
            VarDeclItem::Int {
                id,
                annos,
                expr: None,
            },
        )),
        BasicVarType::Float => Ok((
            input,
            VarDeclItem::Float {
                id,
                annos,
                expr: None,
            },
        )),
        BasicVarType::SetOfInt => Ok((
            input,
            VarDeclItem::SetOfInt {
                id,
                expr: None,
                annos,
            },
        )),
        BasicVarType::IntInRange(lb, ub) => Ok((
            input,
            VarDeclItem::IntInRange {
                id,
                lb,
                ub,
                int: None,
                annos,
            },
        )),
        BasicVarType::IntInSet(set) => Ok((
            input,
            VarDeclItem::IntInSet {
                id,
                set,
                int: None,
                annos,
            },
        )),
        BasicVarType::FloatInRange(lb, ub) => Ok((
            input,
            VarDeclItem::FloatInRange {
                id,
                lb,
                ub,
                float: None,
                annos,
            },
        )),
        BasicVarType::SetOfIntInRange(lb, ub) => Ok((
            input,
            VarDeclItem::SetOfIntInRange {
                id,
                lb,
                ub,
                expr: None,
                annos,
            },
        )),
        BasicVarType::SetOfIntInSet(set) => Ok((
            input,
            VarDeclItem::SetOfIntInSet {
                id,
                set,
                expr: None,
                annos,
            },
        )),
    }
}
fn vdi_array_with_assignment<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, VarDeclItem, E> {
    // let (input, avt) = array_var_type(input)?;

    let (input, _tag) = tag("array")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, int) = index_set(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, var_type) = basic_var_type(input)?;

    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, id) = var_par_identifier(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, annos) = annotations(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = space_or_comment0(input)?;
    match var_type {
        BasicVarType::Bool => {
            let (input, array_literal) = array_of_bool_expr(input)?;
            Ok((
                input,
                VarDeclItem::ArrayOfBool {
                    ix: IndexSet(int),
                    id,
                    annos,
                    array_literal,
                },
            ))
        }
        BasicVarType::Int => {
            let (input, array_literal) = array_of_int_expr(input)?;
            Ok((
                input,
                VarDeclItem::ArrayOfInt {
                    ix: IndexSet(int),
                    id,
                    annos,
                    array_literal,
                },
            ))
        }
        BasicVarType::Float => {
            let (input, array_literal) = array_of_float_expr(input)?;
            Ok((
                input,
                VarDeclItem::ArrayOfFloat {
                    ix: IndexSet(int),
                    id,
                    annos,
                    array_literal,
                },
            ))
        }
        BasicVarType::SetOfInt => {
            let (input, array_literal) = array_of_set_expr(input)?;
            Ok((
                input,
                VarDeclItem::ArrayOfSet {
                    ix: IndexSet(int),
                    id,
                    annos,
                    array_literal,
                },
            ))
        }
        BasicVarType::IntInRange(lb, ub) => {
            let (input, array_literal) = array_of_int_expr(input)?;
            Ok((
                input,
                VarDeclItem::ArrayOfIntInRange {
                    lb,
                    ub,
                    ix: IndexSet(int),
                    id,
                    annos,
                    array_literal,
                },
            ))
        }
        BasicVarType::IntInSet(set) => {
            let (input, array_literal) = array_of_int_expr(input)?;
            Ok((
                input,
                VarDeclItem::ArrayOfIntInSet {
                    set,
                    ix: IndexSet(int),
                    id,
                    annos,
                    array_literal,
                },
            ))
        }
        BasicVarType::FloatInRange(lb, ub) => {
            let (input, array_literal) = array_of_float_expr(input)?;
            Ok((
                input,
                VarDeclItem::ArrayOfFloatInRange {
                    lb,
                    ub,
                    ix: IndexSet(int),
                    id,
                    annos,
                    array_literal,
                },
            ))
        }
        BasicVarType::SetOfIntInRange(lb, ub) => {
            let (input, array_literal) = array_of_set_expr(input)?;
            Ok((
                input,
                VarDeclItem::ArrayOfSetOfIntInRange {
                    lb,
                    ub,
                    ix: IndexSet(int),
                    id,
                    annos,
                    array_literal,
                },
            ))
        }
        BasicVarType::SetOfIntInSet(set) => {
            let (input, array_literal) = array_of_set_expr(input)?;
            Ok((
                input,
                VarDeclItem::ArrayOfSetOfIntInSet {
                    set,
                    ix: IndexSet(int),
                    id,
                    annos,
                    array_literal,
                },
            ))
        }
    }
}
fn vdi_array_without_assignment<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, VarDeclItem, E> {
    // let (input, avt) = array_var_type(input)?;

    let (input, _tag) = tag("array")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, int) = index_set(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, var_type) = basic_var_type(input)?;

    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, id) = var_par_identifier(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, annos) = annotations(input)?;
    let (input, _) = space_or_comment0(input)?;
    match var_type {
        BasicVarType::Bool => Ok((
            input,
            VarDeclItem::ArrayOfBool {
                ix: IndexSet(int),
                id,
                annos,
                array_literal: vec![],
            },
        )),
        BasicVarType::Int => Ok((
            input,
            VarDeclItem::ArrayOfInt {
                ix: IndexSet(int),
                id,
                annos,
                array_literal: vec![],
            },
        )),
        BasicVarType::Float => Ok((
            input,
            VarDeclItem::ArrayOfFloat {
                ix: IndexSet(int),
                id,
                annos,
                array_literal: vec![],
            },
        )),
        BasicVarType::SetOfInt => Ok((
            input,
            VarDeclItem::ArrayOfSet {
                ix: IndexSet(int),
                id,
                annos,
                array_literal: vec![],
            },
        )),
        BasicVarType::IntInRange(lb, ub) => Ok((
            input,
            VarDeclItem::ArrayOfIntInRange {
                lb,
                ub,
                ix: IndexSet(int),
                id,
                annos,
                array_literal: vec![],
            },
        )),
        BasicVarType::IntInSet(set) => Ok((
            input,
            VarDeclItem::ArrayOfIntInSet {
                set,
                ix: IndexSet(int),
                id,
                annos,
                array_literal: vec![],
            },
        )),
        BasicVarType::FloatInRange(lb, ub) => Ok((
            input,
            VarDeclItem::ArrayOfFloatInRange {
                lb,
                ub,
                ix: IndexSet(int),
                id,
                annos,
                array_literal: vec![],
            },
        )),
        BasicVarType::SetOfIntInRange(lb, ub) => Ok((
            input,
            VarDeclItem::ArrayOfSetOfIntInRange {
                lb,
                ub,
                ix: IndexSet(int),
                id,
                annos,
                array_literal: vec![],
            },
        )),
        BasicVarType::SetOfIntInSet(set) => Ok((
            input,
            VarDeclItem::ArrayOfSetOfIntInSet {
                set,
                ix: IndexSet(int),
                id,
                annos,
                array_literal: vec![],
            },
        )),
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
                    Expr::Set(SetLiteral::IntRange(1, 2)),
                    Expr::VarParIdentifier("X_52".to_string())
                ],
                annos: vec![Annotation::Id {
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
) -> IResult<&'a str, ConstraintItem, E> {
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("constraint")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, id) = identifier(input)?;
    let (input, _) = char('(')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, exprs) = separated_list(char(','), expr)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(')')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, annos) = annotations(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(';')(input)?;
    let (input, _) = space_or_comment0(input)?;
    Ok((input, ConstraintItem { id, exprs, annos }))
}
#[test]
fn test_solve_item() {
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
                annotations: vec![Annotation::Id {
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
pub fn solve_item<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SolveItem, E> {
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = tag("solve")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, annotations) = annotations(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, goal) = alt((
        satisfy,
        optimize_bool,
        optimize_int,
        optimize_float,
        optimize_set,
    ))(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(';')(input)?;
    let (input, _) = space_or_comment0(input)?;
    Ok((input, SolveItem { annotations, goal }))
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
    let (input, _) = space_or_comment1(input)?;
    let (input, be) = bool_expr(input)?;
    Ok((input, Goal::OptimizeBool(opt_type, be)))
}
fn optimize_int<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Goal, E> {
    let (input, opt_type) = opt_type(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, be) = int_expr(input)?;
    Ok((input, Goal::OptimizeInt(opt_type, be)))
}
fn optimize_float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Goal, E> {
    let (input, opt_type) = opt_type(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, be) = float_expr(input)?;
    Ok((input, Goal::OptimizeFloat(opt_type, be)))
}
fn optimize_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Goal, E> {
    let (input, opt_type) = opt_type(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, be) = set_expr(input)?;
    Ok((input, Goal::OptimizeSet(opt_type, be)))
}
type Annotations = Vec<Annotation>;
fn annotations<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Annotations, E> {
    let (input, annos) = many0(annotation1)(input)?;
    Ok((input, annos))
}
fn annotation1<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Annotation, E> {
    let (input, _) = tag("::")(input)?;
    let (input, _) = space_or_comment0(input)?;
    annotation(input)
}
#[derive(PartialEq, Clone, Debug)]
pub enum Annotation {
    Id {
        id: String,
        expressions: Vec<AnnExpr>,
    },
}
// <annotation> ::= <identifier>
//                | <identifier> "(" <ann-expr> "," ... ")"
fn annotation<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Annotation, E> {
    let (input, id) = identifier(input)?;
    let (input, we) = opt(char('('))(input)?;
    if we.is_some() {
        let (input, expressions_what) = separated_list(char(','), ann_expr)(input)?;
        let (input, _) = char(')')(input)?;
        Ok((
            input,
            Annotation::Id {
                id,
                expressions: expressions_what,
            },
        ))
    } else {
        let (input, _) = space_or_comment0(input)?;
        Ok((
            input,
            Annotation::Id {
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
fn ann_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AnnExpr, E> {
    let (input, expr) = alt((ann_non_array_expr, ae_annotations))(input)?;
    Ok((input, expr))
}
fn ae_annotations<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AnnExpr, E> {
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, res) = separated_list(char(','), annotation)(input)?;
    let (input, _) = space_or_comment0(input)?;
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
fn ann_non_array_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AnnExpr, E> {
    let (input, expr) = alt((ae_expr, string_lit))(input)?;
    Ok((input, expr))
}
fn ae_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AnnExpr, E> {
    let (input, expr) = expr(input)?;
    Ok((input, AnnExpr::Expr(expr)))
}
#[test]
fn test_string_lit() {
    use nom::error::VerboseError;
    assert_eq!(
        string_lit::<VerboseError<&str>>("\"bla\""),
        Ok(("", AnnExpr::String("bla".to_string())))
    );
}
// TODO: implement support for escaped characters in string literals
fn string_lit<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, AnnExpr, E> {
    let (input, _) = char('"')(input)?;
    let (input, string) = take_while(is_valid)(input)?;
    let (input, _) = char('"')(input)?;
    Ok((input, AnnExpr::String(string.to_string())))
}
fn is_valid(c: char) -> bool {
    match c {
        '"' => false,
        _ => true,
    }
}
#[derive(PartialEq, Clone, Debug)]
pub enum SetLiteral {
    IntRange(i128, i128),
    FloatRange(f64, f64),
    SetFloats(Vec<f64>),
    SetInts(Vec<i128>), // possibly empty
}
fn set_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E> {
    let (input, sl) = alt((
        sl_int_range,
        sl_float_range,
        sl_set_of_ints,
        sl_set_of_floats,
    ))(input)?;
    Ok((input, sl))
}
fn sl_int_range<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E> {
    let (input, lb) = int_literal(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ub) = int_literal(input)?;
    Ok((input, SetLiteral::IntRange(lb, ub)))
}
fn sl_float_range<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E> {
    let (input, lb) = float_literal(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ub) = float_literal(input)?;
    Ok((input, SetLiteral::FloatRange(lb, ub)))
}
// "{" <int-literal> "," ... "}"
fn sl_set_of_ints<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E> {
    let (input, _) = char('{')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list(char(','), int_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, SetLiteral::SetInts(v)))
}
// "{" <float-literal> "," ... "}"
fn sl_set_of_floats<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E> {
    let (input, _) = char('{')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list(char(','), float_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, SetLiteral::SetFloats(v)))
}
fn array_of_bool_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<BoolExpr>, E> {
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, al) = separated_list(char(','), bool_expr)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
}
fn array_of_bool_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<bool>, E> {
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, al) = separated_list(char(','), bool_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
}
fn array_of_int_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<IntExpr>, E> {
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, al) = separated_list(char(','), int_expr)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
}
fn array_of_int_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<i128>, E> {
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, al) = separated_list(char(','), int_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
}
fn array_of_float_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<FloatExpr>, E> {
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, al) = separated_list(char(','), float_expr)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
}
fn array_of_float_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<f64>, E> {
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, al) = separated_list(char(','), float_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
}
fn array_of_set_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<SetExpr>, E> {
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, al) = separated_list(char(','), set_expr)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
}
fn array_of_set_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<SetLiteral>, E> {
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, al) = separated_list(char(','), set_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, al))
}
fn identifier<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, String, E> {
    let (input, first) = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")(input)?;
    let (input, rest) = take_while(is_identifier_rest)(input)?;
    let combine = format!("{}{}", first, rest);
    // check for reserved key words
    if is_reserved_key_word(&combine) {
        Err(Err::Error(ParseError::from_error_kind(
            input,
            ErrorKind::IsA,
        )))
    } else {
        Ok((input, combine))
    }
}

fn var_par_identifier<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, String, E> {
    let (input, first) = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")(input)?;
    let (input, rest) = take_while(is_identifier_rest)(input)?;
    let combine = format!("{}{}", first, rest);
    // check for reserved key words
    if is_reserved_key_word(&combine) {
        Err(Err::Error(ParseError::from_error_kind(
            input,
            ErrorKind::IsA,
        )))
    } else {
        Ok((input, combine))
    }
}
fn is_reserved_key_word(string: &str) -> bool {
    match string {
        "annotation" | "any" | "array" | "bool" | "case" | "constraint" | "diff" | "div"
        | "else" | "elseif" | "endif" | "enum" | "false" | "float" | "function" | "if" | "in"
        | "include" | "int" | "intersect" | "let" | "list" | "maximize" | "minimize" | "mod"
        | "not" | "of" | "satisfy" | "subset" | "superset" | "output" | "par" | "predicate"
        | "record" | "set" | "solve" | "string" | "symdiff" | "test" | "then" | "true"
        | "tuple" | "union" | "type" | "var" | "where" | "xor" => true,
        _ => false,
    }
}
fn is_identifier_rest(c: char) -> bool {
    match c {
        'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o'
        | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' | 'A' | 'B' | 'C'
        | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q'
        | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | '_' | '0' | '1' | '2' | '3'
        | '4' | '5' | '6' | '7' | '8' | '9' => true, //one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789")(input.into()) {
        _ => false,
    }
}
fn bool_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, bool, E> {
    let (input, string) = alt((tag("true"), tag("false")))(input)?;
    match string {
        "true" => Ok((input, true)),
        "false" => Ok((input, false)),
        x => panic!("unmatched bool literal {}", x),
    }
}
fn int_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i128, E> {
    let (input, _) = space_or_comment0(input)?;
    let (input, int) = alt((decimal, hexadecimal, octal))(input)?;
    Ok((input, int as i128))
}
fn decimal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i128, E> {
    let (input, negation) = opt(char('-'))(input)?;
    let (input, int) = map_res(take_while1(is_dec_digit), from_dec)(input)?;
    if negation.is_some() {
        Ok((input, -(int as i128)))
    } else {
        Ok((input, int as i128))
    }
}
#[test]
fn test_hex() {
    use nom::error::VerboseError;
    assert_eq!(hexadecimal::<VerboseError<&str>>("-0x2f"), Ok(("", -47)));
}
fn hexadecimal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i128, E> {
    let (input, negation) = opt(char('-'))(input)?;
    let (input, _tag) = tag("0x")(input)?;
    let (input, int) = map_res(take_while1(is_hex_digit), from_hex)(input)?;
    if negation.is_some() {
        Ok((input, -(int as i128)))
    } else {
        Ok((input, int as i128))
    }
}
#[test]
fn test_oct() {
    use nom::error::VerboseError;
    assert_eq!(octal::<VerboseError<&str>>("0o21"), Ok(("", 17)));
}
fn octal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, i128, E> {
    let (input, negation) = opt(char('-'))(input)?;
    let (input, _tag) = tag("0o")(input)?;
    let (input, int) = map_res(take_while1(is_oct_digit), from_oct)(input)?;
    if negation.is_some() {
        Ok((input, -(int as i128)))
    } else {
        Ok((input, int as i128))
    }
}
fn from_hex(input: &str) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input, 16)
}
fn is_hex_digit(c: char) -> bool {
    c.is_digit(16)
}
fn from_oct(input: &str) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input, 8)
}
fn is_oct_digit(c: char) -> bool {
    c.is_digit(8)
}
fn from_dec(input: &str) -> Result<u128, std::num::ParseIntError> {
    u128::from_str_radix(input, 10)
}
fn is_dec_digit(c: char) -> bool {
    c.is_digit(10)
}
#[test]
fn test_float() {
    use nom::error::VerboseError;
    //TODO should return error
    // float_literal::<VerboseError<&str>>("5")
    assert_eq!(
        float_literal::<VerboseError<&str>>("023.21"),
        Ok(("", 023.21))
    );
    assert_eq!(
        float_literal::<VerboseError<&str>>("0023.21E-098"),
        Ok(("", 0023.21E-098))
    );
    assert_eq!(
        float_literal::<VerboseError<&str>>("0023.21e+098"),
        Ok(("", 0023.21e+098))
    );
    assert_eq!(
        float_literal::<VerboseError<&str>>("002e+098"),
        Ok(("", 002e+098))
    );
    assert_eq!(float_literal::<VerboseError<&str>>("0.21"), Ok(("", 0.21)));
    assert_eq!(float_literal::<VerboseError<&str>>("1.0,"), Ok((",", 1.0)));
}
fn float_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, f64, E> {
    let (input, _) = space_or_comment0(input)?;
    let (input, f) = fz_float(input)?;
    Ok((input, f))
}

fn fz_float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, f64, E> {
    let (input, f) = alt((fz_float1, fz_float2))(input)?;
    let f2 = f.parse::<f64>().unwrap();
    Ok((input, f2))
}

fn fz_float1<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, String, E> {
    let (input, sign) = opt(char('-'))(input)?;
    let (input, a) = take_while1(is_dec_digit)(input)?;
    let (input, _) = char('.')(input)?;
    let (input, b) = take_while1(is_dec_digit)(input)?;
    let (input, rest) = opt(bpart)(input)?;
    if let Some(s) = sign {
        if let Some(rest) = rest {
            Ok((input, format!("{}{}.{}{}", s, a, b, rest)))
        } else {
            Ok((input, format!("{}{}.{}", s, a, b)))
        }
    } else if let Some(rest) = rest {
        Ok((input, format!("{}.{}{}", a, b, rest)))
    } else {
        Ok((input, format!("{}.{}", a, b)))
    }
}
fn fz_float2<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, String, E> {
    let (input, sign) = opt(char('-'))(input)?;
    let (input, digits) = take_while1(is_dec_digit)(input)?;
    let (input, rest) = bpart(input)?;
    if let Some(sign) = sign {
        Ok((input, format!("{}{}{}", sign, digits, rest)))
    } else {
        Ok((input, format!("{}{}", digits, rest)))
    }
}
fn bpart<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, String, E> {
    let (input, e) = alt((tag("e"), tag("E")))(input)?;
    let (input, sign) = opt(alt((tag("-"), tag("+"))))(input)?;
    let (input, digits) = take_while1(is_dec_digit)(input)?;
    if let Some(sign) = sign {
        Ok((input, format!("{}{}{}", e, sign, digits)))
    } else {
        Ok((input, format!("{}{}", e, digits)))
    }
}
// white space or comments
fn space_or_comment0<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    let (input, s) = alt((comment, multispace0))(input)?;
    Ok((input, s))
}
fn space_or_comment1<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    let (input, s) = alt((comment, multispace1))(input)?;
    Ok((input, s))
}
#[test]
fn test_comment() {
    use nom::error::VerboseError;
    assert_eq!(
        comment::<VerboseError<&str>>("% Comments can have anyth!ng in it really <3"),
        Ok(("", " Comments can have anyth!ng in it really <3".into()))
    );
}
fn comment<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    let (input, _) = multispace0(input)?;
    let (input, _) = char('%')(input)?;
    let (input, string) = take_till(|c| c == '\n')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = opt(comment)(input)?;
    Ok((input, string))
}
