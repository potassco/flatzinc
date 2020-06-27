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
    let (input, _) = space_or_comment1(input)?;
    let (input, _tag) = tag("of")(input)?;
    let (input, _) = space_or_comment1(input)?;
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
fn var_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, VarType, E> {
    let (input, var_type) = alt((vt_basic_var_type, array_var_type))(input)?;
    Ok((input, var_type))
}
fn vt_basic_var_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, VarType, E> {
    let (input, vt) = basic_var_type(input)?;
    Ok((input, VarType::BasicVarType(vt)))
}
fn array_var_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, VarType, E> {
    let (input, _) = tag("array")(input)?;
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
fn par_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, ParType, E> {
    let (input, par_type) = alt((pt_basic_par_type, array_par_type))(input)?;
    Ok((input, par_type))
}
fn pt_basic_par_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, ParType, E> {
    let (input, pt) = basic_par_type(input)?;
    Ok((input, ParType::BasicParType(pt)))
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
fn basic_var_type<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, BasicVarType, E> {
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("var")(input)?;
    let (input, _) = space_or_comment1(input)?;
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
fn bvt_bounded_float<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicVarType, E> {
    let (input, (lb, ub)) = bounded_float(input)?;
    Ok((input, BasicVarType::BoundedFloat(lb, ub)))
}
fn bvt_subset_of_int_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicVarType, E> {
    let (input, (lb, ub)) = subset_of_int_range(input)?;
    Ok((input, BasicVarType::SubSetOfIntRange(lb, ub)))
}
fn bvt_subset_of_int_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicVarType, E> {
    let (input, set) = subset_of_int_set(input)?;
    Ok((input, BasicVarType::SubSetOfIntSet(set)))
}
fn int_in_range<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (i128, i128), E> {
    let (input, lb) = int_literal(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ub) = int_literal(input)?;
    Ok((input, (lb, ub)))
}
fn bounded_float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (f64, f64), E> {
    let (input, lb) = float_literal(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ub) = float_literal(input)?;
    Ok((input, (lb, ub)))
}
// "{" <float-literal> "," ... "}"
fn float_in_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Vec<f64>, E> {
    let (input, _) = char('{')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list(char(','), float_literal)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, v))
}
// "set" "of" <int_literal> ".." <int_literal>
fn subset_of_int_range<'a, E: ParseError<&'a str>>(
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
fn subset_of_int_set<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Vec<i128>, E> {
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
) -> IResult<&'a str, BasicPredParType, E> {
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
) -> IResult<&'a str, BasicPredParType, E> {
    let (input, bvt) = basic_var_type(input)?;
    Ok((input, BasicPredParType::BasicVarType(bvt)))
}
fn bppt_var_set_of_int<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E> {
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = tag("var")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, _) = tag("set")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, _) = tag("of")(input)?;
    let (input, _) = space_or_comment1(input)?;
    let (input, _) = tag("int")(input)?;
    let (input, _) = space_or_comment0(input)?;
    Ok((input, BasicPredParType::VarSetOfInt))
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
fn bppt_bounded_float<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E> {
    let (input, (lb, ub)) = bounded_float(input)?;
    Ok((input, BasicPredParType::BoundedFloat(lb, ub)))
}
fn bppt_float_in_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E> {
    let (input, set) = float_in_set(input)?;
    Ok((input, BasicPredParType::FloatInSet(set)))
}
fn bppt_subset_of_int_range<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E> {
    let (input, (lb, ub)) = subset_of_int_range(input)?;
    Ok((input, BasicPredParType::SubSetOfIntRange(lb, ub)))
}
fn bppt_subset_of_int_set<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, BasicPredParType, E> {
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
    Set(SetLiteralExpr),
    VarParIdentifier(String),
}
fn set_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetExpr, E> {
    let (input, expr) = alt((se_set_literal_expr, se_var_par_identifier))(input)?;
    Ok((input, expr))
}
fn se_set_literal_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetExpr, E> {
    let (input, sl) = set_literal_expr(input)?;
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
fn expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, _) = space_or_comment0(input)?;
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
    let (input, id) = var_par_identifier(input)?;
    Ok((input, Expr::VarParIdentifier(id)))
}
fn e_bool_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, b) = bool_literal(input)?;
    Ok((input, Expr::Bool(b)))
}
fn e_int_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, int) = int_literal(input)?;
    Ok((input, Expr::Int(int)))
}
fn e_float_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, float) = float_literal(input)?;
    Ok((input, Expr::Float(float)))
}
fn e_set_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, set) = set_literal_expr(input)?;
    Ok((input, Expr::Set(set)))
}
fn e_array_of_bool_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, v) = array_of_bool_expr_literal(input)?;
    Ok((input, Expr::ArrayOfBool(v)))
}
fn e_array_of_int_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, v) = array_of_int_expr_literal(input)?;
    Ok((input, Expr::ArrayOfInt(v)))
}
fn e_array_of_float_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
    let (input, v) = array_of_float_expr_literal(input)?;
    Ok((input, Expr::ArrayOfFloat(v)))
}
fn e_array_of_set_expr<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Expr, E> {
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
pub fn par_decl_item<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ParDeclItem, E> {
    let (input, _) = space_or_comment0(input)?;
    let (input, ptype) = par_type(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, id) = var_par_identifier(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('=')(input)?;
    let (input, _) = space_or_comment0(input)?;
    match ptype {
        ParType::BasicParType(bpt) => match bpt {
            BasicParType::BasicType(bt) => match bt {
                BasicType::Bool => {
                    let (input, bool) = bool_literal(input)?;
                    let (input, _) = space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = space_or_comment0(input)?;
                    Ok((input, ParDeclItem::Bool { id, bool }))
                }
                BasicType::Int => {
                    let (input, int) = int_literal(input)?;
                    let (input, _) = space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = space_or_comment0(input)?;
                    Ok((input, ParDeclItem::Int { id, int }))
                }
                BasicType::Float => {
                    let (input, float) = float_literal(input)?;
                    let (input, _) = space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = space_or_comment0(input)?;
                    Ok((input, ParDeclItem::Float { id, float }))
                }
            },
            BasicParType::SetOfInt => {
                let (input, set_literal) = set_literal(input)?;
                let (input, _) = space_or_comment0(input)?;
                let (input, _) = char(';')(input)?;
                let (input, _) = space_or_comment0(input)?;
                Ok((input, ParDeclItem::SetOfInt { id, set_literal }))
            }
        },
        ParType::Array { ix, par_type } => match par_type {
            BasicParType::BasicType(bt) => match bt {
                BasicType::Bool => {
                    let (input, v) = array_of_bool_literal(input)?;
                    let (input, _) = space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = space_or_comment0(input)?;
                    Ok((input, ParDeclItem::ArrayOfBool { ix, id, v }))
                }
                BasicType::Int => {
                    let (input, v) = array_of_int_literal(input)?;
                    let (input, _) = space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = space_or_comment0(input)?;
                    Ok((input, ParDeclItem::ArrayOfInt { ix, id, v }))
                }
                BasicType::Float => {
                    let (input, v) = array_of_float_literal(input)?;
                    let (input, _) = space_or_comment0(input)?;
                    let (input, _) = char(';')(input)?;
                    let (input, _) = space_or_comment0(input)?;
                    Ok((input, ParDeclItem::ArrayOfFloat { ix, id, v }))
                }
            },
            BasicParType::SetOfInt => {
                let (input, v) = array_of_set_literal(input)?;
                let (input, _) = space_or_comment0(input)?;
                let (input, _) = char(';')(input)?;
                let (input, _) = space_or_comment0(input)?;
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
pub fn var_decl_item<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, VarDeclItem, E> {
    let (input, _) = space_or_comment0(input)?;
    let (input, item) = vdi_var(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(';')(input)?;
    let (input, _) = space_or_comment0(input)?;
    Ok((input, item))
}
fn vdi_var<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, VarDeclItem, E> {
    let (input, vt) = var_type(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, id) = var_par_identifier(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, annos) = annotations(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, assign) = opt(char('='))(input)?;
    let (input, _) = space_or_comment0(input)?;
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
pub struct Annotation {
    pub id: String,
    pub expressions: Vec<AnnExpr>,
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
            Annotation {
                id,
                expressions: expressions_what,
            },
        ))
    } else {
        let (input, _) = space_or_comment0(input)?;
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
pub enum SetLiteralExpr {
    IntInRange(IntExpr, IntExpr),
    BoundedFloat(FloatExpr, FloatExpr),
    SetFloats(Vec<FloatExpr>),
    SetInts(Vec<IntExpr>),
}
fn set_literal_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SetLiteralExpr, E> {
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
) -> IResult<&'a str, SetLiteralExpr, E> {
    let (input, lb) = int_expr(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ub) = int_expr(input)?;
    Ok((input, SetLiteralExpr::IntInRange(lb, ub)))
}
fn sle_bounded_float<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SetLiteralExpr, E> {
    let (input, lb) = float_expr(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ub) = float_expr(input)?;
    Ok((input, SetLiteralExpr::BoundedFloat(lb, ub)))
}
// "{" <int-expr> "," ... "}"
fn sle_set_of_ints<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SetLiteralExpr, E> {
    let (input, _) = char('{')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list(char(','), int_expr)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, SetLiteralExpr::SetInts(v)))
}
// "{" <float-expr> "," ... "}"
fn sle_set_of_floats<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, SetLiteralExpr, E> {
    let (input, _) = char('{')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list(char(','), float_expr)(input)?;
    let (input, _) = space_or_comment0(input)?;
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
fn set_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E> {
    let (input, sl) = alt((
        sl_int_range,
        sl_bounded_float,
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
fn sl_bounded_float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, SetLiteral, E> {
    let (input, lb) = float_literal(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _tag) = tag("..")(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, ub) = float_literal(input)?;
    Ok((input, SetLiteral::BoundedFloat(lb, ub)))
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
#[derive(PartialEq, Clone, Debug)]
pub enum ArrayOfBoolExpr {
    Array(Vec<BoolExpr>),
    VarParIdentifier(String),
}
fn array_of_bool_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ArrayOfBoolExpr, E> {
    let (input, id) = opt(var_par_identifier)(input)?;
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
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list(char(','), bool_expr)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, v))
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
#[derive(PartialEq, Clone, Debug)]
pub enum ArrayOfIntExpr {
    Array(Vec<IntExpr>),
    VarParIdentifier(String),
}
fn array_of_int_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ArrayOfIntExpr, E> {
    let (input, id) = opt(var_par_identifier)(input)?;
    if let Some(id) = id {
        Ok((input, ArrayOfIntExpr::VarParIdentifier(id)))
    } else {
        let (input, v) = array_of_int_expr_literal(input)?;
        Ok((input, ArrayOfIntExpr::Array(v)))
    }
}
fn array_of_int_expr_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<IntExpr>, E> {
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list(char(','), int_expr)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, v))
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
#[derive(PartialEq, Clone, Debug)]
pub enum ArrayOfFloatExpr {
    Array(Vec<FloatExpr>),
    VarParIdentifier(String),
}
fn array_of_float_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ArrayOfFloatExpr, E> {
    let (input, id) = opt(var_par_identifier)(input)?;
    if let Some(id) = id {
        Ok((input, ArrayOfFloatExpr::VarParIdentifier(id)))
    } else {
        let (input, v) = array_of_float_expr_literal(input)?;
        Ok((input, ArrayOfFloatExpr::Array(v)))
    }
}
fn array_of_float_expr_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<FloatExpr>, E> {
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list(char(','), float_expr)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, v))
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
#[derive(PartialEq, Clone, Debug)]
pub enum ArrayOfSetExpr {
    Array(Vec<SetExpr>),
    VarParIdentifier(String),
}
fn array_of_set_expr<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, ArrayOfSetExpr, E> {
    let (input, id) = opt(var_par_identifier)(input)?;
    if let Some(id) = id {
        Ok((input, ArrayOfSetExpr::VarParIdentifier(id)))
    } else {
        let (input, v) = array_of_set_expr_literal(input)?;
        Ok((input, ArrayOfSetExpr::Array(v)))
    }
}
fn array_of_set_expr_literal<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<SetExpr>, E> {
    let (input, _) = char('[')(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, v) = separated_list(char(','), set_expr)(input)?;
    let (input, _) = space_or_comment0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, v))
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
