use winnow::{
    combinator::opt,
    error::{FromExternalError, ParserError},
    PResult, Parser,
};

use crate::{
    basic_types::BasicType,
    comments::space_or_comment0,
    expressions::{
        annotations, array_of_bool_expr, array_of_float_expr, array_of_int_expr, array_of_set_expr,
        bool_expr, float_expr, int_expr, set_expr, Annotations, ArrayOfBoolExpr, ArrayOfFloatExpr,
        ArrayOfIntExpr, ArrayOfSetExpr, BoolExpr, FloatExpr, IntExpr, SetExpr,
    },
    primitive_literals::{var_par_identifier, IndexSet},
    variables::types::{var_type, BasicVarType, VarType},
};

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

pub fn var_decl_item<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<VarDeclItem, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    space_or_comment0(input)?;
    let item = vdi_var(input)?;
    space_or_comment0(input)?;
    ';'.parse_next(input)?;
    space_or_comment0(input)?;
    Ok(item)
}
#[test]
fn test_var_decl_item_1() {
    use crate::{AnnExpr, Annotation, ArrayOfSetExpr, Expr, IntExpr, SetExpr, SetLiteralExpr};
    use winnow::error::ContextError;
    let mut input = "array [1..1] of var set of 1..10: sets:: output_array([1..1]) = [X_0];";
    assert_eq!(
        var_decl_item::<ContextError<&str>>(&mut input),
        Ok(VarDeclItem::ArrayOfSubSetOfIntRange {
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
        })
    );
}
#[test]
fn test_var_decl_item_2() {
    use winnow::error::ContextError;
    let mut input = "array [1..5] of var 0..3: w =X_32;";
    assert_eq!(
        var_decl_item::<ContextError<&str>>(&mut input),
        Ok(VarDeclItem::ArrayOfIntInRange {
            id: "w".to_string(),
            ix: IndexSet(5),
            lb: 0,
            ub: 3,
            array_expr: Some(ArrayOfIntExpr::VarParIdentifier("X_32".to_string())),
            annos: vec![],
        })
    );
}
#[test]
fn test_var_decl_item_3() {
    use winnow::error::ContextError;
    let mut input = "array [1..5] of var {1,2,3}: w;";
    assert_eq!(
        var_decl_item::<ContextError<&str>>(&mut input),
        Ok(VarDeclItem::ArrayOfIntInSet {
            id: "w".to_string(),
            ix: IndexSet(5),
            set: vec![1, 2, 3],
            array_expr: None,
            annos: vec![],
        })
    );
}
#[test]
fn test_var_decl_item_4() {
    use crate::Annotation;
    use winnow::error::ContextError;
    let mut input = "array [1..5] of var 0..3: w;";
    assert_eq!(
        var_decl_item::<ContextError<&str>>(&mut input),
        Ok(VarDeclItem::ArrayOfIntInRange {
            id: "w".to_string(),
            ix: IndexSet(5),
            lb: 0,
            ub: 3,
            array_expr: None,
            annos: vec![],
        })
    );
    let mut input = "var 1..101: objective :: output_var = X_2586;";
    assert_eq!(
        var_decl_item::<ContextError<&str>>(&mut input),
        Ok(VarDeclItem::IntInRange {
            id: "objective".to_string(),
            lb: 1,
            ub: 101,
            expr: Some(IntExpr::VarParIdentifier("X_2586".to_string())),
            annos: vec![Annotation {
                id: "output_var".to_string(),
                expressions: vec![]
            }],
        })
    );
}
#[test]
fn test_var_decl_item_5() {
    use crate::{ArrayOfSetExpr, SetExpr, SetLiteralExpr};
    use winnow::error::ContextError;
    let mut input = "array [1..3] of var set of 17..42: h = [{42,17},23..X,{}];";
    assert_eq!(
        var_decl_item::<ContextError<&str>>(&mut input),
        Ok(VarDeclItem::ArrayOfSubSetOfIntRange {
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
        })
    );
}

fn vdi_var<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<VarDeclItem, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let vt = var_type(input)?;
    space_or_comment0(input)?;
    ':'.parse_next(input)?;
    space_or_comment0(input)?;
    let id = var_par_identifier(input)?;
    space_or_comment0(input)?;
    let annos = annotations(input)?;
    space_or_comment0(input)?;
    let assign = opt('=').parse_next(input)?;
    let assign = assign.is_some();
    space_or_comment0(input)?;
    match vt {
        VarType::BasicVarType(bvt) => match bvt {
            BasicVarType::BasicType(BasicType::Bool) => {
                let expr = parse_rhs(assign, bool_expr, input)?;
                Ok(VarDeclItem::Bool { id, annos, expr })
            }
            BasicVarType::BasicType(BasicType::Int) => {
                let expr = parse_rhs(assign, int_expr, input)?;
                Ok(VarDeclItem::Int { id, annos, expr })
            }
            BasicVarType::BasicType(BasicType::Float) => {
                let expr = parse_rhs(assign, float_expr, input)?;
                Ok(VarDeclItem::Float { id, annos, expr })
            }
            BasicVarType::IntInRange(lb, ub) => {
                let expr = parse_rhs(assign, int_expr, input)?;
                Ok(VarDeclItem::IntInRange {
                    id,
                    lb,
                    ub,
                    expr,
                    annos,
                })
            }
            BasicVarType::IntInSet(set) => {
                let expr = parse_rhs(assign, int_expr, input)?;
                Ok(VarDeclItem::IntInSet {
                    id,
                    set,
                    expr,
                    annos,
                })
            }
            BasicVarType::BoundedFloat(lb, ub) => {
                let expr = parse_rhs(assign, float_expr, input)?;
                Ok(VarDeclItem::BoundedFloat {
                    id,
                    lb,
                    ub,
                    expr,
                    annos,
                })
            }
            BasicVarType::SubSetOfIntRange(lb, ub) => {
                let expr = parse_rhs(assign, set_expr, input)?;
                Ok(VarDeclItem::SubSetOfIntRange {
                    id,
                    lb,
                    ub,
                    expr,
                    annos,
                })
            }
            BasicVarType::SubSetOfIntSet(set) => {
                let expr = parse_rhs(assign, set_expr, input)?;
                Ok(VarDeclItem::SubSetOfIntSet {
                    id,
                    set,
                    expr,
                    annos,
                })
            }
        },
        VarType::Array { ix, var_type } => match var_type {
            BasicVarType::BasicType(bt) => match bt {
                BasicType::Bool => {
                    let array_expr = parse_rhs(assign, array_of_bool_expr, input)?;
                    Ok(VarDeclItem::ArrayOfBool {
                        ix,
                        id,
                        annos,
                        array_expr,
                    })
                }
                BasicType::Int => {
                    let array_expr = parse_rhs(assign, array_of_int_expr, input)?;
                    Ok(VarDeclItem::ArrayOfInt {
                        ix,
                        id,
                        annos,
                        array_expr,
                    })
                }
                BasicType::Float => {
                    let array_expr = parse_rhs(assign, array_of_float_expr, input)?;
                    Ok(VarDeclItem::ArrayOfFloat {
                        ix,
                        id,
                        annos,
                        array_expr,
                    })
                }
            },
            BasicVarType::IntInRange(lb, ub) => {
                let array_expr = parse_rhs(assign, array_of_int_expr, input)?;
                Ok(VarDeclItem::ArrayOfIntInRange {
                    lb,
                    ub,
                    ix,
                    id,
                    annos,
                    array_expr,
                })
            }
            BasicVarType::IntInSet(set) => {
                let array_expr = parse_rhs(assign, array_of_int_expr, input)?;
                Ok(VarDeclItem::ArrayOfIntInSet {
                    set,
                    ix,
                    id,
                    annos,
                    array_expr,
                })
            }
            BasicVarType::BoundedFloat(lb, ub) => {
                let array_expr = parse_rhs(assign, array_of_float_expr, input)?;
                Ok(VarDeclItem::ArrayOfBoundedFloat {
                    lb,
                    ub,
                    ix,
                    id,
                    annos,
                    array_expr,
                })
            }
            BasicVarType::SubSetOfIntRange(lb, ub) => {
                let array_expr = parse_rhs(assign, array_of_set_expr, input)?;
                Ok(VarDeclItem::ArrayOfSubSetOfIntRange {
                    lb,
                    ub,
                    ix,
                    id,
                    annos,
                    array_expr,
                })
            }
            BasicVarType::SubSetOfIntSet(set) => {
                let array_expr = parse_rhs(assign, array_of_set_expr, input)?;
                Ok(VarDeclItem::ArrayOfSubSetOfIntSet {
                    set,
                    ix,
                    id,
                    annos,
                    array_expr,
                })
            }
        },
    }
}

/// Parse the right hand side of a variable declaration if there is an assignment
fn parse_rhs<'a, O, E>(
    assign: bool,
    parser: impl Fn(&mut &'a str) -> PResult<O, E>,
    input: &mut &'a str,
) -> PResult<Option<O>, E> {
    Ok(if assign {
        let expr = parser(input)?;
        Some(expr)
    } else {
        None
    })
}
