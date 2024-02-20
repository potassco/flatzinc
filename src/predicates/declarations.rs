use winnow::{
    combinator::{eof, separated},
    error::{FromExternalError, ParserError},
    PResult, Parser,
};

use crate::{
    comments::{space_or_comment0, space_or_comment1},
    predicates::types::{pred_par_type, PredParType},
    primitive_literals::identifier,
};

#[derive(PartialEq, Clone, Debug)]
pub struct PredicateItem {
    pub id: String,
    pub parameters: Vec<(PredParType, String)>,
}

pub fn predicate_item<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> PResult<PredicateItem, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    space_or_comment0(input)?;
    "predicate".parse_next(input)?;
    let item = predicate_item_tail(input).map_err(|e| e.cut())?;
    Ok(item)
}
pub fn predicate_item_tail<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<PredicateItem, E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    space_or_comment1(input)?;
    let id = identifier(input)?;
    '('.parse_next(input)?;
    let parameters = separated(1.., pred_par_type_ident_pair, ",").parse_next(input)?;
    ')'.parse_next(input)?;
    space_or_comment0(input)?;
    ';'.parse_next(input)?;
    space_or_comment0(input)?;
    eof.parse_next(input)?;
    Ok(PredicateItem { id, parameters })
}
#[test]
fn test_predicate_item() {
    use crate::predicates::types::BasicPredParType;
    use std::str;
    use winnow::error::ContextError;
    let mut input = "predicate float_03({1.0,3.3}:c);";
    assert_eq!(
        predicate_item::<ContextError<&str>>(&mut input),
        Ok(PredicateItem {
            id: "float_03".to_string(),
            parameters: vec![(
                PredParType::Basic(BasicPredParType::FloatInSet(vec![1.0, 3.3])),
                "c".to_string()
            )]
        })
    );
}
#[test]
fn test_predicate_item2() {
    use crate::predicates::types::BasicPredParType;
    use std::str;
    use winnow::error::ContextError;
    let mut input = "predicate my_pred({1.0,3.3}:c);";
    assert_eq!(
        predicate_item::<ContextError<&str>>(&mut input),
        Ok(PredicateItem {
            id: "my_pred".to_string(),
            parameters: vec![(
                PredParType::Basic(BasicPredParType::FloatInSet(vec![1.0, 3.3])),
                "c".to_string()
            )]
        })
    );
}
#[test]
#[should_panic]
fn test_predicate_item_3() {
    use std::str;
    use winnow::error::ContextError;
    let mut input = "predicate float_01(set of float:c);";
    predicate_item::<ContextError<&str>>(&mut input).unwrap();
}

pub fn pred_par_type_ident_pair<'a, E: ParserError<&'a str>>(
    input: &mut &'a str,
) -> PResult<(PredParType, String), E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    space_or_comment0(input)?;
    let pred_par_type = pred_par_type(input)?;
    space_or_comment0(input)?;
    ':'.parse_next(input)?;
    space_or_comment0(input)?;
    let ident = identifier(input)?;
    space_or_comment0(input)?;
    Ok((pred_par_type, ident))
}
#[test]
fn test_pred_par_type_ident_pair() {
    use crate::predicates::declarations;
    use crate::predicates::types;
    use winnow::error::ContextError;
    let mut input = "var set of int: g";
    assert_eq!(
        declarations::pred_par_type_ident_pair::<ContextError<&str>>(&mut input),
        Ok((
            types::PredParType::Basic(types::BasicPredParType::VarSetOfInt),
            "g".to_string()
        ))
    );
}
