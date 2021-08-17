use std::str;

use nom::bytes::complete::tag;
use nom::character::complete::char;
use nom::multi::separated_list1;

use crate::statements::predicate_types;
use crate::statements::predicate_types::PredParType;
use crate::{comments, primitive_literals, FromExternalError, IResult, ParseError};

#[test]
fn test_predicate_item() {
    use crate::statements::predicate_types::BasicPredParType;
    use nom::error::VerboseError;
    use std::str;
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
    use nom::error::VerboseError;
    use std::str;
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

pub fn pred_par_type_ident_pair<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, (PredParType, String), E>
where
    E: FromExternalError<&'a str, std::num::ParseIntError>
        + FromExternalError<&'a str, std::num::ParseFloatError>,
{
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, pred_par_type) = predicate_types::pred_par_type(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    let (input, ident) = primitive_literals::identifier(input)?;
    let (input, _) = comments::space_or_comment0(input)?;
    Ok((input, (pred_par_type, ident)))
}
