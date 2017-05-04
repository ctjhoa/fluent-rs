#[macro_use]
extern crate nom;

use nom::{IResult, space, alphanumeric, eol};

use std::str;

// Entry:
// - Entity
// - Macro
// - Comment

named!(word, take_while!(call!(|c| {
    c != '{' as u8 &&
        c != '}' as u8 &&
        c != '[' as u8 &&
        c != ']' as u8 &&
        c != '\n' as u8 &&
        c != '\\' as u8
})));

named!(section<&str>, map_res!(
    delimited!(
        tag!("[["),
        word,
        tag!("]]")
    ),
    str::from_utf8
));

named!(value <Vec<&str> >,
    alt!(
        do_parse!(
            opt!(space) >>
            char!('\n') >>
            val: many0!(
                do_parse!(
                    space >>
                    line: map_res!(
                        take_while!(call!(|c| c != '\n' as u8)),
                        str::from_utf8
                    ) >>
                    eol >>
                    (line)
                )
            ) >>
            (val)
        ) |
        do_parse!(
            opt!(space) >>
            val: map_res!(
                take_while!(call!(|c| c != '\n' as u8)),
                str::from_utf8
            ) >>
            eol >>
            (vec![val])
        )
    )
);

named!(entity_value <&[u8], (&str,Vec<&str>)>,
    do_parse!(
        key: map_res!(alphanumeric, str::from_utf8) >>
        opt!(space) >>
        char!('=') >>
        val: value >>
        (key, val)
    )
);

#[test]
fn parse_section_test() {
    let ftl_file = &b"[[section]]
entity1=value1
entity2 = value2"[..];

    let ftl_without_section = &b"\nentity1=value1
entity2 = value2"[..];

    let res = section(ftl_file);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {:?} | o: {:?}", str::from_utf8(i), o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(ftl_without_section, "section"));
}

#[test]
fn parse_value_oneline_test() {
    let ftl_value = &b"value1
entity2 = value2"[..];

    let ftl_without_value = &b"entity2 = value2"[..];

    let res = value(ftl_value);
    assert_eq!(res, IResult::Done(ftl_without_value, vec!["value1"]));
}

#[test]
fn parse_value_multiline_test() {
    let ftl_value = &b"
 value1_1
 value1_2
entity2 = value2"[..];

    let ftl_without_value = &b"entity2 = value2"[..];

    let res = value(ftl_value);
    assert_eq!(res, IResult::Done(ftl_without_value, vec!["value1_1", "value1_2"]));
}

#[test]
fn parse_entity_value_oneline_test() {
    let ftl_value = &b"entity1=value 1
entity2 = value2"[..];

    let ftl_without_entity_value = &b"entity2 = value2"[..];

    let res = entity_value(ftl_value);
    assert_eq!(res, IResult::Done(ftl_without_entity_value, ("entity1",vec!["value 1"])));
}

#[test]
fn parse_entity_value_multiline_test() {
    let ftl_value = &b"entity1=
 value1 1
 value1 2
entity2 = value2"[..];

    let ftl_without_entity_value = &b"entity2 = value2"[..];

    let res = entity_value(ftl_value);
    assert_eq!(res, IResult::Done(ftl_without_entity_value, ("entity1", vec!["value1 1", "value1 2"])));
}
