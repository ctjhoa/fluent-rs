#[macro_use]
extern crate nom;
#[macro_use]
extern crate lazy_static;
extern crate regex;

use nom::{IResult, space, alphanumeric, eol};

use std::str;

// TODO: body ::= (_* NL)* (entry NL)* entry? EOF
// TODO: entry ::= comment | section | message

named!(comment<&str>, map_res!(do_parse!(
    char!('/') >>
    char!('/') >>
    value: take_while!(call!(|c| {
        c != '\r' as u8 &&
        c != '\n' as u8
    })) >>
    eol >>
    (value)
), str::from_utf8));
named!(section<&str>, map_res!(delimited!(
    tag!("[["),
    variant_symbol,
    tag!("]]")
), str::from_utf8));

named!(break_indent<&[u8], ()>, do_parse!(
    many1!(
        do_parse!(
            many0!(eol) >>
            space >>
            ()
        )
    ) >>
    ()
));

named!(identifier, re_bytes_find_static!(r"^[a-zA-Z_?-][a-zA-Z0-9_?-]*"));
named!(external, do_parse!(
    char!('$') >>
    identifier: identifier >>
    (identifier)
));

named!(word, take_while!(call!(|c| {
    c != '{' as u8 &&
    c != '}' as u8 &&
    c != '[' as u8 &&
    c != ']' as u8 &&
    c != '\n' as u8 &&
    c != '\r' as u8 &&
    c != '\\' as u8
})));
named!(builtin, re_bytes_find_static!(r"^[A-Z_?-]+"));
named!(number, re_bytes_find_static!(r"^[-+]?[0-9]*\.?[0-9]+"));

named!(variant_key, alt!(number | variant_symbol));
named!(variant_symbol, take_while!(call!(|c| {
    c != '{' as u8 &&
    c != '}' as u8 &&
    c != '[' as u8 &&
    c != ']' as u8 &&
    c != '\n' as u8 &&
    c != '\r' as u8 &&
    c != ' ' as u8 &&
    c != '\t' as u8 &&
    c != '\\' as u8
})));
// TODO: variant ::= NL __ '[' _? variant-key _? ']' __ pattern
// TODO: default-variant ::= NL __ '*[' _? variant-key _? ']' __ pattern
// TODO: variant-list ::= variant* default-variant variant*

named!(tag, do_parse!(
    char!('#') >>
    word: word >>
    (word)
));
named!(tag_list <Vec<&str> >, do_parse!(
    eol >>
    list: many1!(do_parse!(
        break_indent >>
        tag: map_res!(tag, str::from_utf8) >>
        (tag)
    )) >>
    (list)
));

// TODO: attribute ::= NL __ '.' identifier value
// TODO: attribute-list ::= attribute+

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
fn parse_identifier_test() {
    let ftl = &b"iden-tifi?er blah"[..];

    let ftl_without_identifier = &b" blah"[..];

    let res = identifier(ftl);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {:?} | o: {:?}", str::from_utf8(i), o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(ftl_without_identifier, &b"iden-tifi?er"[..]));
}

#[test]
fn parse_external_test() {
    let ftl = &b"$iden-tifi?er blah"[..];

    let ftl_without_external = &b" blah"[..];

    let res = external(ftl);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {:?} | o: {:?}", str::from_utf8(i), o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(ftl_without_external, &b"iden-tifi?er"[..]));
}

#[test]
fn parse_section_test() {
    let ftl = &b"[[section]]
entity1=value1
entity2 = value2"[..];

    let ftl_without_section = &b"\nentity1=value1
entity2 = value2"[..];

    let res = section(ftl);
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
