#[macro_use]
extern crate nom;
#[macro_use]
extern crate lazy_static;
extern crate regex;

use nom::{IResult, space, alphanumeric, eol};

use std::str;

// TODO: body ::= (_* NL)* (entry NL)* entry? EOF
// TODO: entry ::= comment | section | message

named!(comment<&str, &str>, do_parse!(
    tag_s!("//") >>
    value: take_while_s!(call!(|c| {
        c != '\r' &&
        c != '\n'
    })) >>
    eol >>
    (value)
));
named!(section<&str, &str>, delimited!(
    tag_s!("[["),
    variant_symbol,
    tag_s!("]]")
));

named!(break_indent<&str, ()>, do_parse!(
    many1!(
        do_parse!(
            many0!(eol) >>
            space >>
            ()
        )
    ) >>
    ()
));

named!(identifier<&str, &str>, re_find_static!(r"^[a-zA-Z_?-][a-zA-Z0-9_?-]*"));
named!(external<&str, &str>, do_parse!(
    char!('$') >>
    identifier: identifier >>
    (identifier)
));

named!(word<&str, &str>, take_while_s!(call!(|c| {
    c != '{' &&
    c != '}' &&
    c != '[' &&
    c != ']' &&
    c != ' ' &&
    c != '\n' &&
    c != '\r' &&
    c != '\\'
})));
named!(builtin <&str, &str>, re_find_static!(r"^[A-Z_?-]+"));
named!(number <&str, &str>, re_find_static!(r"^[-+]?[0-9]*\.?[0-9]+"));

named!(variant_key <&str, &str>, alt!(number | variant_symbol));
named!(variant_symbol<&str, &str>, re_find_static!(r"^[\w&&[^\{\}\[\]\s\\]]+([\t\x20][\w&&[^\{\}\[\]\s\\]]+)*"));

// TODO: variant ::= NL __ '[' _? variant-key _? ']' __ pattern
// WAIT FOR 'pattern'
// named!(variant<&str, (&str, &str)>, do_parse!(
//     eol >>
//     break_indent >>
//     char!('[') >>
//     opt!(space) >>
//     variant_key: variant_key >>
//     opt!(space) >>
//     char!(']') >>
//     pattern: pattern >>
//     (variant_key, pattern)
// ));
// TODO: default-variant ::= NL __ '*[' _? variant-key _? ']' __ pattern
// TODO: variant-list ::= variant* default-variant variant*

named!(tag <&str, &str>, do_parse!(
    char!('#') >>
    word: word >>
    (word)
));
named!(tag_list <&str, Vec<&str> >, do_parse!(
    eol >>
    list: many1!(do_parse!(
        break_indent >>
        tag: tag >>
        (tag)
    )) >>
    (list)
));

// TODO: attribute ::= NL __ '.' identifier value
// WAIT FOR 'value'
// named!(attribute<&str, (&str, <Vec<&str>)>, do_parse!(
//     eol >>
//     break_indent >>
//     char!('.') >>
//     identifier: identifier >>
//     value: value >>
//     (identifier, value)
// ));
// TODO: attribute-list ::= attribute+

// TODO: message              ::= identifier ((value tag-list?) | (value? attribute-list))
// TODO: value                ::= _? '=' __? pattern
// COMPLETE REWORK
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

// TODO: pattern              ::= (text | placeable)+
//       /* text can only include newlines if they're followed by an indent */
//       /* \ and { must be escaped */
// TODO: text-char            ::= (char - line-break) - [#x5c#x7b]
//                          | break-indent
//                          | '\u' hexdigit hexdigit hexdigit hexdigit
//                          | '\' [#x5c#x7b]

// TODO: text                 ::= text-char+

// TODO: handle escaped quote when https://github.com/Geal/nom/issues/300 will be fixed
named!(quoted_text <&str, &str>, do_parse!(
    char!('"') >>
    text: take_while_s!(call!(|c| c != '"')) >>
    char!('"') >>
    (text)
));

// TODO: placeable ::= '{' __? (expression | select-expression | variant-list) __? '}'
// TODO: expression           ::= quoted-text | number | identifier | external | attribute-expression | variant-expression | call-expression | placeable
// TODO: select-expression ::= expression __ '->' __ variant-list
named!(attribute_expression <&str, (&str,&str)>, do_parse!(
    identifier1: identifier >>
    char!('.') >>
    identifier2: identifier >>
    (identifier1, identifier2)
));
named!(variant_expression <&str, (&str,&str)>, do_parse!(
    identifier: identifier >>
    char!('[') >>
    opt!(space) >>
    variant_key: variant_key >>
    opt!(space) >>
    char!(']') >>
    (identifier, variant_key)
));

named!(named_argument <&str, (&str,&str)>, do_parse!(
    identifier: identifier >>
    opt!(break_indent) >>
    char!(':') >>
    opt!(break_indent) >>
    value: alt!(quoted_text | number) >>
    (identifier, value)
));

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
fn parse_comment_test() {
    let source = "//This is a comment!
entity1=value1";

    let remaining = "entity1=value1";

    let res = comment(source);
    println!("{:?}", source);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, "This is a comment!"));
}

#[test]
fn parse_section_test() {
    let source = "[[section]]
entity1=value1
entity2 = value2";

    let remaining = "\nentity1=value1
entity2 = value2";

    let res = section(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, "section"));
}

#[test]
fn parse_break_indent_test() {
    let source = "
  Long value";

    let remaining = "Long value";

    let res = break_indent(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ()));
}

#[test]
fn parse_identifier_test() {
    let source = "iden-tifi?er foobar";

    let remaining = " foobar";

    let res = identifier(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, "iden-tifi?er"));
}

#[test]
fn parse_external_test() {
    let source = "$iden-tifi?er foobar";

    let remaining = " foobar";

    let res = external(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, "iden-tifi?er"));
}

#[test]
fn parse_word_test() {
    let source = "foo\\bar";

    let remaining = "\\bar";

    let res = word(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, "foo"));
}

#[test]
fn parse_builtin_test() {
    let source = "NUMBER(foobar)";

    let remaining = "(foobar)";

    let res = builtin(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, "NUMBER"));
}

#[test]
fn parse_number_test() {
    let source = "-0.9 foobar";

    let remaining = " foobar";

    let res = number(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, "-0.9"));
}

#[test]
fn parse_variant_key_1_test() {
    let source = "-0.9
remaining";

    let remaining = "\nremaining";

    let res = variant_key(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, "-0.9"));
}

#[test]
fn parse_variant_key_2_test() {
    let source = "foo bar baz
remaining";

    let remaining = "\nremaining";

    let res = variant_key(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, "foo bar baz"));
}

#[test]
fn parse_variant_symbol_test() {
    let source = "foo bar baz
remaining";

    let remaining = "\nremaining";

    let res = variant_symbol(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, "foo bar baz"));
}

#[test]
fn parse_tag_test() {
    let source = "#foo
bar";

    let remaining = "\nbar";

    let res = tag(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, "foo"));
}

#[test]
fn parse_tag_list_test() {
    let source = "
  #foo
  #bar
baz";

    let remaining = "\nbaz";

    let res = tag_list(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, vec!("foo", "bar")));
}

#[test]
fn parse_quoted_text_test() {
    let source = "\"foo bar\"
baz";

    let remaining = "\nbaz";

    let res = quoted_text(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, "foo bar"));
}

#[test]
fn parse_attribute_expression_test() {
    let source = "foo.bar
baz";

    let remaining = "\nbaz";

    let res = attribute_expression(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ("foo", "bar")));
}

#[test]
fn parse_variant_expression_1_test() {
    let source = "foo[bar]
baz";

    let remaining = "\nbaz";

    let res = variant_expression(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ("foo", "bar")));
}

#[test]
fn parse_variant_expression_2_test() {
    let source = "foo[ bar ]
baz";

    let remaining = "\nbaz";

    let res = variant_expression(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ("foo", "bar")));
}

#[test]
fn parse_named_argument_1_test() {
    let source = "foo:\"bar\"
baz";

    let remaining = "\nbaz";

    let res = named_argument(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ("foo", "bar")));
}

#[test]
fn parse_named_argument_2_test() {
    let source = "foo
  :
  -0.9
baz";

    let remaining = "\nbaz";

    let res = named_argument(source);
    println!("{:?}", res);
    match res {
        IResult::Done(i, o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ("foo", "-0.9")));
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
