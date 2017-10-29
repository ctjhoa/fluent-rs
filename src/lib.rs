#[macro_use]
extern crate nom;
#[macro_use]
extern crate lazy_static;
extern crate regex;


mod ast;

use nom::{IResult, space, alphanumeric, eol};

use std::str;

// TODO: body ::= blank-line* (entry NL blank-line*)* entry? EOF
// TODO: entry ::= comment | section | message

named!(comment<&str, ast::Comment>, do_parse!(
    tag_s!("//") >>
    value: take_while_s!(call!(|c| {
        c != '\r' &&
        c != '\n'
    })) >>
    eol >>
    (ast::Comment{ content: value.to_string() })
));
named!(section<&str, ast::Section>, delimited!(
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
// TODO: blank-line ::= inline-space* line-break

named!(identifier<&str, ast::Identifier>, do_parse!(
    name: re_find_static!(r"^[a-zA-Z_?-][a-zA-Z0-9_?-]*") >>
    (ast::Identifier{ name: name.to_string() })
));

named!(external<&str, ast::External>, do_parse!(
    char!('$') >>
    identifier: identifier >>
    (ast::External{ identifier })
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
named!(number <&str, ast::Number>, do_parse!(
    value: re_find_static!(r"^[-+]?[0-9]*\.?[0-9]+") >>
    (ast::Number{ value: value.to_string() })
));

named!(variant_key <&str, ast::VariantKey>, alt!(do_parse!(
        number: number >>
        (ast::VariantKey::Number(number))
    ) | do_parse!(
        variant_symbol: variant_symbol >>
        (ast::VariantKey::VariantSymbol(variant_symbol))
    )
));
named!(variant_symbol<&str, ast::VariantSymbol>, do_parse!(
    value: re_find_static!(r"^[\w&&[^\{\}\[\]\s\\]]+([\t\x20][\w&&[^\{\}\[\]\s\\]]+)*") >>
    (ast::VariantSymbol{ name: value.to_string() })
));

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

named!(tag <&str, ast::Tag>, do_parse!(
    char!('#') >>
    word: word >>
    (ast::Tag{ name: word.to_string() })
));
named!(tag_list <&str, Vec<ast::Tag> >, do_parse!(
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
named!(quoted_text <&str, ast::QuotedText>, do_parse!(
    char!('"') >>
    text: take_while_s!(call!(|c| c != '"')) >>
    char!('"') >>
    (ast::QuotedText { value: text.to_string() })
));

// TODO: placeable ::= '{' __? (inline-expression | block-expression) __? '}'
named!(placeable <&str, ast::InlineExpression>, do_parse!(
    char!('{') >>
    opt!(space) >>
    expression: inline_expression >>
    opt!(space) >>
    char!('}') >>
    (expression)
));

// TODO: inline-expression           ::= quoted-text | number | identifier | external | attribute-expression | variant-expression | call-expression | placeable
named!(inline_expression <&str, ast::InlineExpression>, alt!(do_parse!(
        quoted_text: quoted_text >>
        (ast::InlineExpression::QuotedText(quoted_text))
    ) | do_parse!(
        number: number >>
        (ast::InlineExpression::Number(number))
    ) | do_parse!(
        identifier: identifier >>
        (ast::InlineExpression::Identifier(identifier))
    ) | do_parse!(
        external: external >>
        (ast::InlineExpression::External(external))
    ) | placeable
));

// TODO: block-expression ::= select-expression | variant-list
// TODO: select-expression ::= inline-expression __ '->' __ variant-list
named!(attribute_expression <&str, ast::AttributeExpression>, do_parse!(
    id: identifier >>
    char!('.') >>
    name: identifier >>
    (ast::AttributeExpression{ identifier: id, value: name })
));
named!(variant_expression <&str, ast::VariantExpression>, do_parse!(
    identifier: identifier >>
    char!('[') >>
    opt!(space) >>
    key: variant_key >>
    opt!(space) >>
    char!(']') >>
    (ast::VariantExpression{ identifier, key })
));

named!(argument <&str, ast::Argument>, alt!(do_parse!(
        inline_expression: inline_expression >>
        (ast::Argument::InlineExpression(inline_expression))
    ) | do_parse!(
        named_argument: named_argument >>
        (ast::Argument::NamedArgument(named_argument))
    )
));
named!(named_argument_value <&str, ast::NamedArgumentValue>, alt!(do_parse!(
        quoted_text: quoted_text >>
        (ast::NamedArgumentValue::QuotedText(quoted_text))
    ) | do_parse!(
        number: number >>
        (ast::NamedArgumentValue::Number(number))
    )
));
named!(named_argument <&str, ast::NamedArgument>, do_parse!(
    identifier: identifier >>
    opt!(break_indent) >>
    char!(':') >>
    opt!(break_indent) >>
    value: named_argument_value >>
    (ast::NamedArgument{ identifier, value })
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
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::Comment{ content: "This is a comment!".to_string() }));
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
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::Section{ name: "section".to_string() }));
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
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::Identifier{ name: "iden-tifi?er".to_string() }));
}

#[test]
fn parse_external_test() {
    let source = "$iden-tifi?er foobar";

    let remaining = " foobar";

    let res = external(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::External{ identifier: ast::Identifier{ name: "iden-tifi?er".to_string() } }));
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
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::Number{ value: "-0.9".to_string() }));
}

#[test]
fn parse_variant_key_1_test() {
    let source = "-0.9
remaining";

    let remaining = "\nremaining";

    let res = variant_key(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::VariantKey::Number(ast::Number{ value: "-0.9".to_string() })));
}

#[test]
fn parse_variant_key_2_test() {
    let source = "foo bar baz
remaining";

    let remaining = "\nremaining";

    let res = variant_key(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::VariantKey::VariantSymbol(ast::VariantSymbol{ name: "foo bar baz".to_string() })));
}

#[test]
fn parse_variant_symbol_test() {
    let source = "foo bar baz
remaining";

    let remaining = "\nremaining";

    let res = variant_symbol(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::VariantSymbol{ name: "foo bar baz".to_string() }));
}

#[test]
fn parse_tag_test() {
    let source = "#foo
bar";

    let remaining = "\nbar";

    let res = tag(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::Tag{ name: "foo".to_string() }));
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

    assert_eq!(res, IResult::Done(remaining, vec!(ast::Tag{ name: "foo".to_string() }, ast::Tag{ name: "bar".to_string() })));
}

#[test]
fn parse_quoted_text_test() {
    let source = "\"foo bar\"
baz";

    let remaining = "\nbaz";

    let res = quoted_text(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::QuotedText{ value: "foo bar".to_string() }));
}

#[test]
fn parse_attribute_expression_test() {
    let source = "foo.bar
baz";

    let remaining = "\nbaz";

    let res = attribute_expression(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::AttributeExpression{
        identifier: ast::Identifier{
            name: "foo".to_string()
        }, value: ast::Identifier{
            name: "bar".to_string()
        }
    }));
}

#[test]
fn parse_variant_expression_1_test() {
    let source = "foo[bar]
baz";

    let remaining = "\nbaz";

    let res = variant_expression(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::VariantExpression{
        identifier: ast::Identifier {
            name: "foo".to_string()
        }, key: ast::VariantKey::VariantSymbol(ast::VariantSymbol{
            name: "bar".to_string()
        })
    }));
}

#[test]
fn parse_variant_expression_2_test() {
    let source = "foo[ bar ]
baz";

    let remaining = "\nbaz";

    let res = variant_expression(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::VariantExpression{
        identifier: ast::Identifier {
            name: "foo".to_string()
        },
        key: ast::VariantKey::VariantSymbol(ast::VariantSymbol{
            name: "bar".to_string()
        })
    }));
}

#[test]
fn parse_named_argument_1_test() {
    let source = "foo:\"bar\"
baz";

    let remaining = "\nbaz";

    let res = named_argument(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::NamedArgument{
        identifier: ast::Identifier{
            name: "foo".to_string()
        }, value: ast::NamedArgumentValue::QuotedText(ast::QuotedText{
            value: "bar".to_string()
        })
    }));
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
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::NamedArgument{
        identifier: ast::Identifier{
            name: "foo".to_string()
        },
        value: ast::NamedArgumentValue::Number(ast::Number{
            value: "-0.9".to_string()
        })
    }));
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
