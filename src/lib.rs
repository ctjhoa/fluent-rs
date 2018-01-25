#[macro_use]
extern crate nom;
#[macro_use]
extern crate lazy_static;
extern crate regex;


mod ast;

use nom::{IResult, space, alphanumeric, eol};

use std::str;
use std::iter::FromIterator;

// TODO: body ::= blank-line* (entry NL blank-line*)* entry? EOF
// TODO: entry ::= comment | section | message

// FIXME: comment ::= ('//' (char - NL)* NL)+
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

named!(blank_line<&str, ()>, do_parse!(
    space >>
    eol >>
    ()
));

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
// TODO: TEST
named!(variant<&str, ast::Variant>, do_parse!(
    eol >>
    break_indent >>
    char!('[') >>
    opt!(space) >>
    variant_key: variant_key >>
    opt!(space) >>
    char!(']') >>
    break_indent >>
    pattern: pattern >>
    (ast::Variant{ key: variant_key, pattern: pattern })
));
// TODO: default-variant ::= NL __ '*[' _? variant-key _? ']' __ pattern
// TODO: TEST
named!(default_variant<&str, ast::Variant>, do_parse!(
    eol >>
    break_indent >>
    tag!("*[") >>
    opt!(space) >>
    variant_key: variant_key >>
    opt!(space) >>
    char!(']') >>
    break_indent >>
    pattern: pattern >>
    (ast::Variant{ key: variant_key, pattern: pattern })
));

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

named!(pattern <&str, ast::Pattern>, alt!(
    do_parse!(
        text: text >>
        (ast::Pattern::Text(text))
    ) | do_parse!(
        placeable: placeable >>
        (ast::Pattern::Placeable(placeable))
    )
));

named!(text_char <&str, char>, alt_complete!(
    do_parse!(
        char!('\\') >>
        char!('\u{005C}') >>
        ('\u{005C}')
    ) | do_parse!(
        char!('\\') >>
        char!('\u{007B}') >>
        ('\u{007B}')
    ) | do_parse!(
        char!('\\') >>
        char!('u') >>
        hex: take_s!(4) >>
        (u8::from_str_radix(hex, 16).map(|n| n as char).unwrap())
    ) | do_parse!(
        break_indent >>
        (' ')
    ) | none_of!("\u{000A}\u{000D}\u{005C}\u{007B}")
));

named!(text <&str, String>, do_parse!(
    chars: many1!(text_char) >>
    (String::from_iter(chars))
));

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

named!(inline_expression <&str, ast::InlineExpression>, alt!(do_parse!(
        quoted_text: quoted_text >>
        (ast::InlineExpression::QuotedText(quoted_text))
    ) | do_parse!(
        number: number >>
        (ast::InlineExpression::Number(number))
    ) | do_parse!(
        attribute_expression: attribute_expression >>
        (ast::InlineExpression::AttributeExpression(attribute_expression))
    ) | do_parse!(
        external: external >>
        (ast::InlineExpression::External(external))
    ) | do_parse!(
        variant_expression: variant_expression >>
        (ast::InlineExpression::VariantExpression(variant_expression))
    ) | do_parse!(
        call_expression: call_expression >>
        (ast::InlineExpression::CallExpression(call_expression))
    ) | do_parse!(
        identifier: identifier >>
        (ast::InlineExpression::Identifier(identifier))
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
named!(call_expression <&str, ast::CallExpression>, do_parse!(
    builtin: builtin >>
    char!('(') >>
    opt!(space) >>
    arguments: separated_nonempty_list_complete!(
        ws!(tag!(",")),
        argument
    ) >>
    opt!(space) >>
    char!(')') >>
    (ast::CallExpression{ callee: builtin.to_string(), arguments })
));

named!(argument <&str, ast::Argument>, alt!(do_parse!(
        named_argument: named_argument >>
        (ast::Argument::NamedArgument(named_argument))
    ) | do_parse!(
        inline_expression: inline_expression >>
        (ast::Argument::InlineExpression(inline_expression))
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
fn parse_blank_line_test() {
    let source = "\u{0020}\u{0020}
Long value";

    let remaining = "Long value";

    let res = blank_line(source);
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
fn parse_pattern_1_test() {
    let source = "foo
bar";

    let remaining = "\nbar";

    let res = pattern(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::Pattern::Text("foo".to_string())));
}

#[test]
fn parse_pattern_2_test() {
    let source = "\\\u{005C}bc
bar";

    let remaining = "\nbar";

    let res = pattern(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::Pattern::Text("\u{005C}bc".to_string())));
}

#[test]
fn parse_pattern_3_test() {
    let source = "{\"foo bar\"}
baz";

    let remaining = "\nbaz";

    let res = pattern(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::Pattern::Placeable(
        ast::InlineExpression::QuotedText(ast::QuotedText{
            value: "foo bar".to_string()
        })
    )));
}

#[test]
fn parse_text_char_1_test() {
    let source = "\\\u{005C}bc";

    let remaining = "bc";

    let res = text_char(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, '\u{005C}'));
}

#[test]
fn parse_text_char_2_test() {
    let source = "\\\u{007B}bc";

    let remaining = "bc";

    let res = text_char(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, '\u{007B}'));
}

#[test]
fn parse_text_char_3_test() {
    let source = "\\u007Bbc";

    let remaining = "bc";

    let res = text_char(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, '\u{007B}'));
}

#[test]
fn parse_text_char_4_test() {
    let source = "
  bc";

    let remaining = "bc";

    let res = text_char(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ' '));
}

#[test]
fn parse_text_char_5_test() {
    let source = "abc";

    let remaining = "bc";

    let res = text_char(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, 'a'));
}

#[test]
fn parse_text_test() {
    let source = "foo
bar";

    let remaining = "\nbar";

    let res = text(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, "foo".to_string()));
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
fn parse_placeable_test() {
    let source = "{\"foo bar\"}
baz";

    let remaining = "\nbaz";

    let res = placeable(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::InlineExpression::QuotedText(ast::QuotedText{
        value: "foo bar".to_string()
    })));
}

#[test]
fn parse_inline_expression_1_test() {
    let source = "\"foo bar\"
baz";

    let remaining = "\nbaz";

    let res = inline_expression(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::InlineExpression::QuotedText(ast::QuotedText{
        value: "foo bar".to_string()
    })));
}

#[test]
fn parse_inline_expression_2_test() {
    let source = "1
baz";

    let remaining = "\nbaz";

    let res = inline_expression(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::InlineExpression::Number(ast::Number{
        value: "1".to_string()
    })));
}

#[test]
fn parse_inline_expression_3_test() {
    let source = "foo
baz";

    let remaining = "\nbaz";

    let res = inline_expression(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::InlineExpression::Identifier(ast::Identifier{
        name: "foo".to_string()
    })));
}

#[test]
fn parse_inline_expression_4_test() {
    let source = "$foo
baz";

    let remaining = "\nbaz";

    let res = inline_expression(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::InlineExpression::External(ast::External{
        identifier: ast::Identifier{
            name: "foo".to_string()
        }
    })));
}

#[test]
fn parse_inline_expression_5_test() {
    let source = "foo.bar
baz";

    let remaining = "\nbaz";

    let res = inline_expression(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::InlineExpression::AttributeExpression(ast::AttributeExpression{
        identifier: ast::Identifier{
            name: "foo".to_string()
        },
        value: ast::Identifier{
            name: "bar".to_string()
        }
    })));
}

#[test]
fn parse_inline_expression_6_test() {
    let source = "foo[1]
baz";

    let remaining = "\nbaz";

    let res = inline_expression(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::InlineExpression::VariantExpression(ast::VariantExpression{
        identifier: ast::Identifier{
            name: "foo".to_string()
        },
        key: ast::VariantKey::Number(ast::Number{
            value: "1".to_string()
        })
    })));
}

#[test]
fn parse_inline_expression_7_test() {
    let source = "NUMBER($ratio, minimumFractionDigits: 2)
baz";

    let remaining = "\nbaz";

    let res = inline_expression(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::InlineExpression::CallExpression(
        ast::CallExpression{
            callee: "NUMBER".to_string(),
            arguments: vec![
                ast::Argument::InlineExpression(
                    ast::InlineExpression::External(
                        ast::External{
                            identifier: ast::Identifier{
                                name: "ratio".to_string()
                            }
                        }
                    )
                ),
                ast::Argument::NamedArgument(
                    ast::NamedArgument{
                        identifier: ast::Identifier{
                            name: "minimumFractionDigits".to_string()
                        },
                        value: ast::NamedArgumentValue::Number(
                            ast::Number{
                                value: "2".to_string()
                            }
                        )
                    }
                )
            ]
        }
    )));
}

#[test]
fn parse_inline_expression_8_test() {
    let source = "{foo[1]}
baz";

    let remaining = "\nbaz";

    let res = inline_expression(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::InlineExpression::VariantExpression(ast::VariantExpression{
        identifier: ast::Identifier{
            name: "foo".to_string()
        },
        key: ast::VariantKey::Number(ast::Number{
            value: "1".to_string()
        })
    })));
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
fn parse_argument_1_test() {
    let source = "foo:\"bar\"
baz";

    let remaining = "\nbaz";

    let res = argument(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::Argument::NamedArgument(
        ast::NamedArgument {
            identifier: ast::Identifier{
                name: "foo".to_string()
            }, value: ast::NamedArgumentValue::QuotedText(ast::QuotedText{
                value: "bar".to_string()
            })
        }
    )));
}

#[test]
fn parse_argument_2_test() {
    let source = "\"bar\"
baz";

    let remaining = "\nbaz";

    let res = argument(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::Argument::InlineExpression(
        ast::InlineExpression::QuotedText(
            ast::QuotedText{
                value: "bar".to_string()
            }
        )
    )));
}

#[test]
fn parse_argument_3_test() {
    let source = "-0.9
baz";

    let remaining = "\nbaz";

    let res = argument(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::Argument::InlineExpression(
        ast::InlineExpression::Number(
            ast::Number{
                value: "-0.9".to_string()
            }
        )
    )));
}

#[test]
fn parse_argument_4_test() {
    let source = "iden-tifi?er
baz";

    let remaining = "\nbaz";

    let res = argument(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::Argument::InlineExpression(
        ast::InlineExpression::Identifier(
            ast::Identifier{
                name: "iden-tifi?er".to_string()
            }
        )
    )));
}

#[test]
fn parse_argument_5_test() {
    let source = "$iden-tifi?er
baz";

    let remaining = "\nbaz";

    let res = argument(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::Argument::InlineExpression(
        ast::InlineExpression::External(
            ast::External{
                identifier: ast::Identifier{
                    name: "iden-tifi?er".to_string()
                }
            }
        )
    )));
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
fn parse_call_expression_test() {
    let source = "NUMBER($ratio, minimumFractionDigits: 2)
baz";

    let remaining = "\nbaz";

    let res = call_expression(source);
    println!("{:?}", res);
    match res {
        IResult::Done(ref i, ref o) => println!("i: {} | o: {:?}", i, o),
        _ => println!("error")
    }

    assert_eq!(res, IResult::Done(remaining, ast::CallExpression{
        callee: "NUMBER".to_string(),
        arguments: vec![
            ast::Argument::InlineExpression(
                ast::InlineExpression::External(
                    ast::External{
                        identifier: ast::Identifier{
                            name: "ratio".to_string()
                        }
                    }
                )
            ),
            ast::Argument::NamedArgument(
                ast::NamedArgument{
                    identifier: ast::Identifier{
                        name: "minimumFractionDigits".to_string()
                    },
                    value: ast::NamedArgumentValue::Number(
                        ast::Number{
                            value: "2".to_string()
                        }
                    )
                }
            )
        ]
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
