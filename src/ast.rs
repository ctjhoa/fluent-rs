#[derive(Debug, PartialEq)]
pub struct Comment {
    pub content: String,
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct External {
    pub identifier: Identifier,
}

#[derive(Debug, PartialEq)]
pub struct Number {
    pub value: String,
}

#[derive(Debug, PartialEq)]
pub enum VariantKey {
    VariantSymbol(VariantSymbol),
    Number(Number),
}

#[derive(Debug, PartialEq)]
pub struct VariantSymbol {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct Tag {
    pub name: String,
}

pub type Section = VariantSymbol;

#[derive(Debug, PartialEq)]
pub struct QuotedText {
    pub value: String,
}

#[derive(Debug, PartialEq)]
pub enum InlineExpression {
    QuotedText(QuotedText),
    Number(Number),
    Identifier(Identifier),
    External(External),
    AttributeExpression(AttributeExpression),
    VariantExpression(VariantExpression),
    CallExpression(CallExpression),
}

#[derive(Debug, PartialEq)]
pub struct CallExpression {
    pub callee: String,
    pub arguments: Vec<Argument>,
}

#[derive(Debug, PartialEq)]
pub struct AttributeExpression {
    pub identifier: Identifier,
    pub value: Identifier,
}

#[derive(Debug, PartialEq)]
pub struct VariantExpression {
    pub identifier: Identifier,
    pub key: VariantKey,
}

#[derive(Debug, PartialEq)]
pub struct NamedArgument {
    pub identifier: Identifier,
    pub value: NamedArgumentValue,
}

#[derive(Debug, PartialEq)]
pub enum NamedArgumentValue {
    QuotedText(QuotedText),
    Number(Number),
}

#[derive(Debug, PartialEq)]
pub enum Argument {
    InlineExpression(InlineExpression),
    NamedArgument(NamedArgument),
}

#[derive(Debug, PartialEq)]
pub enum Pattern {
    Placeable(InlineExpression),
    Text(String)
}
