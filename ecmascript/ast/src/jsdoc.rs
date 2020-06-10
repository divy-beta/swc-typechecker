use crate::Str;
use serde::{Deserialize, Serialize};
use swc_atoms::JsWord;
#[cfg(feature = "fold")]
use swc_common::Fold;
use swc_common::{ast_node, Span};

#[derive(Debug, Clone, PartialEq)]
pub struct JsDoc {
    pub span: Span,

    pub description: Str,
    pub tags: Vec<JsDocTag>,
}

#[ast_node]
pub struct JsDocTag {
    pub span: Span,
    pub kind: JsDocTagKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[cfg_attr(feature = "fold", derive(Fold))]
pub enum JsDocTagKind {
    Unknown(JsWord),
    Augments,
    Implements,
    Author,
    Class,
    Public,
    Private,
    Protected,
    Readonly,
    Callback,
    Enum,
    Parameter,
    Return,
    This,
    Type,
    Template,
    Typedef,
    Property,
}

#[ast_node]
pub struct JsDocType {
    pub span: Span,
    pub kind: JsDocTypeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[cfg_attr(feature = "fold", derive(Fold))]
pub enum JsDocTypeKind {
    /// `*`
    All,
    /// `?`
    Unknown,
    Nullable,
    NonNullable,
    Optional,
    Function,
    Variadic,
    // https://jsdoc.app/about-namepaths.html
    NamePath(JsWord),
}
