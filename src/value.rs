use core::fmt;
use std::borrow::Cow;

use crate::{CloneError, FieldsKind, Reflect, ReflectKind, VariantId};

pub struct EntryValue {
    pub key: Value,
    pub value: Value,
}

pub enum FieldValues {
    Unit,
    Unnamed(Vec<Value>),
    Named(Vec<(Cow<'static, str>, Value)>),
}

impl FieldValues {
    pub fn is_empty(&self) -> bool {
        match self {
            FieldValues::Unit => true,
            FieldValues::Unnamed(v) if v.len() == 0 => true,
            FieldValues::Named(v) if v.len() == 0 => true,
            _ => false,
        }
    }

    pub fn kind(&self) -> FieldsKind {
        match self {
            FieldValues::Unit => FieldsKind::Unit,
            FieldValues::Unnamed(_) => FieldsKind::Unnamed,
            FieldValues::Named(_) => FieldsKind::Named,
        }
    }

    pub fn try_into_unit(self) -> Result<(), Recover<FromValueError, Self>> {
        match self {
            FieldValues::Unit => Ok(()),
            _ => Err(Recover {
                err: FromValueError::FieldsMismatch {
                    expected: FieldsKind::Unit,
                    found: self.kind(),
                },
                recover: self,
            }),
        }
    }

    pub fn try_into_unnamed(self) -> Result<Vec<Value>, Recover<FromValueError, Self>> {
        match self {
            FieldValues::Unnamed(fields) => Ok(fields),
            _ => Err(Recover {
                err: FromValueError::FieldsMismatch {
                    expected: FieldsKind::Unnamed,
                    found: self.kind(),
                },
                recover: self,
            }),
        }
    }

    pub fn try_into_named(
        self,
    ) -> Result<Vec<(Cow<'static, str>, Value)>, Recover<FromValueError, Self>> {
        match self {
            FieldValues::Named(fields) => Ok(fields),
            _ => Err(Recover {
                err: FromValueError::FieldsMismatch {
                    expected: FieldsKind::Named,
                    found: self.kind(),
                },
                recover: self,
            }),
        }
    }

    pub fn try_as_unit(&self) -> Result<&(), FromValueError> {
        match self {
            FieldValues::Unit => Ok(&()),
            _ => Err(FromValueError::FieldsMismatch {
                expected: FieldsKind::Unit,
                found: self.kind(),
            }),
        }
    }

    pub fn try_as_unnamed(&self) -> Result<&[Value], FromValueError> {
        match self {
            FieldValues::Unnamed(fields) => Ok(fields),
            _ => Err(FromValueError::FieldsMismatch {
                expected: FieldsKind::Unnamed,
                found: self.kind(),
            }),
        }
    }

    pub fn try_as_named(&self) -> Result<&[(Cow<'static, str>, Value)], FromValueError> {
        match self {
            FieldValues::Named(fields) => Ok(fields),
            _ => Err(FromValueError::FieldsMismatch {
                expected: FieldsKind::Named,
                found: self.kind(),
            }),
        }
    }
}

pub struct VariantValue {
    pub id: VariantId,
    pub fields: FieldValues,
}

// questions.
//
// 1. perhaps include common primitives in variants somehow?
// boxing definitely seems inefficient for them
// so maybe just store inline (with vtable ptr) up to some (small) size limit.
//
// 2. maybe some serde_json-like drop magic for these vecs..?
//    maybe (probably) smallvec or something, often its only 1 or 2 fields.
//
// 3. maybe use lifetimes and more clone-on-write, this is a *very* allocation heavy object...
//
// this data model doesn't match with `ReflectRef`.
pub enum Value {
    Dyn(Box<dyn Reflect>),
    Seq(Vec<Value>),
    Struct(FieldValues),
    Enum(VariantValue),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ValueKind {
    Dyn,
    Seq,
    Struct,
    Enum,
}

impl Value {
    pub fn from_reflect<T: Reflect>(t: T) -> Self {
        Self::Dyn(Box::new(t))
    }

    pub fn kind(&self) -> ValueKind {
        match self {
            Value::Dyn(_) => ValueKind::Dyn,
            Value::Seq(_) => ValueKind::Seq,
            Value::Struct(_) => ValueKind::Struct,
            Value::Enum(_) => ValueKind::Enum,
        }
    }

    pub fn try_downcast_ref<T: Reflect>(&self) -> Result<&T, FromValueError> {
        match self {
            Value::Dyn(dy) => dy.downcast_ref().map_err(From::from),
            _ => Err(FromValueError::KindMismatch {
                found: self.kind(),
                expected: T::kind(),
            }),
        }
    }

    pub fn try_downcast_mut<T: Reflect>(&mut self) -> Result<&mut T, FromValueError> {
        match self {
            Value::Dyn(dy) => dy.downcast_mut().map_err(From::from),
            _ => Err(FromValueError::KindMismatch {
                found: self.kind(),
                expected: T::kind(),
            }),
        }
    }

    pub fn try_downcast<T: Reflect>(self) -> Result<Box<T>, Recover<FromValueError, Value>> {
        match self {
            Value::Dyn(dy) => dy.downcast().map_err(|rec| Recover {
                err: rec.err.into(),
                recover: Value::Dyn(rec.recover),
            }),
            _ => Err(Recover {
                err: FromValueError::KindMismatch {
                    found: self.kind(),
                    expected: T::kind(),
                },
                recover: self,
            }),
        }
    }

    pub fn try_take<T: Reflect>(self) -> Result<T, Recover<FromValueError, Value>> {
        self.try_downcast().map(|boxed| *boxed)
    }
}

impl AsRef<Value> for Value {
    fn as_ref(&self) -> &Value {
        self
    }
}

impl Into<Value> for Box<dyn Reflect> {
    fn into(self) -> Value {
        Value::Dyn(self)
    }
}

impl Into<Value> for FieldValues {
    fn into(self) -> Value {
        Value::Struct(self)
    }
}

impl Into<Value> for VariantValue {
    fn into(self) -> Value {
        Value::Enum(self)
    }
}

pub trait FromValue {
    // dynamic -> concrete conversion via clones.
    //
    // also slightly more brittle since some internal type might not be clonable.
    fn from_value_ref(value: impl AsRef<Value>) -> Result<Self, FromValueError>
    where
        Self: Sized;

    // cloneless (on the happy path) dynamic -> concrete conversion
    fn from_value(value: impl Into<Value>) -> Result<Self, Recover<FromValueError, Value>>
    where
        Self: Sized;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FieldName {
    Index(usize),
    Name(Cow<'static, str>),
}

impl fmt::Display for FieldName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FieldName::Index(n) => write!(f, "field with index {n}"),
            FieldName::Name(name) => write!(f, "field with name {name}"),
        }
    }
}

#[derive(thiserror::Error, Debug, PartialEq, Eq)]
#[error("expected a value of type `{expected}`, but found type `{found}`")]
pub struct DowncastError {
    pub found: Cow<'static, str>,
    pub expected: &'static str,
}

#[derive(thiserror::Error, Debug, PartialEq, Eq)]
pub enum FromValueError {
    #[error(transparent)]
    Downcast(#[from] DowncastError),
    #[error(transparent)]
    Clone(#[from] CloneError),
    #[error("expected a value with the kind {expected:?}, but found the kind {found:?}")]
    KindMismatch {
        found: ValueKind,
        expected: ReflectKind,
    },
    #[error(
        "expected a variant in the set {variants}, but found the variant `{found}`",
        variants = PrintVariants(expected),
    )]
    VariantMismatch {
        found: VariantId,
        expected: Vec<VariantId>,
    },
    #[error("expected {expected} fields, but found {found} fields")]
    FieldsMismatch {
        expected: FieldsKind,
        found: FieldsKind,
    },
    #[error("expected a {name}, but it was not present")]
    MissingField { name: FieldName },
    #[error(
        "field {p}: {inner}",
        p = PrintFieldPath(path),
    )]
    Nested {
        path: Vec<PathSegment>,
        inner: Box<FromValueError>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum PathSegment {
    Variant(VariantId),
    TupleIndex(usize),
    StructKey(&'static str),
}

impl From<VariantId> for PathSegment {
    fn from(value: VariantId) -> Self {
        Self::Variant(value)
    }
}
impl From<usize> for PathSegment {
    fn from(value: usize) -> Self {
        Self::TupleIndex(value)
    }
}
impl From<&'static str> for PathSegment {
    fn from(value: &'static str) -> Self {
        Self::StructKey(value)
    }
}

impl FromValueError {
    pub fn nested_in(self, key: impl Into<PathSegment>) -> Self {
        match self {
            FromValueError::Nested { mut path, inner } => {
                path.push(key.into());
                FromValueError::Nested { path, inner }
            }
            inner => {
                let path = vec![key.into()];
                FromValueError::Nested {
                    path,
                    inner: Box::new(inner),
                }
            }
        }
    }
}

struct PrintVariants<'a>(&'a [VariantId]);

impl fmt::Display for PrintVariants<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;

        let Some((last, rest)) = self.0.split_last() else {
            return write!(f, ")");
        };

        for el in rest {
            write!(f, "{el}, ")?;
        }

        write!(f, "{last})")
    }
}

struct PrintFieldPath<'a>(&'a Vec<PathSegment>);

impl fmt::Display for PathSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathSegment::Variant(id) => write!(f, "{id}"),
            PathSegment::TupleIndex(n) => write!(f, "{n}"),
            PathSegment::StructKey(key) => f.write_str(key),
        }
    }
}
impl fmt::Display for PrintFieldPath<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Some((first, rest)) = self.0.split_first() else {
            return f.write_str("self");
        };

        for el in rest.iter().rev() {
            write!(f, "{el}.")?;
        }

        write!(f, "{first}")
    }
}

#[derive(thiserror::Error, Debug)]
#[error("{err}")]
pub struct Recover<E, T> {
    pub err: E,
    pub recover: T,
}

impl<E, T> Recover<E, T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Recover<E, U> {
        Recover {
            err: self.err,
            recover: f(self.recover),
        }
    }
    pub fn map_err<F>(self, e: impl FnOnce(E) -> F) -> Recover<F, T> {
        Recover {
            err: e(self.err),
            recover: self.recover,
        }
    }

    pub fn map_both<U, F>(self, e: impl FnOnce(E) -> F, f: impl FnOnce(T) -> U) -> Recover<F, U> {
        Recover {
            err: e(self.err),
            recover: f(self.recover),
        }
    }
}
