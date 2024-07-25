//! A whole bunch of types which are required for working with [`Reflect`],
//! but which would overwhelm the [`crate::reflect`] module.

use core::fmt;
use std::borrow::Cow;

use crate::Reflect;

pub trait Struct {
    fn field(&self, name: &str) -> Option<&dyn Reflect>;
    fn field_mut(&mut self, name: &str) -> Option<&mut dyn Reflect>;
    fn fields(&self) -> Box<dyn Iterator<Item = (&str, &dyn Reflect)> + '_>;
}

pub trait Tuple {
    fn field(&self, index: usize) -> Option<&dyn Reflect>;
    fn field_mut(&mut self, index: usize) -> Option<&mut dyn Reflect>;
    fn fields(&self) -> Box<dyn Iterator<Item = &dyn Reflect> + '_>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FieldsKind {
    Unit,
    Named,
    Unnamed,
}

impl fmt::Display for FieldsKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            FieldsKind::Unit => "unit",
            FieldsKind::Named => "named",
            FieldsKind::Unnamed => "unnamed",
        };

        f.write_str(s)
    }
}

macro_rules! impl_fields_enums {
    ($ty:ty) => {
        impl $ty {
            pub fn kind(&self) -> FieldsKind {
                match self {
                    Self::Unit { .. } => FieldsKind::Unit,
                    Self::Named { .. } => FieldsKind::Named,
                    Self::Unnamed { .. } => FieldsKind::Unnamed,
                }
            }
        }
    };
}

#[derive(Default)]
pub enum FieldsOwned {
    #[default]
    Unit,
    Named(Box<dyn Struct>),
    Unnamed(Box<dyn Tuple>),
}

impl FieldsOwned {
    pub fn as_ref(&self) -> FieldsRef {
        match self {
            FieldsOwned::Unit => FieldsRef::Unit,
            FieldsOwned::Named(n) => FieldsRef::Named(&**n),
            FieldsOwned::Unnamed(u) => FieldsRef::Unnamed(&**u),
        }
    }

    pub fn as_mut(&mut self) -> FieldsMut {
        match self {
            FieldsOwned::Unit => FieldsMut::Unit,
            FieldsOwned::Named(n) => FieldsMut::Named(&mut **n),
            FieldsOwned::Unnamed(u) => FieldsMut::Unnamed(&mut **u),
        }
    }
}

impl_fields_enums!(FieldsOwned);

#[derive(Default)]
pub enum FieldsRef<'a> {
    #[default]
    Unit,
    Named(&'a dyn Struct),
    Unnamed(&'a dyn Tuple),
}

impl_fields_enums!(FieldsRef<'_>);

#[derive(Default)]
pub enum FieldsMut<'a> {
    #[default]
    Unit,
    Named(&'a mut dyn Struct),
    Unnamed(&'a mut dyn Tuple),
}

impl_fields_enums!(FieldsMut<'_>);

pub trait Fields {
    fn fields_ref(&self) -> FieldsRef<'_>;
    fn fields_mut(&mut self) -> FieldsMut<'_>;
}

pub trait Enum: Fields {
    fn variant_id(&self) -> VariantId;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReflectKind {
    Atom,
    Struct,
    Enum,
    // list, array, map, etc. omitted for brevity
}

macro_rules! impl_reflect_enums {
    ($ty:ty) => {
        impl $ty {
            pub fn kind(&self) -> ReflectKind {
                match self {
                    Self::Atom { .. } => ReflectKind::Atom,
                    Self::Struct { .. } => ReflectKind::Struct,
                    Self::Enum { .. } => ReflectKind::Enum,
                }
            }
        }
    };
}

pub enum ReflectRef<'a> {
    Atom,
    Struct(&'a dyn Fields),
    Enum(&'a dyn Enum),
    // list, array, map, etc. omitted for brevity
}

impl_reflect_enums!(ReflectRef<'_>);

pub enum ReflectMut<'a> {
    Atom,
    Struct(&'a mut dyn Fields),
    Enum(&'a mut dyn Enum),
    // list, array, map, etc. omitted for brevity
}

impl_reflect_enums!(ReflectMut<'_>);

pub enum ReflectOwned {
    Atom(Box<dyn Reflect>),
    Struct(Box<dyn Fields>),
    Enum(Box<dyn Enum>),
    // list, array, map, etc. omitted for brevity
}

impl_reflect_enums!(ReflectOwned);

// maybe useful to identify differently?
//
// like with an enum which allows opaque indices for serialization
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantId {
    pub name: Cow<'static, str>,
}

impl From<&'static str> for VariantId {
    fn from(value: &'static str) -> Self {
        Self { name: value.into() }
    }
}

impl fmt::Display for VariantId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.name)
    }
}
