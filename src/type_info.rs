//! Very light shim of `bevy_reflect::TypeInfo`.

use std::any::TypeId;

use crate::ReflectKind;

pub struct TypeInfo {
    pub path: &'static str,
    pub id: TypeId,
}

pub trait TypePath {
    fn type_path() -> &'static str
    where
        Self: Sized;
}

pub trait Typed {
    fn create_type_info() -> TypeInfo;

    fn kind() -> ReflectKind;
}
