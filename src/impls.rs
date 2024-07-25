use std::any::TypeId;

use crate::{
    registry::{GetTypeRegistration, Registration, Registry},
    CloneError, Fields, FieldsKind, FieldsMut, FieldsRef, FromValue, FromValueError, Recover,
    Reflect, ReflectKind, ReflectMut, ReflectOwned, ReflectRef, TryClone, TypeInfo, TypePath,
    Typed, Value,
};

macro_rules! impl_reflect_atom_clone {
    ($($ty:ty as $path:literal;)*) => {
        $(
        impl TryClone for $ty {
            fn try_clone(&self) -> Result<Self, CloneError> {
                Ok(self.clone())
            }
            fn try_clone_from(&mut self, rhs: &Self) -> Result<(), CloneError> {
                Ok(self.clone_from(rhs))
            }
        }

        impl FromValue for $ty {
            fn from_value_ref(value: impl AsRef<Value>) -> Result<Self, FromValueError> {
                value.as_ref().try_downcast_ref().cloned()
            }

            fn from_value(value: impl Into<Value>) -> Result<Self, Recover<FromValueError, Value>> {
                value.into().try_take()
            }
        }

        impl Typed for $ty {
            fn create_type_info() -> TypeInfo {
                TypeInfo {
                    id: TypeId::of::<Self>(),
                }
            }

            fn kind() -> ReflectKind {
                ReflectKind::Atom
            }
        }

        impl TypePath for $ty {
            fn type_path() -> &'static str {
                $path
            }
        }

        impl GetTypeRegistration for $ty {
            fn type_registration() -> Registration<Self> {
                Registration::default()
            }

            fn register_type_dependencies(_: &Registry) {}
        }

        impl Reflect for $ty {
            fn reflect_ref(&self) -> ReflectRef {
                ReflectRef::Atom
            }

            fn reflect_mut(&mut self) -> ReflectMut {
                ReflectMut::Atom
            }

            fn reflect_owned(self: Box<Self>) -> ReflectOwned {
                ReflectOwned::Atom(self)
            }

            fn as_reflect(&self) -> &dyn Reflect {
                self
            }

            fn as_reflect_mut(&mut self) -> &mut dyn Reflect {
                self
            }
        }
        )*
    };
}

impl_reflect_atom_clone! {
    String as "::alloc::string::String";
    bool as "bool";
    f32 as "f32";
    f64 as "f64";
    char as "char";
    u8 as "u8";
    u16 as "u16";
    u32 as "u32";
    u64 as "u64";
    u128 as "u128";
    usize as "usize";
    i8 as "i8";
    i16 as "i16";
    i32 as "i32";
    i64 as "i64";
    i128 as "i128";
    isize as "isize";
}

impl TryClone for () {
    fn try_clone(&self) -> Result<Self, CloneError> {
        Ok(())
    }
    fn try_clone_from(&mut self, _rhs: &Self) -> Result<(), CloneError> {
        Ok(())
    }
}

impl FromValue for () {
    fn from_value_ref(value: impl AsRef<Value>) -> Result<(), FromValueError> {
        let value = value.as_ref();
        value
            .try_downcast_ref()
            .map(|&()| ())
            .or_else(|err| match value {
                Value::Struct(fields) if fields.is_empty() => Ok(()),
                Value::Struct(fields) => Err(FromValueError::FieldsMismatch {
                    expected: FieldsKind::Unit,
                    found: fields.kind(),
                }),
                _ => Err(err),
            })
    }

    fn from_value(value: impl Into<Value>) -> Result<(), Recover<FromValueError, Value>> {
        value.into().try_take().or_else(|rec| match rec.recover {
            Value::Struct(fields) if fields.is_empty() => Ok(()),
            Value::Struct(fields) => Err(Recover {
                err: FromValueError::FieldsMismatch {
                    expected: FieldsKind::Unit,
                    found: fields.kind(),
                },
                recover: Value::Struct(fields),
            }),
            _ => Err(rec),
        })
    }
}

impl TypePath for () {
    fn type_path() -> &'static str {
        "()"
    }
}

impl Typed for () {
    fn create_type_info() -> TypeInfo {
        TypeInfo {
            id: TypeId::of::<Self>(),
        }
    }

    fn kind() -> ReflectKind {
        ReflectKind::Struct
    }
}

impl GetTypeRegistration for () {
    fn type_registration() -> Registration<Self> {
        Registration::default()
    }

    fn register_type_dependencies(_: &Registry) {}
}

impl Fields for () {
    fn fields_ref(&self) -> FieldsRef<'_> {
        FieldsRef::Unit
    }

    fn fields_mut(&mut self) -> FieldsMut<'_> {
        FieldsMut::Unit
    }
}

impl Reflect for () {
    fn reflect_ref(&self) -> ReflectRef {
        ReflectRef::Struct(self)
    }

    fn reflect_mut(&mut self) -> ReflectMut {
        ReflectMut::Struct(self)
    }

    fn reflect_owned(self: Box<Self>) -> ReflectOwned {
        ReflectOwned::Struct(self)
    }

    fn as_reflect(&self) -> &dyn Reflect {
        self
    }

    fn as_reflect_mut(&mut self) -> &mut dyn Reflect {
        self
    }
}
