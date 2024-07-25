use core::fmt;
use std::any::TypeId;

use crate::{
    registry::GetTypeRegistration, FieldsRef, FromValue, ReflectMut, ReflectOwned, ReflectRef,
    TypeInfo, TypePath, Typed,
};
use crate::{DowncastError, Recover, ReflectKind};

use crate::registry::{Registration, Registry};

#[derive(thiserror::Error, Debug, PartialEq, Eq)]
#[error("values of type `{ty}` are not able to be cloned")]
pub struct CloneError {
    pub ty: &'static str,
}

pub trait TryClone {
    fn try_clone(&self) -> Result<Self, CloneError>
    where
        Self: Sized;
    fn try_clone_from(&mut self, rhs: &Self) -> Result<(), CloneError>
    where
        Self: Sized,
    {
        *self = rhs.try_clone()?;
        Ok(())
    }
}

// idk i just think rustfmt style is overly defensive in this case.
#[rustfmt::skip]
/// Encapsulates the various subtraits which reflection relies on.
pub trait Reflectable: TypePath + FromValue + TryClone {
    /// An alias for [`Typed::create_type_info`].
    fn create_type_info() -> TypeInfo where Self: Sized;
    /// An alias for [`Typed::kind`].
    fn kind() -> ReflectKind where Self: Sized;
    /// An alias for [`GetTypeRegistration::type_registration`].
    fn type_registration() -> Registration<Self> where Self: Sized;
    /// An alias for [`GetTypeRegistration::register_type_dependencies`].
    fn register_type_dependencies(registry: &Registry) where Self: Sized;

    /// A dynamic alias for [`TypePath::type_path`].
    fn dyn_type_path(&self) -> &'static str;
}

// kinda strange and verbose but i like it.
impl<T> Reflectable for T
where
    T: TryClone + FromValue + Typed + TypePath + GetTypeRegistration,
{
    fn create_type_info() -> TypeInfo {
        Self::create_type_info()
    }
    fn kind() -> ReflectKind {
        Self::kind()
    }
    fn type_registration() -> Registration<Self> {
        Self::type_registration()
    }
    fn register_type_dependencies(registry: &Registry) {
        Self::register_type_dependencies(registry)
    }

    fn dyn_type_path(&self) -> &'static str {
        Self::type_path()
    }
}

pub trait Reflect: Reflectable + downcast_rs::DowncastSync {
    fn reflect_kind(&self) -> ReflectKind {
        self.reflect_ref().kind()
    }

    fn reflect_ref(&self) -> ReflectRef;
    fn reflect_mut(&mut self) -> ReflectMut;
    fn reflect_owned(self: Box<Self>) -> ReflectOwned;

    fn as_reflect(&self) -> &dyn Reflect;
    fn as_reflect_mut(&mut self) -> &mut dyn Reflect;
}

impl dyn Reflect {
    pub fn is<T: Reflect>(&self) -> bool {
        self.type_id() == TypeId::of::<T>()
    }

    pub fn downcast_ref<T: Reflect>(&self) -> Result<&T, DowncastError> {
        self.as_any().downcast_ref().ok_or_else(|| DowncastError {
            found: self.dyn_type_path().into(),
            expected: T::type_path(),
        })
    }
    pub fn downcast_mut<T: Reflect>(&mut self) -> Result<&mut T, DowncastError> {
        if self.is::<T>() {
            self.as_any_mut()
                .downcast_mut()
                .ok_or_else(|| unreachable!())
        } else {
            Err(DowncastError {
                found: self.dyn_type_path().into(),
                expected: T::type_path(),
            })
        }
    }
    pub fn downcast<T: Reflect>(
        self: Box<dyn Reflect>,
    ) -> Result<Box<T>, Recover<DowncastError, Box<dyn Reflect>>> {
        if self.is::<T>() {
            self.into_any().downcast().or_else(|_| unreachable!())
        } else {
            Err(Recover {
                err: DowncastError {
                    found: self.dyn_type_path().into(),
                    expected: T::type_path(),
                },
                recover: self,
            })
        }
    }

    pub fn take<T: Reflect>(
        self: Box<dyn Reflect>,
    ) -> Result<T, Recover<DowncastError, Box<dyn Reflect>>> {
        self.downcast().map(|boxed| *boxed)
    }
}

impl<'a> fmt::Debug for FieldsRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FieldsRef::Unit => Ok(()),
            FieldsRef::Named(struct_) => {
                let mut s = f.debug_struct("");
                for (name, value) in struct_.fields() {
                    s.field(name, &value);
                }

                s.finish()
            }
            FieldsRef::Unnamed(tuple) => {
                let mut t = f.debug_tuple("");
                for value in tuple.fields() {
                    t.field(&value);
                }

                t.finish()
            }
        }
    }
}

impl fmt::Debug for dyn Reflect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.reflect_ref() {
            ReflectRef::Atom => write!(f, "{}", self.dyn_type_path()),
            ReflectRef::Struct(fields) => {
                write!(
                    f,
                    "{name}{contents:?}",
                    name = self.dyn_type_path(),
                    contents = fields.fields_ref(),
                )
            }
            ReflectRef::Enum(variant) => {
                write!(
                    f,
                    "{name}::{variant_name}{contents:?}",
                    name = self.dyn_type_path(),
                    variant_name = self.dyn_type_path(),
                    contents = variant.fields_ref(),
                )
            }
        }
    }
}
macro_rules! assert_object_safe {
    ($tr:path) => {
        const _: () = {
            use core::marker::PhantomData;
            const ASSERT_OBJECT_SAFE: PhantomData<&dyn $tr> = PhantomData;
        };
    };
}

assert_object_safe!(Reflect);
