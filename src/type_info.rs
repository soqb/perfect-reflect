//! See [`TypeInfo`] and [`TypePath`].

use std::{any::TypeId, borrow::Cow};

use crate::ReflectKind;

/// Extremely light shim of `bevy_reflect::TypeInfo`.
pub struct TypeInfo {
    pub id: TypeId,
}

/// An interface for accessing stable type paths.
pub trait TypePath: 'static {
    /// Returns a static, stable type path.
    fn type_path() -> &'static str
    where
        Self: Sized;

    /// Encapsulates the allocations which *might* be necessary for generating
    /// a type path.
    ///
    /// The default implementation of this method simply wraps [`type_path`] but
    /// if allocation is required, its best to use this method to contain that logic,
    /// and use [`type_path`] to [intern the path] through the [type registry].
    ///
    /// [`type_path`]: Self::type_path
    /// [intern the path]: crate::registry::Registry::register_path
    /// [type registry]: crate::registry::global_registry
    #[inline]
    fn create_type_path() -> Cow<'static, str>
    where
        Self: Sized,
    {
        Cow::Borrowed(Self::type_path())
    }
}

pub trait Typed {
    fn create_type_info() -> TypeInfo;

    fn kind() -> ReflectKind;
}
