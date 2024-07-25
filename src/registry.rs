use std::{
    any::{Any, TypeId},
    borrow::Cow,
    marker::PhantomData,
    ptr::NonNull,
    sync::OnceLock,
};

use crate::{Reflect, TypeInfo, TypePath};

pub trait TypeData: Sized + Sync + 'static {}

pub trait RegisterData<T>: TypeData
where
    T: Reflect,
{
    fn make() -> Self;
}

mod registrations {
    use bevy_ptr::PtrMut;

    use super::*;

    pub struct ReflectDefault {
        default: fn() -> Box<dyn Reflect>,
    }

    impl ReflectDefault {
        pub fn default(&self) -> Box<dyn Reflect> {
            (self.default)()
        }
    }

    impl TypeData for ReflectDefault {}
    impl<T: Reflect + Default> RegisterData<T> for ReflectDefault {
        fn make() -> Self {
            Self {
                default: || Box::new(T::default()),
            }
        }
    }

    pub struct ReflectFromPtr {
        from_ptr: unsafe fn(*const u8) -> *const dyn Reflect,
    }

    impl ReflectFromPtr {
        /// Convert `Ptr` into `&dyn Reflect`.
        ///
        /// # Safety
        ///
        /// `val` must be a pointer to a value of the type that the [`ReflectFromPtr`] was constructed for.
        pub unsafe fn from_ptr<'a>(&self, ptr: Ptr<'a>) -> &'a dyn Reflect {
            // SAFETY: The caller contract guarantees `ptr` is typed correctly.
            // And thus, the contract of `Ptr` ensures the `ptr` is valid.
            unsafe { &*(self.from_ptr)(ptr.as_ptr()) }
        }
        /// Convert `PtrMut` into `&mut dyn Reflect`.
        ///
        /// # Safety
        ///
        /// `val` must be a pointer to a value of the type that the [`ReflectFromPtr`] was constructed for
        pub unsafe fn from_ptr_mut<'a>(&self, ptr: PtrMut<'a>) -> &'a mut dyn Reflect {
            // SAFETY: The caller contract guarantees `ptr` is typed correctly.
            // And thus, the contract of `PtrMut` ensures the `ptr` is valid.
            unsafe { &mut *(self.from_ptr)(ptr.as_ptr()).cast_mut() }
        }
    }

    impl TypeData for ReflectFromPtr {}
    impl<T: Reflect> RegisterData<T> for ReflectFromPtr {
        fn make() -> Self {
            Self {
                from_ptr: |ptr| ptr.cast::<T>() as *const dyn Reflect,
            }
        }
    }
}

pub use registrations::{ReflectDefault, ReflectFromPtr};

pub struct Registration<T> {
    info: TypeInfo,
    data: Vec<Box<dyn Any>>,
    _marker: PhantomData<fn(T) -> T>,
}

impl<T: Reflect> Default for Registration<T> {
    fn default() -> Self {
        Self {
            info: T::create_type_info(),
            data: Vec::new(),
            _marker: PhantomData,
        }
    }
}

impl<T: Reflect> Registration<T> {
    pub fn data<D>(&mut self) -> &mut Self
    where
        D: RegisterData<T>,
    {
        let data = Box::new(D::make());
        self.data.push(data);
        self
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
struct DataKey {
    data: TypeId,
    type_: TypeId,
}

mod query {
    use super::*;

    pub struct RegistryEntry {
        pub path: &'static str,
        pub info: TypeInfo,
    }

    #[derive(Clone, Copy)]
    pub enum EntryStatus {
        OnlyPath(&'static str),
        Registered(usize),
        Missing,
    }

    pub trait TypeKey: Copy {
        fn status(key: Self, registry: &Registry) -> EntryStatus;
        fn get(key: Self, registry: &Registry) -> Option<&RegistryEntry>;
        #[inline]
        fn to_id(key: Self, registry: &Registry) -> Option<TypeId> {
            Self::get(key, registry).map(|entry| entry.info.id)
        }
    }

    impl<'a> TypeKey for &'a str {
        #[inline]
        fn status(key: Self, registry: &Registry) -> EntryStatus {
            registry
                .by_path
                .pin()
                .get(key)
                .map_or(EntryStatus::Missing, |&idx| EntryStatus::Registered(idx))
        }
        #[inline]
        fn get(key: Self, registry: &Registry) -> Option<&RegistryEntry> {
            registry
                .by_path
                .pin()
                .get(key)
                .and_then(|&idx| registry.registrations.get(idx))
        }
    }

    impl TypeKey for TypeId {
        #[inline]
        fn status(key: TypeId, registry: &Registry) -> EntryStatus {
            registry
                .by_id
                .pin()
                .get(&key)
                .map_or(EntryStatus::Missing, |idx| match idx {
                    ListIdx::Path(path) => EntryStatus::OnlyPath(path),
                    ListIdx::Idx(idx) => EntryStatus::Registered(*idx),
                })
        }
        #[inline]
        fn get(ref key: TypeId, registry: &Registry) -> Option<&RegistryEntry> {
            registry
                .by_id
                .pin()
                .get(key)
                .and_then(|idx| match idx {
                    ListIdx::Path(_) => None,
                    ListIdx::Idx(idx) => Some(*idx),
                })
                .and_then(|idx| registry.registrations.get(idx))
        }

        #[inline]
        fn to_id(key: TypeId, _registry: &Registry) -> Option<TypeId> {
            Some(key)
        }
    }
}

use bevy_ptr::Ptr;
pub use query::TypeKey;
use query::{EntryStatus, RegistryEntry};

#[derive(Clone, Copy)]
enum ListIdx {
    Path(&'static str),
    Idx(usize),
}

#[derive(Default)]
pub struct Registry {
    registrations: boxcar::Vec<RegistryEntry>,
    data_db: papaya::HashMap<DataKey, Ptr<'static>>,
    by_id: papaya::HashMap<TypeId, ListIdx, bevy_utils::NoOpHash>,
    by_path: papaya::HashMap<&'static str, usize>,
}

// SAFETY: both of the following impls are safe considering the raw pointers
// exclusively point to values of types implementing `TypeData` which implies `Sync`.
unsafe impl Send for Registry {}
unsafe impl Sync for Registry {}

// work around restriction in `bevy_ptr` which could be lifted.
fn unsized_ref_to_ptr<T: ?Sized>(r: &T) -> Ptr<'_> {
    // SAFETY: The returned pointer has the same lifetime as the passed reference.
    // Access is immutable.
    unsafe { Ptr::new(NonNull::from(r).cast()) }
}

impl Registry {
    /// Interns a [type path](`TypePath`) into the registry.
    ///
    /// For a full registration of a type, see [`Registry::register`].
    ///
    /// Note that after this method has been called,
    /// a call to [`Registry::register`] will still update and complete the registry entry.
    pub fn register_path<T: TypePath>(&self) -> &'static str {
        // if `T` has already been registered, return the path.
        match self.entry_status(TypeId::of::<T>()) {
            EntryStatus::OnlyPath(path) => return path,
            EntryStatus::Registered(idx) => return self.registrations[idx].path,
            EntryStatus::Missing => (),
        }

        let path: &'static str = match T::create_type_path() {
            Cow::Borrowed(path) => path,
            Cow::Owned(path) => Box::leak(path.into_boxed_str()),
        };

        let idx = ListIdx::Path(path);
        self.by_id.pin().insert(TypeId::of::<T>(), idx);

        path
    }

    fn default_registrations<T: Reflect>() -> impl Iterator<Item = Box<dyn Any>> {
        macro_rules! make {
            ($ty:ty) => {
                Box::new(<$ty as RegisterData<T>>::make()) as Box<dyn Any>
            };
        }

        [make!(ReflectFromPtr)].into_iter()
    }

    /// Register a single type into the registry.
    ///
    /// The registration is determined by the type's implementation of [`GetTypeRegistration`].
    pub fn register<T: Reflect>(&self) {
        let status = self.entry_status(TypeId::of::<T>());
        if let EntryStatus::Registered(_) = status {
            // `T` has already been registered;
            return;
        }

        // only call `create_type_path` (which might allocate)
        // if we can't otherwise get the path.
        let path: &'static str = match status {
            EntryStatus::OnlyPath(path) => path,
            _ => match T::create_type_path() {
                Cow::Borrowed(path) => path,
                Cow::Owned(path) => Box::leak(path.into_boxed_str()),
            },
        };
        let id = TypeId::of::<T>();

        T::register_type_dependencies(self);

        let registration = T::type_registration();
        let idx = self.registrations.push(RegistryEntry {
            path,
            info: registration.info,
        });

        let db_pin = self.data_db.pin();
        for data in Self::default_registrations::<T>().chain(registration.data.into_iter()) {
            let key = DataKey {
                data: data.type_id(),
                type_: TypeId::of::<T>(),
            };

            // NB: data inserted into the db is keyed by its own `TypeId`.
            let data_ptr: Ptr<'static> = unsized_ref_to_ptr(Box::leak(data));
            db_pin.insert(key, data_ptr);
        }

        self.by_path.pin().insert(path, idx);
        self.by_id.pin().insert(id, ListIdx::Idx(idx));
    }

    pub fn register_data_for_type<T: Reflect, D: RegisterData<T>>(&self) {
        // NB: data is a `&'static D` and is keyed by the type id of D.
        let data: Box<D> = Box::new(D::make());
        let key = DataKey {
            data: TypeId::of::<D>(),
            type_: TypeId::of::<T>(),
        };

        // NB: data inserted into the db is keyed by its own `TypeId`.
        let data_ptr: Ptr<'static> = unsized_ref_to_ptr(Box::leak(data));
        self.data_db.pin().insert(key, data_ptr);
    }

    fn entry_status(&self, key: impl TypeKey) -> EntryStatus {
        TypeKey::status(key, self)
    }

    pub fn path(&self, key: TypeId) -> Option<&'static str> {
        match self.entry_status(key) {
            EntryStatus::OnlyPath(path) => Some(path),
            _ => None,
        }
    }

    pub fn info(&self, key: impl TypeKey) -> Option<&TypeInfo> {
        TypeKey::get(key, self).map(|entry| &entry.info)
    }

    pub fn data<D: TypeData>(&self, key: impl TypeKey) -> Option<&'static D> {
        let data_ptr = *self.data_db.pin().get(&DataKey {
            data: TypeId::of::<D>(),
            type_: TypeKey::to_id(key, self)?,
        })?;

        // SAFETY: data_ptr is effectively a `&'static D`:
        // - Validity is guaranteed by the semantics of `Ptr`.
        // - The type is guaranteed to be `D` by the methods
        //   `register`, and `register_data_vertical`,
        //   which ensure the entries in `self.data_db` are only
        //   indexed by their own type ids.
        //
        // Not using a trait object saves memory & time.
        let data: &'static D = unsafe { data_ptr.deref() };
        Some(data)
    }
}

pub fn global_registry() -> &'static Registry {
    static REG: OnceLock<Registry> = OnceLock::new();
    REG.get_or_init(Registry::default)
}

pub trait GetTypeRegistration: Sized {
    fn type_registration() -> Registration<Self>;
    fn register_type_dependencies(registry: &Registry);
}
