use std::{any::TypeId, marker::PhantomData, ptr, sync::OnceLock};

use crate::{Reflect, TypeInfo};

pub trait TypeData: Sized + 'static {}

pub trait RegisterData<T>: TypeData + 'static
where
    T: Reflect,
{
    fn make() -> Self;
}

pub struct Registration<T> {
    info: TypeInfo,
    data: Vec<(TypeId, *const ())>,
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
        // NB: data is a `&'static D` and is keyed by the type id of D.
        let data: &'static D = Box::leak(Box::new(D::make()));
        self.data
            .push((TypeId::of::<D>(), ptr::from_ref(data).cast()));

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
        pub(super) info: TypeInfo,
    }

    pub trait TypeKey: Copy {
        fn contains(key: Self, registry: &Registry) -> bool;
        fn get(key: Self, registry: &Registry) -> Option<&RegistryEntry>;
        #[inline]
        fn to_id(key: Self, registry: &Registry) -> Option<TypeId> {
            Self::get(key, registry).map(|entry| entry.info.id)
        }
    }

    impl<'a> TypeKey for &'a str {
        #[inline]
        fn contains(key: Self, registry: &Registry) -> bool {
            registry.by_path.pin().contains_key(key)
        }
        #[inline]
        fn get(key: Self, registry: &Registry) -> Option<&RegistryEntry> {
            registry
                .by_path
                .pin()
                .get(key)
                .and_then(|i| registry.registrations.get(*i))
        }
    }

    impl TypeKey for TypeId {
        #[inline]
        fn contains(key: TypeId, registry: &Registry) -> bool {
            registry.by_id.pin().contains_key(&key)
        }
        #[inline]
        fn get(ref key: TypeId, registry: &Registry) -> Option<&RegistryEntry> {
            registry
                .by_id
                .pin()
                .get(key)
                .and_then(|i| registry.registrations.get(*i))
        }

        #[inline]
        fn to_id(key: TypeId, _registry: &Registry) -> Option<TypeId> {
            Some(key)
        }
    }
}

use query::RegistryEntry;
pub use query::TypeKey;

#[derive(Default)]
pub struct Registry {
    registrations: boxcar::Vec<RegistryEntry>,
    data_db: papaya::HashMap<DataKey, *const ()>,
    by_id: papaya::HashMap<TypeId, usize>,
    by_path: papaya::HashMap<&'static str, usize>,
}

unsafe impl Send for Registry {}
unsafe impl Sync for Registry {}

impl Registry {
    /// Register a single type into the registry.
    ///
    /// The registration is determined by the type's implementation of [`GetTypeRegistration`].
    pub fn register<T: Reflect>(&self) {
        if self.contains_key(TypeId::of::<T>()) {
            // `T` has already been registered;
            return;
        }

        T::register_type_dependencies(self);

        let registration = T::type_registration();

        let TypeInfo { id, path, .. } = registration.info;

        let idx = self.registrations.push(RegistryEntry {
            info: registration.info,
        });

        // i think in this case by-ref iteration and copying is faster than `into_iter`
        for &(data, data_ptr) in &registration.data {
            let key = DataKey {
                data,
                type_: TypeId::of::<T>(),
            };
            self.data_db.pin().insert(key, data_ptr);
        }

        self.by_path.pin().insert(path, idx);
        self.by_id.pin().insert(id, idx);
    }

    pub fn register_data_for_type<T: Reflect, D: RegisterData<T>>(&self) {
        // NB: data is a `&'static D` and is keyed by the type id of D.
        let data: &'static D = Box::leak(Box::new(D::make()));
        let key = DataKey {
            data: TypeId::of::<D>(),
            type_: TypeId::of::<T>(),
        };

        let data_ptr = ptr::from_ref(data).cast();
        self.data_db.pin().insert(key, data_ptr);
    }

    pub fn contains_key(&self, key: impl TypeKey) -> bool {
        TypeKey::contains(key, self)
    }

    pub fn info(&self, key: impl TypeKey) -> Option<&TypeInfo> {
        TypeKey::get(key, self).map(|entry| &entry.info)
    }

    pub fn data<D: TypeData>(&self, key: impl TypeKey) -> Option<&'static D> {
        let data_ptr = *self.data_db.pin().get(&DataKey {
            data: TypeId::of::<D>(),
            type_: TypeKey::to_id(key, self)?,
        })?;

        // SAFETY: data_ptr is effectively a `'static D`.
        // validity and type are guaranteed by `Registration::data`,
        // and `register_data_vertical`.
        // The only reason we don't use a trait object is to save memory
        let data = unsafe { &*data_ptr.cast::<D>() };
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
