use std::{any::TypeId, borrow::Cow, iter};

use perfect_reflect::*;
use registry::{GetTypeRegistration, Registration};

#[derive(PartialEq, Debug)]
enum Foo<T> {
    Bar,
    Baz(T, usize),
}

impl<T: Reflect> FromValue for Foo<T> {
    fn from_value_ref(value: impl AsRef<Value>) -> Result<Self, FromValueError> {
        fn baz_variant<T: Reflect>(fields: &[Value]) -> Result<Foo<T>, FromValueError> {
            let [_0, _1] = fields else {
                return Err(FromValueError::MissingField {
                    name: FieldName::Index(1),
                });
            };

            let _0 = T::from_value_ref(_0).map_err(|err| err.nested_in(0))?;
            let _1 = usize::from_value_ref(_1).map_err(|err| err.nested_in(1))?;
            Ok(Foo::Baz(_0, _1))
        }

        let value = value.as_ref();
        value
            .try_downcast_ref()
            .and_then(|r: &Self| r.try_clone().map_err(From::from))
            .or_else(|err| match value {
                Value::Enum(variant) => match variant.id.name.as_ref() {
                    "Bar" => variant
                        .fields
                        .try_as_unit()
                        // kinda silly; i'm trying to imagine what a macro would generate.
                        .and_then(|()| Ok(Foo::Bar))
                        .map_err(|e| e.nested_in(VariantId::from("Bar"))),
                    "Baz" => variant
                        .fields
                        .try_as_unnamed()
                        .and_then(|fields| baz_variant(fields))
                        .map_err(|e| e.nested_in(VariantId::from("Baz"))),
                    _ => Err(FromValueError::VariantMismatch {
                        found: variant.id.clone(),
                        expected: vec!["Bar".into(), "Baz".into()],
                    }),
                },
                _ => Err(err),
            })
    }

    // this impl turned out a lot more complicated than i expected.
    // most of it is error recovery.
    fn from_value(value: impl Into<Value>) -> Result<Self, Recover<FromValueError, Value>> {
        fn baz_variant<T: Reflect>(
            fields: Vec<Value>,
        ) -> Result<Foo<T>, Recover<FromValueError, Vec<Value>>> {
            let [_0, _1] = match <[Value; 2]>::try_from(fields) {
                Ok(fields) => fields,
                Err(fields) => {
                    return Err(Recover {
                        err: FromValueError::MissingField {
                            name: FieldName::Index(1),
                        },
                        recover: fields,
                    })
                }
            };

            let _0 = match T::from_value(_0) {
                Ok(good) => good,
                Err(rec) => return Err(rec.map_both(|err| err.nested_in(0), |v| vec![v, _1])),
            };
            let _1 = match usize::from_value(_1) {
                Ok(good) => good,
                Err(rec) => {
                    return Err(
                        rec.map_both(|err| err.nested_in(1), |v| vec![Value::from_reflect(_0), v])
                    )
                }
            };
            Ok(Foo::Baz(_0, _1))
        }
        value.into().try_take().or_else(
            |Recover {
                 recover: value,
                 err,
             }| match value {
                Value::Enum(variant) => match variant.id.name.as_ref() {
                    "Bar" => variant
                        .fields
                        .try_into_unit()
                        // kinda silly; i'm trying to imagine what a macro would generate.
                        .and_then(|()| Ok(Foo::Bar))
                        .map_err(|rec| {
                            rec.map_both(
                                |e| e.nested_in(VariantId::from("Bar")),
                                |v| {
                                    VariantValue {
                                        id: "Baz".into(),
                                        fields: v,
                                    }
                                    .into()
                                },
                            )
                        }),
                    "Baz" => variant
                        .fields
                        .try_into_unnamed()
                        .and_then(|fields| {
                            baz_variant(fields).map_err(|rec| rec.map(FieldValues::Unnamed))
                        })
                        .map_err(|rec| {
                            rec.map_both(
                                |e| e.nested_in(VariantId::from("Baz")),
                                |v| {
                                    VariantValue {
                                        id: "Baz".into(),
                                        fields: v,
                                    }
                                    .into()
                                },
                            )
                        }),
                    _ => Err(Recover {
                        err: FromValueError::VariantMismatch {
                            found: variant.id.clone(),
                            expected: vec!["Bar".into(), "Baz".into()],
                        },
                        recover: variant.into(),
                    }),
                },
                value => Err(Recover {
                    err,
                    recover: value,
                }),
            },
        )
    }
}

impl<T: TypePath> TypePath for Foo<T> {
    fn type_path() -> &'static str {
        let reg = registry::global_registry();
        reg.register_path::<Foo<T>>()
    }

    fn create_type_path() -> Cow<'static, str> {
        let path = format!(concat!(module_path!(), "::Foo::<{}>"), T::type_path());
        Cow::Owned(path)
    }
}

impl<T: Reflect> Typed for Foo<T> {
    fn create_type_info() -> TypeInfo {
        TypeInfo {
            id: TypeId::of::<T>(),
        }
    }

    fn kind() -> ReflectKind {
        ReflectKind::Struct
    }
}

impl<T: Reflect> TryClone for Foo<T> {
    fn try_clone(&self) -> Result<Self, CloneError> {
        match self {
            Foo::Bar => Ok(Foo::Bar),
            Foo::Baz(a, b) => Ok(Foo::Baz(a.try_clone()?, b.try_clone()?)),
        }
    }
}

impl<T: Reflect> GetTypeRegistration for Foo<T> {
    fn type_registration() -> Registration<Self> {
        Registration::default()
    }

    fn register_type_dependencies(registry: &registry::Registry) {
        registry.register::<T>();
        registry.register::<usize>();
    }
}

#[allow(non_camel_case_types)]
#[derive(ref_cast::RefCast)]
#[repr(transparent)]
struct Foo_BazView<T>(Foo<T>);

impl<T: Reflect> Tuple for Foo_BazView<T> {
    fn field(&self, index: usize) -> Option<&dyn Reflect> {
        let Foo::Baz(_0, _1) = &self.0 else {
            unreachable!()
        };
        match index {
            0 => Some(_0),
            1 => Some(_1),
            _ => None,
        }
    }

    fn field_mut(&mut self, index: usize) -> Option<&mut dyn Reflect> {
        let Foo::Baz(_0, _1) = &mut self.0 else {
            unreachable!()
        };
        match index {
            0 => Some(_0),
            1 => Some(_1),
            _ => None,
        }
    }

    fn fields(&self) -> Box<dyn Iterator<Item = &dyn Reflect> + '_> {
        let mut i = 0;
        Box::new(iter::from_fn(move || {
            i += 1;
            self.field(i)
        }))
    }
}

impl<T: Reflect> Fields for Foo<T> {
    fn fields_ref(&self) -> FieldsRef<'_> {
        match self {
            Foo::Bar => FieldsRef::Unit,
            var @ Foo::Baz(_, _) => {
                FieldsRef::Unnamed(<Foo_BazView<T> as ref_cast::RefCast>::ref_cast(var))
            }
        }
    }

    fn fields_mut(&mut self) -> FieldsMut<'_> {
        match self {
            Foo::Bar => FieldsMut::Unit,
            var @ Foo::Baz(_, _) => {
                FieldsMut::Unnamed(<Foo_BazView<T> as ref_cast::RefCast>::ref_cast_mut(var))
            }
        }
    }
}

impl<T: Reflect> Enum for Foo<T> {
    fn variant_id(&self) -> VariantId {
        match self {
            Foo::Bar { .. } => "Bar".into(),
            Foo::Baz { .. } => "Baz".into(),
        }
    }
}

impl<T: Reflect> Reflect for Foo<T> {
    fn reflect_ref(&self) -> ReflectRef {
        ReflectRef::Enum(self)
    }

    fn reflect_mut(&mut self) -> ReflectMut {
        ReflectMut::Enum(self)
    }

    fn reflect_owned(self: Box<Self>) -> ReflectOwned {
        ReflectOwned::Enum(self)
    }

    fn as_reflect(&self) -> &dyn Reflect {
        self
    }

    fn as_reflect_mut(&mut self) -> &mut dyn Reflect {
        self
    }
}

fn main() {
    let value = Value::Enum(VariantValue {
        id: "Baz".into(),
        fields: FieldValues::Unnamed(vec![
            Value::Struct(FieldValues::Unit),
            Value::from_reflect(5_usize),
        ]),
    });
    assert_eq!(<Foo<()>>::from_value_ref(&value), Ok(Foo::Baz((), 5)));

    let value = Value::Enum(VariantValue {
        id: "Baz".into(),
        fields: FieldValues::Unnamed(vec![
            Value::Struct(FieldValues::Unit),
            Value::from_reflect(4_i64),
        ]),
    });
    assert_eq!(
        <Foo<()>>::from_value_ref(&value),
        Err(FromValueError::Downcast(DowncastError {
            found: Cow::Borrowed(i64::type_path()),
            expected: usize::type_path()
        })
        .nested_in(1)
        .nested_in(VariantId::from("Baz")))
    );

    assert_eq!(
        <Foo<()>>::from_value_ref(&value).unwrap_err().to_string(),
        "field Baz.1: expected a value of type `usize`, but found type `i64`",
    );

    assert_eq!(<Foo<()>>::type_path(), "implementing_traits::Foo::<()>");
    assert_eq!(
        registry::global_registry().path(TypeId::of::<Foo<()>>()),
        Some("implementing_traits::Foo::<()>")
    );
}
