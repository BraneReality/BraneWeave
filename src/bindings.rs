use std::sync::{Arc, Mutex};

/// Type for creating lazy-evaluated bindings

pub trait Binding {
    type Value: 'static;

    fn get(&self) -> Self::Value;
    fn version(&self) -> usize;
}

pub trait MutBinding<Value: 'static>: Binding<Value = Value> {
    fn set(&self, value: Value);
}

pub trait ToBinding<Value: 'static> {
    type Type: Binding<Value = Value> + 'static;
    fn to_binding(self) -> Self::Type;
}

impl<B: Binding + 'static> ToBinding<B::Value> for B {
    type Type = B;
    fn to_binding(self) -> Self::Type {
        self
    }
}

pub trait JoinableBinding: Binding + Sized + Clone + 'static {
    fn join<O, V>(self, other: O) -> JoinedBinding<Self, O::Type>
    where
        V: Clone + 'static,
        O: ToBinding<V>,
    {
        JoinedBinding::<Self, O::Type>::new(self.clone(), other)
    }
}
impl<B: Binding + Clone + 'static> JoinableBinding for B {}

pub trait MapableBinding: Binding + Sized + Clone + 'static {
    fn map<M, MF>(self, f: MF) -> MappedBinding<M, Self, MF>
    where
        M: 'static + Clone,
        MF: Fn(<Self as Binding>::Value) -> M + 'static + Clone,
    {
        MappedBinding::new(self, f)
    }
}
impl<B: Binding + Clone + 'static> MapableBinding for B {}

pub trait BoxableBinding: Binding + 'static {
    fn boxed(self) -> BindingBox<<Self as Binding>::Value> {
        BindingBox::new(self)
    }
}
impl<B: Binding + 'static> BoxableBinding for B {}

#[derive(Clone)]
pub struct BindingBox<Value> {
    b: Arc<dyn Binding<Value = Value>>,
}

impl<Value> BindingBox<Value> {
    pub fn new(b: impl Binding<Value = Value> + 'static) -> Self {
        let b = Arc::new(b);
        Self { b }
    }
}

impl<Value: 'static> Binding for BindingBox<Value> {
    type Value = Value;

    fn get(&self) -> Self::Value {
        self.b.get()
    }

    fn version(&self) -> usize {
        self.b.version()
    }
}

#[derive(Clone)]
pub struct CopyBinding<Value>
where
    Value: 'static + Copy + Clone,
{
    inner: Value,
}

impl<Value: 'static + Copy + Clone> CopyBinding<Value> {}

impl<Value: 'static + Copy + Clone> Binding for CopyBinding<Value> {
    type Value = Value;
    fn get(&self) -> Value {
        self.inner
    }

    fn version(&self) -> usize {
        0
    }
}

macro_rules! impl_copy_binding {
    // `$binding:ident` is the generic binding type constructor like `ArcBinding`
    ($($ty:ty),*) => {
        $(
            impl ToBinding<$ty> for $ty {
                type Type = CopyBinding<$ty>;
                fn to_binding(self) -> Self::Type {
                    CopyBinding {
                        inner: self,
                    }
                }
            }
        )*
    };
}

#[derive(Clone)]
pub struct ArcBinding<Value>
where
    Value: 'static + Clone,
{
    inner: Arc<Value>,
}

impl<Value: 'static + Clone> ArcBinding<Value> {}

impl<Value: 'static + Clone> Binding for ArcBinding<Value> {
    type Value = Value;
    fn get(&self) -> Value {
        (*self.inner).clone()
    }

    fn version(&self) -> usize {
        0
    }
}

macro_rules! impl_arc_binding {
    // `$binding:ident` is the generic binding type constructor like `ArcBinding`
    ($($ty:ty),*) => {
        $(
            impl ToBinding<$ty> for $ty {
                type Type = ArcBinding<$ty>;
                fn to_binding(self) -> Self::Type {
                    ArcBinding {
                        inner: std::sync::Arc::new(self),
                    }
                }
            }
        )*
    };
}

// Now call the macro with all desired scalar types:
impl_copy_binding!(
    i8, i16, i32, i64, i128, u8, u16, u32, u64, u128, f32, f64, bool, char
);

impl_arc_binding!(String);

#[derive(Clone)]
pub struct ValueBinding<Value>
where
    Value: 'static + Clone,
{
    inner: Arc<Mutex<(usize, Value)>>,
}

impl<Value: 'static + Clone> ValueBinding<Value> {
    pub fn new(value: Value) -> Self {
        Self {
            inner: Arc::new(Mutex::new((0, value))),
        }
    }
}

impl<Value> MutBinding<Value> for ValueBinding<Value>
where
    Value: 'static + Clone,
{
    fn set(&self, value: Value) {
        let mut inner = self.inner.lock().unwrap();
        inner.0 = inner.0.wrapping_add(1);
        inner.1 = value;
    }
}

impl<Value: 'static + Clone> Binding for ValueBinding<Value> {
    type Value = Value;
    fn get(&self) -> Value {
        (*self.inner).lock().unwrap().1.clone()
    }

    fn version(&self) -> usize {
        self.inner.lock().unwrap().0
    }
}

#[derive(Clone)]
pub struct MappedBinding<Value, Arg, F>
where
    Value: 'static + Clone,
    Arg: Binding,
    F: Fn(Arg::Value) -> Value + 'static + Clone,
{
    src: Arg,
    cache: Arc<Mutex<(usize, Value)>>,
    f: F,
}

impl<Value, Arg, F> MappedBinding<Value, Arg, F>
where
    Value: 'static + Clone,
    Arg: Binding + 'static,
    F: Fn(Arg::Value) -> Value + 'static + Clone,
{
    pub fn new(src: impl ToBinding<Arg::Value, Type = Arg>, f: F) -> Self {
        let src = src.to_binding();
        let cache = Arc::new(Mutex::new((0usize, f(src.get()))));
        Self { src, cache, f }
    }
}

impl<Value, Arg, F> Binding for MappedBinding<Value, Arg, F>
where
    Value: 'static + Clone,
    Arg: Binding,
    F: Fn(Arg::Value) -> Value + 'static + Clone,
{
    type Value = Value;

    fn get(&self) -> Value {
        let mut cache = self.cache.lock().unwrap();
        let version = self.src.version();
        if cache.0 != version {
            cache.1 = (self.f)(self.src.get());
            cache.0 = version;
        }
        cache.1.clone()
    }

    fn version(&self) -> usize {
        self.src.version()
    }
}

#[derive(Clone)]
pub struct JoinedBinding<B1, B2>
where
    B1: Binding,
    B2: Binding,
{
    src1: B1,
    src2: B2,
}

impl<B1: Binding + 'static, B2: Binding + 'static> JoinedBinding<B1, B2> {
    pub fn new(
        b1: impl ToBinding<B1::Value, Type = B1>,
        b2: impl ToBinding<B2::Value, Type = B2>,
    ) -> Self {
        Self {
            src1: b1.to_binding(),
            src2: b2.to_binding(),
        }
    }
}

impl<B1: Binding, B2: Binding> Binding for JoinedBinding<B1, B2> {
    type Value = (B1::Value, B2::Value);
    fn get(&self) -> (B1::Value, B2::Value) {
        (self.src1.get(), self.src2.get())
    }

    fn version(&self) -> usize {
        self.src1.version().wrapping_add(self.src2.version())
    }
}
