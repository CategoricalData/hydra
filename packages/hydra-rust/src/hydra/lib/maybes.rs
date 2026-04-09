// hydra.lib.maybes primitives

pub fn apply<A, B>(mf: Option<std::rc::Rc<dyn Fn(A) -> B>>, ma: Option<A>) -> Option<B> {
    match (mf, ma) {
        (Some(f), Some(a)) => Some(f(a)),
        _ => None,
    }
}

pub fn bind<A, B>(m: Option<A>, f: impl Fn(A) -> Option<B>) -> Option<B> {
    m.and_then(|x| f(x))
}

pub fn cat<A>(l: Vec<Option<A>>) -> Vec<A> {
    l.into_iter().flatten().collect()
}

pub fn from_maybe<A>(default: A, m: Option<A>) -> A {
    m.unwrap_or(default)
}

pub fn is_just<A>(m: Option<A>) -> bool {
    m.is_some()
}

pub fn is_nothing<A>(m: Option<A>) -> bool {
    m.is_none()
}

pub fn map<A, B>(f: impl Fn(A) -> B, m: Option<A>) -> Option<B> {
    m.map(f)
}

pub fn maybe<A, B>(default: B, f: impl Fn(A) -> B, m: Option<A>) -> B {
    match m {
        Some(a) => f(a),
        None => default,
    }
}

pub fn pure_<A>(x: A) -> Option<A> {
    Some(x)
}
