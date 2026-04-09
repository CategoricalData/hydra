// hydra.lib.logic primitives

pub fn and_(a: bool, b: bool) -> bool {
    a && b
}

pub fn if_else<A>(cond: bool, then_val: A, else_val: A) -> A {
    if cond { then_val } else { else_val }
}

pub fn not_(a: bool) -> bool {
    !a
}

pub fn or_(a: bool, b: bool) -> bool {
    a || b
}
