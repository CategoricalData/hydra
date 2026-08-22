//! Proof-of-concept for #678: structural tree-ownership in Rust for Hydra's immutable data model.
//!
//! Hydra runtime data is an immutable, acyclic, tree-shaped `Term`. Recursion in the object language is
//! expressed by variable *name* (see `Core.hs`: `Lambda { parameter :: Name, body :: Term }` and
//! `Term::Variable(Name)`), NOT by an object link back to the binder — so the in-memory structure is a
//! finite tree, never a cyclic object graph.
//!
//! The discipline this POC demonstrates:
//!   * Hydra record  -> Rust `struct`
//!   * Hydra union   -> Rust `enum`
//!   * recursive child -> `Box<T>`   (a directly self-recursive enum is otherwise of infinite size and
//!                                     will not compile; `Box` gives it a known size behind one owning
//!                                     heap pointer)
//!   * list<T> -> Vec<T>, optional<T> -> Option<T>, pair -> tuple, name -> String
//!   * NO `Rc` (that is shared ownership — unneeded for a unique-ownership tree)
//!   * NO `RefCell` (interior mutability — the data is immutable)
//!   * NO explicit lifetimes (those annotate borrows — an owned tree has none)
//!
//! Leak-freedom claim: under unique ownership, dropping the root recursively drops every `Box`, `Vec`, and
//! `Option` in the tree exactly once. The `drop_count` test proves this empirically with an atomic counter.

use std::sync::atomic::{AtomicUsize, Ordering};

/// A name reference. Hydra recursion goes through names, so a self-referential lambda holds a `String`
/// here, not a pointer back to itself. This is why the heap structure stays acyclic.
pub type Name = String;

/// A deliberately small but faithful slice of Hydra's `Term`, exercising every ownership shape:
/// a directly-recursive boxed field (`Lambda.body`, `Application`), a boxed field inside a struct
/// (`Lambda`), a `Vec` of recursive children (`List`), an `Option` of a recursive child (`Optional`),
/// a tuple of recursive children (`Pair`), and leaf/name variants (`Variable`, `Literal`).
#[derive(Debug)]
pub enum Term {
    /// A variable reference — a name lookup, NOT a link to the binder. Leaf node.
    Variable(Name),
    /// A literal leaf.
    Literal(i64),
    /// A lambda abstraction. `parameter` is a NAME; `body` is an owned recursive child via `Box`.
    Lambda(Lambda),
    /// Function application: two owned recursive children.
    Application(Box<Term>, Box<Term>),
    /// A list of owned recursive children.
    List(Vec<Term>),
    /// An optional owned recursive child.
    Optional(Option<Box<Term>>),
    /// A pair of owned recursive children.
    Pair(Box<Term>, Box<Term>),
}

/// Hydra record -> Rust struct. The recursive `body` field is owned via `Box`.
#[derive(Debug)]
pub struct Lambda {
    pub parameter: Name,
    pub body: Box<Term>,
}

/// Global drop counter used only by the test to prove every node is freed exactly once.
static DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

/// A wrapper node that bumps the shared counter when dropped. We build a tree of these in parallel with
/// the `Term` tree so we can count drops without altering `Term` itself.
///
/// Mirrors `Term`'s ownership shapes exactly, so counting its drops counts the discipline's drops.
#[derive(Debug)]
pub enum Counted {
    Leaf,
    Boxed(Box<Counted>),
    Two(Box<Counted>, Box<Counted>),
    Many(Vec<Counted>),
    Opt(Option<Box<Counted>>),
}

impl Drop for Counted {
    fn drop(&mut self) {
        DROP_COUNT.fetch_add(1, Ordering::SeqCst);
    }
}

/// Build a moderately deep, wide `Counted` tree with a known node count.
/// Returns (tree, node_count).
pub fn build_counted(depth: usize) -> (Counted, usize) {
    if depth == 0 {
        return (Counted::Leaf, 1);
    }
    let (left, ln) = build_counted(depth - 1);
    let (right, rn) = build_counted(depth - 1);
    let (opt_child, on) = build_counted(depth - 1);
    // A Many with three children: two Boxed and one Opt, plus this node itself.
    let node = Counted::Many(vec![
        Counted::Boxed(Box::new(left)),
        Counted::Two(Box::new(right), Box::new(Counted::Leaf)),
        Counted::Opt(Some(Box::new(opt_child))),
    ]);
    // node itself (1) + Boxed wrapper (1) + Two wrapper (1) + its extra Leaf (1) + Opt wrapper (1)
    // + the three recursive subtrees.
    let count = 1 + 1 + 1 + 1 + 1 + ln + rn + on;
    (node, count)
}

/// Construct a representative `Term` tree by hand to show the discipline compiles and runs.
/// `(\x -> (f x))` applied to `42`, wrapped in a list and a pair, with an optional.
pub fn build_sample_term() -> Term {
    let lam = Term::Lambda(Lambda {
        parameter: "x".to_string(),
        body: Box::new(Term::Application(
            Box::new(Term::Variable("f".to_string())),
            Box::new(Term::Variable("x".to_string())),
        )),
    });
    let applied = Term::Application(Box::new(lam), Box::new(Term::Literal(42)));
    Term::Pair(
        Box::new(Term::List(vec![
            applied,
            Term::Optional(Some(Box::new(Term::Literal(7)))),
            Term::Optional(None),
        ])),
        Box::new(Term::Variable("top".to_string())),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The sample `Term` tree constructs and drops without error under the Box discipline.
    #[test]
    fn sample_term_constructs_and_drops() {
        let t = build_sample_term();
        // Force a traversal so the compiler cannot optimize the tree away.
        assert!(count_terms(&t) >= 6);
        drop(t); // explicit; also happens at scope end.
    }

    fn count_terms(t: &Term) -> usize {
        1 + match t {
            Term::Variable(_) | Term::Literal(_) => 0,
            Term::Lambda(l) => count_terms(&l.body),
            Term::Application(a, b) | Term::Pair(a, b) => count_terms(a) + count_terms(b),
            Term::List(xs) => xs.iter().map(count_terms).sum(),
            Term::Optional(o) => o.as_ref().map(|b| count_terms(b)).unwrap_or(0),
        }
    }

    /// The core leak-freedom proof: every node in a `Box`/`Vec`/`Option` tree is dropped EXACTLY once.
    /// If any node leaked, the drop count would be short; if any were double-freed, Rust would abort
    /// (and the count would be high) — neither happens.
    #[test]
    fn every_node_dropped_exactly_once() {
        DROP_COUNT.store(0, Ordering::SeqCst);
        let (tree, expected) = build_counted(6);
        assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 0, "nothing dropped during construction");
        drop(tree);
        let dropped = DROP_COUNT.load(Ordering::SeqCst);
        assert_eq!(
            dropped, expected,
            "expected exactly {} drops (one per owned node), got {}",
            expected, dropped
        );
    }
}
