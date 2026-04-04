"""Data model definition for the Lispy interpreter.

This is a Python translation of the Haskell Lispy.hs module,
demonstrating how to use the Hydra Types DSL in Python.
"""

from hydra.core import Binding, Name, TypeScheme, Type
from hydra.dsl.annotations import doc
from hydra.dsl.python import Just, Nothing
from hydra.packaging import Module, Namespace
import hydra.dsl.types as T


# Namespace for this module
ns = Namespace("org.example.lispy")


def qualified_name(local: str) -> Name:
    """Create a fully qualified name in this namespace."""
    return Name(f"org.example.lispy.{local}")


def define(local_name: str, typ: Type) -> Binding:
    """Define a type binding in this module's namespace."""
    return Binding(
        name=qualified_name(local_name),
        term=typ,  # Note: In a full implementation, this would be the encoded type
        type=Nothing()  # Type scheme is optional
    )


# Type references (for recursive types, use variable references)
atom = T.variable("org.example.lispy.Atom")
closure = T.variable("org.example.lispy.Closure")
env = T.variable("org.example.lispy.Env")
expr = T.variable("org.example.lispy.Expr")
lisp_list = T.variable("org.example.lispy.LispList")
number = T.variable("org.example.lispy.Number")
proc = T.variable("org.example.lispy.Proc")
s_expr = T.variable("org.example.lispy.SExpr")
symbol = T.variable("org.example.lispy.Symbol")
value = T.variable("org.example.lispy.Value")


# Type definitions

atom_type = doc(
    "An atomic Lispy value: number, boolean, None, or symbol",
    T.union([
        T.field("number", number),
        T.field("boolean", T.unit()),
        T.field("none", T.unit()),
        T.field("symbol", symbol),
    ])
)

closure_type = T.record([
    T.field("params", T.list_(symbol)),
    T.field("body", expr),
    T.field("env", env),
])

env_type = doc(
    "Lexical environment with optional parent chaining.",
    T.record([
        T.field("bindings", T.map_(T.string(), value)),
        T.field("parent", T.optional(env)),
    ])
)

expr_type = doc(
    "An expression is either an atom or an S-expression.",
    T.union([
        T.field("atom", atom),
        T.field("sexpr", s_expr),
    ])
)

lisp_list_type = T.list_(value)

number_type = doc(
    "Either an integer or a floating point number. "
    "NOTE: `bool` is a subclass of `int` at runtime, but we keep it explicit here to "
    "reflect Lispy truthiness (only `False`/`None` are falsey).",
    T.enum(["int", "float"])
)

proc_type = doc(
    "A built-in procedure",
    T.string()  # Placeholder for Callable[..., Value]
)

s_expr_type = doc(
    "An S-expression is a tuple whose elements are expressions (recursive type).",
    T.list_(expr)
)

symbol_type = T.string()

value_type = doc(
    "Runtime values:\n"
    "- atoms (numbers/bools/None/symbols)\n"
    "- Lisp lists (tuples of values)\n"
    "- procedures (builtins and closures)\n\n"
    "We keep Expr separate because it includes unevaluated S-expressions; Value is "
    "what evaluation can produce.",
    T.union([
        T.field("atom", atom),
        T.field("list", lisp_list),
        T.field("proc", proc),
        T.field("closure", closure),
    ])
)


# Type bindings for the module
atom_binding = define("Atom", atom_type)
closure_binding = define("Closure", closure_type)
env_binding = define("Env", env_type)
expr_binding = define("Expr", expr_type)
lisp_list_binding = define("LispList", lisp_list_type)
number_binding = define("Number", number_type)
proc_binding = define("Proc", proc_type)
s_expr_binding = define("SExpr", s_expr_type)
symbol_binding = define("Symbol", symbol_type)
value_binding = define("Value", value_type)


# Module definition
module_ = Module(
    namespace=ns,
    elements=(
        atom_binding,
        closure_binding,
        env_binding,
        expr_binding,
        lisp_list_binding,
        number_binding,
        proc_binding,
        s_expr_binding,
        symbol_binding,
        value_binding,
    ),
    term_dependencies=(),
    type_dependencies=(),  # Would include Core.ns equivalent if needed
    description=Just("Data model definition for the Lispy interpreter"),
)
