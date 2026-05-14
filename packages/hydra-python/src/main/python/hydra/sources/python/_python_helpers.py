"""Python equivalent of Hydra.Dsl.Python.Helpers.

Hand-written helpers built on top of the generated hydra.dsl.python.syntax DSL,
mirroring the Haskell helper module of the same name. Add functions here as needed
by the Python source DSL files.
"""

from hydra.dsl.meta.phantoms import *  # noqa: F401,F403
import hydra.dsl.python.syntax as PySyn


# Expression conversion pipeline: Primary -> ... -> Expression
# Each level wraps the inner result in the next AST node up the precedence chain.

def py_primary_to_py_expression(p):
    return PySyn.expression_simple(py_primary_to_py_disjunction(p))


def py_primary_to_py_disjunction(p):
    return wrap("hydra.python.syntax.Disjunction",
        list_([py_primary_to_py_conjunction(p)]),
    )


def py_primary_to_py_conjunction(p):
    return wrap("hydra.python.syntax.Conjunction",
        list_([py_primary_to_py_inversion(p)]),
    )


def py_primary_to_py_inversion(p):
    return PySyn.inversion_simple(py_primary_to_py_comparison(p))


def py_primary_to_py_comparison(p):
    return PySyn.comparison(py_primary_to_py_bitwise_or(p), list_([]))


def py_primary_to_py_bitwise_or(p):
    return PySyn.bitwise_or(nothing(), py_primary_to_py_bitwise_xor(p))


def py_primary_to_py_bitwise_xor(p):
    return PySyn.bitwise_xor(nothing(), py_primary_to_py_bitwise_and(p))


def py_primary_to_py_bitwise_and(p):
    return PySyn.bitwise_and(nothing(), py_primary_to_py_shift_expression(p))


def py_primary_to_py_shift_expression(p):
    return PySyn.shift_expression(nothing(), py_primary_to_py_sum(p))


def py_primary_to_py_sum(p):
    return PySyn.sum(nothing(), py_primary_to_py_term(p))


def py_primary_to_py_term(p):
    return PySyn.term(nothing(), py_primary_to_py_factor(p))


def py_primary_to_py_factor(p):
    return PySyn.factor_simple(py_primary_to_py_power(p))


def py_primary_to_py_power(p):
    return PySyn.power(py_primary_to_py_await_primary(p), nothing())


def py_primary_to_py_await_primary(p):
    return PySyn.await_primary(false(), p)


# Convenience constructors for common cases

def py_name_to_py_primary(n):
    return PySyn.primary_simple(PySyn.atom_name(n))


def py_name_to_py_expression(n):
    return py_primary_to_py_expression(py_name_to_py_primary(n))


def py_string_to_py_expression(s):
    return py_primary_to_py_expression(PySyn.primary_simple(PySyn.atom_string(s)))


def py_comparison_to_py_expression(c):
    return PySyn.expression_simple(
        wrap("hydra.python.syntax.Disjunction",
            list_(
                [
                    wrap("hydra.python.syntax.Conjunction",
                        list_(
                            [PySyn.inversion_simple(c)]
                        ),
                    )
                ]
            ),
        )
    )


def comp_pair_eq(rhs):
    return PySyn.compare_op_bitwise_or_pair(PySyn.compare_op_eq, rhs)


def double_quoted_string(val):
    return PySyn.string(val, PySyn.quote_style_double)


# Custom helpers (mirrors of Hydra.Dsl.Python.Helpers)

lambda_parameters_empty = PySyn.lambda_parameters(
    nothing(),
    list_([]),
    list_([]),
    nothing(),
)


def class_pattern_simple(name_or_attr):
    return PySyn.class_pattern(name_or_attr, nothing(), nothing())


def class_pattern_with_keywords(name_or_attr, keyword_patterns):
    return PySyn.class_pattern(
        name_or_attr, nothing(), just(keyword_patterns)
    )


def lambda_parameters_simple(no_default):
    return PySyn.lambda_parameters(
        nothing(),
        no_default,
        list_([]),
        nothing(),
    )


def args_positional_only(pos):
    return PySyn.args(
        pos,
        list_([]),
        list_([]),
    )


def py_list(els):
    """Wrap a list of star-named-expressions (TTerm-of-list) in hydra.python.syntax.List."""
    return wrap("hydra.python.syntax.List", els)


def param_no_default_parameters_simple(no_default):
    return PySyn.param_no_default_parameters(
        no_default,
        list_([]),
        nothing(),
    )


def param_no_default_simple(p):
    return PySyn.param_no_default(p, nothing())


def param_simple(n):
    return PySyn.param(n, nothing())


def simple_type_parameter_simple(n):
    return PySyn.simple_type_parameter(n, nothing(), nothing())


def string_(val, style):
    return PySyn.string(val, style)


def untyped_assignment_simple(targets, rhs):
    return PySyn.untyped_assignment(targets, rhs, nothing())
