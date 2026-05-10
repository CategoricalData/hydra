"""Python equivalent of Hydra.Dsl.Python.Helpers.

Hand-written helpers built on top of the generated hydra.dsl.python.syntax DSL,
mirroring the Haskell helper module of the same name. Add functions here as needed
by the Python source DSL files.
"""

import hydra.dsl.meta.phantoms as Phantoms
import hydra.dsl.python.syntax as PySyn


# Expression conversion pipeline: Primary -> ... -> Expression
# Each level wraps the inner result in the next AST node up the precedence chain.

def py_primary_to_py_expression(p):
    return PySyn.expression_simple(py_primary_to_py_disjunction(p))


def py_primary_to_py_disjunction(p):
    return Phantoms.wrap(
        Phantoms.Name("hydra.python.syntax.Disjunction"),
        Phantoms.list_([py_primary_to_py_conjunction(p)]),
    )


def py_primary_to_py_conjunction(p):
    return Phantoms.wrap(
        Phantoms.Name("hydra.python.syntax.Conjunction"),
        Phantoms.list_([py_primary_to_py_inversion(p)]),
    )


def py_primary_to_py_inversion(p):
    return PySyn.inversion_simple(py_primary_to_py_comparison(p))


def py_primary_to_py_comparison(p):
    return PySyn.comparison(py_primary_to_py_bitwise_or(p), Phantoms.list_([]))


def py_primary_to_py_bitwise_or(p):
    return PySyn.bitwise_or(Phantoms.nothing(), py_primary_to_py_bitwise_xor(p))


def py_primary_to_py_bitwise_xor(p):
    return PySyn.bitwise_xor(Phantoms.nothing(), py_primary_to_py_bitwise_and(p))


def py_primary_to_py_bitwise_and(p):
    return PySyn.bitwise_and(Phantoms.nothing(), py_primary_to_py_shift_expression(p))


def py_primary_to_py_shift_expression(p):
    return PySyn.shift_expression(Phantoms.nothing(), py_primary_to_py_sum(p))


def py_primary_to_py_sum(p):
    return PySyn.sum(Phantoms.nothing(), py_primary_to_py_term(p))


def py_primary_to_py_term(p):
    return PySyn.term(Phantoms.nothing(), py_primary_to_py_factor(p))


def py_primary_to_py_factor(p):
    return PySyn.factor_simple(py_primary_to_py_power(p))


def py_primary_to_py_power(p):
    return PySyn.power(py_primary_to_py_await_primary(p), Phantoms.nothing())


def py_primary_to_py_await_primary(p):
    return PySyn.await_primary(Phantoms.false(), p)


# Convenience constructors for common cases

def py_name_to_py_primary(n):
    return PySyn.primary_simple(PySyn.atom_name(n))


def py_name_to_py_expression(n):
    return py_primary_to_py_expression(py_name_to_py_primary(n))


def py_string_to_py_expression(s):
    return py_primary_to_py_expression(PySyn.primary_simple(PySyn.atom_string(s)))


def py_comparison_to_py_expression(c):
    return PySyn.expression_simple(
        Phantoms.wrap(
            Phantoms.Name("hydra.python.syntax.Disjunction"),
            Phantoms.list_(
                [
                    Phantoms.wrap(
                        Phantoms.Name("hydra.python.syntax.Conjunction"),
                        Phantoms.list_(
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
    Phantoms.nothing(),
    Phantoms.list_([]),
    Phantoms.list_([]),
    Phantoms.nothing(),
)


def class_pattern_simple(name_or_attr):
    return PySyn.class_pattern(name_or_attr, Phantoms.nothing(), Phantoms.nothing())


def class_pattern_with_keywords(name_or_attr, keyword_patterns):
    return PySyn.class_pattern(
        name_or_attr, Phantoms.nothing(), Phantoms.just(keyword_patterns)
    )


def lambda_parameters_simple(no_default):
    return PySyn.lambda_parameters(
        Phantoms.nothing(),
        no_default,
        Phantoms.list_([]),
        Phantoms.nothing(),
    )


def args_positional_only(pos):
    return PySyn.args(
        pos,
        Phantoms.list_([]),
        Phantoms.list_([]),
    )


def list_(els):
    """Wrap a list of star-named-expressions in a List."""
    return Phantoms.wrap(Phantoms.Name("hydra.python.syntax.List"), els)


def param_no_default_parameters_simple(no_default):
    return PySyn.param_no_default_parameters(
        no_default,
        Phantoms.list_([]),
        Phantoms.nothing(),
    )


def param_no_default_simple(p):
    return PySyn.param_no_default(p, Phantoms.nothing())


def param_simple(n):
    return PySyn.param(n, Phantoms.nothing())


def simple_type_parameter_simple(n):
    return PySyn.simple_type_parameter(n, Phantoms.nothing(), Phantoms.nothing())


def string_(val, style):
    return PySyn.string(val, style)


def untyped_assignment_simple(targets, rhs):
    return PySyn.untyped_assignment(targets, rhs, Phantoms.nothing())
