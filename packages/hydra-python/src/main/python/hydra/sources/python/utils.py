"""Python utilities for constructing Python syntax trees.

Mirror of packages/hydra-python/src/main/haskell/Hydra/Sources/Python/Utils.hs.
"""

from hydra.core import Name
from hydra.dsl.python import Just, Nothing
from hydra.packaging import Module, Namespace

import hydra.dsl.meta.lib.equality as Equality
import hydra.dsl.meta.lib.lists as Lists
import hydra.dsl.meta.lib.logic as Logic
import hydra.dsl.meta.lib.maps as Maps
import hydra.dsl.meta.lib.maybes as Maybes
import hydra.dsl.meta.lib.pairs as Pairs
import hydra.dsl.meta.lib.strings as Strings
from hydra.dsl.meta.phantoms import *  # noqa: F401,F403
import hydra.dsl.packaging as Packaging
import hydra.dsl.python.syntax as PySyn

from hydra.sources.python import _python_helpers as PyDsl


# ----------------------------------------------------------------------
# Module setup
# ----------------------------------------------------------------------

NS = Namespace("hydra.python.utils")

from hydra.sources.python._source_dsl import (
    KERNEL_TYPES_NAMESPACES,
    make_def,
    make_local,
)

# Mirror Haskell:
#   [PyNames.ns, PySerde.ns, Serialization.ns, Analysis.ns] L.++
#   (PyEnvironmentSource.ns:PySyntax.ns:KernelTypes.kernelTypesNamespaces)
DEPENDENCIES = [
    Namespace("hydra.python.names"),
    Namespace("hydra.python.serde"),
    Namespace("hydra.serialization"),
    Namespace("hydra.analysis"),
    Namespace("hydra.python.environment"),
    Namespace("hydra.python.syntax"),
] + KERNEL_TYPES_NAMESPACES


_PLACEHOLDER = Module(
    Just("Python utilities for constructing Python syntax trees"),
    NS,
    DEPENDENCIES,
    (),
)




_def = make_def(_PLACEHOLDER)
_local = make_local("hydra.python.utils")
# Frequently used names
_PY_NAME = Name("hydra.python.syntax.Name")

# Shorthands re-exported from _source_dsl
from hydra.sources.python._source_dsl import py_name as _py_name, py_helper_name as _py_helper_name

# Kernel-side serialization / analysis refs (PyDsl uses no-prefix Hydra functions)
_serialization_print_expr = var("hydra.serialization.printExpr")
_analysis_namespaces_for_definitions = var("hydra.analysis.namespacesForDefinitions")
_pyserde_expression_to_expr = var("hydra.python.serde.expressionToExpr")
_pynames_encode_namespace = var("hydra.python.names.encodeNamespace")


# ----------------------------------------------------------------------
# Definitions (alphabetical)
# ----------------------------------------------------------------------

def _annotated_expression():
    body = Maybes.maybe(
        var("expr"),
        lam(
            "c",
            _local("pyPrimaryToPyExpression")(_local("primaryWithExpressionSlices")(_local("pyNameToPyPrimary")(_py_name("Annotated")), list_([
                        var("expr"),
                        _local("doubleQuotedString")(var("c")),
                    ]))),
        ),
        var("mcomment"),
    )
    return _def(
        "annotatedExpression",
        doc(
            "Annotate an expression with an optional comment using Annotated[]",
            lambdas(["mcomment", "expr"], body),
        ),
    )


def _annotated_statement():
    body = Maybes.maybe(
        var("stmt"),
        lam(
            "c",
            PySyn.statement_annotated(
                PySyn.annotated_statement(var("c"), var("stmt")),
            ),
        ),
        var("mcomment"),
    )
    return _def(
        "annotatedStatement",
        doc(
            "Annotate a statement with an optional comment",
            lambdas(["mcomment", "stmt"], body),
        ),
    )


def _assignment():
    body = _local("pyAssignmentToPyStatement")(PySyn.assignment_untyped(
            PyDsl.untyped_assignment_simple(
                list_([_local("pyNameToPyStarTarget")(var("name"))]),
                var("rhs"),
            ),
        ))
    return _def(
        "assignment",
        doc(
            "Create an assignment statement from name and annotated rhs",
            lambdas(["name", "rhs"], body),
        ),
    )


def _assignment_statement():
    body = _local("assignment")(var("name"), _local("pyExpressionToPyAnnotatedRhs")(var("expr")))
    return _def(
        "assignmentStatement",
        doc(
            "Create an assignment statement from name and expression",
            lambdas(["name", "expr"], body),
        ),
    )


def _cast_to():
    body = _local("functionCall")(_local("pyNameToPyPrimary")(_py_name("cast")), list_([var("pytype"), var("pyexpr")]))
    return _def(
        "castTo",
        doc(
            "Create a cast expression: cast(type, expr)",
            lambdas(["pytype", "pyexpr"], body),
        ),
    )


def _comment_statement():
    body = _local("pyExpressionToPyStatement")(_local("tripleQuotedString")(var("s")))
    return _def(
        "commentStatement",
        doc(
            "Create a comment statement (triple-quoted string)",
            lambdas(["s"], body),
        ),
    )


def _short_circuit_to_nothing(predicates_and_else):
    """Build nested ifElse pred1 nothing $ ifElse pred2 nothing $ ... else_term.

    predicates_and_else: list of TTerm predicates, last element is the else-term TTerm.
    """
    *preds, else_t = predicates_and_else
    out = else_t
    for p in reversed(preds):
        out = Logic.if_else(p, nothing(), out)
    return out


def _decode_py_comparison_to_py_await_primary():
    body = lets(
        [
            field("rhs", PySyn.comparison_rhs(var("c"))),
            field("lhs", PySyn.comparison_lhs(var("c"))),
            field("orLhs", PySyn.bitwise_or_lhs(var("lhs"))),
            field("orRhs", PySyn.bitwise_or_rhs(var("lhs"))),
            field("xorLhs", PySyn.bitwise_xor_lhs(var("orRhs"))),
            field("xorRhs", PySyn.bitwise_xor_rhs(var("orRhs"))),
            field("andLhs", PySyn.bitwise_and_lhs(var("xorRhs"))),
            field("andRhs", PySyn.bitwise_and_rhs(var("xorRhs"))),
            field("shiftLhs",
                project("hydra.python.syntax.ShiftExpression", Name("lhs"))(var("andRhs")),
            ),
            field("shiftRhs", PySyn.shift_expression_rhs(var("andRhs"))),
            field("sumLhs",
                project("hydra.python.syntax.Sum", Name("lhs"))(var("shiftRhs")),
            ),
            field("sumRhs", PySyn.sum_rhs(var("shiftRhs"))),
            field("termLhs",
                project("hydra.python.syntax.Term", Name("lhs"))(var("sumRhs")),
            ),
            field("termRhs", PySyn.term_rhs(var("sumRhs"))),
        ],
        _short_circuit_to_nothing([
            Logic.not_(Lists.null(var("rhs"))),
            Maybes.is_just(var("orLhs")),
            Maybes.is_just(var("xorLhs")),
            Maybes.is_just(var("andLhs")),
            Maybes.is_just(var("shiftLhs")),
            Maybes.is_just(var("sumLhs")),
            Maybes.is_just(var("termLhs")),
            apply(
                match("hydra.python.syntax.Factor",
                    Just(nothing()),
                    [
                        field("simple",
                            lam(
                                "power",
                                _local("decodePyPowerToPyPrimary")(var("power")),
                            ),
                        ),
                    ],
                ),
                var("termRhs"),
            ),
        ]),
    )
    return _def(
        "decodePyComparisonToPyAwaitPrimary",
        doc(
            "Decode a Comparison to a Primary if possible",
            lambdas(["c"], body),
        ),
    )


def _decode_py_conjunction_to_py_primary():
    body = lets(
        [
            field("inversions", PySyn.un_conjunction(var("c"))),
        ],
        Logic.if_else(
            Equality.equal(Lists.length(var("inversions")), int32(1)),
            Maybes.bind(
                Lists.maybe_head(var("inversions")),
                lam("i", _local("decodePyInversionToPyPrimary")(var("i"))),
            ),
            nothing(),
        ),
    )
    return _def(
        "decodePyConjunctionToPyPrimary",
        doc(
            "Decode a Conjunction to a Primary if possible",
            lambdas(["c"], body),
        ),
    )


def _decode_py_expression_to_py_primary():
    body = apply(
        match("hydra.python.syntax.Expression",
            Just(nothing()),
            [
                field("simple",
                    lam(
                        "disj",
                        lets(
                            [
                                field("conjunctions",
                                    PySyn.un_disjunction(var("disj")),
                                ),
                            ],
                            Logic.if_else(
                                Equality.equal(
                                    Lists.length(var("conjunctions")),
                                    int32(1),
                                ),
                                Maybes.bind(
                                    Lists.maybe_head(var("conjunctions")),
                                    lam(
                                        "c2",
                                        _local("decodePyConjunctionToPyPrimary")(var("c2")),
                                    ),
                                ),
                                nothing(),
                            ),
                        ),
                    ),
                ),
            ],
        ),
        var("e"),
    )
    return _def(
        "decodePyExpressionToPyPrimary",
        doc(
            "Decode an Expression to a Primary if possible",
            lambdas(["e"], body),
        ),
    )


def _decode_py_inversion_to_py_primary():
    body = apply(
        match("hydra.python.syntax.Inversion",
            Just(nothing()),
            [
                field("simple",
                    lam(
                        "comparison",
                        _local("decodePyComparisonToPyAwaitPrimary")(var("comparison")),
                    ),
                ),
            ],
        ),
        var("i"),
    )
    return _def(
        "decodePyInversionToPyPrimary",
        doc(
            "Decode an Inversion to a Primary if possible",
            lambdas(["i"], body),
        ),
    )


def _decode_py_power_to_py_primary():
    body = lets(
        [
            field("lhs", PySyn.power_lhs(var("p"))),
            field("await", PySyn.await_primary_await(var("lhs"))),
            field("prim", PySyn.await_primary_primary(var("lhs"))),
        ],
        Logic.if_else(
            var("await"),
            nothing(),
            just(var("prim")),
        ),
    )
    return _def(
        "decodePyPowerToPyPrimary",
        doc(
            "Decode a Power to a Primary if possible",
            lambdas(["p"], body),
        ),
    )


def _dotted_assignment_statement():
    target_def = PySyn.star_target_unstarred(
        inject("hydra.python.syntax.TargetWithStarAtom",
            Name("project"),
            record("hydra.python.syntax.TPrimaryAndName",
                [
                    field("primary",
                        inject("hydra.python.syntax.TPrimary",
                            Name("atom"),
                            inject("hydra.python.syntax.Atom",
                                Name("name"),
                                var("obj"),
                            ),
                        ),
                    ),
                    field("name", var("attr")),
                ],
            ),
        ),
    )
    body = let_chain(
        [("target", target_def)],
        _local("pyAssignmentToPyStatement")(PySyn.assignment_untyped(
                PyDsl.untyped_assignment_simple(
                    list_([var("target")]),
                    _local("pyExpressionToPyAnnotatedRhs")(var("expr")),
                ),
            )),
    )
    return _def(
        "dottedAssignmentStatement",
        doc(
            "Create a dotted assignment statement: obj.attr = expr",
            lambdas(["obj", "attr", "expr"], body),
        ),
    )


def _double_quoted_string():
    body = _local("stringToPyExpression")(PySyn.quote_style_double, var("s"))
    return _def(
        "doubleQuotedString",
        doc(
            "Create a double-quoted string expression",
            lambdas(["s"], body),
        ),
    )


def _find_namespaces():
    body = lets(
        [
            field("coreNs",
                Packaging.namespace(string("hydra.core")),
            ),
            field("namespaces",
                _analysis_namespaces_for_definitions(_pynames_encode_namespace, var("focusNs"), var("defs")),
            ),
        ],
        Logic.if_else(
            Equality.equal(
                Packaging.un_namespace(
                    Pairs.first(Packaging.namespaces_focus(var("namespaces"))),
                ),
                Packaging.un_namespace(var("coreNs")),
            ),
            var("namespaces"),
            Packaging.namespaces(
                Packaging.namespaces_focus(var("namespaces")),
                Maps.insert(
                    var("coreNs"),
                    _pynames_encode_namespace(var("coreNs")),
                    Packaging.namespaces_mapping(var("namespaces")),
                ),
            ),
        ),
    )
    return _def(
        "findNamespaces",
        doc(
            "Find all namespaces referenced by a list of definitions, plus the core namespace",
            lambdas(["focusNs", "defs"], body),
        ),
    )


def _function_call():
    body = _local("pyPrimaryToPyExpression")(_local("primaryWithRhs")(var("func"), PySyn.primary_rhs_call(_local("pyExpressionsToPyArgs")(var("args")))))
    return _def(
        "functionCall",
        doc(
            "Create a function call expression",
            lambdas(["func", "args"], body),
        ),
    )


def _get_item_params():
    body = PySyn.parameters_param_no_default(
        PyDsl.param_no_default_parameters_simple(list_([
            PyDsl.param_no_default_simple(PyDsl.param_simple(_py_name("cls"))),
            PyDsl.param_no_default_simple(PyDsl.param_simple(_py_name("item"))),
        ])),
    )
    return _def("getItemParams", body)


def _indented_block():
    body = lets(
        [
            field("commentGroup",
                Maybes.maybe(
                    list_([]),
                    lam(
                        "s",
                        list_([_local("commentStatement")(var("s"))]),
                    ),
                    var("mcomment"),
                ),
            ),
            field("groups",
                Lists.filter(
                    lam("g", Logic.not_(Lists.null(var("g")))),
                    Lists.cons(var("commentGroup"), var("stmts")),
                ),
            ),
        ],
        Logic.if_else(
            Lists.null(var("groups")),
            PySyn.block_indented(list_([
                list_([
                    PySyn.statement_simple(list_([
                        _local("pyExpressionToPySimpleStatement")(_local("pyAtomToPyExpression")(PySyn.atom_ellipsis)),
                    ])),
                ]),
            ])),
            PySyn.block_indented(var("groups")),
        ),
    )
    return _def(
        "indentedBlock",
        doc(
            "Create an indented block with optional comment",
            lambdas(["mcomment", "stmts"], body),
        ),
    )


def _name_and_params():
    body = _local("primaryAndParams")(_local("pyNameToPyPrimary")(var("pyName")), var("params"))
    return _def(
        "nameAndParams",
        doc(
            "Create a name with parameters",
            lambdas(["pyName", "params"], body),
        ),
    )


def _newtype_statement():
    body = _local("annotatedStatement")(var("mcomment"), _local("assignmentStatement")(var("name"), _local("functionCall")(PyDsl.py_name_to_py_primary(_py_name("NewType")), list_([
                    _local("doubleQuotedString")(PySyn.un_name(var("name"))),
                    var("expr"),
                ]))))
    return _def(
        "newtypeStatement",
        doc(
            "Create a NewType statement",
            lambdas(["name", "mcomment", "expr"], body),
        ),
    )


def _or_expression():
    # Inner recursive 'build' lambda
    build_body = Maybes.maybe(
        # Unreachable fallback
        PySyn.bitwise_or(
            var("prev"),
            _local("pyPrimaryToPyBitwiseXor")(PySyn.primary_simple(PySyn.atom_ellipsis)),
        ),
        lam(
            "p",
            Logic.if_else(
                Lists.null(Pairs.second(var("p"))),
                PySyn.bitwise_or(
                    var("prev"),
                    _local("pyPrimaryToPyBitwiseXor")(Pairs.first(var("p"))),
                ),
                var("build")(just(PySyn.bitwise_or(
                        var("prev"),
                        _local("pyPrimaryToPyBitwiseXor")(Pairs.first(var("p"))),
                    )), Pairs.second(var("p"))),
            ),
        ),
        Lists.uncons(var("ps")),
    )
    body = lets(
        [
            field("build",
                lambdas(["prev", "ps"], build_body),
            ),
        ],
        _local("pyBitwiseOrToPyExpression")(var("build")(nothing(), var("prims"))),
    )
    return _def(
        "orExpression",
        doc(
            "Build an or-expression from multiple primaries",
            lambdas(["prims"], body),
        ),
    )


def _primary_and_params():
    body = _local("pyPrimaryToPyExpression")(_local("primaryWithExpressionSlices")(var("prim"), var("params")))
    return _def(
        "primaryAndParams",
        doc(
            "Create a primary with parameters (subscript)",
            lambdas(["prim", "params"], body),
        ),
    )


def _primary_with_expression_slices():
    body = Maybes.from_maybe(
        var("prim"),
        Maybes.map(
            lam(
                "p",
                _local("primaryWithSlices")(var("prim"), _local("pyExpressionToPySlice")(Pairs.first(var("p"))), Lists.map(
                        lam(
                            "e",
                            PySyn.slice_or_starred_expression_slice(
                                _local("pyExpressionToPySlice")(var("e")),
                            ),
                        ),
                        Pairs.second(var("p")),
                    )),
            ),
            Lists.uncons(var("exprs")),
        ),
    )
    return _def(
        "primaryWithExpressionSlices",
        doc(
            "Create a Primary with expression slices",
            lambdas(["prim", "exprs"], body),
        ),
    )


def _primary_with_rhs():
    body = PySyn.primary_compound(
        PySyn.primary_with_rhs(var("prim"), var("rhs")),
    )
    return _def(
        "primaryWithRhs",
        doc(
            "Combine a Primary with a PrimaryRhs",
            lambdas(["prim", "rhs"], body),
        ),
    )


def _primary_with_slices():
    body = _local("primaryWithRhs")(var("prim"), PySyn.primary_rhs_slices(PySyn.slices(var("first"), var("rest"))))
    return _def(
        "primaryWithSlices",
        doc(
            "Create a Primary with slices",
            lambdas(["prim", "first", "rest"], body),
        ),
    )


def _project_from_expression():
    body = lets(
        [
            field("prim",
                PySyn.primary_simple(PySyn.atom_group(
                    PySyn.group_expression(PySyn.named_expression_simple(var("exp"))),
                )),
            ),
        ],
        _local("pyPrimaryToPyExpression")(PySyn.primary_compound(
                PySyn.primary_with_rhs(
                    var("prim"),
                    PySyn.primary_rhs_project(var("name")),
                ),
            )),
    )
    return _def(
        "projectFromExpression",
        doc(
            "Project a field from an expression",
            lambdas(["exp", "name"], body),
        ),
    )


def _py_assignment_to_py_statement():
    body = _local("pySimpleStatementToPyStatement")(PySyn.simple_statement_assignment(var("a")))
    return _def(
        "pyAssignmentToPyStatement",
        doc(
            "Convert an Assignment to a Statement",
            lambdas(["a"], body),
        ),
    )


def _py_atom_to_py_expression():
    body = _local("pyPrimaryToPyExpression")(PySyn.primary_simple(var("atom")))
    return _def(
        "pyAtomToPyExpression",
        doc(
            "Convert an Atom to an Expression",
            lambdas(["atom"], body),
        ),
    )


def _py_bitwise_or_to_py_conjunction():
    body = PySyn.conjunction(list_([
        PySyn.inversion_simple(
            PySyn.comparison(var("bor"), list_([])),
        ),
    ]))
    return _def(
        "pyBitwiseOrToPyConjunction",
        doc(
            "Convert a BitwiseOr to a Conjunction",
            lambdas(["bor"], body),
        ),
    )


def _py_bitwise_or_to_py_expression():
    body = _local("pyConjunctionToPyExpression")(_local("pyBitwiseOrToPyConjunction")(var("bor")))
    return _def(
        "pyBitwiseOrToPyExpression",
        doc(
            "Convert a BitwiseOr to an Expression",
            lambdas(["bor"], body),
        ),
    )


def _py_class_definition_to_py_statement():
    body = PySyn.statement_compound(PySyn.compound_statement_class_def(var("cd")))
    return _def(
        "pyClassDefinitionToPyStatement",
        doc(
            "Convert a ClassDefinition to a Statement",
            lambdas(["cd"], body),
        ),
    )


def _py_closed_pattern_to_py_patterns():
    body = PySyn.patterns_pattern(
        PySyn.pattern_or(PySyn.or_pattern(list_([var("p")]))),
    )
    return _def(
        "pyClosedPatternToPyPatterns",
        doc(
            "Convert a ClosedPattern to Patterns",
            lambdas(["p"], body),
        ),
    )


def _py_conjunction_to_py_expression():
    body = PySyn.expression_simple(
        PySyn.disjunction(list_([var("conj")])),
    )
    return _def(
        "pyConjunctionToPyExpression",
        doc(
            "Convert a Conjunction to an Expression",
            lambdas(["conj"], body),
        ),
    )


def _py_expression_to_bitwise_or():
    body = PyDsl.py_primary_to_py_bitwise_or(
        PySyn.primary_simple(
            PySyn.atom_group(
                PySyn.group_expression(PySyn.named_expression_simple(var("e"))),
            ),
        ),
    )
    return _def(
        "pyExpressionToBitwiseOr",
        doc(
            "Convert an Expression to a BitwiseOr, wrapping in parens if needed",
            lambdas(["e"], body),
        ),
    )


def _py_expression_to_disjunction():
    body = cases_with_default("hydra.python.syntax.Expression", var("e"), PySyn.disjunction(list_([
            _local("pyPrimaryToPyConjunction")(PySyn.primary_simple(
                    PySyn.atom_group(
                        PySyn.group_expression(PySyn.named_expression_simple(var("e"))),
                    ),
                )),
        ])),
            field("simple",
                lam("disj", var("disj")),
            ))
    return _def(
        "pyExpressionToDisjunction",
        doc(
            "Convert an Expression to a Disjunction, wrapping in parens if needed",
            lambdas(["e"], body),
        ),
    )


def _py_expression_to_py_annotated_rhs():
    body = PySyn.annotated_rhs_star(
        list_([PySyn.star_expression_simple(var("expr"))]),
    )
    return _def(
        "pyExpressionToPyAnnotatedRhs",
        doc(
            "Convert an Expression to an AnnotatedRhs",
            lambdas(["expr"], body),
        ),
    )


def _py_expression_to_py_primary():
    body = Maybes.maybe(
        PySyn.primary_simple(
            PySyn.atom_group(
                PySyn.group_expression(PySyn.named_expression_simple(var("e"))),
            ),
        ),
        lam("prim", var("prim")),
        _local("decodePyExpressionToPyPrimary")(var("e")),
    )
    return _def(
        "pyExpressionToPyPrimary",
        doc(
            "Extracts the primary from an expression, or wraps it in parentheses if the expression does not contain a primary",
            lambdas(["e"], body),
        ),
    )


def _py_expression_to_py_simple_statement():
    body = PySyn.simple_statement_star_expressions(
        list_([PySyn.star_expression_simple(var("expr"))]),
    )
    return _def(
        "pyExpressionToPySimpleStatement",
        doc(
            "Convert an Expression to a SimpleStatement (as star expressions)",
            lambdas(["expr"], body),
        ),
    )


def _py_expression_to_py_slice():
    body = PySyn.slice_named(PySyn.named_expression_simple(var("expr")))
    return _def(
        "pyExpressionToPySlice",
        doc(
            "Convert an Expression to a Slice",
            lambdas(["expr"], body),
        ),
    )


def _py_expression_to_py_star_named_expression():
    body = PySyn.star_named_expression_simple(
        PySyn.named_expression_simple(var("expr")),
    )
    return _def(
        "pyExpressionToPyStarNamedExpression",
        doc(
            "Convert an Expression to a StarNamedExpression",
            lambdas(["expr"], body),
        ),
    )


def _py_expression_to_py_statement():
    body = _local("pySimpleStatementToPyStatement")(_local("pyExpressionToPySimpleStatement")(var("expr")))
    return _def(
        "pyExpressionToPyStatement",
        doc(
            "Convert an Expression to a Statement",
            lambdas(["expr"], body),
        ),
    )


def _py_expressions_to_py_args():
    body = PyDsl.args_positional_only(
        Lists.map(
            lam("e", PySyn.pos_arg_expression(var("e"))),
            var("exprs"),
        ),
    )
    return _def(
        "pyExpressionsToPyArgs",
        doc(
            "Convert a list of Expressions to Args",
            lambdas(["exprs"], body),
        ),
    )


def _py_list():
    body = PyDsl.py_list(
        Lists.map(_local("pyExpressionToPyStarNamedExpression"), var("exprs")),
    )
    return _def(
        "pyList",
        doc(
            "Create a Python list from expressions",
            lambdas(["exprs"], body),
        ),
    )


def _py_name_to_py_expression():
    body = _local("pyPrimaryToPyExpression")(_local("pyNameToPyPrimary")(var("name")))
    return _def(
        "pyNameToPyExpression",
        doc(
            "Convert a Name to an Expression",
            lambdas(["name"], body),
        ),
    )


def _py_name_to_py_named_expression():
    body = PySyn.named_expression_simple(
        _local("pyNameToPyExpression")(var("name")),
    )
    return _def(
        "pyNameToPyNamedExpression",
        doc(
            "Convert a Name to a NamedExpression",
            lambdas(["name"], body),
        ),
    )


def _py_name_to_py_primary():
    body = PySyn.primary_simple(PySyn.atom_name(var("name")))
    return _def(
        "pyNameToPyPrimary",
        doc(
            "Convert a Name to a Primary (simple atom)",
            lambdas(["name"], body),
        ),
    )


def _py_name_to_py_star_target():
    body = PySyn.star_target_unstarred(
        PySyn.target_with_star_atom_atom(PySyn.star_atom_name(var("name"))),
    )
    return _def(
        "pyNameToPyStarTarget",
        doc(
            "Convert a Name to a StarTarget",
            lambdas(["name"], body),
        ),
    )


def _py_name_to_py_type_parameter():
    body = PySyn.type_parameter_simple(
        PyDsl.simple_type_parameter_simple(var("name")),
    )
    return _def(
        "pyNameToPyTypeParameter",
        doc(
            "Convert a Name to a TypeParameter",
            lambdas(["name"], body),
        ),
    )


def _py_none():
    return _def(
        "pyNone",
        doc(
            "The Python None value as a Name",
            _py_name("None"),
        ),
    )


def _py_primary_to_py_bitwise_or():
    body = PyDsl.py_primary_to_py_bitwise_or(var("prim"))
    return _def(
        "pyPrimaryToPyBitwiseOr",
        doc(
            "Convert a Primary to a BitwiseOr",
            lambdas(["prim"], body),
        ),
    )


def _py_primary_to_py_bitwise_xor():
    body = PyDsl.py_primary_to_py_bitwise_xor(var("prim"))
    return _def(
        "pyPrimaryToPyBitwiseXor",
        doc(
            "Convert a Primary to a BitwiseXor",
            lambdas(["prim"], body),
        ),
    )


def _py_primary_to_py_conjunction():
    body = _local("pyBitwiseOrToPyConjunction")(_local("pyPrimaryToPyBitwiseOr")(var("prim")))
    return _def(
        "pyPrimaryToPyConjunction",
        doc(
            "Convert a Primary to a Conjunction",
            lambdas(["prim"], body),
        ),
    )


def _py_primary_to_py_expression():
    body = _local("pyConjunctionToPyExpression")(_local("pyPrimaryToPyConjunction")(var("prim")))
    return _def(
        "pyPrimaryToPyExpression",
        doc(
            "Convert a Primary to an Expression",
            lambdas(["prim"], body),
        ),
    )


def _py_primary_to_py_slice():
    body = _local("pyExpressionToPySlice")(_local("pyPrimaryToPyExpression")(var("prim")))
    return _def(
        "pyPrimaryToPySlice",
        doc(
            "Convert a Primary to a Slice",
            lambdas(["prim"], body),
        ),
    )


def _py_simple_statement_to_py_statement():
    body = PySyn.statement_simple(list_([var("s")]))
    return _def(
        "pySimpleStatementToPyStatement",
        doc(
            "Convert a SimpleStatement to a Statement",
            lambdas(["s"], body),
        ),
    )


def _raise_assertion_error():
    body = _local("pySimpleStatementToPyStatement")(PySyn.simple_statement_raise(
            PySyn.raise_statement(just(
                PySyn.raise_expression(
                    _local("functionCall")(PyDsl.py_name_to_py_primary(_py_name("AssertionError")), list_([_local("doubleQuotedString")(var("msg"))])),
                    nothing(),
                ),
            )),
        ))
    return _def(
        "raiseAssertionError",
        doc(
            "Create a raise AssertionError statement",
            lambdas(["msg"], body),
        ),
    )


def _raise_type_error():
    body = _local("pySimpleStatementToPyStatement")(PySyn.simple_statement_raise(
            PySyn.raise_statement(just(
                PySyn.raise_expression(
                    _local("functionCall")(PyDsl.py_name_to_py_primary(_py_name("TypeError")), list_([_local("doubleQuotedString")(var("msg"))])),
                    nothing(),
                ),
            )),
        ))
    return _def(
        "raiseTypeError",
        doc(
            "Create a raise TypeError statement",
            lambdas(["msg"], body),
        ),
    )


def _return_single():
    body = _local("pySimpleStatementToPyStatement")(PySyn.simple_statement_return(
            PySyn.return_statement(list_([
                PySyn.star_expression_simple(var("expr")),
            ])),
        ))
    return _def(
        "returnSingle",
        doc(
            "Create a return statement with a single expression",
            lambdas(["expr"], body),
        ),
    )


def _self_only_params():
    body = PySyn.parameters_param_no_default(
        PyDsl.param_no_default_parameters_simple(list_([
            PyDsl.param_no_default_simple(PyDsl.param_simple(_py_name("self"))),
        ])),
    )
    return _def("selfOnlyParams", body)


def _self_other_params():
    body = PySyn.parameters_param_no_default(
        PyDsl.param_no_default_parameters_simple(list_([
            PyDsl.param_no_default_simple(PyDsl.param_simple(_py_name("self"))),
            PyDsl.param_no_default_simple(PyDsl.param_simple(_py_name("other"))),
        ])),
    )
    return _def("selfOtherParams", body)


def _single_quoted_string():
    body = _local("stringToPyExpression")(PySyn.quote_style_single, var("s"))
    return _def(
        "singleQuotedString",
        doc(
            "Create a single-quoted string expression",
            lambdas(["s"], body),
        ),
    )


def _string_to_py_expression():
    body = _local("pyAtomToPyExpression")(PySyn.atom_string(PyDsl.string_(var("s"), var("style"))))
    return _def(
        "stringToPyExpression",
        doc(
            "Create a string expression with a given quote style",
            lambdas(["style", "s"], body),
        ),
    )


def _target_python_version():
    body = inject_unit("hydra.python.environment.PythonVersion",
        Name("python310"),
    )
    return _def(
        "targetPythonVersion",
        doc(
            "Current target Python version for code generation",
            body,
        ),
    )


def _triple_quoted_string():
    body = _local("stringToPyExpression")(PySyn.quote_style_triple, var("s"))
    return _def(
        "tripleQuotedString",
        doc(
            "Create a triple-quoted string expression",
            lambdas(["s"], body),
        ),
    )


def _type_alias_statement():
    body = _local("annotatedStatement")(var("mcomment"), _local("pySimpleStatementToPyStatement")(PySyn.simple_statement_type_alias(
                PySyn.type_alias(
                    var("name"),
                    var("tparams"),
                    var("tyexpr"),
                ),
            )))
    return _def(
        "typeAliasStatement",
        doc(
            "Generate a type alias statement using PEP 695 syntax (Python 3.12+)",
            lambdas(["name", "tparams", "mcomment", "tyexpr"], body),
        ),
    )


def _type_alias_statement_310():
    inner = lets(
        [
            field("quotedExpr",
                _local("doubleQuotedString")(_serialization_print_expr(_pyserde_expression_to_expr(var("tyexpr")))),
            ),
        ],
        _local("annotatedStatement")(var("mcomment"), _local("pyAssignmentToPyStatement")(PySyn.assignment_typed(
                    PySyn.typed_assignment(
                        PySyn.single_target_name(var("name")),
                        PyDsl.py_name_to_py_expression(_py_name("TypeAlias")),
                        just(
                            _local("pyExpressionToPyAnnotatedRhs")(var("quotedExpr")),
                        ),
                    ),
                ))),
    )
    return _def(
        "typeAliasStatement310",
        doc(
            "Generate a type alias statement using Python 3.10-compatible syntax: Name: TypeAlias = \"TypeExpression\"",
            lambdas(["name", "_tparams", "mcomment", "tyexpr"], inner),
        ),
    )


def _union_type_class_statements_310():
    # Build the body: lots of nested let bindings, finally returns list of 2 statements.
    return_object_def = _local("pySimpleStatementToPyStatement")(PySyn.simple_statement_return(
            PySyn.return_statement(list_([
                PySyn.star_expression_simple(
                    PyDsl.py_name_to_py_expression(_py_name("object")),
                ),
            ])),
        ))
    get_item_method_def = PySyn.statement_compound(
        PySyn.compound_statement_function(
            PySyn.function_definition(
                nothing(),
                PySyn.function_def_raw(
                    false(),
                    _py_name("__getitem__"),
                    list_([]),
                    just(_local("getItemParams")),
                    nothing(),
                    nothing(),
                    _local("indentedBlock")(nothing(), list_([
                            list_([var("returnObject")]),
                        ])),
                ),
            ),
        ),
    )
    meta_class_def = _local("pyClassDefinitionToPyStatement")(PySyn.class_definition(
            nothing(),
            var("metaName"),
            list_([]),
            just(_local("pyExpressionsToPyArgs")(list_([PyDsl.py_name_to_py_expression(_py_name("type"))]))),
            _local("indentedBlock")(nothing(), list_([list_([var("getItemMethod")])])),
        ))
    doc_stmt_def = _local("pyExpressionToPyStatement")(_local("tripleQuotedString")(var("docString")))
    body_groups_def = Logic.if_else(
        Lists.null(var("extraStmts")),
        let_chain(
            [("passStmt", _local("pySimpleStatementToPyStatement")(PySyn.simple_statement_pass))],
            list_([
                list_([var("docStmt")]),
                list_([var("passStmt")]),
            ]),
        ),
        list_([
            list_([var("docStmt")]),
            var("extraStmts"),
        ]),
    )
    metaclass_arg_def = PySyn.kwarg(
        _py_name("metaclass"),
        PyDsl.py_name_to_py_expression(var("metaName")),
    )
    union_class_def = _local("annotatedStatement")(var("mcomment"), _local("pyClassDefinitionToPyStatement")(PySyn.class_definition(
                nothing(),
                var("name"),
                list_([]),
                just(PySyn.args(
                    list_([]),
                    list_([PySyn.kwarg_or_starred_kwarg(var("metaclassArg"))]),
                    list_([]),
                )),
                _local("indentedBlock")(nothing(), var("bodyGroups")),
            )))
    body = let_chain(
        [
            ("nameStr", PySyn.un_name(var("name"))),
            ("metaName", _py_name_concat3("_", "nameStr", "Meta")),
            ("docString", _serialization_print_expr(_pyserde_expression_to_expr(var("tyexpr")))),
            ("returnObject", return_object_def),
            ("getItemMethod", get_item_method_def),
            ("metaClass", meta_class_def),
            ("docStmt", doc_stmt_def),
            ("bodyGroups", body_groups_def),
            ("metaclassArg", metaclass_arg_def),
            ("unionClass", union_class_def),
        ],
        list_([
            var("metaClass"),
            var("unionClass"),
        ]),
    )
    return _def(
        "unionTypeClassStatements310",
        doc(
            "Generate a subscriptable union class for Python 3.10",
            lambdas(["name", "mcomment", "tyexpr", "extraStmts"], body),
        ),
    )


def _py_name_concat3(prefix_str, var_name, suffix_str):
    """Helper for: PyDsl.name $ string prefix ++ var "x" ++ string suffix.

    Haskell ++ is left-associative: (("_" ++ nameStr) ++ "Meta").
    """
    return PySyn.name(Strings.cat2(
        Strings.cat2(string(prefix_str), var(var_name)),
        string(suffix_str),
    ))


def _unit_variant_methods():
    slots_stmt_def = _local("assignmentStatement")(_py_name("__slots__"), _local("pyPrimaryToPyExpression")(PySyn.primary_simple(PySyn.atom_tuple(PySyn.tuple(list_([]))))))
    return_isinstance_def = _local("pySimpleStatementToPyStatement")(PySyn.simple_statement_return(
            PySyn.return_statement(list_([
                PySyn.star_expression_simple(
                    _local("functionCall")(PyDsl.py_name_to_py_primary(_py_name("isinstance")), list_([
                            PyDsl.py_name_to_py_expression(_py_name("other")),
                            PyDsl.py_name_to_py_expression(var("className")),
                        ])),
                ),
            ])),
        ))
    eq_method_def = PySyn.statement_compound(
        PySyn.compound_statement_function(
            PySyn.function_definition(
                nothing(),
                PySyn.function_def_raw(
                    false(),
                    _py_name("__eq__"),
                    list_([]),
                    just(_local("selfOtherParams")),
                    nothing(),
                    nothing(),
                    _local("indentedBlock")(nothing(), list_([list_([var("returnIsinstance")])])),
                ),
            ),
        ),
    )
    return_hash_def = _local("pySimpleStatementToPyStatement")(PySyn.simple_statement_return(
            PySyn.return_statement(list_([
                PySyn.star_expression_simple(
                    _local("functionCall")(PyDsl.py_name_to_py_primary(_py_name("hash")), list_([_local("doubleQuotedString")(var("classNameStr"))])),
                ),
            ])),
        ))
    hash_method_def = PySyn.statement_compound(
        PySyn.compound_statement_function(
            PySyn.function_definition(
                nothing(),
                PySyn.function_def_raw(
                    false(),
                    _py_name("__hash__"),
                    list_([]),
                    just(_local("selfOnlyParams")),
                    nothing(),
                    nothing(),
                    _local("indentedBlock")(nothing(), list_([list_([var("returnHash")])])),
                ),
            ),
        ),
    )
    body = let_chain(
        [
            ("classNameStr", PySyn.un_name(var("className"))),
            ("slotsStmt", slots_stmt_def),
            ("returnIsinstance", return_isinstance_def),
            ("eqMethod", eq_method_def),
            ("returnHash", return_hash_def),
            ("hashMethod", hash_method_def),
        ],
        list_([
            var("slotsStmt"),
            var("eqMethod"),
            var("hashMethod"),
        ]),
    )
    return _def(
        "unitVariantMethods",
        doc(
            "Generate __slots__, __eq__, and __hash__ methods for unit-typed union variants",
            lambdas(["className"], body),
        ),
    )


def _build_module() -> Module:
    return Module(
        _PLACEHOLDER.description,
        _PLACEHOLDER.namespace,
        _PLACEHOLDER.dependencies,
        (
            to_definition(_annotated_expression()),
            to_definition(_annotated_statement()),
            to_definition(_assignment()),
            to_definition(_assignment_statement()),
            to_definition(_cast_to()),
            to_definition(_comment_statement()),
            to_definition(_decode_py_comparison_to_py_await_primary()),
            to_definition(_decode_py_conjunction_to_py_primary()),
            to_definition(_decode_py_expression_to_py_primary()),
            to_definition(_decode_py_inversion_to_py_primary()),
            to_definition(_decode_py_power_to_py_primary()),
            to_definition(_dotted_assignment_statement()),
            to_definition(_double_quoted_string()),
            to_definition(_find_namespaces()),
            to_definition(_function_call()),
            to_definition(_get_item_params()),
            to_definition(_indented_block()),
            to_definition(_name_and_params()),
            to_definition(_newtype_statement()),
            to_definition(_or_expression()),
            to_definition(_primary_and_params()),
            to_definition(_primary_with_expression_slices()),
            to_definition(_primary_with_rhs()),
            to_definition(_primary_with_slices()),
            to_definition(_project_from_expression()),
            to_definition(_py_assignment_to_py_statement()),
            to_definition(_py_atom_to_py_expression()),
            to_definition(_py_bitwise_or_to_py_conjunction()),
            to_definition(_py_bitwise_or_to_py_expression()),
            to_definition(_py_class_definition_to_py_statement()),
            to_definition(_py_closed_pattern_to_py_patterns()),
            to_definition(_py_conjunction_to_py_expression()),
            to_definition(_py_expression_to_bitwise_or()),
            to_definition(_py_expression_to_disjunction()),
            to_definition(_py_expression_to_py_annotated_rhs()),
            to_definition(_py_expression_to_py_primary()),
            to_definition(_py_expression_to_py_simple_statement()),
            to_definition(_py_expression_to_py_slice()),
            to_definition(_py_expression_to_py_star_named_expression()),
            to_definition(_py_expression_to_py_statement()),
            to_definition(_py_expressions_to_py_args()),
            to_definition(_py_list()),
            to_definition(_py_name_to_py_expression()),
            to_definition(_py_name_to_py_named_expression()),
            to_definition(_py_name_to_py_primary()),
            to_definition(_py_name_to_py_star_target()),
            to_definition(_py_name_to_py_type_parameter()),
            to_definition(_py_none()),
            to_definition(_py_primary_to_py_bitwise_or()),
            to_definition(_py_primary_to_py_bitwise_xor()),
            to_definition(_py_primary_to_py_conjunction()),
            to_definition(_py_primary_to_py_expression()),
            to_definition(_py_primary_to_py_slice()),
            to_definition(_py_simple_statement_to_py_statement()),
            to_definition(_raise_assertion_error()),
            to_definition(_raise_type_error()),
            to_definition(_return_single()),
            to_definition(_self_only_params()),
            to_definition(_self_other_params()),
            to_definition(_single_quoted_string()),
            to_definition(_string_to_py_expression()),
            to_definition(_target_python_version()),
            to_definition(_triple_quoted_string()),
            to_definition(_type_alias_statement()),
            to_definition(_type_alias_statement_310()),
            to_definition(_union_type_class_statements_310()),
            to_definition(_unit_variant_methods()),
        ),
    )


module_ = _build_module()
