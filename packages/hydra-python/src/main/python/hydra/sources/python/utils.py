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
import hydra.dsl.meta.phantoms as Phantoms
import hydra.dsl.packaging as Packaging
import hydra.dsl.python.syntax as PySyn

from hydra.sources.python import _python_helpers as PyDsl


# ----------------------------------------------------------------------
# Module setup
# ----------------------------------------------------------------------

NS = Namespace("hydra.python.utils")

KERNEL_TYPES_NAMESPACES = [
    Namespace(n) for n in [
        "hydra.paths", "hydra.ast", "hydra.classes", "hydra.coders",
        "hydra.context", "hydra.core", "hydra.error.checking", "hydra.error.core",
        "hydra.error.packaging", "hydra.errors", "hydra.graph", "hydra.json.model",
        "hydra.packaging", "hydra.parsing", "hydra.phantoms", "hydra.query",
        "hydra.relational", "hydra.tabular", "hydra.testing", "hydra.topology",
        "hydra.typing", "hydra.util", "hydra.variants",
    ]
]

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


def _def(local_name, term):
    return Phantoms.definition_in_module(_PLACEHOLDER, local_name, term)


def _local(local_name: str):
    return Phantoms.var(f"hydra.python.utils.{local_name}")


def _ap(fun, *args):
    out = fun
    for a in args:
        out = Phantoms.apply(out, a)
    return out


# Frequently used names
_PY_NAME = Name("hydra.python.syntax.Name")


def _py_name(s):
    """PyDsl.name $ string s — wrap a string into a Py.Name."""
    return PySyn.name(Phantoms.string(s))


def _py_helper_name(s):
    """Phantoms.var('hydra.python.names.<s>') — reference a Names sibling def."""
    return Phantoms.var(f"hydra.python.names.{s}")


# Kernel-side serialization / analysis refs (PyDsl uses no-prefix Hydra functions)
_serialization_print_expr = Phantoms.var("hydra.serialization.printExpr")
_analysis_namespaces_for_definitions = Phantoms.var("hydra.analysis.namespacesForDefinitions")
_pyserde_expression_to_expr = Phantoms.var("hydra.python.serde.expressionToExpr")
_pynames_encode_namespace = Phantoms.var("hydra.python.names.encodeNamespace")


def _let_chain(bindings, body):
    """Build chained single-binding lets: let1 n1 v1 (let1 n2 v2 (... body)).

    bindings: list of (name_str, value_TTerm) pairs.
    Mirrors Haskell's chained '<~' operator (which is let1 under the hood).
    """
    out = body
    for name, val in reversed(bindings):
        out = Phantoms.let1(name, val, out)
    return out


# ----------------------------------------------------------------------
# Definitions (alphabetical)
# ----------------------------------------------------------------------

def _annotated_expression():
    body = Maybes.maybe(
        Phantoms.var("expr"),
        Phantoms.lam(
            "c",
            _ap(
                _local("pyPrimaryToPyExpression"),
                _ap(
                    _local("primaryWithExpressionSlices"),
                    _ap(_local("pyNameToPyPrimary"), _py_name("Annotated")),
                    Phantoms.list_([
                        Phantoms.var("expr"),
                        _ap(_local("doubleQuotedString"), Phantoms.var("c")),
                    ]),
                ),
            ),
        ),
        Phantoms.var("mcomment"),
    )
    return _def(
        "annotatedExpression",
        Phantoms.doc(
            "Annotate an expression with an optional comment using Annotated[]",
            Phantoms.lambdas(["mcomment", "expr"], body),
        ),
    )


def _annotated_statement():
    body = Maybes.maybe(
        Phantoms.var("stmt"),
        Phantoms.lam(
            "c",
            PySyn.statement_annotated(
                PySyn.annotated_statement(Phantoms.var("c"), Phantoms.var("stmt")),
            ),
        ),
        Phantoms.var("mcomment"),
    )
    return _def(
        "annotatedStatement",
        Phantoms.doc(
            "Annotate a statement with an optional comment",
            Phantoms.lambdas(["mcomment", "stmt"], body),
        ),
    )


def _assignment():
    body = _ap(
        _local("pyAssignmentToPyStatement"),
        PySyn.assignment_untyped(
            PyDsl.untyped_assignment_simple(
                Phantoms.list_([_ap(_local("pyNameToPyStarTarget"), Phantoms.var("name"))]),
                Phantoms.var("rhs"),
            ),
        ),
    )
    return _def(
        "assignment",
        Phantoms.doc(
            "Create an assignment statement from name and annotated rhs",
            Phantoms.lambdas(["name", "rhs"], body),
        ),
    )


def _assignment_statement():
    body = _ap(
        _local("assignment"),
        Phantoms.var("name"),
        _ap(_local("pyExpressionToPyAnnotatedRhs"), Phantoms.var("expr")),
    )
    return _def(
        "assignmentStatement",
        Phantoms.doc(
            "Create an assignment statement from name and expression",
            Phantoms.lambdas(["name", "expr"], body),
        ),
    )


def _cast_to():
    body = _ap(
        _local("functionCall"),
        _ap(_local("pyNameToPyPrimary"), _py_name("cast")),
        Phantoms.list_([Phantoms.var("pytype"), Phantoms.var("pyexpr")]),
    )
    return _def(
        "castTo",
        Phantoms.doc(
            "Create a cast expression: cast(type, expr)",
            Phantoms.lambdas(["pytype", "pyexpr"], body),
        ),
    )


def _comment_statement():
    body = _ap(
        _local("pyExpressionToPyStatement"),
        _ap(_local("tripleQuotedString"), Phantoms.var("s")),
    )
    return _def(
        "commentStatement",
        Phantoms.doc(
            "Create a comment statement (triple-quoted string)",
            Phantoms.lambdas(["s"], body),
        ),
    )


def _short_circuit_to_nothing(predicates_and_else):
    """Build nested ifElse pred1 nothing $ ifElse pred2 nothing $ ... else_term.

    predicates_and_else: list of TTerm predicates, last element is the else-term TTerm.
    """
    *preds, else_t = predicates_and_else
    out = else_t
    for p in reversed(preds):
        out = Logic.if_else(p, Phantoms.nothing(), out)
    return out


def _decode_py_comparison_to_py_await_primary():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("rhs"), PySyn.comparison_rhs(Phantoms.var("c"))),
            Phantoms.field(Name("lhs"), PySyn.comparison_lhs(Phantoms.var("c"))),
            Phantoms.field(Name("orLhs"), PySyn.bitwise_or_lhs(Phantoms.var("lhs"))),
            Phantoms.field(Name("orRhs"), PySyn.bitwise_or_rhs(Phantoms.var("lhs"))),
            Phantoms.field(Name("xorLhs"), PySyn.bitwise_xor_lhs(Phantoms.var("orRhs"))),
            Phantoms.field(Name("xorRhs"), PySyn.bitwise_xor_rhs(Phantoms.var("orRhs"))),
            Phantoms.field(Name("andLhs"), PySyn.bitwise_and_lhs(Phantoms.var("xorRhs"))),
            Phantoms.field(Name("andRhs"), PySyn.bitwise_and_rhs(Phantoms.var("xorRhs"))),
            Phantoms.field(
                Name("shiftLhs"),
                _ap(
                    Phantoms.project(Name("hydra.python.syntax.ShiftExpression"), Name("lhs")),
                    Phantoms.var("andRhs"),
                ),
            ),
            Phantoms.field(Name("shiftRhs"), PySyn.shift_expression_rhs(Phantoms.var("andRhs"))),
            Phantoms.field(
                Name("sumLhs"),
                _ap(
                    Phantoms.project(Name("hydra.python.syntax.Sum"), Name("lhs")),
                    Phantoms.var("shiftRhs"),
                ),
            ),
            Phantoms.field(Name("sumRhs"), PySyn.sum_rhs(Phantoms.var("shiftRhs"))),
            Phantoms.field(
                Name("termLhs"),
                _ap(
                    Phantoms.project(Name("hydra.python.syntax.Term"), Name("lhs")),
                    Phantoms.var("sumRhs"),
                ),
            ),
            Phantoms.field(Name("termRhs"), PySyn.term_rhs(Phantoms.var("sumRhs"))),
        ],
        _short_circuit_to_nothing([
            Logic.not_(Lists.null(Phantoms.var("rhs"))),
            Maybes.is_just(Phantoms.var("orLhs")),
            Maybes.is_just(Phantoms.var("xorLhs")),
            Maybes.is_just(Phantoms.var("andLhs")),
            Maybes.is_just(Phantoms.var("shiftLhs")),
            Maybes.is_just(Phantoms.var("sumLhs")),
            Maybes.is_just(Phantoms.var("termLhs")),
            Phantoms.apply(
                Phantoms.match(
                    Name("hydra.python.syntax.Factor"),
                    Just(Phantoms.nothing()),
                    [
                        Phantoms.field(
                            Name("simple"),
                            Phantoms.lam(
                                "power",
                                _ap(_local("decodePyPowerToPyPrimary"), Phantoms.var("power")),
                            ),
                        ),
                    ],
                ),
                Phantoms.var("termRhs"),
            ),
        ]),
    )
    return _def(
        "decodePyComparisonToPyAwaitPrimary",
        Phantoms.doc(
            "Decode a Comparison to a Primary if possible",
            Phantoms.lambdas(["c"], body),
        ),
    )


def _decode_py_conjunction_to_py_primary():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("inversions"), PySyn.un_conjunction(Phantoms.var("c"))),
        ],
        Logic.if_else(
            Equality.equal(Lists.length(Phantoms.var("inversions")), Phantoms.int32(1)),
            Maybes.bind(
                Lists.maybe_head(Phantoms.var("inversions")),
                Phantoms.lam("i", _ap(_local("decodePyInversionToPyPrimary"), Phantoms.var("i"))),
            ),
            Phantoms.nothing(),
        ),
    )
    return _def(
        "decodePyConjunctionToPyPrimary",
        Phantoms.doc(
            "Decode a Conjunction to a Primary if possible",
            Phantoms.lambdas(["c"], body),
        ),
    )


def _decode_py_expression_to_py_primary():
    body = Phantoms.apply(
        Phantoms.match(
            Name("hydra.python.syntax.Expression"),
            Just(Phantoms.nothing()),
            [
                Phantoms.field(
                    Name("simple"),
                    Phantoms.lam(
                        "disj",
                        Phantoms.lets(
                            [
                                Phantoms.field(
                                    Name("conjunctions"),
                                    PySyn.un_disjunction(Phantoms.var("disj")),
                                ),
                            ],
                            Logic.if_else(
                                Equality.equal(
                                    Lists.length(Phantoms.var("conjunctions")),
                                    Phantoms.int32(1),
                                ),
                                Maybes.bind(
                                    Lists.maybe_head(Phantoms.var("conjunctions")),
                                    Phantoms.lam(
                                        "c2",
                                        _ap(
                                            _local("decodePyConjunctionToPyPrimary"),
                                            Phantoms.var("c2"),
                                        ),
                                    ),
                                ),
                                Phantoms.nothing(),
                            ),
                        ),
                    ),
                ),
            ],
        ),
        Phantoms.var("e"),
    )
    return _def(
        "decodePyExpressionToPyPrimary",
        Phantoms.doc(
            "Decode an Expression to a Primary if possible",
            Phantoms.lambdas(["e"], body),
        ),
    )


def _decode_py_inversion_to_py_primary():
    body = Phantoms.apply(
        Phantoms.match(
            Name("hydra.python.syntax.Inversion"),
            Just(Phantoms.nothing()),
            [
                Phantoms.field(
                    Name("simple"),
                    Phantoms.lam(
                        "comparison",
                        _ap(
                            _local("decodePyComparisonToPyAwaitPrimary"),
                            Phantoms.var("comparison"),
                        ),
                    ),
                ),
            ],
        ),
        Phantoms.var("i"),
    )
    return _def(
        "decodePyInversionToPyPrimary",
        Phantoms.doc(
            "Decode an Inversion to a Primary if possible",
            Phantoms.lambdas(["i"], body),
        ),
    )


def _decode_py_power_to_py_primary():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("lhs"), PySyn.power_lhs(Phantoms.var("p"))),
            Phantoms.field(Name("await"), PySyn.await_primary_await(Phantoms.var("lhs"))),
            Phantoms.field(Name("prim"), PySyn.await_primary_primary(Phantoms.var("lhs"))),
        ],
        Logic.if_else(
            Phantoms.var("await"),
            Phantoms.nothing(),
            Phantoms.just(Phantoms.var("prim")),
        ),
    )
    return _def(
        "decodePyPowerToPyPrimary",
        Phantoms.doc(
            "Decode a Power to a Primary if possible",
            Phantoms.lambdas(["p"], body),
        ),
    )


def _dotted_assignment_statement():
    target_def = PySyn.star_target_unstarred(
        Phantoms.inject(
            Name("hydra.python.syntax.TargetWithStarAtom"),
            Name("project"),
            Phantoms.record(
                Name("hydra.python.syntax.TPrimaryAndName"),
                [
                    Phantoms.field(
                        Name("primary"),
                        Phantoms.inject(
                            Name("hydra.python.syntax.TPrimary"),
                            Name("atom"),
                            Phantoms.inject(
                                Name("hydra.python.syntax.Atom"),
                                Name("name"),
                                Phantoms.var("obj"),
                            ),
                        ),
                    ),
                    Phantoms.field(Name("name"), Phantoms.var("attr")),
                ],
            ),
        ),
    )
    body = _let_chain(
        [("target", target_def)],
        _ap(
            _local("pyAssignmentToPyStatement"),
            PySyn.assignment_untyped(
                PyDsl.untyped_assignment_simple(
                    Phantoms.list_([Phantoms.var("target")]),
                    _ap(_local("pyExpressionToPyAnnotatedRhs"), Phantoms.var("expr")),
                ),
            ),
        ),
    )
    return _def(
        "dottedAssignmentStatement",
        Phantoms.doc(
            "Create a dotted assignment statement: obj.attr = expr",
            Phantoms.lambdas(["obj", "attr", "expr"], body),
        ),
    )


def _double_quoted_string():
    body = _ap(
        _local("stringToPyExpression"),
        PySyn.quote_style_double,
        Phantoms.var("s"),
    )
    return _def(
        "doubleQuotedString",
        Phantoms.doc(
            "Create a double-quoted string expression",
            Phantoms.lambdas(["s"], body),
        ),
    )


def _find_namespaces():
    body = Phantoms.lets(
        [
            Phantoms.field(
                Name("coreNs"),
                Packaging.namespace(Phantoms.string("hydra.core")),
            ),
            Phantoms.field(
                Name("namespaces"),
                _ap(
                    _analysis_namespaces_for_definitions,
                    _pynames_encode_namespace,
                    Phantoms.var("focusNs"),
                    Phantoms.var("defs"),
                ),
            ),
        ],
        Logic.if_else(
            Equality.equal(
                Packaging.un_namespace(
                    Pairs.first(Packaging.namespaces_focus(Phantoms.var("namespaces"))),
                ),
                Packaging.un_namespace(Phantoms.var("coreNs")),
            ),
            Phantoms.var("namespaces"),
            Packaging.namespaces(
                Packaging.namespaces_focus(Phantoms.var("namespaces")),
                Maps.insert(
                    Phantoms.var("coreNs"),
                    _ap(_pynames_encode_namespace, Phantoms.var("coreNs")),
                    Packaging.namespaces_mapping(Phantoms.var("namespaces")),
                ),
            ),
        ),
    )
    return _def(
        "findNamespaces",
        Phantoms.doc(
            "Find all namespaces referenced by a list of definitions, plus the core namespace",
            Phantoms.lambdas(["focusNs", "defs"], body),
        ),
    )


def _function_call():
    body = _ap(
        _local("pyPrimaryToPyExpression"),
        _ap(
            _local("primaryWithRhs"),
            Phantoms.var("func"),
            PySyn.primary_rhs_call(_ap(_local("pyExpressionsToPyArgs"), Phantoms.var("args"))),
        ),
    )
    return _def(
        "functionCall",
        Phantoms.doc(
            "Create a function call expression",
            Phantoms.lambdas(["func", "args"], body),
        ),
    )


def _get_item_params():
    body = PySyn.parameters_param_no_default(
        PyDsl.param_no_default_parameters_simple(Phantoms.list_([
            PyDsl.param_no_default_simple(PyDsl.param_simple(_py_name("cls"))),
            PyDsl.param_no_default_simple(PyDsl.param_simple(_py_name("item"))),
        ])),
    )
    return _def("getItemParams", body)


def _indented_block():
    body = Phantoms.lets(
        [
            Phantoms.field(
                Name("commentGroup"),
                Maybes.maybe(
                    Phantoms.list_([]),
                    Phantoms.lam(
                        "s",
                        Phantoms.list_([_ap(_local("commentStatement"), Phantoms.var("s"))]),
                    ),
                    Phantoms.var("mcomment"),
                ),
            ),
            Phantoms.field(
                Name("groups"),
                Lists.filter(
                    Phantoms.lam("g", Logic.not_(Lists.null(Phantoms.var("g")))),
                    Lists.cons(Phantoms.var("commentGroup"), Phantoms.var("stmts")),
                ),
            ),
        ],
        Logic.if_else(
            Lists.null(Phantoms.var("groups")),
            PySyn.block_indented(Phantoms.list_([
                Phantoms.list_([
                    PySyn.statement_simple(Phantoms.list_([
                        _ap(
                            _local("pyExpressionToPySimpleStatement"),
                            _ap(_local("pyAtomToPyExpression"), PySyn.atom_ellipsis),
                        ),
                    ])),
                ]),
            ])),
            PySyn.block_indented(Phantoms.var("groups")),
        ),
    )
    return _def(
        "indentedBlock",
        Phantoms.doc(
            "Create an indented block with optional comment",
            Phantoms.lambdas(["mcomment", "stmts"], body),
        ),
    )


def _name_and_params():
    body = _ap(
        _local("primaryAndParams"),
        _ap(_local("pyNameToPyPrimary"), Phantoms.var("pyName")),
        Phantoms.var("params"),
    )
    return _def(
        "nameAndParams",
        Phantoms.doc(
            "Create a name with parameters",
            Phantoms.lambdas(["pyName", "params"], body),
        ),
    )


def _newtype_statement():
    body = _ap(
        _local("annotatedStatement"),
        Phantoms.var("mcomment"),
        _ap(
            _local("assignmentStatement"),
            Phantoms.var("name"),
            _ap(
                _local("functionCall"),
                PyDsl.py_name_to_py_primary(_py_name("NewType")),
                Phantoms.list_([
                    _ap(_local("doubleQuotedString"), PySyn.un_name(Phantoms.var("name"))),
                    Phantoms.var("expr"),
                ]),
            ),
        ),
    )
    return _def(
        "newtypeStatement",
        Phantoms.doc(
            "Create a NewType statement",
            Phantoms.lambdas(["name", "mcomment", "expr"], body),
        ),
    )


def _or_expression():
    # Inner recursive 'build' lambda
    build_body = Maybes.maybe(
        # Unreachable fallback
        PySyn.bitwise_or(
            Phantoms.var("prev"),
            _ap(
                _local("pyPrimaryToPyBitwiseXor"),
                PySyn.primary_simple(PySyn.atom_ellipsis),
            ),
        ),
        Phantoms.lam(
            "p",
            Logic.if_else(
                Lists.null(Pairs.second(Phantoms.var("p"))),
                PySyn.bitwise_or(
                    Phantoms.var("prev"),
                    _ap(_local("pyPrimaryToPyBitwiseXor"), Pairs.first(Phantoms.var("p"))),
                ),
                _ap(
                    Phantoms.var("build"),
                    Phantoms.just(PySyn.bitwise_or(
                        Phantoms.var("prev"),
                        _ap(_local("pyPrimaryToPyBitwiseXor"), Pairs.first(Phantoms.var("p"))),
                    )),
                    Pairs.second(Phantoms.var("p")),
                ),
            ),
        ),
        Lists.uncons(Phantoms.var("ps")),
    )
    body = Phantoms.lets(
        [
            Phantoms.field(
                Name("build"),
                Phantoms.lambdas(["prev", "ps"], build_body),
            ),
        ],
        _ap(
            _local("pyBitwiseOrToPyExpression"),
            _ap(Phantoms.var("build"), Phantoms.nothing(), Phantoms.var("prims")),
        ),
    )
    return _def(
        "orExpression",
        Phantoms.doc(
            "Build an or-expression from multiple primaries",
            Phantoms.lambdas(["prims"], body),
        ),
    )


def _primary_and_params():
    body = _ap(
        _local("pyPrimaryToPyExpression"),
        _ap(_local("primaryWithExpressionSlices"), Phantoms.var("prim"), Phantoms.var("params")),
    )
    return _def(
        "primaryAndParams",
        Phantoms.doc(
            "Create a primary with parameters (subscript)",
            Phantoms.lambdas(["prim", "params"], body),
        ),
    )


def _primary_with_expression_slices():
    body = Maybes.from_maybe(
        Phantoms.var("prim"),
        Maybes.map(
            Phantoms.lam(
                "p",
                _ap(
                    _local("primaryWithSlices"),
                    Phantoms.var("prim"),
                    _ap(_local("pyExpressionToPySlice"), Pairs.first(Phantoms.var("p"))),
                    Lists.map(
                        Phantoms.lam(
                            "e",
                            PySyn.slice_or_starred_expression_slice(
                                _ap(_local("pyExpressionToPySlice"), Phantoms.var("e")),
                            ),
                        ),
                        Pairs.second(Phantoms.var("p")),
                    ),
                ),
            ),
            Lists.uncons(Phantoms.var("exprs")),
        ),
    )
    return _def(
        "primaryWithExpressionSlices",
        Phantoms.doc(
            "Create a Primary with expression slices",
            Phantoms.lambdas(["prim", "exprs"], body),
        ),
    )


def _primary_with_rhs():
    body = PySyn.primary_compound(
        PySyn.primary_with_rhs(Phantoms.var("prim"), Phantoms.var("rhs")),
    )
    return _def(
        "primaryWithRhs",
        Phantoms.doc(
            "Combine a Primary with a PrimaryRhs",
            Phantoms.lambdas(["prim", "rhs"], body),
        ),
    )


def _primary_with_slices():
    body = _ap(
        _local("primaryWithRhs"),
        Phantoms.var("prim"),
        PySyn.primary_rhs_slices(PySyn.slices(Phantoms.var("first"), Phantoms.var("rest"))),
    )
    return _def(
        "primaryWithSlices",
        Phantoms.doc(
            "Create a Primary with slices",
            Phantoms.lambdas(["prim", "first", "rest"], body),
        ),
    )


def _project_from_expression():
    body = Phantoms.lets(
        [
            Phantoms.field(
                Name("prim"),
                PySyn.primary_simple(PySyn.atom_group(
                    PySyn.group_expression(PySyn.named_expression_simple(Phantoms.var("exp"))),
                )),
            ),
        ],
        _ap(
            _local("pyPrimaryToPyExpression"),
            PySyn.primary_compound(
                PySyn.primary_with_rhs(
                    Phantoms.var("prim"),
                    PySyn.primary_rhs_project(Phantoms.var("name")),
                ),
            ),
        ),
    )
    return _def(
        "projectFromExpression",
        Phantoms.doc(
            "Project a field from an expression",
            Phantoms.lambdas(["exp", "name"], body),
        ),
    )


def _py_assignment_to_py_statement():
    body = _ap(
        _local("pySimpleStatementToPyStatement"),
        PySyn.simple_statement_assignment(Phantoms.var("a")),
    )
    return _def(
        "pyAssignmentToPyStatement",
        Phantoms.doc(
            "Convert an Assignment to a Statement",
            Phantoms.lambdas(["a"], body),
        ),
    )


def _py_atom_to_py_expression():
    body = _ap(
        _local("pyPrimaryToPyExpression"),
        PySyn.primary_simple(Phantoms.var("atom")),
    )
    return _def(
        "pyAtomToPyExpression",
        Phantoms.doc(
            "Convert an Atom to an Expression",
            Phantoms.lambdas(["atom"], body),
        ),
    )


def _py_bitwise_or_to_py_conjunction():
    body = PySyn.conjunction(Phantoms.list_([
        PySyn.inversion_simple(
            PySyn.comparison(Phantoms.var("bor"), Phantoms.list_([])),
        ),
    ]))
    return _def(
        "pyBitwiseOrToPyConjunction",
        Phantoms.doc(
            "Convert a BitwiseOr to a Conjunction",
            Phantoms.lambdas(["bor"], body),
        ),
    )


def _py_bitwise_or_to_py_expression():
    body = _ap(
        _local("pyConjunctionToPyExpression"),
        _ap(_local("pyBitwiseOrToPyConjunction"), Phantoms.var("bor")),
    )
    return _def(
        "pyBitwiseOrToPyExpression",
        Phantoms.doc(
            "Convert a BitwiseOr to an Expression",
            Phantoms.lambdas(["bor"], body),
        ),
    )


def _py_class_definition_to_py_statement():
    body = PySyn.statement_compound(PySyn.compound_statement_class_def(Phantoms.var("cd")))
    return _def(
        "pyClassDefinitionToPyStatement",
        Phantoms.doc(
            "Convert a ClassDefinition to a Statement",
            Phantoms.lambdas(["cd"], body),
        ),
    )


def _py_closed_pattern_to_py_patterns():
    body = PySyn.patterns_pattern(
        PySyn.pattern_or(PySyn.or_pattern(Phantoms.list_([Phantoms.var("p")]))),
    )
    return _def(
        "pyClosedPatternToPyPatterns",
        Phantoms.doc(
            "Convert a ClosedPattern to Patterns",
            Phantoms.lambdas(["p"], body),
        ),
    )


def _py_conjunction_to_py_expression():
    body = PySyn.expression_simple(
        PySyn.disjunction(Phantoms.list_([Phantoms.var("conj")])),
    )
    return _def(
        "pyConjunctionToPyExpression",
        Phantoms.doc(
            "Convert a Conjunction to an Expression",
            Phantoms.lambdas(["conj"], body),
        ),
    )


def _py_expression_to_bitwise_or():
    body = PyDsl.py_primary_to_py_bitwise_or(
        PySyn.primary_simple(
            PySyn.atom_group(
                PySyn.group_expression(PySyn.named_expression_simple(Phantoms.var("e"))),
            ),
        ),
    )
    return _def(
        "pyExpressionToBitwiseOr",
        Phantoms.doc(
            "Convert an Expression to a BitwiseOr, wrapping in parens if needed",
            Phantoms.lambdas(["e"], body),
        ),
    )


def _py_expression_to_disjunction():
    body = Phantoms.cases(
        Name("hydra.python.syntax.Expression"),
        Phantoms.var("e"),
        Just(PySyn.disjunction(Phantoms.list_([
            _ap(
                _local("pyPrimaryToPyConjunction"),
                PySyn.primary_simple(
                    PySyn.atom_group(
                        PySyn.group_expression(PySyn.named_expression_simple(Phantoms.var("e"))),
                    ),
                ),
            ),
        ]))),
        [
            Phantoms.field(
                Name("simple"),
                Phantoms.lam("disj", Phantoms.var("disj")),
            ),
        ],
    )
    return _def(
        "pyExpressionToDisjunction",
        Phantoms.doc(
            "Convert an Expression to a Disjunction, wrapping in parens if needed",
            Phantoms.lambdas(["e"], body),
        ),
    )


def _py_expression_to_py_annotated_rhs():
    body = PySyn.annotated_rhs_star(
        Phantoms.list_([PySyn.star_expression_simple(Phantoms.var("expr"))]),
    )
    return _def(
        "pyExpressionToPyAnnotatedRhs",
        Phantoms.doc(
            "Convert an Expression to an AnnotatedRhs",
            Phantoms.lambdas(["expr"], body),
        ),
    )


def _py_expression_to_py_primary():
    body = Maybes.maybe(
        PySyn.primary_simple(
            PySyn.atom_group(
                PySyn.group_expression(PySyn.named_expression_simple(Phantoms.var("e"))),
            ),
        ),
        Phantoms.lam("prim", Phantoms.var("prim")),
        _ap(_local("decodePyExpressionToPyPrimary"), Phantoms.var("e")),
    )
    return _def(
        "pyExpressionToPyPrimary",
        Phantoms.doc(
            "Extracts the primary from an expression, or wraps it in parentheses if the expression does not contain a primary",
            Phantoms.lambdas(["e"], body),
        ),
    )


def _py_expression_to_py_simple_statement():
    body = PySyn.simple_statement_star_expressions(
        Phantoms.list_([PySyn.star_expression_simple(Phantoms.var("expr"))]),
    )
    return _def(
        "pyExpressionToPySimpleStatement",
        Phantoms.doc(
            "Convert an Expression to a SimpleStatement (as star expressions)",
            Phantoms.lambdas(["expr"], body),
        ),
    )


def _py_expression_to_py_slice():
    body = PySyn.slice_named(PySyn.named_expression_simple(Phantoms.var("expr")))
    return _def(
        "pyExpressionToPySlice",
        Phantoms.doc(
            "Convert an Expression to a Slice",
            Phantoms.lambdas(["expr"], body),
        ),
    )


def _py_expression_to_py_star_named_expression():
    body = PySyn.star_named_expression_simple(
        PySyn.named_expression_simple(Phantoms.var("expr")),
    )
    return _def(
        "pyExpressionToPyStarNamedExpression",
        Phantoms.doc(
            "Convert an Expression to a StarNamedExpression",
            Phantoms.lambdas(["expr"], body),
        ),
    )


def _py_expression_to_py_statement():
    body = _ap(
        _local("pySimpleStatementToPyStatement"),
        _ap(_local("pyExpressionToPySimpleStatement"), Phantoms.var("expr")),
    )
    return _def(
        "pyExpressionToPyStatement",
        Phantoms.doc(
            "Convert an Expression to a Statement",
            Phantoms.lambdas(["expr"], body),
        ),
    )


def _py_expressions_to_py_args():
    body = PyDsl.args_positional_only(
        Lists.map(
            Phantoms.lam("e", PySyn.pos_arg_expression(Phantoms.var("e"))),
            Phantoms.var("exprs"),
        ),
    )
    return _def(
        "pyExpressionsToPyArgs",
        Phantoms.doc(
            "Convert a list of Expressions to Args",
            Phantoms.lambdas(["exprs"], body),
        ),
    )


def _py_list():
    body = PyDsl.list_(
        Lists.map(_local("pyExpressionToPyStarNamedExpression"), Phantoms.var("exprs")),
    )
    return _def(
        "pyList",
        Phantoms.doc(
            "Create a Python list from expressions",
            Phantoms.lambdas(["exprs"], body),
        ),
    )


def _py_name_to_py_expression():
    body = _ap(
        _local("pyPrimaryToPyExpression"),
        _ap(_local("pyNameToPyPrimary"), Phantoms.var("name")),
    )
    return _def(
        "pyNameToPyExpression",
        Phantoms.doc(
            "Convert a Name to an Expression",
            Phantoms.lambdas(["name"], body),
        ),
    )


def _py_name_to_py_named_expression():
    body = PySyn.named_expression_simple(
        _ap(_local("pyNameToPyExpression"), Phantoms.var("name")),
    )
    return _def(
        "pyNameToPyNamedExpression",
        Phantoms.doc(
            "Convert a Name to a NamedExpression",
            Phantoms.lambdas(["name"], body),
        ),
    )


def _py_name_to_py_primary():
    body = PySyn.primary_simple(PySyn.atom_name(Phantoms.var("name")))
    return _def(
        "pyNameToPyPrimary",
        Phantoms.doc(
            "Convert a Name to a Primary (simple atom)",
            Phantoms.lambdas(["name"], body),
        ),
    )


def _py_name_to_py_star_target():
    body = PySyn.star_target_unstarred(
        PySyn.target_with_star_atom_atom(PySyn.star_atom_name(Phantoms.var("name"))),
    )
    return _def(
        "pyNameToPyStarTarget",
        Phantoms.doc(
            "Convert a Name to a StarTarget",
            Phantoms.lambdas(["name"], body),
        ),
    )


def _py_name_to_py_type_parameter():
    body = PySyn.type_parameter_simple(
        PyDsl.simple_type_parameter_simple(Phantoms.var("name")),
    )
    return _def(
        "pyNameToPyTypeParameter",
        Phantoms.doc(
            "Convert a Name to a TypeParameter",
            Phantoms.lambdas(["name"], body),
        ),
    )


def _py_none():
    return _def(
        "pyNone",
        Phantoms.doc(
            "The Python None value as a Name",
            _py_name("None"),
        ),
    )


def _py_primary_to_py_bitwise_or():
    body = PyDsl.py_primary_to_py_bitwise_or(Phantoms.var("prim"))
    return _def(
        "pyPrimaryToPyBitwiseOr",
        Phantoms.doc(
            "Convert a Primary to a BitwiseOr",
            Phantoms.lambdas(["prim"], body),
        ),
    )


def _py_primary_to_py_bitwise_xor():
    body = PyDsl.py_primary_to_py_bitwise_xor(Phantoms.var("prim"))
    return _def(
        "pyPrimaryToPyBitwiseXor",
        Phantoms.doc(
            "Convert a Primary to a BitwiseXor",
            Phantoms.lambdas(["prim"], body),
        ),
    )


def _py_primary_to_py_conjunction():
    body = _ap(
        _local("pyBitwiseOrToPyConjunction"),
        _ap(_local("pyPrimaryToPyBitwiseOr"), Phantoms.var("prim")),
    )
    return _def(
        "pyPrimaryToPyConjunction",
        Phantoms.doc(
            "Convert a Primary to a Conjunction",
            Phantoms.lambdas(["prim"], body),
        ),
    )


def _py_primary_to_py_expression():
    body = _ap(
        _local("pyConjunctionToPyExpression"),
        _ap(_local("pyPrimaryToPyConjunction"), Phantoms.var("prim")),
    )
    return _def(
        "pyPrimaryToPyExpression",
        Phantoms.doc(
            "Convert a Primary to an Expression",
            Phantoms.lambdas(["prim"], body),
        ),
    )


def _py_primary_to_py_slice():
    body = _ap(
        _local("pyExpressionToPySlice"),
        _ap(_local("pyPrimaryToPyExpression"), Phantoms.var("prim")),
    )
    return _def(
        "pyPrimaryToPySlice",
        Phantoms.doc(
            "Convert a Primary to a Slice",
            Phantoms.lambdas(["prim"], body),
        ),
    )


def _py_simple_statement_to_py_statement():
    body = PySyn.statement_simple(Phantoms.list_([Phantoms.var("s")]))
    return _def(
        "pySimpleStatementToPyStatement",
        Phantoms.doc(
            "Convert a SimpleStatement to a Statement",
            Phantoms.lambdas(["s"], body),
        ),
    )


def _raise_assertion_error():
    body = _ap(
        _local("pySimpleStatementToPyStatement"),
        PySyn.simple_statement_raise(
            PySyn.raise_statement(Phantoms.just(
                PySyn.raise_expression(
                    _ap(
                        _local("functionCall"),
                        PyDsl.py_name_to_py_primary(_py_name("AssertionError")),
                        Phantoms.list_([_ap(_local("doubleQuotedString"), Phantoms.var("msg"))]),
                    ),
                    Phantoms.nothing(),
                ),
            )),
        ),
    )
    return _def(
        "raiseAssertionError",
        Phantoms.doc(
            "Create a raise AssertionError statement",
            Phantoms.lambdas(["msg"], body),
        ),
    )


def _raise_type_error():
    body = _ap(
        _local("pySimpleStatementToPyStatement"),
        PySyn.simple_statement_raise(
            PySyn.raise_statement(Phantoms.just(
                PySyn.raise_expression(
                    _ap(
                        _local("functionCall"),
                        PyDsl.py_name_to_py_primary(_py_name("TypeError")),
                        Phantoms.list_([_ap(_local("doubleQuotedString"), Phantoms.var("msg"))]),
                    ),
                    Phantoms.nothing(),
                ),
            )),
        ),
    )
    return _def(
        "raiseTypeError",
        Phantoms.doc(
            "Create a raise TypeError statement",
            Phantoms.lambdas(["msg"], body),
        ),
    )


def _return_single():
    body = _ap(
        _local("pySimpleStatementToPyStatement"),
        PySyn.simple_statement_return(
            PySyn.return_statement(Phantoms.list_([
                PySyn.star_expression_simple(Phantoms.var("expr")),
            ])),
        ),
    )
    return _def(
        "returnSingle",
        Phantoms.doc(
            "Create a return statement with a single expression",
            Phantoms.lambdas(["expr"], body),
        ),
    )


def _self_only_params():
    body = PySyn.parameters_param_no_default(
        PyDsl.param_no_default_parameters_simple(Phantoms.list_([
            PyDsl.param_no_default_simple(PyDsl.param_simple(_py_name("self"))),
        ])),
    )
    return _def("selfOnlyParams", body)


def _self_other_params():
    body = PySyn.parameters_param_no_default(
        PyDsl.param_no_default_parameters_simple(Phantoms.list_([
            PyDsl.param_no_default_simple(PyDsl.param_simple(_py_name("self"))),
            PyDsl.param_no_default_simple(PyDsl.param_simple(_py_name("other"))),
        ])),
    )
    return _def("selfOtherParams", body)


def _single_quoted_string():
    body = _ap(
        _local("stringToPyExpression"),
        PySyn.quote_style_single,
        Phantoms.var("s"),
    )
    return _def(
        "singleQuotedString",
        Phantoms.doc(
            "Create a single-quoted string expression",
            Phantoms.lambdas(["s"], body),
        ),
    )


def _string_to_py_expression():
    body = _ap(
        _local("pyAtomToPyExpression"),
        PySyn.atom_string(PyDsl.string_(Phantoms.var("s"), Phantoms.var("style"))),
    )
    return _def(
        "stringToPyExpression",
        Phantoms.doc(
            "Create a string expression with a given quote style",
            Phantoms.lambdas(["style", "s"], body),
        ),
    )


def _target_python_version():
    body = Phantoms.inject_unit(
        Name("hydra.python.environment.PythonVersion"),
        Name("python310"),
    )
    return _def(
        "targetPythonVersion",
        Phantoms.doc(
            "Current target Python version for code generation",
            body,
        ),
    )


def _triple_quoted_string():
    body = _ap(
        _local("stringToPyExpression"),
        PySyn.quote_style_triple,
        Phantoms.var("s"),
    )
    return _def(
        "tripleQuotedString",
        Phantoms.doc(
            "Create a triple-quoted string expression",
            Phantoms.lambdas(["s"], body),
        ),
    )


def _type_alias_statement():
    body = _ap(
        _local("annotatedStatement"),
        Phantoms.var("mcomment"),
        _ap(
            _local("pySimpleStatementToPyStatement"),
            PySyn.simple_statement_type_alias(
                PySyn.type_alias(
                    Phantoms.var("name"),
                    Phantoms.var("tparams"),
                    Phantoms.var("tyexpr"),
                ),
            ),
        ),
    )
    return _def(
        "typeAliasStatement",
        Phantoms.doc(
            "Generate a type alias statement using PEP 695 syntax (Python 3.12+)",
            Phantoms.lambdas(["name", "tparams", "mcomment", "tyexpr"], body),
        ),
    )


def _type_alias_statement_310():
    inner = Phantoms.lets(
        [
            Phantoms.field(
                Name("quotedExpr"),
                _ap(
                    _local("doubleQuotedString"),
                    _ap(
                        _serialization_print_expr,
                        _ap(_pyserde_expression_to_expr, Phantoms.var("tyexpr")),
                    ),
                ),
            ),
        ],
        _ap(
            _local("annotatedStatement"),
            Phantoms.var("mcomment"),
            _ap(
                _local("pyAssignmentToPyStatement"),
                PySyn.assignment_typed(
                    PySyn.typed_assignment(
                        PySyn.single_target_name(Phantoms.var("name")),
                        PyDsl.py_name_to_py_expression(_py_name("TypeAlias")),
                        Phantoms.just(
                            _ap(_local("pyExpressionToPyAnnotatedRhs"), Phantoms.var("quotedExpr")),
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "typeAliasStatement310",
        Phantoms.doc(
            "Generate a type alias statement using Python 3.10-compatible syntax: Name: TypeAlias = \"TypeExpression\"",
            Phantoms.lambdas(["name", "_tparams", "mcomment", "tyexpr"], inner),
        ),
    )


def _union_type_class_statements_310():
    # Build the body: lots of nested let bindings, finally returns list of 2 statements.
    return_object_def = _ap(
        _local("pySimpleStatementToPyStatement"),
        PySyn.simple_statement_return(
            PySyn.return_statement(Phantoms.list_([
                PySyn.star_expression_simple(
                    PyDsl.py_name_to_py_expression(_py_name("object")),
                ),
            ])),
        ),
    )
    get_item_method_def = PySyn.statement_compound(
        PySyn.compound_statement_function(
            PySyn.function_definition(
                Phantoms.nothing(),
                PySyn.function_def_raw(
                    Phantoms.false(),
                    _py_name("__getitem__"),
                    Phantoms.list_([]),
                    Phantoms.just(_local("getItemParams")),
                    Phantoms.nothing(),
                    Phantoms.nothing(),
                    _ap(
                        _local("indentedBlock"),
                        Phantoms.nothing(),
                        Phantoms.list_([
                            Phantoms.list_([Phantoms.var("returnObject")]),
                        ]),
                    ),
                ),
            ),
        ),
    )
    meta_class_def = _ap(
        _local("pyClassDefinitionToPyStatement"),
        PySyn.class_definition(
            Phantoms.nothing(),
            Phantoms.var("metaName"),
            Phantoms.list_([]),
            Phantoms.just(_ap(
                _local("pyExpressionsToPyArgs"),
                Phantoms.list_([PyDsl.py_name_to_py_expression(_py_name("type"))]),
            )),
            _ap(
                _local("indentedBlock"),
                Phantoms.nothing(),
                Phantoms.list_([Phantoms.list_([Phantoms.var("getItemMethod")])]),
            ),
        ),
    )
    doc_stmt_def = _ap(
        _local("pyExpressionToPyStatement"),
        _ap(_local("tripleQuotedString"), Phantoms.var("docString")),
    )
    body_groups_def = Logic.if_else(
        Lists.null(Phantoms.var("extraStmts")),
        _let_chain(
            [("passStmt", _ap(_local("pySimpleStatementToPyStatement"), PySyn.simple_statement_pass))],
            Phantoms.list_([
                Phantoms.list_([Phantoms.var("docStmt")]),
                Phantoms.list_([Phantoms.var("passStmt")]),
            ]),
        ),
        Phantoms.list_([
            Phantoms.list_([Phantoms.var("docStmt")]),
            Phantoms.var("extraStmts"),
        ]),
    )
    metaclass_arg_def = PySyn.kwarg(
        _py_name("metaclass"),
        PyDsl.py_name_to_py_expression(Phantoms.var("metaName")),
    )
    union_class_def = _ap(
        _local("annotatedStatement"),
        Phantoms.var("mcomment"),
        _ap(
            _local("pyClassDefinitionToPyStatement"),
            PySyn.class_definition(
                Phantoms.nothing(),
                Phantoms.var("name"),
                Phantoms.list_([]),
                Phantoms.just(PySyn.args(
                    Phantoms.list_([]),
                    Phantoms.list_([PySyn.kwarg_or_starred_kwarg(Phantoms.var("metaclassArg"))]),
                    Phantoms.list_([]),
                )),
                _ap(
                    _local("indentedBlock"),
                    Phantoms.nothing(),
                    Phantoms.var("bodyGroups"),
                ),
            ),
        ),
    )
    body = _let_chain(
        [
            ("nameStr", PySyn.un_name(Phantoms.var("name"))),
            ("metaName", _py_name_concat3("_", "nameStr", "Meta")),
            ("docString", _ap(
                _serialization_print_expr,
                _ap(_pyserde_expression_to_expr, Phantoms.var("tyexpr")),
            )),
            ("returnObject", return_object_def),
            ("getItemMethod", get_item_method_def),
            ("metaClass", meta_class_def),
            ("docStmt", doc_stmt_def),
            ("bodyGroups", body_groups_def),
            ("metaclassArg", metaclass_arg_def),
            ("unionClass", union_class_def),
        ],
        Phantoms.list_([
            Phantoms.var("metaClass"),
            Phantoms.var("unionClass"),
        ]),
    )
    return _def(
        "unionTypeClassStatements310",
        Phantoms.doc(
            "Generate a subscriptable union class for Python 3.10",
            Phantoms.lambdas(["name", "mcomment", "tyexpr", "extraStmts"], body),
        ),
    )


def _py_name_concat3(prefix_str, var_name, suffix_str):
    """Helper for: PyDsl.name $ string prefix ++ var "x" ++ string suffix.

    Haskell ++ is left-associative: (("_" ++ nameStr) ++ "Meta").
    """
    return PySyn.name(Strings.cat2(
        Strings.cat2(Phantoms.string(prefix_str), Phantoms.var(var_name)),
        Phantoms.string(suffix_str),
    ))


def _unit_variant_methods():
    slots_stmt_def = _ap(
        _local("assignmentStatement"),
        _py_name("__slots__"),
        _ap(
            _local("pyPrimaryToPyExpression"),
            PySyn.primary_simple(PySyn.atom_tuple(PySyn.tuple(Phantoms.list_([])))),
        ),
    )
    return_isinstance_def = _ap(
        _local("pySimpleStatementToPyStatement"),
        PySyn.simple_statement_return(
            PySyn.return_statement(Phantoms.list_([
                PySyn.star_expression_simple(
                    _ap(
                        _local("functionCall"),
                        PyDsl.py_name_to_py_primary(_py_name("isinstance")),
                        Phantoms.list_([
                            PyDsl.py_name_to_py_expression(_py_name("other")),
                            PyDsl.py_name_to_py_expression(Phantoms.var("className")),
                        ]),
                    ),
                ),
            ])),
        ),
    )
    eq_method_def = PySyn.statement_compound(
        PySyn.compound_statement_function(
            PySyn.function_definition(
                Phantoms.nothing(),
                PySyn.function_def_raw(
                    Phantoms.false(),
                    _py_name("__eq__"),
                    Phantoms.list_([]),
                    Phantoms.just(_local("selfOtherParams")),
                    Phantoms.nothing(),
                    Phantoms.nothing(),
                    _ap(
                        _local("indentedBlock"),
                        Phantoms.nothing(),
                        Phantoms.list_([Phantoms.list_([Phantoms.var("returnIsinstance")])]),
                    ),
                ),
            ),
        ),
    )
    return_hash_def = _ap(
        _local("pySimpleStatementToPyStatement"),
        PySyn.simple_statement_return(
            PySyn.return_statement(Phantoms.list_([
                PySyn.star_expression_simple(
                    _ap(
                        _local("functionCall"),
                        PyDsl.py_name_to_py_primary(_py_name("hash")),
                        Phantoms.list_([_ap(_local("doubleQuotedString"), Phantoms.var("classNameStr"))]),
                    ),
                ),
            ])),
        ),
    )
    hash_method_def = PySyn.statement_compound(
        PySyn.compound_statement_function(
            PySyn.function_definition(
                Phantoms.nothing(),
                PySyn.function_def_raw(
                    Phantoms.false(),
                    _py_name("__hash__"),
                    Phantoms.list_([]),
                    Phantoms.just(_local("selfOnlyParams")),
                    Phantoms.nothing(),
                    Phantoms.nothing(),
                    _ap(
                        _local("indentedBlock"),
                        Phantoms.nothing(),
                        Phantoms.list_([Phantoms.list_([Phantoms.var("returnHash")])]),
                    ),
                ),
            ),
        ),
    )
    body = _let_chain(
        [
            ("classNameStr", PySyn.un_name(Phantoms.var("className"))),
            ("slotsStmt", slots_stmt_def),
            ("returnIsinstance", return_isinstance_def),
            ("eqMethod", eq_method_def),
            ("returnHash", return_hash_def),
            ("hashMethod", hash_method_def),
        ],
        Phantoms.list_([
            Phantoms.var("slotsStmt"),
            Phantoms.var("eqMethod"),
            Phantoms.var("hashMethod"),
        ]),
    )
    return _def(
        "unitVariantMethods",
        Phantoms.doc(
            "Generate __slots__, __eq__, and __hash__ methods for unit-typed union variants",
            Phantoms.lambdas(["className"], body),
        ),
    )


def _build_module() -> Module:
    return Module(
        _PLACEHOLDER.description,
        _PLACEHOLDER.namespace,
        _PLACEHOLDER.dependencies,
        (
            Phantoms.to_definition(_annotated_expression()),
            Phantoms.to_definition(_annotated_statement()),
            Phantoms.to_definition(_assignment()),
            Phantoms.to_definition(_assignment_statement()),
            Phantoms.to_definition(_cast_to()),
            Phantoms.to_definition(_comment_statement()),
            Phantoms.to_definition(_decode_py_comparison_to_py_await_primary()),
            Phantoms.to_definition(_decode_py_conjunction_to_py_primary()),
            Phantoms.to_definition(_decode_py_expression_to_py_primary()),
            Phantoms.to_definition(_decode_py_inversion_to_py_primary()),
            Phantoms.to_definition(_decode_py_power_to_py_primary()),
            Phantoms.to_definition(_dotted_assignment_statement()),
            Phantoms.to_definition(_double_quoted_string()),
            Phantoms.to_definition(_find_namespaces()),
            Phantoms.to_definition(_function_call()),
            Phantoms.to_definition(_get_item_params()),
            Phantoms.to_definition(_indented_block()),
            Phantoms.to_definition(_name_and_params()),
            Phantoms.to_definition(_newtype_statement()),
            Phantoms.to_definition(_or_expression()),
            Phantoms.to_definition(_primary_and_params()),
            Phantoms.to_definition(_primary_with_expression_slices()),
            Phantoms.to_definition(_primary_with_rhs()),
            Phantoms.to_definition(_primary_with_slices()),
            Phantoms.to_definition(_project_from_expression()),
            Phantoms.to_definition(_py_assignment_to_py_statement()),
            Phantoms.to_definition(_py_atom_to_py_expression()),
            Phantoms.to_definition(_py_bitwise_or_to_py_conjunction()),
            Phantoms.to_definition(_py_bitwise_or_to_py_expression()),
            Phantoms.to_definition(_py_class_definition_to_py_statement()),
            Phantoms.to_definition(_py_closed_pattern_to_py_patterns()),
            Phantoms.to_definition(_py_conjunction_to_py_expression()),
            Phantoms.to_definition(_py_expression_to_bitwise_or()),
            Phantoms.to_definition(_py_expression_to_disjunction()),
            Phantoms.to_definition(_py_expression_to_py_annotated_rhs()),
            Phantoms.to_definition(_py_expression_to_py_primary()),
            Phantoms.to_definition(_py_expression_to_py_simple_statement()),
            Phantoms.to_definition(_py_expression_to_py_slice()),
            Phantoms.to_definition(_py_expression_to_py_star_named_expression()),
            Phantoms.to_definition(_py_expression_to_py_statement()),
            Phantoms.to_definition(_py_expressions_to_py_args()),
            Phantoms.to_definition(_py_list()),
            Phantoms.to_definition(_py_name_to_py_expression()),
            Phantoms.to_definition(_py_name_to_py_named_expression()),
            Phantoms.to_definition(_py_name_to_py_primary()),
            Phantoms.to_definition(_py_name_to_py_star_target()),
            Phantoms.to_definition(_py_name_to_py_type_parameter()),
            Phantoms.to_definition(_py_none()),
            Phantoms.to_definition(_py_primary_to_py_bitwise_or()),
            Phantoms.to_definition(_py_primary_to_py_bitwise_xor()),
            Phantoms.to_definition(_py_primary_to_py_conjunction()),
            Phantoms.to_definition(_py_primary_to_py_expression()),
            Phantoms.to_definition(_py_primary_to_py_slice()),
            Phantoms.to_definition(_py_simple_statement_to_py_statement()),
            Phantoms.to_definition(_raise_assertion_error()),
            Phantoms.to_definition(_raise_type_error()),
            Phantoms.to_definition(_return_single()),
            Phantoms.to_definition(_self_only_params()),
            Phantoms.to_definition(_self_other_params()),
            Phantoms.to_definition(_single_quoted_string()),
            Phantoms.to_definition(_string_to_py_expression()),
            Phantoms.to_definition(_target_python_version()),
            Phantoms.to_definition(_triple_quoted_string()),
            Phantoms.to_definition(_type_alias_statement()),
            Phantoms.to_definition(_type_alias_statement_310()),
            Phantoms.to_definition(_union_type_class_statements_310()),
            Phantoms.to_definition(_unit_variant_methods()),
        ),
    )


module_ = _build_module()
