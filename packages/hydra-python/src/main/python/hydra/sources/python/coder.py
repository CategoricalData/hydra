"""Python code generator: converts Hydra modules to Python source code.

Mirror of packages/hydra-python/src/main/haskell/Hydra/Sources/Python/Coder.hs.
"""

from hydra.core import Name
from hydra.dsl.python import Just, Nothing  # noqa: F401
from hydra.packaging import Module, Namespace

import hydra.dsl.meta.lib.equality as Equality
import hydra.dsl.meta.lib.lists as Lists
import hydra.dsl.meta.lib.logic as Logic
import hydra.dsl.meta.lib.maps as Maps
import hydra.dsl.meta.lib.maybes as Maybes
import hydra.dsl.meta.lib.pairs as Pairs
import hydra.dsl.meta.lib.sets as Sets
import hydra.dsl.meta.lib.strings as Strings
import hydra.dsl.meta.phantoms as Phantoms
import hydra.dsl.meta.lib.eithers as Eithers
import hydra.dsl.python.syntax as PySyn

import hydra.dsl.coders as Coders_dsl
import hydra.dsl.core as Core
import hydra.dsl.errors as Errors_dsl
import hydra.dsl.graph as Graph_dsl
import hydra.dsl.meta.core as MetaCore
import hydra.dsl.meta.lib.literals as Literals
import hydra.dsl.meta.lib.math as Math
import hydra.dsl.packaging as Pkg

from hydra.sources.python import _kernel_refs as _kref
from hydra.sources.python import _python_helpers as PyDsl  # noqa: F401


# ----------------------------------------------------------------------
# Module setup
# ----------------------------------------------------------------------

NS = Namespace("hydra.python.coder")

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
#   [PyUtils.ns, PyNames.ns, PySerde.ns, Serialization.ns, Analysis.ns,
#    Environment.ns, Formatting.ns, Names.ns, Predicates.ns, Resolution.ns,
#    Rewriting.ns, Dependencies.ns, Scoping.ns, Strip.ns, Variables.ns,
#    ShowCore.ns, Reduction.ns, Sorting.ns, Inference.ns]
#   L.++ (PyEnvironmentSource.ns:PySyntax.ns:KernelTypes.kernelTypesNamespaces)
DEPENDENCIES = [
    Namespace(n) for n in [
        "hydra.python.utils",
        "hydra.python.names",
        "hydra.python.serde",
        "hydra.serialization",
        "hydra.analysis",
        "hydra.environment",
        "hydra.formatting",
        "hydra.names",
        "hydra.predicates",
        "hydra.resolution",
        "hydra.rewriting",
        "hydra.dependencies",
        "hydra.scoping",
        "hydra.strip",
        "hydra.variables",
        "hydra.show.core",
        "hydra.reduction",
        "hydra.sorting",
        "hydra.inference",
        "hydra.python.environment",
        "hydra.python.syntax",
    ]
] + KERNEL_TYPES_NAMESPACES


_PLACEHOLDER = Module(
    Just("Python code generator: converts Hydra modules to Python source code"),
    NS,
    DEPENDENCIES,
    (),
)


def _def(local_name, term):
    return Phantoms.definition_in_module(_PLACEHOLDER, local_name, term)


def _local(local_name: str):
    return Phantoms.var(f"hydra.python.coder.{local_name}")


def _ap(fun, *args):
    out = fun
    for a in args:
        out = Phantoms.apply(out, a)
    return out


def _let_chain(bindings, body):
    out = body
    for name, val in reversed(bindings):
        out = Phantoms.let1(name, val, out)
    return out


def _proj(type_fq: str, field_name: str, var_name: str):
    """project(typeName, fieldName) @@ var(varName)"""
    return _ap(
        Phantoms.project(Name(type_fq), Name(field_name)),
        Phantoms.var(var_name),
    )


def _env(field: str, var_name: str):
    return _proj("hydra.python.environment.PythonEnvironment", field, var_name)


def _pygraph(field: str, var_name: str):
    return _proj("hydra.python.environment.PyGraph", field, var_name)


def _meta_proj(field: str, var_name: str):
    return _proj("hydra.python.environment.PythonModuleMetadata", field, var_name)


_META_FIELDS = (
    "namespaces",
    "typeVariables",
    "usesAnnotated",
    "usesCallable",
    "usesCast",
    "usesLruCache",
    "usesTypeAlias",
    "usesDataclass",
    "usesDecimal",
    "usesEither",
    "usesEnum",
    "usesFrozenDict",
    "usesFrozenList",
    "usesFrozenSet",
    "usesGeneric",
    "usesJust",
    "usesLeft",
    "usesMaybe",
    "usesName",
    "usesNode",
    "usesNothing",
    "usesRight",
    "usesTypeVar",
)


def _meta_record_with_field_set(set_field: str, set_value, m_var: str = "m"):
    """Build a PythonModuleMetadata record where one field is set to set_value
    and all other fields are projected from var(m_var)."""
    fields = []
    for f in _META_FIELDS:
        if f == set_field:
            fields.append(Phantoms.field(Name(f), set_value))
        else:
            fields.append(Phantoms.field(Name(f), _meta_proj(f, m_var)))
    return Phantoms.record(
        Name("hydra.python.environment.PythonModuleMetadata"), fields
    )


def _type_cases_with_one_branch(arg_term, default_result, branch_field,
                                 other_variants_in_order):
    """Build cases _Type arg Nothing [<branch>, ...other_variants -> default_result].

    The branch_field is emitted first; the remaining variants in
    `other_variants_in_order` each get `constant default_result`. The order is
    significant — match the order in the Haskell source for the def being ported.
    """
    fields = [branch_field]
    for v in other_variants_in_order:
        fields.append(
            Phantoms.field(Name(v), Phantoms.constant(default_result))
        )
    return Phantoms.cases(
        Name("hydra.core.Type"),
        _ap(_kref.strip_deannotate_type, arg_term),
        Nothing(),
        fields,
    )


def _empty_meta_record(namespaces_term):
    """Build a fresh PythonModuleMetadata record where namespaces=arg, all use*=false, typeVariables=Sets.empty."""
    fields = []
    for f in _META_FIELDS:
        if f == "namespaces":
            fields.append(Phantoms.field(Name(f), namespaces_term))
        elif f == "typeVariables":
            fields.append(Phantoms.field(Name(f), Sets.empty()))
        else:
            fields.append(Phantoms.field(Name(f), Phantoms.false()))
    return Phantoms.record(
        Name("hydra.python.environment.PythonModuleMetadata"), fields
    )


def _set_meta_use_def(local_name: str, field: str, b_first: bool):
    """Generate a setMetaUses<X> def that sets field to var "b". By default,
    params are (m, b); if b_first, params are (b, m). No doc annotation."""
    params = ["b", "m"] if b_first else ["m", "b"]
    body = Phantoms.lambdas(
        params,
        _meta_record_with_field_set(field, Phantoms.var("b"), m_var="m"),
    )
    return _def(local_name, body)


def _py_name(s):
    """PyDsl.name $ string s — wrap a string in hydra.python.syntax.Name."""
    if isinstance(s, str):
        s = Phantoms.string(s)
    return Phantoms.wrap(Name("hydra.python.syntax.Name"), s)


# ----------------------------------------------------------------------
# Definitions (alphabetical)
# ----------------------------------------------------------------------

def _analyze_python_function():
    body = Phantoms.lambdas(
        ["cx", "env", "term"],
        _ap(
            _kref.analysis_analyze_function_term_with,
            Phantoms.var("cx"),
            _local("pythonBindingMetadata"),
            _local("pythonEnvironmentGetGraph"),
            _local("pythonEnvironmentSetGraph"),
            Phantoms.var("env"),
            Phantoms.var("term"),
        ),
    )
    return _def(
        "analyzePythonFunction",
        Phantoms.doc(
            "Analyze a function term with Python-specific Graph management",
            body,
        ),
    )


def _class_variant_pattern_with_capture():
    body = Phantoms.lambdas(
        ["env", "pyVariantName", "varName"],
        _let_chain(
            [
                (
                    "pyVarNameAttr",
                    PySyn.name_or_attribute(Phantoms.list_([Phantoms.var("pyVariantName")])),
                ),
                (
                    "capturePattern",
                    PySyn.closed_pattern_capture(
                        PySyn.capture_pattern(
                            PySyn.pattern_capture_target(
                                _ap(
                                    _kref.names_encode_name,
                                    Phantoms.false(),
                                    _kref.util_case_convention_lower_snake,
                                    Phantoms.var("env"),
                                    Phantoms.var("varName"),
                                )
                            )
                        )
                    ),
                ),
                (
                    "keywordPattern",
                    PySyn.keyword_pattern(
                        _py_name("value"),
                        PySyn.pattern_or(
                            PySyn.or_pattern(Phantoms.list_([Phantoms.var("capturePattern")]))
                        ),
                    ),
                ),
            ],
            PySyn.closed_pattern_class(
                PyDsl.class_pattern_with_keywords(
                    Phantoms.var("pyVarNameAttr"),
                    PySyn.keyword_patterns(
                        Phantoms.list_([Phantoms.var("keywordPattern")])
                    ),
                )
            ),
        ),
    )
    return _def(
        "classVariantPatternWithCapture",
        Phantoms.doc(
            "Create a class pattern for a variant with captured value", body
        ),
    )


def _case_block_to_expr():
    eff_lambda_default = _let_chain(
        [
            ("syntheticVar", Core.name(Phantoms.string("_matchValue"))),
        ],
        Core.lambda_(
            Phantoms.var("syntheticVar"),
            Phantoms.nothing(),
            Core.term_application(
                Core.application(
                    Phantoms.var("stripped"),
                    Core.term_variable(Phantoms.var("syntheticVar")),
                )
            ),
        ),
    )
    body = Phantoms.lambdas(
        ["cx", "env", "tname", "rowType", "isEnum", "encodeBody", "field"],
        _let_chain(
            [
                ("fname", Core.field_name(Phantoms.var("field"))),
                ("fterm", Core.field_term(Phantoms.var("field"))),
                (
                    "stripped",
                    _ap(
                        _kref.strip_deannotate_and_detype_term, Phantoms.var("fterm")
                    ),
                ),
                (
                    "effectiveLambda",
                    Phantoms.cases(
                        Name("hydra.core.Term"),
                        Phantoms.var("stripped"),
                        Just(eff_lambda_default),
                        [
                            Phantoms.field(
                                Name("lambda"),
                                Phantoms.lam("lam", Phantoms.var("lam")),
                            ),
                        ],
                    ),
                ),
                ("v", Core.lambda_parameter(Phantoms.var("effectiveLambda"))),
                ("rawBody", Core.lambda_body(Phantoms.var("effectiveLambda"))),
                (
                    "isUnitVariant",
                    _ap(
                        _local("isVariantUnitType"),
                        Phantoms.var("rowType"),
                        Phantoms.var("fname"),
                    ),
                ),
                (
                    "effectiveBody",
                    Logic.if_else(
                        Phantoms.var("isUnitVariant"),
                        _ap(
                            _local("eliminateUnitVar"),
                            Phantoms.var("v"),
                            Phantoms.var("rawBody"),
                        ),
                        Phantoms.var("rawBody"),
                    ),
                ),
                (
                    "shouldCapture",
                    Logic.not_(
                        Logic.or_(
                            Phantoms.var("isUnitVariant"),
                            Logic.or_(
                                _ap(
                                    _kref.variables_is_free_variable_in_term,
                                    Phantoms.var("v"),
                                    Phantoms.var("rawBody"),
                                ),
                                _ap(
                                    _kref.predicates_is_unit_term,
                                    Phantoms.var("rawBody"),
                                ),
                            ),
                        )
                    ),
                ),
                (
                    "env2",
                    _ap(
                        _local("pythonEnvironmentSetGraph"),
                        _ap(
                            _kref.scoping_extend_graph_for_lambda,
                            _ap(
                                _local("pythonEnvironmentGetGraph"), Phantoms.var("env")
                            ),
                            Phantoms.var("effectiveLambda"),
                        ),
                        Phantoms.var("env"),
                    ),
                ),
                (
                    "pyVariantName",
                    _ap(
                        _local("deconflictVariantName"),
                        Phantoms.true(),
                        Phantoms.var("env2"),
                        Phantoms.var("tname"),
                        Phantoms.var("fname"),
                        _env("graph", "env2"),
                    ),
                ),
                (
                    "pattern",
                    _ap(
                        _local("variantClosedPattern"),
                        Phantoms.var("env2"),
                        Phantoms.var("tname"),
                        Phantoms.var("fname"),
                        Phantoms.var("pyVariantName"),
                        Phantoms.var("rowType"),
                        Phantoms.var("isEnum"),
                        Phantoms.var("v"),
                        Phantoms.var("shouldCapture"),
                    ),
                ),
            ],
            Eithers.bind(
                _ap(
                    Phantoms.var("encodeBody"),
                    Phantoms.var("env2"),
                    Phantoms.var("effectiveBody"),
                ),
                Phantoms.lam(
                    "stmts",
                    _let_chain(
                        [
                            (
                                "pyBody",
                                _ap(
                                    _kref.utils_indented_block,
                                    Phantoms.nothing(),
                                    Phantoms.list_([Phantoms.var("stmts")]),
                                ),
                            ),
                        ],
                        Phantoms.right(
                            PySyn.case_block(
                                _ap(
                                    _kref.utils_py_closed_pattern_to_py_patterns,
                                    Phantoms.var("pattern"),
                                ),
                                Phantoms.nothing(),
                                Phantoms.var("pyBody"),
                            )
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "caseBlockToExpr",
        Phantoms.doc(
            "Encode a single case (Field) into a CaseBlock for a match statement",
            body,
        ),
    )


def _class_variant_pattern_unit():
    body = Phantoms.lambdas(
        ["pyVariantName"],
        PySyn.closed_pattern_class(
            PyDsl.class_pattern_simple(
                PySyn.name_or_attribute(Phantoms.list_([Phantoms.var("pyVariantName")]))
            )
        ),
    )
    return _def(
        "classVariantPatternUnit",
        Phantoms.doc(
            "Create a class pattern for a unit variant (no value captured)",
            body,
        ),
    )


def _cond_import_symbol():
    body = Phantoms.lambdas(
        ["name", "flag"],
        Logic.if_else(
            Phantoms.var("flag"),
            Phantoms.just(Phantoms.var("name")),
            Phantoms.nothing(),
        ),
    )
    return _def(
        "condImportSymbol",
        Phantoms.doc(
            "Conditionally include a symbol name based on a boolean flag",
            body,
        ),
    )


def _collect_type_variables():
    body = Phantoms.lambdas(
        ["initial", "typ"],
        Phantoms.cases(
            Name("hydra.core.Type"),
            _ap(_kref.strip_deannotate_type, Phantoms.var("typ")),
            Just(
                _let_chain(
                    [
                        (
                            "freeVars",
                            _ap(
                                _kref.variables_free_variables_in_type,
                                Phantoms.var("typ"),
                            ),
                        ),
                        (
                            "isTypeVar",
                            Phantoms.lam(
                                "n",
                                _ap(_local("isTypeVariableName"), Phantoms.var("n")),
                            ),
                        ),
                        (
                            "filteredList",
                            Lists.filter(
                                Phantoms.var("isTypeVar"),
                                Sets.to_list(Phantoms.var("freeVars")),
                            ),
                        ),
                    ],
                    Sets.union(
                        Phantoms.var("initial"),
                        Sets.from_list(Phantoms.var("filteredList")),
                    ),
                )
            ),
            [
                Phantoms.field(
                    Name("forall"),
                    Phantoms.lam(
                        "ft",
                        _let_chain(
                            [
                                (
                                    "v",
                                    Core.forall_type_parameter(Phantoms.var("ft")),
                                ),
                                (
                                    "body",
                                    Core.forall_type_body(Phantoms.var("ft")),
                                ),
                            ],
                            _ap(
                                _local("collectTypeVariables"),
                                Sets.insert(
                                    Phantoms.var("v"), Phantoms.var("initial")
                                ),
                                Phantoms.var("body"),
                            ),
                        ),
                    ),
                ),
            ],
        ),
    )
    return _def(
        "collectTypeVariables",
        Phantoms.doc("Collect type variables from a type", body),
    )


def _deconflict_variant_name():
    body = Phantoms.lambdas(
        ["isQualified", "env", "unionName", "fname", "g"],
        _let_chain(
            [
                (
                    "candidateHydraName",
                    Phantoms.wrap(
                        Name("hydra.core.Name"),
                        Strings.cat2(
                            Core.un_name(Phantoms.var("unionName")),
                            _ap(
                                _kref.formatting_capitalize,
                                Core.un_name(Phantoms.var("fname")),
                            ),
                        ),
                    ),
                ),
                (
                    "termCollision",
                    Maps.member(
                        Phantoms.var("candidateHydraName"),
                        Graph_dsl.graph_bound_terms(Phantoms.var("g")),
                    ),
                ),
                (
                    "typeCollision",
                    Maps.member(
                        Phantoms.var("candidateHydraName"),
                        Graph_dsl.graph_schema_types(Phantoms.var("g")),
                    ),
                ),
                (
                    "collision",
                    Logic.or_(
                        Phantoms.var("termCollision"), Phantoms.var("typeCollision")
                    ),
                ),
            ],
            Logic.if_else(
                Phantoms.var("collision"),
                _py_name(
                    Strings.cat2(
                        _ap(
                            Phantoms.unwrap(Name("hydra.python.syntax.Name")),
                            _ap(
                                _kref.names_variant_name,
                                Phantoms.var("isQualified"),
                                Phantoms.var("env"),
                                Phantoms.var("unionName"),
                                Phantoms.var("fname"),
                            ),
                        ),
                        Phantoms.string("_"),
                    )
                ),
                _ap(
                    _kref.names_variant_name,
                    Phantoms.var("isQualified"),
                    Phantoms.var("env"),
                    Phantoms.var("unionName"),
                    Phantoms.var("fname"),
                ),
            ),
        ),
    )
    return _def(
        "deconflictVariantName",
        Phantoms.doc(
            "Deconflict a variant name to avoid collisions with type names", body
        ),
    )


def _deduplicate_case_variables():
    rewrite_lambda = _let_chain(
        [
            ("v", Core.lambda_parameter(Phantoms.var("lam"))),
            ("mdom", Core.lambda_domain(Phantoms.var("lam"))),
            ("body", Core.lambda_body(Phantoms.var("lam"))),
        ],
        Maybes.maybe(
            Phantoms.pair(
                Maps.insert(
                    Phantoms.var("v"),
                    Phantoms.int32(1),
                    Phantoms.var("countByName"),
                ),
                Lists.cons(Phantoms.var("field"), Phantoms.var("done")),
            ),
            Phantoms.lam(
                "count",
                _let_chain(
                    [
                        ("count2", Math.add(Phantoms.var("count"), Phantoms.int32(1))),
                        (
                            "v2",
                            Core.name(
                                Strings.cat2(
                                    Core.un_name(Phantoms.var("v")),
                                    Literals.show_int32(Phantoms.var("count2")),
                                )
                            ),
                        ),
                        (
                            "newBody",
                            _ap(
                                _kref.reduction_alpha_convert,
                                Phantoms.var("v"),
                                Phantoms.var("v2"),
                                Phantoms.var("body"),
                            ),
                        ),
                        (
                            "newLam",
                            Core.lambda_(
                                Phantoms.var("v2"),
                                Phantoms.var("mdom"),
                                Phantoms.var("newBody"),
                            ),
                        ),
                        (
                            "newTerm",
                            Phantoms.inject(
                                Name("hydra.core.Term"),
                                Name("lambda"),
                                Phantoms.var("newLam"),
                            ),
                        ),
                        (
                            "newField",
                            Core.field(Phantoms.var("fname"), Phantoms.var("newTerm")),
                        ),
                    ],
                    Phantoms.pair(
                        Maps.insert(
                            Phantoms.var("v"),
                            Phantoms.var("count2"),
                            Phantoms.var("countByName"),
                        ),
                        Lists.cons(Phantoms.var("newField"), Phantoms.var("done")),
                    ),
                ),
            ),
            Maps.lookup(Phantoms.var("v"), Phantoms.var("countByName")),
        ),
    )
    rewrite_case = Phantoms.lambdas(
        ["state", "field"],
        _let_chain(
            [
                ("countByName", Pairs.first(Phantoms.var("state"))),
                ("done", Pairs.second(Phantoms.var("state"))),
                ("fname", Core.field_name(Phantoms.var("field"))),
                ("fterm", Core.field_term(Phantoms.var("field"))),
            ],
            Phantoms.cases(
                Name("hydra.core.Term"),
                _ap(_kref.strip_deannotate_and_detype_term, Phantoms.var("fterm")),
                Just(
                    Phantoms.pair(
                        Phantoms.var("countByName"),
                        Lists.cons(Phantoms.var("field"), Phantoms.var("done")),
                    )
                ),
                [
                    Phantoms.field(
                        Name("lambda"),
                        Phantoms.lam("lam", rewrite_lambda),
                    ),
                ],
            ),
        ),
    )
    body = Phantoms.lambdas(
        ["cases_"],
        _let_chain(
            [
                ("rewriteCase", rewrite_case),
                (
                    "result",
                    Lists.foldl(
                        Phantoms.var("rewriteCase"),
                        Phantoms.pair(Maps.empty(), Phantoms.list_([])),
                        Phantoms.var("cases_"),
                    ),
                ),
            ],
            Lists.reverse(Pairs.second(Phantoms.var("result"))),
        ),
    )
    return _def(
        "deduplicateCaseVariables",
        Phantoms.doc(
            "Rewrite case statements to avoid variable name collisions", body
        ),
    )


def _dig_for_wrap():
    body = Phantoms.lambdas(
        ["isTermAnnot", "meta", "typ"],
        Phantoms.cases(
            Name("hydra.core.Type"),
            _ap(_kref.strip_deannotate_type, Phantoms.var("typ")),
            Just(Phantoms.var("meta")),
            [
                Phantoms.field(
                    Name("forall"),
                    Phantoms.lam(
                        "ft",
                        _ap(
                            _local("digForWrap"),
                            Phantoms.var("isTermAnnot"),
                            Phantoms.var("meta"),
                            Core.forall_type_body(Phantoms.var("ft")),
                        ),
                    ),
                ),
                Phantoms.field(
                    Name("wrap"),
                    Phantoms.constant(
                        Logic.if_else(
                            Phantoms.var("isTermAnnot"),
                            Phantoms.var("meta"),
                            _ap(
                                _local("setMetaUsesNode"),
                                Phantoms.var("meta"),
                                Phantoms.true(),
                            ),
                        )
                    ),
                ),
            ],
        ),
    )
    return _def(
        "digForWrap",
        Phantoms.doc("Recursively dig through forall types to find wrap types", body),
    )


def _eliminate_unit_var():
    rewrite_field = Phantoms.lambdas(
        ["rewrite", "fld"],
        Core.field(
            Core.field_name(Phantoms.var("fld")),
            _ap(Phantoms.var("rewrite"), Core.field_term(Phantoms.var("fld"))),
        ),
    )
    rewrite_binding = Phantoms.lambdas(
        ["rewrite", "bnd"],
        Core.binding(
            Core.binding_name(Phantoms.var("bnd")),
            _ap(Phantoms.var("rewrite"), Core.binding_term(Phantoms.var("bnd"))),
            Core.binding_type_scheme(Phantoms.var("bnd")),
        ),
    )
    rewrite_body_fields = [
        Phantoms.field(
            Name("variable"),
            Phantoms.lam(
                "n",
                Logic.if_else(
                    Equality.equal(Phantoms.var("n"), Phantoms.var("v")),
                    Core.term_unit,
                    Phantoms.var("term"),
                ),
            ),
        ),
        Phantoms.field(
            Name("annotated"),
            Phantoms.lam(
                "at",
                Core.term_annotated(
                    Core.annotated_term(
                        _ap(
                            Phantoms.var("recurse"),
                            Core.annotated_term_body(Phantoms.var("at")),
                        ),
                        Core.annotated_term_annotation(Phantoms.var("at")),
                    )
                ),
            ),
        ),
        Phantoms.field(
            Name("application"),
            Phantoms.lam(
                "app",
                Core.term_application(
                    Core.application(
                        _ap(
                            Phantoms.var("recurse"),
                            Core.application_function(Phantoms.var("app")),
                        ),
                        _ap(
                            Phantoms.var("recurse"),
                            Core.application_argument(Phantoms.var("app")),
                        ),
                    )
                ),
            ),
        ),
        Phantoms.field(
            Name("lambda"),
            Phantoms.lam(
                "lam",
                Logic.if_else(
                    Equality.equal(
                        Core.lambda_parameter(Phantoms.var("lam")), Phantoms.var("v")
                    ),
                    Phantoms.var("term"),
                    Core.term_lambda(
                        Core.lambda_(
                            Core.lambda_parameter(Phantoms.var("lam")),
                            Core.lambda_domain(Phantoms.var("lam")),
                            _ap(
                                Phantoms.var("recurse"),
                                Core.lambda_body(Phantoms.var("lam")),
                            ),
                        )
                    ),
                ),
            ),
        ),
        Phantoms.field(
            Name("cases"),
            Phantoms.lam(
                "cs",
                Core.term_cases(
                    Core.case_statement(
                        Core.case_statement_type_name(Phantoms.var("cs")),
                        Maybes.map(
                            Phantoms.var("recurse"),
                            Core.case_statement_default(Phantoms.var("cs")),
                        ),
                        Lists.map(
                            _ap(
                                Phantoms.var("rewriteField"), Phantoms.var("recurse")
                            ),
                            Core.case_statement_cases(Phantoms.var("cs")),
                        ),
                    )
                ),
            ),
        ),
        Phantoms.field(
            Name("let"),
            Phantoms.lam(
                "lt",
                Core.term_let(
                    getattr(Core, "let")(
                        Lists.map(
                            _ap(
                                Phantoms.var("rewriteBinding"),
                                Phantoms.var("recurse"),
                            ),
                            Core.let_bindings(Phantoms.var("lt")),
                        ),
                        _ap(
                            Phantoms.var("recurse"),
                            Core.let_body(Phantoms.var("lt")),
                        ),
                    )
                ),
            ),
        ),
        Phantoms.field(
            Name("list"),
            Phantoms.lam(
                "ts",
                Core.term_list(
                    Lists.map(Phantoms.var("recurse"), Phantoms.var("ts"))
                ),
            ),
        ),
        Phantoms.field(
            Name("map"),
            Phantoms.lam(
                "m",
                Core.term_map(
                    Maps.from_list(
                        Lists.map(
                            Phantoms.lam(
                                "kv",
                                Phantoms.pair(
                                    _ap(
                                        Phantoms.var("recurse"),
                                        Pairs.first(Phantoms.var("kv")),
                                    ),
                                    _ap(
                                        Phantoms.var("recurse"),
                                        Pairs.second(Phantoms.var("kv")),
                                    ),
                                ),
                            ),
                            Maps.to_list(Phantoms.var("m")),
                        )
                    )
                ),
            ),
        ),
        Phantoms.field(
            Name("record"),
            Phantoms.lam(
                "rec",
                Core.term_record(
                    Core.record(
                        Core.record_type_name(Phantoms.var("rec")),
                        Lists.map(
                            _ap(
                                Phantoms.var("rewriteField"),
                                Phantoms.var("recurse"),
                            ),
                            Core.record_fields(Phantoms.var("rec")),
                        ),
                    )
                ),
            ),
        ),
        Phantoms.field(
            Name("set"),
            Phantoms.lam(
                "s",
                Core.term_set(
                    Sets.map(Phantoms.var("recurse"), Phantoms.var("s"))
                ),
            ),
        ),
        Phantoms.field(
            Name("inject"),
            Phantoms.lam(
                "inj",
                Core.term_inject(
                    Core.injection(
                        Core.injection_type_name(Phantoms.var("inj")),
                        _ap(
                            Phantoms.var("rewriteField"),
                            Phantoms.var("recurse"),
                            Core.injection_field(Phantoms.var("inj")),
                        ),
                    )
                ),
            ),
        ),
        Phantoms.field(
            Name("maybe"),
            Phantoms.lam(
                "mt",
                Core.term_maybe(
                    Maybes.map(Phantoms.var("recurse"), Phantoms.var("mt"))
                ),
            ),
        ),
        Phantoms.field(
            Name("pair"),
            Phantoms.lam(
                "p",
                Core.term_pair(
                    Phantoms.pair(
                        _ap(
                            Phantoms.var("recurse"), Pairs.first(Phantoms.var("p"))
                        ),
                        _ap(
                            Phantoms.var("recurse"), Pairs.second(Phantoms.var("p"))
                        ),
                    )
                ),
            ),
        ),
        Phantoms.field(
            Name("wrap"),
            Phantoms.lam(
                "wt",
                Core.term_wrap(
                    Core.wrapped_term(
                        Core.wrapped_term_type_name(Phantoms.var("wt")),
                        _ap(
                            Phantoms.var("recurse"),
                            Core.wrapped_term_body(Phantoms.var("wt")),
                        ),
                    )
                ),
            ),
        ),
        Phantoms.field(
            Name("either"),
            Phantoms.lam(
                "e",
                Core.term_either(
                    Eithers.bimap(
                        Phantoms.var("recurse"),
                        Phantoms.var("recurse"),
                        Phantoms.var("e"),
                    )
                ),
            ),
        ),
        Phantoms.field(
            Name("typeApplication"),
            Phantoms.lam(
                "ta",
                Core.term_type_application(
                    Core.type_application_term(
                        _ap(
                            Phantoms.var("recurse"),
                            Core.type_application_term_body(Phantoms.var("ta")),
                        ),
                        Core.type_application_term_type(Phantoms.var("ta")),
                    )
                ),
            ),
        ),
        Phantoms.field(
            Name("typeLambda"),
            Phantoms.lam(
                "tl",
                Core.term_type_lambda(
                    Core.type_lambda(
                        Core.type_lambda_parameter(Phantoms.var("tl")),
                        _ap(
                            Phantoms.var("recurse"),
                            Core.type_lambda_body(Phantoms.var("tl")),
                        ),
                    )
                ),
            ),
        ),
    ]
    rewrite = Phantoms.lambdas(
        ["recurse", "term"],
        Phantoms.cases(
            Name("hydra.core.Term"),
            _ap(_kref.strip_deannotate_and_detype_term, Phantoms.var("term")),
            Just(Phantoms.var("term")),
            rewrite_body_fields,
        ),
    )
    body = Phantoms.lambdas(
        ["v", "term0"],
        _let_chain(
            [
                ("rewriteField", rewrite_field),
                ("rewriteBinding", rewrite_binding),
                ("rewrite", rewrite),
                (
                    "go",
                    Phantoms.lam(
                        "term",
                        _ap(
                            Phantoms.var("rewrite"),
                            Phantoms.var("go"),
                            Phantoms.var("term"),
                        ),
                    ),
                ),
            ],
            _ap(Phantoms.var("go"), Phantoms.var("term0")),
        ),
    )
    return _def(
        "eliminateUnitVar",
        Phantoms.doc(
            "Substitute unit for a variable in a term (for unit variant case handling)",
            body,
        ),
    )


def _empty_metadata():
    body = Phantoms.lambdas(["ns"], _empty_meta_record(Phantoms.var("ns")))
    return _def(
        "emptyMetadata",
        Phantoms.doc(
            "Create an initial empty metadata record with given namespaces", body
        ),
    )


def _encode_application():
    body = Phantoms.lambdas(
        ["cx", "env", "app"],
        _let_chain(
            [
                (
                    "g",
                    _ap(_local("pythonEnvironmentGetGraph"), Phantoms.var("env")),
                ),
                ("term", Core.term_application(Phantoms.var("app"))),
                (
                    "gathered",
                    _ap(
                        _kref.analysis_gather_args,
                        Phantoms.var("term"),
                        Phantoms.list_([]),
                    ),
                ),
                ("fun", Pairs.first(Phantoms.var("gathered"))),
                ("args", Pairs.second(Phantoms.var("gathered"))),
                (
                    "knownArity",
                    _ap(
                        _local("termArityWithPrimitives"),
                        Phantoms.var("g"),
                        Phantoms.var("fun"),
                    ),
                ),
                (
                    "arity",
                    Math.max_(
                        Phantoms.var("knownArity"), Lists.length(Phantoms.var("args"))
                    ),
                ),
            ],
            Eithers.bind(
                Eithers.map_list(
                    Phantoms.lam(
                        "t",
                        _ap(
                            _local("encodeTermInline"),
                            Phantoms.var("cx"),
                            Phantoms.var("env"),
                            Phantoms.false(),
                            Phantoms.var("t"),
                        ),
                    ),
                    Phantoms.var("args"),
                ),
                Phantoms.lam(
                    "pargs",
                    _let_chain(
                        [
                            (
                                "hargs",
                                Lists.take(Phantoms.var("arity"), Phantoms.var("pargs")),
                            ),
                            (
                                "rargs",
                                Lists.drop(Phantoms.var("arity"), Phantoms.var("pargs")),
                            ),
                        ],
                        Eithers.bind(
                            _ap(
                                _local("encodeApplicationInner"),
                                Phantoms.var("cx"),
                                Phantoms.var("env"),
                                Phantoms.var("fun"),
                                Phantoms.var("hargs"),
                                Phantoms.var("rargs"),
                            ),
                            Phantoms.lam(
                                "result",
                                _let_chain(
                                    [
                                        ("lhs", Pairs.first(Phantoms.var("result"))),
                                        (
                                            "remainingRargs",
                                            Pairs.second(Phantoms.var("result")),
                                        ),
                                        (
                                            "pyapp",
                                            Lists.foldl(
                                                Phantoms.lambdas(
                                                    ["t", "a"],
                                                    _ap(
                                                        _kref.utils_function_call,
                                                        _ap(
                                                            _kref.utils_py_expression_to_py_primary,
                                                            Phantoms.var("t"),
                                                        ),
                                                        Phantoms.list_(
                                                            [Phantoms.var("a")]
                                                        ),
                                                    ),
                                                ),
                                                Phantoms.var("lhs"),
                                                Phantoms.var("remainingRargs"),
                                            ),
                                        ),
                                    ],
                                    Phantoms.right(Phantoms.var("pyapp")),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeApplication",
        Phantoms.doc("Encode a function application to a Python expression", body),
    )


def _encode_application_inner():
    with_rest = Phantoms.lam(
        "e",
        Logic.if_else(
            Lists.null(Phantoms.var("restArgs")),
            Phantoms.var("e"),
            _ap(
                _kref.utils_function_call,
                _ap(_kref.utils_py_expression_to_py_primary, Phantoms.var("e")),
                Phantoms.var("restArgs"),
            ),
        ),
    )
    default_case = Eithers.bind(
        _ap(
            _local("encodeTermInline"),
            Phantoms.var("cx"),
            Phantoms.var("env"),
            Phantoms.false(),
            Phantoms.var("fun"),
        ),
        Phantoms.lam(
            "pfun",
            Phantoms.right(
                Phantoms.pair(
                    _ap(
                        _kref.utils_function_call,
                        _ap(
                            _kref.utils_py_expression_to_py_primary,
                            Phantoms.var("pfun"),
                        ),
                        Phantoms.var("hargs"),
                    ),
                    Phantoms.var("rargs"),
                )
            ),
        ),
    )
    project_branch = Phantoms.lam(
        "proj",
        _let_chain(
            [
                ("fname", _proj("hydra.core.Projection", "field", "proj")),
                (
                    "fieldExpr",
                    _ap(
                        _kref.utils_project_from_expression,
                        Phantoms.var("firstArg"),
                        _ap(
                            _kref.names_encode_field_name,
                            Phantoms.var("env"),
                            Phantoms.var("fname"),
                        ),
                    ),
                ),
            ],
            Phantoms.right(
                Phantoms.pair(
                    _ap(Phantoms.var("withRest"), Phantoms.var("fieldExpr")),
                    Phantoms.var("rargs"),
                )
            ),
        ),
    )
    cases_branch = Phantoms.lam(
        "cs",
        Eithers.bind(
            _ap(
                _local("encodeUnionEliminationInline"),
                Phantoms.var("cx"),
                Phantoms.var("env"),
                Phantoms.var("cs"),
                Phantoms.var("firstArg"),
            ),
            Phantoms.lam(
                "inlineExpr",
                Phantoms.right(
                    Phantoms.pair(
                        _ap(Phantoms.var("withRest"), Phantoms.var("inlineExpr")),
                        Phantoms.var("rargs"),
                    )
                ),
            ),
        ),
    )
    unwrap_branch = Phantoms.constant(
        _let_chain(
            [
                (
                    "valueExpr",
                    _ap(
                        _kref.utils_project_from_expression,
                        Phantoms.var("firstArg"),
                        _py_name("value"),
                    ),
                ),
                (
                    "allArgs",
                    Lists.concat2(Phantoms.var("restArgs"), Phantoms.var("rargs")),
                ),
            ],
            Logic.if_else(
                Lists.null(Phantoms.var("allArgs")),
                Phantoms.right(
                    Phantoms.pair(
                        Phantoms.var("valueExpr"), Phantoms.list_([])
                    )
                ),
                Phantoms.right(
                    Phantoms.pair(
                        _ap(
                            _kref.utils_function_call,
                            _ap(
                                _kref.utils_py_expression_to_py_primary,
                                Phantoms.var("valueExpr"),
                            ),
                            Phantoms.var("allArgs"),
                        ),
                        Phantoms.list_([]),
                    )
                ),
            ),
        )
    )
    lambda_branch = Phantoms.constant(
        Eithers.bind(
            _ap(
                _local("encodeTermInline"),
                Phantoms.var("cx"),
                Phantoms.var("env"),
                Phantoms.false(),
                Phantoms.var("fun"),
            ),
            Phantoms.lam(
                "pfun",
                Phantoms.right(
                    Phantoms.pair(
                        _ap(
                            _kref.utils_function_call,
                            _ap(
                                _kref.utils_py_expression_to_py_primary,
                                Phantoms.var("pfun"),
                            ),
                            Phantoms.var("hargs"),
                        ),
                        Phantoms.var("rargs"),
                    )
                ),
            ),
        )
    )
    # Variable branch: complex
    not_in_graph_branch = Eithers.bind(
        _ap(
            _local("encodeVariable"),
            Phantoms.var("cx"),
            Phantoms.var("env"),
            Phantoms.var("name"),
            Phantoms.var("hargs"),
        ),
        Phantoms.lam(
            "expr",
            Phantoms.right(
                Phantoms.pair(Phantoms.var("expr"), Phantoms.var("rargs"))
            ),
        ),
    )
    has_ts_branch = Phantoms.lam(
        "ts",
        _let_chain(
            [
                (
                    "elArity",
                    _ap(_kref.arity_type_scheme_arity, Phantoms.var("ts")),
                ),
                (
                    "consumeCount",
                    Math.min_(
                        Phantoms.var("elArity"), Lists.length(Phantoms.var("allArgs"))
                    ),
                ),
                (
                    "consumedArgs",
                    Lists.take(
                        Phantoms.var("consumeCount"), Phantoms.var("allArgs")
                    ),
                ),
                (
                    "remainingArgs",
                    Lists.drop(
                        Phantoms.var("consumeCount"), Phantoms.var("allArgs")
                    ),
                ),
            ],
            Logic.if_else(
                Lists.null(Phantoms.var("consumedArgs")),
                Eithers.bind(
                    _ap(
                        _local("encodeVariable"),
                        Phantoms.var("cx"),
                        Phantoms.var("env"),
                        Phantoms.var("name"),
                        Phantoms.list_([]),
                    ),
                    Phantoms.lam(
                        "expr",
                        Phantoms.right(
                            Phantoms.pair(
                                Phantoms.var("expr"), Phantoms.var("rargs")
                            )
                        ),
                    ),
                ),
                Phantoms.right(
                    Phantoms.pair(
                        _ap(
                            _kref.utils_function_call,
                            _ap(
                                _kref.utils_py_name_to_py_primary,
                                _ap(
                                    _kref.names_encode_name,
                                    Phantoms.true(),
                                    _kref.util_case_convention_lower_snake,
                                    Phantoms.var("env"),
                                    Phantoms.var("name"),
                                ),
                            ),
                            Phantoms.var("consumedArgs"),
                        ),
                        Phantoms.var("remainingArgs"),
                    )
                ),
            ),
        ),
    )
    in_graph_branch = Phantoms.lam(
        "el",
        Maybes.maybe(
            Eithers.bind(
                _ap(
                    _local("encodeVariable"),
                    Phantoms.var("cx"),
                    Phantoms.var("env"),
                    Phantoms.var("name"),
                    Phantoms.var("hargs"),
                ),
                Phantoms.lam(
                    "expr",
                    Phantoms.right(
                        Phantoms.pair(
                            Phantoms.var("expr"), Phantoms.var("rargs")
                        )
                    ),
                ),
            ),
            has_ts_branch,
            Core.binding_type_scheme(Phantoms.var("el")),
        ),
    )
    not_primitive_branch = Maybes.maybe(
        not_in_graph_branch,
        in_graph_branch,
        _ap(
            _kref.lexical_lookup_binding, Phantoms.var("g"), Phantoms.var("name")
        ),
    )
    is_primitive_branch = Phantoms.lam(
        "_prim",
        _let_chain(
            [
                (
                    "wrappedArgs",
                    _ap(
                        _local("wrapLazyArguments"),
                        Phantoms.var("name"),
                        Phantoms.var("hargs"),
                    ),
                ),
            ],
            Eithers.bind(
                _ap(
                    _local("encodeVariable"),
                    Phantoms.var("cx"),
                    Phantoms.var("env"),
                    Phantoms.var("name"),
                    Phantoms.var("wrappedArgs"),
                ),
                Phantoms.lam(
                    "expr",
                    Phantoms.right(
                        Phantoms.pair(
                            Phantoms.var("expr"), Phantoms.var("rargs")
                        )
                    ),
                ),
            ),
        ),
    )
    variable_branch = Phantoms.lam(
        "name",
        _let_chain(
            [
                (
                    "g",
                    _ap(_local("pythonEnvironmentGetGraph"), Phantoms.var("env")),
                ),
                (
                    "allArgs",
                    Lists.concat2(Phantoms.var("hargs"), Phantoms.var("rargs")),
                ),
            ],
            Maybes.cases(
                Maps.lookup(
                    Phantoms.var("name"),
                    Graph_dsl.graph_primitives(Phantoms.var("g")),
                ),
                not_primitive_branch,
                is_primitive_branch,
            ),
        ),
    )
    body = Phantoms.lambdas(
        ["cx", "env", "fun", "hargs", "rargs"],
        _let_chain(
            [
                (
                    "firstArg",
                    Maybes.from_maybe(
                        _ap(_kref.utils_py_name_to_py_expression, _py_name("")),
                        Lists.maybe_head(Phantoms.var("hargs")),
                    ),
                ),
                (
                    "restArgs",
                    Lists.drop(Phantoms.int32(1), Phantoms.var("hargs")),
                ),
                ("withRest", with_rest),
                ("defaultCase", default_case),
            ],
            Phantoms.cases(
                Name("hydra.core.Term"),
                _ap(
                    _kref.strip_deannotate_and_detype_term, Phantoms.var("fun")
                ),
                Just(Phantoms.var("defaultCase")),
                [
                    Phantoms.field(Name("project"), project_branch),
                    Phantoms.field(Name("cases"), cases_branch),
                    Phantoms.field(Name("unwrap"), unwrap_branch),
                    Phantoms.field(Name("lambda"), lambda_branch),
                    Phantoms.field(Name("variable"), variable_branch),
                ],
            ),
        ),
    )
    return _def(
        "encodeApplicationInner",
        Phantoms.doc("Inner helper for encodeApplication", body),
    )


def _encode_application_type():
    gather_params_inner = Phantoms.lambdas(
        ["t", "ps"],
        _type_cases_with_one_branch(
            Phantoms.var("t"),
            Phantoms.pair(Phantoms.var("t"), Phantoms.var("ps")),
            Phantoms.field(
                Name("application"),
                Phantoms.lam(
                    "appT",
                    _ap(
                        Phantoms.var("gatherParams"),
                        _proj("hydra.core.ApplicationType", "function", "appT"),
                        Lists.cons(
                            _proj("hydra.core.ApplicationType", "argument", "appT"),
                            Phantoms.var("ps"),
                        ),
                    ),
                ),
            ),
            ["annotated", "function", "forall", "list", "literal", "map",
             "maybe", "either", "pair", "record", "set", "union", "unit",
             "variable", "void", "wrap"],
        ),
    )
    body = Phantoms.lambdas(
        ["env", "at"],
        _let_chain(
            [("gatherParams", gather_params_inner)],
            _let_chain(
                [
                    (
                        "bodyAndArgs",
                        _ap(
                            Phantoms.var("gatherParams"),
                            Phantoms.inject(
                                Name("hydra.core.Type"),
                                Name("application"),
                                Phantoms.var("at"),
                            ),
                            Phantoms.list_([]),
                        ),
                    ),
                    ("body", Pairs.first(Phantoms.var("bodyAndArgs"))),
                    ("args", Pairs.second(Phantoms.var("bodyAndArgs"))),
                ],
                Eithers.bind(
                    _ap(_local("encodeType"), Phantoms.var("env"), Phantoms.var("body")),
                    Phantoms.lam(
                        "pyBody",
                        Eithers.bind(
                            Eithers.map_list(
                                _ap(_local("encodeType"), Phantoms.var("env")),
                                Phantoms.var("args"),
                            ),
                            Phantoms.lam(
                                "pyArgs",
                                Phantoms.right(
                                    _ap(
                                        _kref.utils_primary_and_params,
                                        _ap(
                                            _kref.utils_py_expression_to_py_primary,
                                            Phantoms.var("pyBody"),
                                        ),
                                        Phantoms.var("pyArgs"),
                                    )
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeApplicationType",
        Phantoms.doc("Encode an application type to Python expression", body),
    )


def _encode_binding_as():
    # Helper: build a function definition from cs (CaseStatement), single param "x".
    # Used by both branches that fall back to "case elimination function".
    def case_elim_fn(cs_var):
        return _let_chain(
            [
                ("tname", Core.case_statement_type_name(Phantoms.var(cs_var))),
                ("dflt", Core.case_statement_default(Phantoms.var(cs_var))),
                ("cases_", Core.case_statement_cases(Phantoms.var(cs_var))),
            ],
            Eithers.bind(
                _ap(
                    _kref.resolution_require_union_type,
                    Phantoms.var("cx"),
                    _ap(_local("pythonEnvironmentGetGraph"), Phantoms.var("env")),
                    Phantoms.var("tname"),
                ),
                Phantoms.lam(
                    "rt",
                    _let_chain(
                        [
                            (
                                "isEnum",
                                _ap(
                                    _kref.predicates_is_enum_row_type,
                                    Phantoms.var("rt"),
                                ),
                            ),
                            (
                                "isFull",
                                _ap(
                                    _local("isCasesFull"),
                                    Phantoms.var("rt"),
                                    Phantoms.var("cases_"),
                                ),
                            ),
                            (
                                "innerParam",
                                PySyn.param(_py_name("x"), Phantoms.nothing()),
                            ),
                            (
                                "param",
                                Phantoms.record(
                                    Name("hydra.python.syntax.ParamNoDefault"),
                                    [
                                        Phantoms.field(
                                            Name("param"), Phantoms.var("innerParam")
                                        ),
                                        Phantoms.field(
                                            Name("typeComment"), Phantoms.nothing()
                                        ),
                                    ],
                                ),
                            ),
                            (
                                "params",
                                PySyn.parameters_param_no_default(
                                    Phantoms.record(
                                        Name(
                                            "hydra.python.syntax.ParamNoDefaultParameters"
                                        ),
                                        [
                                            Phantoms.field(
                                                Name("paramNoDefault"),
                                                Phantoms.list_(
                                                    [Phantoms.var("param")]
                                                ),
                                            ),
                                            Phantoms.field(
                                                Name("paramWithDefault"),
                                                Phantoms.list_([]),
                                            ),
                                            Phantoms.field(
                                                Name("starEtc"), Phantoms.nothing()
                                            ),
                                        ],
                                    )
                                ),
                            ),
                        ],
                        Eithers.bind(
                            Eithers.map_list(
                                _ap(
                                    _local("caseBlockToExpr"),
                                    Phantoms.var("cx"),
                                    Phantoms.var("env"),
                                    Phantoms.var("tname"),
                                    Phantoms.var("rt"),
                                    Phantoms.var("isEnum"),
                                    Phantoms.lambdas(
                                        ["e", "t"],
                                        _ap(
                                            _local("encodeTermMultiline"),
                                            Phantoms.var("cx"),
                                            Phantoms.var("e"),
                                            Phantoms.var("t"),
                                        ),
                                    ),
                                ),
                                Phantoms.var("cases_"),
                            ),
                            Phantoms.lam(
                                "pyCases",
                                Eithers.bind(
                                    _ap(
                                        _local("encodeDefaultCaseBlock"),
                                        Phantoms.lam(
                                            "t",
                                            _ap(
                                                _local("encodeTermInline"),
                                                Phantoms.var("cx"),
                                                Phantoms.var("env"),
                                                Phantoms.false(),
                                                Phantoms.var("t"),
                                            ),
                                        ),
                                        Phantoms.var("isFull"),
                                        Phantoms.var("dflt"),
                                        Phantoms.var("tname"),
                                    ),
                                    Phantoms.lam(
                                        "pyDflt",
                                        _let_chain(
                                            [
                                                (
                                                    "subj",
                                                    PySyn.subject_expression_simple(
                                                        PySyn.named_expression_simple(
                                                            _ap(
                                                                _kref.utils_py_name_to_py_expression,
                                                                _py_name("x"),
                                                            )
                                                        )
                                                    ),
                                                ),
                                                (
                                                    "allCases",
                                                    Lists.concat2(
                                                        Phantoms.var("pyCases"),
                                                        Phantoms.var("pyDflt"),
                                                    ),
                                                ),
                                                (
                                                    "matchStmt",
                                                    PySyn.statement_compound(
                                                        PySyn.compound_statement_match(
                                                            Phantoms.record(
                                                                Name(
                                                                    "hydra.python.syntax.MatchStatement"
                                                                ),
                                                                [
                                                                    Phantoms.field(
                                                                        Name("subject"),
                                                                        Phantoms.var(
                                                                            "subj"
                                                                        ),
                                                                    ),
                                                                    Phantoms.field(
                                                                        Name("cases"),
                                                                        Phantoms.var(
                                                                            "allCases"
                                                                        ),
                                                                    ),
                                                                ],
                                                            )
                                                        )
                                                    ),
                                                ),
                                                (
                                                    "body",
                                                    _ap(
                                                        _kref.utils_indented_block,
                                                        Phantoms.nothing(),
                                                        Phantoms.list_(
                                                            [
                                                                Phantoms.list_(
                                                                    [
                                                                        Phantoms.var(
                                                                            "matchStmt"
                                                                        )
                                                                    ]
                                                                )
                                                            ]
                                                        ),
                                                    ),
                                                ),
                                                (
                                                    "funcDefRaw",
                                                    Phantoms.record(
                                                        Name(
                                                            "hydra.python.syntax.FunctionDefRaw"
                                                        ),
                                                        [
                                                            Phantoms.field(
                                                                Name("async"),
                                                                Phantoms.false(),
                                                            ),
                                                            Phantoms.field(
                                                                Name("name"),
                                                                Phantoms.var("fname"),
                                                            ),
                                                            Phantoms.field(
                                                                Name("typeParams"),
                                                                Phantoms.list_([]),
                                                            ),
                                                            Phantoms.field(
                                                                Name("params"),
                                                                Phantoms.just(
                                                                    Phantoms.var(
                                                                        "params"
                                                                    )
                                                                ),
                                                            ),
                                                            Phantoms.field(
                                                                Name("returnType"),
                                                                Phantoms.nothing(),
                                                            ),
                                                            Phantoms.field(
                                                                Name(
                                                                    "funcTypeComment"
                                                                ),
                                                                Phantoms.nothing(),
                                                            ),
                                                            Phantoms.field(
                                                                Name("block"),
                                                                Phantoms.var("body"),
                                                            ),
                                                        ],
                                                    ),
                                                ),
                                            ],
                                            Phantoms.right(
                                                PySyn.statement_compound(
                                                    PySyn.compound_statement_function(
                                                        PySyn.function_definition(
                                                            Phantoms.nothing(),
                                                            Phantoms.var("funcDefRaw"),
                                                        )
                                                    )
                                                )
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        )

    # Helper: not-a-case-elimination fallback (encodeTermMultiline + take first).
    fallback = Eithers.bind(
        _ap(
            _local("encodeTermMultiline"),
            Phantoms.var("cx"),
            Phantoms.var("env"),
            Phantoms.var("term1"),
        ),
        Phantoms.lam(
            "stmts",
            Maybes.maybe(
                Phantoms.left(
                    Errors_dsl.error_other(
                        Errors_dsl.other_error(
                            Phantoms.string(
                                "encodeTermMultiline returned no statements"
                            )
                        )
                    )
                ),
                Phantoms.lam("x", Phantoms.right(Phantoms.var("x"))),
                Lists.maybe_head(Phantoms.var("stmts")),
            ),
        ),
    )

    no_ts_no_csa_branch = _let_chain(
        [
            ("mcs", _ap(_local("extractCaseElimination"), Phantoms.var("term1"))),
        ],
        Maybes.maybe(fallback, Phantoms.lam("cs", case_elim_fn("cs")), Phantoms.var("mcs")),
    )

    # Hoisted binding branch: with lambda params, use captured + match params
    hoisted_branch = Phantoms.lam(
        "csa",
        Logic.if_else(
            Lists.null(Phantoms.var("lambdaParams")),
            no_ts_no_csa_branch,
            _let_chain(
                [
                    ("tname", Pairs.first(Phantoms.var("csa"))),
                    ("rest1", Pairs.second(Phantoms.var("csa"))),
                    ("dflt", Pairs.first(Phantoms.var("rest1"))),
                    ("rest2", Pairs.second(Phantoms.var("rest1"))),
                    ("cases_", Pairs.first(Phantoms.var("rest2"))),
                ],
                Eithers.bind(
                    _ap(
                        _kref.resolution_require_union_type,
                        Phantoms.var("cx"),
                        _ap(
                            _local("pythonEnvironmentGetGraph"), Phantoms.var("env")
                        ),
                        Phantoms.var("tname"),
                    ),
                    Phantoms.lam(
                        "rt",
                        _let_chain(
                            [
                                (
                                    "isEnum",
                                    _ap(
                                        _kref.predicates_is_enum_row_type,
                                        Phantoms.var("rt"),
                                    ),
                                ),
                                (
                                    "isFull",
                                    _ap(
                                        _local("isCasesFull"),
                                        Phantoms.var("rt"),
                                        Phantoms.var("cases_"),
                                    ),
                                ),
                                (
                                    "capturedVarNames",
                                    Maybes.from_maybe(
                                        Phantoms.list_([]),
                                        Lists.maybe_init(Phantoms.var("lambdaParams")),
                                    ),
                                ),
                                (
                                    "matchLambdaParam",
                                    Maybes.from_maybe(
                                        Phantoms.wrap(
                                            Name("hydra.core.Name"),
                                            Phantoms.string(""),
                                        ),
                                        Lists.maybe_last(Phantoms.var("lambdaParams")),
                                    ),
                                ),
                                (
                                    "capturedParams",
                                    Lists.map(
                                        Phantoms.lam(
                                            "n",
                                            Phantoms.record(
                                                Name(
                                                    "hydra.python.syntax.ParamNoDefault"
                                                ),
                                                [
                                                    Phantoms.field(
                                                        Name("param"),
                                                        PySyn.param(
                                                            _ap(
                                                                _kref.names_encode_name,
                                                                Phantoms.false(),
                                                                _kref.util_case_convention_lower_snake,
                                                                Phantoms.var("env"),
                                                                Phantoms.var("n"),
                                                            ),
                                                            Phantoms.nothing(),
                                                        ),
                                                    ),
                                                    Phantoms.field(
                                                        Name("typeComment"),
                                                        Phantoms.nothing(),
                                                    ),
                                                ],
                                            ),
                                        ),
                                        Phantoms.var("capturedVarNames"),
                                    ),
                                ),
                                (
                                    "matchArgName",
                                    _ap(
                                        _kref.names_encode_name,
                                        Phantoms.false(),
                                        _kref.util_case_convention_lower_snake,
                                        Phantoms.var("env"),
                                        Phantoms.var("matchLambdaParam"),
                                    ),
                                ),
                                (
                                    "matchParam",
                                    Phantoms.record(
                                        Name(
                                            "hydra.python.syntax.ParamNoDefault"
                                        ),
                                        [
                                            Phantoms.field(
                                                Name("param"),
                                                PySyn.param(
                                                    Phantoms.var("matchArgName"),
                                                    Phantoms.nothing(),
                                                ),
                                            ),
                                            Phantoms.field(
                                                Name("typeComment"),
                                                Phantoms.nothing(),
                                            ),
                                        ],
                                    ),
                                ),
                                (
                                    "allParams",
                                    Lists.concat2(
                                        Phantoms.var("capturedParams"),
                                        Phantoms.list_([Phantoms.var("matchParam")]),
                                    ),
                                ),
                                (
                                    "params",
                                    PySyn.parameters_param_no_default(
                                        Phantoms.record(
                                            Name(
                                                "hydra.python.syntax.ParamNoDefaultParameters"
                                            ),
                                            [
                                                Phantoms.field(
                                                    Name("paramNoDefault"),
                                                    Phantoms.var("allParams"),
                                                ),
                                                Phantoms.field(
                                                    Name("paramWithDefault"),
                                                    Phantoms.list_([]),
                                                ),
                                                Phantoms.field(
                                                    Name("starEtc"),
                                                    Phantoms.nothing(),
                                                ),
                                            ],
                                        )
                                    ),
                                ),
                                (
                                    "envWithParams",
                                    _ap(
                                        _local("extendEnvWithLambdaParams"),
                                        Phantoms.var("env"),
                                        Phantoms.var("term1"),
                                    ),
                                ),
                            ],
                            Eithers.bind(
                                Eithers.map_list(
                                    _ap(
                                        _local("caseBlockToExpr"),
                                        Phantoms.var("cx"),
                                        Phantoms.var("envWithParams"),
                                        Phantoms.var("tname"),
                                        Phantoms.var("rt"),
                                        Phantoms.var("isEnum"),
                                        Phantoms.lambdas(
                                            ["e", "t"],
                                            _ap(
                                                _local("encodeTermMultiline"),
                                                Phantoms.var("cx"),
                                                Phantoms.var("e"),
                                                Phantoms.var("t"),
                                            ),
                                        ),
                                    ),
                                    Phantoms.var("cases_"),
                                ),
                                Phantoms.lam(
                                    "pyCases",
                                    Eithers.bind(
                                        _ap(
                                            _local("encodeDefaultCaseBlock"),
                                            Phantoms.lam(
                                                "t",
                                                _ap(
                                                    _local("encodeTermInline"),
                                                    Phantoms.var("cx"),
                                                    Phantoms.var("envWithParams"),
                                                    Phantoms.false(),
                                                    Phantoms.var("t"),
                                                ),
                                            ),
                                            Phantoms.var("isFull"),
                                            Phantoms.var("dflt"),
                                            Phantoms.var("tname"),
                                        ),
                                        Phantoms.lam(
                                            "pyDflt",
                                            _let_chain(
                                                [
                                                    (
                                                        "subj",
                                                        PySyn.subject_expression_simple(
                                                            PySyn.named_expression_simple(
                                                                _ap(
                                                                    _kref.utils_py_name_to_py_expression,
                                                                    Phantoms.var(
                                                                        "matchArgName"
                                                                    ),
                                                                )
                                                            )
                                                        ),
                                                    ),
                                                    (
                                                        "allCases",
                                                        Lists.concat2(
                                                            Phantoms.var("pyCases"),
                                                            Phantoms.var("pyDflt"),
                                                        ),
                                                    ),
                                                    (
                                                        "matchStmt",
                                                        PySyn.statement_compound(
                                                            PySyn.compound_statement_match(
                                                                Phantoms.record(
                                                                    Name(
                                                                        "hydra.python.syntax.MatchStatement"
                                                                    ),
                                                                    [
                                                                        Phantoms.field(
                                                                            Name(
                                                                                "subject"
                                                                            ),
                                                                            Phantoms.var(
                                                                                "subj"
                                                                            ),
                                                                        ),
                                                                        Phantoms.field(
                                                                            Name(
                                                                                "cases"
                                                                            ),
                                                                            Phantoms.var(
                                                                                "allCases"
                                                                            ),
                                                                        ),
                                                                    ],
                                                                )
                                                            )
                                                        ),
                                                    ),
                                                    (
                                                        "body",
                                                        _ap(
                                                            _kref.utils_indented_block,
                                                            Phantoms.nothing(),
                                                            Phantoms.list_(
                                                                [
                                                                    Phantoms.list_(
                                                                        [
                                                                            Phantoms.var(
                                                                                "matchStmt"
                                                                            )
                                                                        ]
                                                                    )
                                                                ]
                                                            ),
                                                        ),
                                                    ),
                                                    (
                                                        "funcDefRaw",
                                                        Phantoms.record(
                                                            Name(
                                                                "hydra.python.syntax.FunctionDefRaw"
                                                            ),
                                                            [
                                                                Phantoms.field(
                                                                    Name("async"),
                                                                    Phantoms.false(),
                                                                ),
                                                                Phantoms.field(
                                                                    Name("name"),
                                                                    Phantoms.var(
                                                                        "fname"
                                                                    ),
                                                                ),
                                                                Phantoms.field(
                                                                    Name("typeParams"),
                                                                    Phantoms.list_([]),
                                                                ),
                                                                Phantoms.field(
                                                                    Name("params"),
                                                                    Phantoms.just(
                                                                        Phantoms.var(
                                                                            "params"
                                                                        )
                                                                    ),
                                                                ),
                                                                Phantoms.field(
                                                                    Name("returnType"),
                                                                    Phantoms.nothing(),
                                                                ),
                                                                Phantoms.field(
                                                                    Name(
                                                                        "funcTypeComment"
                                                                    ),
                                                                    Phantoms.nothing(),
                                                                ),
                                                                Phantoms.field(
                                                                    Name("block"),
                                                                    Phantoms.var(
                                                                        "body"
                                                                    ),
                                                                ),
                                                            ],
                                                        ),
                                                    ),
                                                ],
                                                Phantoms.right(
                                                    PySyn.statement_compound(
                                                        PySyn.compound_statement_function(
                                                            PySyn.function_definition(
                                                                Phantoms.nothing(),
                                                                Phantoms.var(
                                                                    "funcDefRaw"
                                                                ),
                                                            )
                                                        )
                                                    )
                                                ),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )

    no_ts_branch = _let_chain(
        [
            ("gathered", _ap(_local("gatherLambdas"), Phantoms.var("term1"))),
            ("lambdaParams", Pairs.first(Phantoms.var("gathered"))),
            ("innerBody", Pairs.second(Phantoms.var("gathered"))),
            (
                "mcsa",
                _ap(
                    _local("isCaseStatementApplication"), Phantoms.var("innerBody")
                ),
            ),
        ],
        Maybes.maybe(no_ts_no_csa_branch, hoisted_branch, Phantoms.var("mcsa")),
    )

    has_ts_branch = Phantoms.lam(
        "ts",
        Eithers.bind(
            _ap(
                _kref.annotations_get_term_description,
                Phantoms.var("cx"),
                _ap(_local("pythonEnvironmentGetGraph"), Phantoms.var("env")),
                Phantoms.var("term1"),
            ),
            Phantoms.lam(
                "comment",
                _let_chain(
                    [
                        (
                            "normComment",
                            Maybes.map(
                                _kref.formatting_normalize_comment,
                                Phantoms.var("comment"),
                            ),
                        ),
                    ],
                    _ap(
                        _local("encodeTermAssignment"),
                        Phantoms.var("cx"),
                        Phantoms.var("env"),
                        Phantoms.var("name1"),
                        Phantoms.var("term1"),
                        Phantoms.var("ts"),
                        Phantoms.var("normComment"),
                    ),
                ),
            ),
        ),
    )

    body = Phantoms.lambdas(
        ["cx", "env", "binding"],
        _let_chain(
            [
                ("name1", Core.binding_name(Phantoms.var("binding"))),
                ("term1", Core.binding_term(Phantoms.var("binding"))),
                ("mts", Core.binding_type_scheme(Phantoms.var("binding"))),
                (
                    "fname",
                    _ap(
                        _kref.names_encode_name,
                        Phantoms.true(),
                        _kref.util_case_convention_lower_snake,
                        Phantoms.var("env"),
                        Phantoms.var("name1"),
                    ),
                ),
            ],
            Maybes.maybe(no_ts_branch, has_ts_branch, Phantoms.var("mts")),
        ),
    )
    return _def(
        "encodeBindingAs",
        Phantoms.doc(
            "Encode a binding as a Python statement (function definition or assignment)",
            body,
        ),
    )


def _encode_binding_as_assignment():
    body = Phantoms.lambdas(
        ["cx", "allowThunking", "env", "binding"],
        _let_chain(
            [
                ("name", Core.binding_name(Phantoms.var("binding"))),
                ("term", Core.binding_term(Phantoms.var("binding"))),
                ("mts", Core.binding_type_scheme(Phantoms.var("binding"))),
                (
                    "pyName",
                    _ap(
                        _kref.names_encode_name,
                        Phantoms.false(),
                        _kref.util_case_convention_lower_snake,
                        Phantoms.var("env"),
                        Phantoms.var("name"),
                    ),
                ),
            ],
            Eithers.bind(
                _ap(
                    _local("encodeTermInline"),
                    Phantoms.var("cx"),
                    Phantoms.var("env"),
                    Phantoms.false(),
                    Phantoms.var("term"),
                ),
                Phantoms.lam(
                    "pbody",
                    _let_chain(
                        [
                            ("tc", _env("graph", "env")),
                            (
                                "isComplexVar",
                                _ap(
                                    _kref.predicates_is_complex_variable,
                                    Phantoms.var("tc"),
                                    Phantoms.var("name"),
                                ),
                            ),
                            (
                                "termIsComplex",
                                _ap(
                                    _kref.predicates_is_complex_term,
                                    Phantoms.var("tc"),
                                    Phantoms.var("term"),
                                ),
                            ),
                            (
                                "isTrivial",
                                _ap(
                                    _kref.predicates_is_trivial_term,
                                    Phantoms.var("term"),
                                ),
                            ),
                            (
                                "needsThunk",
                                Logic.if_else(
                                    Phantoms.var("isTrivial"),
                                    Phantoms.boolean(False),
                                    Maybes.maybe(
                                        Logic.and_(
                                            Phantoms.var("allowThunking"),
                                            Logic.or_(
                                                Phantoms.var("isComplexVar"),
                                                Phantoms.var("termIsComplex"),
                                            ),
                                        ),
                                        Phantoms.lam(
                                            "ts",
                                            Logic.and_(
                                                Phantoms.var("allowThunking"),
                                                Logic.and_(
                                                    Equality.equal(
                                                        _ap(
                                                            _kref.arity_type_scheme_arity,
                                                            Phantoms.var("ts"),
                                                        ),
                                                        Phantoms.int_(0),
                                                    ),
                                                    Logic.or_(
                                                        Phantoms.var("isComplexVar"),
                                                        Phantoms.var("termIsComplex"),
                                                    ),
                                                ),
                                            ),
                                        ),
                                        Phantoms.var("mts"),
                                    ),
                                ),
                            ),
                            (
                                "pterm",
                                Logic.if_else(
                                    Phantoms.var("needsThunk"),
                                    _ap(_local("makeThunk"), Phantoms.var("pbody")),
                                    Phantoms.var("pbody"),
                                ),
                            ),
                        ],
                        Phantoms.right(
                            PySyn.named_expression_assignment(
                                PySyn.assignment_expression(
                                    Phantoms.var("pyName"), Phantoms.var("pterm")
                                )
                            )
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeBindingAsAssignment",
        Phantoms.doc(
            "Encode a binding as a walrus operator assignment", body
        ),
    )


def _encode_bindings_as_defs():
    body = Phantoms.lambdas(
        ["env", "encodeBinding", "bindings"],
        Eithers.map_list(
            _ap(Phantoms.var("encodeBinding"), Phantoms.var("env")),
            Phantoms.var("bindings"),
        ),
    )
    return _def(
        "encodeBindingsAsDefs",
        Phantoms.doc("Encode bindings as function definitions", body),
    )


def _encode_default_case_block():
    body = Phantoms.lambdas(
        ["termToExpr", "isFull", "mdflt", "tname"],
        Eithers.bind(
            Maybes.maybe(
                Phantoms.right(
                    Logic.if_else(
                        Phantoms.var("isFull"),
                        _ap(
                            _kref.utils_raise_assertion_error,
                            Phantoms.string("Unreachable: all variants handled"),
                        ),
                        _ap(
                            _kref.utils_raise_type_error,
                            Strings.cat2(
                                Phantoms.string("Unsupported "),
                                _ap(
                                    _kref.names_local_name_of,
                                    Phantoms.var("tname"),
                                ),
                            ),
                        ),
                    )
                ),
                Phantoms.lam(
                    "d",
                    Eithers.bind(
                        _ap(Phantoms.var("termToExpr"), Phantoms.var("d")),
                        Phantoms.lam(
                            "pyexpr",
                            Phantoms.right(
                                _ap(_kref.utils_return_single, Phantoms.var("pyexpr"))
                            ),
                        ),
                    ),
                ),
                Phantoms.var("mdflt"),
            ),
            Phantoms.lam(
                "stmt",
                _let_chain(
                    [
                        (
                            "patterns",
                            _ap(
                                _kref.utils_py_closed_pattern_to_py_patterns,
                                PySyn.closed_pattern_wildcard,
                            ),
                        ),
                        (
                            "body",
                            _ap(
                                _kref.utils_indented_block,
                                Phantoms.nothing(),
                                Phantoms.list_(
                                    [Phantoms.list_([Phantoms.var("stmt")])]
                                ),
                            ),
                        ),
                    ],
                    Phantoms.right(
                        Phantoms.list_(
                            [
                                PySyn.case_block(
                                    Phantoms.var("patterns"),
                                    Phantoms.nothing(),
                                    Phantoms.var("body"),
                                )
                            ]
                        )
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeDefaultCaseBlock",
        Phantoms.doc(
            "Encode the default (wildcard) case block for a match statement", body
        ),
    )


def _encode_definition():
    term_branch = _let_chain(
        [
            ("name", _proj("hydra.packaging.TermDefinition", "name", "td")),
            ("term", _proj("hydra.packaging.TermDefinition", "term", "td")),
            (
                "typ",
                Maybes.maybe(
                    Core.type_scheme(
                        Phantoms.list_([]),
                        Core.type_variable(
                            Phantoms.wrap(
                                Name("hydra.core.Name"),
                                Phantoms.string("hydra.core.Unit"),
                            )
                        ),
                        Phantoms.nothing(),
                    ),
                    Phantoms.lam("x", Phantoms.var("x")),
                    _proj("hydra.packaging.TermDefinition", "typeScheme", "td"),
                ),
            ),
        ],
        Eithers.bind(
            _ap(
                _kref.annotations_get_term_description,
                Phantoms.var("cx"),
                _ap(_local("pythonEnvironmentGetGraph"), Phantoms.var("env")),
                Phantoms.var("term"),
            ),
            Phantoms.lam(
                "comment",
                _let_chain(
                    [
                        (
                            "normComment",
                            Maybes.map(
                                _kref.formatting_normalize_comment,
                                Phantoms.var("comment"),
                            ),
                        ),
                    ],
                    Eithers.bind(
                        _ap(
                            _local("encodeTermAssignment"),
                            Phantoms.var("cx"),
                            Phantoms.var("env"),
                            Phantoms.var("name"),
                            Phantoms.var("term"),
                            Phantoms.var("typ"),
                            Phantoms.var("normComment"),
                        ),
                        Phantoms.lam(
                            "stmt",
                            Phantoms.right(
                                Phantoms.list_(
                                    [Phantoms.list_([Phantoms.var("stmt")])]
                                )
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    type_branch = _let_chain(
        [
            ("name", _proj("hydra.packaging.TypeDefinition", "name", "td")),
            (
                "typ",
                Core.type_scheme_body(
                    _proj("hydra.packaging.TypeDefinition", "typeScheme", "td")
                ),
            ),
        ],
        Eithers.bind(
            _ap(
                _kref.annotations_get_type_description,
                Phantoms.var("cx"),
                _ap(_local("pythonEnvironmentGetGraph"), Phantoms.var("env")),
                Phantoms.var("typ"),
            ),
            Phantoms.lam(
                "comment",
                _let_chain(
                    [
                        (
                            "normComment",
                            Maybes.map(
                                _kref.formatting_normalize_comment,
                                Phantoms.var("comment"),
                            ),
                        ),
                    ],
                    _ap(
                        _local("encodeTypeAssignment"),
                        Phantoms.var("cx"),
                        Phantoms.var("env"),
                        Phantoms.var("name"),
                        Phantoms.var("typ"),
                        Phantoms.var("normComment"),
                    ),
                ),
            ),
        ),
    )
    body = Phantoms.lambdas(
        ["cx", "env", "def_"],
        Phantoms.cases(
            Name("hydra.packaging.Definition"),
            Phantoms.var("def_"),
            Nothing(),
            [
                Phantoms.field(
                    Name("term"),
                    Phantoms.lam("td", term_branch),
                ),
                Phantoms.field(
                    Name("type"),
                    Phantoms.lam("td", type_branch),
                ),
            ],
        ),
    )
    return _def(
        "encodeDefinition",
        Phantoms.doc("Encode a definition (term or type) to Python statements", body),
    )


def _encode_enum_value_assignment():
    body = Phantoms.lambdas(
        ["cx", "env", "fieldType"],
        _let_chain(
            [
                ("fname", Core.field_type_name(Phantoms.var("fieldType"))),
                ("ftype", Core.field_type_type(Phantoms.var("fieldType"))),
            ],
            Eithers.bind(
                _ap(
                    _kref.annotations_get_type_description,
                    Phantoms.var("cx"),
                    _ap(_local("pythonEnvironmentGetGraph"), Phantoms.var("env")),
                    Phantoms.var("ftype"),
                ),
                Phantoms.lam(
                    "mcomment",
                    _let_chain(
                        [
                            (
                                "pyName",
                                _ap(
                                    _kref.names_encode_enum_value,
                                    Phantoms.var("env"),
                                    Phantoms.var("fname"),
                                ),
                            ),
                            ("fnameStr", Core.un_name(Phantoms.var("fname"))),
                            (
                                "pyValue",
                                _ap(
                                    _kref.utils_function_call,
                                    _ap(
                                        _kref.utils_py_name_to_py_primary,
                                        _ap(
                                            _kref.names_encode_name,
                                            Phantoms.true(),
                                            _kref.util_case_convention_pascal,
                                            Phantoms.var("env"),
                                            Core.name(Phantoms.string("hydra.core.Name")),
                                        ),
                                    ),
                                    Phantoms.list_(
                                        [
                                            _ap(
                                                _kref.utils_double_quoted_string,
                                                Phantoms.var("fnameStr"),
                                            )
                                        ]
                                    ),
                                ),
                            ),
                            (
                                "assignStmt",
                                _ap(
                                    _kref.utils_assignment_statement,
                                    Phantoms.var("pyName"),
                                    Phantoms.var("pyValue"),
                                ),
                            ),
                        ],
                        Phantoms.right(
                            Maybes.maybe(
                                Phantoms.list_([Phantoms.var("assignStmt")]),
                                Phantoms.lam(
                                    "c",
                                    Phantoms.list_(
                                        [
                                            Phantoms.var("assignStmt"),
                                            _ap(
                                                _kref.utils_py_expression_to_py_statement,
                                                _ap(
                                                    _kref.utils_triple_quoted_string,
                                                    Phantoms.var("c"),
                                                ),
                                            ),
                                        ]
                                    ),
                                ),
                                Phantoms.var("mcomment"),
                            )
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeEnumValueAssignment",
        Phantoms.doc(
            "Encode an enum value assignment statement with optional comment", body
        ),
    )


def _encode_field():
    body = Phantoms.lambdas(
        ["cx", "env", "field", "termToExpr"],
        _let_chain(
            [
                ("fname", Core.field_name(Phantoms.var("field"))),
                ("fterm", Core.field_term(Phantoms.var("field"))),
            ],
            Eithers.bind(
                _ap(Phantoms.var("termToExpr"), Phantoms.var("fterm")),
                Phantoms.lam(
                    "pterm",
                    Phantoms.right(
                        Phantoms.pair(
                            _ap(
                                _kref.names_encode_field_name,
                                Phantoms.var("env"),
                                Phantoms.var("fname"),
                            ),
                            Phantoms.var("pterm"),
                        )
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeField",
        Phantoms.doc(
            "Encode a field (name-value pair) to a Python (Name, Expression) pair", body
        ),
    )


def _encode_field_type():
    body = Phantoms.lambdas(
        ["cx", "env", "fieldType"],
        _let_chain(
            [
                ("fname", Core.field_type_name(Phantoms.var("fieldType"))),
                ("ftype", Core.field_type_type(Phantoms.var("fieldType"))),
            ],
            Eithers.bind(
                _ap(
                    _kref.annotations_get_type_description,
                    Phantoms.var("cx"),
                    _ap(_local("pythonEnvironmentGetGraph"), Phantoms.var("env")),
                    Phantoms.var("ftype"),
                ),
                Phantoms.lam(
                    "comment",
                    _let_chain(
                        [
                            (
                                "pyName",
                                PySyn.single_target_name(
                                    _ap(
                                        _kref.names_encode_field_name,
                                        Phantoms.var("env"),
                                        Phantoms.var("fname"),
                                    )
                                ),
                            ),
                        ],
                        Eithers.bind(
                            _ap(
                                _local("encodeType"),
                                Phantoms.var("env"),
                                Phantoms.var("ftype"),
                            ),
                            Phantoms.lam(
                                "pyType",
                                _let_chain(
                                    [
                                        (
                                            "annotatedPyType",
                                            _ap(
                                                _kref.utils_annotated_expression,
                                                Phantoms.var("comment"),
                                                Phantoms.var("pyType"),
                                            ),
                                        ),
                                    ],
                                    Phantoms.right(
                                        _ap(
                                            _kref.utils_py_assignment_to_py_statement,
                                            PySyn.assignment_typed(
                                                PySyn.typed_assignment(
                                                    Phantoms.var("pyName"),
                                                    Phantoms.var("annotatedPyType"),
                                                    Phantoms.nothing(),
                                                )
                                            ),
                                        )
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeFieldType",
        Phantoms.doc(
            "Encode a field type for record definitions (field: type annotation)",
            body,
        ),
    )


def _encode_float_value():
    body = Phantoms.lambdas(
        ["fv"],
        Phantoms.cases(
            Name("hydra.core.FloatValue"),
            Phantoms.var("fv"),
            Nothing(),
            [
                Phantoms.field(
                    Name("bigfloat"),
                    Phantoms.lam(
                        "f",
                        Phantoms.right(
                            _ap(
                                _kref.utils_function_call,
                                _ap(_kref.utils_py_name_to_py_primary, _py_name("Decimal")),
                                Phantoms.list_(
                                    [
                                        _ap(
                                            _kref.utils_single_quoted_string,
                                            Literals.show_bigfloat(Phantoms.var("f")),
                                        )
                                    ]
                                ),
                            )
                        ),
                    ),
                ),
                Phantoms.field(
                    Name("float32"),
                    Phantoms.lam(
                        "f",
                        _ap(_local("encodeFloatValue_encodeFloat32"), Phantoms.var("f")),
                    ),
                ),
                Phantoms.field(
                    Name("float64"),
                    Phantoms.lam(
                        "f",
                        _ap(_local("encodeFloatValue_encodeFloat64"), Phantoms.var("f")),
                    ),
                ),
            ],
        ),
    )
    return _def(
        "encodeFloatValue",
        Phantoms.doc("Encode a float value to a Python expression", body),
    )


def _encode_float_value_encode_float32():
    body = Phantoms.lambdas(
        ["v"],
        _let_chain(
            [("s", Literals.show_float32(Phantoms.var("v")))],
            Logic.if_else(
                Equality.equal(Phantoms.var("s"), Phantoms.string("NaN")),
                Phantoms.right(
                    _ap(_local("encodeFloatValue_pySpecialFloat"), Phantoms.string("nan"))
                ),
                Logic.if_else(
                    Equality.equal(Phantoms.var("s"), Phantoms.string("Infinity")),
                    Phantoms.right(
                        _ap(_local("encodeFloatValue_pySpecialFloat"), Phantoms.string("inf"))
                    ),
                    Logic.if_else(
                        Equality.equal(Phantoms.var("s"), Phantoms.string("-Infinity")),
                        Phantoms.right(
                            _ap(
                                _local("encodeFloatValue_pySpecialFloat"),
                                Phantoms.string("-inf"),
                            )
                        ),
                        Phantoms.right(
                            _ap(
                                _kref.utils_py_atom_to_py_expression,
                                PySyn.atom_number(
                                    PySyn.number_float(
                                        Literals.float32_to_bigfloat(Phantoms.var("v"))
                                    )
                                ),
                            )
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def("encodeFloatValue_encodeFloat32", body)


def _encode_float_value_encode_float64():
    body = Phantoms.lambdas(
        ["v"],
        _let_chain(
            [("s", Literals.show_float64(Phantoms.var("v")))],
            Logic.if_else(
                Equality.equal(Phantoms.var("s"), Phantoms.string("NaN")),
                Phantoms.right(
                    _ap(_local("encodeFloatValue_pySpecialFloat"), Phantoms.string("nan"))
                ),
                Logic.if_else(
                    Equality.equal(Phantoms.var("s"), Phantoms.string("Infinity")),
                    Phantoms.right(
                        _ap(_local("encodeFloatValue_pySpecialFloat"), Phantoms.string("inf"))
                    ),
                    Logic.if_else(
                        Equality.equal(Phantoms.var("s"), Phantoms.string("-Infinity")),
                        Phantoms.right(
                            _ap(
                                _local("encodeFloatValue_pySpecialFloat"),
                                Phantoms.string("-inf"),
                            )
                        ),
                        Logic.if_else(
                            Equality.equal(Phantoms.var("s"), Phantoms.string("-0.0")),
                            Phantoms.right(
                                _ap(
                                    _local("encodeFloatValue_pySpecialFloat"),
                                    Phantoms.string("-0.0"),
                                )
                            ),
                            Phantoms.right(
                                _ap(
                                    _kref.utils_py_atom_to_py_expression,
                                    PySyn.atom_number(
                                        PySyn.number_float(
                                            Literals.float64_to_bigfloat(Phantoms.var("v"))
                                        )
                                    ),
                                )
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def("encodeFloatValue_encodeFloat64", body)


def _encode_float_value_py_special_float():
    body = Phantoms.lambdas(
        ["value"],
        _ap(
            _kref.utils_function_call,
            _ap(_kref.utils_py_name_to_py_primary, _py_name("float")),
            Phantoms.list_(
                [_ap(_kref.utils_single_quoted_string, Phantoms.var("value"))]
            ),
        ),
    )
    return _def("encodeFloatValue_pySpecialFloat", body)


def _encode_name_constants():
    to_stmt = Phantoms.lam(
        "pair",
        _ap(
            _kref.utils_assignment_statement,
            Pairs.first(Phantoms.var("pair")),
            _ap(
                _kref.utils_function_call,
                _ap(
                    _kref.utils_py_name_to_py_primary,
                    _ap(
                        _kref.names_encode_name,
                        Phantoms.true(),
                        _kref.util_case_convention_pascal,
                        Phantoms.var("env"),
                        Core.name(Phantoms.string("hydra.core.Name")),
                    ),
                ),
                Phantoms.list_(
                    [
                        _ap(
                            _kref.utils_double_quoted_string,
                            Core.un_name(Pairs.second(Phantoms.var("pair"))),
                        )
                    ]
                ),
            ),
        ),
    )
    body = Phantoms.lambdas(
        ["env", "name", "fields"],
        _let_chain(
            [
                ("toStmt", to_stmt),
                (
                    "namePair",
                    Phantoms.pair(
                        _ap(
                            _kref.names_encode_constant_for_type_name,
                            Phantoms.var("env"),
                            Phantoms.var("name"),
                        ),
                        Phantoms.var("name"),
                    ),
                ),
                (
                    "fieldPairs",
                    Lists.map(
                        Phantoms.lam(
                            "field",
                            Phantoms.pair(
                                _ap(
                                    _kref.names_encode_constant_for_field_name,
                                    Phantoms.var("env"),
                                    Phantoms.var("name"),
                                    _proj(
                                        "hydra.core.FieldType",
                                        "name",
                                        "field",
                                    ),
                                ),
                                _proj("hydra.core.FieldType", "name", "field"),
                            ),
                        ),
                        Phantoms.var("fields"),
                    ),
                ),
            ],
            Lists.map(
                Phantoms.var("toStmt"),
                Lists.cons(Phantoms.var("namePair"), Phantoms.var("fieldPairs")),
            ),
        ),
    )
    return _def(
        "encodeNameConstants",
        Phantoms.doc(
            "Generate name constants for a type as class-level attributes", body
        ),
    )


def _encode_python_module():
    post_defStmts_body = _let_chain(
        [
            (
                "meta2",
                Logic.if_else(
                    Logic.and_(
                        Logic.not_(Phantoms.var("isTypeMod")),
                        _local("useInlineTypeParams"),
                    ),
                    _ap(
                        _local("setMetaUsesTypeVar"),
                        Phantoms.var("meta0"),
                        Phantoms.false(),
                    ),
                    Phantoms.var("meta0"),
                ),
            ),
            (
                "meta",
                Logic.if_else(
                    Logic.and_(
                        Phantoms.var("isTypeMod"),
                        Equality.equal(
                            _local("targetPythonVersion"),
                            Phantoms.inject_unit(
                                Name("hydra.python.environment.PythonVersion"),
                                Name("python310"),
                            ),
                        ),
                    ),
                    _ap(
                        _local("setMetaUsesTypeAlias"),
                        Phantoms.var("meta2"),
                        Phantoms.true(),
                    ),
                    Phantoms.var("meta2"),
                ),
            ),
            (
                "namespaces",
                _meta_proj("namespaces", "meta0"),
            ),
            (
                "commentStmts",
                Maybes.maybe(
                    Phantoms.list_([]),
                    Phantoms.lam(
                        "c",
                        Phantoms.list_(
                            [
                                _ap(
                                    _kref.utils_comment_statement,
                                    Phantoms.var("c"),
                                )
                            ]
                        ),
                    ),
                    Maybes.map(
                        _kref.formatting_normalize_comment,
                        Pkg.module_description(Phantoms.var("mod")),
                    ),
                ),
            ),
            (
                "importStmts",
                _ap(
                    _local("moduleImports"),
                    Phantoms.var("namespaces"),
                    Phantoms.var("meta"),
                ),
            ),
            (
                "tvars",
                Logic.if_else(
                    Logic.or_(
                        Phantoms.var("isTypeMod"),
                        Logic.not_(_local("useInlineTypeParams")),
                    ),
                    _meta_proj("typeVariables", "meta"),
                    Sets.empty(),
                ),
            ),
            (
                "tvarStmts",
                Lists.map(
                    Phantoms.lam(
                        "tv",
                        _ap(
                            _local("tvarStatement"),
                            _ap(
                                _kref.names_encode_type_variable, Phantoms.var("tv")
                            ),
                        ),
                    ),
                    Sets.to_list(Phantoms.var("tvars")),
                ),
            ),
        ],
        _let_chain(
            [
                (
                    "body",
                    Lists.filter(
                        Phantoms.lam(
                            "group",
                            Logic.not_(Lists.null(Phantoms.var("group"))),
                        ),
                        Lists.concat(
                            Phantoms.list_(
                                [
                                    Phantoms.list_(
                                        [
                                            Phantoms.var("commentStmts"),
                                            Phantoms.var("importStmts"),
                                            Phantoms.var("tvarStmts"),
                                        ]
                                    ),
                                    Phantoms.var("defStmts"),
                                ]
                            )
                        ),
                    ),
                ),
            ],
            Phantoms.right(PySyn.module(Phantoms.var("body"))),
        ),
    )
    inner_body = Eithers.bind(
        Eithers.map_(
            Phantoms.lam("xs", Lists.concat(Phantoms.var("xs"))),
            Eithers.map_list(
                Phantoms.lam(
                    "d",
                    _ap(
                        _local("encodeDefinition"),
                        Phantoms.var("cx"),
                        Phantoms.var("env"),
                        Phantoms.var("d"),
                    ),
                ),
                Phantoms.var("defs"),
            ),
        ),
        Phantoms.lam("defStmts", post_defStmts_body),
    )
    body = Phantoms.lambdas(
        ["cx", "g", "mod", "defs0"],
        _let_chain(
            [
                (
                    "defs",
                    _ap(_kref.environment_reorder_defs, Phantoms.var("defs0")),
                ),
                (
                    "meta0",
                    _ap(
                        _local("gatherMetadata"),
                        Pkg.module_namespace(Phantoms.var("mod")),
                        Phantoms.var("defs"),
                    ),
                ),
                (
                    "namespaces0",
                    _meta_proj("namespaces", "meta0"),
                ),
                (
                    "env0",
                    _ap(
                        _local("initialEnvironment"),
                        Phantoms.var("namespaces0"),
                        Phantoms.var("g"),
                    ),
                ),
                (
                    "isTypeMod",
                    _ap(_local("isTypeModuleCheck"), Phantoms.var("defs0")),
                ),
            ],
            _ap(
                _local("withDefinitions"),
                Phantoms.var("env0"),
                Phantoms.var("defs"),
                Phantoms.lam("env", inner_body),
            ),
        ),
    )
    return _def(
        "encodePythonModule",
        Phantoms.doc("Encode a Hydra module to a Python module AST", body),
    )


def _encode_record_type():
    body = Phantoms.lambdas(
        ["cx", "env", "name", "rowType", "comment"],
        Eithers.bind(
            Eithers.map_list(
                _ap(
                    _local("encodeFieldType"),
                    Phantoms.var("cx"),
                    Phantoms.var("env"),
                ),
                Phantoms.var("rowType"),
            ),
            Phantoms.lam(
                "pyFields",
                _let_chain(
                    [
                        (
                            "constStmts",
                            _ap(
                                _local("encodeNameConstants"),
                                Phantoms.var("env"),
                                Phantoms.var("name"),
                                Phantoms.var("rowType"),
                            ),
                        ),
                        (
                            "body",
                            _ap(
                                _kref.utils_indented_block,
                                Phantoms.var("comment"),
                                Phantoms.list_(
                                    [
                                        Phantoms.var("pyFields"),
                                        Phantoms.var("constStmts"),
                                    ]
                                ),
                            ),
                        ),
                        ("boundVars", _env("boundTypeVariables", "env")),
                        ("tparamList", Pairs.first(Phantoms.var("boundVars"))),
                        (
                            "mGenericArg",
                            _ap(_local("genericArg"), Phantoms.var("tparamList")),
                        ),
                        (
                            "args",
                            Maybes.maybe(
                                Phantoms.nothing(),
                                Phantoms.lam(
                                    "a",
                                    Phantoms.just(
                                        _ap(
                                            _kref.utils_py_expressions_to_py_args,
                                            Phantoms.list_([Phantoms.var("a")]),
                                        )
                                    ),
                                ),
                                Phantoms.var("mGenericArg"),
                            ),
                        ),
                        (
                            "decs",
                            Phantoms.just(
                                Phantoms.wrap(
                                    Name("hydra.python.syntax.Decorators"),
                                    Phantoms.list_([_local("dataclassDecorator")]),
                                )
                            ),
                        ),
                        (
                            "pyName",
                            _ap(
                                _kref.names_encode_name,
                                Phantoms.false(),
                                _kref.util_case_convention_pascal,
                                Phantoms.var("env"),
                                Phantoms.var("name"),
                            ),
                        ),
                        ("noTypeParams", Phantoms.list_([])),
                    ],
                    Phantoms.right(
                        _ap(
                            _kref.utils_py_class_definition_to_py_statement,
                            Phantoms.record(
                                Name("hydra.python.syntax.ClassDefinition"),
                                [
                                    Phantoms.field(
                                        Name("decorators"), Phantoms.var("decs")
                                    ),
                                    Phantoms.field(
                                        Name("name"), Phantoms.var("pyName")
                                    ),
                                    Phantoms.field(
                                        Name("typeParams"),
                                        Phantoms.var("noTypeParams"),
                                    ),
                                    Phantoms.field(
                                        Name("arguments"), Phantoms.var("args")
                                    ),
                                    Phantoms.field(
                                        Name("body"), Phantoms.var("body")
                                    ),
                                ],
                            ),
                        )
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeRecordType",
        Phantoms.doc("Encode a record type as a Python dataclass", body),
    )


def _encode_type_def_single():
    body = Phantoms.lambdas(
        ["env", "name", "comment", "typeExpr"],
        _let_chain(
            [
                (
                    "pyName",
                    _ap(
                        _kref.names_encode_name,
                        Phantoms.false(),
                        _kref.util_case_convention_pascal,
                        Phantoms.var("env"),
                        Phantoms.var("name"),
                    ),
                ),
                (
                    "tparams",
                    _ap(_local("environmentTypeParameters"), Phantoms.var("env")),
                ),
            ],
            Phantoms.list_(
                [
                    _ap(
                        _local("typeAliasStatementFor"),
                        Phantoms.var("env"),
                        Phantoms.var("pyName"),
                        Phantoms.var("tparams"),
                        Phantoms.var("comment"),
                        Phantoms.var("typeExpr"),
                    )
                ]
            ),
        ),
    )
    return _def(
        "encodeTypeDefSingle",
        Phantoms.doc("Encode a simple type alias definition", body),
    )


def _encode_type_quoted():
    body = Phantoms.lambdas(
        ["env", "typ"],
        Eithers.bind(
            _ap(_local("encodeType"), Phantoms.var("env"), Phantoms.var("typ")),
            Phantoms.lam(
                "pytype",
                Phantoms.right(
                    Logic.if_else(
                        Sets.null(
                            _ap(
                                _kref.variables_free_variables_in_type,
                                Phantoms.var("typ"),
                            )
                        ),
                        Phantoms.var("pytype"),
                        _ap(
                            _kref.utils_double_quoted_string,
                            _ap(
                                Phantoms.var("hydra.serialization.printExpr"),
                                _ap(
                                    Phantoms.var("hydra.python.serde.expressionToExpr"),
                                    Phantoms.var("pytype"),
                                ),
                            ),
                        ),
                    )
                ),
            ),
        ),
    )
    return _def(
        "encodeTypeQuoted",
        Phantoms.doc(
            "Encode a type to a Python expression, quoting if the type has free variables",
            body,
        ),
    )


def _encode_term_assignment():
    def fs_proj(field):
        return _proj("hydra.typing.FunctionStructure", field, "fs")

    body = Phantoms.lambdas(
        ["cx", "env", "name", "term", "ts", "comment"],
        Eithers.bind(
            _ap(
                _local("analyzePythonFunction"),
                Phantoms.var("cx"),
                Phantoms.var("env"),
                Phantoms.var("term"),
            ),
            Phantoms.lam(
                "fs",
                _let_chain(
                    [
                        ("tparams", fs_proj("typeParams")),
                        ("params", fs_proj("params")),
                        ("bindings", fs_proj("bindings")),
                        ("body", fs_proj("body")),
                        ("doms", fs_proj("domains")),
                        ("mcod", fs_proj("codomain")),
                        ("env2", fs_proj("environment")),
                        ("tc", _env("graph", "env2")),
                        (
                            "binding",
                            Core.binding(
                                Phantoms.var("name"),
                                Phantoms.var("term"),
                                Phantoms.just(Phantoms.var("ts")),
                            ),
                        ),
                        (
                            "isComplex",
                            _ap(
                                _kref.predicates_is_complex_binding,
                                Phantoms.var("tc"),
                                Phantoms.var("binding"),
                            ),
                        ),
                        (
                            "isTrivial",
                            _ap(
                                _kref.predicates_is_trivial_term,
                                Phantoms.var("term"),
                            ),
                        ),
                    ],
                    Logic.if_else(
                        Logic.and_(
                            Phantoms.var("isComplex"),
                            Logic.not_(Phantoms.var("isTrivial")),
                        ),
                        Eithers.bind(
                            Eithers.map_list(
                                _ap(
                                    _local("encodeBindingAs"),
                                    Phantoms.var("cx"),
                                    Phantoms.var("env2"),
                                ),
                                Phantoms.var("bindings"),
                            ),
                            Phantoms.lam(
                                "bindingStmts",
                                _ap(
                                    _local("functionDefinitionToExpr"),
                                    Phantoms.var("cx"),
                                    Phantoms.var("env2"),
                                    Phantoms.var("name"),
                                    Phantoms.var("tparams"),
                                    Phantoms.var("params"),
                                    Phantoms.var("body"),
                                    Phantoms.var("doms"),
                                    Phantoms.var("mcod"),
                                    Phantoms.var("comment"),
                                    Phantoms.var("bindingStmts"),
                                ),
                            ),
                        ),
                        Eithers.bind(
                            _ap(
                                _local("encodeTermInline"),
                                Phantoms.var("cx"),
                                Phantoms.var("env2"),
                                Phantoms.false(),
                                Phantoms.var("body"),
                            ),
                            Phantoms.lam(
                                "bodyExpr",
                                _let_chain(
                                    [
                                        (
                                            "pyName",
                                            _ap(
                                                _kref.names_encode_name,
                                                Phantoms.false(),
                                                _kref.util_case_convention_lower_snake,
                                                Phantoms.var("env2"),
                                                Phantoms.var("name"),
                                            ),
                                        ),
                                    ],
                                    Phantoms.right(
                                        _ap(
                                            _kref.utils_annotated_statement,
                                            Phantoms.var("comment"),
                                            _ap(
                                                _kref.utils_assignment_statement,
                                                Phantoms.var("pyName"),
                                                Phantoms.var("bodyExpr"),
                                            ),
                                        )
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeTermAssignment",
        Phantoms.doc("Encode a term assignment to a Python statement", body),
    )


def _encode_term_inline():
    def fs_proj(field):
        return _proj("hydra.typing.FunctionStructure", field, "fs")

    encode_helper = Phantoms.lam(
        "t",
        _ap(
            _local("encodeTermInline"),
            Phantoms.var("cx"),
            Phantoms.var("env"),
            Phantoms.false(),
            Phantoms.var("t"),
        ),
    )
    strip_type_apps = Phantoms.lam(
        "t",
        Phantoms.cases(
            Name("hydra.core.Term"),
            Phantoms.var("t"),
            Just(Phantoms.var("t")),
            [
                Phantoms.field(
                    Name("annotated"),
                    Phantoms.lam(
                        "ann",
                        _ap(
                            Phantoms.var("stripTypeApps"),
                            Core.annotated_term_body(Phantoms.var("ann")),
                        ),
                    ),
                ),
                Phantoms.field(
                    Name("typeApplication"),
                    Phantoms.lam(
                        "ta",
                        _ap(
                            Phantoms.var("stripTypeApps"),
                            Core.type_application_term_body(Phantoms.var("ta")),
                        ),
                    ),
                ),
            ],
        ),
    )
    with_cast = Phantoms.lam(
        "pyexp",
        Logic.if_else(
            Logic.or_(
                Phantoms.var("noCast"),
                _env("skipCasts", "env"),
            ),
            Phantoms.right(Phantoms.var("pyexp")),
            _let_chain(
                [
                    ("tc", _env("graph", "env")),
                    (
                        "mtyp",
                        Eithers.map_(
                            Phantoms.lam("_r", Pairs.first(Phantoms.var("_r"))),
                            _ap(
                                Phantoms.var("hydra.checking.typeOf"),
                                Phantoms.var("cx"),
                                Phantoms.var("tc"),
                                Phantoms.list_([]),
                                Phantoms.var("term"),
                            ),
                        ),
                    ),
                ],
                Eithers.either(
                    Phantoms.constant(Phantoms.right(Phantoms.var("pyexp"))),
                    Phantoms.lam(
                        "typ",
                        Eithers.either(
                            Phantoms.constant(Phantoms.right(Phantoms.var("pyexp"))),
                            Phantoms.lam(
                                "pytyp",
                                Phantoms.right(
                                    _ap(
                                        Phantoms.var("hydra.python.utils.castTo"),
                                        Phantoms.var("pytyp"),
                                        Phantoms.var("pyexp"),
                                    )
                                ),
                            ),
                            _ap(
                                _local("encodeType"),
                                Phantoms.var("env"),
                                Phantoms.var("typ"),
                            ),
                        ),
                    ),
                    Phantoms.var("mtyp"),
                ),
            ),
        ),
    )

    # 16 case dispatch fields
    application_branch = Phantoms.lam(
        "app",
        _ap(
            _local("encodeApplication"),
            Phantoms.var("cx"),
            Phantoms.var("env"),
            Phantoms.var("app"),
        ),
    )
    either_branch = Phantoms.lam(
        "et",
        Eithers.either(
            Phantoms.lam(
                "t1",
                Eithers.bind(
                    _ap(Phantoms.var("encode"), Phantoms.var("t1")),
                    Phantoms.lam(
                        "pyexp",
                        _ap(
                            Phantoms.var("withCast"),
                            _ap(
                                _kref.utils_function_call,
                                _ap(
                                    _kref.utils_py_name_to_py_primary,
                                    _py_name("Left"),
                                ),
                                Phantoms.list_([Phantoms.var("pyexp")]),
                            ),
                        ),
                    ),
                ),
            ),
            Phantoms.lam(
                "t1",
                Eithers.bind(
                    _ap(Phantoms.var("encode"), Phantoms.var("t1")),
                    Phantoms.lam(
                        "pyexp",
                        _ap(
                            Phantoms.var("withCast"),
                            _ap(
                                _kref.utils_function_call,
                                _ap(
                                    _kref.utils_py_name_to_py_primary,
                                    _py_name("Right"),
                                ),
                                Phantoms.list_([Phantoms.var("pyexp")]),
                            ),
                        ),
                    ),
                ),
            ),
            Phantoms.var("et"),
        ),
    )
    lambda_branch = Phantoms.lam(
        "lam",
        Eithers.bind(
            _ap(
                _local("analyzePythonFunction"),
                Phantoms.var("cx"),
                Phantoms.var("env"),
                Core.term_lambda(Phantoms.var("lam")),
            ),
            Phantoms.lam(
                "fs",
                _let_chain(
                    [
                        ("params", fs_proj("params")),
                        ("bindings", fs_proj("bindings")),
                        ("innerBody", fs_proj("body")),
                        ("innerEnv0", fs_proj("environment")),
                        (
                            "bindingNames",
                            Lists.map(
                                Phantoms.lam(
                                    "b", Core.binding_name(Phantoms.var("b"))
                                ),
                                Phantoms.var("bindings"),
                            ),
                        ),
                        (
                            "innerEnv",
                            Phantoms.record(
                                Name("hydra.python.environment.PythonEnvironment"),
                                [
                                    Phantoms.field(
                                        Name("namespaces"),
                                        _env("namespaces", "innerEnv0"),
                                    ),
                                    Phantoms.field(
                                        Name("boundTypeVariables"),
                                        _env("boundTypeVariables", "innerEnv0"),
                                    ),
                                    Phantoms.field(
                                        Name("graph"), _env("graph", "innerEnv0")
                                    ),
                                    Phantoms.field(
                                        Name("nullaryBindings"),
                                        _env("nullaryBindings", "innerEnv0"),
                                    ),
                                    Phantoms.field(
                                        Name("version"),
                                        _env("version", "innerEnv0"),
                                    ),
                                    Phantoms.field(
                                        Name("skipCasts"),
                                        _env("skipCasts", "innerEnv0"),
                                    ),
                                    Phantoms.field(
                                        Name("inlineVariables"),
                                        Sets.union(
                                            Sets.from_list(
                                                Phantoms.var("bindingNames")
                                            ),
                                            _env("inlineVariables", "innerEnv0"),
                                        ),
                                    ),
                                ],
                            ),
                        ),
                    ],
                    Eithers.bind(
                        _ap(
                            _local("encodeTermInline"),
                            Phantoms.var("cx"),
                            Phantoms.var("innerEnv"),
                            Phantoms.false(),
                            Phantoms.var("innerBody"),
                        ),
                        Phantoms.lam(
                            "pbody",
                            _let_chain(
                                [
                                    (
                                        "pparams",
                                        Lists.map(
                                            _ap(
                                                _kref.names_encode_name,
                                                Phantoms.false(),
                                                _kref.util_case_convention_lower_snake,
                                                Phantoms.var("innerEnv"),
                                            ),
                                            Phantoms.var("params"),
                                        ),
                                    ),
                                ],
                                Logic.if_else(
                                    Lists.null(Phantoms.var("bindings")),
                                    Phantoms.right(
                                        _ap(
                                            _local("makeUncurriedLambda"),
                                            Phantoms.var("pparams"),
                                            Phantoms.var("pbody"),
                                        )
                                    ),
                                    Eithers.bind(
                                        Eithers.map_list(
                                            _ap(
                                                _local("encodeBindingAsAssignment"),
                                                Phantoms.var("cx"),
                                                Phantoms.false(),
                                                Phantoms.var("innerEnv"),
                                            ),
                                            Phantoms.var("bindings"),
                                        ),
                                        Phantoms.lam(
                                            "pbindingExprs",
                                            _let_chain(
                                                [
                                                    (
                                                        "pbindingStarExprs",
                                                        Lists.map(
                                                            Phantoms.lam(
                                                                "ne",
                                                                PySyn.star_named_expression_simple(
                                                                    Phantoms.var("ne")
                                                                ),
                                                            ),
                                                            Phantoms.var("pbindingExprs"),
                                                        ),
                                                    ),
                                                    (
                                                        "pbodyStarExpr",
                                                        _ap(
                                                            Phantoms.var(
                                                                "hydra.python.utils.pyExpressionToPyStarNamedExpression"
                                                            ),
                                                            Phantoms.var("pbody"),
                                                        ),
                                                    ),
                                                    (
                                                        "tupleElements",
                                                        Lists.concat2(
                                                            Phantoms.var("pbindingStarExprs"),
                                                            Phantoms.list_(
                                                                [
                                                                    Phantoms.var("pbodyStarExpr")
                                                                ]
                                                            ),
                                                        ),
                                                    ),
                                                    (
                                                        "tupleExpr",
                                                        _ap(
                                                            _kref.utils_py_atom_to_py_expression,
                                                            PySyn.atom_tuple(
                                                                getattr(PySyn, "tuple")(
                                                                    Phantoms.var("tupleElements")
                                                                )
                                                            ),
                                                        ),
                                                    ),
                                                    (
                                                        "indexValue",
                                                        _ap(
                                                            _kref.utils_py_atom_to_py_expression,
                                                            PySyn.atom_number(
                                                                PySyn.number_integer(
                                                                    Literals.int32_to_bigint(
                                                                        Lists.length(
                                                                            Phantoms.var("bindings")
                                                                        )
                                                                    )
                                                                )
                                                            ),
                                                        ),
                                                    ),
                                                    (
                                                        "indexedExpr",
                                                        _ap(
                                                            _kref.utils_primary_with_expression_slices,
                                                            _ap(
                                                                _kref.utils_py_expression_to_py_primary,
                                                                Phantoms.var("tupleExpr"),
                                                            ),
                                                            Phantoms.list_(
                                                                [Phantoms.var("indexValue")]
                                                            ),
                                                        ),
                                                    ),
                                                ],
                                                Phantoms.right(
                                                    _ap(
                                                        _local("makeUncurriedLambda"),
                                                        Phantoms.var("pparams"),
                                                        _ap(
                                                            _kref.utils_py_primary_to_py_expression,
                                                            Phantoms.var("indexedExpr"),
                                                        ),
                                                    )
                                                ),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    project_branch = Phantoms.lam(
        "proj",
        _let_chain(
            [
                ("fname", _proj("hydra.core.Projection", "field", "proj")),
            ],
            Phantoms.right(
                _ap(
                    _local("makeCurriedLambda"),
                    Phantoms.list_([_py_name("v1")]),
                    _ap(
                        _kref.utils_project_from_expression,
                        PyDsl.py_name_to_py_expression(_py_name("v1")),
                        _ap(
                            _kref.names_encode_field_name,
                            Phantoms.var("env"),
                            Phantoms.var("fname"),
                        ),
                    ),
                )
            ),
        ),
    )
    unwrap_branch = Phantoms.constant(
        Phantoms.right(
            _ap(
                _local("makeCurriedLambda"),
                Phantoms.list_([_py_name("v1")]),
                _ap(
                    _kref.utils_project_from_expression,
                    PyDsl.py_name_to_py_expression(_py_name("v1")),
                    _py_name("value"),
                ),
            )
        )
    )
    cases_branch_unsupported = Phantoms.constant(
        Phantoms.right(
            _ap(
                _local("unsupportedExpression"),
                Phantoms.string(
                    "case expressions as values are not yet supported"
                ),
            )
        )
    )
    let_branch = Phantoms.lam(
        "lt",
        _let_chain(
            [
                ("bindings", Core.let_bindings(Phantoms.var("lt"))),
                ("body", Core.let_body(Phantoms.var("lt"))),
            ],
            Logic.if_else(
                Lists.null(Phantoms.var("bindings")),
                _ap(
                    _local("encodeTermInline"),
                    Phantoms.var("cx"),
                    Phantoms.var("env"),
                    Phantoms.false(),
                    Phantoms.var("body"),
                ),
                _ap(
                    _local("withLetInline"),
                    Phantoms.var("env"),
                    Phantoms.var("lt"),
                    Phantoms.lam(
                        "innerEnv",
                        Eithers.bind(
                            Eithers.map_list(
                                _ap(
                                    _local("encodeBindingAsAssignment"),
                                    Phantoms.var("cx"),
                                    Phantoms.false(),
                                    Phantoms.var("innerEnv"),
                                ),
                                Phantoms.var("bindings"),
                            ),
                            Phantoms.lam(
                                "pbindingExprs",
                                Eithers.bind(
                                    _ap(
                                        _local("encodeTermInline"),
                                        Phantoms.var("cx"),
                                        Phantoms.var("innerEnv"),
                                        Phantoms.false(),
                                        Phantoms.var("body"),
                                    ),
                                    Phantoms.lam(
                                        "pbody",
                                        _let_chain(
                                            [
                                                (
                                                    "pbindingStarExprs",
                                                    Lists.map(
                                                        Phantoms.lam(
                                                            "ne",
                                                            PySyn.star_named_expression_simple(
                                                                Phantoms.var("ne")
                                                            ),
                                                        ),
                                                        Phantoms.var("pbindingExprs"),
                                                    ),
                                                ),
                                                (
                                                    "pbodyStarExpr",
                                                    _ap(
                                                        Phantoms.var(
                                                            "hydra.python.utils.pyExpressionToPyStarNamedExpression"
                                                        ),
                                                        Phantoms.var("pbody"),
                                                    ),
                                                ),
                                                (
                                                    "tupleElements",
                                                    Lists.concat2(
                                                        Phantoms.var("pbindingStarExprs"),
                                                        Phantoms.list_(
                                                            [Phantoms.var("pbodyStarExpr")]
                                                        ),
                                                    ),
                                                ),
                                                (
                                                    "tupleExpr",
                                                    _ap(
                                                        _kref.utils_py_atom_to_py_expression,
                                                        PySyn.atom_tuple(
                                                            getattr(PySyn, "tuple")(
                                                                Phantoms.var("tupleElements")
                                                            )
                                                        ),
                                                    ),
                                                ),
                                                (
                                                    "indexValue",
                                                    _ap(
                                                        _kref.utils_py_atom_to_py_expression,
                                                        PySyn.atom_number(
                                                            PySyn.number_integer(
                                                                Literals.int32_to_bigint(
                                                                    Lists.length(
                                                                        Phantoms.var("bindings")
                                                                    )
                                                                )
                                                            )
                                                        ),
                                                    ),
                                                ),
                                                (
                                                    "indexedExpr",
                                                    _ap(
                                                        _kref.utils_primary_with_expression_slices,
                                                        _ap(
                                                            _kref.utils_py_expression_to_py_primary,
                                                            Phantoms.var("tupleExpr"),
                                                        ),
                                                        Phantoms.list_(
                                                            [Phantoms.var("indexValue")]
                                                        ),
                                                    ),
                                                ),
                                            ],
                                            Phantoms.right(
                                                _ap(
                                                    _kref.utils_py_primary_to_py_expression,
                                                    Phantoms.var("indexedExpr"),
                                                )
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    list_branch = Phantoms.lam(
        "terms",
        Eithers.bind(
            Eithers.map_list(Phantoms.var("encode"), Phantoms.var("terms")),
            Phantoms.lam(
                "pyExprs",
                Phantoms.right(
                    _ap(
                        _kref.utils_py_atom_to_py_expression,
                        PySyn.atom_tuple(
                            getattr(PySyn, "tuple")(
                                Lists.map(
                                    Phantoms.var(
                                        "hydra.python.utils.pyExpressionToPyStarNamedExpression"
                                    ),
                                    Phantoms.var("pyExprs"),
                                )
                            )
                        ),
                    )
                ),
            ),
        ),
    )
    literal_branch = Phantoms.lam(
        "lit", _ap(_local("encodeLiteral"), Phantoms.var("lit"))
    )
    map_branch = Phantoms.lam(
        "m",
        Eithers.bind(
            Eithers.map_list(
                Phantoms.lam(
                    "kv",
                    _let_chain(
                        [
                            ("k", Pairs.first(Phantoms.var("kv"))),
                            ("v", Pairs.second(Phantoms.var("kv"))),
                        ],
                        Eithers.bind(
                            _ap(Phantoms.var("encode"), Phantoms.var("k")),
                            Phantoms.lam(
                                "pyK",
                                Eithers.bind(
                                    _ap(Phantoms.var("encode"), Phantoms.var("v")),
                                    Phantoms.lam(
                                        "pyV",
                                        Phantoms.right(
                                            PySyn.double_starred_kvpair_pair(
                                                PySyn.kvpair(
                                                    Phantoms.var("pyK"),
                                                    Phantoms.var("pyV"),
                                                )
                                            )
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
                Maps.to_list(Phantoms.var("m")),
            ),
            Phantoms.lam(
                "pairs",
                Phantoms.right(
                    _ap(
                        _kref.utils_function_call,
                        _ap(
                            _kref.utils_py_name_to_py_primary,
                            _py_name("FrozenDict"),
                        ),
                        Phantoms.list_(
                            [
                                _ap(
                                    _kref.utils_py_atom_to_py_expression,
                                    PySyn.atom_dict(
                                        getattr(PySyn, "dict")(Phantoms.var("pairs"))
                                    ),
                                )
                            ]
                        ),
                    )
                ),
            ),
        ),
    )
    maybe_branch = Phantoms.lam(
        "mt",
        Maybes.maybe(
            Phantoms.right(
                _ap(
                    _kref.utils_function_call,
                    _ap(_kref.utils_py_name_to_py_primary, _py_name("Nothing")),
                    Phantoms.list_([]),
                )
            ),
            Phantoms.lam(
                "t1",
                Eithers.bind(
                    _ap(Phantoms.var("encode"), Phantoms.var("t1")),
                    Phantoms.lam(
                        "pyexp",
                        _ap(
                            Phantoms.var("withCast"),
                            _ap(
                                _kref.utils_function_call,
                                _ap(
                                    _kref.utils_py_name_to_py_primary,
                                    _py_name("Just"),
                                ),
                                Phantoms.list_([Phantoms.var("pyexp")]),
                            ),
                        ),
                    ),
                ),
            ),
            Phantoms.var("mt"),
        ),
    )
    pair_branch = Phantoms.lam(
        "p",
        _let_chain(
            [
                ("t1", Pairs.first(Phantoms.var("p"))),
                ("t2", Pairs.second(Phantoms.var("p"))),
            ],
            Eithers.bind(
                _ap(Phantoms.var("encode"), Phantoms.var("t1")),
                Phantoms.lam(
                    "pyExpr1",
                    Eithers.bind(
                        _ap(Phantoms.var("encode"), Phantoms.var("t2")),
                        Phantoms.lam(
                            "pyExpr2",
                            Phantoms.right(
                                _ap(
                                    _kref.utils_py_atom_to_py_expression,
                                    PySyn.atom_tuple(
                                        getattr(PySyn, "tuple")(
                                            Phantoms.list_(
                                                [
                                                    _ap(
                                                        Phantoms.var(
                                                            "hydra.python.utils.pyExpressionToPyStarNamedExpression"
                                                        ),
                                                        Phantoms.var("pyExpr1"),
                                                    ),
                                                    _ap(
                                                        Phantoms.var(
                                                            "hydra.python.utils.pyExpressionToPyStarNamedExpression"
                                                        ),
                                                        Phantoms.var("pyExpr2"),
                                                    ),
                                                ]
                                            )
                                        )
                                    ),
                                )
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    record_branch = Phantoms.lam(
        "r",
        _let_chain(
            [
                ("tname", Core.record_type_name(Phantoms.var("r"))),
                ("fields", Core.record_fields(Phantoms.var("r"))),
            ],
            Eithers.bind(
                Eithers.map_list(
                    Phantoms.lam(
                        "fld",
                        _ap(
                            Phantoms.var("encode"),
                            Core.field_term(Phantoms.var("fld")),
                        ),
                    ),
                    Phantoms.var("fields"),
                ),
                Phantoms.lam(
                    "pargs",
                    Phantoms.right(
                        _ap(
                            _kref.utils_function_call,
                            _ap(
                                _kref.utils_py_name_to_py_primary,
                                _ap(
                                    _kref.names_encode_name_qualified,
                                    Phantoms.var("env"),
                                    Phantoms.var("tname"),
                                ),
                            ),
                            Phantoms.var("pargs"),
                        )
                    ),
                ),
            ),
        ),
    )
    set_branch = Phantoms.lam(
        "s",
        Eithers.bind(
            Eithers.map_list(
                Phantoms.var("encode"), Sets.to_list(Phantoms.var("s"))
            ),
            Phantoms.lam(
                "pyEls",
                Phantoms.right(
                    _ap(
                        _kref.utils_function_call,
                        _ap(_kref.utils_py_name_to_py_primary, _py_name("frozenset")),
                        Phantoms.list_(
                            [
                                _ap(
                                    _kref.utils_py_atom_to_py_expression,
                                    PySyn.atom_set(
                                        getattr(PySyn, "set")(
                                            Lists.map(
                                                Phantoms.var(
                                                    "hydra.python.utils.pyExpressionToPyStarNamedExpression"
                                                ),
                                                Phantoms.var("pyEls"),
                                            )
                                        )
                                    ),
                                )
                            ]
                        ),
                    )
                ),
            ),
        ),
    )
    type_application_branch = Phantoms.lam(
        "ta",
        _let_chain(
            [
                ("body", Core.type_application_term_body(Phantoms.var("ta"))),
            ],
            Eithers.bind(
                _ap(
                    _local("encodeTermInline"),
                    Phantoms.var("cx"),
                    Phantoms.var("env"),
                    Phantoms.true(),
                    _ap(Phantoms.var("stripTypeApps"), Phantoms.var("body")),
                ),
                Phantoms.lam(
                    "pybase", _ap(Phantoms.var("withCast"), Phantoms.var("pybase"))
                ),
            ),
        ),
    )
    type_lambda_branch = Phantoms.lam(
        "tl",
        _let_chain(
            [
                ("body", Core.type_lambda_body(Phantoms.var("tl"))),
            ],
            _ap(
                _local("withTypeLambda"),
                Phantoms.var("env"),
                Phantoms.var("tl"),
                Phantoms.lam(
                    "env2",
                    _ap(
                        _local("encodeTermInline"),
                        Phantoms.var("cx"),
                        Phantoms.var("env2"),
                        Phantoms.var("noCast"),
                        Phantoms.var("body"),
                    ),
                ),
            ),
        ),
    )
    inject_branch = Phantoms.lam(
        "inj",
        _let_chain(
            [
                ("tname", Core.injection_type_name(Phantoms.var("inj"))),
                ("field", Core.injection_field(Phantoms.var("inj"))),
            ],
            Eithers.bind(
                _ap(
                    _kref.resolution_require_union_type,
                    Phantoms.var("cx"),
                    _ap(_local("pythonEnvironmentGetGraph"), Phantoms.var("env")),
                    Phantoms.var("tname"),
                ),
                Phantoms.lam(
                    "rt",
                    Logic.if_else(
                        _ap(_kref.predicates_is_enum_row_type, Phantoms.var("rt")),
                        Phantoms.right(
                            _ap(
                                _kref.utils_project_from_expression,
                                _ap(
                                    _kref.utils_py_name_to_py_expression,
                                    _ap(
                                        _kref.names_encode_name_qualified,
                                        Phantoms.var("env"),
                                        Phantoms.var("tname"),
                                    ),
                                ),
                                _ap(
                                    _kref.names_encode_enum_value,
                                    Phantoms.var("env"),
                                    Core.field_name(Phantoms.var("field")),
                                ),
                            )
                        ),
                        _let_chain(
                            [
                                ("fname", Core.field_name(Phantoms.var("field"))),
                                (
                                    "isUnitVariant",
                                    Maybes.maybe(
                                        Phantoms.false(),
                                        Phantoms.lam(
                                            "ft",
                                            _ap(
                                                _kref.predicates_is_unit_type,
                                                _ap(
                                                    _kref.strip_deannotate_type,
                                                    Core.field_type_type(
                                                        Phantoms.var("ft")
                                                    ),
                                                ),
                                            ),
                                        ),
                                        Lists.find(
                                            Phantoms.lam(
                                                "ft",
                                                MetaCore.equal_name(
                                                    Core.field_type_name(
                                                        Phantoms.var("ft")
                                                    ),
                                                    Phantoms.var("fname"),
                                                ),
                                            ),
                                            Phantoms.var("rt"),
                                        ),
                                    ),
                                ),
                            ],
                            Eithers.bind(
                                Logic.if_else(
                                    Logic.or_(
                                        _ap(
                                            _kref.predicates_is_unit_term,
                                            Core.field_term(Phantoms.var("field")),
                                        ),
                                        Phantoms.var("isUnitVariant"),
                                    ),
                                    Phantoms.right(Phantoms.list_([])),
                                    Eithers.bind(
                                        _ap(
                                            Phantoms.var("encode"),
                                            Core.field_term(Phantoms.var("field")),
                                        ),
                                        Phantoms.lam(
                                            "parg",
                                            Phantoms.right(
                                                Phantoms.list_(
                                                    [Phantoms.var("parg")]
                                                )
                                            ),
                                        ),
                                    ),
                                ),
                                Phantoms.lam(
                                    "args",
                                    _let_chain(
                                        [
                                            (
                                                "deconflictedName",
                                                _ap(
                                                    _local("deconflictVariantName"),
                                                    Phantoms.true(),
                                                    Phantoms.var("env"),
                                                    Phantoms.var("tname"),
                                                    Phantoms.var("fname"),
                                                    _env("graph", "env"),
                                                ),
                                            ),
                                        ],
                                        Phantoms.right(
                                            _ap(
                                                Phantoms.var(
                                                    "hydra.python.utils.castTo"
                                                ),
                                                _ap(
                                                    _kref.names_type_variable_reference,
                                                    Phantoms.var("env"),
                                                    Phantoms.var("tname"),
                                                ),
                                                _ap(
                                                    _kref.utils_function_call,
                                                    _ap(
                                                        _kref.utils_py_name_to_py_primary,
                                                        Phantoms.var("deconflictedName"),
                                                    ),
                                                    Phantoms.var("args"),
                                                ),
                                            )
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    unit_branch = Phantoms.constant(
        Phantoms.right(
            _ap(_kref.utils_py_name_to_py_expression, _kref.utils_py_none)
        )
    )
    variable_branch = Phantoms.lam(
        "name",
        _ap(
            _local("encodeVariable"),
            Phantoms.var("cx"),
            Phantoms.var("env"),
            Phantoms.var("name"),
            Phantoms.list_([]),
        ),
    )
    wrap_branch = Phantoms.lam(
        "wrapped",
        _let_chain(
            [
                ("tname", Core.wrapped_term_type_name(Phantoms.var("wrapped"))),
                ("inner", Core.wrapped_term_body(Phantoms.var("wrapped"))),
            ],
            Eithers.bind(
                _ap(Phantoms.var("encode"), Phantoms.var("inner")),
                Phantoms.lam(
                    "parg",
                    Phantoms.right(
                        _ap(
                            _kref.utils_function_call,
                            _ap(
                                _kref.utils_py_name_to_py_primary,
                                _ap(
                                    _kref.names_encode_name_qualified,
                                    Phantoms.var("env"),
                                    Phantoms.var("tname"),
                                ),
                            ),
                            Phantoms.list_([Phantoms.var("parg")]),
                        )
                    ),
                ),
            ),
        ),
    )

    body = Phantoms.lambdas(
        ["cx", "env", "noCast", "term"],
        _let_chain(
            [
                ("encode", encode_helper),
                ("stripTypeApps", strip_type_apps),
                ("withCast", with_cast),
            ],
            Phantoms.cases(
                Name("hydra.core.Term"),
                _ap(_kref.strip_deannotate_and_detype_term, Phantoms.var("term")),
                Nothing(),
                [
                    Phantoms.field(Name("application"), application_branch),
                    Phantoms.field(Name("either"), either_branch),
                    Phantoms.field(Name("lambda"), lambda_branch),
                    Phantoms.field(Name("project"), project_branch),
                    Phantoms.field(Name("unwrap"), unwrap_branch),
                    Phantoms.field(Name("cases"), cases_branch_unsupported),
                    Phantoms.field(Name("let"), let_branch),
                    Phantoms.field(Name("list"), list_branch),
                    Phantoms.field(Name("literal"), literal_branch),
                    Phantoms.field(Name("map"), map_branch),
                    Phantoms.field(Name("maybe"), maybe_branch),
                    Phantoms.field(Name("pair"), pair_branch),
                    Phantoms.field(Name("record"), record_branch),
                    Phantoms.field(Name("set"), set_branch),
                    Phantoms.field(Name("typeApplication"), type_application_branch),
                    Phantoms.field(Name("typeLambda"), type_lambda_branch),
                    Phantoms.field(Name("inject"), inject_branch),
                    Phantoms.field(Name("unit"), unit_branch),
                    Phantoms.field(Name("variable"), variable_branch),
                    Phantoms.field(Name("wrap"), wrap_branch),
                ],
            ),
        ),
    )
    return _def(
        "encodeTermInline",
        Phantoms.doc("Encode a term to a Python expression (inline form)", body),
    )


def _encode_term_multiline():
    def fs_proj(field):
        return _proj("hydra.typing.FunctionStructure", field, "fs")

    dflt_logic = Eithers.bind(
        _ap(
            _local("analyzePythonFunction"),
            Phantoms.var("cx"),
            Phantoms.var("env"),
            Phantoms.var("term"),
        ),
        Phantoms.lam(
            "fs",
            _let_chain(
                [
                    ("params", fs_proj("params")),
                    ("bindings", fs_proj("bindings")),
                    ("innerBody", fs_proj("body")),
                    ("env2", fs_proj("environment")),
                ],
                Logic.if_else(
                    Lists.null(Phantoms.var("bindings")),
                    Eithers.bind(
                        _ap(
                            _local("encodeTermInline"),
                            Phantoms.var("cx"),
                            Phantoms.var("env"),
                            Phantoms.false(),
                            Phantoms.var("term"),
                        ),
                        Phantoms.lam(
                            "expr",
                            Phantoms.right(
                                Phantoms.list_(
                                    [
                                        _ap(
                                            _kref.utils_return_single,
                                            Phantoms.var("expr"),
                                        )
                                    ]
                                )
                            ),
                        ),
                    ),
                    Eithers.bind(
                        Eithers.map_list(
                            _ap(
                                _local("encodeBindingAs"),
                                Phantoms.var("cx"),
                                Phantoms.var("env2"),
                            ),
                            Phantoms.var("bindings"),
                        ),
                        Phantoms.lam(
                            "bindingStmts",
                            Eithers.bind(
                                _ap(
                                    _local("encodeTermMultiline"),
                                    Phantoms.var("cx"),
                                    Phantoms.var("env2"),
                                    Phantoms.var("innerBody"),
                                ),
                                Phantoms.lam(
                                    "bodyStmts",
                                    Phantoms.right(
                                        Lists.concat2(
                                            Phantoms.var("bindingStmts"),
                                            Phantoms.var("bodyStmts"),
                                        )
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    cases_branch = Phantoms.lam(
        "cs",
        _let_chain(
            [
                ("tname", Core.case_statement_type_name(Phantoms.var("cs"))),
                ("dflt", Core.case_statement_default(Phantoms.var("cs"))),
                ("cases_", Core.case_statement_cases(Phantoms.var("cs"))),
            ],
            Eithers.bind(
                _ap(
                    _kref.resolution_require_union_type,
                    Phantoms.var("cx"),
                    _ap(_local("pythonEnvironmentGetGraph"), Phantoms.var("env")),
                    Phantoms.var("tname"),
                ),
                Phantoms.lam(
                    "rt",
                    _let_chain(
                        [
                            (
                                "isEnum",
                                _ap(
                                    _kref.predicates_is_enum_row_type,
                                    Phantoms.var("rt"),
                                ),
                            ),
                            (
                                "isFull",
                                _ap(
                                    _local("isCasesFull"),
                                    Phantoms.var("rt"),
                                    Phantoms.var("cases_"),
                                ),
                            ),
                        ],
                        Eithers.bind(
                            _ap(
                                _local("encodeTermInline"),
                                Phantoms.var("cx"),
                                Phantoms.var("env"),
                                Phantoms.false(),
                                Phantoms.var("arg"),
                            ),
                            Phantoms.lam(
                                "pyArg",
                                Eithers.bind(
                                    Eithers.map_list(
                                        _ap(
                                            _local("caseBlockToExpr"),
                                            Phantoms.var("cx"),
                                            Phantoms.var("env"),
                                            Phantoms.var("tname"),
                                            Phantoms.var("rt"),
                                            Phantoms.var("isEnum"),
                                            Phantoms.lambdas(
                                                ["e", "t"],
                                                _ap(
                                                    _local("encodeTermMultiline"),
                                                    Phantoms.var("cx"),
                                                    Phantoms.var("e"),
                                                    Phantoms.var("t"),
                                                ),
                                            ),
                                        ),
                                        _ap(
                                            _local("deduplicateCaseVariables"),
                                            Phantoms.var("cases_"),
                                        ),
                                    ),
                                    Phantoms.lam(
                                        "pyCases",
                                        Eithers.bind(
                                            _ap(
                                                _local("encodeDefaultCaseBlock"),
                                                Phantoms.lam(
                                                    "t",
                                                    _ap(
                                                        _local("encodeTermInline"),
                                                        Phantoms.var("cx"),
                                                        Phantoms.var("env"),
                                                        Phantoms.false(),
                                                        Phantoms.var("t"),
                                                    ),
                                                ),
                                                Phantoms.var("isFull"),
                                                Phantoms.var("dflt"),
                                                Phantoms.var("tname"),
                                            ),
                                            Phantoms.lam(
                                                "pyDflt",
                                                _let_chain(
                                                    [
                                                        (
                                                            "subj",
                                                            PySyn.subject_expression_simple(
                                                                PySyn.named_expression_simple(
                                                                    Phantoms.var("pyArg")
                                                                )
                                                            ),
                                                        ),
                                                        (
                                                            "matchStmt",
                                                            PySyn.statement_compound(
                                                                PySyn.compound_statement_match(
                                                                    Phantoms.record(
                                                                        Name(
                                                                            "hydra.python.syntax.MatchStatement"
                                                                        ),
                                                                        [
                                                                            Phantoms.field(
                                                                                Name("subject"),
                                                                                Phantoms.var(
                                                                                    "subj"
                                                                                ),
                                                                            ),
                                                                            Phantoms.field(
                                                                                Name("cases"),
                                                                                Lists.concat2(
                                                                                    Phantoms.var(
                                                                                        "pyCases"
                                                                                    ),
                                                                                    Phantoms.var(
                                                                                        "pyDflt"
                                                                                    ),
                                                                                ),
                                                                            ),
                                                                        ],
                                                                    )
                                                                )
                                                            ),
                                                        ),
                                                    ],
                                                    Phantoms.right(
                                                        Phantoms.list_(
                                                            [Phantoms.var("matchStmt")]
                                                        )
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    body = Phantoms.lambdas(
        ["cx", "env", "term"],
        _let_chain(
            [
                ("dfltLogic", dflt_logic),
                (
                    "gathered",
                    _ap(_kref.analysis_gather_applications, Phantoms.var("term")),
                ),
                ("args", Pairs.first(Phantoms.var("gathered"))),
                ("body", Pairs.second(Phantoms.var("gathered"))),
            ],
            Logic.if_else(
                Equality.equal(
                    Lists.length(Phantoms.var("args")), Phantoms.int32(1)
                ),
                _let_chain(
                    [
                        (
                            "arg",
                            Maybes.from_maybe(
                                Core.term_unit,
                                Lists.maybe_head(Phantoms.var("args")),
                            ),
                        ),
                    ],
                    Phantoms.cases(
                        Name("hydra.core.Term"),
                        _ap(
                            _kref.strip_deannotate_and_detype_term,
                            Phantoms.var("body"),
                        ),
                        Just(Phantoms.var("dfltLogic")),
                        [
                            Phantoms.field(Name("cases"), cases_branch),
                        ],
                    ),
                ),
                Phantoms.var("dfltLogic"),
            ),
        ),
    )
    return _def(
        "encodeTermMultiline",
        Phantoms.doc(
            "Encode a term to a list of statements with return as final statement",
            body,
        ),
    )


def _encode_term_multiline_tco():
    cases_branch = Phantoms.lam(
        "cs",
        _let_chain(
            [
                ("tname", Core.case_statement_type_name(Phantoms.var("cs"))),
                ("dflt", Core.case_statement_default(Phantoms.var("cs"))),
                ("cases_", Core.case_statement_cases(Phantoms.var("cs"))),
            ],
            Eithers.bind(
                _ap(
                    _kref.resolution_require_union_type,
                    Phantoms.var("cx"),
                    _ap(_local("pythonEnvironmentGetGraph"), Phantoms.var("env")),
                    Phantoms.var("tname"),
                ),
                Phantoms.lam(
                    "rt",
                    _let_chain(
                        [
                            (
                                "isEnum",
                                _ap(
                                    _kref.predicates_is_enum_row_type,
                                    Phantoms.var("rt"),
                                ),
                            ),
                            (
                                "isFull",
                                _ap(
                                    _local("isCasesFull"),
                                    Phantoms.var("rt"),
                                    Phantoms.var("cases_"),
                                ),
                            ),
                        ],
                        Eithers.bind(
                            _ap(
                                _local("encodeTermInline"),
                                Phantoms.var("cx"),
                                Phantoms.var("env"),
                                Phantoms.false(),
                                Phantoms.var("arg"),
                            ),
                            Phantoms.lam(
                                "pyArg",
                                Eithers.bind(
                                    Eithers.map_list(
                                        _ap(
                                            _local("caseBlockToExpr"),
                                            Phantoms.var("cx"),
                                            Phantoms.var("env"),
                                            Phantoms.var("tname"),
                                            Phantoms.var("rt"),
                                            Phantoms.var("isEnum"),
                                            Phantoms.lambdas(
                                                ["e2", "t2"],
                                                _ap(
                                                    _local("encodeTermMultilineTCO"),
                                                    Phantoms.var("cx"),
                                                    Phantoms.var("e2"),
                                                    Phantoms.var("funcName"),
                                                    Phantoms.var("paramNames"),
                                                    Phantoms.var("t2"),
                                                ),
                                            ),
                                        ),
                                        _ap(
                                            _local("deduplicateCaseVariables"),
                                            Phantoms.var("cases_"),
                                        ),
                                    ),
                                    Phantoms.lam(
                                        "pyCases",
                                        Eithers.bind(
                                            _ap(
                                                _local("encodeDefaultCaseBlock"),
                                                Phantoms.lam(
                                                    "t2",
                                                    _ap(
                                                        _local("encodeTermInline"),
                                                        Phantoms.var("cx"),
                                                        Phantoms.var("env"),
                                                        Phantoms.false(),
                                                        Phantoms.var("t2"),
                                                    ),
                                                ),
                                                Phantoms.var("isFull"),
                                                Phantoms.var("dflt"),
                                                Phantoms.var("tname"),
                                            ),
                                            Phantoms.lam(
                                                "pyDflt",
                                                _let_chain(
                                                    [
                                                        (
                                                            "subj",
                                                            PySyn.subject_expression_simple(
                                                                PySyn.named_expression_simple(
                                                                    Phantoms.var("pyArg")
                                                                )
                                                            ),
                                                        ),
                                                        (
                                                            "matchStmt",
                                                            PySyn.statement_compound(
                                                                PySyn.compound_statement_match(
                                                                    Phantoms.record(
                                                                        Name(
                                                                            "hydra.python.syntax.MatchStatement"
                                                                        ),
                                                                        [
                                                                            Phantoms.field(
                                                                                Name("subject"),
                                                                                Phantoms.var(
                                                                                    "subj"
                                                                                ),
                                                                            ),
                                                                            Phantoms.field(
                                                                                Name("cases"),
                                                                                Lists.concat2(
                                                                                    Phantoms.var(
                                                                                        "pyCases"
                                                                                    ),
                                                                                    Phantoms.var(
                                                                                        "pyDflt"
                                                                                    ),
                                                                                ),
                                                                            ),
                                                                        ],
                                                                    )
                                                                )
                                                            ),
                                                        ),
                                                    ],
                                                    Phantoms.right(
                                                        Phantoms.list_(
                                                            [Phantoms.var("matchStmt")]
                                                        )
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    not_self_call_branch = _let_chain(
        [
            (
                "gathered2",
                _ap(_kref.analysis_gather_applications, Phantoms.var("term")),
            ),
            ("args2", Pairs.first(Phantoms.var("gathered2"))),
            ("body2", Pairs.second(Phantoms.var("gathered2"))),
        ],
        Logic.if_else(
            Equality.equal(
                Lists.length(Phantoms.var("args2")), Phantoms.int32(1)
            ),
            _let_chain(
                [
                    (
                        "arg",
                        Maybes.from_maybe(
                            Core.term_unit,
                            Lists.maybe_head(Phantoms.var("args2")),
                        ),
                    ),
                ],
                Phantoms.cases(
                    Name("hydra.core.Term"),
                    _ap(
                        _kref.strip_deannotate_and_detype_term,
                        Phantoms.var("body2"),
                    ),
                    Just(
                        Eithers.bind(
                            _ap(
                                _local("encodeTermInline"),
                                Phantoms.var("cx"),
                                Phantoms.var("env"),
                                Phantoms.false(),
                                Phantoms.var("term"),
                            ),
                            Phantoms.lam(
                                "expr",
                                Phantoms.right(
                                    Phantoms.list_(
                                        [
                                            _ap(
                                                _kref.utils_return_single,
                                                Phantoms.var("expr"),
                                            )
                                        ]
                                    )
                                ),
                            ),
                        )
                    ),
                    [
                        Phantoms.field(Name("cases"), cases_branch),
                    ],
                ),
            ),
            Eithers.bind(
                _ap(
                    _local("encodeTermInline"),
                    Phantoms.var("cx"),
                    Phantoms.var("env"),
                    Phantoms.false(),
                    Phantoms.var("term"),
                ),
                Phantoms.lam(
                    "expr",
                    Phantoms.right(
                        Phantoms.list_(
                            [
                                _ap(
                                    _kref.utils_return_single,
                                    Phantoms.var("expr"),
                                )
                            ]
                        )
                    ),
                ),
            ),
        ),
    )
    body = Phantoms.lambdas(
        ["cx", "env", "funcName", "paramNames", "term"],
        _let_chain(
            [
                (
                    "stripped",
                    _ap(_kref.strip_deannotate_and_detype_term, Phantoms.var("term")),
                ),
                (
                    "gathered",
                    _ap(
                        _kref.analysis_gather_applications, Phantoms.var("stripped")
                    ),
                ),
                ("gatherArgs", Pairs.first(Phantoms.var("gathered"))),
                ("gatherFun", Pairs.second(Phantoms.var("gathered"))),
                (
                    "strippedFun",
                    _ap(
                        _kref.strip_deannotate_and_detype_term,
                        Phantoms.var("gatherFun"),
                    ),
                ),
                (
                    "isSelfCall",
                    Phantoms.cases(
                        Name("hydra.core.Term"),
                        Phantoms.var("strippedFun"),
                        Just(Phantoms.false()),
                        [
                            Phantoms.field(
                                Name("variable"),
                                Phantoms.lam(
                                    "n",
                                    Equality.equal(
                                        Phantoms.var("n"), Phantoms.var("funcName")
                                    ),
                                ),
                            ),
                        ],
                    ),
                ),
            ],
            Logic.if_else(
                Logic.and_(
                    Phantoms.var("isSelfCall"),
                    Equality.equal(
                        Lists.length(Phantoms.var("gatherArgs")),
                        Lists.length(Phantoms.var("paramNames")),
                    ),
                ),
                Eithers.bind(
                    Eithers.map_list(
                        Phantoms.lam(
                            "a",
                            _ap(
                                _local("encodeTermInline"),
                                Phantoms.var("cx"),
                                Phantoms.var("env"),
                                Phantoms.false(),
                                Phantoms.var("a"),
                            ),
                        ),
                        Phantoms.var("gatherArgs"),
                    ),
                    Phantoms.lam(
                        "pyArgs",
                        _let_chain(
                            [
                                (
                                    "assignments",
                                    Lists.map(
                                        Phantoms.lam(
                                            "pair",
                                            _let_chain(
                                                [
                                                    (
                                                        "paramName",
                                                        Pairs.first(
                                                            Phantoms.var("pair")
                                                        ),
                                                    ),
                                                    (
                                                        "pyArg",
                                                        Pairs.second(
                                                            Phantoms.var("pair")
                                                        ),
                                                    ),
                                                ],
                                                _ap(
                                                    _kref.utils_assignment_statement,
                                                    _ap(
                                                        _kref.names_encode_name,
                                                        Phantoms.false(),
                                                        _kref.util_case_convention_lower_snake,
                                                        Phantoms.var("env"),
                                                        Phantoms.var("paramName"),
                                                    ),
                                                    Phantoms.var("pyArg"),
                                                ),
                                            ),
                                        ),
                                        Lists.zip(
                                            Phantoms.var("paramNames"),
                                            Phantoms.var("pyArgs"),
                                        ),
                                    ),
                                ),
                                (
                                    "continueStmt",
                                    PySyn.statement_simple(
                                        Phantoms.list_(
                                            [PySyn.simple_statement_continue]
                                        )
                                    ),
                                ),
                            ],
                            Phantoms.right(
                                Lists.concat2(
                                    Phantoms.var("assignments"),
                                    Phantoms.list_([Phantoms.var("continueStmt")]),
                                )
                            ),
                        ),
                    ),
                ),
                not_self_call_branch,
            ),
        ),
    )
    return _def(
        "encodeTermMultilineTCO",
        Phantoms.doc(
            "Encode a term body for TCO: tail self-calls become param reassignment + continue",
            body,
        ),
    )


def _encode_type():
    def call_self(arg_var):
        return _ap(
            _local("encodeType"), Phantoms.var("env"), Phantoms.var(arg_var)
        )

    def name_params(name, args_list):
        return _ap(
            _kref.utils_name_and_params,
            _py_name(name),
            args_list,
        )

    def primary_with_expr_slices_inline_named(name, args_list):
        return _ap(
            _kref.utils_py_primary_to_py_expression,
            _ap(
                _kref.utils_primary_with_expression_slices,
                PySyn.primary_simple(PySyn.atom_name(_py_name(name))),
                args_list,
            ),
        )

    fields = [
        Phantoms.field(
            Name("application"),
            Phantoms.lam(
                "at",
                _ap(
                    _local("encodeApplicationType"),
                    Phantoms.var("env"),
                    Phantoms.var("at"),
                ),
            ),
        ),
        Phantoms.field(
            Name("function"),
            Phantoms.lam(
                "ft",
                _ap(
                    _local("encodeFunctionType"),
                    Phantoms.var("env"),
                    Phantoms.var("ft"),
                ),
            ),
        ),
        Phantoms.field(
            Name("forall"),
            Phantoms.lam(
                "lt",
                _ap(
                    _local("encodeForallType"),
                    Phantoms.var("env"),
                    Phantoms.var("lt"),
                ),
            ),
        ),
        Phantoms.field(
            Name("list"),
            Phantoms.lam(
                "et",
                Eithers.bind(
                    call_self("et"),
                    Phantoms.lam(
                        "pyet",
                        Phantoms.right(
                            name_params(
                                "frozenlist",
                                Phantoms.list_([Phantoms.var("pyet")]),
                            )
                        ),
                    ),
                ),
            ),
        ),
        Phantoms.field(
            Name("map"),
            Phantoms.lam(
                "mt",
                Eithers.bind(
                    _ap(
                        _local("encodeType"),
                        Phantoms.var("env"),
                        _proj("hydra.core.MapType", "keys", "mt"),
                    ),
                    Phantoms.lam(
                        "pykt",
                        Eithers.bind(
                            _ap(
                                _local("encodeType"),
                                Phantoms.var("env"),
                                _proj("hydra.core.MapType", "values", "mt"),
                            ),
                            Phantoms.lam(
                                "pyvt",
                                Phantoms.right(
                                    name_params(
                                        "FrozenDict",
                                        Phantoms.list_(
                                            [
                                                Phantoms.var("pykt"),
                                                Phantoms.var("pyvt"),
                                            ]
                                        ),
                                    )
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
        Phantoms.field(
            Name("literal"),
            Phantoms.lam(
                "lt",
                _ap(_local("encodeLiteralType"), Phantoms.var("lt")),
            ),
        ),
        Phantoms.field(
            Name("maybe"),
            Phantoms.lam(
                "et",
                Eithers.bind(
                    call_self("et"),
                    Phantoms.lam(
                        "ptype",
                        Phantoms.right(
                            primary_with_expr_slices_inline_named(
                                "Maybe",
                                Phantoms.list_([Phantoms.var("ptype")]),
                            )
                        ),
                    ),
                ),
            ),
        ),
        Phantoms.field(
            Name("either"),
            Phantoms.lam(
                "eitherT",
                Eithers.bind(
                    _ap(
                        _local("encodeType"),
                        Phantoms.var("env"),
                        _proj("hydra.core.EitherType", "left", "eitherT"),
                    ),
                    Phantoms.lam(
                        "pyleft",
                        Eithers.bind(
                            _ap(
                                _local("encodeType"),
                                Phantoms.var("env"),
                                _proj("hydra.core.EitherType", "right", "eitherT"),
                            ),
                            Phantoms.lam(
                                "pyright",
                                Phantoms.right(
                                    primary_with_expr_slices_inline_named(
                                        "Either",
                                        Phantoms.list_(
                                            [
                                                Phantoms.var("pyleft"),
                                                Phantoms.var("pyright"),
                                            ]
                                        ),
                                    )
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
        Phantoms.field(
            Name("pair"),
            Phantoms.lam(
                "pairT",
                Eithers.bind(
                    _ap(
                        _local("encodeType"),
                        Phantoms.var("env"),
                        _proj("hydra.core.PairType", "first", "pairT"),
                    ),
                    Phantoms.lam(
                        "pyFirst",
                        Eithers.bind(
                            _ap(
                                _local("encodeType"),
                                Phantoms.var("env"),
                                _proj("hydra.core.PairType", "second", "pairT"),
                            ),
                            Phantoms.lam(
                                "pySecond",
                                Phantoms.right(
                                    name_params(
                                        "tuple",
                                        Phantoms.list_(
                                            [
                                                Phantoms.var("pyFirst"),
                                                Phantoms.var("pySecond"),
                                            ]
                                        ),
                                    )
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
        Phantoms.field(
            Name("record"), Phantoms.constant(Phantoms.var("dflt"))
        ),
        Phantoms.field(
            Name("set"),
            Phantoms.lam(
                "et",
                Eithers.bind(
                    call_self("et"),
                    Phantoms.lam(
                        "pyet",
                        Phantoms.right(
                            name_params(
                                "frozenset",
                                Phantoms.list_([Phantoms.var("pyet")]),
                            )
                        ),
                    ),
                ),
            ),
        ),
        Phantoms.field(
            Name("union"), Phantoms.constant(Phantoms.var("dflt"))
        ),
        Phantoms.field(
            Name("unit"),
            Phantoms.constant(
                Phantoms.right(
                    _ap(_kref.utils_py_name_to_py_expression, _kref.utils_py_none)
                )
            ),
        ),
        Phantoms.field(
            Name("void"),
            Phantoms.constant(
                Phantoms.right(
                    _ap(_kref.utils_py_name_to_py_expression, _kref.utils_py_none)
                )
            ),
        ),
        Phantoms.field(
            Name("variable"),
            Phantoms.lam(
                "name",
                Phantoms.right(
                    _ap(
                        _kref.names_type_variable_reference,
                        Phantoms.var("env"),
                        Phantoms.var("name"),
                    )
                ),
            ),
        ),
        Phantoms.field(
            Name("wrap"), Phantoms.constant(Phantoms.var("dflt"))
        ),
        Phantoms.field(
            Name("annotated"), Phantoms.constant(Phantoms.var("dflt"))
        ),
    ]
    body = Phantoms.lambdas(
        ["env", "typ"],
        _let_chain(
            [
                (
                    "dflt",
                    Phantoms.right(
                        _ap(
                            _kref.utils_double_quoted_string,
                            Strings.cat2(
                                Phantoms.string("type = "),
                                _ap(
                                    _kref.show_core_type,
                                    _ap(
                                        _kref.strip_deannotate_type,
                                        Phantoms.var("typ"),
                                    ),
                                ),
                            ),
                        )
                    ),
                ),
            ],
            Phantoms.cases(
                Name("hydra.core.Type"),
                _ap(_kref.strip_deannotate_type, Phantoms.var("typ")),
                Nothing(),
                fields,
            ),
        ),
    )
    return _def(
        "encodeType",
        Phantoms.doc("Encode a Hydra type to a Python type expression", body),
    )


def _encode_type_assignment():
    body = Phantoms.lambdas(
        ["cx", "env", "name", "typ", "comment"],
        Eithers.bind(
            _ap(
                _local("encodeTypeAssignmentInner"),
                Phantoms.var("cx"),
                Phantoms.var("env"),
                Phantoms.var("name"),
                Phantoms.var("typ"),
                Phantoms.var("comment"),
            ),
            Phantoms.lam(
                "defStmts",
                Phantoms.right(
                    Lists.map(
                        Phantoms.lam(
                            "s", Phantoms.list_([Phantoms.var("s")])
                        ),
                        Phantoms.var("defStmts"),
                    )
                ),
            ),
        ),
    )
    return _def(
        "encodeTypeAssignment",
        Phantoms.doc(
            "Encode a type definition, dispatching based on type structure", body
        ),
    )


def _encode_type_assignment_inner():
    dflt = Eithers.bind(
        _ap(_local("encodeType"), Phantoms.var("env"), Phantoms.var("typ")),
        Phantoms.lam(
            "typeExpr",
            Phantoms.right(
                _ap(
                    _local("encodeTypeDefSingle"),
                    Phantoms.var("env"),
                    Phantoms.var("name"),
                    Phantoms.var("comment"),
                    Phantoms.var("typeExpr"),
                )
            ),
        ),
    )
    body = Phantoms.lambdas(
        ["cx", "env", "name", "typ", "comment"],
        _let_chain(
            [
                ("stripped", _ap(_kref.strip_deannotate_type, Phantoms.var("typ"))),
                ("dflt", dflt),
            ],
            Phantoms.cases(
                Name("hydra.core.Type"),
                Phantoms.var("stripped"),
                Just(Phantoms.var("dflt")),
                [
                    Phantoms.field(
                        Name("forall"),
                        Phantoms.lam(
                            "ft",
                            _let_chain(
                                [
                                    (
                                        "tvar",
                                        Core.forall_type_parameter(Phantoms.var("ft")),
                                    ),
                                    (
                                        "body",
                                        Core.forall_type_body(Phantoms.var("ft")),
                                    ),
                                    (
                                        "newEnv",
                                        _ap(
                                            _local("extendEnvWithTypeVar"),
                                            Phantoms.var("env"),
                                            Phantoms.var("tvar"),
                                        ),
                                    ),
                                ],
                                _ap(
                                    _local("encodeTypeAssignmentInner"),
                                    Phantoms.var("cx"),
                                    Phantoms.var("newEnv"),
                                    Phantoms.var("name"),
                                    Phantoms.var("body"),
                                    Phantoms.var("comment"),
                                ),
                            ),
                        ),
                    ),
                    Phantoms.field(
                        Name("record"),
                        Phantoms.lam(
                            "rt",
                            Eithers.map_(
                                Phantoms.lam(
                                    "s", Phantoms.list_([Phantoms.var("s")])
                                ),
                                _ap(
                                    _local("encodeRecordType"),
                                    Phantoms.var("cx"),
                                    Phantoms.var("env"),
                                    Phantoms.var("name"),
                                    Phantoms.var("rt"),
                                    Phantoms.var("comment"),
                                ),
                            ),
                        ),
                    ),
                    Phantoms.field(
                        Name("union"),
                        Phantoms.lam(
                            "rt",
                            _ap(
                                _local("encodeUnionType"),
                                Phantoms.var("cx"),
                                Phantoms.var("env"),
                                Phantoms.var("name"),
                                Phantoms.var("rt"),
                                Phantoms.var("comment"),
                            ),
                        ),
                    ),
                    Phantoms.field(
                        Name("wrap"),
                        Phantoms.lam(
                            "wt",
                            _ap(
                                _local("encodeWrappedType"),
                                Phantoms.var("env"),
                                Phantoms.var("name"),
                                Phantoms.var("wt"),
                                Phantoms.var("comment"),
                            ),
                        ),
                    ),
                ],
            ),
        ),
    )
    return _def(
        "encodeTypeAssignmentInner",
        Phantoms.doc(
            "Encode the inner type definition, unwrapping forall types", body
        ),
    )


def _encode_union_elimination_inline():
    encode_branch = Phantoms.lam(
        "field",
        _let_chain(
            [
                ("fname", Core.field_name(Phantoms.var("field"))),
                ("fterm", Core.field_term(Phantoms.var("field"))),
                (
                    "isUnitVariant",
                    _ap(
                        _local("isVariantUnitType"),
                        Phantoms.var("rt"),
                        Phantoms.var("fname"),
                    ),
                ),
                (
                    "pyVariantName",
                    _ap(
                        _local("deconflictVariantName"),
                        Phantoms.true(),
                        Phantoms.var("env"),
                        Phantoms.var("tname"),
                        Phantoms.var("fname"),
                        _env("graph", "env"),
                    ),
                ),
                (
                    "pyTypeName",
                    _ap(
                        _kref.names_encode_name,
                        Phantoms.true(),
                        _kref.util_case_convention_pascal,
                        Phantoms.var("env"),
                        Phantoms.var("tname"),
                    ),
                ),
                (
                    "pyEnumValue",
                    _ap(
                        _kref.names_encode_enum_value,
                        Phantoms.var("env"),
                        Phantoms.var("fname"),
                    ),
                ),
                (
                    "enumMemberExpr",
                    _ap(
                        _kref.utils_py_primary_to_py_expression,
                        PySyn.primary_compound(
                            PySyn.primary_with_rhs(
                                _ap(
                                    _kref.utils_py_name_to_py_primary,
                                    Phantoms.var("pyTypeName"),
                                ),
                                PySyn.primary_rhs_project(Phantoms.var("pyEnumValue")),
                            )
                        ),
                    ),
                ),
                (
                    "isinstanceCheck",
                    Logic.if_else(
                        Phantoms.var("isEnum"),
                        PyDsl.py_comparison_to_py_expression(
                            PySyn.comparison(
                                _ap(
                                    _kref.utils_py_expression_to_bitwise_or,
                                    Phantoms.var("pyArg"),
                                ),
                                Phantoms.list_(
                                    [
                                        PyDsl.comp_pair_eq(
                                            _ap(
                                                _kref.utils_py_expression_to_bitwise_or,
                                                Phantoms.var("enumMemberExpr"),
                                            )
                                        )
                                    ]
                                ),
                            )
                        ),
                        _ap(
                            _kref.utils_function_call,
                            Phantoms.var("isinstancePrimary"),
                            Phantoms.list_(
                                [
                                    Phantoms.var("pyArg"),
                                    _ap(
                                        _kref.utils_py_name_to_py_expression,
                                        Phantoms.var("pyVariantName"),
                                    ),
                                ]
                            ),
                        ),
                    ),
                ),
            ],
            Eithers.bind(
                _ap(
                    _local("encodeTermInline"),
                    Phantoms.var("cx"),
                    Phantoms.var("env"),
                    Phantoms.false(),
                    Phantoms.var("fterm"),
                ),
                Phantoms.lam(
                    "pyBranch",
                    _let_chain(
                        [
                            (
                                "pyResult",
                                Logic.if_else(
                                    Phantoms.var("isEnum"),
                                    _ap(
                                        _kref.utils_function_call,
                                        _ap(
                                            _kref.utils_py_expression_to_py_primary,
                                            Phantoms.var("pyBranch"),
                                        ),
                                        Phantoms.list_([Phantoms.var("pyArg")]),
                                    ),
                                    Logic.if_else(
                                        Phantoms.var("isUnitVariant"),
                                        _ap(
                                            _kref.utils_function_call,
                                            _ap(
                                                _kref.utils_py_expression_to_py_primary,
                                                Phantoms.var("pyBranch"),
                                            ),
                                            Phantoms.list_(
                                                [Phantoms.var("pyArg")]
                                            ),
                                        ),
                                        _ap(
                                            _kref.utils_function_call,
                                            _ap(
                                                _kref.utils_py_expression_to_py_primary,
                                                Phantoms.var("pyBranch"),
                                            ),
                                            Phantoms.list_(
                                                [Phantoms.var("valueExpr")]
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ],
                        Phantoms.right(
                            Phantoms.pair(
                                Phantoms.var("isinstanceCheck"),
                                Phantoms.var("pyResult"),
                            )
                        ),
                    ),
                ),
            ),
        ),
    )
    build_chain = Phantoms.lambdas(
        ["elseExpr", "branchPair"],
        _let_chain(
            [
                ("checkExpr", Pairs.first(Phantoms.var("branchPair"))),
                ("resultExpr", Pairs.second(Phantoms.var("branchPair"))),
            ],
            PySyn.expression_conditional(
                PySyn.conditional(
                    _ap(
                        _kref.utils_py_expression_to_disjunction,
                        Phantoms.var("resultExpr"),
                    ),
                    _ap(
                        _kref.utils_py_expression_to_disjunction,
                        Phantoms.var("checkExpr"),
                    ),
                    Phantoms.var("elseExpr"),
                )
            ),
        ),
    )
    body = Phantoms.lambdas(
        ["cx", "env", "cs", "pyArg"],
        _let_chain(
            [
                ("tname", Core.case_statement_type_name(Phantoms.var("cs"))),
                ("mdefault", Core.case_statement_default(Phantoms.var("cs"))),
                ("cases_", Core.case_statement_cases(Phantoms.var("cs"))),
            ],
            Eithers.bind(
                _ap(
                    _kref.resolution_require_union_type,
                    Phantoms.var("cx"),
                    _ap(_local("pythonEnvironmentGetGraph"), Phantoms.var("env")),
                    Phantoms.var("tname"),
                ),
                Phantoms.lam(
                    "rt",
                    _let_chain(
                        [
                            (
                                "isEnum",
                                _ap(
                                    _kref.predicates_is_enum_row_type, Phantoms.var("rt")
                                ),
                            ),
                            (
                                "valueExpr",
                                _ap(
                                    _kref.utils_project_from_expression,
                                    Phantoms.var("pyArg"),
                                    _py_name("value"),
                                ),
                            ),
                            (
                                "isinstancePrimary",
                                _ap(
                                    _kref.utils_py_name_to_py_primary,
                                    _py_name("isinstance"),
                                ),
                            ),
                        ],
                        Eithers.bind(
                            Maybes.maybe(
                                Phantoms.right(
                                    _ap(
                                        _local("unsupportedExpression"),
                                        Phantoms.string(
                                            "no matching case in inline union elimination"
                                        ),
                                    )
                                ),
                                Phantoms.lam(
                                    "dflt",
                                    _ap(
                                        _local("encodeTermInline"),
                                        Phantoms.var("cx"),
                                        Phantoms.var("env"),
                                        Phantoms.false(),
                                        Phantoms.var("dflt"),
                                    ),
                                ),
                                Phantoms.var("mdefault"),
                            ),
                            Phantoms.lam(
                                "pyDefault",
                                _let_chain(
                                    [("encodeBranch", encode_branch)],
                                    Eithers.bind(
                                        Eithers.map_list(
                                            Phantoms.var("encodeBranch"),
                                            Phantoms.var("cases_"),
                                        ),
                                        Phantoms.lam(
                                            "encodedBranches",
                                            _let_chain(
                                                [("buildChain", build_chain)],
                                                Phantoms.right(
                                                    Lists.foldl(
                                                        Phantoms.var("buildChain"),
                                                        Phantoms.var("pyDefault"),
                                                        Lists.reverse(
                                                            Phantoms.var("encodedBranches")
                                                        ),
                                                    )
                                                ),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeUnionEliminationInline",
        Phantoms.doc(
            "Encode a union elimination as an inline conditional chain (isinstance-based ternary)",
            body,
        ),
    )


def _encode_union_field():
    body = Phantoms.lambdas(
        ["cx", "env", "unionName", "fieldType"],
        _let_chain(
            [
                ("fname", Core.field_type_name(Phantoms.var("fieldType"))),
                ("ftype", Core.field_type_type(Phantoms.var("fieldType"))),
            ],
            Eithers.bind(
                _ap(
                    _kref.annotations_get_type_description,
                    Phantoms.var("cx"),
                    _ap(_local("pythonEnvironmentGetGraph"), Phantoms.var("env")),
                    Phantoms.var("ftype"),
                ),
                Phantoms.lam(
                    "fcomment",
                    _let_chain(
                        [
                            (
                                "isUnit",
                                Equality.equal(
                                    _ap(
                                        _kref.strip_deannotate_type,
                                        Phantoms.var("ftype"),
                                    ),
                                    Core.type_unit,
                                ),
                            ),
                            (
                                "varName",
                                _ap(
                                    _local("deconflictVariantName"),
                                    Phantoms.false(),
                                    Phantoms.var("env"),
                                    Phantoms.var("unionName"),
                                    Phantoms.var("fname"),
                                    _env("graph", "env"),
                                ),
                            ),
                            (
                                "tparamNames",
                                _ap(
                                    _local("findTypeParams"),
                                    Phantoms.var("env"),
                                    Phantoms.var("ftype"),
                                ),
                            ),
                            (
                                "tparamPyNames",
                                Lists.map(
                                    _kref.names_encode_type_variable,
                                    Phantoms.var("tparamNames"),
                                ),
                            ),
                            (
                                "fieldParams",
                                Lists.map(
                                    _kref.utils_py_name_to_py_type_parameter,
                                    Phantoms.var("tparamPyNames"),
                                ),
                            ),
                            (
                                "body",
                                Logic.if_else(
                                    Phantoms.var("isUnit"),
                                    _ap(
                                        _kref.utils_indented_block,
                                        Phantoms.var("fcomment"),
                                        Phantoms.list_(
                                            [
                                                _ap(
                                                    _kref.utils_unit_variant_methods,
                                                    Phantoms.var("varName"),
                                                )
                                            ]
                                        ),
                                    ),
                                    _ap(
                                        _kref.utils_indented_block,
                                        Phantoms.var("fcomment"),
                                        Phantoms.list_([]),
                                    ),
                                ),
                            ),
                        ],
                        Eithers.bind(
                            Logic.if_else(
                                Phantoms.var("isUnit"),
                                Phantoms.right(Phantoms.nothing()),
                                Eithers.bind(
                                    _ap(
                                        _local("encodeTypeQuoted"),
                                        Phantoms.var("env"),
                                        Phantoms.var("ftype"),
                                    ),
                                    Phantoms.lam(
                                        "quotedType",
                                        Phantoms.right(
                                            Phantoms.just(
                                                _ap(
                                                    _local("variantArgs"),
                                                    Phantoms.var("quotedType"),
                                                    Phantoms.list_([]),
                                                )
                                            )
                                        ),
                                    ),
                                ),
                            ),
                            Phantoms.lam(
                                "margs",
                                Phantoms.right(
                                    _ap(
                                        _kref.utils_py_class_definition_to_py_statement,
                                        PySyn.class_definition(
                                            Phantoms.nothing(),
                                            Phantoms.var("varName"),
                                            Phantoms.var("fieldParams"),
                                            Phantoms.var("margs"),
                                            Phantoms.var("body"),
                                        ),
                                    )
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeUnionField",
        Phantoms.doc("Encode a union field as a variant class", body),
    )


def _encode_union_field_alt():
    body = Phantoms.lambdas(
        ["env", "unionName", "fieldType"],
        _let_chain(
            [
                ("fname", Core.field_type_name(Phantoms.var("fieldType"))),
                ("ftype", Core.field_type_type(Phantoms.var("fieldType"))),
                (
                    "tparamNames",
                    _ap(
                        _local("findTypeParams"),
                        Phantoms.var("env"),
                        Phantoms.var("ftype"),
                    ),
                ),
                (
                    "tparams",
                    Lists.map(
                        _kref.names_encode_type_variable,
                        Phantoms.var("tparamNames"),
                    ),
                ),
                (
                    "namePrim",
                    _ap(
                        _kref.utils_py_name_to_py_primary,
                        _ap(
                            _kref.names_variant_name,
                            Phantoms.false(),
                            Phantoms.var("env"),
                            Phantoms.var("unionName"),
                            Phantoms.var("fname"),
                        ),
                    ),
                ),
            ],
            Logic.if_else(
                Lists.null(Phantoms.var("tparams")),
                Phantoms.var("namePrim"),
                _let_chain(
                    [
                        (
                            "tparamExprs",
                            Lists.map(
                                _kref.utils_py_name_to_py_expression,
                                Phantoms.var("tparams"),
                            ),
                        ),
                    ],
                    _ap(
                        _kref.utils_primary_with_expression_slices,
                        Phantoms.var("namePrim"),
                        Phantoms.var("tparamExprs"),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeUnionFieldAlt",
        Phantoms.doc(
            "Encode a union field as a primary expression for | alternatives", body
        ),
    )


def _encode_forall_type():
    gather_params_inner = Phantoms.lambdas(
        ["t", "ps"],
        _type_cases_with_one_branch(
            Phantoms.var("t"),
            Phantoms.pair(Phantoms.var("t"), Lists.reverse(Phantoms.var("ps"))),
            Phantoms.field(
                Name("forall"),
                Phantoms.lam(
                    "forallT",
                    _ap(
                        Phantoms.var("gatherParams"),
                        _proj("hydra.core.ForallType", "body", "forallT"),
                        Lists.cons(
                            _proj("hydra.core.ForallType", "parameter", "forallT"),
                            Phantoms.var("ps"),
                        ),
                    ),
                ),
            ),
            ["annotated", "application", "function", "list", "literal", "map",
             "maybe", "either", "pair", "record", "set", "union", "unit",
             "variable", "void", "wrap"],
        ),
    )
    body = Phantoms.lambdas(
        ["env", "lt"],
        _let_chain(
            [("gatherParams", gather_params_inner)],
            _let_chain(
                [
                    (
                        "bodyAndParams",
                        _ap(
                            Phantoms.var("gatherParams"),
                            Phantoms.inject(
                                Name("hydra.core.Type"),
                                Name("forall"),
                                Phantoms.var("lt"),
                            ),
                            Phantoms.list_([]),
                        ),
                    ),
                    ("body", Pairs.first(Phantoms.var("bodyAndParams"))),
                    ("params", Pairs.second(Phantoms.var("bodyAndParams"))),
                ],
                Eithers.bind(
                    _ap(_local("encodeType"), Phantoms.var("env"), Phantoms.var("body")),
                    Phantoms.lam(
                        "pyBody",
                        Phantoms.right(
                            _ap(
                                _kref.utils_primary_and_params,
                                _ap(
                                    _kref.utils_py_expression_to_py_primary,
                                    Phantoms.var("pyBody"),
                                ),
                                Lists.map(
                                    Phantoms.lam(
                                        "n",
                                        PyDsl.py_name_to_py_expression(
                                            _py_name(Core.un_name(Phantoms.var("n")))
                                        ),
                                    ),
                                    Phantoms.var("params"),
                                ),
                            )
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeForallType",
        Phantoms.doc("Encode a forall type to Python expression", body),
    )


def _encode_function_type():
    gather_params_inner = Phantoms.lambdas(
        ["rdoms", "ftype"],
        _let_chain(
            [
                (
                    "innerCod",
                    _proj("hydra.core.FunctionType", "codomain", "ftype"),
                ),
                (
                    "dom",
                    _proj("hydra.core.FunctionType", "domain", "ftype"),
                ),
            ],
            _type_cases_with_one_branch(
                Phantoms.var("innerCod"),
                Phantoms.pair(
                    Lists.reverse(
                        Lists.cons(Phantoms.var("dom"), Phantoms.var("rdoms"))
                    ),
                    Phantoms.var("innerCod"),
                ),
                Phantoms.field(
                    Name("function"),
                    Phantoms.lam(
                        "ft2",
                        _ap(
                            Phantoms.var("gatherParams"),
                            Lists.cons(Phantoms.var("dom"), Phantoms.var("rdoms")),
                            Phantoms.var("ft2"),
                        ),
                    ),
                ),
                ["annotated", "application", "forall", "list", "literal", "map",
                 "maybe", "either", "pair", "record", "set", "union", "unit",
                 "variable", "void", "wrap"],
            ),
        ),
    )
    body = Phantoms.lambdas(
        ["env", "ft"],
        _let_chain(
            [("gatherParams", gather_params_inner)],
            _let_chain(
                [
                    (
                        "domsAndCod",
                        _ap(
                            Phantoms.var("gatherParams"),
                            Phantoms.list_([]),
                            Phantoms.var("ft"),
                        ),
                    ),
                    ("doms", Pairs.first(Phantoms.var("domsAndCod"))),
                    ("cod", Pairs.second(Phantoms.var("domsAndCod"))),
                ],
                Eithers.bind(
                    Eithers.map_list(
                        _ap(_local("encodeType"), Phantoms.var("env")),
                        Phantoms.var("doms"),
                    ),
                    Phantoms.lam(
                        "pydoms",
                        Eithers.bind(
                            _ap(
                                _local("encodeType"),
                                Phantoms.var("env"),
                                Phantoms.var("cod"),
                            ),
                            Phantoms.lam(
                                "pycod",
                                Phantoms.right(
                                    _ap(
                                        _kref.utils_py_primary_to_py_expression,
                                        _ap(
                                            _kref.utils_primary_with_slices,
                                            PySyn.primary_simple(
                                                PySyn.atom_name(_py_name("Callable"))
                                            ),
                                            _ap(
                                                _kref.utils_py_primary_to_py_slice,
                                                PySyn.primary_simple(
                                                    PySyn.atom_list(
                                                        _ap(
                                                            _kref.utils_py_list,
                                                            Phantoms.var("pydoms"),
                                                        )
                                                    )
                                                ),
                                            ),
                                            Phantoms.list_(
                                                [
                                                    PySyn.slice_or_starred_expression_slice(
                                                        _ap(
                                                            _kref.utils_py_expression_to_py_slice,
                                                            Phantoms.var("pycod"),
                                                        )
                                                    )
                                                ]
                                            ),
                                        ),
                                    )
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeFunctionType",
        Phantoms.doc("Encode a function type to Python Callable expression", body),
    )


def _encode_integer_value():
    to_py_int = Phantoms.lam(
        "n",
        Phantoms.right(
            _ap(
                _kref.utils_py_atom_to_py_expression,
                PySyn.atom_number(PySyn.number_integer(Phantoms.var("n"))),
            )
        ),
    )

    def lift(fname, conv_fn):
        return Phantoms.field(
            Name(fname),
            Phantoms.lam(
                "i",
                _ap(Phantoms.var("toPyInt"), conv_fn(Phantoms.var("i"))),
            ),
        )

    body = Phantoms.lambdas(
        ["iv"],
        _let_chain(
            [("toPyInt", to_py_int)],
            Phantoms.cases(
                Name("hydra.core.IntegerValue"),
                Phantoms.var("iv"),
                Nothing(),
                [
                    Phantoms.field(
                        Name("bigint"),
                        Phantoms.lam(
                            "i",
                            _ap(Phantoms.var("toPyInt"), Phantoms.var("i")),
                        ),
                    ),
                    lift("int8", Literals.int8_to_bigint),
                    lift("int16", Literals.int16_to_bigint),
                    lift("int32", Literals.int32_to_bigint),
                    lift("int64", Literals.int64_to_bigint),
                    lift("uint8", Literals.uint8_to_bigint),
                    lift("uint16", Literals.uint16_to_bigint),
                    lift("uint32", Literals.uint32_to_bigint),
                    lift("uint64", Literals.uint64_to_bigint),
                ],
            ),
        ),
    )
    return _def(
        "encodeIntegerValue",
        Phantoms.doc("Encode an integer value to a Python expression", body),
    )


def _encode_literal():
    body = Phantoms.lambdas(
        ["lit"],
        Phantoms.cases(
            Name("hydra.core.Literal"),
            Phantoms.var("lit"),
            Nothing(),
            [
                Phantoms.field(
                    Name("binary"),
                    Phantoms.lam(
                        "bs",
                        _let_chain(
                            [("byteValues", Literals.binary_to_bytes(Phantoms.var("bs")))],
                            Phantoms.right(
                                _ap(
                                    _kref.utils_function_call,
                                    PySyn.primary_simple(
                                        PySyn.atom_name(_py_name("bytes"))
                                    ),
                                    Phantoms.list_(
                                        [
                                            _ap(
                                                _kref.utils_py_atom_to_py_expression,
                                                PySyn.atom_list(
                                                    _ap(
                                                        _kref.utils_py_list,
                                                        Lists.map(
                                                            Phantoms.lam(
                                                                "byteVal",
                                                                _ap(
                                                                    _kref.utils_py_atom_to_py_expression,
                                                                    PySyn.atom_number(
                                                                        PySyn.number_integer(
                                                                            Literals.int32_to_bigint(
                                                                                Phantoms.var("byteVal")
                                                                            )
                                                                        )
                                                                    ),
                                                                ),
                                                            ),
                                                            Phantoms.var("byteValues"),
                                                        ),
                                                    )
                                                ),
                                            )
                                        ]
                                    ),
                                )
                            ),
                        ),
                    ),
                ),
                Phantoms.field(
                    Name("boolean"),
                    Phantoms.lam(
                        "b",
                        Phantoms.right(
                            _ap(
                                _kref.utils_py_atom_to_py_expression,
                                Logic.if_else(
                                    Phantoms.var("b"),
                                    PySyn.atom_true,
                                    PySyn.atom_false,
                                ),
                            )
                        ),
                    ),
                ),
                Phantoms.field(
                    Name("decimal"),
                    Phantoms.lam(
                        "d",
                        Phantoms.right(
                            _ap(
                                _kref.utils_function_call,
                                _ap(_kref.utils_py_name_to_py_primary, _py_name("Decimal")),
                                Phantoms.list_(
                                    [
                                        _ap(
                                            _kref.utils_single_quoted_string,
                                            Literals.show_decimal(Phantoms.var("d")),
                                        )
                                    ]
                                ),
                            )
                        ),
                    ),
                ),
                Phantoms.field(
                    Name("float"),
                    Phantoms.lam(
                        "f", _ap(_local("encodeFloatValue"), Phantoms.var("f"))
                    ),
                ),
                Phantoms.field(
                    Name("integer"),
                    Phantoms.lam(
                        "i", _ap(_local("encodeIntegerValue"), Phantoms.var("i"))
                    ),
                ),
                Phantoms.field(
                    Name("string"),
                    Phantoms.lam(
                        "s",
                        Phantoms.right(
                            _ap(
                                _kref.utils_string_to_py_expression,
                                PySyn.quote_style_double,
                                Phantoms.var("s"),
                            )
                        ),
                    ),
                ),
            ],
        ),
    )
    return _def(
        "encodeLiteral",
        Phantoms.doc("Encode a literal value to a Python expression", body),
    )


def _encode_literal_type():
    body = Phantoms.lambdas(
        ["lt"],
        _let_chain(
            [
                (
                    "findName",
                    Phantoms.cases(
                        Name("hydra.core.LiteralType"),
                        Phantoms.var("lt"),
                        Nothing(),
                        [
                            Phantoms.field(
                                Name("binary"),
                                Phantoms.constant(Phantoms.string("bytes")),
                            ),
                            Phantoms.field(
                                Name("boolean"),
                                Phantoms.constant(Phantoms.string("bool")),
                            ),
                            Phantoms.field(
                                Name("decimal"),
                                Phantoms.constant(Phantoms.string("Decimal")),
                            ),
                            Phantoms.field(
                                Name("float"),
                                Phantoms.lam(
                                    "ft",
                                    Phantoms.cases(
                                        Name("hydra.core.FloatType"),
                                        Phantoms.var("ft"),
                                        Nothing(),
                                        [
                                            Phantoms.field(
                                                Name("bigfloat"),
                                                Phantoms.constant(Phantoms.string("Decimal")),
                                            ),
                                            Phantoms.field(
                                                Name("float32"),
                                                Phantoms.constant(Phantoms.string("float")),
                                            ),
                                            Phantoms.field(
                                                Name("float64"),
                                                Phantoms.constant(Phantoms.string("float")),
                                            ),
                                        ],
                                    ),
                                ),
                            ),
                            Phantoms.field(
                                Name("integer"),
                                Phantoms.constant(Phantoms.string("int")),
                            ),
                            Phantoms.field(
                                Name("string"),
                                Phantoms.constant(Phantoms.string("str")),
                            ),
                        ],
                    ),
                )
            ],
            Phantoms.right(
                PyDsl.py_name_to_py_expression(_py_name(Phantoms.var("findName")))
            ),
        ),
    )
    return _def(
        "encodeLiteralType",
        Phantoms.doc("Encode a literal type to a Python type expression", body),
    )


def _environment_type_parameters():
    # Lists.map (PyUtils.pyNameToPyTypeParameter <.> PyNames.encodeTypeVariable)
    #          (Pairs.first (env.boundTypeVariables))
    # The composition <.> is hydra.lib.lists.compose? Actually `Util.compose`. Let me check
    # — actually it's just function composition. In the term-level DSL, this is constructed via
    # Phantoms.compose.
    body = Phantoms.lambdas(
        ["env"],
        Lists.map(
            Phantoms.compose(
                _kref.utils_py_name_to_py_type_parameter,
                _kref.names_encode_type_variable,
            ),
            Pairs.first(_env("boundTypeVariables", "env")),
        ),
    )
    return _def(
        "environmentTypeParameters",
        Phantoms.doc(
            "Get type parameters from environment as Python TypeParameters", body
        ),
    )


def _extend_env_with_lambda_params():
    inner_go = Phantoms.lambdas(
        ["e", "t"],
        Phantoms.cases(
            Name("hydra.core.Term"),
            _ap(_kref.strip_deannotate_and_detype_term, Phantoms.var("t")),
            Just(Phantoms.var("e")),
            [
                Phantoms.field(
                    Name("lambda"),
                    Phantoms.lam(
                        "lam",
                        _let_chain(
                            [
                                (
                                    "newTc",
                                    _ap(
                                        _kref.scoping_extend_graph_for_lambda,
                                        _ap(
                                            _local("pythonEnvironmentGetGraph"),
                                            Phantoms.var("e"),
                                        ),
                                        Phantoms.var("lam"),
                                    ),
                                ),
                                (
                                    "newEnv",
                                    _ap(
                                        _local("pythonEnvironmentSetGraph"),
                                        Phantoms.var("newTc"),
                                        Phantoms.var("e"),
                                    ),
                                ),
                            ],
                            _ap(
                                Phantoms.var("go"),
                                Phantoms.var("newEnv"),
                                Core.lambda_body(Phantoms.var("lam")),
                            ),
                        ),
                    ),
                ),
            ],
        ),
    )
    body = Phantoms.lambdas(
        ["env", "term"],
        _let_chain(
            [("go", inner_go)],
            _ap(Phantoms.var("go"), Phantoms.var("env"), Phantoms.var("term")),
        ),
    )
    return _def(
        "extendEnvWithLambdaParams",
        Phantoms.doc(
            "Extend environment with lambda parameters from a term", body
        ),
    )


def _extend_env_with_type_var():
    body = Phantoms.lambdas(
        ["env", "var_"],
        _let_chain(
            [
                ("oldBound", _env("boundTypeVariables", "env")),
                ("tparamList", Pairs.first(Phantoms.var("oldBound"))),
                ("tparamMap", Pairs.second(Phantoms.var("oldBound"))),
                (
                    "newList",
                    Lists.concat2(
                        Phantoms.var("tparamList"),
                        Phantoms.list_([Phantoms.var("var_")]),
                    ),
                ),
                (
                    "newMap",
                    Maps.insert(
                        Phantoms.var("var_"),
                        _ap(_kref.names_encode_type_variable, Phantoms.var("var_")),
                        Phantoms.var("tparamMap"),
                    ),
                ),
            ],
            Phantoms.record(
                Name("hydra.python.environment.PythonEnvironment"),
                [
                    Phantoms.field(
                        Name("namespaces"), _env("namespaces", "env")
                    ),
                    Phantoms.field(
                        Name("boundTypeVariables"),
                        Phantoms.pair(Phantoms.var("newList"), Phantoms.var("newMap")),
                    ),
                    Phantoms.field(Name("graph"), _env("graph", "env")),
                    Phantoms.field(
                        Name("nullaryBindings"), _env("nullaryBindings", "env")
                    ),
                    Phantoms.field(Name("version"), _env("version", "env")),
                    Phantoms.field(Name("skipCasts"), _env("skipCasts", "env")),
                    Phantoms.field(
                        Name("inlineVariables"), _env("inlineVariables", "env")
                    ),
                ],
            ),
        ),
    )
    return _def(
        "extendEnvWithTypeVar",
        Phantoms.doc(
            "Extend a PythonEnvironment with a new bound type variable", body
        ),
    )


def _encode_union_type():
    enum_branch = Eithers.bind(
        Eithers.map_list(
            _ap(
                _local("encodeEnumValueAssignment"),
                Phantoms.var("cx"),
                Phantoms.var("env"),
            ),
            Phantoms.var("rowType"),
        ),
        Phantoms.lam(
            "vals",
            _let_chain(
                [
                    (
                        "body",
                        _ap(
                            _kref.utils_indented_block,
                            Phantoms.var("comment"),
                            Phantoms.var("vals"),
                        ),
                    ),
                    ("enumName", _py_name("Enum")),
                    (
                        "args",
                        Phantoms.just(
                            _ap(
                                _kref.utils_py_expressions_to_py_args,
                                Phantoms.list_(
                                    [
                                        _ap(
                                            _kref.utils_py_name_to_py_expression,
                                            Phantoms.var("enumName"),
                                        )
                                    ]
                                ),
                            )
                        ),
                    ),
                    (
                        "pyName",
                        _ap(
                            _kref.names_encode_name,
                            Phantoms.false(),
                            _kref.util_case_convention_pascal,
                            Phantoms.var("env"),
                            Phantoms.var("name"),
                        ),
                    ),
                    (
                        "typeConstStmt",
                        _ap(
                            _kref.utils_dotted_assignment_statement,
                            Phantoms.var("pyName"),
                            _ap(
                                _kref.names_encode_constant_for_type_name,
                                Phantoms.var("env"),
                                Phantoms.var("name"),
                            ),
                            _ap(
                                _kref.utils_function_call,
                                _ap(
                                    _kref.utils_py_name_to_py_primary,
                                    _ap(
                                        _kref.names_encode_name,
                                        Phantoms.true(),
                                        _kref.util_case_convention_pascal,
                                        Phantoms.var("env"),
                                        Core.name(Phantoms.string("hydra.core.Name")),
                                    ),
                                ),
                                Phantoms.list_(
                                    [
                                        _ap(
                                            _kref.utils_double_quoted_string,
                                            Core.un_name(Phantoms.var("name")),
                                        )
                                    ]
                                ),
                            ),
                        ),
                    ),
                ],
                Phantoms.right(
                    Phantoms.list_(
                        [
                            _ap(
                                _kref.utils_py_class_definition_to_py_statement,
                                PySyn.class_definition(
                                    Phantoms.nothing(),
                                    Phantoms.var("pyName"),
                                    Phantoms.list_([]),
                                    Phantoms.var("args"),
                                    Phantoms.var("body"),
                                ),
                            ),
                            Phantoms.var("typeConstStmt"),
                        ]
                    )
                ),
            ),
        ),
    )
    union_branch = _let_chain(
        [
            (
                "constStmts",
                _ap(
                    _local("encodeNameConstants"),
                    Phantoms.var("env"),
                    Phantoms.var("name"),
                    Phantoms.var("rowType"),
                ),
            ),
        ],
        Eithers.bind(
            Eithers.map_list(
                _ap(
                    _local("encodeUnionField"),
                    Phantoms.var("cx"),
                    Phantoms.var("env"),
                    Phantoms.var("name"),
                ),
                Phantoms.var("rowType"),
            ),
            Phantoms.lam(
                "fieldStmts",
                _let_chain(
                    [
                        (
                            "tparams",
                            _ap(
                                _local("environmentTypeParameters"),
                                Phantoms.var("env"),
                            ),
                        ),
                        (
                            "unionAlts",
                            Lists.map(
                                _ap(
                                    _local("encodeUnionFieldAlt"),
                                    Phantoms.var("env"),
                                    Phantoms.var("name"),
                                ),
                                Phantoms.var("rowType"),
                            ),
                        ),
                        (
                            "unionStmts",
                            _ap(
                                _local("unionTypeStatementsFor"),
                                Phantoms.var("env"),
                                _ap(
                                    _kref.names_encode_name,
                                    Phantoms.false(),
                                    _kref.util_case_convention_pascal,
                                    Phantoms.var("env"),
                                    Phantoms.var("name"),
                                ),
                                Phantoms.var("tparams"),
                                Phantoms.var("comment"),
                                _ap(
                                    _kref.utils_or_expression,
                                    Phantoms.var("unionAlts"),
                                ),
                                Phantoms.var("constStmts"),
                            ),
                        ),
                    ],
                    Phantoms.right(
                        Lists.concat2(
                            Phantoms.var("fieldStmts"), Phantoms.var("unionStmts")
                        )
                    ),
                ),
            ),
        ),
    )
    body = Phantoms.lambdas(
        ["cx", "env", "name", "rowType", "comment"],
        Logic.if_else(
            _ap(_kref.predicates_is_enum_row_type, Phantoms.var("rowType")),
            enum_branch,
            union_branch,
        ),
    )
    return _def(
        "encodeUnionType",
        Phantoms.doc(
            "Encode a union type as an enum (for unit-only fields) or variant classes",
            body,
        ),
    )


def _encode_variable():
    # asFunctionRefHelper: same shape used in many branches
    def as_function_ref(typ_var):
        return Logic.if_else(
            Logic.not_(
                Sets.null(
                    _ap(
                        _kref.variables_free_variables_in_type,
                        Phantoms.var(typ_var),
                    )
                )
            ),
            _ap(
                _local("makeSimpleLambda"),
                _ap(_kref.arity_type_arity, Phantoms.var(typ_var)),
                Phantoms.var("asVariable"),
            ),
            Phantoms.var("asVariable"),
        )

    # When name IS in tcMetadata
    metadata_branch = Logic.if_else(
        Logic.and_(
            Equality.equal(
                _ap(_kref.arity_type_arity, Phantoms.var("typ")),
                Phantoms.int32(0),
            ),
            _ap(
                _kref.predicates_is_complex_variable,
                Phantoms.var("tc"),
                Phantoms.var("name"),
            ),
        ),
        Phantoms.right(Phantoms.var("asFunctionCall")),
        _let_chain(
            [("asFunctionRef", as_function_ref("typ"))],
            Phantoms.right(Phantoms.var("asFunctionRef")),
        ),
    )

    # When name in graphBoundTypes but NOT in metadata: check graph elements
    el_typed_branch = Phantoms.lam(
        "ts",
        Logic.if_else(
            Logic.and_(
                Logic.and_(
                    Equality.equal(
                        _ap(_kref.arity_type_arity, Phantoms.var("typ")),
                        Phantoms.int32(0),
                    ),
                    _ap(
                        _kref.predicates_is_complex_binding,
                        Phantoms.var("tc"),
                        Phantoms.var("el"),
                    ),
                ),
                Logic.not_(Phantoms.var("elTrivial")),
            ),
            Phantoms.right(Phantoms.var("asFunctionCall")),
            _let_chain(
                [("asFunctionRef", as_function_ref("typ"))],
                Phantoms.right(Phantoms.var("asFunctionRef")),
            ),
        ),
    )
    el_branch = Phantoms.lam(
        "el",
        _let_chain(
            [
                (
                    "elTrivial",
                    _ap(
                        _kref.predicates_is_trivial_term,
                        Core.binding_term(Phantoms.var("el")),
                    ),
                ),
            ],
            Maybes.maybe(
                Logic.if_else(
                    Logic.and_(
                        Equality.equal(
                            _ap(_kref.arity_type_arity, Phantoms.var("typ")),
                            Phantoms.int32(0),
                        ),
                        Logic.not_(Phantoms.var("elTrivial")),
                    ),
                    Phantoms.right(Phantoms.var("asFunctionCall")),
                    _let_chain(
                        [("asFunctionRef", as_function_ref("typ"))],
                        Phantoms.right(Phantoms.var("asFunctionRef")),
                    ),
                ),
                el_typed_branch,
                Core.binding_type_scheme(Phantoms.var("el")),
            ),
        ),
    )
    not_in_metadata_branch = Maybes.maybe(
        _let_chain(
            [("asFunctionRef", as_function_ref("typ"))],
            Phantoms.right(Phantoms.var("asFunctionRef")),
        ),
        el_branch,
        _ap(_kref.lexical_lookup_binding, Phantoms.var("g"), Phantoms.var("name")),
    )

    has_typ_branch = Phantoms.lam(
        "typ",
        Logic.if_else(
            Sets.member(Phantoms.var("name"), Phantoms.var("tcLambdaVars")),
            Phantoms.right(Phantoms.var("asVariable")),
            Logic.if_else(
                Sets.member(Phantoms.var("name"), Phantoms.var("inlineVars")),
                _let_chain(
                    [("asFunctionRef", as_function_ref("typ"))],
                    Phantoms.right(Phantoms.var("asFunctionRef")),
                ),
                Logic.if_else(
                    Logic.not_(
                        Maps.member(
                            Phantoms.var("name"), Phantoms.var("tcMetadata")
                        )
                    ),
                    not_in_metadata_branch,
                    metadata_branch,
                ),
            ),
        ),
    )

    # When name NOT in graphBoundTypes
    # graph element with type scheme branch
    el_branch_no_typ_inner = Phantoms.lam(
        "ts",
        Logic.if_else(
            Logic.and_(
                Logic.and_(
                    Equality.equal(
                        _ap(_kref.arity_type_scheme_arity, Phantoms.var("ts")),
                        Phantoms.int32(0),
                    ),
                    _ap(
                        _kref.predicates_is_complex_binding,
                        Phantoms.var("tc"),
                        Phantoms.var("el"),
                    ),
                ),
                Logic.not_(Phantoms.var("elTrivial1")),
            ),
            Phantoms.right(Phantoms.var("asFunctionCall")),
            _let_chain(
                [
                    (
                        "asFunctionRef",
                        Logic.if_else(
                            Logic.not_(
                                Lists.null(
                                    Core.type_scheme_variables(Phantoms.var("ts"))
                                )
                            ),
                            _ap(
                                _local("makeSimpleLambda"),
                                _ap(
                                    _kref.arity_type_arity,
                                    Core.type_scheme_body(Phantoms.var("ts")),
                                ),
                                Phantoms.var("asVariable"),
                            ),
                            Phantoms.var("asVariable"),
                        ),
                    ),
                ],
                Phantoms.right(Phantoms.var("asFunctionRef")),
            ),
        ),
    )
    no_prim_no_typ_el_branch = Phantoms.lam(
        "el",
        _let_chain(
            [
                (
                    "elTrivial1",
                    _ap(
                        _kref.predicates_is_trivial_term,
                        Core.binding_term(Phantoms.var("el")),
                    ),
                ),
            ],
            Maybes.maybe(
                Phantoms.right(Phantoms.var("asVariable")),
                el_branch_no_typ_inner,
                Core.binding_type_scheme(Phantoms.var("el")),
            ),
        ),
    )
    not_in_graphBoundTypes_no_prim = Maybes.maybe(
        Maybes.maybe(
            Phantoms.left(
                Errors_dsl.error_other(
                    Errors_dsl.other_error(
                        Strings.cat2(
                            Phantoms.string("Unknown variable: "),
                            Core.un_name(Phantoms.var("name")),
                        )
                    )
                )
            ),
            Phantoms.constant(Phantoms.right(Phantoms.var("asFunctionCall"))),
            Maps.lookup(Phantoms.var("name"), Phantoms.var("tcMetadata")),
        ),
        no_prim_no_typ_el_branch,
        _ap(_kref.lexical_lookup_binding, Phantoms.var("g"), Phantoms.var("name")),
    )
    is_prim_no_typ_branch = Phantoms.lam(
        "prim",
        _let_chain(
            [
                (
                    "primArity",
                    _ap(_kref.arity_primitive_arity, Phantoms.var("prim")),
                ),
            ],
            Logic.if_else(
                Equality.equal(Phantoms.var("primArity"), Phantoms.int32(0)),
                Phantoms.right(Phantoms.var("asFunctionCall")),
                _let_chain(
                    [
                        (
                            "ts",
                            _proj("hydra.graph.Primitive", "typeScheme", "prim"),
                        ),
                        (
                            "asFunctionRef",
                            Logic.if_else(
                                Logic.not_(
                                    Lists.null(
                                        Core.type_scheme_variables(Phantoms.var("ts"))
                                    )
                                ),
                                _ap(
                                    _local("makeSimpleLambda"),
                                    _ap(
                                        _kref.arity_type_arity,
                                        Core.type_scheme_body(Phantoms.var("ts")),
                                    ),
                                    Phantoms.var("asVariable"),
                                ),
                                Phantoms.var("asVariable"),
                            ),
                        ),
                    ],
                    Phantoms.right(Phantoms.var("asFunctionRef")),
                ),
            ),
        ),
    )
    no_typ_branch = Logic.if_else(
        Sets.member(Phantoms.var("name"), Phantoms.var("tcLambdaVars")),
        Phantoms.right(Phantoms.var("asVariable")),
        Logic.if_else(
            Sets.member(Phantoms.var("name"), Phantoms.var("inlineVars")),
            Phantoms.right(Phantoms.var("asVariable")),
            Maybes.maybe(
                not_in_graphBoundTypes_no_prim,
                is_prim_no_typ_branch,
                _ap(
                    _kref.lexical_lookup_primitive,
                    Phantoms.var("g"),
                    Phantoms.var("name"),
                ),
            ),
        ),
    )

    empty_args_branch = Maybes.maybe(
        no_typ_branch, has_typ_branch, Phantoms.var("mTyp")
    )

    # Non-empty args branch: primitive lookup
    prim_branch = Phantoms.lam(
        "prim",
        _let_chain(
            [
                (
                    "primArity",
                    _ap(_kref.arity_primitive_arity, Phantoms.var("prim")),
                ),
            ],
            Logic.if_else(
                Equality.equal(
                    Phantoms.var("primArity"), Lists.length(Phantoms.var("args"))
                ),
                Phantoms.right(Phantoms.var("asFunctionCall")),
                _let_chain(
                    [
                        (
                            "numRemaining",
                            Math.sub(
                                Phantoms.var("primArity"),
                                Lists.length(Phantoms.var("args")),
                            ),
                        ),
                        (
                            "remainingParams",
                            Lists.map(
                                Phantoms.lam(
                                    "i",
                                    _py_name(
                                        Strings.cat2(
                                            Phantoms.string("x"),
                                            Literals.show_int32(Phantoms.var("i")),
                                        )
                                    ),
                                ),
                                Math.range_(
                                    Phantoms.int32(1), Phantoms.var("numRemaining")
                                ),
                            ),
                        ),
                        (
                            "remainingExprs",
                            Lists.map(
                                Phantoms.lam(
                                    "n",
                                    PyDsl.py_name_to_py_expression(Phantoms.var("n")),
                                ),
                                Phantoms.var("remainingParams"),
                            ),
                        ),
                        (
                            "allArgs",
                            Lists.concat2(
                                Phantoms.var("args"), Phantoms.var("remainingExprs")
                            ),
                        ),
                        (
                            "fullCall",
                            _ap(
                                _kref.utils_function_call,
                                _ap(
                                    _kref.utils_py_name_to_py_primary,
                                    _ap(
                                        _kref.names_encode_name,
                                        Phantoms.true(),
                                        _kref.util_case_convention_lower_snake,
                                        Phantoms.var("env"),
                                        Phantoms.var("name"),
                                    ),
                                ),
                                Phantoms.var("allArgs"),
                            ),
                        ),
                    ],
                    Phantoms.right(
                        _ap(
                            _local("makeUncurriedLambda"),
                            Phantoms.var("remainingParams"),
                            Phantoms.var("fullCall"),
                        )
                    ),
                ),
            ),
        ),
    )
    nonempty_args_branch = Maybes.maybe(
        Phantoms.right(Phantoms.var("asFunctionCall")),
        prim_branch,
        _ap(
            _kref.lexical_lookup_primitive,
            Phantoms.var("g"),
            Phantoms.var("name"),
        ),
    )

    body = Phantoms.lambdas(
        ["cx", "env", "name", "args"],
        _let_chain(
            [
                (
                    "g",
                    _ap(_local("pythonEnvironmentGetGraph"), Phantoms.var("env")),
                ),
                ("tc", _env("graph", "env")),
                ("tcTypes", Graph_dsl.graph_bound_types(Phantoms.var("tc"))),
                (
                    "tcLambdaVars",
                    Graph_dsl.graph_lambda_variables(Phantoms.var("tc")),
                ),
                ("tcMetadata", Graph_dsl.graph_metadata(Phantoms.var("tc"))),
                ("inlineVars", _env("inlineVariables", "env")),
                (
                    "mTypScheme",
                    Maps.lookup(Phantoms.var("name"), Phantoms.var("tcTypes")),
                ),
                (
                    "mTyp",
                    Maybes.map(
                        Phantoms.lam(
                            "ts_", Core.type_scheme_body(Phantoms.var("ts_"))
                        ),
                        Phantoms.var("mTypScheme"),
                    ),
                ),
                (
                    "asVariable",
                    _ap(
                        _kref.names_term_variable_reference,
                        Phantoms.var("env"),
                        Phantoms.var("name"),
                    ),
                ),
                (
                    "asFunctionCall",
                    _ap(
                        _kref.utils_function_call,
                        _ap(
                            _kref.utils_py_name_to_py_primary,
                            _ap(
                                _kref.names_encode_name,
                                Phantoms.true(),
                                _kref.util_case_convention_lower_snake,
                                Phantoms.var("env"),
                                Phantoms.var("name"),
                            ),
                        ),
                        Phantoms.var("args"),
                    ),
                ),
            ],
            Logic.if_else(
                Logic.not_(Lists.null(Phantoms.var("args"))),
                nonempty_args_branch,
                empty_args_branch,
            ),
        ),
    )
    return _def(
        "encodeVariable",
        Phantoms.doc(
            "Encode a variable reference to a Python expression", body
        ),
    )


def _encode_wrapped_type():
    body = Phantoms.lambdas(
        ["env", "name", "typ", "comment"],
        _let_chain(
            [
                ("tparamList", Pairs.first(_env("boundTypeVariables", "env"))),
            ],
            Eithers.bind(
                _ap(
                    _local("encodeTypeQuoted"),
                    Phantoms.var("env"),
                    Phantoms.var("typ"),
                ),
                Phantoms.lam(
                    "ptypeQuoted",
                    _let_chain(
                        [
                            (
                                "pyName",
                                _ap(
                                    _kref.names_encode_name,
                                    Phantoms.false(),
                                    _kref.util_case_convention_pascal,
                                    Phantoms.var("env"),
                                    Phantoms.var("name"),
                                ),
                            ),
                            (
                                "body",
                                _ap(
                                    _kref.utils_indented_block,
                                    Phantoms.var("comment"),
                                    Phantoms.list_([]),
                                ),
                            ),
                            (
                                "typeConstStmt",
                                _ap(
                                    _kref.utils_dotted_assignment_statement,
                                    Phantoms.var("pyName"),
                                    _ap(
                                        _kref.names_encode_constant_for_type_name,
                                        Phantoms.var("env"),
                                        Phantoms.var("name"),
                                    ),
                                    _ap(
                                        _kref.utils_function_call,
                                        _ap(
                                            _kref.utils_py_name_to_py_primary,
                                            _ap(
                                                _kref.names_encode_name,
                                                Phantoms.true(),
                                                _kref.util_case_convention_pascal,
                                                Phantoms.var("env"),
                                                Core.name(Phantoms.string("hydra.core.Name")),
                                            ),
                                        ),
                                        Phantoms.list_(
                                            [
                                                _ap(
                                                    _kref.utils_double_quoted_string,
                                                    Core.un_name(Phantoms.var("name")),
                                                )
                                            ]
                                        ),
                                    ),
                                ),
                            ),
                        ],
                        Phantoms.right(
                            Phantoms.list_(
                                [
                                    _ap(
                                        _kref.utils_py_class_definition_to_py_statement,
                                        PySyn.class_definition(
                                            Phantoms.nothing(),
                                            Phantoms.var("pyName"),
                                            Lists.map(
                                                Phantoms.compose(
                                                    _kref.utils_py_name_to_py_type_parameter,
                                                    _kref.names_encode_type_variable,
                                                ),
                                                _ap(
                                                    _local("findTypeParams"),
                                                    Phantoms.var("env"),
                                                    Phantoms.var("typ"),
                                                ),
                                            ),
                                            Phantoms.just(
                                                _ap(
                                                    _local("variantArgs"),
                                                    Phantoms.var("ptypeQuoted"),
                                                    Phantoms.var("tparamList"),
                                                )
                                            ),
                                            Phantoms.var("body"),
                                        ),
                                    ),
                                    Phantoms.var("typeConstStmt"),
                                ]
                            )
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeWrappedType",
        Phantoms.doc(
            "Encode a wrapped type (newtype) to a Python class definition", body
        ),
    )


def _enum_variant_pattern():
    body = Phantoms.lambdas(
        ["env", "typeName", "fieldName"],
        PySyn.closed_pattern_value(
            PySyn.value_pattern(
                PySyn.attribute(
                    Phantoms.list_(
                        [
                            _ap(
                                _kref.names_encode_name,
                                Phantoms.true(),
                                _kref.util_case_convention_pascal,
                                Phantoms.var("env"),
                                Phantoms.var("typeName"),
                            ),
                            _ap(
                                _kref.names_encode_enum_value,
                                Phantoms.var("env"),
                                Phantoms.var("fieldName"),
                            ),
                        ]
                    )
                )
            )
        ),
    )
    return _def(
        "enumVariantPattern",
        Phantoms.doc("Create a value pattern for an enum variant", body),
    )


def _extend_meta_for_term():
    step_inner = Phantoms.lambdas(
        ["meta", "t"],
        Phantoms.cases(
            Name("hydra.core.Term"),
            Phantoms.var("t"),
            Just(Phantoms.var("meta")),
            [
                Phantoms.field(
                    Name("either"),
                    Phantoms.lam(
                        "e",
                        _let_chain(
                            [
                                (
                                    "metaWithCast",
                                    _ap(
                                        _local("setMetaUsesCast"),
                                        Phantoms.true(),
                                        Phantoms.var("meta"),
                                    ),
                                ),
                            ],
                            Eithers.either(
                                Phantoms.constant(
                                    _ap(
                                        _local("setMetaUsesLeft"),
                                        Phantoms.var("metaWithCast"),
                                        Phantoms.true(),
                                    )
                                ),
                                Phantoms.constant(
                                    _ap(
                                        _local("setMetaUsesRight"),
                                        Phantoms.var("metaWithCast"),
                                        Phantoms.true(),
                                    )
                                ),
                                Phantoms.var("e"),
                            ),
                        ),
                    ),
                ),
                Phantoms.field(
                    Name("lambda"),
                    Phantoms.lam(
                        "lam",
                        Maybes.maybe(
                            Phantoms.var("meta"),
                            Phantoms.lam(
                                "dom",
                                Logic.if_else(
                                    Phantoms.var("topLevel"),
                                    _ap(
                                        _local("extendMetaForType"),
                                        Phantoms.true(),
                                        Phantoms.false(),
                                        Phantoms.var("dom"),
                                        Phantoms.var("meta"),
                                    ),
                                    Phantoms.var("meta"),
                                ),
                            ),
                            Core.lambda_domain(Phantoms.var("lam")),
                        ),
                    ),
                ),
                Phantoms.field(
                    Name("let"),
                    Phantoms.lam(
                        "lt",
                        _let_chain(
                            [
                                (
                                    "bindings",
                                    Core.let_bindings(Phantoms.var("lt")),
                                ),
                            ],
                            Lists.foldl(
                                _let_chain(
                                    [
                                        (
                                            "forBinding",
                                            Phantoms.lambdas(
                                                ["m", "b"],
                                                Maybes.maybe(
                                                    Phantoms.var("m"),
                                                    Phantoms.lam(
                                                        "ts",
                                                        _let_chain(
                                                            [
                                                                (
                                                                    "term1",
                                                                    Core.binding_term(
                                                                        Phantoms.var("b")
                                                                    ),
                                                                ),
                                                            ],
                                                            Logic.if_else(
                                                                _ap(
                                                                    _kref.analysis_is_simple_assignment,
                                                                    Phantoms.var("term1"),
                                                                ),
                                                                Phantoms.var("m"),
                                                                _ap(
                                                                    _local("extendMetaForType"),
                                                                    Phantoms.true(),
                                                                    Phantoms.true(),
                                                                    Core.type_scheme_body(
                                                                        Phantoms.var("ts")
                                                                    ),
                                                                    Phantoms.var("m"),
                                                                ),
                                                            ),
                                                        ),
                                                    ),
                                                    Core.binding_type_scheme(
                                                        Phantoms.var("b")
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ],
                                    Phantoms.var("forBinding"),
                                ),
                                Phantoms.var("meta"),
                                Phantoms.var("bindings"),
                            ),
                        ),
                    ),
                ),
                Phantoms.field(
                    Name("literal"),
                    Phantoms.lam(
                        "l",
                        Phantoms.cases(
                            Name("hydra.core.Literal"),
                            Phantoms.var("l"),
                            Just(Phantoms.var("meta")),
                            [
                                Phantoms.field(
                                    Name("decimal"),
                                    Phantoms.constant(
                                        _ap(
                                            _local("setMetaUsesDecimal"),
                                            Phantoms.var("meta"),
                                            Phantoms.true(),
                                        )
                                    ),
                                ),
                                Phantoms.field(
                                    Name("float"),
                                    Phantoms.lam(
                                        "fv",
                                        Phantoms.cases(
                                            Name("hydra.core.FloatValue"),
                                            Phantoms.var("fv"),
                                            Just(Phantoms.var("meta")),
                                            [
                                                Phantoms.field(
                                                    Name("bigfloat"),
                                                    Phantoms.constant(
                                                        _ap(
                                                            _local("setMetaUsesDecimal"),
                                                            Phantoms.var("meta"),
                                                            Phantoms.true(),
                                                        )
                                                    ),
                                                ),
                                            ],
                                        ),
                                    ),
                                ),
                            ],
                        ),
                    ),
                ),
                Phantoms.field(
                    Name("map"),
                    Phantoms.constant(
                        _ap(
                            _local("setMetaUsesFrozenDict"),
                            Phantoms.var("meta"),
                            Phantoms.true(),
                        )
                    ),
                ),
                Phantoms.field(
                    Name("maybe"),
                    Phantoms.lam(
                        "m",
                        Maybes.maybe(
                            _ap(
                                _local("setMetaUsesNothing"),
                                Phantoms.var("meta"),
                                Phantoms.true(),
                            ),
                            Phantoms.constant(
                                _ap(
                                    _local("setMetaUsesJust"),
                                    Phantoms.var("meta"),
                                    Phantoms.true(),
                                )
                            ),
                            Phantoms.var("m"),
                        ),
                    ),
                ),
                Phantoms.field(
                    Name("inject"),
                    Phantoms.constant(
                        _ap(
                            _local("setMetaUsesCast"),
                            Phantoms.true(),
                            Phantoms.var("meta"),
                        )
                    ),
                ),
            ],
        ),
    )
    body = Phantoms.lambdas(
        ["topLevel", "meta0", "term"],
        _let_chain(
            [("step", step_inner)],
            _ap(
                _kref.rewriting_fold_over_term,
                Coders_dsl.traversal_order_pre,
                Phantoms.var("step"),
                Phantoms.var("meta0"),
                Phantoms.var("term"),
            ),
        ),
    )
    return _def(
        "extendMetaForTerm",
        Phantoms.doc(
            "Extend metadata based on a term (used during module encoding)", body
        ),
    )


def _extend_meta_for_type():
    case_fields = [
        Phantoms.field(
            Name("function"),
            Phantoms.lam(
                "ft",
                _let_chain(
                    [
                        ("cod", Core.function_type_codomain(Phantoms.var("ft"))),
                        ("dom", Core.function_type_domain(Phantoms.var("ft"))),
                        (
                            "meta2",
                            _ap(
                                _local("extendMetaForType"),
                                Phantoms.var("topLevel"),
                                Phantoms.var("isTermAnnot"),
                                Phantoms.var("cod"),
                                Phantoms.var("metaWithSubtypes"),
                            ),
                        ),
                        (
                            "meta3",
                            _ap(
                                _local("extendMetaForType"),
                                Phantoms.false(),
                                Phantoms.var("isTermAnnot"),
                                Phantoms.var("dom"),
                                Phantoms.var("meta2"),
                            ),
                        ),
                    ],
                    Logic.if_else(
                        Logic.and_(
                            Phantoms.var("isTermAnnot"), Phantoms.var("topLevel")
                        ),
                        Phantoms.var("meta3"),
                        _ap(
                            _local("setMetaUsesCallable"),
                            Phantoms.var("meta3"),
                            Phantoms.true(),
                        ),
                    ),
                ),
            ),
        ),
        Phantoms.field(
            Name("list"),
            Phantoms.constant(
                _ap(
                    _local("setMetaUsesFrozenList"),
                    Phantoms.var("metaWithSubtypes"),
                    Phantoms.true(),
                )
            ),
        ),
        Phantoms.field(
            Name("map"),
            Phantoms.constant(
                _ap(
                    _local("setMetaUsesFrozenDict"),
                    Phantoms.var("metaWithSubtypes"),
                    Phantoms.true(),
                )
            ),
        ),
        Phantoms.field(
            Name("maybe"),
            Phantoms.constant(
                _ap(
                    _local("setMetaUsesMaybe"),
                    Phantoms.var("metaWithSubtypes"),
                    Phantoms.true(),
                )
            ),
        ),
        Phantoms.field(
            Name("either"),
            Phantoms.constant(
                _ap(
                    _local("setMetaUsesEither"),
                    Phantoms.var("metaWithSubtypes"),
                    Phantoms.true(),
                )
            ),
        ),
        Phantoms.field(
            Name("literal"),
            Phantoms.lam(
                "lt",
                Phantoms.cases(
                    Name("hydra.core.LiteralType"),
                    Phantoms.var("lt"),
                    Just(Phantoms.var("metaWithSubtypes")),
                    [
                        Phantoms.field(
                            Name("decimal"),
                            Phantoms.constant(
                                _ap(
                                    _local("setMetaUsesDecimal"),
                                    Phantoms.var("metaWithSubtypes"),
                                    Phantoms.true(),
                                )
                            ),
                        ),
                        Phantoms.field(
                            Name("float"),
                            Phantoms.lam(
                                "ft",
                                Phantoms.cases(
                                    Name("hydra.core.FloatType"),
                                    Phantoms.var("ft"),
                                    Just(Phantoms.var("metaWithSubtypes")),
                                    [
                                        Phantoms.field(
                                            Name("bigfloat"),
                                            Phantoms.constant(
                                                _ap(
                                                    _local("setMetaUsesDecimal"),
                                                    Phantoms.var("metaWithSubtypes"),
                                                    Phantoms.true(),
                                                )
                                            ),
                                        ),
                                    ],
                                ),
                            ),
                        ),
                    ],
                ),
            ),
        ),
        Phantoms.field(
            Name("union"),
            Phantoms.lam(
                "rt",
                Logic.if_else(
                    _ap(_kref.predicates_is_enum_row_type, Phantoms.var("rt")),
                    _ap(
                        _local("setMetaUsesEnum"),
                        Phantoms.var("metaWithSubtypes"),
                        Phantoms.true(),
                    ),
                    Logic.if_else(
                        Logic.not_(Lists.null(Phantoms.var("rt"))),
                        _ap(
                            _local("setMetaUsesNode"),
                            Phantoms.var("metaWithSubtypes"),
                            Phantoms.true(),
                        ),
                        Phantoms.var("metaWithSubtypes"),
                    ),
                ),
            ),
        ),
        Phantoms.field(
            Name("forall"),
            Phantoms.lam(
                "ft",
                _let_chain(
                    [
                        ("body", Core.forall_type_body(Phantoms.var("ft"))),
                        (
                            "metaForWrap",
                            _ap(
                                _local("digForWrap"),
                                Phantoms.var("isTermAnnot"),
                                Phantoms.var("metaWithSubtypes"),
                                Phantoms.var("body"),
                            ),
                        ),
                    ],
                    Phantoms.cases(
                        Name("hydra.core.Type"),
                        _ap(
                            _kref.strip_deannotate_type, Phantoms.var("body")
                        ),
                        Just(Phantoms.var("metaForWrap")),
                        [
                            Phantoms.field(
                                Name("record"),
                                Phantoms.constant(
                                    _ap(
                                        _local("setMetaUsesGeneric"),
                                        Phantoms.var("metaForWrap"),
                                        Phantoms.true(),
                                    )
                                ),
                            ),
                        ],
                    ),
                ),
            ),
        ),
        Phantoms.field(
            Name("record"),
            Phantoms.lam(
                "rt",
                _let_chain(
                    [
                        (
                            "hasAnnotated",
                            Lists.foldl(
                                Phantoms.lambdas(
                                    ["b", "ft"],
                                    Logic.or_(
                                        Phantoms.var("b"),
                                        _ap(
                                            _kref.annotations_has_type_description,
                                            Core.field_type_type(
                                                Phantoms.var("ft")
                                            ),
                                        ),
                                    ),
                                ),
                                Phantoms.false(),
                                Phantoms.var("rt"),
                            ),
                        ),
                        (
                            "meta1",
                            Logic.if_else(
                                Lists.null(Phantoms.var("rt")),
                                Phantoms.var("metaWithSubtypes"),
                                _ap(
                                    _local("setMetaUsesDataclass"),
                                    Phantoms.var("metaWithSubtypes"),
                                    Phantoms.true(),
                                ),
                            ),
                        ),
                    ],
                    Logic.if_else(
                        Phantoms.var("hasAnnotated"),
                        _ap(
                            _local("setMetaUsesAnnotated"),
                            Phantoms.var("meta1"),
                            Phantoms.true(),
                        ),
                        Phantoms.var("meta1"),
                    ),
                ),
            ),
        ),
        Phantoms.field(
            Name("wrap"),
            Phantoms.constant(
                Logic.if_else(
                    Phantoms.var("isTermAnnot"),
                    Phantoms.var("metaWithSubtypes"),
                    _ap(
                        _local("setMetaUsesNode"),
                        Phantoms.var("metaWithSubtypes"),
                        Phantoms.true(),
                    ),
                )
            ),
        ),
    ]
    body = Phantoms.lambdas(
        ["topLevel", "isTermAnnot", "typ", "meta"],
        _let_chain(
            [
                ("currentTvars", _meta_proj("typeVariables", "meta")),
                (
                    "newTvars",
                    _ap(
                        _local("collectTypeVariables"),
                        Phantoms.var("currentTvars"),
                        Phantoms.var("typ"),
                    ),
                ),
                (
                    "metaWithTvars",
                    _ap(
                        _local("setMetaTypeVariables"),
                        Phantoms.var("meta"),
                        Phantoms.var("newTvars"),
                    ),
                ),
                (
                    "metaWithSubtypes",
                    Lists.foldl(
                        Phantoms.lambdas(
                            ["m", "t"],
                            _ap(
                                _local("extendMetaForType"),
                                Phantoms.false(),
                                Phantoms.var("isTermAnnot"),
                                Phantoms.var("t"),
                                Phantoms.var("m"),
                            ),
                        ),
                        Phantoms.var("metaWithTvars"),
                        _ap(_kref.rewriting_subtypes, Phantoms.var("typ")),
                    ),
                ),
            ],
            Phantoms.cases(
                Name("hydra.core.Type"),
                _ap(_kref.strip_deannotate_type, Phantoms.var("typ")),
                Just(Phantoms.var("metaWithSubtypes")),
                case_fields,
            ),
        ),
    )
    return _def(
        "extendMetaForType",
        Phantoms.doc(
            "Extend metadata based on a type (used during module encoding)", body
        ),
    )


def _extend_meta_for_types():
    body = Phantoms.lambdas(
        ["types", "meta"],
        _let_chain(
            [
                (
                    "names",
                    Sets.unions(
                        Lists.map(
                            Phantoms.lam(
                                "t",
                                _ap(
                                    _kref.dependencies_type_dependency_names,
                                    Phantoms.false(),
                                    Phantoms.var("t"),
                                ),
                            ),
                            Phantoms.var("types"),
                        )
                    ),
                ),
                ("currentNs", _meta_proj("namespaces", "meta")),
                (
                    "updatedNs",
                    _ap(
                        _kref.analysis_add_names_to_namespaces,
                        _kref.names_encode_namespace,
                        Phantoms.var("names"),
                        Phantoms.var("currentNs"),
                    ),
                ),
                (
                    "meta1",
                    _ap(
                        _local("setMetaNamespaces"),
                        Phantoms.var("updatedNs"),
                        Phantoms.var("meta"),
                    ),
                ),
            ],
            Lists.foldl(
                Phantoms.lambdas(
                    ["m", "t"],
                    _ap(
                        _local("extendMetaForType"),
                        Phantoms.true(),
                        Phantoms.false(),
                        Phantoms.var("t"),
                        Phantoms.var("m"),
                    ),
                ),
                Phantoms.var("meta1"),
                Phantoms.var("types"),
            ),
        ),
    )
    return _def(
        "extendMetaForTypes",
        Phantoms.doc("Extend metadata for a list of types", body),
    )


def _extract_case_elimination():
    body = Phantoms.lambdas(
        ["term"],
        Phantoms.cases(
            Name("hydra.core.Term"),
            _ap(_kref.strip_deannotate_and_detype_term, Phantoms.var("term")),
            Just(Phantoms.nothing()),
            [
                Phantoms.field(
                    Name("cases"),
                    Phantoms.lam("cs", Phantoms.just(Phantoms.var("cs"))),
                ),
            ],
        ),
    )
    return _def(
        "extractCaseElimination",
        Phantoms.doc("Extract CaseStatement from a case elimination term", body),
    )


def _find_type_params():
    body = Phantoms.lambdas(
        ["env", "typ"],
        _let_chain(
            [
                ("boundVars", Pairs.second(_env("boundTypeVariables", "env"))),
                (
                    "isBound",
                    Phantoms.lam(
                        "v",
                        Maybes.is_just(Maps.lookup(Phantoms.var("v"), Phantoms.var("boundVars"))),
                    ),
                ),
            ],
            Lists.filter(
                Phantoms.var("isBound"),
                Sets.to_list(
                    _ap(_kref.variables_free_variables_in_type, Phantoms.var("typ"))
                ),
            ),
        ),
    )
    return _def(
        "findTypeParams",
        Phantoms.doc(
            "Find type parameters in a type that are bound in the environment",
            body,
        ),
    )


def _function_definition_to_expr():
    py_args_action = Eithers.map_list(
        Phantoms.lam(
            "pair",
            _let_chain(
                [
                    ("argName", Pairs.first(Phantoms.var("pair"))),
                    ("typ", Pairs.second(Phantoms.var("pair"))),
                ],
                Eithers.bind(
                    _ap(_local("encodeType"), Phantoms.var("env"), Phantoms.var("typ")),
                    Phantoms.lam(
                        "pyTyp",
                        Phantoms.right(
                            PyDsl.param_no_default_simple(
                                PySyn.param(
                                    _ap(
                                        _kref.names_encode_name,
                                        Phantoms.false(),
                                        _kref.util_case_convention_lower_snake,
                                        Phantoms.var("env"),
                                        Phantoms.var("argName"),
                                    ),
                                    Phantoms.just(
                                        PySyn.annotation(Phantoms.var("pyTyp"))
                                    ),
                                )
                            )
                        ),
                    ),
                ),
            ),
        ),
        Lists.zip(Phantoms.var("args"), Phantoms.var("doms")),
    )

    block_tco = Eithers.bind(
        _ap(
            _local("encodeTermMultilineTCO"),
            Phantoms.var("cx"),
            Phantoms.var("env"),
            Phantoms.var("name"),
            Phantoms.var("args"),
            Phantoms.var("body"),
        ),
        Phantoms.lam(
            "tcoStmts",
            _let_chain(
                [
                    (
                        "trueExpr",
                        PySyn.named_expression_simple(
                            _ap(
                                _kref.utils_py_atom_to_py_expression,
                                PySyn.atom_true,
                            )
                        ),
                    ),
                    (
                        "whileBody",
                        _ap(
                            _kref.utils_indented_block,
                            Phantoms.nothing(),
                            Phantoms.list_(
                                [
                                    Lists.concat2(
                                        Phantoms.var("prefixes"),
                                        Phantoms.var("tcoStmts"),
                                    )
                                ]
                            ),
                        ),
                    ),
                    (
                        "whileStmt",
                        PySyn.statement_compound(
                            PySyn.compound_statement_while(
                                PySyn.while_statement(
                                    Phantoms.var("trueExpr"),
                                    Phantoms.var("whileBody"),
                                    Phantoms.nothing(),
                                )
                            )
                        ),
                    ),
                ],
                Phantoms.right(
                    _ap(
                        _kref.utils_indented_block,
                        Phantoms.var("comment"),
                        Phantoms.list_(
                            [Phantoms.list_([Phantoms.var("whileStmt")])]
                        ),
                    )
                ),
            ),
        ),
    )
    block_normal = Eithers.bind(
        _ap(
            _local("encodeTermMultiline"),
            Phantoms.var("cx"),
            Phantoms.var("env"),
            Phantoms.var("body"),
        ),
        Phantoms.lam(
            "stmts",
            Phantoms.right(
                _ap(
                    _kref.utils_indented_block,
                    Phantoms.var("comment"),
                    Phantoms.list_(
                        [
                            Lists.concat2(
                                Phantoms.var("prefixes"), Phantoms.var("stmts")
                            )
                        ]
                    ),
                )
            ),
        ),
    )
    body = Phantoms.lambdas(
        ["cx", "env", "name", "tparams", "args", "body", "doms", "mcod", "comment", "prefixes"],
        Eithers.bind(
            py_args_action,
            Phantoms.lam(
                "pyArgs",
                _let_chain(
                    [
                        (
                            "pyParams",
                            PyDsl.parameters_param_no_default(
                                PyDsl.param_no_default_parameters_simple(
                                    Phantoms.var("pyArgs")
                                )
                            )
                            if False
                            else PySyn.parameters_param_no_default(
                                PySyn.param_no_default_parameters(
                                    Phantoms.var("pyArgs"),
                                    Phantoms.list_([]),
                                    Phantoms.nothing(),
                                )
                            ),
                        ),
                        (
                            "isTCO",
                            Logic.and_(
                                Logic.not_(Lists.null(Phantoms.var("args"))),
                                _ap(
                                    _kref.analysis_is_self_tail_recursive,
                                    Phantoms.var("name"),
                                    Phantoms.var("body"),
                                ),
                            ),
                        ),
                    ],
                    Eithers.bind(
                        Logic.if_else(Phantoms.var("isTCO"), block_tco, block_normal),
                        Phantoms.lam(
                            "block",
                            Eithers.bind(
                                Maybes.maybe(
                                    Phantoms.right(Phantoms.nothing()),
                                    Phantoms.lam(
                                        "cod",
                                        Eithers.bind(
                                            _ap(
                                                _local("encodeType"),
                                                Phantoms.var("env"),
                                                Phantoms.var("cod"),
                                            ),
                                            Phantoms.lam(
                                                "pytyp",
                                                Phantoms.right(
                                                    Phantoms.just(
                                                        Phantoms.var("pytyp")
                                                    )
                                                ),
                                            ),
                                        ),
                                    ),
                                    Phantoms.var("mcod"),
                                ),
                                Phantoms.lam(
                                    "mreturnType",
                                    _let_chain(
                                        [
                                            (
                                                "pyTparams",
                                                Logic.if_else(
                                                    _local("useInlineTypeParams"),
                                                    Lists.map(
                                                        Phantoms.compose(
                                                            _kref.utils_py_name_to_py_type_parameter,
                                                            _kref.names_encode_type_variable,
                                                        ),
                                                        Phantoms.var("tparams"),
                                                    ),
                                                    Phantoms.list_([]),
                                                ),
                                            ),
                                            (
                                                "isThunk",
                                                Lists.null(Phantoms.var("args")),
                                            ),
                                            (
                                                "mDecorators",
                                                Logic.if_else(
                                                    Phantoms.var("isThunk"),
                                                    Phantoms.just(
                                                        Phantoms.wrap(
                                                            Name(
                                                                "hydra.python.syntax.Decorators"
                                                            ),
                                                            Phantoms.list_(
                                                                [
                                                                    _local(
                                                                        "lruCacheDecorator"
                                                                    )
                                                                ]
                                                            ),
                                                        )
                                                    ),
                                                    Phantoms.nothing(),
                                                ),
                                            ),
                                            (
                                                "pyName",
                                                _ap(
                                                    _kref.names_encode_name,
                                                    Phantoms.false(),
                                                    _kref.util_case_convention_lower_snake,
                                                    Phantoms.var("env"),
                                                    Phantoms.var("name"),
                                                ),
                                            ),
                                        ],
                                        Phantoms.right(
                                            PySyn.statement_compound(
                                                PySyn.compound_statement_function(
                                                    PySyn.function_definition(
                                                        Phantoms.var("mDecorators"),
                                                        PySyn.function_def_raw(
                                                            Phantoms.false(),
                                                            Phantoms.var("pyName"),
                                                            Phantoms.var("pyTparams"),
                                                            Phantoms.just(
                                                                Phantoms.var("pyParams")
                                                            ),
                                                            Phantoms.var("mreturnType"),
                                                            Phantoms.nothing(),
                                                            Phantoms.var("block"),
                                                        ),
                                                    )
                                                )
                                            )
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "functionDefinitionToExpr",
        Phantoms.doc("Encode a function definition with parameters and body", body),
    )


def _gather_lambdas():
    inner_go = Phantoms.lambdas(
        ["params", "t"],
        Phantoms.cases(
            Name("hydra.core.Term"),
            _ap(_kref.strip_deannotate_and_detype_term, Phantoms.var("t")),
            Just(Phantoms.pair(Phantoms.var("params"), Phantoms.var("t"))),
            [
                Phantoms.field(
                    Name("lambda"),
                    Phantoms.lam(
                        "l",
                        _ap(
                            Phantoms.var("go"),
                            Lists.concat2(
                                Phantoms.var("params"),
                                Phantoms.list_([Core.lambda_parameter(Phantoms.var("l"))]),
                            ),
                            Core.lambda_body(Phantoms.var("l")),
                        ),
                    ),
                ),
            ],
        ),
    )
    body = Phantoms.lambdas(
        ["term"],
        _let_chain(
            [("go", inner_go)],
            _ap(Phantoms.var("go"), Phantoms.list_([]), Phantoms.var("term")),
        ),
    )
    return _def(
        "gatherLambdas",
        Phantoms.doc("Extract lambdas and their bodies from a term", body),
    )


def _gather_metadata():
    add_def = Phantoms.lambdas(
        ["meta", "def"],
        Phantoms.cases(
            Name("hydra.packaging.Definition"),
            Phantoms.var("def"),
            Nothing(),
            [
                Phantoms.field(
                    Name("term"),
                    Phantoms.lam(
                        "termDef",
                        _let_chain(
                            [
                                (
                                    "term",
                                    Pkg.term_definition_term(Phantoms.var("termDef")),
                                ),
                                (
                                    "typ",
                                    Maybes.maybe(
                                        Core.type_variable(
                                            Phantoms.wrap(
                                                Name("hydra.core.Name"),
                                                Phantoms.string("hydra.core.Unit"),
                                            )
                                        ),
                                        Phantoms.unary_function(
                                            Core.type_scheme_body
                                        ),
                                        Pkg.term_definition_type_scheme(
                                            Phantoms.var("termDef")
                                        ),
                                    ),
                                ),
                                (
                                    "meta2",
                                    _ap(
                                        _local("extendMetaForType"),
                                        Phantoms.true(),
                                        Phantoms.true(),
                                        Phantoms.var("typ"),
                                        Phantoms.var("meta"),
                                    ),
                                ),
                            ],
                            _ap(
                                _local("extendMetaForTerm"),
                                Phantoms.true(),
                                Phantoms.var("meta2"),
                                Phantoms.var("term"),
                            ),
                        ),
                    ),
                ),
                Phantoms.field(
                    Name("type"),
                    Phantoms.lam(
                        "typeDef",
                        _let_chain(
                            [
                                (
                                    "typ",
                                    Core.type_scheme_body(
                                        Pkg.type_definition_type_scheme(
                                            Phantoms.var("typeDef")
                                        )
                                    ),
                                ),
                                (
                                    "meta2",
                                    _ap(
                                        _local("setMetaUsesName"),
                                        Phantoms.var("meta"),
                                        Phantoms.true(),
                                    ),
                                ),
                            ],
                            _ap(
                                _kref.rewriting_fold_over_type,
                                Coders_dsl.traversal_order_pre,
                                Phantoms.lambdas(
                                    ["m", "t"],
                                    _ap(
                                        _local("extendMetaForType"),
                                        Phantoms.true(),
                                        Phantoms.false(),
                                        Phantoms.var("t"),
                                        Phantoms.var("m"),
                                    ),
                                ),
                                Phantoms.var("meta2"),
                                Phantoms.var("typ"),
                            ),
                        ),
                    ),
                ),
            ],
        ),
    )
    body = Phantoms.lambdas(
        ["focusNs", "defs"],
        _let_chain(
            [
                (
                    "start",
                    _ap(
                        _local("emptyMetadata"),
                        _ap(
                            _kref.utils_find_namespaces,
                            Phantoms.var("focusNs"),
                            Phantoms.var("defs"),
                        ),
                    ),
                ),
                ("addDef", add_def),
                (
                    "result",
                    Lists.foldl(
                        Phantoms.var("addDef"),
                        Phantoms.var("start"),
                        Phantoms.var("defs"),
                    ),
                ),
                ("tvars", _meta_proj("typeVariables", "result")),
                (
                    "result2",
                    _ap(
                        _local("setMetaUsesCast"),
                        Phantoms.true(),
                        _ap(
                            _local("setMetaUsesLruCache"),
                            Phantoms.true(),
                            Phantoms.var("result"),
                        ),
                    ),
                ),
            ],
            _ap(
                _local("setMetaUsesTypeVar"),
                Phantoms.var("result2"),
                Logic.not_(Sets.null(Phantoms.var("tvars"))),
            ),
        ),
    )
    return _def(
        "gatherMetadata",
        Phantoms.doc("Gather metadata from definitions", body),
    )


def _generic_arg():
    body = Phantoms.lambdas(
        ["tparamList"],
        Logic.if_else(
            Lists.null(Phantoms.var("tparamList")),
            Phantoms.nothing(),
            Phantoms.just(
                _ap(
                    _kref.utils_py_primary_to_py_expression,
                    _ap(
                        _kref.utils_primary_with_expression_slices,
                        PySyn.primary_simple(PySyn.atom_name(_py_name("Generic"))),
                        Lists.map(
                            Phantoms.lam(
                                "n",
                                PyDsl.py_name_to_py_expression(
                                    _ap(_kref.names_encode_type_variable, Phantoms.var("n"))
                                ),
                            ),
                            Phantoms.var("tparamList"),
                        ),
                    ),
                )
            ),
        ),
    )
    return _def(
        "genericArg",
        Phantoms.doc("Create Generic[...] argument expression for class definition", body),
    )


def _initial_environment():
    body = Phantoms.lambdas(
        ["namespaces", "tcontext"],
        Phantoms.record(
            Name("hydra.python.environment.PythonEnvironment"),
            [
                Phantoms.field(Name("namespaces"), Phantoms.var("namespaces")),
                Phantoms.field(
                    Name("boundTypeVariables"),
                    Phantoms.pair(Phantoms.list_([]), Maps.empty()),
                ),
                Phantoms.field(Name("graph"), Phantoms.var("tcontext")),
                Phantoms.field(Name("nullaryBindings"), Sets.empty()),
                Phantoms.field(Name("version"), _local("targetPythonVersion")),
                Phantoms.field(Name("skipCasts"), Phantoms.true()),
                Phantoms.field(Name("inlineVariables"), Sets.empty()),
            ],
        ),
    )
    return _def(
        "initialEnvironment",
        Phantoms.doc("Create an initial Python environment for code generation", body),
    )


def _initial_metadata():
    body = Phantoms.lambdas(
        ["ns"],
        _let_chain(
            [
                ("dottedNs", _ap(_kref.names_encode_namespace, Phantoms.var("ns"))),
                (
                    "emptyNs",
                    Pkg.namespaces(
                        Phantoms.pair(Phantoms.var("ns"), Phantoms.var("dottedNs")),
                        Maps.empty(),
                    ),
                ),
            ],
            _empty_meta_record(Phantoms.var("emptyNs")),
        ),
    )
    return _def(
        "initialMetadata",
        Phantoms.doc("Create initial empty metadata for a Python module", body),
    )


def _is_case_statement_application():
    body = Phantoms.lambdas(
        ["term"],
        _let_chain(
            [
                ("gathered", _ap(_kref.analysis_gather_applications, Phantoms.var("term"))),
                ("args", Pairs.first(Phantoms.var("gathered"))),
                ("body", Pairs.second(Phantoms.var("gathered"))),
            ],
            Logic.if_else(
                Logic.not_(
                    Equality.equal(Lists.length(Phantoms.var("args")), Phantoms.int32(1))
                ),
                Phantoms.nothing(),
                _let_chain(
                    [
                        (
                            "arg",
                            Maybes.from_maybe(
                                Core.term_unit,
                                Lists.maybe_head(Phantoms.var("args")),
                            ),
                        ),
                    ],
                    Phantoms.cases(
                        Name("hydra.core.Term"),
                        _ap(_kref.strip_deannotate_and_detype_term, Phantoms.var("body")),
                        Just(Phantoms.nothing()),
                        [
                            Phantoms.field(
                                Name("cases"),
                                Phantoms.lam(
                                    "cs",
                                    Phantoms.just(
                                        Phantoms.tuple4(
                                            Core.case_statement_type_name(Phantoms.var("cs")),
                                            Core.case_statement_default(Phantoms.var("cs")),
                                            Core.case_statement_cases(Phantoms.var("cs")),
                                            Phantoms.var("arg"),
                                        )
                                    ),
                                ),
                            ),
                        ],
                    ),
                ),
            ),
        ),
    )
    return _def(
        "isCaseStatementApplication",
        Phantoms.doc(
            "Check if a term is a case statement applied to exactly one argument", body
        ),
    )


def _is_cases_full():
    body = Phantoms.lambdas(
        ["rowType", "cases_"],
        _let_chain(
            [
                ("numCases", Lists.length(Phantoms.var("cases_"))),
                ("numFields", Lists.length(Phantoms.var("rowType"))),
            ],
            Logic.not_(Equality.lt(Phantoms.var("numCases"), Phantoms.var("numFields"))),
        ),
    )
    return _def(
        "isCasesFull",
        Phantoms.doc("Check if union cases are fully covered", body),
    )


def _is_type_module_check():
    body = Phantoms.lambdas(
        ["defs"],
        Logic.not_(
            Lists.null(
                Lists.filter(
                    Phantoms.lam(
                        "d",
                        Phantoms.cases(
                            Name("hydra.packaging.Definition"),
                            Phantoms.var("d"),
                            Just(Phantoms.false()),
                            [
                                Phantoms.field(
                                    Name("type"),
                                    Phantoms.constant(Phantoms.true()),
                                ),
                            ],
                        ),
                    ),
                    Phantoms.var("defs"),
                )
            )
        ),
    )
    return _def(
        "isTypeModuleCheck",
        Phantoms.doc(
            "Check whether a list of definitions contains any type definitions", body
        ),
    )


def _is_type_variable_name():
    body = Phantoms.lambdas(
        ["name"],
        Equality.equal(
            Phantoms.int32(1),
            Lists.length(
                Strings.split_on(Phantoms.string("."), Core.un_name(Phantoms.var("name")))
            ),
        ),
    )
    return _def(
        "isTypeVariableName",
        Phantoms.doc(
            "Check if a name is a type variable (unqualified - no dots)", body
        ),
    )


def _is_variant_unit_type():
    body = Phantoms.lambdas(
        ["rowType", "fieldName"],
        _let_chain(
            [
                (
                    "mfield",
                    Lists.find(
                        Phantoms.lam(
                            "ft",
                            Equality.equal(
                                Core.field_type_name(Phantoms.var("ft")),
                                Phantoms.var("fieldName"),
                            ),
                        ),
                        Phantoms.var("rowType"),
                    ),
                ),
            ],
            Maybes.from_maybe(
                Phantoms.false(),
                Maybes.map(
                    Phantoms.lam(
                        "ft",
                        _ap(
                            _kref.predicates_is_unit_type,
                            _ap(
                                _kref.strip_deannotate_type,
                                Core.field_type_type(Phantoms.var("ft")),
                            ),
                        ),
                    ),
                    Phantoms.var("mfield"),
                ),
            ),
        ),
    )
    return _def(
        "isVariantUnitType",
        Phantoms.doc("Check if a variant field has unit type", body),
    )


def _dataclass_decorator():
    inner_atom = PySyn.atom_name(
        Phantoms.wrap(Name("hydra.python.syntax.Name"), Phantoms.string("dataclass"))
    )
    primary = PySyn.primary_simple(inner_atom)
    kwarg = PySyn.kwarg_or_starred_kwarg(
        PySyn.kwarg(
            Phantoms.wrap(Name("hydra.python.syntax.Name"), Phantoms.string("frozen")),
            _ap(_kref.utils_py_atom_to_py_expression, PySyn.atom_true),
        )
    )
    args_term = PySyn.args(
        Phantoms.list_([]),
        Phantoms.list_([kwarg]),
        Phantoms.list_([]),
    )
    rhs = PySyn.primary_rhs_call(args_term)
    body = PySyn.named_expression_simple(
        _ap(
            _kref.utils_py_primary_to_py_expression,
            _ap(_kref.utils_primary_with_rhs, primary, rhs),
        )
    )
    return _def(
        "dataclassDecorator",
        Phantoms.doc("Create a @dataclass(frozen=True) decorator", body),
    )


def _make_curried_lambda():
    body = Phantoms.lambdas(
        ["params", "body"],
        Lists.foldl(
            Phantoms.lambdas(
                ["acc", "p"],
                PySyn.expression_lambda(
                    PySyn.lambda_(
                        PyDsl.lambda_parameters_simple(
                            Phantoms.list_(
                                [PySyn.lambda_param_no_default(Phantoms.var("p"))]
                            )
                        ),
                        Phantoms.var("acc"),
                    )
                ),
            ),
            Phantoms.var("body"),
            Lists.reverse(Phantoms.var("params")),
        ),
    )
    return _def(
        "makeCurriedLambda",
        Phantoms.doc(
            "Create a curried lambda chain from a list of parameter names and a body",
            body,
        ),
    )


def _make_py_graph():
    body = Phantoms.lambdas(
        ["g", "m"],
        Phantoms.record(
            Name("hydra.python.environment.PyGraph"),
            [
                Phantoms.field(Name("graph"), Phantoms.var("g")),
                Phantoms.field(Name("metadata"), Phantoms.var("m")),
            ],
        ),
    )
    return _def(
        "makePyGraph",
        Phantoms.doc("Constructor for PyGraph record", body),
    )


def _make_simple_lambda():
    body = Phantoms.lambdas(
        ["arity", "lhs"],
        _let_chain(
            [
                (
                    "args",
                    Lists.map(
                        Phantoms.lam(
                            "i",
                            _py_name(
                                Strings.cat2(
                                    Phantoms.string("x"),
                                    Literals.show_int32(Phantoms.var("i")),
                                )
                            ),
                        ),
                        Math.range_(Phantoms.int32(1), Phantoms.var("arity")),
                    ),
                ),
            ],
            Logic.if_else(
                Equality.equal(Phantoms.var("arity"), Phantoms.int32(0)),
                Phantoms.var("lhs"),
                PySyn.expression_lambda(
                    PySyn.lambda_(
                        PyDsl.lambda_parameters_simple(
                            Lists.map(
                                Phantoms.lam(
                                    "a",
                                    PySyn.lambda_param_no_default(Phantoms.var("a")),
                                ),
                                Phantoms.var("args"),
                            )
                        ),
                        _ap(
                            _kref.utils_function_call,
                            _ap(_kref.utils_py_expression_to_py_primary, Phantoms.var("lhs")),
                            Lists.map(
                                Phantoms.lam(
                                    "a",
                                    PyDsl.py_name_to_py_expression(Phantoms.var("a")),
                                ),
                                Phantoms.var("args"),
                            ),
                        ),
                    )
                ),
            ),
        ),
    )
    return _def(
        "makeSimpleLambda",
        Phantoms.doc(
            "Wrap a bare reference to a polymorphic function in an uncurried lambda",
            body,
        ),
    )


def _make_thunk():
    body = Phantoms.lambdas(
        ["pbody"],
        _ap(
            _kref.utils_function_call,
            _ap(
                _kref.utils_py_expression_to_py_primary,
                _ap(
                    _kref.utils_function_call,
                    PySyn.primary_simple(PySyn.atom_name(_py_name("lru_cache"))),
                    Phantoms.list_([_ap(_local("pyInt"), Phantoms.bigint(1))]),
                ),
            ),
            Phantoms.list_([_ap(_local("wrapInNullaryLambda"), Phantoms.var("pbody"))]),
        ),
    )
    return _def(
        "makeThunk",
        Phantoms.doc(
            "Create a thunk (zero-argument lambda) wrapped with lru_cache(1) for memoization",
            body,
        ),
    )


def _make_lazy():
    body = Phantoms.lambdas(
        ["pbody"],
        _ap(
            _kref.utils_function_call,
            PySyn.primary_simple(PySyn.atom_name(_py_name("Lazy"))),
            Phantoms.list_([_ap(_local("wrapInNullaryLambda"), Phantoms.var("pbody"))]),
        ),
    )
    return _def(
        "makeLazy",
        Phantoms.doc(
            "Wrap an expression in Lazy(lambda: ...) for one-shot lazy memoization",
            body,
        ),
    )


def _make_uncurried_lambda():
    body = Phantoms.lambdas(
        ["params", "body"],
        PySyn.expression_lambda(
            PySyn.lambda_(
                PyDsl.lambda_parameters_simple(
                    Lists.map(
                        Phantoms.lam(
                            "p",
                            PySyn.lambda_param_no_default(Phantoms.var("p")),
                        ),
                        Phantoms.var("params"),
                    )
                ),
                Phantoms.var("body"),
            )
        ),
    )
    return _def(
        "makeUncurriedLambda",
        Phantoms.doc(
            "Create an uncurried lambda with multiple parameters", body
        ),
    )


def _module_imports():
    body = Phantoms.lambdas(
        ["namespaces", "meta"],
        Lists.map(
            Phantoms.lam(
                "imp",
                _ap(
                    _kref.utils_py_simple_statement_to_py_statement,
                    PySyn.simple_statement_import(Phantoms.var("imp")),
                ),
            ),
            Lists.concat(
                Phantoms.list_(
                    [
                        _ap(_local("moduleStandardImports"), Phantoms.var("meta")),
                        _ap(
                            _local("moduleDomainImports"), Phantoms.var("namespaces")
                        ),
                    ]
                )
            ),
        ),
    )
    return _def(
        "moduleImports",
        Phantoms.doc("Generate all import statements for a Python module", body),
    )


def _module_standard_imports():
    def cond(symbol, flag_field):
        return _ap(
            _local("condImportSymbol"),
            Phantoms.string(symbol),
            _meta_proj(flag_field, "meta"),
        )

    pairs = [
        Phantoms.pair(
            Phantoms.string("__future__"),
            Phantoms.list_(
                [
                    _ap(
                        _local("condImportSymbol"),
                        Phantoms.string("annotations"),
                        _kref.names_use_future_annotations,
                    )
                ]
            ),
        ),
        Phantoms.pair(
            Phantoms.string("collections.abc"),
            Phantoms.list_([cond("Callable", "usesCallable")]),
        ),
        Phantoms.pair(
            Phantoms.string("dataclasses"),
            Phantoms.list_([cond("dataclass", "usesDataclass")]),
        ),
        Phantoms.pair(
            Phantoms.string("decimal"),
            Phantoms.list_([cond("Decimal", "usesDecimal")]),
        ),
        Phantoms.pair(
            Phantoms.string("enum"),
            Phantoms.list_([cond("Enum", "usesEnum")]),
        ),
        Phantoms.pair(
            Phantoms.string("functools"),
            Phantoms.list_([cond("lru_cache", "usesLruCache")]),
        ),
        Phantoms.pair(
            Phantoms.string("hydra.dsl.python"),
            Phantoms.list_(
                [
                    cond("Either", "usesEither"),
                    cond("FrozenDict", "usesFrozenDict"),
                    cond("Just", "usesJust"),
                    cond("Left", "usesLeft"),
                    cond("Maybe", "usesMaybe"),
                    cond("Node", "usesNode"),
                    cond("Nothing", "usesNothing"),
                    cond("Right", "usesRight"),
                    cond("frozenlist", "usesFrozenList"),
                ]
            ),
        ),
        Phantoms.pair(
            Phantoms.string("typing"),
            Phantoms.list_(
                [
                    cond("Annotated", "usesAnnotated"),
                    cond("Generic", "usesGeneric"),
                    cond("TypeAlias", "usesTypeAlias"),
                    cond("TypeVar", "usesTypeVar"),
                    cond("cast", "usesCast"),
                ]
            ),
        ),
    ]
    body = Phantoms.lambdas(
        ["meta"],
        _let_chain(
            [
                ("pairs", Phantoms.list_(pairs)),
                (
                    "simplified",
                    Maybes.cat(
                        Lists.map(
                            Phantoms.lam(
                                "p",
                                _let_chain(
                                    [
                                        ("modName", Pairs.first(Phantoms.var("p"))),
                                        (
                                            "symbols",
                                            Maybes.cat(Pairs.second(Phantoms.var("p"))),
                                        ),
                                    ],
                                    Logic.if_else(
                                        Lists.null(Phantoms.var("symbols")),
                                        Phantoms.nothing(),
                                        Phantoms.just(
                                            Phantoms.pair(
                                                Phantoms.var("modName"),
                                                Phantoms.var("symbols"),
                                            )
                                        ),
                                    ),
                                ),
                            ),
                            Phantoms.var("pairs"),
                        )
                    ),
                ),
            ],
            Lists.map(
                Phantoms.lam(
                    "p",
                    _ap(
                        _local("standardImportStatement"),
                        Pairs.first(Phantoms.var("p")),
                        Pairs.second(Phantoms.var("p")),
                    ),
                ),
                Phantoms.var("simplified"),
            ),
        ),
    )
    return _def(
        "moduleStandardImports",
        Phantoms.doc(
            "Generate standard import statements based on module metadata", body
        ),
    )


def _module_domain_imports():
    body = Phantoms.lambdas(
        ["namespaces"],
        _let_chain(
            [
                (
                    "names",
                    Lists.sort(
                        Maps.elems(Pkg.namespaces_mapping(Phantoms.var("namespaces")))
                    ),
                ),
            ],
            Lists.map(
                Phantoms.lam(
                    "ns",
                    Phantoms.inject(
                        Name("hydra.python.syntax.ImportStatement"),
                        Name("name"),
                        Phantoms.wrap(
                            Name("hydra.python.syntax.ImportName"),
                            Phantoms.list_(
                                [
                                    Phantoms.record(
                                        Name("hydra.python.syntax.DottedAsName"),
                                        [
                                            Phantoms.field(Name("name"), Phantoms.var("ns")),
                                            Phantoms.field(Name("as"), Phantoms.nothing()),
                                        ],
                                    )
                                ]
                            ),
                        ),
                    ),
                ),
                Phantoms.var("names"),
            ),
        ),
    )
    return _def(
        "moduleDomainImports",
        Phantoms.doc("Generate domain import statements from namespace mappings", body),
    )


def _module_to_python():
    body = Phantoms.lambdas(
        ["mod", "defs", "cx", "g"],
        Eithers.bind(
            _ap(
                _local("encodePythonModule"),
                Phantoms.var("cx"),
                Phantoms.var("g"),
                Phantoms.var("mod"),
                Phantoms.var("defs"),
            ),
            Phantoms.lam(
                "file",
                _let_chain(
                    [
                        (
                            "s",
                            _ap(
                                Phantoms.var("hydra.serialization.printExpr"),
                                _ap(
                                    Phantoms.var("hydra.serialization.parenthesize"),
                                    _ap(
                                        Phantoms.var("hydra.python.serde.moduleToExpr"),
                                        Phantoms.var("file"),
                                    ),
                                ),
                            ),
                        ),
                        (
                            "path",
                            _ap(
                                Phantoms.var("hydra.names.namespaceToFilePath"),
                                _kref.util_case_convention_lower_snake,
                                Phantoms.wrap(
                                    Name("hydra.packaging.FileExtension"),
                                    Phantoms.string("py"),
                                ),
                                Pkg.module_namespace(Phantoms.var("mod")),
                            ),
                        ),
                    ],
                    Phantoms.right(
                        Maps.singleton(Phantoms.var("path"), Phantoms.var("s"))
                    ),
                ),
            ),
        ),
    )
    return _def(
        "moduleToPython",
        Phantoms.doc("Convert a Hydra module to Python source files", body),
    )


def _python_binding_metadata():
    meta_true = Core.term_literal(Core.literal_boolean(Phantoms.true()))
    body = Phantoms.lambdas(
        ["g", "b"],
        Logic.if_else(
            _ap(_local("shouldThunkBinding"), Phantoms.var("g"), Phantoms.var("b")),
            Logic.if_else(
                _ap(_kref.predicates_is_complex_binding, Phantoms.var("g"), Phantoms.var("b")),
                Phantoms.just(meta_true),
                Phantoms.nothing(),
            ),
            Phantoms.nothing(),
        ),
    )
    return _def(
        "pythonBindingMetadata",
        Phantoms.doc(
            "Like bindingMetadata, but only for bindings that will actually be thunked",
            body,
        ),
    )


def _lazy_dot_get():
    body = Phantoms.lambdas(
        ["expr"],
        _ap(
            _kref.utils_function_call,
            _ap(
                _kref.utils_py_expression_to_py_primary,
                _ap(
                    _kref.utils_project_from_expression,
                    Phantoms.var("expr"),
                    PySyn.name(Phantoms.string("get")),
                ),
            ),
            Phantoms.list_([]),
        ),
    )
    return _def(
        "lazyDotGet",
        Phantoms.doc(
            "Wrap an expression in a .get() method call (for Lazy unwrap at use sites)",
            body,
        ),
    )


def _lru_cache_decorator():
    inner_atom = PySyn.atom_name(
        Phantoms.wrap(Name("hydra.python.syntax.Name"), Phantoms.string("lru_cache"))
    )
    primary = PySyn.primary_simple(inner_atom)
    one_arg = Phantoms.list_([_ap(_local("pyInt"), Phantoms.bigint(1))])
    body = PySyn.named_expression_simple(
        _ap(_kref.utils_function_call, primary, one_arg)
    )
    return _def(
        "lruCacheDecorator",
        Phantoms.doc(
            "Decorator for @lru_cache(1) to memoize zero-argument function results",
            body,
        ),
    )


def _py_graph_graph():
    body = Phantoms.lambdas(["pyg"], _pygraph("graph", "pyg"))
    return _def(
        "pyGraphGraph",
        Phantoms.doc("Accessor for the graph field of PyGraph", body),
    )


def _py_graph_metadata():
    body = Phantoms.lambdas(["pyg"], _pygraph("metadata", "pyg"))
    return _def(
        "pyGraphMetadata",
        Phantoms.doc("Accessor for the metadata field of PyGraph", body),
    )


def _py_int():
    body = Phantoms.lambdas(
        ["n"],
        _ap(
            _kref.utils_py_atom_to_py_expression,
            PySyn.atom_number(PySyn.number_integer(Phantoms.var("n"))),
        ),
    )
    return _def(
        "pyInt",
        Phantoms.doc("Create integer literal expression", body),
    )


def _python_environment_get_graph():
    body = Phantoms.lambdas(["env"], _env("graph", "env"))
    return _def(
        "pythonEnvironmentGetGraph",
        Phantoms.doc("Get the Graph from a PythonEnvironment", body),
    )


def _python_environment_set_graph():
    body = Phantoms.lambdas(
        ["tc", "env"],
        Phantoms.record(
            Name("hydra.python.environment.PythonEnvironment"),
            [
                Phantoms.field(Name("namespaces"), _env("namespaces", "env")),
                Phantoms.field(Name("boundTypeVariables"), _env("boundTypeVariables", "env")),
                Phantoms.field(Name("graph"), Phantoms.var("tc")),
                Phantoms.field(Name("nullaryBindings"), _env("nullaryBindings", "env")),
                Phantoms.field(Name("version"), _env("version", "env")),
                Phantoms.field(Name("skipCasts"), _env("skipCasts", "env")),
                Phantoms.field(Name("inlineVariables"), _env("inlineVariables", "env")),
            ],
        ),
    )
    return _def(
        "pythonEnvironmentSetGraph",
        Phantoms.doc("Set the Graph in a PythonEnvironment", body),
    )


def _target_python_version():
    return _def(
        "targetPythonVersion",
        Phantoms.doc(
            "The target Python version for code generation",
            _kref.utils_target_python_version,
        ),
    )


def _use_inline_type_params():
    return _def(
        "useInlineTypeParams",
        Phantoms.doc(
            "Legacy constant for backward compatibility; use useInlineTypeParamsFor in new code",
            _ap(_local("useInlineTypeParamsFor"), _kref.utils_target_python_version),
        ),
    )


def _use_inline_type_params_for():
    body = Phantoms.lambdas(
        ["version"],
        Equality.equal(
            Phantoms.var("version"),
            Phantoms.inject_unit(
                Name("hydra.python.environment.PythonVersion"), Name("python312")
            ),
        ),
    )
    return _def(
        "useInlineTypeParamsFor",
        Phantoms.doc("Version-aware inline type parameters", body),
    )


def _set_meta_namespaces():
    body = Phantoms.lambdas(
        ["ns", "m"],
        _meta_record_with_field_set("namespaces", Phantoms.var("ns"), m_var="m"),
    )
    return _def("setMetaNamespaces", body)


def _set_meta_type_variables():
    body = Phantoms.lambdas(
        ["m", "tvars"],
        _meta_record_with_field_set("typeVariables", Phantoms.var("tvars"), m_var="m"),
    )
    return _def("setMetaTypeVariables", body)


# setMetaUses* family (22 defs that all share the same record structure;
# 2 of them — Cast and LruCache — take params in (b, m) order, the rest (m, b)).
def _set_meta_uses_annotated():
    return _set_meta_use_def("setMetaUsesAnnotated", "usesAnnotated", b_first=False)


def _set_meta_uses_callable():
    return _set_meta_use_def("setMetaUsesCallable", "usesCallable", b_first=False)


def _set_meta_uses_cast():
    return _set_meta_use_def("setMetaUsesCast", "usesCast", b_first=True)


def _set_meta_uses_dataclass():
    return _set_meta_use_def("setMetaUsesDataclass", "usesDataclass", b_first=False)


def _set_meta_uses_decimal():
    return _set_meta_use_def("setMetaUsesDecimal", "usesDecimal", b_first=False)


def _set_meta_uses_either():
    return _set_meta_use_def("setMetaUsesEither", "usesEither", b_first=False)


def _set_meta_uses_enum():
    return _set_meta_use_def("setMetaUsesEnum", "usesEnum", b_first=False)


def _set_meta_uses_frozen_dict():
    return _set_meta_use_def("setMetaUsesFrozenDict", "usesFrozenDict", b_first=False)


def _set_meta_uses_frozen_list():
    return _set_meta_use_def("setMetaUsesFrozenList", "usesFrozenList", b_first=False)


def _set_meta_uses_frozen_set():
    return _set_meta_use_def("setMetaUsesFrozenSet", "usesFrozenSet", b_first=False)


def _set_meta_uses_generic():
    return _set_meta_use_def("setMetaUsesGeneric", "usesGeneric", b_first=False)


def _set_meta_uses_just():
    return _set_meta_use_def("setMetaUsesJust", "usesJust", b_first=False)


def _set_meta_uses_left():
    return _set_meta_use_def("setMetaUsesLeft", "usesLeft", b_first=False)


def _set_meta_uses_lru_cache():
    return _set_meta_use_def("setMetaUsesLruCache", "usesLruCache", b_first=True)


def _set_meta_uses_maybe():
    return _set_meta_use_def("setMetaUsesMaybe", "usesMaybe", b_first=False)


def _set_meta_uses_name():
    return _set_meta_use_def("setMetaUsesName", "usesName", b_first=False)


def _set_meta_uses_node():
    return _set_meta_use_def("setMetaUsesNode", "usesNode", b_first=False)


def _set_meta_uses_nothing():
    return _set_meta_use_def("setMetaUsesNothing", "usesNothing", b_first=False)


def _set_meta_uses_right():
    return _set_meta_use_def("setMetaUsesRight", "usesRight", b_first=False)


def _set_meta_uses_type_alias():
    return _set_meta_use_def("setMetaUsesTypeAlias", "usesTypeAlias", b_first=False)


def _set_meta_uses_type_var():
    return _set_meta_use_def("setMetaUsesTypeVar", "usesTypeVar", b_first=False)


def _should_thunk_binding():
    body = Phantoms.lambdas(
        ["g", "b"],
        Logic.and_(
            _ap(_kref.predicates_is_complex_binding, Phantoms.var("g"), Phantoms.var("b")),
            Logic.not_(
                _ap(_kref.predicates_is_trivial_term, Core.binding_term(Phantoms.var("b")))
            ),
        ),
    )
    return _def(
        "shouldThunkBinding",
        Phantoms.doc(
            "Determine if a binding should be thunked based on its complexity and triviality",
            body,
        ),
    )


def _standard_import_statement():
    body = Phantoms.lambdas(
        ["modName", "symbols"],
        Phantoms.inject(
            Name("hydra.python.syntax.ImportStatement"),
            Name("from"),
            Phantoms.record(
                Name("hydra.python.syntax.ImportFrom"),
                [
                    Phantoms.field(Name("prefixes"), Phantoms.list_([])),
                    Phantoms.field(
                        Name("dottedName"),
                        Phantoms.just(
                            Phantoms.wrap(
                                Name("hydra.python.syntax.DottedName"),
                                Phantoms.list_([_py_name(Phantoms.var("modName"))]),
                            )
                        ),
                    ),
                    Phantoms.field(
                        Name("targets"),
                        Phantoms.inject(
                            Name("hydra.python.syntax.ImportFromTargets"),
                            Name("simple"),
                            Lists.map(
                                Phantoms.lam(
                                    "s",
                                    Phantoms.record(
                                        Name("hydra.python.syntax.ImportFromAsName"),
                                        [
                                            Phantoms.field(
                                                Name("name"), _py_name(Phantoms.var("s"))
                                            ),
                                            Phantoms.field(Name("as"), Phantoms.nothing()),
                                        ],
                                    ),
                                ),
                                Phantoms.var("symbols"),
                            ),
                        ),
                    ),
                ],
            ),
        ),
    )
    return _def(
        "standardImportStatement",
        Phantoms.doc("Generate a single from-import statement", body),
    )


def _term_arity_with_primitives():
    body = Phantoms.lambdas(
        ["graph", "term"],
        Phantoms.cases(
            Name("hydra.core.Term"),
            _ap(_kref.strip_deannotate_and_detype_term, Phantoms.var("term")),
            Just(Phantoms.int_(0)),
            [
                Phantoms.field(
                    Name("application"),
                    Phantoms.lam(
                        "app",
                        Math.max_(
                            Phantoms.int_(0),
                            Math.sub(
                                _ap(
                                    _local("termArityWithPrimitives"),
                                    Phantoms.var("graph"),
                                    Core.application_function(Phantoms.var("app")),
                                ),
                                Phantoms.int_(1),
                            ),
                        ),
                    ),
                ),
                Phantoms.field(
                    Name("lambda"),
                    Phantoms.lam(
                        "lam",
                        Math.add(
                            Phantoms.int_(1),
                            _ap(
                                _local("termArityWithPrimitives"),
                                Phantoms.var("graph"),
                                Core.lambda_body(Phantoms.var("lam")),
                            ),
                        ),
                    ),
                ),
                Phantoms.field(
                    Name("project"), Phantoms.constant(Phantoms.int_(1))
                ),
                Phantoms.field(
                    Name("unwrap"), Phantoms.constant(Phantoms.int_(1))
                ),
                Phantoms.field(
                    Name("cases"), Phantoms.constant(Phantoms.int_(1))
                ),
                Phantoms.field(
                    Name("variable"),
                    Phantoms.lam(
                        "name",
                        Maybes.maybe(
                            Phantoms.int_(0),
                            Phantoms.lam(
                                "el",
                                Maybes.maybe(
                                    _ap(
                                        _kref.arity_term_arity,
                                        Core.binding_term(Phantoms.var("el")),
                                    ),
                                    Phantoms.lam(
                                        "ts",
                                        _ap(
                                            _kref.arity_type_scheme_arity,
                                            Phantoms.var("ts"),
                                        ),
                                    ),
                                    Core.binding_type_scheme(Phantoms.var("el")),
                                ),
                            ),
                            _ap(
                                _kref.lexical_lookup_binding,
                                Phantoms.var("graph"),
                                Phantoms.var("name"),
                            ),
                        ),
                    ),
                ),
            ],
        ),
    )
    return _def(
        "termArityWithPrimitives",
        Phantoms.doc("Calculate term arity with proper primitive handling", body),
    )


def _tvar_statement():
    body = Phantoms.lambdas(
        ["name"],
        _ap(
            _kref.utils_assignment_statement,
            Phantoms.var("name"),
            _ap(
                _kref.utils_function_call,
                PySyn.primary_simple(PySyn.atom_name(_py_name("TypeVar"))),
                Phantoms.list_(
                    [
                        _ap(
                            _kref.utils_double_quoted_string,
                            _ap(
                                Phantoms.unwrap(Name("hydra.python.syntax.Name")),
                                Phantoms.var("name"),
                            ),
                        )
                    ]
                ),
            ),
        ),
    )
    return _def(
        "tvarStatement",
        Phantoms.doc(
            "Create a TypeVar assignment statement for a type variable name", body
        ),
    )


def _type_alias_statement_for():
    body = Phantoms.lambdas(
        ["env", "name", "tparams", "mcomment", "tyexpr"],
        Logic.if_else(
            _ap(_local("useInlineTypeParamsFor"), _env("version", "env")),
            _ap(
                _kref.utils_type_alias_statement,
                Phantoms.var("name"),
                Phantoms.var("tparams"),
                Phantoms.var("mcomment"),
                Phantoms.var("tyexpr"),
            ),
            _ap(
                _kref.utils_type_alias_statement310,
                Phantoms.var("name"),
                Phantoms.var("tparams"),
                Phantoms.var("mcomment"),
                Phantoms.var("tyexpr"),
            ),
        ),
    )
    return _def(
        "typeAliasStatementFor",
        Phantoms.doc("Version-aware type alias statement generation", body),
    )


def _union_type_statements_for():
    body = Phantoms.lambdas(
        ["env", "name", "tparams", "mcomment", "tyexpr", "extraStmts"],
        Logic.if_else(
            _ap(_local("useInlineTypeParamsFor"), _env("version", "env")),
            Lists.concat2(
                Phantoms.list_(
                    [
                        _ap(
                            _kref.utils_type_alias_statement,
                            Phantoms.var("name"),
                            Phantoms.var("tparams"),
                            Phantoms.var("mcomment"),
                            Phantoms.var("tyexpr"),
                        )
                    ]
                ),
                Phantoms.var("extraStmts"),
            ),
            _ap(
                _kref.utils_union_type_class_statements310,
                Phantoms.var("name"),
                Phantoms.var("mcomment"),
                Phantoms.var("tyexpr"),
                Phantoms.var("extraStmts"),
            ),
        ),
    )
    return _def(
        "unionTypeStatementsFor",
        Phantoms.doc("Version-aware union type statement generation", body),
    )


def _unsupported_expression():
    body = Phantoms.lambdas(
        ["msg"],
        _ap(
            _kref.utils_function_call,
            _ap(
                _kref.utils_py_expression_to_py_primary,
                _ap(
                    _kref.utils_project_from_expression,
                    _ap(
                        _kref.utils_project_from_expression,
                        _ap(
                            _kref.utils_project_from_expression,
                            PyDsl.py_name_to_py_expression(_py_name("hydra")),
                            _py_name("dsl"),
                        ),
                        _py_name("python"),
                    ),
                    _py_name("unsupported"),
                ),
            ),
            Phantoms.list_(
                [
                    _ap(
                        _kref.utils_string_to_py_expression,
                        PySyn.quote_style_double,
                        Phantoms.var("msg"),
                    )
                ]
            ),
        ),
    )
    return _def(
        "unsupportedExpression",
        Phantoms.doc(
            "Create an expression that calls hydra.dsl.python.unsupported(message) at runtime",
            body,
        ),
    )


def _variant_args():
    body = Phantoms.lambdas(
        ["ptype", "tparams"],
        _ap(
            _kref.utils_py_expressions_to_py_args,
            Maybes.cat(
                Phantoms.list_(
                    [
                        Phantoms.just(
                            _ap(
                                _kref.utils_py_primary_to_py_expression,
                                _ap(
                                    _kref.utils_primary_with_expression_slices,
                                    PySyn.primary_simple(PySyn.atom_name(_py_name("Node"))),
                                    Phantoms.list_([Phantoms.var("ptype")]),
                                ),
                            )
                        ),
                        _ap(_local("genericArg"), Phantoms.var("tparams")),
                    ]
                )
            ),
        ),
    )
    return _def(
        "variantArgs",
        Phantoms.doc("Create args for variant (Node[type], Generic[tparams])", body),
    )


def _variant_closed_pattern():
    body = Phantoms.lambdas(
        ["env", "typeName", "fieldName", "pyVariantName", "rowType", "isEnum",
         "varName", "shouldCapture"],
        Logic.if_else(
            Phantoms.var("isEnum"),
            _ap(
                _local("enumVariantPattern"),
                Phantoms.var("env"),
                Phantoms.var("typeName"),
                Phantoms.var("fieldName"),
            ),
            Logic.if_else(
                Logic.not_(Phantoms.var("shouldCapture")),
                _ap(_local("classVariantPatternUnit"), Phantoms.var("pyVariantName")),
                _ap(
                    _local("classVariantPatternWithCapture"),
                    Phantoms.var("env"),
                    Phantoms.var("pyVariantName"),
                    Phantoms.var("varName"),
                ),
            ),
        ),
    )
    return _def(
        "variantClosedPattern",
        Phantoms.doc(
            "Create a ClosedPattern for a variant based on its characteristics", body
        ),
    )


def _wildcard_case_block():
    body = Phantoms.lambdas(
        ["stmt"],
        PySyn.case_block(
            _ap(_kref.utils_py_closed_pattern_to_py_patterns, PySyn.closed_pattern_wildcard),
            Phantoms.nothing(),
            _ap(
                _kref.utils_indented_block,
                Phantoms.nothing(),
                Phantoms.list_([Phantoms.list_([Phantoms.var("stmt")])]),
            ),
        ),
    )
    return _def(
        "wildcardCaseBlock",
        Phantoms.doc("Create a wildcard case block with a given body statement", body),
    )


def _with_lambda():
    body = _ap(
        _kref.environment_with_lambda_context,
        _local("pythonEnvironmentGetGraph"),
        _local("pythonEnvironmentSetGraph"),
    )
    return _def(
        "withLambda",
        Phantoms.doc(
            "Execute a computation with lambda context (adds lambda parameter to Graph)",
            body,
        ),
    )


def _with_definitions():
    body = Phantoms.lambdas(
        ["env", "defs", "body"],
        _let_chain(
            [
                (
                    "bindings",
                    Maybes.cat(
                        Lists.map(
                            Phantoms.lam(
                                "def_",
                                Phantoms.cases(
                                    Name("hydra.packaging.Definition"),
                                    Phantoms.var("def_"),
                                    Just(Phantoms.nothing()),
                                    [
                                        Phantoms.field(
                                            Name("term"),
                                            Phantoms.lam(
                                                "td",
                                                Phantoms.just(
                                                    Core.binding(
                                                        _proj(
                                                            "hydra.packaging.TermDefinition",
                                                            "name",
                                                            "td",
                                                        ),
                                                        _proj(
                                                            "hydra.packaging.TermDefinition",
                                                            "term",
                                                            "td",
                                                        ),
                                                        _proj(
                                                            "hydra.packaging.TermDefinition",
                                                            "typeScheme",
                                                            "td",
                                                        ),
                                                    )
                                                ),
                                            ),
                                        ),
                                        Phantoms.field(
                                            Name("type"),
                                            Phantoms.constant(Phantoms.nothing()),
                                        ),
                                    ],
                                ),
                            ),
                            Phantoms.var("defs"),
                        )
                    ),
                ),
                (
                    "dummyLet",
                    getattr(Core, "let")(
                        Phantoms.var("bindings"),
                        Core.term_literal(Core.literal_string(Phantoms.string("dummy"))),
                    ),
                ),
            ],
            _ap(
                _local("withLet"),
                Phantoms.var("env"),
                Phantoms.var("dummyLet"),
                Phantoms.var("body"),
            ),
        ),
    )
    return _def(
        "withDefinitions",
        Phantoms.doc("Execute a computation with definitions in scope", body),
    )


def _with_let():
    body = _ap(
        _kref.environment_with_let_context,
        _local("pythonEnvironmentGetGraph"),
        _local("pythonEnvironmentSetGraph"),
        _local("pythonBindingMetadata"),
    )
    return _def(
        "withLet",
        Phantoms.doc(
            "Execute a computation with let context (adds let bindings to Graph)",
            body,
        ),
    )


def _with_let_inline():
    inner_lambda = Phantoms.lam(
        "innerEnv",
        _let_chain(
            [
                (
                    "updatedEnv",
                    Phantoms.record(
                        Name("hydra.python.environment.PythonEnvironment"),
                        [
                            Phantoms.field(
                                Name("namespaces"), _env("namespaces", "innerEnv")
                            ),
                            Phantoms.field(
                                Name("boundTypeVariables"),
                                _env("boundTypeVariables", "innerEnv"),
                            ),
                            Phantoms.field(Name("graph"), _env("graph", "innerEnv")),
                            Phantoms.field(
                                Name("nullaryBindings"),
                                _env("nullaryBindings", "innerEnv"),
                            ),
                            Phantoms.field(Name("version"), _env("version", "innerEnv")),
                            Phantoms.field(
                                Name("skipCasts"), _env("skipCasts", "innerEnv")
                            ),
                            Phantoms.field(
                                Name("inlineVariables"),
                                Sets.union(
                                    Phantoms.var("inlineVars"),
                                    _env("inlineVariables", "innerEnv"),
                                ),
                            ),
                        ],
                    ),
                )
            ],
            _ap(Phantoms.var("body"), Phantoms.var("updatedEnv")),
        ),
    )
    body = Phantoms.lambdas(
        ["env", "lt", "body"],
        _let_chain(
            [
                (
                    "bindingNames",
                    Lists.map(
                        Phantoms.lam(
                            "b", Core.binding_name(Phantoms.var("b"))
                        ),
                        Core.let_bindings(Phantoms.var("lt")),
                    ),
                ),
                ("inlineVars", Sets.from_list(Phantoms.var("bindingNames"))),
                (
                    "noMetadata",
                    Phantoms.lambdas(["tc", "b"], Phantoms.nothing()),
                ),
            ],
            _ap(
                _kref.environment_with_let_context,
                _local("pythonEnvironmentGetGraph"),
                _local("pythonEnvironmentSetGraph"),
                Phantoms.var("noMetadata"),
                Phantoms.var("env"),
                Phantoms.var("lt"),
                inner_lambda,
            ),
        ),
    )
    return _def(
        "withLetInline",
        Phantoms.doc(
            "Execute a computation with inline let context (for walrus operators)", body
        ),
    )


def _with_type_lambda():
    body = _ap(
        _kref.environment_with_type_lambda_context,
        _local("pythonEnvironmentGetGraph"),
        _local("pythonEnvironmentSetGraph"),
    )
    return _def(
        "withTypeLambda",
        Phantoms.doc("Execute a computation with type lambda context", body),
    )


def _wrap_lazy_arguments():
    arg_at = Phantoms.lam(
        "i",
        Maybes.from_maybe(
            Phantoms.var("dummyExpr"),
            Lists.maybe_at(Phantoms.var("i"), Phantoms.var("args")),
        ),
    )
    body = Phantoms.lambdas(
        ["name", "args"],
        Phantoms.lets(
            [
                Phantoms.field(
                    Name("dummyExpr"),
                    _ap(_kref.utils_py_name_to_py_expression, _py_name("")),
                ),
                Phantoms.field(Name("argAt"), arg_at),
            ],
            Logic.if_else(
                Logic.and_(
                    Equality.equal(
                        Phantoms.var("name"),
                        Core.name(Phantoms.string("hydra.lib.logic.ifElse")),
                    ),
                    Equality.equal(
                        Lists.length(Phantoms.var("args")), Phantoms.int32(3)
                    ),
                ),
                Phantoms.list_(
                    [
                        _ap(Phantoms.var("argAt"), Phantoms.int32(0)),
                        _ap(
                            _local("wrapInNullaryLambda"),
                            _ap(Phantoms.var("argAt"), Phantoms.int32(1)),
                        ),
                        _ap(
                            _local("wrapInNullaryLambda"),
                            _ap(Phantoms.var("argAt"), Phantoms.int32(2)),
                        ),
                    ]
                ),
                Logic.if_else(
                    Logic.and_(
                        Equality.equal(
                            Phantoms.var("name"),
                            Core.name(Phantoms.string("hydra.lib.maybes.cases")),
                        ),
                        Equality.equal(
                            Lists.length(Phantoms.var("args")), Phantoms.int32(3)
                        ),
                    ),
                    Phantoms.list_(
                        [
                            _ap(Phantoms.var("argAt"), Phantoms.int32(0)),
                            _ap(
                                _local("wrapInNullaryLambda"),
                                _ap(Phantoms.var("argAt"), Phantoms.int32(1)),
                            ),
                            _ap(Phantoms.var("argAt"), Phantoms.int32(2)),
                        ]
                    ),
                    Logic.if_else(
                        Logic.and_(
                            Logic.or_(
                                Equality.equal(
                                    Phantoms.var("name"),
                                    Core.name(Phantoms.string("hydra.lib.maybes.maybe")),
                                ),
                                Equality.equal(
                                    Phantoms.var("name"),
                                    Core.name(
                                        Phantoms.string("hydra.lib.maybes.fromMaybe")
                                    ),
                                ),
                            ),
                            Equality.gte(
                                Lists.length(Phantoms.var("args")), Phantoms.int32(1)
                            ),
                        ),
                        Lists.cons(
                            _ap(
                                _local("wrapInNullaryLambda"),
                                _ap(Phantoms.var("argAt"), Phantoms.int32(0)),
                            ),
                            Lists.drop(Phantoms.int32(1), Phantoms.var("args")),
                        ),
                        Phantoms.var("args"),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "wrapLazyArguments",
        Phantoms.doc(
            "Wrap specific arguments in nullary lambdas for primitives that require lazy evaluation",
            body,
        ),
    )


def _wrap_in_nullary_lambda():
    body = Phantoms.lambdas(
        ["expr"],
        PySyn.expression_lambda(
            PySyn.lambda_(PyDsl.lambda_parameters_empty, Phantoms.var("expr"))
        ),
    )
    return _def(
        "wrapInNullaryLambda",
        Phantoms.doc(
            "Wrap a Python expression in a nullary lambda (thunk) for lazy evaluation",
            body,
        ),
    )


# ----------------------------------------------------------------------
# Module assembly (incremental; defs added as ported)
# ----------------------------------------------------------------------

def _load_environment_reorder_defs():
    """Load hydra.environment.reorderDefs as a Definition by reading the kernel JSON.

    Mirrors Haskell's `toDefinition Environment.reorderDefs`, which re-exports the
    Environment.reorderDefs definition into hydra.python.coder's definitions list.
    """
    import os
    from hydra.generation import bootstrap_graph, bootstrap_schema_map, parse_json_file, decode_module
    base = os.path.join(
        os.path.dirname(__file__),
        "../../../../../../../..",
        "dist/json/hydra-kernel/src/main/json",
    )
    file_path = os.path.normpath(os.path.join(base, "hydra/environment.json"))
    json_val = parse_json_file(file_path)
    mod = decode_module(bootstrap_graph(), bootstrap_schema_map(), json_val)
    for d in mod.definitions:
        from hydra.packaging import DefinitionTerm
        if isinstance(d, DefinitionTerm) and d.value.name.value == "hydra.environment.reorderDefs":
            return d
    raise RuntimeError("hydra.environment.reorderDefs not found in kernel JSON")


def _build_module() -> Module:
    return Module(
        _PLACEHOLDER.description,
        _PLACEHOLDER.namespace,
        _PLACEHOLDER.dependencies,
        (
            _load_environment_reorder_defs(),
            Phantoms.to_definition(_analyze_python_function()),
            Phantoms.to_definition(_class_variant_pattern_unit()),
            Phantoms.to_definition(_class_variant_pattern_with_capture()),
            Phantoms.to_definition(_collect_type_variables()),
            Phantoms.to_definition(_cond_import_symbol()),
            Phantoms.to_definition(_dataclass_decorator()),
            Phantoms.to_definition(_deconflict_variant_name()),
            Phantoms.to_definition(_deduplicate_case_variables()),
            Phantoms.to_definition(_dig_for_wrap()),
            Phantoms.to_definition(_eliminate_unit_var()),
            Phantoms.to_definition(_empty_metadata()),
            Phantoms.to_definition(_encode_application()),
            Phantoms.to_definition(_encode_application_inner()),
            Phantoms.to_definition(_encode_application_type()),
            Phantoms.to_definition(_encode_binding_as()),
            Phantoms.to_definition(_encode_binding_as_assignment()),
            Phantoms.to_definition(_encode_bindings_as_defs()),
            Phantoms.to_definition(_case_block_to_expr()),
            Phantoms.to_definition(_encode_default_case_block()),
            Phantoms.to_definition(_encode_definition()),
            Phantoms.to_definition(_encode_enum_value_assignment()),
            Phantoms.to_definition(_encode_field()),
            Phantoms.to_definition(_encode_field_type()),
            Phantoms.to_definition(_encode_float_value()),
            Phantoms.to_definition(_encode_forall_type()),
            Phantoms.to_definition(_encode_function_type()),
            Phantoms.to_definition(_encode_float_value_encode_float32()),
            Phantoms.to_definition(_encode_float_value_encode_float64()),
            Phantoms.to_definition(_encode_float_value_py_special_float()),
            Phantoms.to_definition(_encode_integer_value()),
            Phantoms.to_definition(_encode_literal()),
            Phantoms.to_definition(_encode_literal_type()),
            Phantoms.to_definition(_encode_name_constants()),
            Phantoms.to_definition(_encode_python_module()),
            Phantoms.to_definition(_encode_record_type()),
            Phantoms.to_definition(_encode_term_assignment()),
            Phantoms.to_definition(_encode_term_inline()),
            Phantoms.to_definition(_encode_term_multiline()),
            Phantoms.to_definition(_encode_term_multiline_tco()),
            Phantoms.to_definition(_encode_type()),
            Phantoms.to_definition(_encode_type_assignment()),
            Phantoms.to_definition(_encode_type_assignment_inner()),
            Phantoms.to_definition(_encode_type_def_single()),
            Phantoms.to_definition(_encode_type_quoted()),
            Phantoms.to_definition(_encode_union_elimination_inline()),
            Phantoms.to_definition(_encode_union_field()),
            Phantoms.to_definition(_encode_union_field_alt()),
            Phantoms.to_definition(_encode_union_type()),
            Phantoms.to_definition(_encode_variable()),
            Phantoms.to_definition(_encode_wrapped_type()),
            Phantoms.to_definition(_environment_type_parameters()),
            Phantoms.to_definition(_enum_variant_pattern()),
            Phantoms.to_definition(_extend_env_with_lambda_params()),
            Phantoms.to_definition(_extend_env_with_type_var()),
            Phantoms.to_definition(_extend_meta_for_term()),
            Phantoms.to_definition(_extend_meta_for_type()),
            Phantoms.to_definition(_extend_meta_for_types()),
            Phantoms.to_definition(_extract_case_elimination()),
            Phantoms.to_definition(_find_type_params()),
            Phantoms.to_definition(_function_definition_to_expr()),
            Phantoms.to_definition(_gather_lambdas()),
            Phantoms.to_definition(_gather_metadata()),
            Phantoms.to_definition(_generic_arg()),
            Phantoms.to_definition(_initial_environment()),
            Phantoms.to_definition(_initial_metadata()),
            Phantoms.to_definition(_is_case_statement_application()),
            Phantoms.to_definition(_is_cases_full()),
            Phantoms.to_definition(_is_type_module_check()),
            Phantoms.to_definition(_is_type_variable_name()),
            Phantoms.to_definition(_is_variant_unit_type()),
            Phantoms.to_definition(_lazy_dot_get()),
            Phantoms.to_definition(_lru_cache_decorator()),
            Phantoms.to_definition(_make_curried_lambda()),
            Phantoms.to_definition(_make_lazy()),
            Phantoms.to_definition(_make_py_graph()),
            Phantoms.to_definition(_make_simple_lambda()),
            Phantoms.to_definition(_make_thunk()),
            Phantoms.to_definition(_make_uncurried_lambda()),
            Phantoms.to_definition(_module_domain_imports()),
            Phantoms.to_definition(_module_imports()),
            Phantoms.to_definition(_module_standard_imports()),
            Phantoms.to_definition(_module_to_python()),
            Phantoms.to_definition(_py_graph_graph()),
            Phantoms.to_definition(_py_graph_metadata()),
            Phantoms.to_definition(_py_int()),
            Phantoms.to_definition(_python_binding_metadata()),
            Phantoms.to_definition(_python_environment_get_graph()),
            Phantoms.to_definition(_python_environment_set_graph()),
            Phantoms.to_definition(_set_meta_namespaces()),
            Phantoms.to_definition(_set_meta_type_variables()),
            Phantoms.to_definition(_set_meta_uses_annotated()),
            Phantoms.to_definition(_set_meta_uses_callable()),
            Phantoms.to_definition(_set_meta_uses_cast()),
            Phantoms.to_definition(_set_meta_uses_dataclass()),
            Phantoms.to_definition(_set_meta_uses_decimal()),
            Phantoms.to_definition(_set_meta_uses_either()),
            Phantoms.to_definition(_set_meta_uses_enum()),
            Phantoms.to_definition(_set_meta_uses_frozen_dict()),
            Phantoms.to_definition(_set_meta_uses_frozen_list()),
            Phantoms.to_definition(_set_meta_uses_frozen_set()),
            Phantoms.to_definition(_set_meta_uses_generic()),
            Phantoms.to_definition(_set_meta_uses_just()),
            Phantoms.to_definition(_set_meta_uses_left()),
            Phantoms.to_definition(_set_meta_uses_lru_cache()),
            Phantoms.to_definition(_set_meta_uses_maybe()),
            Phantoms.to_definition(_set_meta_uses_name()),
            Phantoms.to_definition(_set_meta_uses_node()),
            Phantoms.to_definition(_set_meta_uses_nothing()),
            Phantoms.to_definition(_set_meta_uses_right()),
            Phantoms.to_definition(_set_meta_uses_type_alias()),
            Phantoms.to_definition(_set_meta_uses_type_var()),
            Phantoms.to_definition(_should_thunk_binding()),
            Phantoms.to_definition(_standard_import_statement()),
            Phantoms.to_definition(_target_python_version()),
            Phantoms.to_definition(_term_arity_with_primitives()),
            Phantoms.to_definition(_tvar_statement()),
            Phantoms.to_definition(_type_alias_statement_for()),
            Phantoms.to_definition(_union_type_statements_for()),
            Phantoms.to_definition(_unsupported_expression()),
            Phantoms.to_definition(_use_inline_type_params()),
            Phantoms.to_definition(_use_inline_type_params_for()),
            Phantoms.to_definition(_variant_args()),
            Phantoms.to_definition(_variant_closed_pattern()),
            Phantoms.to_definition(_wildcard_case_block()),
            Phantoms.to_definition(_with_definitions()),
            Phantoms.to_definition(_with_lambda()),
            Phantoms.to_definition(_with_let()),
            Phantoms.to_definition(_with_let_inline()),
            Phantoms.to_definition(_with_type_lambda()),
            Phantoms.to_definition(_wrap_in_nullary_lambda()),
            Phantoms.to_definition(_wrap_lazy_arguments()),
        ),
    )


module_ = _build_module()
