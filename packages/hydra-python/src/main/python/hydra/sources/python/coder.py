"""Python code generator: converts Hydra modules to Python source code.

Mirror of packages/hydra-python/src/main/haskell/Hydra/Sources/Python/Coder.hs.
"""

from hydra.core import Name
from hydra.dsl.python import Just, Nothing  # noqa: F401
from hydra.packaging import Module, ModuleName

import hydra.dsl.meta.lib.equality as Equality
import hydra.dsl.meta.lib.lists as Lists
import hydra.dsl.meta.lib.logic as Logic
import hydra.dsl.meta.lib.maps as Maps
import hydra.dsl.meta.lib.maybes as Maybes
import hydra.dsl.meta.lib.pairs as Pairs
import hydra.dsl.meta.lib.sets as Sets
import hydra.dsl.meta.lib.strings as Strings
from hydra.dsl.meta.phantoms import *  # noqa: F401,F403
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
import hydra.dsl.util as Util

from hydra.sources.python import _kernel_refs as _kref
from hydra.sources.python import _python_helpers as PyDsl  # noqa: F401


# ----------------------------------------------------------------------
# Module setup
# ----------------------------------------------------------------------

NS = ModuleName("hydra.python.coder")

from hydra.sources.python._source_dsl import (

    KERNEL_TYPES_NAMESPACES,
    make_def,
    make_local,
    proj as _proj,
    unqualified_dep,
)

# Mirror Haskell:
#   [PyUtils.ns, PyNames.ns, PySerde.ns, Serialization.ns, Analysis.ns,
#    Environment.ns, Formatting.ns, Names.ns, Predicates.ns, Resolution.ns,
#    Rewriting.ns, Dependencies.ns, Scoping.ns, Strip.ns, Variables.ns,
#    ShowCore.ns, Reduction.ns, Sorting.ns, Inference.ns]
#   L.++ (PyEnvironmentSource.ns:PySyntax.ns:KernelTypes.kernelTypesNamespaces)
DEPENDENCIES = [
    unqualified_dep(ModuleName(n)) for n in [
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
    NS,
    Just("Python code generator: converts Hydra modules to Python source code"),
    DEPENDENCIES,
    (),
)




_def = make_def(_PLACEHOLDER)
_local = make_local("hydra.python.coder")
def _lets_flat(bindings, body):
    # Emit a single flat `let` with multiple bindings (matches Haskell `lets [...]`).
    fields = [field_op(name, val) for name, val in bindings]
    return lets(fields, body)


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
            fields.append(field(f, set_value))
        else:
            fields.append(field(f, _meta_proj(f, m_var)))
    return record("hydra.python.environment.PythonModuleMetadata", fields
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
            field(v, constant(default_result))
        )
    return cases("hydra.core.Type",
        _kref.strip_deannotate_type(arg_term),
        Nothing(),
        fields,
    )


def _empty_meta_record(namespaces_term):
    """Build a fresh PythonModuleMetadata record where namespaces=arg, all use*=false, typeVariables=Sets.empty."""
    fields = []
    for f in _META_FIELDS:
        if f == "namespaces":
            fields.append(field(f, namespaces_term))
        elif f == "typeVariables":
            fields.append(field(f, Sets.empty()))
        else:
            fields.append(field(f, false()))
    return record("hydra.python.environment.PythonModuleMetadata", fields
    )


def _set_meta_use_def(local_name: str, field: str, b_first: bool):
    """Generate a setMetaUses<X> def that sets field to var "b". By default,
    params are (m, b); if b_first, params are (b, m). No doc annotation."""
    params = ["b", "m"] if b_first else ["m", "b"]
    body = lambdas(
        params,
        _meta_record_with_field_set(field, var("b"), m_var="m"),
    )
    return _def(local_name, body)


from hydra.sources.python._source_dsl import py_name as _py_name


# ----------------------------------------------------------------------
# Definitions (alphabetical)
# ----------------------------------------------------------------------

def _analyze_python_function():
    body = lambdas(
        ["cx", "env", "term"],
        _kref.analysis_analyze_function_term_with(var("cx"), _local("pythonBindingMetadata"), _local("pythonEnvironmentGetGraph"), _local("pythonEnvironmentSetGraph"), var("env"), var("term")),
    )
    return _def(
        "analyzePythonFunction",
        doc(
            "Analyze a function term with Python-specific Graph management",
            body,
        ),
    )


def _class_variant_pattern_with_capture():
    body = lambdas(
        ["env", "pyVariantName", "varName"],
        let_chain(
            [
                (
                    "pyVarNameAttr",
                    PySyn.name_or_attribute(list_([var("pyVariantName")])),
                ),
                (
                    "capturePattern",
                    PySyn.closed_pattern_capture(
                        PySyn.capture_pattern(
                            PySyn.pattern_capture_target(
                                _kref.names_encode_name(false(), _kref.util_case_convention_lower_snake, var("env"), var("varName"))
                            )
                        )
                    ),
                ),
                (
                    "keywordPattern",
                    PySyn.keyword_pattern(
                        _py_name("value"),
                        PySyn.pattern_or(
                            PySyn.or_pattern(list_([var("capturePattern")]))
                        ),
                    ),
                ),
            ],
            PySyn.closed_pattern_class(
                PyDsl.class_pattern_with_keywords(
                    var("pyVarNameAttr"),
                    PySyn.keyword_patterns(
                        list_([var("keywordPattern")])
                    ),
                )
            ),
        ),
    )
    return _def(
        "classVariantPatternWithCapture",
        doc(
            "Create a class pattern for a variant with captured value", body
        ),
    )


def _case_block_to_expr():
    eff_lambda_default = let_chain(
        [
            ("syntheticVar", Core.name(string("_matchValue"))),
        ],
        Core.lambda_(
            var("syntheticVar"),
            nothing(),
            Core.term_application(
                Core.application(
                    var("stripped"),
                    Core.term_variable(var("syntheticVar")),
                )
            ),
        ),
    )
    body = lambdas(
        ["cx", "env", "tname", "rowType", "isEnum", "encodeBody", "field"],
        let_chain(
            [
                ("fname", Core.field_name(var("field"))),
                ("fterm", Core.field_term(var("field"))),
                (
                    "stripped",
                    _kref.strip_deannotate_and_detype_term(var("fterm")),
                ),
                (
                    "effectiveLambda",
                    cases_with_default("hydra.core.Term", var("stripped"), eff_lambda_default,
            field("lambda",
                                lam("lam", var("lam")),
                            )),
                ),
                ("v", Core.lambda_parameter(var("effectiveLambda"))),
                ("rawBody", Core.lambda_body(var("effectiveLambda"))),
                (
                    "isUnitVariant",
                    _local("isVariantUnitType")(var("rowType"), var("fname")),
                ),
                (
                    "effectiveBody",
                    Logic.if_else(
                        var("isUnitVariant"),
                        _local("eliminateUnitVar")(var("v"), var("rawBody")),
                        var("rawBody"),
                    ),
                ),
                (
                    "shouldCapture",
                    Logic.not_(
                        Logic.or_(
                            var("isUnitVariant"),
                            Logic.or_(
                                _kref.variables_is_free_variable_in_term(var("v"), var("rawBody")),
                                _kref.predicates_is_unit_term(var("rawBody")),
                            ),
                        )
                    ),
                ),
                (
                    "env2",
                    _local("pythonEnvironmentSetGraph")(_kref.scoping_extend_graph_for_lambda(_local("pythonEnvironmentGetGraph")(var("env")), var("effectiveLambda")), var("env")),
                ),
                (
                    "pyVariantName",
                    _local("deconflictVariantName")(true(), var("env2"), var("tname"), var("fname"), _env("graph", "env2")),
                ),
                (
                    "pattern",
                    _local("variantClosedPattern")(var("env2"), var("tname"), var("fname"), var("pyVariantName"), var("rowType"), var("isEnum"), var("v"), var("shouldCapture")),
                ),
            ],
            Eithers.bind(
                var("encodeBody")(var("env2"), var("effectiveBody")),
                lam(
                    "stmts",
                    let_chain(
                        [
                            (
                                "pyBody",
                                _kref.utils_indented_block(nothing(), list_([var("stmts")])),
                            ),
                        ],
                        right(
                            PySyn.case_block(
                                _kref.utils_py_closed_pattern_to_py_patterns(var("pattern")),
                                nothing(),
                                var("pyBody"),
                            )
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "caseBlockToExpr",
        doc(
            "Encode a single case (Field) into a CaseBlock for a match statement",
            body,
        ),
    )


def _class_variant_pattern_unit():
    body = lambdas(
        ["pyVariantName"],
        PySyn.closed_pattern_class(
            PyDsl.class_pattern_simple(
                PySyn.name_or_attribute(list_([var("pyVariantName")]))
            )
        ),
    )
    return _def(
        "classVariantPatternUnit",
        doc(
            "Create a class pattern for a unit variant (no value captured)",
            body,
        ),
    )


def _cond_import_symbol():
    body = lambdas(
        ["name", "flag"],
        Logic.if_else(
            var("flag"),
            just(var("name")),
            nothing(),
        ),
    )
    return _def(
        "condImportSymbol",
        doc(
            "Conditionally include a symbol name based on a boolean flag",
            body,
        ),
    )


def _collect_type_variables():
    body = lambdas(
        ["initial", "typ"],
        cases_with_default("hydra.core.Type", _kref.strip_deannotate_type(var("typ")), let_chain(
                    [
                        (
                            "freeVars",
                            _kref.variables_free_variables_in_type(var("typ")),
                        ),
                        (
                            "isTypeVar",
                            lam(
                                "n",
                                _local("isTypeVariableName")(var("n")),
                            ),
                        ),
                        (
                            "filteredList",
                            Lists.filter(
                                var("isTypeVar"),
                                Sets.to_list(var("freeVars")),
                            ),
                        ),
                    ],
                    Sets.union(
                        var("initial"),
                        Sets.from_list(var("filteredList")),
                    ),
                ),
            field("forall",
                    lam(
                        "ft",
                        let_chain(
                            [
                                (
                                    "v",
                                    Core.forall_type_parameter(var("ft")),
                                ),
                                (
                                    "body",
                                    Core.forall_type_body(var("ft")),
                                ),
                            ],
                            _local("collectTypeVariables")(Sets.insert(
                                    var("v"), var("initial")
                                ), var("body")),
                        ),
                    ),
                )),
    )
    return _def(
        "collectTypeVariables",
        doc("Collect type variables from a type", body),
    )


def _deconflict_variant_name():
    body = lambdas(
        ["isQualified", "env", "unionName", "fname", "g"],
        let_chain(
            [
                (
                    "candidateHydraName",
                    wrap("hydra.core.Name",
                        Strings.cat2(
                            Core.un_name(var("unionName")),
                            _kref.formatting_capitalize(Core.un_name(var("fname"))),
                        ),
                    ),
                ),
                (
                    "termCollision",
                    Maps.member(
                        var("candidateHydraName"),
                        Graph_dsl.graph_bound_terms(var("g")),
                    ),
                ),
                (
                    "typeCollision",
                    Maps.member(
                        var("candidateHydraName"),
                        Graph_dsl.graph_schema_types(var("g")),
                    ),
                ),
                (
                    "collision",
                    Logic.or_(
                        var("termCollision"), var("typeCollision")
                    ),
                ),
            ],
            Logic.if_else(
                var("collision"),
                _py_name(
                    Strings.cat2(
                        unwrap("hydra.python.syntax.Name")(_kref.names_variant_name(var("isQualified"), var("env"), var("unionName"), var("fname"))),
                        string("_"),
                    )
                ),
                _kref.names_variant_name(var("isQualified"), var("env"), var("unionName"), var("fname")),
            ),
        ),
    )
    return _def(
        "deconflictVariantName",
        doc(
            "Deconflict a variant name to avoid collisions with type names", body
        ),
    )


def _deduplicate_case_variables():
    rewrite_lambda = let_chain(
        [
            ("v", Core.lambda_parameter(var("lam"))),
            ("mdom", Core.lambda_domain(var("lam"))),
            ("body", Core.lambda_body(var("lam"))),
        ],
        Maybes.maybe(
            pair(
                Maps.insert(
                    var("v"),
                    int32(1),
                    var("countByName"),
                ),
                Lists.cons(var("field"), var("done")),
            ),
            lam(
                "count",
                let_chain(
                    [
                        ("count2", Math.add(var("count"), int32(1))),
                        (
                            "v2",
                            Core.name(
                                Strings.cat2(
                                    Core.un_name(var("v")),
                                    Literals.show_int32(var("count2")),
                                )
                            ),
                        ),
                        (
                            "newBody",
                            _kref.reduction_alpha_convert(var("v"), var("v2"), var("body")),
                        ),
                        (
                            "newLam",
                            Core.lambda_(
                                var("v2"),
                                var("mdom"),
                                var("newBody"),
                            ),
                        ),
                        (
                            "newTerm",
                            inject("hydra.core.Term",
                                Name("lambda"),
                                var("newLam"),
                            ),
                        ),
                        (
                            "newField",
                            Core.field(var("fname"), var("newTerm")),
                        ),
                    ],
                    pair(
                        Maps.insert(
                            var("v"),
                            var("count2"),
                            var("countByName"),
                        ),
                        Lists.cons(var("newField"), var("done")),
                    ),
                ),
            ),
            Maps.lookup(var("v"), var("countByName")),
        ),
    )
    rewrite_case = lambdas(
        ["state", "field"],
        let_chain(
            [
                ("countByName", Pairs.first(var("state"))),
                ("done", Pairs.second(var("state"))),
                ("fname", Core.field_name(var("field"))),
                ("fterm", Core.field_term(var("field"))),
            ],
            cases_with_default("hydra.core.Term", _kref.strip_deannotate_and_detype_term(var("fterm")), pair(
                        var("countByName"),
                        Lists.cons(var("field"), var("done")),
                    ),
            field("lambda",
                        lam("lam", rewrite_lambda),
                    )),
        ),
    )
    body = lambdas(
        ["cases_"],
        let_chain(
            [
                ("rewriteCase", rewrite_case),
                (
                    "result",
                    Lists.foldl(
                        var("rewriteCase"),
                        pair(Maps.empty(), list_([])),
                        var("cases_"),
                    ),
                ),
            ],
            Lists.reverse(Pairs.second(var("result"))),
        ),
    )
    return _def(
        "deduplicateCaseVariables",
        doc(
            "Rewrite case statements to avoid variable name collisions", body
        ),
    )


def _dig_for_wrap():
    body = lambdas(
        ["isTermAnnot", "meta", "typ"],
        cases_with_default("hydra.core.Type", _kref.strip_deannotate_type(var("typ")), var("meta"),
            field("forall",
                    lam(
                        "ft",
                        _local("digForWrap")(var("isTermAnnot"), var("meta"), Core.forall_type_body(var("ft"))),
                    ),
                ),
            field("wrap",
                    constant(
                        Logic.if_else(
                            var("isTermAnnot"),
                            var("meta"),
                            _local("setMetaUsesNode")(var("meta"), true()),
                        )
                    ),
                )),
    )
    return _def(
        "digForWrap",
        doc("Recursively dig through forall types to find wrap types", body),
    )


def _eliminate_unit_var():
    rewrite_field = lambdas(
        ["rewrite", "fld"],
        Core.field(
            Core.field_name(var("fld")),
            var("rewrite")(Core.field_term(var("fld"))),
        ),
    )
    rewrite_binding = lambdas(
        ["rewrite", "bnd"],
        Core.binding(
            Core.binding_name(var("bnd")),
            var("rewrite")(Core.binding_term(var("bnd"))),
            Core.binding_type_scheme(var("bnd")),
        ),
    )
    rewrite_body_fields = [
        field("variable",
            lam(
                "n",
                Logic.if_else(
                    Equality.equal(var("n"), var("v")),
                    Core.term_unit,
                    var("term"),
                ),
            ),
        ),
        field("annotated",
            lam(
                "at",
                Core.term_annotated(
                    Core.annotated_term(
                        var("recurse")(Core.annotated_term_body(var("at"))),
                        Core.annotated_term_annotation(var("at")),
                    )
                ),
            ),
        ),
        field("application",
            lam(
                "app",
                Core.term_application(
                    Core.application(
                        var("recurse")(Core.application_function(var("app"))),
                        var("recurse")(Core.application_argument(var("app"))),
                    )
                ),
            ),
        ),
        field("lambda",
            lam(
                "lam",
                Logic.if_else(
                    Equality.equal(
                        Core.lambda_parameter(var("lam")), var("v")
                    ),
                    var("term"),
                    Core.term_lambda(
                        Core.lambda_(
                            Core.lambda_parameter(var("lam")),
                            Core.lambda_domain(var("lam")),
                            var("recurse")(Core.lambda_body(var("lam"))),
                        )
                    ),
                ),
            ),
        ),
        field("cases",
            lam(
                "cs",
                Core.term_cases(
                    Core.case_statement(
                        Core.case_statement_type_name(var("cs")),
                        Maybes.map(
                            var("recurse"),
                            Core.case_statement_default(var("cs")),
                        ),
                        Lists.map(
                            var("rewriteField")(var("recurse")),
                            Core.case_statement_cases(var("cs")),
                        ),
                    )
                ),
            ),
        ),
        field("let",
            lam(
                "lt",
                Core.term_let(
                    getattr(Core, "let")(
                        Lists.map(
                            var("rewriteBinding")(var("recurse")),
                            Core.let_bindings(var("lt")),
                        ),
                        var("recurse")(Core.let_body(var("lt"))),
                    )
                ),
            ),
        ),
        field("list",
            lam(
                "ts",
                Core.term_list(
                    Lists.map(var("recurse"), var("ts"))
                ),
            ),
        ),
        field("map",
            lam(
                "m",
                Core.term_map(
                    Maps.from_list(
                        Lists.map(
                            lam(
                                "kv",
                                pair(
                                    var("recurse")(Pairs.first(var("kv"))),
                                    var("recurse")(Pairs.second(var("kv"))),
                                ),
                            ),
                            Maps.to_list(var("m")),
                        )
                    )
                ),
            ),
        ),
        field("record",
            lam(
                "rec",
                Core.term_record(
                    Core.record(
                        Core.record_type_name(var("rec")),
                        Lists.map(
                            var("rewriteField")(var("recurse")),
                            Core.record_fields(var("rec")),
                        ),
                    )
                ),
            ),
        ),
        field("set",
            lam(
                "s",
                Core.term_set(
                    Sets.map(var("recurse"), var("s"))
                ),
            ),
        ),
        field("inject",
            lam(
                "inj",
                Core.term_inject(
                    Core.injection(
                        Core.injection_type_name(var("inj")),
                        var("rewriteField")(var("recurse"), Core.injection_field(var("inj"))),
                    )
                ),
            ),
        ),
        field("maybe",
            lam(
                "mt",
                Core.term_maybe(
                    Maybes.map(var("recurse"), var("mt"))
                ),
            ),
        ),
        field("pair",
            lam(
                "p",
                Core.term_pair(
                    pair(
                        var("recurse")(Pairs.first(var("p"))),
                        var("recurse")(Pairs.second(var("p"))),
                    )
                ),
            ),
        ),
        field("wrap",
            lam(
                "wt",
                Core.term_wrap(
                    Core.wrapped_term(
                        Core.wrapped_term_type_name(var("wt")),
                        var("recurse")(Core.wrapped_term_body(var("wt"))),
                    )
                ),
            ),
        ),
        field("either",
            lam(
                "e",
                Core.term_either(
                    Eithers.bimap(
                        var("recurse"),
                        var("recurse"),
                        var("e"),
                    )
                ),
            ),
        ),
        field("typeApplication",
            lam(
                "ta",
                Core.term_type_application(
                    Core.type_application_term(
                        var("recurse")(Core.type_application_term_body(var("ta"))),
                        Core.type_application_term_type(var("ta")),
                    )
                ),
            ),
        ),
        field("typeLambda",
            lam(
                "tl",
                Core.term_type_lambda(
                    Core.type_lambda(
                        Core.type_lambda_parameter(var("tl")),
                        var("recurse")(Core.type_lambda_body(var("tl"))),
                    )
                ),
            ),
        ),
    ]
    rewrite = lambdas(
        ["recurse", "term"],
        cases("hydra.core.Term",
            _kref.strip_deannotate_and_detype_term(var("term")),
            Just(var("term")),
            rewrite_body_fields,
        ),
    )
    body = lambdas(
        ["v", "term0"],
        let_chain(
            [
                ("rewriteField", rewrite_field),
                ("rewriteBinding", rewrite_binding),
                ("rewrite", rewrite),
                (
                    "go",
                    lam(
                        "term",
                        var("rewrite")(var("go"), var("term")),
                    ),
                ),
            ],
            var("go")(var("term0")),
        ),
    )
    return _def(
        "eliminateUnitVar",
        doc(
            "Substitute unit for a variable in a term (for unit variant case handling)",
            body,
        ),
    )


def _empty_metadata():
    body = lambdas(["ns"], _empty_meta_record(var("ns")))
    return _def(
        "emptyMetadata",
        doc(
            "Create an initial empty metadata record with given namespaces", body
        ),
    )


def _encode_application():
    body = lambdas(
        ["cx", "env", "app"],
        let_chain(
            [
                (
                    "g",
                    _local("pythonEnvironmentGetGraph")(var("env")),
                ),
                ("term", Core.term_application(var("app"))),
                (
                    "gathered",
                    _kref.analysis_gather_args(var("term"), list_([])),
                ),
                ("fun", Pairs.first(var("gathered"))),
                ("args", Pairs.second(var("gathered"))),
                (
                    "knownArity",
                    _local("termArityWithPrimitives")(var("g"), var("fun")),
                ),
                (
                    "arity",
                    Math.max_(
                        var("knownArity"), Lists.length(var("args"))
                    ),
                ),
            ],
            Eithers.bind(
                Eithers.map_list(
                    lam(
                        "t",
                        _local("encodeTermInline")(var("cx"), var("env"), false(), var("t")),
                    ),
                    var("args"),
                ),
                lam(
                    "pargs",
                    let_chain(
                        [
                            (
                                "hargs",
                                Lists.take(var("arity"), var("pargs")),
                            ),
                            (
                                "rargs",
                                Lists.drop(var("arity"), var("pargs")),
                            ),
                        ],
                        Eithers.bind(
                            _local("encodeApplicationInner")(var("cx"), var("env"), var("fun"), var("hargs"), var("rargs")),
                            lam(
                                "result",
                                let_chain(
                                    [
                                        ("lhs", Pairs.first(var("result"))),
                                        (
                                            "remainingRargs",
                                            Pairs.second(var("result")),
                                        ),
                                        (
                                            "pyapp",
                                            Lists.foldl(
                                                lambdas(
                                                    ["t", "a"],
                                                    _kref.utils_function_call(_kref.utils_py_expression_to_py_primary(var("t")), list_(
                                                            [var("a")]
                                                        )),
                                                ),
                                                var("lhs"),
                                                var("remainingRargs"),
                                            ),
                                        ),
                                    ],
                                    right(var("pyapp")),
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
        doc("Encode a function application to a Python expression", body),
    )


def _encode_application_inner():
    with_rest = lam(
        "e",
        Logic.if_else(
            Lists.null(var("restArgs")),
            var("e"),
            _kref.utils_function_call(_kref.utils_py_expression_to_py_primary(var("e")), var("restArgs")),
        ),
    )
    default_case = Eithers.bind(
        _local("encodeTermInline")(var("cx"), var("env"), false(), var("fun")),
        lam(
            "pfun",
            right(
                pair(
                    _kref.utils_function_call(_kref.utils_py_expression_to_py_primary(var("pfun")), var("hargs")),
                    var("rargs"),
                )
            ),
        ),
    )
    project_branch = lam(
        "proj",
        let_chain(
            [
                ("fname", _proj("hydra.core.Projection", "fieldName", "proj")),
                (
                    "fieldExpr",
                    _kref.utils_project_from_expression(var("firstArg"), _kref.names_encode_field_name(var("env"), var("fname"))),
                ),
            ],
            right(
                pair(
                    var("withRest")(var("fieldExpr")),
                    var("rargs"),
                )
            ),
        ),
    )
    cases_branch = lam(
        "cs",
        Eithers.bind(
            _local("encodeUnionEliminationInline")(var("cx"), var("env"), var("cs"), var("firstArg")),
            lam(
                "inlineExpr",
                right(
                    pair(
                        var("withRest")(var("inlineExpr")),
                        var("rargs"),
                    )
                ),
            ),
        ),
    )
    unwrap_branch = constant(
        let_chain(
            [
                (
                    "valueExpr",
                    _kref.utils_project_from_expression(var("firstArg"), _py_name("value")),
                ),
                (
                    "allArgs",
                    Lists.concat2(var("restArgs"), var("rargs")),
                ),
            ],
            Logic.if_else(
                Lists.null(var("allArgs")),
                right(
                    pair(
                        var("valueExpr"), list_([])
                    )
                ),
                right(
                    pair(
                        _kref.utils_function_call(_kref.utils_py_expression_to_py_primary(var("valueExpr")), var("allArgs")),
                        list_([]),
                    )
                ),
            ),
        )
    )
    lambda_branch = constant(
        Eithers.bind(
            _local("encodeTermInline")(var("cx"), var("env"), false(), var("fun")),
            lam(
                "pfun",
                right(
                    pair(
                        _kref.utils_function_call(_kref.utils_py_expression_to_py_primary(var("pfun")), var("hargs")),
                        var("rargs"),
                    )
                ),
            ),
        )
    )
    # Variable branch: complex
    not_in_graph_branch = Eithers.bind(
        _local("encodeVariable")(var("cx"), var("env"), var("name"), var("hargs")),
        lam(
            "expr",
            right(
                pair(var("expr"), var("rargs"))
            ),
        ),
    )
    has_ts_branch = lam(
        "ts",
        let_chain(
            [
                (
                    "elArity",
                    _kref.arity_type_scheme_arity(var("ts")),
                ),
                (
                    "consumeCount",
                    Math.min_(
                        var("elArity"), Lists.length(var("allArgs"))
                    ),
                ),
                (
                    "consumedArgs",
                    Lists.take(
                        var("consumeCount"), var("allArgs")
                    ),
                ),
                (
                    "remainingArgs",
                    Lists.drop(
                        var("consumeCount"), var("allArgs")
                    ),
                ),
            ],
            Logic.if_else(
                Lists.null(var("consumedArgs")),
                Eithers.bind(
                    _local("encodeVariable")(var("cx"), var("env"), var("name"), list_([])),
                    lam(
                        "expr",
                        right(
                            pair(
                                var("expr"), var("rargs")
                            )
                        ),
                    ),
                ),
                # Lazy-aware: if the binding is an inline let (Lazy-wrapped),
                # call .get() before applying.
                Logic.if_else(
                    Sets.member(var("name"), var("inlineVars")),
                    right(
                        pair(
                            _kref.utils_function_call(_kref.utils_py_expression_to_py_primary(_local("lazyDotGet")(_kref.names_term_variable_reference(var("env"), var("name")))), var("consumedArgs")),
                            var("remainingArgs"),
                        )
                    ),
                    right(
                        pair(
                            _kref.utils_function_call(_kref.utils_py_name_to_py_primary(_kref.names_encode_name(true(), _kref.util_case_convention_lower_snake, var("env"), var("name"))), var("consumedArgs")),
                            var("remainingArgs"),
                        )
                    ),
                ),
            ),
        ),
    )
    in_graph_branch = lam(
        "el",
        Maybes.maybe(
            Eithers.bind(
                _local("encodeVariable")(var("cx"), var("env"), var("name"), var("hargs")),
                lam(
                    "expr",
                    right(
                        pair(
                            var("expr"), var("rargs")
                        )
                    ),
                ),
            ),
            has_ts_branch,
            Core.binding_type_scheme(var("el")),
        ),
    )
    not_primitive_branch = Maybes.maybe(
        not_in_graph_branch,
        in_graph_branch,
        _kref.lexical_lookup_binding(var("g"), var("name")),
    )
    is_primitive_branch = lam(
        "_prim",
        let_chain(
            [
                (
                    "wrappedArgs",
                    _local("wrapLazyArguments")(var("g"), var("name"), var("hargs")),
                ),
            ],
            Eithers.bind(
                _local("encodeVariable")(var("cx"), var("env"), var("name"), var("wrappedArgs")),
                lam(
                    "expr",
                    right(
                        pair(
                            var("expr"), var("rargs")
                        )
                    ),
                ),
            ),
        ),
    )
    variable_branch = lam(
        "name",
        let_chain(
            [
                (
                    "g",
                    _local("pythonEnvironmentGetGraph")(var("env")),
                ),
                (
                    "allArgs",
                    Lists.concat2(var("hargs"), var("rargs")),
                ),
                ("inlineVars", _env("inlineVariables", "env")),
            ],
            Maybes.cases(
                Maps.lookup(
                    var("name"),
                    Graph_dsl.graph_primitives(var("g")),
                ),
                not_primitive_branch,
                is_primitive_branch,
            ),
        ),
    )
    body = lambdas(
        ["cx", "env", "fun", "hargs", "rargs"],
        let_chain(
            [
                (
                    "firstArg",
                    Maybes.from_maybe(
                        _kref.utils_py_name_to_py_expression(_py_name("")),
                        Lists.maybe_head(var("hargs")),
                    ),
                ),
                (
                    "restArgs",
                    Lists.drop(int32(1), var("hargs")),
                ),
                ("withRest", with_rest),
                ("defaultCase", default_case),
            ],
            cases_with_default("hydra.core.Term", _kref.strip_deannotate_and_detype_term(var("fun")), var("defaultCase"),
            field("project", project_branch),
            field("cases", cases_branch),
            field("unwrap", unwrap_branch),
            field("lambda", lambda_branch),
            field("variable", variable_branch)),
        ),
    )
    return _def(
        "encodeApplicationInner",
        doc("Inner helper for encodeApplication", body),
    )


def _encode_application_type():
    gather_params_inner = lambdas(
        ["t", "ps"],
        _type_cases_with_one_branch(
            var("t"),
            pair(var("t"), var("ps")),
            field("application",
                lam(
                    "appT",
                    var("gatherParams")(_proj("hydra.core.ApplicationType", "function", "appT"), Lists.cons(
                            _proj("hydra.core.ApplicationType", "argument", "appT"),
                            var("ps"),
                        )),
                ),
            ),
            ["annotated", "function", "forall", "list", "literal", "map",
             "maybe", "either", "pair", "record", "set", "union", "unit",
             "variable", "void", "wrap"],
        ),
    )
    body = lambdas(
        ["env", "at"],
        let_chain(
            [("gatherParams", gather_params_inner)],
            let_chain(
                [
                    (
                        "bodyAndArgs",
                        var("gatherParams")(inject("hydra.core.Type",
                                Name("application"),
                                var("at"),
                            ), list_([])),
                    ),
                    ("body", Pairs.first(var("bodyAndArgs"))),
                    ("args", Pairs.second(var("bodyAndArgs"))),
                ],
                Eithers.bind(
                    _local("encodeType")(var("env"), var("body")),
                    lam(
                        "pyBody",
                        Eithers.bind(
                            Eithers.map_list(
                                _local("encodeType")(var("env")),
                                var("args"),
                            ),
                            lam(
                                "pyArgs",
                                right(
                                    _kref.utils_primary_and_params(_kref.utils_py_expression_to_py_primary(var("pyBody")), var("pyArgs"))
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
        doc("Encode an application type to Python expression", body),
    )


def _encode_binding_as():
    # Helper: build a function definition from cs (CaseStatement), single param "x".
    # Used by both branches that fall back to "case elimination function".
    def case_elim_fn(cs_var):
        return let_chain(
            [
                ("tname", Core.case_statement_type_name(var(cs_var))),
                ("dflt", Core.case_statement_default(var(cs_var))),
                ("cases_", Core.case_statement_cases(var(cs_var))),
            ],
            Eithers.bind(
                _kref.resolution_require_union_type(var("cx"), _local("pythonEnvironmentGetGraph")(var("env")), var("tname")),
                lam(
                    "rt",
                    let_chain(
                        [
                            (
                                "isEnum",
                                _kref.predicates_is_enum_row_type(var("rt")),
                            ),
                            (
                                "isFull",
                                _local("isCasesFull")(var("rt"), var("cases_")),
                            ),
                            (
                                "innerParam",
                                PySyn.param(_py_name("x"), nothing()),
                            ),
                            (
                                "param",
                                record("hydra.python.syntax.ParamNoDefault",
                                    [
                                        field("param", var("innerParam")
                                        ),
                                        field("typeComment", nothing()
                                        ),
                                    ],
                                ),
                            ),
                            (
                                "params",
                                PySyn.parameters_param_no_default(
                                    record(
                                        Name(
                                            "hydra.python.syntax.ParamNoDefaultParameters"
                                        ),
                                        [
                                            field("paramNoDefault",
                                                list_(
                                                    [var("param")]
                                                ),
                                            ),
                                            field("paramWithDefault",
                                                list_([]),
                                            ),
                                            field("starEtc", nothing()
                                            ),
                                        ],
                                    )
                                ),
                            ),
                        ],
                        Eithers.bind(
                            Eithers.map_list(
                                _local("caseBlockToExpr")(var("cx"), var("env"), var("tname"), var("rt"), var("isEnum"), lambdas(
                                        ["e", "t"],
                                        _local("encodeTermMultiline")(var("cx"), var("e"), var("t")),
                                    )),
                                var("cases_"),
                            ),
                            lam(
                                "pyCases",
                                Eithers.bind(
                                    _local("encodeDefaultCaseBlock")(lam(
                                            "t",
                                            _local("encodeTermInline")(var("cx"), var("env"), false(), var("t")),
                                        ), var("isFull"), var("dflt"), var("tname")),
                                    lam(
                                        "pyDflt",
                                        let_chain(
                                            [
                                                (
                                                    "subj",
                                                    PySyn.subject_expression_simple(
                                                        PySyn.named_expression_simple(
                                                            _kref.utils_py_name_to_py_expression(_py_name("x"))
                                                        )
                                                    ),
                                                ),
                                                (
                                                    "allCases",
                                                    Lists.concat2(
                                                        var("pyCases"),
                                                        var("pyDflt"),
                                                    ),
                                                ),
                                                (
                                                    "matchStmt",
                                                    PySyn.statement_compound(
                                                        PySyn.compound_statement_match(
                                                            record(
                                                                Name(
                                                                    "hydra.python.syntax.MatchStatement"
                                                                ),
                                                                [
                                                                    field("subject",
                                                                        var(
                                                                            "subj"
                                                                        ),
                                                                    ),
                                                                    field("cases",
                                                                        var(
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
                                                    _kref.utils_indented_block(nothing(), list_(
                                                            [
                                                                list_(
                                                                    [
                                                                        var(
                                                                            "matchStmt"
                                                                        )
                                                                    ]
                                                                )
                                                            ]
                                                        )),
                                                ),
                                                (
                                                    "funcDefRaw",
                                                    record(
                                                        Name(
                                                            "hydra.python.syntax.FunctionDefRaw"
                                                        ),
                                                        [
                                                            field("async",
                                                                false(),
                                                            ),
                                                            field("name",
                                                                var("fname"),
                                                            ),
                                                            field("typeParams",
                                                                list_([]),
                                                            ),
                                                            field("params",
                                                                just(
                                                                    var(
                                                                        "params"
                                                                    )
                                                                ),
                                                            ),
                                                            field("returnType",
                                                                nothing(),
                                                            ),
                                                            field(
                                                                Name(
                                                                    "funcTypeComment"
                                                                ),
                                                                nothing(),
                                                            ),
                                                            field("block",
                                                                var("body"),
                                                            ),
                                                        ],
                                                    ),
                                                ),
                                            ],
                                            right(
                                                PySyn.statement_compound(
                                                    PySyn.compound_statement_function(
                                                        PySyn.function_definition(
                                                            nothing(),
                                                            var("funcDefRaw"),
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
        _local("encodeTermMultiline")(var("cx"), var("env"), var("term1")),
        lam(
            "stmts",
            Maybes.maybe(
                left(
                    Errors_dsl.error_other(
                        Errors_dsl.other_error(
                            string(
                                "encodeTermMultiline returned no statements"
                            )
                        )
                    )
                ),
                lam("x", right(var("x"))),
                Lists.maybe_head(var("stmts")),
            ),
        ),
    )

    no_ts_no_csa_branch = let_chain(
        [
            ("mcs", _local("extractCaseElimination")(var("term1"))),
        ],
        Maybes.maybe(fallback, lam("cs", case_elim_fn("cs")), var("mcs")),
    )

    # Hoisted binding branch: with lambda params, use captured + match params
    hoisted_branch = lam(
        "csa",
        Logic.if_else(
            Lists.null(var("lambdaParams")),
            no_ts_no_csa_branch,
            let_chain(
                [
                    ("tname", Pairs.first(var("csa"))),
                    ("rest1", Pairs.second(var("csa"))),
                    ("dflt", Pairs.first(var("rest1"))),
                    ("rest2", Pairs.second(var("rest1"))),
                    ("cases_", Pairs.first(var("rest2"))),
                ],
                Eithers.bind(
                    _kref.resolution_require_union_type(var("cx"), _local("pythonEnvironmentGetGraph")(var("env")), var("tname")),
                    lam(
                        "rt",
                        let_chain(
                            [
                                (
                                    "isEnum",
                                    _kref.predicates_is_enum_row_type(var("rt")),
                                ),
                                (
                                    "isFull",
                                    _local("isCasesFull")(var("rt"), var("cases_")),
                                ),
                                (
                                    "capturedVarNames",
                                    Maybes.from_maybe(
                                        list_([]),
                                        Lists.maybe_init(var("lambdaParams")),
                                    ),
                                ),
                                (
                                    "matchLambdaParam",
                                    Maybes.from_maybe(
                                        wrap("hydra.core.Name",
                                            string(""),
                                        ),
                                        Lists.maybe_last(var("lambdaParams")),
                                    ),
                                ),
                                (
                                    "capturedParams",
                                    Lists.map(
                                        lam(
                                            "n",
                                            record(
                                                Name(
                                                    "hydra.python.syntax.ParamNoDefault"
                                                ),
                                                [
                                                    field("param",
                                                        PySyn.param(
                                                            _kref.names_encode_name(false(), _kref.util_case_convention_lower_snake, var("env"), var("n")),
                                                            nothing(),
                                                        ),
                                                    ),
                                                    field("typeComment",
                                                        nothing(),
                                                    ),
                                                ],
                                            ),
                                        ),
                                        var("capturedVarNames"),
                                    ),
                                ),
                                (
                                    "matchArgName",
                                    _kref.names_encode_name(false(), _kref.util_case_convention_lower_snake, var("env"), var("matchLambdaParam")),
                                ),
                                (
                                    "matchParam",
                                    record(
                                        Name(
                                            "hydra.python.syntax.ParamNoDefault"
                                        ),
                                        [
                                            field("param",
                                                PySyn.param(
                                                    var("matchArgName"),
                                                    nothing(),
                                                ),
                                            ),
                                            field("typeComment",
                                                nothing(),
                                            ),
                                        ],
                                    ),
                                ),
                                (
                                    "allParams",
                                    Lists.concat2(
                                        var("capturedParams"),
                                        list_([var("matchParam")]),
                                    ),
                                ),
                                (
                                    "params",
                                    PySyn.parameters_param_no_default(
                                        record(
                                            Name(
                                                "hydra.python.syntax.ParamNoDefaultParameters"
                                            ),
                                            [
                                                field("paramNoDefault",
                                                    var("allParams"),
                                                ),
                                                field("paramWithDefault",
                                                    list_([]),
                                                ),
                                                field("starEtc",
                                                    nothing(),
                                                ),
                                            ],
                                        )
                                    ),
                                ),
                                (
                                    "envWithParams",
                                    _local("extendEnvWithLambdaParams")(var("env"), var("term1")),
                                ),
                            ],
                            Eithers.bind(
                                Eithers.map_list(
                                    _local("caseBlockToExpr")(var("cx"), var("envWithParams"), var("tname"), var("rt"), var("isEnum"), lambdas(
                                            ["e", "t"],
                                            _local("encodeTermMultiline")(var("cx"), var("e"), var("t")),
                                        )),
                                    var("cases_"),
                                ),
                                lam(
                                    "pyCases",
                                    Eithers.bind(
                                        _local("encodeDefaultCaseBlock")(lam(
                                                "t",
                                                _local("encodeTermInline")(var("cx"), var("envWithParams"), false(), var("t")),
                                            ), var("isFull"), var("dflt"), var("tname")),
                                        lam(
                                            "pyDflt",
                                            let_chain(
                                                [
                                                    (
                                                        "subj",
                                                        PySyn.subject_expression_simple(
                                                            PySyn.named_expression_simple(
                                                                _kref.utils_py_name_to_py_expression(var(
                                                                        "matchArgName"
                                                                    ))
                                                            )
                                                        ),
                                                    ),
                                                    (
                                                        "allCases",
                                                        Lists.concat2(
                                                            var("pyCases"),
                                                            var("pyDflt"),
                                                        ),
                                                    ),
                                                    (
                                                        "matchStmt",
                                                        PySyn.statement_compound(
                                                            PySyn.compound_statement_match(
                                                                record(
                                                                    Name(
                                                                        "hydra.python.syntax.MatchStatement"
                                                                    ),
                                                                    [
                                                                        field(
                                                                            Name(
                                                                                "subject"
                                                                            ),
                                                                            var(
                                                                                "subj"
                                                                            ),
                                                                        ),
                                                                        field(
                                                                            Name(
                                                                                "cases"
                                                                            ),
                                                                            var(
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
                                                        _kref.utils_indented_block(nothing(), list_(
                                                                [
                                                                    list_(
                                                                        [
                                                                            var(
                                                                                "matchStmt"
                                                                            )
                                                                        ]
                                                                    )
                                                                ]
                                                            )),
                                                    ),
                                                    (
                                                        "funcDefRaw",
                                                        record(
                                                            Name(
                                                                "hydra.python.syntax.FunctionDefRaw"
                                                            ),
                                                            [
                                                                field("async",
                                                                    false(),
                                                                ),
                                                                field("name",
                                                                    var(
                                                                        "fname"
                                                                    ),
                                                                ),
                                                                field("typeParams",
                                                                    list_([]),
                                                                ),
                                                                field("params",
                                                                    just(
                                                                        var(
                                                                            "params"
                                                                        )
                                                                    ),
                                                                ),
                                                                field("returnType",
                                                                    nothing(),
                                                                ),
                                                                field(
                                                                    Name(
                                                                        "funcTypeComment"
                                                                    ),
                                                                    nothing(),
                                                                ),
                                                                field("block",
                                                                    var(
                                                                        "body"
                                                                    ),
                                                                ),
                                                            ],
                                                        ),
                                                    ),
                                                ],
                                                right(
                                                    PySyn.statement_compound(
                                                        PySyn.compound_statement_function(
                                                            PySyn.function_definition(
                                                                nothing(),
                                                                var(
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

    no_ts_branch = let_chain(
        [
            ("gathered", _local("gatherLambdas")(var("term1"))),
            ("lambdaParams", Pairs.first(var("gathered"))),
            ("innerBody", Pairs.second(var("gathered"))),
            (
                "mcsa",
                _local("isCaseStatementApplication")(var("innerBody")),
            ),
        ],
        Maybes.maybe(no_ts_no_csa_branch, hoisted_branch, var("mcsa")),
    )

    has_ts_branch = lam(
        "ts",
        Eithers.bind(
            _kref.annotations_get_term_description(var("cx"), _local("pythonEnvironmentGetGraph")(var("env")), var("term1")),
            lam(
                "comment",
                let_chain(
                    [
                        (
                            "normComment",
                            Maybes.map(
                                _kref.formatting_normalize_comment,
                                var("comment"),
                            ),
                        ),
                    ],
                    _local("encodeTermAssignment")(var("cx"), var("env"), false(), var("name1"), var("term1"), var("ts"), var("normComment")),
                ),
            ),
        ),
    )

    body = lambdas(
        ["cx", "env", "binding"],
        let_chain(
            [
                ("name1", Core.binding_name(var("binding"))),
                ("term1", Core.binding_term(var("binding"))),
                ("mts", Core.binding_type_scheme(var("binding"))),
                (
                    "fname",
                    _kref.names_encode_name(true(), _kref.util_case_convention_lower_snake, var("env"), var("name1")),
                ),
            ],
            Maybes.maybe(no_ts_branch, has_ts_branch, var("mts")),
        ),
    )
    return _def(
        "encodeBindingAs",
        doc(
            "Encode a binding as a Python statement (function definition or assignment)",
            body,
        ),
    )


def _encode_binding_as_assignment():
    body = lambdas(
        ["cx", "allowThunking", "env", "binding"],
        let_chain(
            [
                ("name", Core.binding_name(var("binding"))),
                ("term", Core.binding_term(var("binding"))),
                ("mts", Core.binding_type_scheme(var("binding"))),
                (
                    "pyName",
                    _kref.names_encode_name(false(), _kref.util_case_convention_lower_snake, var("env"), var("name")),
                ),
            ],
            Eithers.bind(
                _local("encodeTermInline")(var("cx"), var("env"), false(), var("term")),
                lam(
                    "pbody",
                    let_chain(
                        [
                            ("tc", _env("graph", "env")),
                            (
                                "isComplexVar",
                                _kref.predicates_is_complex_variable(var("tc"), var("name")),
                            ),
                            (
                                "termIsComplex",
                                _kref.predicates_is_complex_term(var("tc"), var("term")),
                            ),
                            (
                                "isTrivial",
                                _kref.predicates_is_trivial_term(var("term")),
                            ),
                            (
                                "needsThunk",
                                Logic.if_else(
                                    var("isTrivial"),
                                    boolean(False),
                                    Maybes.maybe(
                                        Logic.and_(
                                            var("allowThunking"),
                                            Logic.or_(
                                                var("isComplexVar"),
                                                var("termIsComplex"),
                                            ),
                                        ),
                                        lam(
                                            "ts",
                                            Logic.and_(
                                                var("allowThunking"),
                                                Logic.and_(
                                                    Equality.equal(
                                                        _kref.arity_type_scheme_arity(var("ts")),
                                                        int_(0),
                                                    ),
                                                    Logic.or_(
                                                        var("isComplexVar"),
                                                        var("termIsComplex"),
                                                    ),
                                                ),
                                            ),
                                        ),
                                        var("mts"),
                                    ),
                                ),
                            ),
                            # Always wrap walrus values in Lazy(...) so the
                            # evaluation is deferred until forced by .get()
                            # at use sites.
                            (
                                "pterm",
                                _local("makeLazy")(var("pbody")),
                            ),
                        ],
                        right(
                            PySyn.named_expression_assignment(
                                PySyn.assignment_expression(
                                    var("pyName"), var("pterm")
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
        doc(
            "Encode a binding as a walrus operator assignment", body
        ),
    )


def _encode_bindings_as_defs():
    body = lambdas(
        ["env", "encodeBinding", "bindings"],
        Eithers.map_list(
            var("encodeBinding")(var("env")),
            var("bindings"),
        ),
    )
    return _def(
        "encodeBindingsAsDefs",
        doc("Encode bindings as function definitions", body),
    )


def _encode_default_case_block():
    body = lambdas(
        ["termToExpr", "isFull", "mdflt", "tname"],
        Eithers.bind(
            Maybes.maybe(
                right(
                    Logic.if_else(
                        var("isFull"),
                        _kref.utils_raise_assertion_error(string("Unreachable: all variants handled")),
                        _kref.utils_raise_type_error(Strings.cat2(
                                string("Unsupported "),
                                _kref.names_local_name_of(var("tname")),
                            )),
                    )
                ),
                lam(
                    "d",
                    Eithers.bind(
                        var("termToExpr")(var("d")),
                        lam(
                            "pyexpr",
                            right(
                                _kref.utils_return_single(var("pyexpr"))
                            ),
                        ),
                    ),
                ),
                var("mdflt"),
            ),
            lam(
                "stmt",
                let_chain(
                    [
                        (
                            "patterns",
                            _kref.utils_py_closed_pattern_to_py_patterns(PySyn.closed_pattern_wildcard),
                        ),
                        (
                            "body",
                            _kref.utils_indented_block(nothing(), list_(
                                    [list_([var("stmt")])]
                                )),
                        ),
                    ],
                    right(
                        list_(
                            [
                                PySyn.case_block(
                                    var("patterns"),
                                    nothing(),
                                    var("body"),
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
        doc(
            "Encode the default (wildcard) case block for a match statement", body
        ),
    )


def _encode_definition():
    term_branch = let_chain(
        [
            ("name", _proj("hydra.packaging.TermDefinition", "name", "td")),
            ("term", _proj("hydra.packaging.TermDefinition", "term", "td")),
            (
                "typ",
                Maybes.maybe(
                    Core.type_scheme(
                        list_([]),
                        Core.type_variable(
                            wrap("hydra.core.Name",
                                string("hydra.core.Unit"),
                            )
                        ),
                        nothing(),
                    ),
                    lam("sig", _kref.scoping_term_signature_to_type_scheme(var("sig"))),
                    _proj("hydra.packaging.TermDefinition", "signature", "td"),
                ),
            ),
        ],
        Eithers.bind(
            _kref.annotations_get_term_description(var("cx"), _local("pythonEnvironmentGetGraph")(var("env")), var("term")),
            lam(
                "comment",
                let_chain(
                    [
                        (
                            "normComment",
                            Maybes.map(
                                _kref.formatting_normalize_comment,
                                var("comment"),
                            ),
                        ),
                    ],
                    Eithers.bind(
                        _local("encodeTermAssignment")(var("cx"), var("env"), true(), var("name"), var("term"), var("typ"), var("normComment")),
                        lam(
                            "stmt",
                            right(
                                list_(
                                    [list_([var("stmt")])]
                                )
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    type_branch = let_chain(
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
            _kref.annotations_get_type_description(var("cx"), _local("pythonEnvironmentGetGraph")(var("env")), var("typ")),
            lam(
                "comment",
                let_chain(
                    [
                        (
                            "normComment",
                            Maybes.map(
                                _kref.formatting_normalize_comment,
                                var("comment"),
                            ),
                        ),
                    ],
                    _local("encodeTypeAssignment")(var("cx"), var("env"), var("name"), var("typ"), var("normComment")),
                ),
            ),
        ),
    )
    body = lambdas(
        ["cx", "env", "def_"],
        cases("hydra.packaging.Definition",
            var("def_"),
            Nothing(),
            [
                field("term",
                    lam("td", term_branch),
                ),
                field("type",
                    lam("td", type_branch),
                ),
            ],
        ),
    )
    return _def(
        "encodeDefinition",
        doc("Encode a definition (term or type) to Python statements", body),
    )


def _encode_enum_value_assignment():
    body = lambdas(
        ["cx", "env", "fieldType"],
        let_chain(
            [
                ("fname", Core.field_type_name(var("fieldType"))),
                ("ftype", Core.field_type_type(var("fieldType"))),
            ],
            Eithers.bind(
                _kref.annotations_get_type_description(var("cx"), _local("pythonEnvironmentGetGraph")(var("env")), var("ftype")),
                lam(
                    "mcomment",
                    let_chain(
                        [
                            (
                                "pyName",
                                _kref.names_encode_enum_value(var("env"), var("fname")),
                            ),
                            ("fnameStr", Core.un_name(var("fname"))),
                            (
                                "pyValue",
                                _kref.utils_function_call(_kref.utils_py_name_to_py_primary(_kref.names_encode_name(true(), _kref.util_case_convention_pascal, var("env"), Core.name(string("hydra.core.Name")))), list_(
                                        [
                                            _kref.utils_double_quoted_string(var("fnameStr"))
                                        ]
                                    )),
                            ),
                            (
                                "assignStmt",
                                _kref.utils_assignment_statement(var("pyName"), var("pyValue")),
                            ),
                        ],
                        right(
                            Maybes.maybe(
                                list_([var("assignStmt")]),
                                lam(
                                    "c",
                                    list_(
                                        [
                                            var("assignStmt"),
                                            _kref.utils_py_expression_to_py_statement(_kref.utils_triple_quoted_string(var("c"))),
                                        ]
                                    ),
                                ),
                                var("mcomment"),
                            )
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeEnumValueAssignment",
        doc(
            "Encode an enum value assignment statement with optional comment", body
        ),
    )


def _encode_field():
    body = lambdas(
        ["cx", "env", "field", "termToExpr"],
        let_chain(
            [
                ("fname", Core.field_name(var("field"))),
                ("fterm", Core.field_term(var("field"))),
            ],
            Eithers.bind(
                var("termToExpr")(var("fterm")),
                lam(
                    "pterm",
                    right(
                        pair(
                            _kref.names_encode_field_name(var("env"), var("fname")),
                            var("pterm"),
                        )
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeField",
        doc(
            "Encode a field (name-value pair) to a Python (Name, Expression) pair", body
        ),
    )


def _encode_field_type():
    body = lambdas(
        ["cx", "env", "fieldType"],
        let_chain(
            [
                ("fname", Core.field_type_name(var("fieldType"))),
                ("ftype", Core.field_type_type(var("fieldType"))),
            ],
            Eithers.bind(
                _kref.annotations_get_type_description(var("cx"), _local("pythonEnvironmentGetGraph")(var("env")), var("ftype")),
                lam(
                    "comment",
                    let_chain(
                        [
                            (
                                "pyName",
                                PySyn.single_target_name(
                                    _kref.names_encode_field_name(var("env"), var("fname"))
                                ),
                            ),
                        ],
                        Eithers.bind(
                            _local("encodeType")(var("env"), var("ftype")),
                            lam(
                                "pyType",
                                let_chain(
                                    [
                                        (
                                            "annotatedPyType",
                                            _kref.utils_annotated_expression(var("comment"), var("pyType")),
                                        ),
                                    ],
                                    right(
                                        _kref.utils_py_assignment_to_py_statement(PySyn.assignment_typed(
                                                PySyn.typed_assignment(
                                                    var("pyName"),
                                                    var("annotatedPyType"),
                                                    nothing(),
                                                )
                                            ))
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
        doc(
            "Encode a field type for record definitions (field: type annotation)",
            body,
        ),
    )


def _encode_float_value():
    body = lambdas(
        ["fv"],
        cases("hydra.core.FloatValue",
            var("fv"),
            Nothing(),
            [
                field("float32",
                    lam(
                        "f",
                        _local("encodeFloatValue_encodeFloat32")(var("f")),
                    ),
                ),
                field("float64",
                    lam(
                        "f",
                        _local("encodeFloatValue_encodeFloat64")(var("f")),
                    ),
                ),
            ],
        ),
    )
    return _def(
        "encodeFloatValue",
        doc("Encode a float value to a Python expression", body),
    )


def _encode_float_value_encode_float32():
    body = lambdas(
        ["v"],
        let_chain(
            [("s", Literals.show_float32(var("v")))],
            Logic.if_else(
                Equality.equal(var("s"), string("NaN")),
                right(
                    _local("encodeFloatValue_pySpecialFloat")(string("nan"))
                ),
                Logic.if_else(
                    Equality.equal(var("s"), string("Infinity")),
                    right(
                        _local("encodeFloatValue_pySpecialFloat")(string("inf"))
                    ),
                    Logic.if_else(
                        Equality.equal(var("s"), string("-Infinity")),
                        right(
                            _local("encodeFloatValue_pySpecialFloat")(string("-inf"))
                        ),
                        right(
                            _kref.utils_py_atom_to_py_expression(PySyn.atom_number(
                                    PySyn.number_float(
                                        Literals.float32_to_float64(var("v"))
                                    )
                                ))
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def("encodeFloatValue_encodeFloat32", body)


def _encode_float_value_encode_float64():
    body = lambdas(
        ["v"],
        let_chain(
            [("s", Literals.show_float64(var("v")))],
            Logic.if_else(
                Equality.equal(var("s"), string("NaN")),
                right(
                    _local("encodeFloatValue_pySpecialFloat")(string("nan"))
                ),
                Logic.if_else(
                    Equality.equal(var("s"), string("Infinity")),
                    right(
                        _local("encodeFloatValue_pySpecialFloat")(string("inf"))
                    ),
                    Logic.if_else(
                        Equality.equal(var("s"), string("-Infinity")),
                        right(
                            _local("encodeFloatValue_pySpecialFloat")(string("-inf"))
                        ),
                        Logic.if_else(
                            Equality.equal(var("s"), string("-0.0")),
                            right(
                                _local("encodeFloatValue_pySpecialFloat")(string("-0.0"))
                            ),
                            right(
                                _kref.utils_py_atom_to_py_expression(PySyn.atom_number(
                                        PySyn.number_float(
                                            var("v")
                                        )
                                    ))
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def("encodeFloatValue_encodeFloat64", body)


def _encode_float_value_py_special_float():
    body = lambdas(
        ["value"],
        _kref.utils_function_call(_kref.utils_py_name_to_py_primary(_py_name("float")), list_(
                [_kref.utils_single_quoted_string(var("value"))]
            )),
    )
    return _def("encodeFloatValue_pySpecialFloat", body)


def _encode_name_constants():
    to_stmt = lam(
        "pair",
        _kref.utils_assignment_statement(Pairs.first(var("pair")), _kref.utils_function_call(_kref.utils_py_name_to_py_primary(_kref.names_encode_name(true(), _kref.util_case_convention_pascal, var("env"), Core.name(string("hydra.core.Name")))), list_(
                    [
                        _kref.utils_double_quoted_string(Core.un_name(Pairs.second(var("pair"))))
                    ]
                ))),
    )
    body = lambdas(
        ["env", "name", "fields"],
        let_chain(
            [
                ("toStmt", to_stmt),
                (
                    "namePair",
                    pair(
                        _kref.names_encode_constant_for_type_name(var("env"), var("name")),
                        var("name"),
                    ),
                ),
                (
                    "fieldPairs",
                    Lists.map(
                        lam(
                            "field",
                            pair(
                                _kref.names_encode_constant_for_field_name(var("env"), var("name"), _proj(
                                        "hydra.core.FieldType",
                                        "name",
                                        "field",
                                    )),
                                _proj("hydra.core.FieldType", "name", "field"),
                            ),
                        ),
                        var("fields"),
                    ),
                ),
            ],
            Lists.map(
                var("toStmt"),
                Lists.cons(var("namePair"), var("fieldPairs")),
            ),
        ),
    )
    return _def(
        "encodeNameConstants",
        doc(
            "Generate name constants for a type as class-level attributes", body
        ),
    )


def _encode_python_module():
    post_defStmts_body = let_chain(
        [
            (
                "meta2",
                Logic.if_else(
                    Logic.and_(
                        Logic.not_(var("isTypeMod")),
                        _local("useInlineTypeParams"),
                    ),
                    _local("setMetaUsesTypeVar")(var("meta0"), false()),
                    var("meta0"),
                ),
            ),
            (
                "meta",
                Logic.if_else(
                    Logic.and_(
                        var("isTypeMod"),
                        Equality.equal(
                            _local("targetPythonVersion"),
                            inject_unit("hydra.python.environment.PythonVersion",
                                Name("python310"),
                            ),
                        ),
                    ),
                    _local("setMetaUsesTypeAlias")(var("meta2"), true()),
                    var("meta2"),
                ),
            ),
            (
                "namespaces",
                _meta_proj("namespaces", "meta0"),
            ),
            (
                "commentStmts",
                Maybes.maybe(
                    list_([]),
                    lam(
                        "c",
                        list_(
                            [
                                _kref.utils_comment_statement(var("c"))
                            ]
                        ),
                    ),
                    Maybes.map(
                        _kref.formatting_normalize_comment,
                        Pkg.module_description(var("mod")),
                    ),
                ),
            ),
            (
                "importStmts",
                _local("moduleImports")(var("namespaces"), var("meta")),
            ),
            (
                "tvars",
                Logic.if_else(
                    Logic.or_(
                        var("isTypeMod"),
                        Logic.not_(_local("useInlineTypeParams")),
                    ),
                    _meta_proj("typeVariables", "meta"),
                    Sets.empty(),
                ),
            ),
            (
                "tvarStmts",
                Lists.map(
                    lam(
                        "tv",
                        _local("tvarStatement")(_kref.names_encode_type_variable(var("tv"))),
                    ),
                    Sets.to_list(var("tvars")),
                ),
            ),
        ],
        let_chain(
            [
                (
                    "body",
                    Lists.filter(
                        lam(
                            "group",
                            Logic.not_(Lists.null(var("group"))),
                        ),
                        Lists.concat(
                            list_(
                                [
                                    list_(
                                        [
                                            var("commentStmts"),
                                            var("importStmts"),
                                            var("tvarStmts"),
                                        ]
                                    ),
                                    var("defStmts"),
                                ]
                            )
                        ),
                    ),
                ),
            ],
            right(PySyn.module(var("body"))),
        ),
    )
    inner_body = Eithers.bind(
        Eithers.map_(
            lam("xs", Lists.concat(var("xs"))),
            Eithers.map_list(
                lam(
                    "d",
                    _local("encodeDefinition")(var("cx"), var("env"), var("d")),
                ),
                var("defs"),
            ),
        ),
        lam("defStmts", post_defStmts_body),
    )
    body = lambdas(
        ["cx", "g", "mod", "defs0"],
        let_chain(
            [
                (
                    "defs",
                    _kref.environment_reorder_defs(var("defs0")),
                ),
                (
                    "meta0",
                    _local("gatherMetadata")(Pkg.module_name(var("mod")), var("defs")),
                ),
                (
                    "namespaces0",
                    _meta_proj("namespaces", "meta0"),
                ),
                (
                    "env0",
                    _local("initialEnvironment")(var("namespaces0"), var("g")),
                ),
                (
                    "isTypeMod",
                    _local("isTypeModuleCheck")(var("defs0")),
                ),
            ],
            _local("withDefinitions")(var("env0"), var("defs"), lam("env", inner_body)),
        ),
    )
    return _def(
        "encodePythonModule",
        doc("Encode a Hydra module to a Python module AST", body),
    )


def _encode_record_type():
    body = lambdas(
        ["cx", "env", "name", "rowType", "comment"],
        Eithers.bind(
            Eithers.map_list(
                _local("encodeFieldType")(var("cx"), var("env")),
                var("rowType"),
            ),
            lam(
                "pyFields",
                let_chain(
                    [
                        (
                            "constStmts",
                            _local("encodeNameConstants")(var("env"), var("name"), var("rowType")),
                        ),
                        (
                            "body",
                            _kref.utils_indented_block(var("comment"), list_(
                                    [
                                        var("pyFields"),
                                        var("constStmts"),
                                    ]
                                )),
                        ),
                        ("boundVars", _env("boundTypeVariables", "env")),
                        ("tparamList", Pairs.first(var("boundVars"))),
                        (
                            "mGenericArg",
                            _local("genericArg")(var("tparamList")),
                        ),
                        (
                            "args",
                            Maybes.maybe(
                                nothing(),
                                lam(
                                    "a",
                                    just(
                                        _kref.utils_py_expressions_to_py_args(list_([var("a")]))
                                    ),
                                ),
                                var("mGenericArg"),
                            ),
                        ),
                        (
                            "decs",
                            just(
                                wrap("hydra.python.syntax.Decorators",
                                    list_([_local("dataclassDecorator")]),
                                )
                            ),
                        ),
                        (
                            "pyName",
                            _kref.names_encode_name(false(), _kref.util_case_convention_pascal, var("env"), var("name")),
                        ),
                        ("noTypeParams", list_([])),
                    ],
                    right(
                        _kref.utils_py_class_definition_to_py_statement(record("hydra.python.syntax.ClassDefinition",
                                [
                                    field("decorators", var("decs")
                                    ),
                                    field("name", var("pyName")
                                    ),
                                    field("typeParams",
                                        var("noTypeParams"),
                                    ),
                                    field("arguments", var("args")
                                    ),
                                    field("body", var("body")
                                    ),
                                ],
                            ))
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeRecordType",
        doc("Encode a record type as a Python dataclass", body),
    )


def _encode_type_def_single():
    body = lambdas(
        ["env", "name", "comment", "typeExpr"],
        let_chain(
            [
                (
                    "pyName",
                    _kref.names_encode_name(false(), _kref.util_case_convention_pascal, var("env"), var("name")),
                ),
                (
                    "tparams",
                    _local("environmentTypeParameters")(var("env")),
                ),
            ],
            list_(
                [
                    _local("typeAliasStatementFor")(var("env"), var("pyName"), var("tparams"), var("comment"), var("typeExpr"))
                ]
            ),
        ),
    )
    return _def(
        "encodeTypeDefSingle",
        doc("Encode a simple type alias definition", body),
    )


def _encode_type_quoted():
    body = lambdas(
        ["env", "typ"],
        Eithers.bind(
            _local("encodeType")(var("env"), var("typ")),
            lam(
                "pytype",
                right(
                    Logic.if_else(
                        Sets.null(
                            _kref.variables_free_variables_in_type(var("typ"))
                        ),
                        var("pytype"),
                        _kref.utils_double_quoted_string(var("hydra.serialization.printExpr")(var("hydra.python.serde.expressionToExpr")(var("pytype")))),
                    )
                ),
            ),
        ),
    )
    return _def(
        "encodeTypeQuoted",
        doc(
            "Encode a type to a Python expression, quoting if the type has free variables",
            body,
        ),
    )


def _encode_term_assignment():
    def fs_proj(field):
        return _proj("hydra.typing.FunctionStructure", field, "fs")

    body = lambdas(
        ["cx", "env", "topLevel", "name", "term", "ts", "comment"],
        Eithers.bind(
            _local("analyzePythonFunction")(var("cx"), var("env"), var("term")),
            lam(
                "fs",
                let_chain(
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
                                var("name"),
                                var("term"),
                                just(var("ts")),
                            ),
                        ),
                        (
                            "isComplex",
                            _kref.predicates_is_complex_binding(var("tc"), var("binding")),
                        ),
                        (
                            "isTrivial",
                            _kref.predicates_is_trivial_term(var("term")),
                        ),
                    ],
                    Logic.if_else(
                        Logic.and_(
                            var("isComplex"),
                            Logic.not_(var("isTrivial")),
                        ),
                        Logic.if_else(
                            Logic.and_(
                                Logic.not_(var("topLevel")),
                                Lists.null(var("params")),
                            ),
                            # Inner zero-arg thunk: emit `name = Lazy(lambda: <body>)`.
                            # Use sites emit `name()` which dispatches to Lazy.get()
                            # via __call__.
                            Eithers.bind(
                                _local("encodeTermInline")(var("cx"), var("env2"), false(), var("term")),
                                lam(
                                    "bodyExpr",
                                    let_chain(
                                        [
                                            (
                                                "pyName",
                                                _kref.names_encode_name(false(), _kref.util_case_convention_lower_snake, var("env2"), var("name")),
                                            ),
                                            (
                                                "lazyExpr",
                                                _local("makeLazy")(var("bodyExpr")),
                                            ),
                                        ],
                                        right(
                                            _kref.utils_annotated_statement(var("comment"), _kref.utils_assignment_statement(var("pyName"), var("lazyExpr")))
                                        ),
                                    ),
                                ),
                            ),
                            # Top-level OR has args: emit normal def via functionDefinitionToExpr
                            Eithers.bind(
                                Eithers.map_list(
                                    _local("encodeBindingAs")(var("cx"), var("env2")),
                                    var("bindings"),
                                ),
                                lam(
                                    "bindingStmts",
                                    _local("functionDefinitionToExpr")(var("cx"), var("env2"), var("name"), var("tparams"), var("params"), var("body"), var("doms"), var("mcod"), var("comment"), var("bindingStmts")),
                                ),
                            ),
                        ),
                        Eithers.bind(
                            _local("encodeTermInline")(var("cx"), var("env2"), false(), var("body")),
                            lam(
                                "bodyExpr",
                                let_chain(
                                    [
                                        (
                                            "pyName",
                                            _kref.names_encode_name(false(), _kref.util_case_convention_lower_snake, var("env2"), var("name")),
                                        ),
                                    ],
                                    right(
                                        _kref.utils_annotated_statement(var("comment"), _kref.utils_assignment_statement(var("pyName"), var("bodyExpr")))
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
        doc("Encode a term assignment to a Python statement", body),
    )


def _encode_term_inline():
    def fs_proj(field):
        return _proj("hydra.typing.FunctionStructure", field, "fs")

    encode_helper = lam(
        "t",
        _local("encodeTermInline")(var("cx"), var("env"), false(), var("t")),
    )
    strip_type_apps = lam(
        "t",
        cases_with_default("hydra.core.Term", var("t"), var("t"),
            field("annotated",
                    lam(
                        "ann",
                        var("stripTypeApps")(Core.annotated_term_body(var("ann"))),
                    ),
                ),
            field("typeApplication",
                    lam(
                        "ta",
                        var("stripTypeApps")(Core.type_application_term_body(var("ta"))),
                    ),
                )),
    )
    with_cast = lam(
        "pyexp",
        Logic.if_else(
            Logic.or_(
                var("noCast"),
                _env("skipCasts", "env"),
            ),
            right(var("pyexp")),
            let_chain(
                [
                    ("tc", _env("graph", "env")),
                    (
                        "mtyp",
                        Eithers.map_(
                            lam("_r", Pairs.first(var("_r"))),
                            var("hydra.checking.typeOf")(var("cx"), var("tc"), list_([]), var("term")),
                        ),
                    ),
                ],
                Eithers.either(
                    constant(right(var("pyexp"))),
                    lam(
                        "typ",
                        Eithers.either(
                            constant(right(var("pyexp"))),
                            lam(
                                "pytyp",
                                right(
                                    var("hydra.python.utils.castTo")(var("pytyp"), var("pyexp"))
                                ),
                            ),
                            _local("encodeType")(var("env"), var("typ")),
                        ),
                    ),
                    var("mtyp"),
                ),
            ),
        ),
    )

    # 16 case dispatch fields
    application_branch = lam(
        "app",
        _local("encodeApplication")(var("cx"), var("env"), var("app")),
    )
    either_branch = lam(
        "et",
        Eithers.either(
            lam(
                "t1",
                Eithers.bind(
                    var("encode")(var("t1")),
                    lam(
                        "pyexp",
                        var("withCast")(_kref.utils_function_call(_kref.utils_py_name_to_py_primary(_py_name("Left")), list_([var("pyexp")]))),
                    ),
                ),
            ),
            lam(
                "t1",
                Eithers.bind(
                    var("encode")(var("t1")),
                    lam(
                        "pyexp",
                        var("withCast")(_kref.utils_function_call(_kref.utils_py_name_to_py_primary(_py_name("Right")), list_([var("pyexp")]))),
                    ),
                ),
            ),
            var("et"),
        ),
    )
    lambda_branch = lam(
        "lam",
        Eithers.bind(
            _local("analyzePythonFunction")(var("cx"), var("env"), Core.term_lambda(var("lam"))),
            lam(
                "fs",
                let_chain(
                    [
                        ("params", fs_proj("params")),
                        ("bindings", fs_proj("bindings")),
                        ("innerBody", fs_proj("body")),
                        ("innerEnv0", fs_proj("environment")),
                        (
                            "bindingNames",
                            Lists.map(
                                lam(
                                    "b", Core.binding_name(var("b"))
                                ),
                                var("bindings"),
                            ),
                        ),
                        (
                            "innerEnv",
                            record("hydra.python.environment.PythonEnvironment",
                                [
                                    field("namespaces",
                                        _env("namespaces", "innerEnv0"),
                                    ),
                                    field("boundTypeVariables",
                                        _env("boundTypeVariables", "innerEnv0"),
                                    ),
                                    field("graph", _env("graph", "innerEnv0")
                                    ),
                                    field("nullaryBindings",
                                        _env("nullaryBindings", "innerEnv0"),
                                    ),
                                    field("version",
                                        _env("version", "innerEnv0"),
                                    ),
                                    field("skipCasts",
                                        _env("skipCasts", "innerEnv0"),
                                    ),
                                    field("inlineVariables",
                                        Sets.union(
                                            Sets.from_list(
                                                var("bindingNames")
                                            ),
                                            _env("inlineVariables", "innerEnv0"),
                                        ),
                                    ),
                                ],
                            ),
                        ),
                    ],
                    Eithers.bind(
                        _local("encodeTermInline")(var("cx"), var("innerEnv"), false(), var("innerBody")),
                        lam(
                            "pbody",
                            let_chain(
                                [
                                    (
                                        "pparams",
                                        Lists.map(
                                            _kref.names_encode_name(false(), _kref.util_case_convention_lower_snake, var("innerEnv")),
                                            var("params"),
                                        ),
                                    ),
                                ],
                                Logic.if_else(
                                    Lists.null(var("bindings")),
                                    right(
                                        _local("makeUncurriedLambda")(var("pparams"), var("pbody"))
                                    ),
                                    Eithers.bind(
                                        Eithers.map_list(
                                            _local("encodeBindingAsAssignment")(var("cx"), false(), var("innerEnv")),
                                            var("bindings"),
                                        ),
                                        lam(
                                            "pbindingExprs",
                                            let_chain(
                                                [
                                                    (
                                                        "pbindingStarExprs",
                                                        Lists.map(
                                                            lam(
                                                                "ne",
                                                                PySyn.star_named_expression_simple(
                                                                    var("ne")
                                                                ),
                                                            ),
                                                            var("pbindingExprs"),
                                                        ),
                                                    ),
                                                    (
                                                        "pbodyStarExpr",
                                                        var(
                                                                "hydra.python.utils.pyExpressionToPyStarNamedExpression"
                                                            )(var("pbody")),
                                                    ),
                                                    (
                                                        "tupleElements",
                                                        Lists.concat2(
                                                            var("pbindingStarExprs"),
                                                            list_(
                                                                [
                                                                    var("pbodyStarExpr")
                                                                ]
                                                            ),
                                                        ),
                                                    ),
                                                    (
                                                        "tupleExpr",
                                                        _kref.utils_py_atom_to_py_expression(PySyn.atom_tuple(
                                                                getattr(PySyn, "tuple")(
                                                                    var("tupleElements")
                                                                )
                                                            )),
                                                    ),
                                                    (
                                                        "indexValue",
                                                        _kref.utils_py_atom_to_py_expression(PySyn.atom_number(
                                                                PySyn.number_integer(
                                                                    Literals.int32_to_bigint(
                                                                        Lists.length(
                                                                            var("bindings")
                                                                        )
                                                                    )
                                                                )
                                                            )),
                                                    ),
                                                    (
                                                        "indexedExpr",
                                                        _kref.utils_primary_with_expression_slices(_kref.utils_py_expression_to_py_primary(var("tupleExpr")), list_(
                                                                [var("indexValue")]
                                                            )),
                                                    ),
                                                ],
                                                right(
                                                    _local("makeUncurriedLambda")(var("pparams"), _kref.utils_py_primary_to_py_expression(var("indexedExpr")))
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
    project_branch = lam(
        "proj",
        let_chain(
            [
                ("fname", _proj("hydra.core.Projection", "fieldName", "proj")),
            ],
            right(
                _local("makeCurriedLambda")(list_([_py_name("v1")]), _kref.utils_project_from_expression(PyDsl.py_name_to_py_expression(_py_name("v1")), _kref.names_encode_field_name(var("env"), var("fname"))))
            ),
        ),
    )
    unwrap_branch = constant(
        right(
            _local("makeCurriedLambda")(list_([_py_name("v1")]), _kref.utils_project_from_expression(PyDsl.py_name_to_py_expression(_py_name("v1")), _py_name("value")))
        )
    )
    cases_branch_unsupported = constant(
        right(
            _local("unsupportedExpression")(string(
                    "case expressions as values are not yet supported"
                ))
        )
    )
    let_branch = lam(
        "lt",
        let_chain(
            [
                ("bindings", Core.let_bindings(var("lt"))),
                ("body", Core.let_body(var("lt"))),
            ],
            Logic.if_else(
                Lists.null(var("bindings")),
                _local("encodeTermInline")(var("cx"), var("env"), false(), var("body")),
                _local("withLetInline")(var("env"), var("lt"), lam(
                        "innerEnv",
                        Eithers.bind(
                            Eithers.map_list(
                                _local("encodeBindingAsAssignment")(var("cx"), false(), var("innerEnv")),
                                var("bindings"),
                            ),
                            lam(
                                "pbindingExprs",
                                Eithers.bind(
                                    _local("encodeTermInline")(var("cx"), var("innerEnv"), false(), var("body")),
                                    lam(
                                        "pbody",
                                        let_chain(
                                            [
                                                (
                                                    "pbindingStarExprs",
                                                    Lists.map(
                                                        lam(
                                                            "ne",
                                                            PySyn.star_named_expression_simple(
                                                                var("ne")
                                                            ),
                                                        ),
                                                        var("pbindingExprs"),
                                                    ),
                                                ),
                                                (
                                                    "pbodyStarExpr",
                                                    var(
                                                            "hydra.python.utils.pyExpressionToPyStarNamedExpression"
                                                        )(var("pbody")),
                                                ),
                                                (
                                                    "tupleElements",
                                                    Lists.concat2(
                                                        var("pbindingStarExprs"),
                                                        list_(
                                                            [var("pbodyStarExpr")]
                                                        ),
                                                    ),
                                                ),
                                                (
                                                    "tupleExpr",
                                                    _kref.utils_py_atom_to_py_expression(PySyn.atom_tuple(
                                                            getattr(PySyn, "tuple")(
                                                                var("tupleElements")
                                                            )
                                                        )),
                                                ),
                                                (
                                                    "indexValue",
                                                    _kref.utils_py_atom_to_py_expression(PySyn.atom_number(
                                                            PySyn.number_integer(
                                                                Literals.int32_to_bigint(
                                                                    Lists.length(
                                                                        var("bindings")
                                                                    )
                                                                )
                                                            )
                                                        )),
                                                ),
                                                (
                                                    "indexedExpr",
                                                    _kref.utils_primary_with_expression_slices(_kref.utils_py_expression_to_py_primary(var("tupleExpr")), list_(
                                                            [var("indexValue")]
                                                        )),
                                                ),
                                            ],
                                            right(
                                                _kref.utils_py_primary_to_py_expression(var("indexedExpr"))
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    )),
            ),
        ),
    )
    # Encode as ConsList.of(items...). Public types stay Sequence; ConsList
    # implements Sequence so callers see the abstract type but get the
    # structurally-shared persistent value.
    list_branch = lam(
        "terms",
        Eithers.bind(
            Eithers.map_list(var("encode"), var("terms")),
            lam(
                "pyExprs",
                let_chain(
                    [
                        (
                            "consListOf",
                            _kref.utils_project_from_expression(_kref.utils_py_name_to_py_expression(_py_name("ConsList")), _py_name("of")),
                        ),
                    ],
                    right(
                        _kref.utils_function_call(_kref.utils_py_expression_to_py_primary(var("consListOf")), var("pyExprs"))
                    ),
                ),
            ),
        ),
    )
    literal_branch = lam(
        "lit", _local("encodeLiteral")(var("lit"))
    )
    # Encode as PersistentMap.of_entries((k, v), ...). Public types stay
    # Mapping[K, V]; PersistentMap is a Mapping so callers see the abstract
    # type but get the structurally-shared persistent value.
    map_branch = lam(
        "m",
        Eithers.bind(
            Eithers.map_list(
                lam(
                    "kv",
                    let_chain(
                        [
                            ("k", Pairs.first(var("kv"))),
                            ("v", Pairs.second(var("kv"))),
                        ],
                        Eithers.bind(
                            var("encode")(var("k")),
                            lam(
                                "pyK",
                                Eithers.bind(
                                    var("encode")(var("v")),
                                    lam(
                                        "pyV",
                                        right(
                                            _kref.utils_py_atom_to_py_expression(PySyn.atom_tuple(
                                                    getattr(PySyn, "tuple")(
                                                        list_(
                                                            [
                                                                _kref.utils_py_expression_to_py_star_named_expression(var("pyK")),
                                                                _kref.utils_py_expression_to_py_star_named_expression(var("pyV")),
                                                            ]
                                                        )
                                                    )
                                                ))
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
                Maps.to_list(var("m")),
            ),
            lam(
                "tuplePairs",
                let_chain(
                    [
                        (
                            "pmapOfEntries",
                            _kref.utils_project_from_expression(_kref.utils_py_name_to_py_expression(_py_name("PersistentMap")), _py_name("of_entries")),
                        ),
                    ],
                    right(
                        _kref.utils_function_call(_kref.utils_py_expression_to_py_primary(var("pmapOfEntries")), var("tuplePairs"))
                    ),
                ),
            ),
        ),
    )
    maybe_branch = lam(
        "mt",
        Maybes.maybe(
            right(
                _kref.utils_function_call(_kref.utils_py_name_to_py_primary(_py_name("Nothing")), list_([]))
            ),
            lam(
                "t1",
                Eithers.bind(
                    var("encode")(var("t1")),
                    lam(
                        "pyexp",
                        var("withCast")(_kref.utils_function_call(_kref.utils_py_name_to_py_primary(_py_name("Just")), list_([var("pyexp")]))),
                    ),
                ),
            ),
            var("mt"),
        ),
    )
    pair_branch = lam(
        "p",
        let_chain(
            [
                ("t1", Pairs.first(var("p"))),
                ("t2", Pairs.second(var("p"))),
            ],
            Eithers.bind(
                var("encode")(var("t1")),
                lam(
                    "pyExpr1",
                    Eithers.bind(
                        var("encode")(var("t2")),
                        lam(
                            "pyExpr2",
                            right(
                                _kref.utils_py_atom_to_py_expression(PySyn.atom_tuple(
                                        getattr(PySyn, "tuple")(
                                            list_(
                                                [
                                                    var(
                                                            "hydra.python.utils.pyExpressionToPyStarNamedExpression"
                                                        )(var("pyExpr1")),
                                                    var(
                                                            "hydra.python.utils.pyExpressionToPyStarNamedExpression"
                                                        )(var("pyExpr2")),
                                                ]
                                            )
                                        )
                                    ))
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
    record_branch = lam(
        "r",
        let_chain(
            [
                ("tname", Core.record_type_name(var("r"))),
                ("fields", Core.record_fields(var("r"))),
            ],
            Eithers.bind(
                Eithers.map_list(
                    lam(
                        "fld",
                        var("encode")(Core.field_term(var("fld"))),
                    ),
                    var("fields"),
                ),
                lam(
                    "pargs",
                    right(
                        _kref.utils_function_call(_kref.utils_py_name_to_py_primary(_kref.names_encode_name_qualified(var("env"), var("tname"))), var("pargs"))
                    ),
                ),
            ),
        ),
    )
    # Encode as PersistentSet.of(...). Public types stay AbstractSet[E];
    # PersistentSet implements Set so callers see the abstract type but get
    # the structurally-shared persistent value.
    set_branch = lam(
        "s",
        Eithers.bind(
            Eithers.map_list(
                var("encode"), Sets.to_list(var("s"))
            ),
            lam(
                "pyEls",
                let_chain(
                    [
                        (
                            "psetOf",
                            _kref.utils_project_from_expression(_kref.utils_py_name_to_py_expression(_py_name("PersistentSet")), _py_name("of")),
                        ),
                    ],
                    right(
                        _kref.utils_function_call(_kref.utils_py_expression_to_py_primary(var("psetOf")), var("pyEls"))
                    ),
                ),
            ),
        ),
    )
    type_application_branch = lam(
        "ta",
        let_chain(
            [
                ("body", Core.type_application_term_body(var("ta"))),
            ],
            Eithers.bind(
                _local("encodeTermInline")(var("cx"), var("env"), true(), var("stripTypeApps")(var("body"))),
                lam(
                    "pybase", var("withCast")(var("pybase"))
                ),
            ),
        ),
    )
    type_lambda_branch = lam(
        "tl",
        let_chain(
            [
                ("body", Core.type_lambda_body(var("tl"))),
            ],
            _local("withTypeLambda")(var("env"), var("tl"), lam(
                    "env2",
                    _local("encodeTermInline")(var("cx"), var("env2"), var("noCast"), var("body")),
                )),
        ),
    )
    inject_branch = lam(
        "inj",
        let_chain(
            [
                ("tname", Core.injection_type_name(var("inj"))),
                ("field", Core.injection_field(var("inj"))),
            ],
            Eithers.bind(
                _kref.resolution_require_union_type(var("cx"), _local("pythonEnvironmentGetGraph")(var("env")), var("tname")),
                lam(
                    "rt",
                    Logic.if_else(
                        _kref.predicates_is_enum_row_type(var("rt")),
                        right(
                            _kref.utils_project_from_expression(_kref.utils_py_name_to_py_expression(_kref.names_encode_name_qualified(var("env"), var("tname"))), _kref.names_encode_enum_value(var("env"), Core.field_name(var("field"))))
                        ),
                        let_chain(
                            [
                                ("fname", Core.field_name(var("field"))),
                                (
                                    "isUnitVariant",
                                    Maybes.maybe(
                                        false(),
                                        lam(
                                            "ft",
                                            _kref.predicates_is_unit_type(_kref.strip_deannotate_type(Core.field_type_type(
                                                        var("ft")
                                                    ))),
                                        ),
                                        Lists.find(
                                            lam(
                                                "ft",
                                                MetaCore.equal_name(
                                                    Core.field_type_name(
                                                        var("ft")
                                                    ),
                                                    var("fname"),
                                                ),
                                            ),
                                            var("rt"),
                                        ),
                                    ),
                                ),
                            ],
                            Eithers.bind(
                                Logic.if_else(
                                    Logic.or_(
                                        _kref.predicates_is_unit_term(Core.field_term(var("field"))),
                                        var("isUnitVariant"),
                                    ),
                                    right(list_([])),
                                    Eithers.bind(
                                        var("encode")(Core.field_term(var("field"))),
                                        lam(
                                            "parg",
                                            right(
                                                list_(
                                                    [var("parg")]
                                                )
                                            ),
                                        ),
                                    ),
                                ),
                                lam(
                                    "args",
                                    let_chain(
                                        [
                                            (
                                                "deconflictedName",
                                                _local("deconflictVariantName")(true(), var("env"), var("tname"), var("fname"), _env("graph", "env")),
                                            ),
                                        ],
                                        right(
                                            var(
                                                    "hydra.python.utils.castTo"
                                                )(_kref.names_type_variable_reference(var("env"), var("tname")), _kref.utils_function_call(_kref.utils_py_name_to_py_primary(var("deconflictedName")), var("args")))
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
    unit_branch = constant(
        right(
            _kref.utils_py_name_to_py_expression(_kref.utils_py_none)
        )
    )
    variable_branch = lam(
        "name",
        _local("encodeVariable")(var("cx"), var("env"), var("name"), list_([])),
    )
    wrap_branch = lam(
        "wrapped",
        let_chain(
            [
                ("tname", Core.wrapped_term_type_name(var("wrapped"))),
                ("inner", Core.wrapped_term_body(var("wrapped"))),
            ],
            Eithers.bind(
                var("encode")(var("inner")),
                lam(
                    "parg",
                    right(
                        _kref.utils_function_call(_kref.utils_py_name_to_py_primary(_kref.names_encode_name_qualified(var("env"), var("tname"))), list_([var("parg")]))
                    ),
                ),
            ),
        ),
    )

    body = lambdas(
        ["cx", "env", "noCast", "term"],
        let_chain(
            [
                ("encode", encode_helper),
                ("stripTypeApps", strip_type_apps),
                ("withCast", with_cast),
            ],
            cases("hydra.core.Term",
                _kref.strip_deannotate_and_detype_term(var("term")),
                Nothing(),
                [
                    field("application", application_branch),
                    field("either", either_branch),
                    field("lambda", lambda_branch),
                    field("project", project_branch),
                    field("unwrap", unwrap_branch),
                    field("cases", cases_branch_unsupported),
                    field("let", let_branch),
                    field("list", list_branch),
                    field("literal", literal_branch),
                    field("map", map_branch),
                    field("maybe", maybe_branch),
                    field("pair", pair_branch),
                    field("record", record_branch),
                    field("set", set_branch),
                    field("typeApplication", type_application_branch),
                    field("typeLambda", type_lambda_branch),
                    field("inject", inject_branch),
                    field("unit", unit_branch),
                    field("variable", variable_branch),
                    field("wrap", wrap_branch),
                ],
            ),
        ),
    )
    return _def(
        "encodeTermInline",
        doc("Encode a term to a Python expression (inline form)", body),
    )


def _encode_term_multiline():
    def fs_proj(field):
        return _proj("hydra.typing.FunctionStructure", field, "fs")

    dflt_logic = Eithers.bind(
        _local("analyzePythonFunction")(var("cx"), var("env"), var("term")),
        lam(
            "fs",
            let_chain(
                [
                    ("params", fs_proj("params")),
                    ("bindings", fs_proj("bindings")),
                    ("innerBody", fs_proj("body")),
                    ("env2", fs_proj("environment")),
                ],
                Logic.if_else(
                    Lists.null(var("bindings")),
                    Eithers.bind(
                        _local("encodeTermInline")(var("cx"), var("env"), false(), var("term")),
                        lam(
                            "expr",
                            right(
                                list_(
                                    [
                                        _kref.utils_return_single(var("expr"))
                                    ]
                                )
                            ),
                        ),
                    ),
                    Eithers.bind(
                        Eithers.map_list(
                            _local("encodeBindingAs")(var("cx"), var("env2")),
                            var("bindings"),
                        ),
                        lam(
                            "bindingStmts",
                            Eithers.bind(
                                _local("encodeTermMultiline")(var("cx"), var("env2"), var("innerBody")),
                                lam(
                                    "bodyStmts",
                                    right(
                                        Lists.concat2(
                                            var("bindingStmts"),
                                            var("bodyStmts"),
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
    cases_branch = lam(
        "cs",
        let_chain(
            [
                ("tname", Core.case_statement_type_name(var("cs"))),
                ("dflt", Core.case_statement_default(var("cs"))),
                ("cases_", Core.case_statement_cases(var("cs"))),
            ],
            Eithers.bind(
                _kref.resolution_require_union_type(var("cx"), _local("pythonEnvironmentGetGraph")(var("env")), var("tname")),
                lam(
                    "rt",
                    let_chain(
                        [
                            (
                                "isEnum",
                                _kref.predicates_is_enum_row_type(var("rt")),
                            ),
                            (
                                "isFull",
                                _local("isCasesFull")(var("rt"), var("cases_")),
                            ),
                        ],
                        Eithers.bind(
                            _local("encodeTermInline")(var("cx"), var("env"), false(), var("arg")),
                            lam(
                                "pyArg",
                                Eithers.bind(
                                    Eithers.map_list(
                                        _local("caseBlockToExpr")(var("cx"), var("env"), var("tname"), var("rt"), var("isEnum"), lambdas(
                                                ["e", "t"],
                                                _local("encodeTermMultiline")(var("cx"), var("e"), var("t")),
                                            )),
                                        _local("deduplicateCaseVariables")(var("cases_")),
                                    ),
                                    lam(
                                        "pyCases",
                                        Eithers.bind(
                                            _local("encodeDefaultCaseBlock")(lam(
                                                    "t",
                                                    _local("encodeTermInline")(var("cx"), var("env"), false(), var("t")),
                                                ), var("isFull"), var("dflt"), var("tname")),
                                            lam(
                                                "pyDflt",
                                                let_chain(
                                                    [
                                                        (
                                                            "subj",
                                                            PySyn.subject_expression_simple(
                                                                PySyn.named_expression_simple(
                                                                    var("pyArg")
                                                                )
                                                            ),
                                                        ),
                                                        (
                                                            "matchStmt",
                                                            PySyn.statement_compound(
                                                                PySyn.compound_statement_match(
                                                                    record(
                                                                        Name(
                                                                            "hydra.python.syntax.MatchStatement"
                                                                        ),
                                                                        [
                                                                            field("subject",
                                                                                var(
                                                                                    "subj"
                                                                                ),
                                                                            ),
                                                                            field("cases",
                                                                                Lists.concat2(
                                                                                    var(
                                                                                        "pyCases"
                                                                                    ),
                                                                                    var(
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
                                                    right(
                                                        list_(
                                                            [var("matchStmt")]
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
    body = lambdas(
        ["cx", "env", "term"],
        let_chain(
            [
                ("dfltLogic", dflt_logic),
                (
                    "gathered",
                    _kref.analysis_gather_applications(var("term")),
                ),
                ("args", Pairs.first(var("gathered"))),
                ("body", Pairs.second(var("gathered"))),
            ],
            Logic.if_else(
                Equality.equal(
                    Lists.length(var("args")), int32(1)
                ),
                let_chain(
                    [
                        (
                            "arg",
                            Maybes.from_maybe(
                                Core.term_unit,
                                Lists.maybe_head(var("args")),
                            ),
                        ),
                    ],
                    cases_with_default("hydra.core.Term", _kref.strip_deannotate_and_detype_term(var("body")), var("dfltLogic"),
            field("cases", cases_branch)),
                ),
                var("dfltLogic"),
            ),
        ),
    )
    return _def(
        "encodeTermMultiline",
        doc(
            "Encode a term to a list of statements with return as final statement",
            body,
        ),
    )


def _encode_term_multiline_tco():
    cases_branch = lam(
        "cs",
        let_chain(
            [
                ("tname", Core.case_statement_type_name(var("cs"))),
                ("dflt", Core.case_statement_default(var("cs"))),
                ("cases_", Core.case_statement_cases(var("cs"))),
            ],
            Eithers.bind(
                _kref.resolution_require_union_type(var("cx"), _local("pythonEnvironmentGetGraph")(var("env")), var("tname")),
                lam(
                    "rt",
                    let_chain(
                        [
                            (
                                "isEnum",
                                _kref.predicates_is_enum_row_type(var("rt")),
                            ),
                            (
                                "isFull",
                                _local("isCasesFull")(var("rt"), var("cases_")),
                            ),
                        ],
                        Eithers.bind(
                            _local("encodeTermInline")(var("cx"), var("env"), false(), var("arg")),
                            lam(
                                "pyArg",
                                Eithers.bind(
                                    Eithers.map_list(
                                        _local("caseBlockToExpr")(var("cx"), var("env"), var("tname"), var("rt"), var("isEnum"), lambdas(
                                                ["e2", "t2"],
                                                _local("encodeTermMultilineTCO")(var("cx"), var("e2"), var("funcName"), var("paramNames"), var("t2")),
                                            )),
                                        _local("deduplicateCaseVariables")(var("cases_")),
                                    ),
                                    lam(
                                        "pyCases",
                                        Eithers.bind(
                                            _local("encodeDefaultCaseBlock")(lam(
                                                    "t2",
                                                    _local("encodeTermInline")(var("cx"), var("env"), false(), var("t2")),
                                                ), var("isFull"), var("dflt"), var("tname")),
                                            lam(
                                                "pyDflt",
                                                let_chain(
                                                    [
                                                        (
                                                            "subj",
                                                            PySyn.subject_expression_simple(
                                                                PySyn.named_expression_simple(
                                                                    var("pyArg")
                                                                )
                                                            ),
                                                        ),
                                                        (
                                                            "matchStmt",
                                                            PySyn.statement_compound(
                                                                PySyn.compound_statement_match(
                                                                    record(
                                                                        Name(
                                                                            "hydra.python.syntax.MatchStatement"
                                                                        ),
                                                                        [
                                                                            field("subject",
                                                                                var(
                                                                                    "subj"
                                                                                ),
                                                                            ),
                                                                            field("cases",
                                                                                Lists.concat2(
                                                                                    var(
                                                                                        "pyCases"
                                                                                    ),
                                                                                    var(
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
                                                    right(
                                                        list_(
                                                            [var("matchStmt")]
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
    not_self_call_branch = let_chain(
        [
            (
                "gathered2",
                _kref.analysis_gather_applications(var("term")),
            ),
            ("args2", Pairs.first(var("gathered2"))),
            ("body2", Pairs.second(var("gathered2"))),
        ],
        Logic.if_else(
            Equality.equal(
                Lists.length(var("args2")), int32(1)
            ),
            let_chain(
                [
                    (
                        "arg",
                        Maybes.from_maybe(
                            Core.term_unit,
                            Lists.maybe_head(var("args2")),
                        ),
                    ),
                ],
                cases_with_default("hydra.core.Term", _kref.strip_deannotate_and_detype_term(var("body2")), Eithers.bind(
                            _local("encodeTermInline")(var("cx"), var("env"), false(), var("term")),
                            lam(
                                "expr",
                                right(
                                    list_(
                                        [
                                            _kref.utils_return_single(var("expr"))
                                        ]
                                    )
                                ),
                            ),
                        ),
            field("cases", cases_branch)),
            ),
            Eithers.bind(
                _local("encodeTermInline")(var("cx"), var("env"), false(), var("term")),
                lam(
                    "expr",
                    right(
                        list_(
                            [
                                _kref.utils_return_single(var("expr"))
                            ]
                        )
                    ),
                ),
            ),
        ),
    )
    body = lambdas(
        ["cx", "env", "funcName", "paramNames", "term"],
        let_chain(
            [
                (
                    "stripped",
                    _kref.strip_deannotate_and_detype_term(var("term")),
                ),
                (
                    "gathered",
                    _kref.analysis_gather_applications(var("stripped")),
                ),
                ("gatherArgs", Pairs.first(var("gathered"))),
                ("gatherFun", Pairs.second(var("gathered"))),
                (
                    "strippedFun",
                    _kref.strip_deannotate_and_detype_term(var("gatherFun")),
                ),
                (
                    "isSelfCall",
                    cases_with_default("hydra.core.Term", var("strippedFun"), false(),
            field("variable",
                                lam(
                                    "n",
                                    Equality.equal(
                                        var("n"), var("funcName")
                                    ),
                                ),
                            )),
                ),
            ],
            Logic.if_else(
                Logic.and_(
                    var("isSelfCall"),
                    Equality.equal(
                        Lists.length(var("gatherArgs")),
                        Lists.length(var("paramNames")),
                    ),
                ),
                Eithers.bind(
                    Eithers.map_list(
                        lam(
                            "a",
                            _local("encodeTermInline")(var("cx"), var("env"), false(), var("a")),
                        ),
                        var("gatherArgs"),
                    ),
                    lam(
                        "pyArgs",
                        let_chain(
                            [
                                (
                                    "assignments",
                                    Lists.map(
                                        lam(
                                            "pair",
                                            let_chain(
                                                [
                                                    (
                                                        "paramName",
                                                        Pairs.first(
                                                            var("pair")
                                                        ),
                                                    ),
                                                    (
                                                        "pyArg",
                                                        Pairs.second(
                                                            var("pair")
                                                        ),
                                                    ),
                                                ],
                                                _kref.utils_assignment_statement(_kref.names_encode_name(false(), _kref.util_case_convention_lower_snake, var("env"), var("paramName")), var("pyArg")),
                                            ),
                                        ),
                                        Lists.zip(
                                            var("paramNames"),
                                            var("pyArgs"),
                                        ),
                                    ),
                                ),
                                (
                                    "continueStmt",
                                    PySyn.statement_simple(
                                        list_(
                                            [PySyn.simple_statement_continue]
                                        )
                                    ),
                                ),
                            ],
                            right(
                                Lists.concat2(
                                    var("assignments"),
                                    list_([var("continueStmt")]),
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
        doc(
            "Encode a term body for TCO: tail self-calls become param reassignment + continue",
            body,
        ),
    )


def _encode_type():
    def call_self(arg_var):
        return _local("encodeType")(var("env"), var(arg_var))

    def name_params(name, args_list):
        return _kref.utils_name_and_params(_py_name(name), args_list)

    def primary_with_expr_slices_inline_named(name, args_list):
        return _kref.utils_py_primary_to_py_expression(_kref.utils_primary_with_expression_slices(PySyn.primary_simple(PySyn.atom_name(_py_name(name))), args_list))

    fields = [
        field("application",
            lam(
                "at",
                _local("encodeApplicationType")(var("env"), var("at")),
            ),
        ),
        field("function",
            lam(
                "ft",
                _local("encodeFunctionType")(var("env"), var("ft")),
            ),
        ),
        field("forall",
            lam(
                "lt",
                _local("encodeForallType")(var("env"), var("lt")),
            ),
        ),
        field("list",
            lam(
                "et",
                Eithers.bind(
                    call_self("et"),
                    lam(
                        "pyet",
                        right(
                            name_params(
                                "Sequence",
                                list_([var("pyet")]),
                            )
                        ),
                    ),
                ),
            ),
        ),
        field("map",
            lam(
                "mt",
                Eithers.bind(
                    _local("encodeType")(var("env"), _proj("hydra.core.MapType", "keys", "mt")),
                    lam(
                        "pykt",
                        Eithers.bind(
                            _local("encodeType")(var("env"), _proj("hydra.core.MapType", "values", "mt")),
                            lam(
                                "pyvt",
                                right(
                                    name_params(
                                        "Mapping",
                                        list_(
                                            [
                                                var("pykt"),
                                                var("pyvt"),
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
        field("literal",
            lam(
                "lt",
                _local("encodeLiteralType")(var("lt")),
            ),
        ),
        field("maybe",
            lam(
                "et",
                Eithers.bind(
                    call_self("et"),
                    lam(
                        "ptype",
                        right(
                            primary_with_expr_slices_inline_named(
                                "Maybe",
                                list_([var("ptype")]),
                            )
                        ),
                    ),
                ),
            ),
        ),
        field("either",
            lam(
                "eitherT",
                Eithers.bind(
                    _local("encodeType")(var("env"), _proj("hydra.core.EitherType", "left", "eitherT")),
                    lam(
                        "pyleft",
                        Eithers.bind(
                            _local("encodeType")(var("env"), _proj("hydra.core.EitherType", "right", "eitherT")),
                            lam(
                                "pyright",
                                right(
                                    primary_with_expr_slices_inline_named(
                                        "Either",
                                        list_(
                                            [
                                                var("pyleft"),
                                                var("pyright"),
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
        field("pair",
            lam(
                "pairT",
                Eithers.bind(
                    _local("encodeType")(var("env"), _proj("hydra.core.PairType", "first", "pairT")),
                    lam(
                        "pyFirst",
                        Eithers.bind(
                            _local("encodeType")(var("env"), _proj("hydra.core.PairType", "second", "pairT")),
                            lam(
                                "pySecond",
                                right(
                                    name_params(
                                        "tuple",
                                        list_(
                                            [
                                                var("pyFirst"),
                                                var("pySecond"),
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
        field("record", constant(var("dflt"))
        ),
        field("set",
            lam(
                "et",
                Eithers.bind(
                    call_self("et"),
                    lam(
                        "pyet",
                        right(
                            name_params(
                                "Set",
                                list_([var("pyet")]),
                            )
                        ),
                    ),
                ),
            ),
        ),
        field("union", constant(var("dflt"))
        ),
        field("unit",
            constant(
                right(
                    _kref.utils_py_name_to_py_expression(_kref.utils_py_none)
                )
            ),
        ),
        field("void",
            constant(
                right(
                    _kref.utils_py_name_to_py_expression(_kref.utils_py_none)
                )
            ),
        ),
        field("variable",
            lam(
                "name",
                right(
                    _kref.names_type_variable_reference(var("env"), var("name"))
                ),
            ),
        ),
        field("wrap", constant(var("dflt"))
        ),
        field("annotated", constant(var("dflt"))
        ),
    ]
    body = lambdas(
        ["env", "typ"],
        let_chain(
            [
                (
                    "dflt",
                    right(
                        _kref.utils_double_quoted_string(Strings.cat2(
                                string("type = "),
                                _kref.show_core_type(_kref.strip_deannotate_type(var("typ"))),
                            ))
                    ),
                ),
            ],
            cases("hydra.core.Type",
                _kref.strip_deannotate_type(var("typ")),
                Nothing(),
                fields,
            ),
        ),
    )
    return _def(
        "encodeType",
        doc("Encode a Hydra type to a Python type expression", body),
    )


def _encode_type_assignment():
    body = lambdas(
        ["cx", "env", "name", "typ", "comment"],
        Eithers.bind(
            _local("encodeTypeAssignmentInner")(var("cx"), var("env"), var("name"), var("typ"), var("comment")),
            lam(
                "defStmts",
                right(
                    Lists.map(
                        lam(
                            "s", list_([var("s")])
                        ),
                        var("defStmts"),
                    )
                ),
            ),
        ),
    )
    return _def(
        "encodeTypeAssignment",
        doc(
            "Encode a type definition, dispatching based on type structure", body
        ),
    )


def _encode_type_assignment_inner():
    dflt = Eithers.bind(
        _local("encodeType")(var("env"), var("typ")),
        lam(
            "typeExpr",
            right(
                _local("encodeTypeDefSingle")(var("env"), var("name"), var("comment"), var("typeExpr"))
            ),
        ),
    )
    body = lambdas(
        ["cx", "env", "name", "typ", "comment"],
        let_chain(
            [
                ("stripped", _kref.strip_deannotate_type(var("typ"))),
                ("dflt", dflt),
            ],
            cases_with_default("hydra.core.Type", var("stripped"), var("dflt"),
            field("forall",
                        lam(
                            "ft",
                            let_chain(
                                [
                                    (
                                        "tvar",
                                        Core.forall_type_parameter(var("ft")),
                                    ),
                                    (
                                        "body",
                                        Core.forall_type_body(var("ft")),
                                    ),
                                    (
                                        "newEnv",
                                        _local("extendEnvWithTypeVar")(var("env"), var("tvar")),
                                    ),
                                ],
                                _local("encodeTypeAssignmentInner")(var("cx"), var("newEnv"), var("name"), var("body"), var("comment")),
                            ),
                        ),
                    ),
            field("record",
                        lam(
                            "rt",
                            Eithers.map_(
                                lam(
                                    "s", list_([var("s")])
                                ),
                                _local("encodeRecordType")(var("cx"), var("env"), var("name"), var("rt"), var("comment")),
                            ),
                        ),
                    ),
            field("union",
                        lam(
                            "rt",
                            _local("encodeUnionType")(var("cx"), var("env"), var("name"), var("rt"), var("comment")),
                        ),
                    ),
            field("wrap",
                        lam(
                            "wt",
                            _local("encodeWrappedType")(var("env"), var("name"), var("wt"), var("comment")),
                        ),
                    )),
        ),
    )
    return _def(
        "encodeTypeAssignmentInner",
        doc(
            "Encode the inner type definition, unwrapping forall types", body
        ),
    )


def _encode_union_elimination_inline():
    encode_branch = lam(
        "field",
        let_chain(
            [
                ("fname", Core.field_name(var("field"))),
                ("fterm", Core.field_term(var("field"))),
                (
                    "isUnitVariant",
                    _local("isVariantUnitType")(var("rt"), var("fname")),
                ),
                (
                    "pyVariantName",
                    _local("deconflictVariantName")(true(), var("env"), var("tname"), var("fname"), _env("graph", "env")),
                ),
                (
                    "pyTypeName",
                    _kref.names_encode_name(true(), _kref.util_case_convention_pascal, var("env"), var("tname")),
                ),
                (
                    "pyEnumValue",
                    _kref.names_encode_enum_value(var("env"), var("fname")),
                ),
                (
                    "enumMemberExpr",
                    _kref.utils_py_primary_to_py_expression(PySyn.primary_compound(
                            PySyn.primary_with_rhs(
                                _kref.utils_py_name_to_py_primary(var("pyTypeName")),
                                PySyn.primary_rhs_project(var("pyEnumValue")),
                            )
                        )),
                ),
                (
                    "isinstanceCheck",
                    Logic.if_else(
                        var("isEnum"),
                        PyDsl.py_comparison_to_py_expression(
                            PySyn.comparison(
                                _kref.utils_py_expression_to_bitwise_or(var("pyArg")),
                                list_(
                                    [
                                        PyDsl.comp_pair_eq(
                                            _kref.utils_py_expression_to_bitwise_or(var("enumMemberExpr"))
                                        )
                                    ]
                                ),
                            )
                        ),
                        _kref.utils_function_call(var("isinstancePrimary"), list_(
                                [
                                    var("pyArg"),
                                    _kref.utils_py_name_to_py_expression(var("pyVariantName")),
                                ]
                            )),
                    ),
                ),
            ],
            Eithers.bind(
                _local("encodeTermInline")(var("cx"), var("env"), false(), var("fterm")),
                lam(
                    "pyBranch",
                    let_chain(
                        [
                            (
                                "pyResult",
                                Logic.if_else(
                                    var("isEnum"),
                                    _kref.utils_function_call(_kref.utils_py_expression_to_py_primary(var("pyBranch")), list_([var("pyArg")])),
                                    Logic.if_else(
                                        var("isUnitVariant"),
                                        _kref.utils_function_call(_kref.utils_py_expression_to_py_primary(var("pyBranch")), list_(
                                                [var("pyArg")]
                                            )),
                                        _kref.utils_function_call(_kref.utils_py_expression_to_py_primary(var("pyBranch")), list_(
                                                [var("valueExpr")]
                                            )),
                                    ),
                                ),
                            ),
                        ],
                        right(
                            pair(
                                var("isinstanceCheck"),
                                var("pyResult"),
                            )
                        ),
                    ),
                ),
            ),
        ),
    )
    build_chain = lambdas(
        ["elseExpr", "branchPair"],
        let_chain(
            [
                ("checkExpr", Pairs.first(var("branchPair"))),
                ("resultExpr", Pairs.second(var("branchPair"))),
            ],
            PySyn.expression_conditional(
                PySyn.conditional(
                    _kref.utils_py_expression_to_disjunction(var("resultExpr")),
                    _kref.utils_py_expression_to_disjunction(var("checkExpr")),
                    var("elseExpr"),
                )
            ),
        ),
    )
    body = lambdas(
        ["cx", "env", "cs", "pyArg"],
        let_chain(
            [
                ("tname", Core.case_statement_type_name(var("cs"))),
                ("mdefault", Core.case_statement_default(var("cs"))),
                ("cases_", Core.case_statement_cases(var("cs"))),
            ],
            Eithers.bind(
                _kref.resolution_require_union_type(var("cx"), _local("pythonEnvironmentGetGraph")(var("env")), var("tname")),
                lam(
                    "rt",
                    let_chain(
                        [
                            (
                                "isEnum",
                                _kref.predicates_is_enum_row_type(var("rt")),
                            ),
                            (
                                "valueExpr",
                                _kref.utils_project_from_expression(var("pyArg"), _py_name("value")),
                            ),
                            (
                                "isinstancePrimary",
                                _kref.utils_py_name_to_py_primary(_py_name("isinstance")),
                            ),
                        ],
                        Eithers.bind(
                            Maybes.maybe(
                                right(
                                    _local("unsupportedExpression")(string(
                                            "no matching case in inline union elimination"
                                        ))
                                ),
                                lam(
                                    "dflt",
                                    _local("encodeTermInline")(var("cx"), var("env"), false(), var("dflt")),
                                ),
                                var("mdefault"),
                            ),
                            lam(
                                "pyDefault",
                                let_chain(
                                    [("encodeBranch", encode_branch)],
                                    Eithers.bind(
                                        Eithers.map_list(
                                            var("encodeBranch"),
                                            var("cases_"),
                                        ),
                                        lam(
                                            "encodedBranches",
                                            let_chain(
                                                [("buildChain", build_chain)],
                                                right(
                                                    Lists.foldl(
                                                        var("buildChain"),
                                                        var("pyDefault"),
                                                        Lists.reverse(
                                                            var("encodedBranches")
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
        doc(
            "Encode a union elimination as an inline conditional chain (isinstance-based ternary)",
            body,
        ),
    )


def _encode_union_field():
    body = lambdas(
        ["cx", "env", "unionName", "fieldType"],
        let_chain(
            [
                ("fname", Core.field_type_name(var("fieldType"))),
                ("ftype", Core.field_type_type(var("fieldType"))),
            ],
            Eithers.bind(
                _kref.annotations_get_type_description(var("cx"), _local("pythonEnvironmentGetGraph")(var("env")), var("ftype")),
                lam(
                    "fcomment",
                    let_chain(
                        [
                            (
                                "isUnit",
                                Equality.equal(
                                    _kref.strip_deannotate_type(var("ftype")),
                                    Core.type_unit,
                                ),
                            ),
                            (
                                "varName",
                                _local("deconflictVariantName")(false(), var("env"), var("unionName"), var("fname"), _env("graph", "env")),
                            ),
                            (
                                "tparamNames",
                                _local("findTypeParams")(var("env"), var("ftype")),
                            ),
                            (
                                "tparamPyNames",
                                Lists.map(
                                    _kref.names_encode_type_variable,
                                    var("tparamNames"),
                                ),
                            ),
                            (
                                "fieldParams",
                                Lists.map(
                                    _kref.utils_py_name_to_py_type_parameter,
                                    var("tparamPyNames"),
                                ),
                            ),
                            (
                                "body",
                                Logic.if_else(
                                    var("isUnit"),
                                    _kref.utils_indented_block(var("fcomment"), list_(
                                            [
                                                _kref.utils_unit_variant_methods(var("varName"))
                                            ]
                                        )),
                                    _kref.utils_indented_block(var("fcomment"), list_([])),
                                ),
                            ),
                        ],
                        Eithers.bind(
                            Logic.if_else(
                                var("isUnit"),
                                right(nothing()),
                                Eithers.bind(
                                    _local("encodeTypeQuoted")(var("env"), var("ftype")),
                                    lam(
                                        "quotedType",
                                        right(
                                            just(
                                                _local("variantArgs")(var("quotedType"), list_([]))
                                            )
                                        ),
                                    ),
                                ),
                            ),
                            lam(
                                "margs",
                                right(
                                    _kref.utils_py_class_definition_to_py_statement(PySyn.class_definition(
                                            nothing(),
                                            var("varName"),
                                            var("fieldParams"),
                                            var("margs"),
                                            var("body"),
                                        ))
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
        doc("Encode a union field as a variant class", body),
    )


def _encode_union_field_alt():
    body = lambdas(
        ["env", "unionName", "fieldType"],
        let_chain(
            [
                ("fname", Core.field_type_name(var("fieldType"))),
                ("ftype", Core.field_type_type(var("fieldType"))),
                (
                    "tparamNames",
                    _local("findTypeParams")(var("env"), var("ftype")),
                ),
                (
                    "tparams",
                    Lists.map(
                        _kref.names_encode_type_variable,
                        var("tparamNames"),
                    ),
                ),
                (
                    "namePrim",
                    _kref.utils_py_name_to_py_primary(_kref.names_variant_name(false(), var("env"), var("unionName"), var("fname"))),
                ),
            ],
            Logic.if_else(
                Lists.null(var("tparams")),
                var("namePrim"),
                let_chain(
                    [
                        (
                            "tparamExprs",
                            Lists.map(
                                _kref.utils_py_name_to_py_expression,
                                var("tparams"),
                            ),
                        ),
                    ],
                    _kref.utils_primary_with_expression_slices(var("namePrim"), var("tparamExprs")),
                ),
            ),
        ),
    )
    return _def(
        "encodeUnionFieldAlt",
        doc(
            "Encode a union field as a primary expression for | alternatives", body
        ),
    )


def _encode_forall_type():
    gather_params_inner = lambdas(
        ["t", "ps"],
        _type_cases_with_one_branch(
            var("t"),
            pair(var("t"), Lists.reverse(var("ps"))),
            field("forall",
                lam(
                    "forallT",
                    var("gatherParams")(_proj("hydra.core.ForallType", "body", "forallT"), Lists.cons(
                            _proj("hydra.core.ForallType", "parameter", "forallT"),
                            var("ps"),
                        )),
                ),
            ),
            ["annotated", "application", "function", "list", "literal", "map",
             "maybe", "either", "pair", "record", "set", "union", "unit",
             "variable", "void", "wrap"],
        ),
    )
    body = lambdas(
        ["env", "lt"],
        let_chain(
            [("gatherParams", gather_params_inner)],
            let_chain(
                [
                    (
                        "bodyAndParams",
                        var("gatherParams")(inject("hydra.core.Type",
                                Name("forall"),
                                var("lt"),
                            ), list_([])),
                    ),
                    ("body", Pairs.first(var("bodyAndParams"))),
                    ("params", Pairs.second(var("bodyAndParams"))),
                ],
                Eithers.bind(
                    _local("encodeType")(var("env"), var("body")),
                    lam(
                        "pyBody",
                        right(
                            _kref.utils_primary_and_params(_kref.utils_py_expression_to_py_primary(var("pyBody")), Lists.map(
                                    lam(
                                        "n",
                                        PyDsl.py_name_to_py_expression(
                                            _py_name(Core.un_name(var("n")))
                                        ),
                                    ),
                                    var("params"),
                                ))
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "encodeForallType",
        doc("Encode a forall type to Python expression", body),
    )


def _encode_function_type():
    gather_params_inner = lambdas(
        ["rdoms", "ftype"],
        let_chain(
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
                var("innerCod"),
                pair(
                    Lists.reverse(
                        Lists.cons(var("dom"), var("rdoms"))
                    ),
                    var("innerCod"),
                ),
                field("function",
                    lam(
                        "ft2",
                        var("gatherParams")(Lists.cons(var("dom"), var("rdoms")), var("ft2")),
                    ),
                ),
                ["annotated", "application", "forall", "list", "literal", "map",
                 "maybe", "either", "pair", "record", "set", "union", "unit",
                 "variable", "void", "wrap"],
            ),
        ),
    )
    body = lambdas(
        ["env", "ft"],
        let_chain(
            [("gatherParams", gather_params_inner)],
            let_chain(
                [
                    (
                        "domsAndCod",
                        var("gatherParams")(list_([]), var("ft")),
                    ),
                    ("doms", Pairs.first(var("domsAndCod"))),
                    ("cod", Pairs.second(var("domsAndCod"))),
                ],
                Eithers.bind(
                    Eithers.map_list(
                        _local("encodeType")(var("env")),
                        var("doms"),
                    ),
                    lam(
                        "pydoms",
                        Eithers.bind(
                            _local("encodeType")(var("env"), var("cod")),
                            lam(
                                "pycod",
                                right(
                                    _kref.utils_py_primary_to_py_expression(_kref.utils_primary_with_slices(PySyn.primary_simple(
                                                PySyn.atom_name(_py_name("Callable"))
                                            ), _kref.utils_py_primary_to_py_slice(PySyn.primary_simple(
                                                    PySyn.atom_list(
                                                        _kref.utils_py_list(var("pydoms"))
                                                    )
                                                )), list_(
                                                [
                                                    PySyn.slice_or_starred_expression_slice(
                                                        _kref.utils_py_expression_to_py_slice(var("pycod"))
                                                    )
                                                ]
                                            )))
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
        doc("Encode a function type to Python Callable expression", body),
    )


def _encode_integer_value():
    to_py_int = lam(
        "n",
        right(
            _kref.utils_py_atom_to_py_expression(PySyn.atom_number(PySyn.number_integer(var("n"))))
        ),
    )

    def lift(fname, conv_fn):
        return field(
            Name(fname),
            lam(
                "i",
                var("toPyInt")(conv_fn(var("i"))),
            ),
        )

    body = lambdas(
        ["iv"],
        let_chain(
            [("toPyInt", to_py_int)],
            cases("hydra.core.IntegerValue",
                var("iv"),
                Nothing(),
                [
                    field("bigint",
                        lam(
                            "i",
                            var("toPyInt")(var("i")),
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
        doc("Encode an integer value to a Python expression", body),
    )


def _encode_literal():
    body = lambdas(
        ["lit"],
        cases("hydra.core.Literal",
            var("lit"),
            Nothing(),
            [
                field("binary",
                    lam(
                        "bs",
                        let_chain(
                            [("byteValues", Literals.binary_to_bytes(var("bs")))],
                            right(
                                _kref.utils_function_call(PySyn.primary_simple(
                                        PySyn.atom_name(_py_name("bytes"))
                                    ), list_(
                                        [
                                            _kref.utils_py_atom_to_py_expression(PySyn.atom_list(
                                                    _kref.utils_py_list(Lists.map(
                                                            lam(
                                                                "byteVal",
                                                                _kref.utils_py_atom_to_py_expression(PySyn.atom_number(
                                                                        PySyn.number_integer(
                                                                            Literals.int32_to_bigint(
                                                                                var("byteVal")
                                                                            )
                                                                        )
                                                                    )),
                                                            ),
                                                            var("byteValues"),
                                                        ))
                                                ))
                                        ]
                                    ))
                            ),
                        ),
                    ),
                ),
                field("boolean",
                    lam(
                        "b",
                        right(
                            _kref.utils_py_atom_to_py_expression(Logic.if_else(
                                    var("b"),
                                    PySyn.atom_true,
                                    PySyn.atom_false,
                                ))
                        ),
                    ),
                ),
                field("decimal",
                    lam(
                        "d",
                        right(
                            _kref.utils_function_call(_kref.utils_py_name_to_py_primary(_py_name("Decimal")), list_(
                                    [
                                        _kref.utils_single_quoted_string(Literals.show_decimal(var("d")))
                                    ]
                                ))
                        ),
                    ),
                ),
                field("float",
                    lam(
                        "f", _local("encodeFloatValue")(var("f"))
                    ),
                ),
                field("integer",
                    lam(
                        "i", _local("encodeIntegerValue")(var("i"))
                    ),
                ),
                field("string",
                    lam(
                        "s",
                        right(
                            _kref.utils_string_to_py_expression(PySyn.quote_style_double, var("s"))
                        ),
                    ),
                ),
            ],
        ),
    )
    return _def(
        "encodeLiteral",
        doc("Encode a literal value to a Python expression", body),
    )


def _encode_literal_type():
    body = lambdas(
        ["lt"],
        let_chain(
            [
                (
                    "findName",
                    cases("hydra.core.LiteralType",
                        var("lt"),
                        Nothing(),
                        [
                            field("binary",
                                constant(string("bytes")),
                            ),
                            field("boolean",
                                constant(string("bool")),
                            ),
                            field("decimal",
                                constant(string("Decimal")),
                            ),
                            field("float",
                                lam(
                                    "ft",
                                    cases("hydra.core.FloatType",
                                        var("ft"),
                                        Nothing(),
                                        [
                                            field("float32",
                                                constant(string("float")),
                                            ),
                                            field("float64",
                                                constant(string("float")),
                                            ),
                                        ],
                                    ),
                                ),
                            ),
                            field("integer",
                                constant(string("int")),
                            ),
                            field("string",
                                constant(string("str")),
                            ),
                        ],
                    ),
                )
            ],
            right(
                PyDsl.py_name_to_py_expression(_py_name(var("findName")))
            ),
        ),
    )
    return _def(
        "encodeLiteralType",
        doc("Encode a literal type to a Python type expression", body),
    )


def _environment_type_parameters():
    # Lists.map (PyUtils.pyNameToPyTypeParameter <.> PyNames.encodeTypeVariable)
    #          (Pairs.first (env.boundTypeVariables))
    # The composition <.> is hydra.lib.lists.compose? Actually `Util.compose`. Let me check
    # — actually it's just function composition. In the term-level DSL, this is constructed via
    # compose.
    body = lambdas(
        ["env"],
        Lists.map(
            compose(
                _kref.utils_py_name_to_py_type_parameter,
                _kref.names_encode_type_variable,
            ),
            Pairs.first(_env("boundTypeVariables", "env")),
        ),
    )
    return _def(
        "environmentTypeParameters",
        doc(
            "Get type parameters from environment as Python TypeParameters", body
        ),
    )


def _extend_env_with_lambda_params():
    inner_go = lambdas(
        ["e", "t"],
        cases_with_default("hydra.core.Term", _kref.strip_deannotate_and_detype_term(var("t")), var("e"),
            field("lambda",
                    lam(
                        "lam",
                        let_chain(
                            [
                                (
                                    "newTc",
                                    _kref.scoping_extend_graph_for_lambda(_local("pythonEnvironmentGetGraph")(var("e")), var("lam")),
                                ),
                                (
                                    "newEnv",
                                    _local("pythonEnvironmentSetGraph")(var("newTc"), var("e")),
                                ),
                            ],
                            var("go")(var("newEnv"), Core.lambda_body(var("lam"))),
                        ),
                    ),
                )),
    )
    body = lambdas(
        ["env", "term"],
        let_chain(
            [("go", inner_go)],
            var("go")(var("env"), var("term")),
        ),
    )
    return _def(
        "extendEnvWithLambdaParams",
        doc(
            "Extend environment with lambda parameters from a term", body
        ),
    )


def _extend_env_with_type_var():
    body = lambdas(
        ["env", "var_"],
        let_chain(
            [
                ("oldBound", _env("boundTypeVariables", "env")),
                ("tparamList", Pairs.first(var("oldBound"))),
                ("tparamMap", Pairs.second(var("oldBound"))),
                (
                    "newList",
                    Lists.concat2(
                        var("tparamList"),
                        list_([var("var_")]),
                    ),
                ),
                (
                    "newMap",
                    Maps.insert(
                        var("var_"),
                        _kref.names_encode_type_variable(var("var_")),
                        var("tparamMap"),
                    ),
                ),
            ],
            record("hydra.python.environment.PythonEnvironment",
                [
                    field("namespaces", _env("namespaces", "env")
                    ),
                    field("boundTypeVariables",
                        pair(var("newList"), var("newMap")),
                    ),
                    field("graph", _env("graph", "env")),
                    field("nullaryBindings", _env("nullaryBindings", "env")
                    ),
                    field("version", _env("version", "env")),
                    field("skipCasts", _env("skipCasts", "env")),
                    field("inlineVariables", _env("inlineVariables", "env")
                    ),
                ],
            ),
        ),
    )
    return _def(
        "extendEnvWithTypeVar",
        doc(
            "Extend a PythonEnvironment with a new bound type variable", body
        ),
    )


def _encode_union_type():
    enum_branch = Eithers.bind(
        Eithers.map_list(
            _local("encodeEnumValueAssignment")(var("cx"), var("env")),
            var("rowType"),
        ),
        lam(
            "vals",
            let_chain(
                [
                    (
                        "body",
                        _kref.utils_indented_block(var("comment"), var("vals")),
                    ),
                    ("enumName", _py_name("Enum")),
                    (
                        "args",
                        just(
                            _kref.utils_py_expressions_to_py_args(list_(
                                    [
                                        _kref.utils_py_name_to_py_expression(var("enumName"))
                                    ]
                                ))
                        ),
                    ),
                    (
                        "pyName",
                        _kref.names_encode_name(false(), _kref.util_case_convention_pascal, var("env"), var("name")),
                    ),
                    (
                        "typeConstStmt",
                        _kref.utils_dotted_assignment_statement(var("pyName"), _kref.names_encode_constant_for_type_name(var("env"), var("name")), _kref.utils_function_call(_kref.utils_py_name_to_py_primary(_kref.names_encode_name(true(), _kref.util_case_convention_pascal, var("env"), Core.name(string("hydra.core.Name")))), list_(
                                    [
                                        _kref.utils_double_quoted_string(Core.un_name(var("name")))
                                    ]
                                ))),
                    ),
                ],
                right(
                    list_(
                        [
                            _kref.utils_py_class_definition_to_py_statement(PySyn.class_definition(
                                    nothing(),
                                    var("pyName"),
                                    list_([]),
                                    var("args"),
                                    var("body"),
                                )),
                            var("typeConstStmt"),
                        ]
                    )
                ),
            ),
        ),
    )
    union_branch = let_chain(
        [
            (
                "constStmts",
                _local("encodeNameConstants")(var("env"), var("name"), var("rowType")),
            ),
        ],
        Eithers.bind(
            Eithers.map_list(
                _local("encodeUnionField")(var("cx"), var("env"), var("name")),
                var("rowType"),
            ),
            lam(
                "fieldStmts",
                let_chain(
                    [
                        (
                            "tparams",
                            _local("environmentTypeParameters")(var("env")),
                        ),
                        (
                            "unionAlts",
                            Lists.map(
                                _local("encodeUnionFieldAlt")(var("env"), var("name")),
                                var("rowType"),
                            ),
                        ),
                        (
                            "unionStmts",
                            _local("unionTypeStatementsFor")(var("env"), _kref.names_encode_name(false(), _kref.util_case_convention_pascal, var("env"), var("name")), var("tparams"), var("comment"), _kref.utils_or_expression(var("unionAlts")), var("constStmts")),
                        ),
                    ],
                    right(
                        Lists.concat2(
                            var("fieldStmts"), var("unionStmts")
                        )
                    ),
                ),
            ),
        ),
    )
    body = lambdas(
        ["cx", "env", "name", "rowType", "comment"],
        Logic.if_else(
            _kref.predicates_is_enum_row_type(var("rowType")),
            enum_branch,
            union_branch,
        ),
    )
    return _def(
        "encodeUnionType",
        doc(
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
                    _kref.variables_free_variables_in_type(var(typ_var))
                )
            ),
            _local("makeSimpleLambda")(_kref.arity_type_arity(var(typ_var)), var("asVariable")),
            var("asVariable"),
        )

    # When name IS in tcMetadata
    metadata_branch = Logic.if_else(
        Logic.and_(
            Equality.equal(
                _kref.arity_type_arity(var("typ")),
                int32(0),
            ),
            _kref.predicates_is_complex_variable(var("tc"), var("name")),
        ),
        right(var("asFunctionCall")),
        let_chain(
            [("asFunctionRef", as_function_ref("typ"))],
            right(var("asFunctionRef")),
        ),
    )

    # When name in graphBoundTypes but NOT in metadata: check graph elements
    el_typed_branch = lam(
        "ts",
        Logic.if_else(
            Logic.and_(
                Logic.and_(
                    Equality.equal(
                        _kref.arity_type_arity(var("typ")),
                        int32(0),
                    ),
                    _kref.predicates_is_complex_binding(var("tc"), var("el")),
                ),
                Logic.not_(var("elTrivial")),
            ),
            right(var("asFunctionCall")),
            let_chain(
                [("asFunctionRef", as_function_ref("typ"))],
                right(var("asFunctionRef")),
            ),
        ),
    )
    el_branch = lam(
        "el",
        let_chain(
            [
                (
                    "elTrivial",
                    _kref.predicates_is_trivial_term(Core.binding_term(var("el"))),
                ),
            ],
            Maybes.maybe(
                Logic.if_else(
                    Logic.and_(
                        Equality.equal(
                            _kref.arity_type_arity(var("typ")),
                            int32(0),
                        ),
                        Logic.not_(var("elTrivial")),
                    ),
                    right(var("asFunctionCall")),
                    let_chain(
                        [("asFunctionRef", as_function_ref("typ"))],
                        right(var("asFunctionRef")),
                    ),
                ),
                el_typed_branch,
                Core.binding_type_scheme(var("el")),
            ),
        ),
    )
    not_in_metadata_branch = Maybes.maybe(
        let_chain(
            [("asFunctionRef", as_function_ref("typ"))],
            right(var("asFunctionRef")),
        ),
        el_branch,
        _kref.lexical_lookup_binding(var("g"), var("name")),
    )

    has_typ_branch = lam(
        "typ",
        Logic.if_else(
            Sets.member(var("name"), var("tcLambdaVars")),
            right(var("asVariable")),
            Logic.if_else(
                Sets.member(var("name"), var("inlineVars")),
                # Inline variable (Lazy-wrapped at definition; force via .get())
                let_chain(
                    [
                        (
                            "unwrapped",
                            _local("lazyDotGet")(var("asVariable")),
                        ),
                        (
                            "asFunctionRef",
                            Logic.if_else(
                                Logic.not_(
                                    Sets.null(
                                        _kref.variables_free_variables_in_type(var("typ"))
                                    )
                                ),
                                _local("makeSimpleLambda")(_kref.arity_type_arity(var("typ")), var("unwrapped")),
                                var("unwrapped"),
                            ),
                        ),
                    ],
                    right(var("asFunctionRef")),
                ),
                Logic.if_else(
                    Logic.not_(
                        Maps.member(
                            var("name"), var("tcMetadata")
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
    el_branch_no_typ_inner = lam(
        "ts",
        Logic.if_else(
            Logic.and_(
                Logic.and_(
                    Equality.equal(
                        _kref.arity_type_scheme_arity(var("ts")),
                        int32(0),
                    ),
                    _kref.predicates_is_complex_binding(var("tc"), var("el")),
                ),
                Logic.not_(var("elTrivial1")),
            ),
            right(var("asFunctionCall")),
            let_chain(
                [
                    (
                        "asFunctionRef",
                        Logic.if_else(
                            Logic.not_(
                                Lists.null(
                                    Core.type_scheme_variables(var("ts"))
                                )
                            ),
                            _local("makeSimpleLambda")(_kref.arity_type_arity(Core.type_scheme_body(var("ts"))), var("asVariable")),
                            var("asVariable"),
                        ),
                    ),
                ],
                right(var("asFunctionRef")),
            ),
        ),
    )
    no_prim_no_typ_el_branch = lam(
        "el",
        let_chain(
            [
                (
                    "elTrivial1",
                    _kref.predicates_is_trivial_term(Core.binding_term(var("el"))),
                ),
            ],
            Maybes.maybe(
                right(var("asVariable")),
                el_branch_no_typ_inner,
                Core.binding_type_scheme(var("el")),
            ),
        ),
    )
    not_in_graphBoundTypes_no_prim = Maybes.maybe(
        Maybes.maybe(
            left(
                Errors_dsl.error_other(
                    Errors_dsl.other_error(
                        Strings.cat2(
                            string("Unknown variable: "),
                            Core.un_name(var("name")),
                        )
                    )
                )
            ),
            constant(right(var("asFunctionCall"))),
            Maps.lookup(var("name"), var("tcMetadata")),
        ),
        no_prim_no_typ_el_branch,
        _kref.lexical_lookup_binding(var("g"), var("name")),
    )
    is_prim_no_typ_branch = lam(
        "prim",
        let_chain(
            [
                (
                    "primArity",
                    _kref.arity_primitive_arity(var("prim")),
                ),
            ],
            Logic.if_else(
                Equality.equal(var("primArity"), int32(0)),
                right(var("asFunctionCall")),
                let_chain(
                    [
                        (
                            "ts",
                            _kref.scoping_term_signature_to_type_scheme(
                                project(Name("hydra.packaging.PrimitiveDefinition"), Name("signature"))(
                                    _proj("hydra.graph.Primitive", "definition", "prim"))),
                        ),
                        (
                            "asFunctionRef",
                            Logic.if_else(
                                Logic.not_(
                                    Lists.null(
                                        Core.type_scheme_variables(var("ts"))
                                    )
                                ),
                                _local("makeSimpleLambda")(_kref.arity_type_arity(Core.type_scheme_body(var("ts"))), var("asVariable")),
                                var("asVariable"),
                            ),
                        ),
                    ],
                    right(var("asFunctionRef")),
                ),
            ),
        ),
    )
    no_typ_branch = Logic.if_else(
        Sets.member(var("name"), var("tcLambdaVars")),
        # Untyped lambda variable
        right(var("asVariable")),
        Logic.if_else(
            Sets.member(var("name"), var("inlineVars")),
            # Untyped inline variable (Lazy-wrapped at definition; force via .get())
            right(
                _local("lazyDotGet")(var("asVariable"))
            ),
            Maybes.maybe(
                not_in_graphBoundTypes_no_prim,
                is_prim_no_typ_branch,
                _kref.lexical_lookup_primitive(var("g"), var("name")),
            ),
        ),
    )

    empty_args_branch = Maybes.maybe(
        no_typ_branch, has_typ_branch, var("mTyp")
    )

    # Non-empty args branch: primitive lookup
    prim_branch = lam(
        "prim",
        let_chain(
            [
                (
                    "primArity",
                    _kref.arity_primitive_arity(var("prim")),
                ),
            ],
            Logic.if_else(
                Equality.equal(
                    var("primArity"), Lists.length(var("args"))
                ),
                right(var("asFunctionCall")),
                let_chain(
                    [
                        (
                            "numRemaining",
                            Math.sub(
                                var("primArity"),
                                Lists.length(var("args")),
                            ),
                        ),
                        (
                            "remainingParams",
                            Lists.map(
                                lam(
                                    "i",
                                    _py_name(
                                        Strings.cat2(
                                            string("x"),
                                            Literals.show_int32(var("i")),
                                        )
                                    ),
                                ),
                                Math.range_(
                                    int32(1), var("numRemaining")
                                ),
                            ),
                        ),
                        (
                            "remainingExprs",
                            Lists.map(
                                lam(
                                    "n",
                                    PyDsl.py_name_to_py_expression(var("n")),
                                ),
                                var("remainingParams"),
                            ),
                        ),
                        (
                            "allArgs",
                            Lists.concat2(
                                var("args"), var("remainingExprs")
                            ),
                        ),
                        (
                            "fullCall",
                            _kref.utils_function_call(_kref.utils_py_name_to_py_primary(_kref.names_encode_name(true(), _kref.util_case_convention_lower_snake, var("env"), var("name"))), var("allArgs")),
                        ),
                    ],
                    right(
                        _local("makeUncurriedLambda")(var("remainingParams"), var("fullCall"))
                    ),
                ),
            ),
        ),
    )
    nonempty_args_branch = Maybes.maybe(
        right(var("asFunctionCall")),
        prim_branch,
        _kref.lexical_lookup_primitive(var("g"), var("name")),
    )

    body = lambdas(
        ["cx", "env", "name", "args"],
        let_chain(
            [
                (
                    "g",
                    _local("pythonEnvironmentGetGraph")(var("env")),
                ),
                ("tc", _env("graph", "env")),
                ("tcTypes", Graph_dsl.graph_bound_types(var("tc"))),
                (
                    "tcLambdaVars",
                    Graph_dsl.graph_lambda_variables(var("tc")),
                ),
                ("tcMetadata", Graph_dsl.graph_metadata(var("tc"))),
                ("inlineVars", _env("inlineVariables", "env")),
                (
                    "mTypScheme",
                    Maps.lookup(var("name"), var("tcTypes")),
                ),
                (
                    "mTyp",
                    Maybes.map(
                        lam(
                            "ts_", Core.type_scheme_body(var("ts_"))
                        ),
                        var("mTypScheme"),
                    ),
                ),
                (
                    "asVariable",
                    _kref.names_term_variable_reference(var("env"), var("name")),
                ),
                (
                    "asFunctionCall",
                    _kref.utils_function_call(_kref.utils_py_name_to_py_primary(_kref.names_encode_name(true(), _kref.util_case_convention_lower_snake, var("env"), var("name"))), var("args")),
                ),
                # Lazy-aware function call for inline-var references: name.get()(args)
                (
                    "asLazyCall",
                    _kref.utils_function_call(_kref.utils_py_expression_to_py_primary(_local("lazyDotGet")(var("asVariable"))), var("args")),
                ),
            ],
            Logic.if_else(
                Logic.not_(Lists.null(var("args"))),
                # Non-empty args: inline-var calls need .get(); otherwise check primitives
                Logic.if_else(
                    Sets.member(var("name"), var("inlineVars")),
                    right(var("asLazyCall")),
                    nonempty_args_branch,
                ),
                empty_args_branch,
            ),
        ),
    )
    return _def(
        "encodeVariable",
        doc(
            "Encode a variable reference to a Python expression", body
        ),
    )


def _encode_wrapped_type():
    body = lambdas(
        ["env", "name", "typ", "comment"],
        let_chain(
            [
                ("tparamList", Pairs.first(_env("boundTypeVariables", "env"))),
            ],
            Eithers.bind(
                _local("encodeTypeQuoted")(var("env"), var("typ")),
                lam(
                    "ptypeQuoted",
                    let_chain(
                        [
                            (
                                "pyName",
                                _kref.names_encode_name(false(), _kref.util_case_convention_pascal, var("env"), var("name")),
                            ),
                            (
                                "body",
                                _kref.utils_indented_block(var("comment"), list_([])),
                            ),
                            (
                                "typeConstStmt",
                                _kref.utils_dotted_assignment_statement(var("pyName"), _kref.names_encode_constant_for_type_name(var("env"), var("name")), _kref.utils_function_call(_kref.utils_py_name_to_py_primary(_kref.names_encode_name(true(), _kref.util_case_convention_pascal, var("env"), Core.name(string("hydra.core.Name")))), list_(
                                            [
                                                _kref.utils_double_quoted_string(Core.un_name(var("name")))
                                            ]
                                        ))),
                            ),
                        ],
                        right(
                            list_(
                                [
                                    _kref.utils_py_class_definition_to_py_statement(PySyn.class_definition(
                                            nothing(),
                                            var("pyName"),
                                            Lists.map(
                                                compose(
                                                    _kref.utils_py_name_to_py_type_parameter,
                                                    _kref.names_encode_type_variable,
                                                ),
                                                _local("findTypeParams")(var("env"), var("typ")),
                                            ),
                                            just(
                                                _local("variantArgs")(var("ptypeQuoted"), var("tparamList"))
                                            ),
                                            var("body"),
                                        )),
                                    var("typeConstStmt"),
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
        doc(
            "Encode a wrapped type (newtype) to a Python class definition", body
        ),
    )


def _enum_variant_pattern():
    body = lambdas(
        ["env", "typeName", "fieldName"],
        PySyn.closed_pattern_value(
            PySyn.value_pattern(
                PySyn.attribute(
                    list_(
                        [
                            _kref.names_encode_name(true(), _kref.util_case_convention_pascal, var("env"), var("typeName")),
                            _kref.names_encode_enum_value(var("env"), var("fieldName")),
                        ]
                    )
                )
            )
        ),
    )
    return _def(
        "enumVariantPattern",
        doc("Create a value pattern for an enum variant", body),
    )


def _extend_meta_for_term():
    step_inner = lambdas(
        ["meta", "t"],
        cases_with_default("hydra.core.Term", var("t"), var("meta"),
            field("either",
                    lam(
                        "e",
                        let_chain(
                            [
                                (
                                    "metaWithCast",
                                    _local("setMetaUsesCast")(true(), var("meta")),
                                ),
                            ],
                            Eithers.either(
                                constant(
                                    _local("setMetaUsesLeft")(var("metaWithCast"), true())
                                ),
                                constant(
                                    _local("setMetaUsesRight")(var("metaWithCast"), true())
                                ),
                                var("e"),
                            ),
                        ),
                    ),
                ),
            field("lambda",
                    lam(
                        "lam",
                        Maybes.maybe(
                            var("meta"),
                            lam(
                                "dom",
                                Logic.if_else(
                                    var("topLevel"),
                                    _local("extendMetaForType")(true(), false(), var("dom"), var("meta")),
                                    var("meta"),
                                ),
                            ),
                            Core.lambda_domain(var("lam")),
                        ),
                    ),
                ),
            field("let",
                    lam(
                        "lt",
                        let_chain(
                            [
                                (
                                    "bindings",
                                    Core.let_bindings(var("lt")),
                                ),
                            ],
                            Lists.foldl(
                                let_chain(
                                    [
                                        (
                                            "forBinding",
                                            lambdas(
                                                ["m", "b"],
                                                Maybes.maybe(
                                                    var("m"),
                                                    lam(
                                                        "ts",
                                                        let_chain(
                                                            [
                                                                (
                                                                    "term1",
                                                                    Core.binding_term(
                                                                        var("b")
                                                                    ),
                                                                ),
                                                            ],
                                                            Logic.if_else(
                                                                _kref.analysis_is_simple_assignment(var("term1")),
                                                                var("m"),
                                                                _local("extendMetaForType")(true(), true(), Core.type_scheme_body(
                                                                        var("ts")
                                                                    ), var("m")),
                                                            ),
                                                        ),
                                                    ),
                                                    Core.binding_type_scheme(
                                                        var("b")
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ],
                                    var("forBinding"),
                                ),
                                var("meta"),
                                var("bindings"),
                            ),
                        ),
                    ),
                ),
            field("literal",
                    lam(
                        "l",
                        cases_with_default("hydra.core.Literal", var("l"), var("meta"),
            field("decimal",
                                    constant(
                                        _local("setMetaUsesDecimal")(var("meta"), true())
                                    ),
                                )),
                    ),
                ),
            field("list",
                    constant(
                        _local("setMetaUsesFrozenList")(var("meta"), true())
                    ),
                ),
            field("map",
                    constant(
                        _local("setMetaUsesFrozenDict")(var("meta"), true())
                    ),
                ),
            field("set",
                    constant(
                        _local("setMetaUsesFrozenSet")(var("meta"), true())
                    ),
                ),
            field("maybe",
                    lam(
                        "m",
                        Maybes.maybe(
                            _local("setMetaUsesNothing")(var("meta"), true()),
                            constant(
                                _local("setMetaUsesJust")(var("meta"), true())
                            ),
                            var("m"),
                        ),
                    ),
                ),
            # Union injections require cast() for proper typing
                field("inject",
                    constant(
                        _local("setMetaUsesCast")(true(), var("meta"))
                    ),
                )),
    )
    body = lambdas(
        ["topLevel", "meta0", "term"],
        let_chain(
            [("step", step_inner)],
            _kref.rewriting_fold_over_term(Coders_dsl.traversal_order_pre, var("step"), var("meta0"), var("term")),
        ),
    )
    return _def(
        "extendMetaForTerm",
        doc(
            "Extend metadata based on a term (used during module encoding)", body
        ),
    )


def _extend_meta_for_type():
    case_fields = [
        field("function",
            # Avoid exponential recursion: subtypes are already covered by
            # the Rewriting.subtypes walk that produced metaWithSubtypes.
            # Re-recursing on cod and dom here gave O(2^N) on deep curried
            # types. (Fix mirrors b7d29d9b4 in the Haskell coder.)
            constant(
                Logic.if_else(
                    Logic.and_(
                        var("isTermAnnot"), var("topLevel")
                    ),
                    var("metaWithSubtypes"),
                    _local("setMetaUsesCallable")(var("metaWithSubtypes"), true()),
                ),
            ),
        ),
        field("list",
            constant(
                _local("setMetaUsesFrozenList")(var("metaWithSubtypes"), true())
            ),
        ),
        field("map",
            constant(
                _local("setMetaUsesFrozenDict")(var("metaWithSubtypes"), true())
            ),
        ),
        field("set",
            constant(
                _local("setMetaUsesFrozenSet")(var("metaWithSubtypes"), true())
            ),
        ),
        field("maybe",
            constant(
                _local("setMetaUsesMaybe")(var("metaWithSubtypes"), true())
            ),
        ),
        field("either",
            constant(
                _local("setMetaUsesEither")(var("metaWithSubtypes"), true())
            ),
        ),
        field("literal",
            lam(
                "lt",
                cases_with_default("hydra.core.LiteralType", var("lt"), var("metaWithSubtypes"),
            field("decimal",
                            constant(
                                _local("setMetaUsesDecimal")(var("metaWithSubtypes"), true())
                            ),
                        )),
            ),
        ),
        field("union",
            lam(
                "rt",
                Logic.if_else(
                    _kref.predicates_is_enum_row_type(var("rt")),
                    _local("setMetaUsesEnum")(var("metaWithSubtypes"), true()),
                    Logic.if_else(
                        Logic.not_(Lists.null(var("rt"))),
                        _local("setMetaUsesNode")(var("metaWithSubtypes"), true()),
                        var("metaWithSubtypes"),
                    ),
                ),
            ),
        ),
        field("forall",
            lam(
                "ft",
                let_chain(
                    [
                        ("body", Core.forall_type_body(var("ft"))),
                        (
                            "metaForWrap",
                            _local("digForWrap")(var("isTermAnnot"), var("metaWithSubtypes"), var("body")),
                        ),
                    ],
                    cases_with_default("hydra.core.Type", _kref.strip_deannotate_type(var("body")), var("metaForWrap"),
            field("record",
                                constant(
                                    _local("setMetaUsesGeneric")(var("metaForWrap"), true())
                                ),
                            )),
                ),
            ),
        ),
        field("record",
            lam(
                "rt",
                let_chain(
                    [
                        (
                            "hasAnnotated",
                            Lists.foldl(
                                lambdas(
                                    ["b", "ft"],
                                    Logic.or_(
                                        var("b"),
                                        _kref.annotations_has_type_description(Core.field_type_type(
                                                var("ft")
                                            )),
                                    ),
                                ),
                                false(),
                                var("rt"),
                            ),
                        ),
                        (
                            "meta1",
                            Logic.if_else(
                                Lists.null(var("rt")),
                                var("metaWithSubtypes"),
                                _local("setMetaUsesDataclass")(var("metaWithSubtypes"), true()),
                            ),
                        ),
                    ],
                    Logic.if_else(
                        var("hasAnnotated"),
                        _local("setMetaUsesAnnotated")(var("meta1"), true()),
                        var("meta1"),
                    ),
                ),
            ),
        ),
        field("wrap",
            constant(
                Logic.if_else(
                    var("isTermAnnot"),
                    var("metaWithSubtypes"),
                    _local("setMetaUsesNode")(var("metaWithSubtypes"), true()),
                )
            ),
        ),
    ]
    body = lambdas(
        ["topLevel", "isTermAnnot", "typ", "meta"],
        let_chain(
            [
                ("currentTvars", _meta_proj("typeVariables", "meta")),
                (
                    "newTvars",
                    _local("collectTypeVariables")(var("currentTvars"), var("typ")),
                ),
                (
                    "metaWithTvars",
                    _local("setMetaTypeVariables")(var("meta"), var("newTvars")),
                ),
                (
                    "metaWithSubtypes",
                    Lists.foldl(
                        lambdas(
                            ["m", "t"],
                            _local("extendMetaForType")(false(), var("isTermAnnot"), var("t"), var("m")),
                        ),
                        var("metaWithTvars"),
                        _kref.rewriting_subtypes(var("typ")),
                    ),
                ),
            ],
            cases("hydra.core.Type",
                _kref.strip_deannotate_type(var("typ")),
                Just(var("metaWithSubtypes")),
                case_fields,
            ),
        ),
    )
    return _def(
        "extendMetaForType",
        doc(
            "Extend metadata based on a type (used during module encoding)", body
        ),
    )


def _extend_meta_for_types():
    body = lambdas(
        ["types", "meta"],
        let_chain(
            [
                (
                    "names",
                    Sets.unions(
                        Lists.map(
                            lam(
                                "t",
                                _kref.dependencies_type_dependency_names(false(), var("t")),
                            ),
                            var("types"),
                        )
                    ),
                ),
                ("currentNs", _meta_proj("namespaces", "meta")),
                (
                    "updatedNs",
                    _kref.analysis_add_names_to_module_names(_kref.names_encode_namespace, var("names"), var("currentNs")),
                ),
                (
                    "meta1",
                    _local("setMetaNamespaces")(var("updatedNs"), var("meta")),
                ),
            ],
            Lists.foldl(
                lambdas(
                    ["m", "t"],
                    _local("extendMetaForType")(true(), false(), var("t"), var("m")),
                ),
                var("meta1"),
                var("types"),
            ),
        ),
    )
    return _def(
        "extendMetaForTypes",
        doc("Extend metadata for a list of types", body),
    )


def _extract_case_elimination():
    body = lambdas(
        ["term"],
        cases_with_default("hydra.core.Term", _kref.strip_deannotate_and_detype_term(var("term")), nothing(),
            field("cases",
                    lam("cs", just(var("cs"))),
                )),
    )
    return _def(
        "extractCaseElimination",
        doc("Extract CaseStatement from a case elimination term", body),
    )


def _find_type_params():
    body = lambdas(
        ["env", "typ"],
        let_chain(
            [
                ("boundVars", Pairs.second(_env("boundTypeVariables", "env"))),
                (
                    "isBound",
                    lam(
                        "v",
                        Maybes.is_just(Maps.lookup(var("v"), var("boundVars"))),
                    ),
                ),
            ],
            Lists.filter(
                var("isBound"),
                Sets.to_list(
                    _kref.variables_free_variables_in_type(var("typ"))
                ),
            ),
        ),
    )
    return _def(
        "findTypeParams",
        doc(
            "Find type parameters in a type that are bound in the environment",
            body,
        ),
    )


def _function_definition_to_expr():
    py_args_action = Eithers.map_list(
        lam(
            "pair",
            let_chain(
                [
                    ("argName", Pairs.first(var("pair"))),
                    ("typ", Pairs.second(var("pair"))),
                ],
                Eithers.bind(
                    _local("encodeType")(var("env"), var("typ")),
                    lam(
                        "pyTyp",
                        right(
                            PyDsl.param_no_default_simple(
                                PySyn.param(
                                    _kref.names_encode_name(false(), _kref.util_case_convention_lower_snake, var("env"), var("argName")),
                                    just(
                                        PySyn.annotation(var("pyTyp"))
                                    ),
                                )
                            )
                        ),
                    ),
                ),
            ),
        ),
        Lists.zip(var("args"), var("doms")),
    )

    block_tco = Eithers.bind(
        _local("encodeTermMultilineTCO")(var("cx"), var("env"), var("name"), var("args"), var("body")),
        lam(
            "tcoStmts",
            let_chain(
                [
                    (
                        "trueExpr",
                        PySyn.named_expression_simple(
                            _kref.utils_py_atom_to_py_expression(PySyn.atom_true)
                        ),
                    ),
                    (
                        "whileBody",
                        _kref.utils_indented_block(nothing(), list_(
                                [
                                    Lists.concat2(
                                        var("prefixes"),
                                        var("tcoStmts"),
                                    )
                                ]
                            )),
                    ),
                    (
                        "whileStmt",
                        PySyn.statement_compound(
                            PySyn.compound_statement_while(
                                PySyn.while_statement(
                                    var("trueExpr"),
                                    var("whileBody"),
                                    nothing(),
                                )
                            )
                        ),
                    ),
                ],
                right(
                    _kref.utils_indented_block(var("comment"), list_(
                            [list_([var("whileStmt")])]
                        ))
                ),
            ),
        ),
    )
    block_normal = Eithers.bind(
        _local("encodeTermMultiline")(var("cx"), var("env"), var("body")),
        lam(
            "stmts",
            right(
                _kref.utils_indented_block(var("comment"), list_(
                        [
                            Lists.concat2(
                                var("prefixes"), var("stmts")
                            )
                        ]
                    ))
            ),
        ),
    )
    body = lambdas(
        ["cx", "env", "name", "tparams", "args", "body", "doms", "mcod", "comment", "prefixes"],
        Eithers.bind(
            py_args_action,
            lam(
                "pyArgs",
                let_chain(
                    [
                        (
                            "pyParams",
                            PyDsl.parameters_param_no_default(
                                PyDsl.param_no_default_parameters_simple(
                                    var("pyArgs")
                                )
                            )
                            if False
                            else PySyn.parameters_param_no_default(
                                PySyn.param_no_default_parameters(
                                    var("pyArgs"),
                                    list_([]),
                                    nothing(),
                                )
                            ),
                        ),
                        (
                            "isTCO",
                            Logic.and_(
                                Logic.not_(Lists.null(var("args"))),
                                _kref.analysis_is_self_tail_recursive(var("name"), var("body")),
                            ),
                        ),
                    ],
                    Eithers.bind(
                        Logic.if_else(var("isTCO"), block_tco, block_normal),
                        lam(
                            "block",
                            Eithers.bind(
                                Maybes.maybe(
                                    right(nothing()),
                                    lam(
                                        "cod",
                                        Eithers.bind(
                                            _local("encodeType")(var("env"), var("cod")),
                                            lam(
                                                "pytyp",
                                                right(
                                                    just(
                                                        var("pytyp")
                                                    )
                                                ),
                                            ),
                                        ),
                                    ),
                                    var("mcod"),
                                ),
                                lam(
                                    "mreturnType",
                                    let_chain(
                                        [
                                            (
                                                "pyTparams",
                                                Logic.if_else(
                                                    _local("useInlineTypeParams"),
                                                    Lists.map(
                                                        compose(
                                                            _kref.utils_py_name_to_py_type_parameter,
                                                            _kref.names_encode_type_variable,
                                                        ),
                                                        var("tparams"),
                                                    ),
                                                    list_([]),
                                                ),
                                            ),
                                            (
                                                "isThunk",
                                                Lists.null(var("args")),
                                            ),
                                            (
                                                "mDecorators",
                                                Logic.if_else(
                                                    var("isThunk"),
                                                    just(
                                                        wrap(
                                                            Name(
                                                                "hydra.python.syntax.Decorators"
                                                            ),
                                                            list_(
                                                                [
                                                                    _local(
                                                                        "lruCacheDecorator"
                                                                    )
                                                                ]
                                                            ),
                                                        )
                                                    ),
                                                    nothing(),
                                                ),
                                            ),
                                            (
                                                "pyName",
                                                _kref.names_encode_name(false(), _kref.util_case_convention_lower_snake, var("env"), var("name")),
                                            ),
                                        ],
                                        right(
                                            PySyn.statement_compound(
                                                PySyn.compound_statement_function(
                                                    PySyn.function_definition(
                                                        var("mDecorators"),
                                                        PySyn.function_def_raw(
                                                            false(),
                                                            var("pyName"),
                                                            var("pyTparams"),
                                                            just(
                                                                var("pyParams")
                                                            ),
                                                            var("mreturnType"),
                                                            nothing(),
                                                            var("block"),
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
        doc("Encode a function definition with parameters and body", body),
    )


def _gather_lambdas():
    inner_go = lambdas(
        ["params", "t"],
        cases_with_default("hydra.core.Term", _kref.strip_deannotate_and_detype_term(var("t")), pair(var("params"), var("t")),
            field("lambda",
                    lam(
                        "l",
                        var("go")(Lists.concat2(
                                var("params"),
                                list_([Core.lambda_parameter(var("l"))]),
                            ), Core.lambda_body(var("l"))),
                    ),
                )),
    )
    body = lambdas(
        ["term"],
        let_chain(
            [("go", inner_go)],
            var("go")(list_([]), var("term")),
        ),
    )
    return _def(
        "gatherLambdas",
        doc("Extract lambdas and their bodies from a term", body),
    )


def _gather_metadata():
    add_def = lambdas(
        ["meta", "def"],
        cases("hydra.packaging.Definition",
            var("def"),
            Nothing(),
            [
                field("term",
                    lam(
                        "termDef",
                        let_chain(
                            [
                                (
                                    "term",
                                    Pkg.term_definition_term(var("termDef")),
                                ),
                                (
                                    "typ",
                                    Maybes.maybe(
                                        Core.type_variable(
                                            wrap("hydra.core.Name",
                                                string("hydra.core.Unit"),
                                            )
                                        ),
                                        lam("sig",
                                            Core.type_scheme_body(
                                                _kref.scoping_term_signature_to_type_scheme(var("sig"))
                                            )
                                        ),
                                        Pkg.term_definition_signature(
                                            var("termDef")
                                        ),
                                    ),
                                ),
                                (
                                    "meta2",
                                    _local("extendMetaForType")(true(), true(), var("typ"), var("meta")),
                                ),
                            ],
                            _local("extendMetaForTerm")(true(), var("meta2"), var("term")),
                        ),
                    ),
                ),
                field("type",
                    lam(
                        "typeDef",
                        let_chain(
                            [
                                (
                                    "typ",
                                    Core.type_scheme_body(
                                        Pkg.type_definition_type_scheme(
                                            var("typeDef")
                                        )
                                    ),
                                ),
                                (
                                    "meta2",
                                    _local("setMetaUsesName")(var("meta"), true()),
                                ),
                            ],
                            _kref.rewriting_fold_over_type(Coders_dsl.traversal_order_pre, lambdas(
                                    ["m", "t"],
                                    _local("extendMetaForType")(true(), false(), var("t"), var("m")),
                                ), var("meta2"), var("typ")),
                        ),
                    ),
                ),
            ],
        ),
    )
    body = lambdas(
        ["focusNs", "defs"],
        let_chain(
            [
                (
                    "start",
                    _local("emptyMetadata")(_kref.utils_find_namespaces(var("focusNs"), var("defs"))),
                ),
                ("addDef", add_def),
                (
                    "result",
                    Lists.foldl(
                        var("addDef"),
                        var("start"),
                        var("defs"),
                    ),
                ),
                ("tvars", _meta_proj("typeVariables", "result")),
                (
                    "result2",
                    _local("setMetaUsesCast")(true(), _local("setMetaUsesLruCache")(true(), var("result"))),
                ),
            ],
            _local("setMetaUsesTypeVar")(var("result2"), Logic.not_(Sets.null(var("tvars")))),
        ),
    )
    return _def(
        "gatherMetadata",
        doc("Gather metadata from definitions", body),
    )


def _generic_arg():
    body = lambdas(
        ["tparamList"],
        Logic.if_else(
            Lists.null(var("tparamList")),
            nothing(),
            just(
                _kref.utils_py_primary_to_py_expression(_kref.utils_primary_with_expression_slices(PySyn.primary_simple(PySyn.atom_name(_py_name("Generic"))), Lists.map(
                            lam(
                                "n",
                                PyDsl.py_name_to_py_expression(
                                    _kref.names_encode_type_variable(var("n"))
                                ),
                            ),
                            var("tparamList"),
                        )))
            ),
        ),
    )
    return _def(
        "genericArg",
        doc("Create Generic[...] argument expression for class definition", body),
    )


def _initial_environment():
    body = lambdas(
        ["namespaces", "tcontext"],
        record("hydra.python.environment.PythonEnvironment",
            [
                field("namespaces", var("namespaces")),
                field("boundTypeVariables",
                    pair(list_([]), Maps.empty()),
                ),
                field("graph", var("tcontext")),
                field("nullaryBindings", Sets.empty()),
                field("version", _local("targetPythonVersion")),
                field("skipCasts", true()),
                field("inlineVariables", Sets.empty()),
            ],
        ),
    )
    return _def(
        "initialEnvironment",
        doc("Create an initial Python environment for code generation", body),
    )


def _initial_metadata():
    body = lambdas(
        ["ns"],
        let_chain(
            [
                ("dottedNs", _kref.names_encode_namespace(var("ns"))),
                (
                    "emptyNs",
                    Util.module_names(
                        pair(var("ns"), var("dottedNs")),
                        Maps.empty(),
                    ),
                ),
            ],
            _empty_meta_record(var("emptyNs")),
        ),
    )
    return _def(
        "initialMetadata",
        doc("Create initial empty metadata for a Python module", body),
    )


def _is_case_statement_application():
    body = lambdas(
        ["term"],
        let_chain(
            [
                ("gathered", _kref.analysis_gather_applications(var("term"))),
                ("args", Pairs.first(var("gathered"))),
                ("body", Pairs.second(var("gathered"))),
            ],
            Logic.if_else(
                Logic.not_(
                    Equality.equal(Lists.length(var("args")), int32(1))
                ),
                nothing(),
                let_chain(
                    [
                        (
                            "arg",
                            Maybes.from_maybe(
                                Core.term_unit,
                                Lists.maybe_head(var("args")),
                            ),
                        ),
                    ],
                    cases_with_default("hydra.core.Term", _kref.strip_deannotate_and_detype_term(var("body")), nothing(),
            field("cases",
                                lam(
                                    "cs",
                                    just(
                                        tuple4(
                                            Core.case_statement_type_name(var("cs")),
                                            Core.case_statement_default(var("cs")),
                                            Core.case_statement_cases(var("cs")),
                                            var("arg"),
                                        )
                                    ),
                                ),
                            )),
                ),
            ),
        ),
    )
    return _def(
        "isCaseStatementApplication",
        doc(
            "Check if a term is a case statement applied to exactly one argument", body
        ),
    )


def _is_cases_full():
    body = lambdas(
        ["rowType", "cases_"],
        let_chain(
            [
                ("numCases", Lists.length(var("cases_"))),
                ("numFields", Lists.length(var("rowType"))),
            ],
            Logic.not_(Equality.lt(var("numCases"), var("numFields"))),
        ),
    )
    return _def(
        "isCasesFull",
        doc("Check if union cases are fully covered", body),
    )


def _is_type_module_check():
    body = lambdas(
        ["defs"],
        Logic.not_(
            Lists.null(
                Lists.filter(
                    lam(
                        "d",
                        cases_with_default("hydra.packaging.Definition", var("d"), false(),
            field("type",
                                    constant(true()),
                                )),
                    ),
                    var("defs"),
                )
            )
        ),
    )
    return _def(
        "isTypeModuleCheck",
        doc(
            "Check whether a list of definitions contains any type definitions", body
        ),
    )


def _is_type_variable_name():
    body = lambdas(
        ["name"],
        Equality.equal(
            int32(1),
            Lists.length(
                Strings.split_on(string("."), Core.un_name(var("name")))
            ),
        ),
    )
    return _def(
        "isTypeVariableName",
        doc(
            "Check if a name is a type variable (unqualified - no dots)", body
        ),
    )


def _is_variant_unit_type():
    body = lambdas(
        ["rowType", "fieldName"],
        let_chain(
            [
                (
                    "mfield",
                    Lists.find(
                        lam(
                            "ft",
                            Equality.equal(
                                Core.field_type_name(var("ft")),
                                var("fieldName"),
                            ),
                        ),
                        var("rowType"),
                    ),
                ),
            ],
            Maybes.from_maybe(
                false(),
                Maybes.map(
                    lam(
                        "ft",
                        _kref.predicates_is_unit_type(_kref.strip_deannotate_type(Core.field_type_type(var("ft")))),
                    ),
                    var("mfield"),
                ),
            ),
        ),
    )
    return _def(
        "isVariantUnitType",
        doc("Check if a variant field has unit type", body),
    )


def _dataclass_decorator():
    inner_atom = PySyn.atom_name(
        wrap("hydra.python.syntax.Name", string("dataclass"))
    )
    primary = PySyn.primary_simple(inner_atom)
    kwarg = PySyn.kwarg_or_starred_kwarg(
        PySyn.kwarg(
            wrap("hydra.python.syntax.Name", string("frozen")),
            _kref.utils_py_atom_to_py_expression(PySyn.atom_true),
        )
    )
    args_term = PySyn.args(
        list_([]),
        list_([kwarg]),
        list_([]),
    )
    rhs = PySyn.primary_rhs_call(args_term)
    body = PySyn.named_expression_simple(
        _kref.utils_py_primary_to_py_expression(_kref.utils_primary_with_rhs(primary, rhs))
    )
    return _def(
        "dataclassDecorator",
        doc("Create a @dataclass(frozen=True) decorator", body),
    )


def _make_curried_lambda():
    body = lambdas(
        ["params", "body"],
        Lists.foldl(
            lambdas(
                ["acc", "p"],
                PySyn.expression_lambda(
                    PySyn.lambda_(
                        PyDsl.lambda_parameters_simple(
                            list_(
                                [PySyn.lambda_param_no_default(var("p"))]
                            )
                        ),
                        var("acc"),
                    )
                ),
            ),
            var("body"),
            Lists.reverse(var("params")),
        ),
    )
    return _def(
        "makeCurriedLambda",
        doc(
            "Create a curried lambda chain from a list of parameter names and a body",
            body,
        ),
    )


def _make_py_graph():
    body = lambdas(
        ["g", "m"],
        record("hydra.python.environment.PyGraph",
            [
                field("graph", var("g")),
                field("metadata", var("m")),
            ],
        ),
    )
    return _def(
        "makePyGraph",
        doc("Constructor for PyGraph record", body),
    )


def _make_simple_lambda():
    body = lambdas(
        ["arity", "lhs"],
        let_chain(
            [
                (
                    "args",
                    Lists.map(
                        lam(
                            "i",
                            _py_name(
                                Strings.cat2(
                                    string("x"),
                                    Literals.show_int32(var("i")),
                                )
                            ),
                        ),
                        Math.range_(int32(1), var("arity")),
                    ),
                ),
            ],
            Logic.if_else(
                Equality.equal(var("arity"), int32(0)),
                var("lhs"),
                PySyn.expression_lambda(
                    PySyn.lambda_(
                        PyDsl.lambda_parameters_simple(
                            Lists.map(
                                lam(
                                    "a",
                                    PySyn.lambda_param_no_default(var("a")),
                                ),
                                var("args"),
                            )
                        ),
                        _kref.utils_function_call(_kref.utils_py_expression_to_py_primary(var("lhs")), Lists.map(
                                lam(
                                    "a",
                                    PyDsl.py_name_to_py_expression(var("a")),
                                ),
                                var("args"),
                            )),
                    )
                ),
            ),
        ),
    )
    return _def(
        "makeSimpleLambda",
        doc(
            "Wrap a bare reference to a polymorphic function in an uncurried lambda",
            body,
        ),
    )


def _make_thunk():
    body = lambdas(
        ["pbody"],
        _kref.utils_function_call(_kref.utils_py_expression_to_py_primary(_kref.utils_function_call(PySyn.primary_simple(PySyn.atom_name(_py_name("lru_cache"))), list_([_local("pyInt")(bigint(1))]))), list_([_local("wrapInNullaryLambda")(var("pbody"))])),
    )
    return _def(
        "makeThunk",
        doc(
            "Create a thunk (zero-argument lambda) wrapped with lru_cache(1) for memoization",
            body,
        ),
    )


def _make_lazy():
    body = lambdas(
        ["pbody"],
        _kref.utils_function_call(PySyn.primary_simple(PySyn.atom_name(_py_name("Lazy"))), list_([_local("wrapInNullaryLambda")(var("pbody"))])),
    )
    return _def(
        "makeLazy",
        doc(
            "Wrap an expression in Lazy(lambda: ...) for one-shot lazy memoization",
            body,
        ),
    )


def _make_uncurried_lambda():
    body = lambdas(
        ["params", "body"],
        PySyn.expression_lambda(
            PySyn.lambda_(
                PyDsl.lambda_parameters_simple(
                    Lists.map(
                        lam(
                            "p",
                            PySyn.lambda_param_no_default(var("p")),
                        ),
                        var("params"),
                    )
                ),
                var("body"),
            )
        ),
    )
    return _def(
        "makeUncurriedLambda",
        doc(
            "Create an uncurried lambda with multiple parameters", body
        ),
    )


def _module_imports():
    body = lambdas(
        ["namespaces", "meta"],
        Lists.map(
            lam(
                "imp",
                _kref.utils_py_simple_statement_to_py_statement(PySyn.simple_statement_import(var("imp"))),
            ),
            Lists.concat(
                list_(
                    [
                        _local("moduleStandardImports")(var("meta")),
                        _local("moduleDomainImports")(var("namespaces")),
                    ]
                )
            ),
        ),
    )
    return _def(
        "moduleImports",
        doc("Generate all import statements for a Python module", body),
    )


def _module_standard_imports():
    def cond(symbol, flag_field):
        return _local("condImportSymbol")(string(symbol), _meta_proj(flag_field, "meta"))

    pairs = [
        pair(
            string("__future__"),
            list_(
                [
                    _local("condImportSymbol")(string("annotations"), _kref.names_use_future_annotations)
                ]
            ),
        ),
        pair(
            string("collections.abc"),
            list_(
                [
                    cond("Callable", "usesCallable"),
                    cond("Mapping", "usesFrozenDict"),
                    cond("Sequence", "usesFrozenList"),
                    cond("Set", "usesFrozenSet"),
                ]
            ),
        ),
        pair(
            string("dataclasses"),
            list_([cond("dataclass", "usesDataclass")]),
        ),
        pair(
            string("decimal"),
            list_([cond("Decimal", "usesDecimal")]),
        ),
        pair(
            string("enum"),
            list_([cond("Enum", "usesEnum")]),
        ),
        pair(
            string("functools"),
            list_([cond("lru_cache", "usesLruCache")]),
        ),
        pair(
            string("hydra.dsl.python"),
            list_(
                [
                    cond("Either", "usesEither"),
                    cond("Just", "usesJust"),
                    cond("Left", "usesLeft"),
                    cond("Maybe", "usesMaybe"),
                    cond("Node", "usesNode"),
                    cond("Nothing", "usesNothing"),
                    cond("Right", "usesRight"),
                ]
            ),
        ),
        pair(
            string("hydra.python.util"),
            list_(
                [
                    cond("ConsList", "usesFrozenList"),
                    # Lazy is unconditionally imported: every module with
                    # let-bindings needs it, and the cost of an unused
                    # import is negligible.
                    _local("condImportSymbol")(string("Lazy"), boolean(True)),
                    cond("PersistentMap", "usesFrozenDict"),
                    cond("PersistentSet", "usesFrozenSet"),
                ]
            ),
        ),
        pair(
            string("typing"),
            list_(
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
    body = lambdas(
        ["meta"],
        let_chain(
            [
                ("pairs", list_(pairs)),
                (
                    "simplified",
                    Maybes.cat(
                        Lists.map(
                            lam(
                                "p",
                                let_chain(
                                    [
                                        ("modName", Pairs.first(var("p"))),
                                        (
                                            "symbols",
                                            Maybes.cat(Pairs.second(var("p"))),
                                        ),
                                    ],
                                    Logic.if_else(
                                        Lists.null(var("symbols")),
                                        nothing(),
                                        just(
                                            pair(
                                                var("modName"),
                                                var("symbols"),
                                            )
                                        ),
                                    ),
                                ),
                            ),
                            var("pairs"),
                        )
                    ),
                ),
            ],
            Lists.map(
                lam(
                    "p",
                    _local("standardImportStatement")(Pairs.first(var("p")), Pairs.second(var("p"))),
                ),
                var("simplified"),
            ),
        ),
    )
    return _def(
        "moduleStandardImports",
        doc(
            "Generate standard import statements based on module metadata", body
        ),
    )


def _module_domain_imports():
    body = lambdas(
        ["namespaces"],
        let_chain(
            [
                (
                    "names",
                    Lists.sort(
                        Maps.elems(Util.module_names_mapping(var("namespaces")))
                    ),
                ),
            ],
            Lists.map(
                lam(
                    "ns",
                    inject("hydra.python.syntax.ImportStatement",
                        Name("name"),
                        wrap("hydra.python.syntax.ImportName",
                            list_(
                                [
                                    record("hydra.python.syntax.DottedAsName",
                                        [
                                            field("name", var("ns")),
                                            field("as", nothing()),
                                        ],
                                    )
                                ]
                            ),
                        ),
                    ),
                ),
                var("names"),
            ),
        ),
    )
    return _def(
        "moduleDomainImports",
        doc("Generate domain import statements from namespace mappings", body),
    )


def _module_to_python():
    body = lambdas(
        ["mod", "defs", "cx", "g"],
        Eithers.bind(
            _local("encodePythonModule")(var("cx"), var("g"), var("mod"), var("defs")),
            lam(
                "file",
                let_chain(
                    [
                        (
                            "s",
                            var("hydra.serialization.printExpr")(var("hydra.serialization.parenthesize")(var("hydra.python.serde.moduleToExpr")(var("file")))),
                        ),
                        (
                            "path",
                            var("hydra.names.moduleNameToFilePath")(_kref.util_case_convention_lower_snake, wrap("hydra.packaging.FileExtension",
                                    string("py"),
                                ), Pkg.module_name(var("mod"))),
                        ),
                    ],
                    right(
                        Maps.singleton(var("path"), var("s"))
                    ),
                ),
            ),
        ),
    )
    return _def(
        "moduleToPython",
        doc("Convert a Hydra module to Python source files", body),
    )


def _python_binding_metadata():
    meta_true = Core.term_literal(Core.literal_boolean(true()))
    body = lambdas(
        ["g", "b"],
        Logic.if_else(
            _local("shouldThunkBinding")(var("g"), var("b")),
            Logic.if_else(
                _kref.predicates_is_complex_binding(var("g"), var("b")),
                just(meta_true),
                nothing(),
            ),
            nothing(),
        ),
    )
    return _def(
        "pythonBindingMetadata",
        doc(
            "Like bindingMetadata, but only for bindings that will actually be thunked",
            body,
        ),
    )


def _lazy_dot_get():
    body = lambdas(
        ["expr"],
        _kref.utils_function_call(_kref.utils_py_expression_to_py_primary(_kref.utils_project_from_expression(var("expr"), PySyn.name(string("get")))), list_([])),
    )
    return _def(
        "lazyDotGet",
        doc(
            "Wrap an expression in a .get() method call (for Lazy unwrap at use sites)",
            body,
        ),
    )


def _lru_cache_decorator():
    inner_atom = PySyn.atom_name(
        wrap("hydra.python.syntax.Name", string("lru_cache"))
    )
    primary = PySyn.primary_simple(inner_atom)
    one_arg = list_([_local("pyInt")(bigint(1))])
    body = PySyn.named_expression_simple(
        _kref.utils_function_call(primary, one_arg)
    )
    return _def(
        "lruCacheDecorator",
        doc(
            "Decorator for @lru_cache(1) to memoize zero-argument function results",
            body,
        ),
    )


def _py_graph_graph():
    body = lambdas(["pyg"], _pygraph("graph", "pyg"))
    return _def(
        "pyGraphGraph",
        doc("Accessor for the graph field of PyGraph", body),
    )


def _py_graph_metadata():
    body = lambdas(["pyg"], _pygraph("metadata", "pyg"))
    return _def(
        "pyGraphMetadata",
        doc("Accessor for the metadata field of PyGraph", body),
    )


def _py_int():
    body = lambdas(
        ["n"],
        _kref.utils_py_atom_to_py_expression(PySyn.atom_number(PySyn.number_integer(var("n")))),
    )
    return _def(
        "pyInt",
        doc("Create integer literal expression", body),
    )


def _python_environment_get_graph():
    body = lambdas(["env"], _env("graph", "env"))
    return _def(
        "pythonEnvironmentGetGraph",
        doc("Get the Graph from a PythonEnvironment", body),
    )


def _python_environment_set_graph():
    body = lambdas(
        ["tc", "env"],
        record("hydra.python.environment.PythonEnvironment",
            [
                field("namespaces", _env("namespaces", "env")),
                field("boundTypeVariables", _env("boundTypeVariables", "env")),
                field("graph", var("tc")),
                field("nullaryBindings", _env("nullaryBindings", "env")),
                field("version", _env("version", "env")),
                field("skipCasts", _env("skipCasts", "env")),
                field("inlineVariables", _env("inlineVariables", "env")),
            ],
        ),
    )
    return _def(
        "pythonEnvironmentSetGraph",
        doc("Set the Graph in a PythonEnvironment", body),
    )


def _target_python_version():
    return _def(
        "targetPythonVersion",
        doc(
            "The target Python version for code generation",
            _kref.utils_target_python_version,
        ),
    )


def _use_inline_type_params():
    return _def(
        "useInlineTypeParams",
        doc(
            "Legacy constant for backward compatibility; use useInlineTypeParamsFor in new code",
            _local("useInlineTypeParamsFor")(_kref.utils_target_python_version),
        ),
    )


def _use_inline_type_params_for():
    body = lambdas(
        ["version"],
        Equality.equal(
            var("version"),
            inject_unit("hydra.python.environment.PythonVersion", Name("python312")
            ),
        ),
    )
    return _def(
        "useInlineTypeParamsFor",
        doc("Version-aware inline type parameters", body),
    )


def _set_meta_namespaces():
    body = lambdas(
        ["ns", "m"],
        _meta_record_with_field_set("namespaces", var("ns"), m_var="m"),
    )
    return _def("setMetaNamespaces", body)


def _set_meta_type_variables():
    body = lambdas(
        ["m", "tvars"],
        _meta_record_with_field_set("typeVariables", var("tvars"), m_var="m"),
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
    body = lambdas(
        ["g", "b"],
        Logic.and_(
            _kref.predicates_is_complex_binding(var("g"), var("b")),
            Logic.not_(
                _kref.predicates_is_trivial_term(Core.binding_term(var("b")))
            ),
        ),
    )
    return _def(
        "shouldThunkBinding",
        doc(
            "Determine if a binding should be thunked based on its complexity and triviality",
            body,
        ),
    )


def _standard_import_statement():
    body = lambdas(
        ["modName", "symbols"],
        inject("hydra.python.syntax.ImportStatement",
            Name("from"),
            record("hydra.python.syntax.ImportFrom",
                [
                    field("prefixes", list_([])),
                    field("dottedName",
                        just(
                            wrap("hydra.python.syntax.DottedName",
                                list_([_py_name(var("modName"))]),
                            )
                        ),
                    ),
                    field("targets",
                        inject("hydra.python.syntax.ImportFromTargets",
                            Name("simple"),
                            Lists.map(
                                lam(
                                    "s",
                                    record("hydra.python.syntax.ImportFromAsName",
                                        [
                                            field("name", _py_name(var("s"))
                                            ),
                                            field("as", nothing()),
                                        ],
                                    ),
                                ),
                                var("symbols"),
                            ),
                        ),
                    ),
                ],
            ),
        ),
    )
    return _def(
        "standardImportStatement",
        doc("Generate a single from-import statement", body),
    )


def _term_arity_with_primitives():
    body = lambdas(
        ["graph", "term"],
        cases_with_default("hydra.core.Term", _kref.strip_deannotate_and_detype_term(var("term")), int_(0),
            field("application",
                    lam(
                        "app",
                        Math.max_(
                            int_(0),
                            Math.sub(
                                _local("termArityWithPrimitives")(var("graph"), Core.application_function(var("app"))),
                                int_(1),
                            ),
                        ),
                    ),
                ),
            field("lambda",
                    lam(
                        "lam",
                        Math.add(
                            int_(1),
                            _local("termArityWithPrimitives")(var("graph"), Core.lambda_body(var("lam"))),
                        ),
                    ),
                ),
            field("project", constant(int_(1))
                ),
            field("unwrap", constant(int_(1))
                ),
            field("cases", constant(int_(1))
                ),
            field("variable",
                    lam(
                        "name",
                        Maybes.maybe(
                            int_(0),
                            lam(
                                "el",
                                Maybes.maybe(
                                    _kref.arity_term_arity(Core.binding_term(var("el"))),
                                    lam(
                                        "ts",
                                        _kref.arity_type_scheme_arity(var("ts")),
                                    ),
                                    Core.binding_type_scheme(var("el")),
                                ),
                            ),
                            _kref.lexical_lookup_binding(var("graph"), var("name")),
                        ),
                    ),
                )),
    )
    return _def(
        "termArityWithPrimitives",
        doc("Calculate term arity with proper primitive handling", body),
    )


def _tvar_statement():
    body = lambdas(
        ["name"],
        _kref.utils_assignment_statement(var("name"), _kref.utils_function_call(PySyn.primary_simple(PySyn.atom_name(_py_name("TypeVar"))), list_(
                    [
                        _kref.utils_double_quoted_string(unwrap("hydra.python.syntax.Name")(var("name")))
                    ]
                ))),
    )
    return _def(
        "tvarStatement",
        doc(
            "Create a TypeVar assignment statement for a type variable name", body
        ),
    )


def _type_alias_statement_for():
    body = lambdas(
        ["env", "name", "tparams", "mcomment", "tyexpr"],
        Logic.if_else(
            _local("useInlineTypeParamsFor")(_env("version", "env")),
            _kref.utils_type_alias_statement(var("name"), var("tparams"), var("mcomment"), var("tyexpr")),
            _kref.utils_type_alias_statement310(var("name"), var("tparams"), var("mcomment"), var("tyexpr")),
        ),
    )
    return _def(
        "typeAliasStatementFor",
        doc("Version-aware type alias statement generation", body),
    )


def _union_type_statements_for():
    body = lambdas(
        ["env", "name", "tparams", "mcomment", "tyexpr", "extraStmts"],
        Logic.if_else(
            _local("useInlineTypeParamsFor")(_env("version", "env")),
            Lists.concat2(
                list_(
                    [
                        _kref.utils_type_alias_statement(var("name"), var("tparams"), var("mcomment"), var("tyexpr"))
                    ]
                ),
                var("extraStmts"),
            ),
            _kref.utils_union_type_class_statements310(var("name"), var("mcomment"), var("tyexpr"), var("extraStmts")),
        ),
    )
    return _def(
        "unionTypeStatementsFor",
        doc("Version-aware union type statement generation", body),
    )


def _unsupported_expression():
    body = lambdas(
        ["msg"],
        _kref.utils_function_call(_kref.utils_py_expression_to_py_primary(_kref.utils_project_from_expression(_kref.utils_project_from_expression(_kref.utils_project_from_expression(PyDsl.py_name_to_py_expression(_py_name("hydra")), _py_name("dsl")), _py_name("python")), _py_name("unsupported"))), list_(
                [
                    _kref.utils_string_to_py_expression(PySyn.quote_style_double, var("msg"))
                ]
            )),
    )
    return _def(
        "unsupportedExpression",
        doc(
            "Create an expression that calls hydra.dsl.python.unsupported(message) at runtime",
            body,
        ),
    )


def _variant_args():
    body = lambdas(
        ["ptype", "tparams"],
        _kref.utils_py_expressions_to_py_args(Maybes.cat(
                list_(
                    [
                        just(
                            _kref.utils_py_primary_to_py_expression(_kref.utils_primary_with_expression_slices(PySyn.primary_simple(PySyn.atom_name(_py_name("Node"))), list_([var("ptype")])))
                        ),
                        _local("genericArg")(var("tparams")),
                    ]
                )
            )),
    )
    return _def(
        "variantArgs",
        doc("Create args for variant (Node[type], Generic[tparams])", body),
    )


def _variant_closed_pattern():
    body = lambdas(
        ["env", "typeName", "fieldName", "pyVariantName", "rowType", "isEnum",
         "varName", "shouldCapture"],
        Logic.if_else(
            var("isEnum"),
            _local("enumVariantPattern")(var("env"), var("typeName"), var("fieldName")),
            Logic.if_else(
                Logic.not_(var("shouldCapture")),
                _local("classVariantPatternUnit")(var("pyVariantName")),
                _local("classVariantPatternWithCapture")(var("env"), var("pyVariantName"), var("varName")),
            ),
        ),
    )
    return _def(
        "variantClosedPattern",
        doc(
            "Create a ClosedPattern for a variant based on its characteristics", body
        ),
    )


def _wildcard_case_block():
    body = lambdas(
        ["stmt"],
        PySyn.case_block(
            _kref.utils_py_closed_pattern_to_py_patterns(PySyn.closed_pattern_wildcard),
            nothing(),
            _kref.utils_indented_block(nothing(), list_([list_([var("stmt")])])),
        ),
    )
    return _def(
        "wildcardCaseBlock",
        doc("Create a wildcard case block with a given body statement", body),
    )


def _with_lambda():
    body = _kref.environment_with_lambda_context(_local("pythonEnvironmentGetGraph"), _local("pythonEnvironmentSetGraph"))
    return _def(
        "withLambda",
        doc(
            "Execute a computation with lambda context (adds lambda parameter to Graph)",
            body,
        ),
    )


def _with_definitions():
    body = lambdas(
        ["env", "defs", "body"],
        let_chain(
            [
                (
                    "bindings",
                    Maybes.cat(
                        Lists.map(
                            lam(
                                "def_",
                                cases_with_default("hydra.packaging.Definition", var("def_"), nothing(),
            field("term",
                                            lam(
                                                "td",
                                                just(
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
                                                        Maybes.map(
                                                            lam("sig", _kref.scoping_term_signature_to_type_scheme(var("sig"))),
                                                            _proj(
                                                                "hydra.packaging.TermDefinition",
                                                                "signature",
                                                                "td",
                                                            ),
                                                        ),
                                                    )
                                                ),
                                            ),
                                        ),
            field("type",
                                            constant(nothing()),
                                        )),
                            ),
                            var("defs"),
                        )
                    ),
                ),
                (
                    "dummyLet",
                    getattr(Core, "let")(
                        var("bindings"),
                        Core.term_literal(Core.literal_string(string("dummy"))),
                    ),
                ),
            ],
            _local("withLet")(var("env"), var("dummyLet"), var("body")),
        ),
    )
    return _def(
        "withDefinitions",
        doc("Execute a computation with definitions in scope", body),
    )


def _with_let():
    body = _kref.environment_with_let_context(_local("pythonEnvironmentGetGraph"), _local("pythonEnvironmentSetGraph"), _local("pythonBindingMetadata"))
    return _def(
        "withLet",
        doc(
            "Execute a computation with let context (adds let bindings to Graph)",
            body,
        ),
    )


def _with_let_inline():
    inner_lambda = lam(
        "innerEnv",
        let_chain(
            [
                (
                    "updatedEnv",
                    record("hydra.python.environment.PythonEnvironment",
                        [
                            field("namespaces", _env("namespaces", "innerEnv")
                            ),
                            field("boundTypeVariables",
                                _env("boundTypeVariables", "innerEnv"),
                            ),
                            field("graph", _env("graph", "innerEnv")),
                            field("nullaryBindings",
                                _env("nullaryBindings", "innerEnv"),
                            ),
                            field("version", _env("version", "innerEnv")),
                            field("skipCasts", _env("skipCasts", "innerEnv")
                            ),
                            field("inlineVariables",
                                Sets.union(
                                    var("inlineVars"),
                                    _env("inlineVariables", "innerEnv"),
                                ),
                            ),
                        ],
                    ),
                )
            ],
            var("body")(var("updatedEnv")),
        ),
    )
    body = lambdas(
        ["env", "lt", "body"],
        let_chain(
            [
                (
                    "bindingNames",
                    Lists.map(
                        lam(
                            "b", Core.binding_name(var("b"))
                        ),
                        Core.let_bindings(var("lt")),
                    ),
                ),
                ("inlineVars", Sets.from_list(var("bindingNames"))),
                (
                    "noMetadata",
                    lambdas(["tc", "b"], nothing()),
                ),
            ],
            _kref.environment_with_let_context(_local("pythonEnvironmentGetGraph"), _local("pythonEnvironmentSetGraph"), var("noMetadata"), var("env"), var("lt"), inner_lambda),
        ),
    )
    return _def(
        "withLetInline",
        doc(
            "Execute a computation with inline let context (for walrus operators)", body
        ),
    )


def _with_type_lambda():
    body = _kref.environment_with_type_lambda_context(_local("pythonEnvironmentGetGraph"), _local("pythonEnvironmentSetGraph"))
    return _def(
        "withTypeLambda",
        doc("Execute a computation with type lambda context", body),
    )


def _lazy_flags_for_primitive():
    body = lambdas(
        ["g", "name"],
        Maybes.cases(
            Maps.lookup(var("name"), Graph_dsl.graph_primitives(var("g"))),
            list_([]),
            lam(
                "prim",
                lets(
                    [
                        field("def0", _proj("hydra.graph.Primitive", "definition", "prim")),
                    ],
                    lets(
                        [
                            field("sig", _proj("hydra.packaging.PrimitiveDefinition", "signature", "def0")),
                        ],
                        Lists.map(
                            lam("p", _proj("hydra.typing.Parameter", "isLazy", "p")),
                            _proj("hydra.typing.TermSignature", "parameters", "sig"),
                        ),
                    ),
                ),
            ),
        ),
    )
    return _def(
        "lazyFlagsForPrimitive",
        doc(
            "Per-parameter isLazy flags of a primitive (by name), or empty if not a primitive. "
            "Single source of truth for which arguments coders thunk; replaces hard-coded name tables (issue #391).",
            body,
        ),
    )


def _wrap_lazy_arguments():
    body = lambdas(
        ["g", "name", "args"],
        lets(
            [
                field("lazyFlags", _local("lazyFlagsForPrimitive")(var("g"), var("name"))),
            ],
            # Zip args with flags (truncates to the shorter list, so partially-applied
            # calls only consider supplied positions), wrapping where the flag is true.
            # The Python runtime accepts a value or thunk (callable() check).
            Lists.map(
                lam(
                    "pair",
                    Logic.if_else(
                        Pairs.second(var("pair")),
                        _local("wrapInNullaryLambda")(Pairs.first(var("pair"))),
                        Pairs.first(var("pair")),
                    ),
                ),
                Lists.zip(var("args"), var("lazyFlags")),
            ),
        ),
    )
    return _def(
        "wrapLazyArguments",
        doc(
            "Wrap lazy-flagged arguments of a primitive call in nullary lambdas, per isLazy metadata (issue #391)",
            body,
        ),
    )


def _wrap_in_nullary_lambda():
    body = lambdas(
        ["expr"],
        PySyn.expression_lambda(
            PySyn.lambda_(PyDsl.lambda_parameters_empty, var("expr"))
        ),
    )
    return _def(
        "wrapInNullaryLambda",
        doc(
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
        _PLACEHOLDER.name,
        _PLACEHOLDER.description,
        _PLACEHOLDER.dependencies,
        (
            _load_environment_reorder_defs(),
            to_definition(_analyze_python_function()),
            to_definition(_class_variant_pattern_unit()),
            to_definition(_class_variant_pattern_with_capture()),
            to_definition(_collect_type_variables()),
            to_definition(_cond_import_symbol()),
            to_definition(_dataclass_decorator()),
            to_definition(_deconflict_variant_name()),
            to_definition(_deduplicate_case_variables()),
            to_definition(_dig_for_wrap()),
            to_definition(_eliminate_unit_var()),
            to_definition(_empty_metadata()),
            to_definition(_encode_application()),
            to_definition(_encode_application_inner()),
            to_definition(_encode_application_type()),
            to_definition(_encode_binding_as()),
            to_definition(_encode_binding_as_assignment()),
            to_definition(_encode_bindings_as_defs()),
            to_definition(_case_block_to_expr()),
            to_definition(_encode_default_case_block()),
            to_definition(_encode_definition()),
            to_definition(_encode_enum_value_assignment()),
            to_definition(_encode_field()),
            to_definition(_encode_field_type()),
            to_definition(_encode_float_value()),
            to_definition(_encode_float_value_encode_float32()),
            to_definition(_encode_float_value_encode_float64()),
            to_definition(_encode_float_value_py_special_float()),
            to_definition(_encode_forall_type()),
            to_definition(_function_definition_to_expr()),
            to_definition(_encode_function_type()),
            to_definition(_encode_integer_value()),
            to_definition(_encode_literal()),
            to_definition(_encode_literal_type()),
            to_definition(_encode_name_constants()),
            to_definition(_encode_python_module()),
            to_definition(_encode_record_type()),
            to_definition(_encode_term_assignment()),
            to_definition(_encode_term_inline()),
            to_definition(_encode_term_multiline()),
            to_definition(_encode_term_multiline_tco()),
            to_definition(_encode_type()),
            to_definition(_encode_type_assignment()),
            to_definition(_encode_type_assignment_inner()),
            to_definition(_encode_type_def_single()),
            to_definition(_encode_type_quoted()),
            to_definition(_encode_union_elimination_inline()),
            to_definition(_encode_union_field()),
            to_definition(_encode_union_field_alt()),
            to_definition(_encode_union_type()),
            to_definition(_encode_variable()),
            to_definition(_encode_wrapped_type()),
            to_definition(_enum_variant_pattern()),
            to_definition(_environment_type_parameters()),
            to_definition(_extend_env_with_lambda_params()),
            to_definition(_extend_env_with_type_var()),
            to_definition(_extend_meta_for_term()),
            to_definition(_extend_meta_for_type()),
            to_definition(_extend_meta_for_types()),
            to_definition(_extract_case_elimination()),
            to_definition(_find_type_params()),
            to_definition(_gather_lambdas()),
            to_definition(_gather_metadata()),
            to_definition(_generic_arg()),
            to_definition(_initial_environment()),
            to_definition(_initial_metadata()),
            to_definition(_is_case_statement_application()),
            to_definition(_is_cases_full()),
            to_definition(_is_type_module_check()),
            to_definition(_is_type_variable_name()),
            to_definition(_is_variant_unit_type()),
            to_definition(_lazy_dot_get()),
            to_definition(_lazy_flags_for_primitive()),
            to_definition(_lru_cache_decorator()),
            to_definition(_make_curried_lambda()),
            to_definition(_make_lazy()),
            to_definition(_make_py_graph()),
            to_definition(_make_simple_lambda()),
            to_definition(_make_thunk()),
            to_definition(_make_uncurried_lambda()),
            to_definition(_module_domain_imports()),
            to_definition(_module_imports()),
            to_definition(_module_standard_imports()),
            to_definition(_module_to_python()),
            to_definition(_py_graph_graph()),
            to_definition(_py_graph_metadata()),
            to_definition(_py_int()),
            to_definition(_python_binding_metadata()),
            to_definition(_python_environment_get_graph()),
            to_definition(_python_environment_set_graph()),
            to_definition(_set_meta_namespaces()),
            to_definition(_set_meta_type_variables()),
            to_definition(_set_meta_uses_annotated()),
            to_definition(_set_meta_uses_callable()),
            to_definition(_set_meta_uses_cast()),
            to_definition(_set_meta_uses_dataclass()),
            to_definition(_set_meta_uses_decimal()),
            to_definition(_set_meta_uses_either()),
            to_definition(_set_meta_uses_enum()),
            to_definition(_set_meta_uses_frozen_dict()),
            to_definition(_set_meta_uses_frozen_list()),
            to_definition(_set_meta_uses_frozen_set()),
            to_definition(_set_meta_uses_generic()),
            to_definition(_set_meta_uses_just()),
            to_definition(_set_meta_uses_left()),
            to_definition(_set_meta_uses_lru_cache()),
            to_definition(_set_meta_uses_maybe()),
            to_definition(_set_meta_uses_name()),
            to_definition(_set_meta_uses_node()),
            to_definition(_set_meta_uses_nothing()),
            to_definition(_set_meta_uses_right()),
            to_definition(_set_meta_uses_type_alias()),
            to_definition(_set_meta_uses_type_var()),
            to_definition(_should_thunk_binding()),
            to_definition(_standard_import_statement()),
            to_definition(_target_python_version()),
            to_definition(_term_arity_with_primitives()),
            to_definition(_tvar_statement()),
            to_definition(_type_alias_statement_for()),
            to_definition(_union_type_statements_for()),
            to_definition(_unsupported_expression()),
            to_definition(_use_inline_type_params()),
            to_definition(_use_inline_type_params_for()),
            to_definition(_variant_args()),
            to_definition(_variant_closed_pattern()),
            to_definition(_wildcard_case_block()),
            to_definition(_with_definitions()),
            to_definition(_with_lambda()),
            to_definition(_with_let()),
            to_definition(_with_let_inline()),
            to_definition(_with_type_lambda()),
            to_definition(_wrap_in_nullary_lambda()),
            to_definition(_wrap_lazy_arguments()),
        ),
    )


module_ = _build_module()
