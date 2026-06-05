"""Python serializer: converts Python AST to concrete syntax (source code).

Mirror of packages/hydra-python/src/main/haskell/Hydra/Sources/Python/Serde.hs.
Serializes the Python syntax model into properly formatted Python source code.
"""

from hydra.core import Name
from hydra.dsl.python import Just, Nothing
from hydra.packaging import EntityMetadata, Module, ModuleName

import hydra.dsl.meta.lib.equality as Equality
import hydra.dsl.meta.lib.lists as Lists
import hydra.dsl.meta.lib.logic as Logic
import hydra.dsl.meta.lib.maybes as Maybes
import hydra.dsl.meta.lib.strings as Strings
from hydra.dsl.meta.phantoms import *  # noqa: F401,F403

from hydra.sources.python._kernel_refs import (
    serialization_braces_list_adaptive,
    serialization_bracket_list,
    serialization_bracket_list_adaptive,
    serialization_comma_sep,
    serialization_comma_sep_adaptive,
    serialization_cst,
    serialization_curly_braces_list,
    serialization_dot_sep,
    serialization_double_newline_sep,
    serialization_half_block_style,
    serialization_inline_style,
    serialization_newline_sep,
    serialization_no_sep,
    serialization_paren_list_adaptive,
    serialization_parens,
    serialization_semicolon_sep,
    serialization_space_sep,
    serialization_symbol_sep,
    serialization_tab_indent_double_space,
)


# ----------------------------------------------------------------------
# Module setup
# ----------------------------------------------------------------------

NS = ModuleName("hydra.python.serde")

from hydra.sources.python._source_dsl import (
    KERNEL_TYPES_NAMESPACES,
    make_def,
    make_local,
    proj as _proj_fq,
    unqualified_dep,
)

# Mirror Haskell:
#   [Constants.ns, Serialization.ns] L.++ (PySyntax.ns:KernelTypes.kernelTypesNamespaces)
DEPENDENCIES = [
    unqualified_dep(ModuleName("hydra.constants")),
    unqualified_dep(ModuleName("hydra.serialization")),
    unqualified_dep(ModuleName("hydra.python.syntax")),
] + KERNEL_TYPES_NAMESPACES


_PLACEHOLDER = Module(
    NS,
    Just(EntityMetadata(
        Just("Python serializer: converts Python AST to concrete syntax"),
        (),
        (),
        Nothing())),
    DEPENDENCIES,
    (),
)




_def = make_def(_PLACEHOLDER)
_local = make_local("hydra.python.serde")


def _flat_let(bindings, body):
    """Emit a single flat `let` with multiple bindings (matches Haskell `lets [...]`)."""
    return lets([field_op(name, val) for name, val in bindings], body)


def _proj(type_name: str, field_name: str, var_name: str):
    """Apply a project to a variable: project(hydra.python.syntax.TypeName, fieldName) @@ var(varName)."""
    return _proj_fq(f"hydra.python.syntax.{type_name}", field_name, var_name)


def _unwrap(type_name: str, var_name: str):
    return unwrap(Name(f"hydra.python.syntax.{type_name}"))(var(var_name))


def _cst(s: str):
    """Serialization.cst @@ string(s)"""
    return serialization_cst(string(s))


def _space_sep(items_list):
    """Serialization.spaceSep @@ list [items...]"""
    return serialization_space_sep(items_list)


def _newline_sep(items_list):
    return serialization_newline_sep(items_list)


def _no_sep(items_list):
    return serialization_no_sep(items_list)


def _comma_sep_inline(items_list):
    """Serialization.commaSep @@ Serialization.inlineStyle @@ items"""
    return serialization_comma_sep(serialization_inline_style, items_list)


def _comma_sep_adaptive(items_list):
    return serialization_comma_sep_adaptive(items_list)


# ----------------------------------------------------------------------
# Type-name shorthand (for cases / project)
# ----------------------------------------------------------------------

def _ty(name: str) -> Name:
    return Name(f"hydra.python.syntax.{name}")


# ----------------------------------------------------------------------
# Definitions (alphabetical)
# ----------------------------------------------------------------------


def _annotated_rhs_to_expr():
    body = _space_sep(list_([
        _cst("="),
        cases(
            _ty("AnnotatedRhs"), var("arhs"), Nothing(),
            [
                field("star",
                    lam(
                        "ses",
                        serialization_comma_sep(serialization_inline_style, Lists.map(_local("starExpressionToExpr"), var("ses"))),
                    ),
                ),
                field("yield",
                    lam("_", _cst("yield ...")),
                ),
            ],
        ),
    ]))
    return _def(
        "annotatedRhsToExpr",
        doc(
            "Serialize an annotated RHS",
            lambdas(["arhs"], body),
        ),
    )


def _annotated_statement_to_expr():
    body = lets(
        [
            field("doc_", _proj("AnnotatedStatement", "comment", "as_")),
            field("stmt", _proj("AnnotatedStatement", "statement", "as_")),
        ],
        _newline_sep(list_([
            serialization_cst(_local("toPythonComments")(var("doc_"))),
            _local("statementToExpr")(var("stmt")),
        ])),
    )
    return _def(
        "annotatedStatementToExpr",
        doc(
            "Serialize an annotated statement (with optional doc comment)",
            lambdas(["as_"], body),
        ),
    )


def _annotation_to_expr():
    body = _space_sep(list_([
        _cst(":"),
        _local("expressionToExpr")(_unwrap("Annotation", "ann")),
    ]))
    return _def(
        "annotationToExpr",
        doc(
            "Serialize a type annotation",
            lambdas(["ann"], body),
        ),
    )


def _args_to_expr():
    body = lets(
        [
            field("pos", _proj("Args", "positional", "args")),
            field("ks", _proj("Args", "kwargOrStarred", "args")),
            field("kss", _proj("Args", "kwargOrDoubleStarred", "args")),
        ],
        _comma_sep_adaptive(Lists.concat(list_([
            Lists.map(_local("posArgToExpr"), var("pos")),
            Lists.map(_local("kwargOrStarredToExpr"), var("ks")),
            Lists.map(_local("kwargOrDoubleStarredToExpr"), var("kss")),
        ]))),
    )
    return _def(
        "argsToExpr",
        doc(
            "Serialize function arguments",
            lambdas(["args"], body),
        ),
    )


def _assignment_to_expr():
    body = cases(
        _ty("Assignment"), var("a"), Nothing(),
        [
            field("typed", lam("t", _local("typedAssignmentToExpr")(var("t")))),
            field("untyped", lam("u", _local("untypedAssignmentToExpr")(var("u")))),
            field("aug", lam("_", _cst("... += ..."))),
        ],
    )
    return _def(
        "assignmentToExpr",
        doc(
            "Serialize an assignment",
            lambdas(["a"], body),
        ),
    )


def _assignment_expression_to_expr():
    body = lets(
        [
            field("name", _proj("AssignmentExpression", "name", "ae")),
            field("expr", _proj("AssignmentExpression", "expression", "ae")),
        ],
        _space_sep(list_([
            _local("nameToExpr")(var("name")),
            _cst(":="),
            _local("expressionToExpr")(var("expr")),
        ])),
    )
    return _def(
        "assignmentExpressionToExpr",
        doc(
            "Serialize an assignment expression (walrus operator)",
            lambdas(["ae"], body),
        ),
    )


def _atom_to_expr():
    body = cases(
        _ty("Atom"), var("atom"), Nothing(),
        [
            field("dict", lam("d", _local("dictToExpr")(var("d")))),
            field("dictcomp", lam("_", _cst("{...}"))),
            field("ellipsis", constant(_cst("..."))),
            field("false", constant(_cst("False"))),
            field("genexp", lam("_", _cst("(...)"))),
            field("group", lam("g", _local("groupToExpr")(var("g")))),
            field("list", lam("l", _local("listToExpr")(var("l")))),
            field("listcomp", lam("_", _cst("[...]"))),
            field("name", lam("n", _local("nameToExpr")(var("n")))),
            field("none", constant(_cst("None"))),
            field("number", lam("n", _local("numberToExpr")(var("n")))),
            field("set", lam("s", _local("setToExpr")(var("s")))),
            field("setcomp", lam("_", _cst("{...}"))),
            field("string", lam("s", _local("stringToExpr")(var("s")))),
            field("true", constant(_cst("True"))),
            field("tuple", lam("t", _local("tupleToExpr")(var("t")))),
        ],
    )
    return _def(
        "atomToExpr",
        doc(
            "Serialize a Python atom (literal or basic expression)",
            lambdas(["atom"], body),
        ),
    )


def _attribute_to_expr():
    body = serialization_dot_sep(Lists.map(_local("nameToExpr"), _unwrap("Attribute", "attr")))
    return _def(
        "attributeToExpr",
        doc(
            "Serialize an attribute access",
            lambdas(["attr"], body),
        ),
    )


def _await_primary_to_expr():
    body = lets(
        [
            field("await_", _proj("AwaitPrimary", "await", "ap")),
            field("primary", _proj("AwaitPrimary", "primary", "ap")),
        ],
        Logic.if_else(
            var("await_"),
            _space_sep(list_([
                _cst("await"),
                _local("primaryToExpr")(var("primary")),
            ])),
            _local("primaryToExpr")(var("primary")),
        ),
    )
    return _def(
        "awaitPrimaryToExpr",
        doc(
            "Serialize an await primary expression",
            lambdas(["ap"], body),
        ),
    )


def _bitwise_op_body(local_name, type_name, lhs_local, rhs_local, op_str):
    return lets(
        [
            field("lhs", _proj(type_name, "lhs", lhs_local)),
            field("rhs", _proj(type_name, "rhs", lhs_local)),
        ],
        serialization_space_sep(Maybes.cat(list_([
            Maybes.map(
                lam("l", _space_sep(list_([
                    _local(local_name)(var("l")),
                    _cst(op_str),
                ]))),
                var("lhs"),
            ),
            just(_local(rhs_local)(var("rhs"))),
        ]))),
    )


def _bitwise_and_to_expr():
    return _def(
        "bitwiseAndToExpr",
        doc(
            "Serialize a bitwise AND expression",
            lambdas(["band"], _bitwise_op_body(
                "bitwiseAndToExpr", "BitwiseAnd", "band", "shiftExpressionToExpr", "&",
            )),
        ),
    )


def _bitwise_or_to_expr():
    return _def(
        "bitwiseOrToExpr",
        doc(
            "Serialize a bitwise OR expression",
            lambdas(["bor"], _bitwise_op_body(
                "bitwiseOrToExpr", "BitwiseOr", "bor", "bitwiseXorToExpr", "|",
            )),
        ),
    )


def _bitwise_xor_to_expr():
    return _def(
        "bitwiseXorToExpr",
        doc(
            "Serialize a bitwise XOR expression",
            lambdas(["bxor"], _bitwise_op_body(
                "bitwiseXorToExpr", "BitwiseXor", "bxor", "bitwiseAndToExpr", "^",
            )),
        ),
    )


def _block_to_expr():
    body = cases(
        _ty("Block"), var("b"), Nothing(),
        [
            field("indented",
                lam(
                    "groups",
                    serialization_tab_indent_double_space(Lists.map(
                            lam(
                                "stmts",
                                _newline_sep(Lists.map(_local("statementToExpr"), var("stmts"))),
                            ),
                            var("groups"),
                        )),
                ),
            ),
            field("simple",
                lam(
                    "ss",
                    serialization_semicolon_sep(Lists.map(_local("simpleStatementToExpr"), var("ss"))),
                ),
            ),
        ],
    )
    return _def(
        "blockToExpr",
        doc(
            "Serialize a block",
            lambdas(["b"], body),
        ),
    )


def _capture_pattern_to_expr():
    body = _local("patternCaptureTargetToExpr")(_unwrap("CapturePattern", "cp"))
    return _def(
        "capturePatternToExpr",
        doc(
            "Serialize a capture pattern",
            lambdas(["cp"], body),
        ),
    )


def _case_block_to_expr():
    body = lets(
        [
            field("patterns", _proj("CaseBlock", "patterns", "cb")),
            field("guard", _proj("CaseBlock", "guard", "cb")),
            field("body", _proj("CaseBlock", "body", "cb")),
        ],
        _newline_sep(list_([
            _no_sep(list_([
                serialization_space_sep(Maybes.cat(list_([
                    just(_cst("case")),
                    just(_local("patternsToExpr")(var("patterns"))),
                    Maybes.map(_local("guardToExpr"), var("guard")),
                ]))),
                _cst(":"),
            ])),
            _local("blockToExpr")(var("body")),
        ])),
    )
    return _def(
        "caseBlockToExpr",
        doc(
            "Serialize a case block",
            lambdas(["cb"], body),
        ),
    )


def _class_definition_to_expr():
    body = lets(
        [
            field("decs", _proj("ClassDefinition", "decorators", "cd")),
            field("name", _proj("ClassDefinition", "name", "cd")),
            field("args", _proj("ClassDefinition", "arguments", "cd")),
            field("body", _proj("ClassDefinition", "body", "cd")),
            field("argPart",
                Maybes.map(
                    lam("a", _no_sep(list_([
                        _cst("("),
                        _local("argsToExpr")(var("a")),
                        _cst(")"),
                    ]))),
                    var("args"),
                ),
            ),
        ],
        serialization_newline_sep(Maybes.cat(list_([
            Maybes.map(_local("decoratorsToExpr"), var("decs")),
            just(serialization_no_sep(Maybes.cat(list_([
                just(_space_sep(list_([
                    _cst("class"),
                    _local("nameToExpr")(var("name")),
                ]))),
                var("argPart"),
                just(_cst(":")),
            ])))),
            just(_local("blockToExpr")(var("body"))),
        ]))),
    )
    return _def(
        "classDefinitionToExpr",
        doc(
            "Serialize a class definition",
            lambdas(["cd"], body),
        ),
    )


def _class_pattern_to_expr():
    body = lets(
        [
            field("noa", _proj("ClassPattern", "nameOrAttribute", "cp")),
            field("pos", _proj("ClassPattern", "positionalPatterns", "cp")),
            field("kw", _proj("ClassPattern", "keywordPatterns", "cp")),
        ],
        serialization_no_sep(Maybes.cat(list_([
            just(_local("nameOrAttributeToExpr")(var("noa"))),
            just(_cst("(")),
            Maybes.map(_local("positionalPatternsToExpr"), var("pos")),
            Maybes.map(_local("keywordPatternsToExpr"), var("kw")),
            just(_cst(")")),
        ]))),
    )
    return _def(
        "classPatternToExpr",
        doc(
            "Serialize a class pattern",
            lambdas(["cp"], body),
        ),
    )


def _closed_pattern_to_expr():
    body = cases(
        _ty("ClosedPattern"), var("cp"), Nothing(),
        [
            field("literal", lam("_", _cst("..."))),
            field("capture", lam("c", _local("capturePatternToExpr")(var("c")))),
            field("wildcard", constant(_cst("_"))),
            field("value", lam("v", _local("valuePatternToExpr")(var("v")))),
            field("group", lam("_", _cst("(...)"))),
            field("sequence", lam("_", _cst("[...]"))),
            field("mapping", lam("_", _cst("{...}"))),
            field("class", lam("c", _local("classPatternToExpr")(var("c")))),
        ],
    )
    return _def(
        "closedPatternToExpr",
        doc(
            "Serialize a closed pattern",
            lambdas(["cp"], body),
        ),
    )


def _compare_op_bitwise_or_pair_to_expr():
    body = _space_sep(list_([
        serialization_cst(_local("compareOpToString")(project(_ty("CompareOpBitwiseOrPair"), Name("operator"))(var("pair")))),
        _local("bitwiseOrToExpr")(project(_ty("CompareOpBitwiseOrPair"), Name("rhs"))(var("pair"))),
    ]))
    return _def(
        "compareOpBitwiseOrPairToExpr",
        doc(
            "Serialize a (compare-op, bitwise-or) pair as `<op> <rhs>`",
            lambdas(["pair"], body),
        ),
    )


def _compare_op_to_string():
    body = cases(
        _ty("CompareOp"), var("op"), Nothing(),
        [
            field("eq", constant(string("=="))),
            field("noteq", constant(string("!="))),
            field("lte", constant(string("<="))),
            field("lt", constant(string("<"))),
            field("gte", constant(string(">="))),
            field("gt", constant(string(">"))),
            field("notin", constant(string("not in"))),
            field("in", constant(string("in"))),
            field("isnot", constant(string("is not"))),
            field("is", constant(string("is"))),
        ],
    )
    return _def(
        "compareOpToString",
        doc(
            "Render a Python comparison operator to its source-code form",
            lambdas(["op"], body),
        ),
    )


def _comparison_to_expr():
    body = _flat_let(
        [
            ("lhs", _proj("Comparison", "lhs", "cmp")),
            ("rhs", _proj("Comparison", "rhs", "cmp")),
        ],
        Logic.if_else(
            Lists.null(var("rhs")),
            _local("bitwiseOrToExpr")(var("lhs")),
            _space_sep(
                Lists.cons(
                    _local("bitwiseOrToExpr")(var("lhs")),
                    Lists.map(_local("compareOpBitwiseOrPairToExpr"), var("rhs")),
                ),
            ),
        ),
    )
    return _def(
        "comparisonToExpr",
        doc(
            "Serialize a comparison expression: `<lhs>` if rhs is empty, otherwise `<lhs> <op1> <rhs1> <op2> <rhs2> ...`",
            lambdas(["cmp"], body),
        ),
    )


def _compound_statement_to_expr():
    body = cases(
        _ty("CompoundStatement"), var("cs"), Nothing(),
        [
            field("function", lam("f", _local("functionDefinitionToExpr")(var("f")))),
            field("if", lam("_", _cst("if ..."))),
            field("classDef", lam("c", _local("classDefinitionToExpr")(var("c")))),
            field("with", lam("_", _cst("with ..."))),
            field("for", lam("_", _cst("for ..."))),
            field("try", lam("_", _cst("try ..."))),
            field("while", lam("w", _local("whileStatementToExpr")(var("w")))),
            field("match", lam("m", _local("matchStatementToExpr")(var("m")))),
        ],
    )
    return _def(
        "compoundStatementToExpr",
        doc(
            "Serialize a compound (multi-line) Python statement",
            lambdas(["cs"], body),
        ),
    )


def _conditional_to_expr():
    body = lets(
        [
            field("body", _proj("Conditional", "body", "c")),
            field("cond", _proj("Conditional", "if", "c")),
            field("elseExpr", _proj("Conditional", "else", "c")),
        ],
        _space_sep(list_([
            _local("disjunctionToExpr")(var("body")),
            _cst("if"),
            _local("disjunctionToExpr")(var("cond")),
            _cst("else"),
            _local("expressionToExpr")(var("elseExpr")),
        ])),
    )
    return _def(
        "conditionalToExpr",
        doc(
            "Serialize a conditional expression (ternary)",
            lambdas(["c"], body),
        ),
    )


def _conjunction_to_expr():
    body = serialization_symbol_sep(string("and"), serialization_inline_style, Lists.map(_local("inversionToExpr"), _unwrap("Conjunction", "c")))
    return _def(
        "conjunctionToExpr",
        doc(
            "Serialize a conjunction (and expression)",
            lambdas(["c"], body),
        ),
    )


def _decorators_to_expr():
    body = serialization_newline_sep(Lists.map(
            lam(
                "ne",
                _no_sep(list_([
                    _cst("@"),
                    _local("namedExpressionToExpr")(var("ne")),
                ])),
            ),
            _unwrap("Decorators", "decs"),
        ))
    return _def(
        "decoratorsToExpr",
        doc(
            "Serialize decorators",
            lambdas(["decs"], body),
        ),
    )


def _dict_to_expr():
    body = serialization_curly_braces_list(nothing(), serialization_half_block_style, Lists.map(_local("doubleStarredKvpairToExpr"), _unwrap("Dict", "d")))
    return _def(
        "dictToExpr",
        doc(
            "Serialize a Python dictionary",
            lambdas(["d"], body),
        ),
    )


def _disjunction_to_expr():
    body = serialization_symbol_sep(string("or"), serialization_inline_style, Lists.map(_local("conjunctionToExpr"), _unwrap("Disjunction", "d")))
    return _def(
        "disjunctionToExpr",
        doc(
            "Serialize a disjunction (or expression)",
            lambdas(["d"], body),
        ),
    )


def _dotted_as_name_to_expr():
    body = lets(
        [
            field("name", _proj("DottedAsName", "name", "dan")),
            field("alias", _proj("DottedAsName", "as", "dan")),
        ],
        serialization_space_sep(Maybes.cat(list_([
            just(_local("dottedNameToExpr")(var("name"))),
            Maybes.map(
                lam("a", _space_sep(list_([
                    _cst("as"),
                    _local("nameToExpr")(var("a")),
                ]))),
                var("alias"),
            ),
        ]))),
    )
    return _def(
        "dottedAsNameToExpr",
        doc(
            "Serialize a dotted as name",
            lambdas(["dan"], body),
        ),
    )


def _dotted_name_to_expr():
    body = serialization_cst(Strings.intercalate(
            string("."),
            Lists.map(
                lam("n", unwrap(_ty("Name"))(var("n"))),
                _unwrap("DottedName", "dn"),
            ),
        ))
    return _def(
        "dottedNameToExpr",
        doc(
            "Serialize a dotted name (e.g., module.submodule)",
            lambdas(["dn"], body),
        ),
    )


def _double_starred_kvpair_to_expr():
    body = cases(
        _ty("DoubleStarredKvpair"), var("dskv"), Nothing(),
        [
            field("pair", lam("p", _local("kvpairToExpr")(var("p")))),
            field("starred", lam("e", _no_sep(list_([
                _cst("**"),
                _local("bitwiseOrToExpr")(var("e")),
            ])))),
        ],
    )
    return _def(
        "doubleStarredKvpairToExpr",
        doc(
            "Serialize a double-starred key-value pair",
            lambdas(["dskv"], body),
        ),
    )


def _expression_to_expr():
    body = cases(
        _ty("Expression"), var("expr"), Nothing(),
        [
            field("simple", lam("d", _local("disjunctionToExpr")(var("d")))),
            field("conditional", lam("c", _local("conditionalToExpr")(var("c")))),
            field("lambda", lam("l", _local("lambdaToExpr")(var("l")))),
        ],
    )
    return _def(
        "expressionToExpr",
        doc(
            "Serialize a Python expression",
            lambdas(["expr"], body),
        ),
    )


def _factor_to_expr():
    body = cases(
        _ty("Factor"), var("f"), Nothing(),
        [
            field("positive",
                lam("inner", _no_sep(list_([
                    _cst("+"),
                    _local("factorToExpr")(var("inner")),
                ]))),
            ),
            field("negative",
                lam("inner", _no_sep(list_([
                    _cst("-"),
                    _local("factorToExpr")(var("inner")),
                ]))),
            ),
            field("complement",
                lam("inner", _no_sep(list_([
                    _cst("~"),
                    _local("factorToExpr")(var("inner")),
                ]))),
            ),
            field("simple", lam("p", _local("powerToExpr")(var("p")))),
        ],
    )
    return _def(
        "factorToExpr",
        doc(
            "Serialize a factor expression",
            lambdas(["f"], body),
        ),
    )


def _function_def_raw_to_expr():
    body = lets(
        [
            field("async_", _proj("FunctionDefRaw", "async", "fdr")),
            field("name", _proj("FunctionDefRaw", "name", "fdr")),
            field("tparams", _proj("FunctionDefRaw", "typeParams", "fdr")),
            field("params", _proj("FunctionDefRaw", "params", "fdr")),
            field("retType", _proj("FunctionDefRaw", "returnType", "fdr")),
            field("block", _proj("FunctionDefRaw", "block", "fdr")),
            field("asyncKw",
                Logic.if_else(
                    var("async_"),
                    just(_cst("async")),
                    nothing(),
                ),
            ),
            field("tparamPart",
                Logic.if_else(
                    Lists.null(var("tparams")),
                    nothing(),
                    just(serialization_bracket_list(serialization_inline_style, Lists.map(_local("typeParameterToExpr"), var("tparams")))),
                ),
            ),
            field("paramPart",
                Maybes.map(_local("parametersToExpr"), var("params")),
            ),
            field("retPart",
                Maybes.map(
                    lam("t", _space_sep(list_([
                        _cst("->"),
                        _local("expressionToExpr")(var("t")),
                    ]))),
                    var("retType"),
                ),
            ),
        ],
        _newline_sep(list_([
            _no_sep(list_([
                serialization_space_sep(Maybes.cat(list_([
                    var("asyncKw"),
                    just(_cst("def")),
                    just(serialization_no_sep(Maybes.cat(list_([
                        just(_local("nameToExpr")(var("name"))),
                        var("tparamPart"),
                        just(_cst("(")),
                        var("paramPart"),
                        just(_cst(")")),
                    ])))),
                    var("retPart"),
                ]))),
                _cst(":"),
            ])),
            _local("blockToExpr")(var("block")),
        ])),
    )
    return _def(
        "functionDefRawToExpr",
        doc(
            "Serialize a raw function definition",
            lambdas(["fdr"], body),
        ),
    )


def _function_definition_to_expr():
    body = lets(
        [
            field("decs", _proj("FunctionDefinition", "decorators", "fd")),
            field("raw", _proj("FunctionDefinition", "raw", "fd")),
        ],
        serialization_newline_sep(Maybes.cat(list_([
            Maybes.map(_local("decoratorsToExpr"), var("decs")),
            just(_local("functionDefRawToExpr")(var("raw"))),
        ]))),
    )
    return _def(
        "functionDefinitionToExpr",
        doc(
            "Serialize a function definition",
            lambdas(["fd"], body),
        ),
    )


def _group_to_expr():
    body = cases(
        _ty("Group"), var("g"), Nothing(),
        [
            field("expression", lam("ne", _local("namedExpressionToExpr")(var("ne")))),
            field("yield", lam("_", _cst("(yield ...)"))),
        ],
    )
    return _def(
        "groupToExpr",
        doc(
            "Serialize a parenthesized group",
            lambdas(["g"], body),
        ),
    )


def _guard_to_expr():
    body = _space_sep(list_([
        _cst("if"),
        _local("namedExpressionToExpr")(_unwrap("Guard", "g")),
    ]))
    return _def(
        "guardToExpr",
        doc(
            "Serialize a guard clause",
            lambdas(["g"], body),
        ),
    )


def _import_from_to_expr():
    body = lets(
        [
            field("prefixes", _proj("ImportFrom", "prefixes", "if_")),
            field("name", _proj("ImportFrom", "dottedName", "if_")),
            field("targets", _proj("ImportFrom", "targets", "if_")),
            field("lhs",
                serialization_no_sep(Maybes.cat(Lists.concat(list_([
                    Lists.map(
                        lam("p", just(_local("relativeImportPrefixToExpr")(var("p")))),
                        var("prefixes"),
                    ),
                    list_([Maybes.map(_local("dottedNameToExpr"), var("name"))]),
                ])))),
            ),
        ],
        _space_sep(list_([
            _cst("from"),
            var("lhs"),
            _cst("import"),
            _local("importFromTargetsToExpr")(var("targets")),
        ])),
    )
    return _def(
        "importFromToExpr",
        doc(
            "Serialize an import from statement",
            lambdas(["if_"], body),
        ),
    )


def _import_from_as_name_to_expr():
    body = lets(
        [
            field("name", _proj("ImportFromAsName", "name", "ifan")),
            field("alias", _proj("ImportFromAsName", "as", "ifan")),
        ],
        Maybes.cases(
            var("alias"),
            _local("nameToExpr")(var("name")),
            lam(
                "a",
                _space_sep(list_([
                    _local("nameToExpr")(var("name")),
                    _cst("as"),
                    _local("nameToExpr")(var("a")),
                ])),
            ),
        ),
    )
    return _def(
        "importFromAsNameToExpr",
        doc(
            "Serialize an import from as name",
            lambdas(["ifan"], body),
        ),
    )


def _import_from_targets_to_expr():
    body = cases(
        _ty("ImportFromTargets"), var("t"), Nothing(),
        [
            field("simple",
                lam(
                    "names",
                    _comma_sep_inline(Lists.map(_local("importFromAsNameToExpr"), var("names"))),
                ),
            ),
            field("parens",
                lam(
                    "names",
                    _no_sep(list_([
                        _cst("("),
                        _comma_sep_inline(Lists.map(_local("importFromAsNameToExpr"), var("names"))),
                        _cst(")"),
                    ])),
                ),
            ),
            field("star", constant(_cst("*"))),
        ],
    )
    return _def(
        "importFromTargetsToExpr",
        doc(
            "Serialize import from targets",
            lambdas(["t"], body),
        ),
    )


def _import_name_to_expr():
    body = _space_sep(list_([
        _cst("import"),
        _comma_sep_inline(Lists.map(_local("dottedAsNameToExpr"), _unwrap("ImportName", "in_"))),
    ]))
    return _def(
        "importNameToExpr",
        doc(
            "Serialize an import name",
            lambdas(["in_"], body),
        ),
    )


def _import_statement_to_expr():
    body = cases(
        _ty("ImportStatement"), var("is_"), Nothing(),
        [
            field("name", lam("n", _local("importNameToExpr")(var("n")))),
            field("from", lam("f", _local("importFromToExpr")(var("f")))),
        ],
    )
    return _def(
        "importStatementToExpr",
        doc(
            "Serialize an import statement",
            lambdas(["is_"], body),
        ),
    )


def _inversion_to_expr():
    body = cases(
        _ty("Inversion"), var("i"), Nothing(),
        [
            field("not",
                lam("other", _space_sep(list_([
                    _cst("not"),
                    _local("inversionToExpr")(var("other")),
                ]))),
            ),
            field("simple", lam("c", _local("comparisonToExpr")(var("c")))),
        ],
    )
    return _def(
        "inversionToExpr",
        doc(
            "Serialize an inversion (not expression)",
            lambdas(["i"], body),
        ),
    )


def _keyword_pattern_to_expr():
    body = lets(
        [
            field("name", _proj("KeywordPattern", "name", "kp")),
            field("pat", _proj("KeywordPattern", "pattern", "kp")),
        ],
        _no_sep(list_([
            _local("nameToExpr")(var("name")),
            _cst("="),
            _local("patternToExpr")(var("pat")),
        ])),
    )
    return _def(
        "keywordPatternToExpr",
        doc(
            "Serialize a keyword pattern",
            lambdas(["kp"], body),
        ),
    )


def _keyword_patterns_to_expr():
    body = _comma_sep_inline(Lists.map(_local("keywordPatternToExpr"), _unwrap("KeywordPatterns", "kp")))
    return _def(
        "keywordPatternsToExpr",
        doc(
            "Serialize keyword patterns",
            lambdas(["kp"], body),
        ),
    )


def _kvpair_to_expr():
    body = lets(
        [
            field("k", _proj("Kvpair", "key", "kv")),
            field("v", _proj("Kvpair", "value", "kv")),
        ],
        _space_sep(list_([
            _no_sep(list_([
                _local("expressionToExpr")(var("k")),
                _cst(":"),
            ])),
            _local("expressionToExpr")(var("v")),
        ])),
    )
    return _def(
        "kvpairToExpr",
        doc(
            "Serialize a key-value pair",
            lambdas(["kv"], body),
        ),
    )


def _kwarg_to_expr():
    body = lets(
        [
            field("name", _proj("Kwarg", "name", "k")),
            field("expr", _proj("Kwarg", "value", "k")),
        ],
        _no_sep(list_([
            _local("nameToExpr")(var("name")),
            _cst("="),
            _local("expressionToExpr")(var("expr")),
        ])),
    )
    return _def(
        "kwargToExpr",
        doc(
            "Serialize a keyword argument",
            lambdas(["k"], body),
        ),
    )


def _kwarg_or_double_starred_to_expr():
    body = cases(
        _ty("KwargOrDoubleStarred"), var("kds"), Nothing(),
        [
            field("kwarg", lam("k", _local("kwargToExpr")(var("k")))),
            field("doubleStarred",
                lam("e", _no_sep(list_([
                    _cst("**"),
                    _local("expressionToExpr")(var("e")),
                ]))),
            ),
        ],
    )
    return _def(
        "kwargOrDoubleStarredToExpr",
        doc(
            "Serialize a kwarg or double starred",
            lambdas(["kds"], body),
        ),
    )


def _kwarg_or_starred_to_expr():
    body = cases(
        _ty("KwargOrStarred"), var("ks"), Nothing(),
        [
            field("kwarg", lam("k", _local("kwargToExpr")(var("k")))),
            field("starred", lam("se", _local("starredExpressionToExpr")(var("se")))),
        ],
    )
    return _def(
        "kwargOrStarredToExpr",
        doc(
            "Serialize a kwarg or starred",
            lambdas(["ks"], body),
        ),
    )


def _lambda_to_expr():
    body = lets(
        [
            field("params", _proj("Lambda", "params", "l")),
            field("body", _proj("Lambda", "body", "l")),
        ],
        serialization_parens(_space_sep(list_([
            _cst("lambda"),
            _no_sep(list_([
                _local("lambdaParametersToExpr")(var("params")),
                _cst(":"),
            ])),
            _local("expressionToExpr")(var("body")),
        ]))),
    )
    return _def(
        "lambdaToExpr",
        doc(
            "Serialize a lambda expression",
            lambdas(["l"], body),
        ),
    )


def _lambda_param_no_default_to_expr():
    body = _local("nameToExpr")(_unwrap("LambdaParamNoDefault", "p"))
    return _def(
        "lambdaParamNoDefaultToExpr",
        doc(
            "Serialize a lambda parameter without default",
            lambdas(["p"], body),
        ),
    )


def _lambda_parameters_to_expr():
    body = lets(
        [
            field("nodef", _proj("LambdaParameters", "paramNoDefault", "lp")),
        ],
        _comma_sep_inline(Lists.map(_local("lambdaParamNoDefaultToExpr"), var("nodef"))),
    )
    return _def(
        "lambdaParametersToExpr",
        doc(
            "Serialize lambda parameters",
            lambdas(["lp"], body),
        ),
    )


def _lambda_star_etc_to_expr():
    body = cases(
        _ty("LambdaStarEtc"), var("lse"), Nothing(),
        [
            field("paramNoDefault", lam("p", _local("lambdaParamNoDefaultToExpr")(var("p")))),
            field("star", lam("_", _cst("*..."))),
            field("paramMaybeDefault", lam("_", _cst("..."))),
            field("kwds", lam("_", _cst("**..."))),
        ],
    )
    return _def(
        "lambdaStarEtcToExpr",
        doc(
            "Serialize lambda star etc",
            lambdas(["lse"], body),
        ),
    )


def _list_to_expr():
    body = serialization_bracket_list_adaptive(Lists.map(_local("starNamedExpressionToExpr"), _unwrap("List", "l")))
    return _def(
        "listToExpr",
        doc(
            "Serialize a Python list",
            lambdas(["l"], body),
        ),
    )


def _match_statement_to_expr():
    body = lets(
        [
            field("subj", _proj("MatchStatement", "subject", "ms")),
            field("cases", _proj("MatchStatement", "cases", "ms")),
        ],
        _newline_sep(list_([
            _space_sep(list_([
                _cst("match"),
                _no_sep(list_([
                    _local("subjectExpressionToExpr")(var("subj")),
                    _cst(":"),
                ])),
            ])),
            serialization_tab_indent_double_space(Lists.map(_local("caseBlockToExpr"), var("cases"))),
        ])),
    )
    return _def(
        "matchStatementToExpr",
        doc(
            "Serialize a match statement",
            lambdas(["ms"], body),
        ),
    )


def _module_to_expr():
    body = lets(
        [
            field("warning",
                serialization_cst(_local("toPythonComments")(var("hydra.constants.warningAutoGeneratedFile"))),  # noqa: E501
            ),
            field("groups",
                Lists.map(
                    lam(
                        "group",
                        _newline_sep(Lists.map(_local("statementToExpr"), var("group"))),
                    ),
                    _unwrap("Module", "mod"),
                ),
            ),
        ],
        serialization_double_newline_sep(Lists.cons(var("warning"), var("groups"))),
    )
    return _def(
        "moduleToExpr",
        doc(
            "Serialize a Python module to an AST expression",
            lambdas(["mod"], body),
        ),
    )


def _name_to_expr():
    body = serialization_cst(_unwrap("Name", "n"))
    return _def(
        "nameToExpr",
        doc(
            "Serialize a Python name/identifier",
            lambdas(["n"], body),
        ),
    )


def _name_or_attribute_to_expr():
    body = serialization_dot_sep(Lists.map(_local("nameToExpr"), _unwrap("NameOrAttribute", "noa")))
    return _def(
        "nameOrAttributeToExpr",
        doc(
            "Serialize a name or attribute",
            lambdas(["noa"], body),
        ),
    )


def _named_expression_to_expr():
    body = cases(
        _ty("NamedExpression"), var("ne"), Nothing(),
        [
            field("simple", lam("e", _local("expressionToExpr")(var("e")))),
            field("assignment", lam("ae", _local("assignmentExpressionToExpr")(var("ae")))),
        ],
    )
    return _def(
        "namedExpressionToExpr",
        doc(
            "Serialize a named expression",
            lambdas(["ne"], body),
        ),
    )


def _number_to_expr():
    body = cases(
        _ty("Number"), var("num"), Nothing(),
        [
            field("float",
                lam("f", serialization_cst(_local("pythonFloatLiteralText")(var("hydra.lib.literals.showFloat64")(var("f"))))),
            ),
            field("imaginary",
                lam("f", serialization_cst(Strings.cat2(
                    _local("pythonFloatLiteralText")(var("hydra.lib.literals.showFloat64")(var("f"))),
                    string("j"),
                ))),
            ),
            field("integer",
                lam("i", serialization_cst(var("hydra.lib.literals.showBigint")(var("i")))),
            ),
        ],
    )
    return _def(
        "numberToExpr",
        doc(
            "Serialize a Python number literal",
            lambdas(["num"], body),
        ),
    )


def _or_pattern_to_expr():
    body = serialization_symbol_sep(string("|"), serialization_inline_style, Lists.map(_local("closedPatternToExpr"), _unwrap("OrPattern", "op")))
    return _def(
        "orPatternToExpr",
        doc(
            "Serialize an or pattern",
            lambdas(["op"], body),
        ),
    )


def _param_to_expr():
    body = lets(
        [
            field("name", _proj("Param", "name", "p")),
            field("ann", _proj("Param", "annotation", "p")),
        ],
        serialization_no_sep(Maybes.cat(list_([
            just(_local("nameToExpr")(var("name"))),
            Maybes.map(_local("annotationToExpr"), var("ann")),
        ]))),
    )
    return _def(
        "paramToExpr",
        doc(
            "Serialize a parameter",
            lambdas(["p"], body),
        ),
    )


def _param_no_default_to_expr():
    body = _local("paramToExpr")(_proj("ParamNoDefault", "param", "pnd"))
    return _def(
        "paramNoDefaultToExpr",
        doc(
            "Serialize a parameter without default",
            lambdas(["pnd"], body),
        ),
    )


def _param_no_default_parameters_to_expr():
    body = lets(
        [
            field("nodef", _proj("ParamNoDefaultParameters", "paramNoDefault", "pndp")),
        ],
        _comma_sep_adaptive(Lists.map(_local("paramNoDefaultToExpr"), var("nodef"))),
    )
    return _def(
        "paramNoDefaultParametersToExpr",
        doc(
            "Serialize parameters without defaults",
            lambdas(["pndp"], body),
        ),
    )


def _parameters_to_expr():
    body = cases(
        _ty("Parameters"), var("p"), Nothing(),
        [
            field("paramNoDefault", lam("pnd", _local("paramNoDefaultParametersToExpr")(var("pnd")))),
            field("slashNoDefault", lam("_", _cst("..."))),
            field("slashWithDefault", lam("_", _cst("..."))),
        ],
    )
    return _def(
        "parametersToExpr",
        doc(
            "Serialize function parameters",
            lambdas(["p"], body),
        ),
    )


def _pattern_to_expr():
    body = cases(
        _ty("Pattern"), var("p"), Nothing(),
        [
            field("or", lam("op", _local("orPatternToExpr")(var("op")))),
            field("as", lam("_", _cst("... as ..."))),
        ],
    )
    return _def(
        "patternToExpr",
        doc(
            "Serialize a pattern",
            lambdas(["p"], body),
        ),
    )


def _pattern_capture_target_to_expr():
    body = _local("nameToExpr")(_unwrap("PatternCaptureTarget", "pct"))
    return _def(
        "patternCaptureTargetToExpr",
        doc(
            "Serialize a pattern capture target",
            lambdas(["pct"], body),
        ),
    )


def _patterns_to_expr():
    body = cases(
        _ty("Patterns"), var("ps"), Nothing(),
        [
            field("pattern", lam("p", _local("patternToExpr")(var("p")))),
            field("sequence", lam("_", _cst("..."))),
        ],
    )
    return _def(
        "patternsToExpr",
        doc(
            "Serialize patterns",
            lambdas(["ps"], body),
        ),
    )


def _pos_arg_to_expr():
    body = cases(
        _ty("PosArg"), var("pa"), Nothing(),
        [
            field("starred", lam("se", _local("starredExpressionToExpr")(var("se")))),
            field("assignment", lam("ae", _local("assignmentExpressionToExpr")(var("ae")))),
            field("expression", lam("e", _local("expressionToExpr")(var("e")))),
        ],
    )
    return _def(
        "posArgToExpr",
        doc(
            "Serialize a positional argument",
            lambdas(["pa"], body),
        ),
    )


def _positional_patterns_to_expr():
    body = _comma_sep_inline(Lists.map(_local("patternToExpr"), _unwrap("PositionalPatterns", "pp")))
    return _def(
        "positionalPatternsToExpr",
        doc(
            "Serialize positional patterns",
            lambdas(["pp"], body),
        ),
    )


def _power_to_expr():
    body = lets(
        [
            field("lhs", _proj("Power", "lhs", "p")),
            field("rhs", _proj("Power", "rhs", "p")),
        ],
        serialization_space_sep(Maybes.cat(list_([
            just(_local("awaitPrimaryToExpr")(var("lhs"))),
            Maybes.map(
                lam("r", _space_sep(list_([
                    _cst("**"),
                    _local("factorToExpr")(var("r")),
                ]))),
                var("rhs"),
            ),
        ]))),
    )
    return _def(
        "powerToExpr",
        doc(
            "Serialize a power expression",
            lambdas(["p"], body),
        ),
    )


def _primary_to_expr():
    body = cases(
        _ty("Primary"), var("p"), Nothing(),
        [
            field("simple", lam("a", _local("atomToExpr")(var("a")))),
            field("compound", lam("pwr", _local("primaryWithRhsToExpr")(var("pwr")))),
        ],
    )
    return _def(
        "primaryToExpr",
        doc(
            "Serialize a primary expression",
            lambdas(["p"], body),
        ),
    )


def _primary_rhs_to_expr():
    body = cases(
        _ty("PrimaryRhs"), var("rhs"), Nothing(),
        [
            field("call",
                lam("args", _no_sep(list_([
                    _cst("("),
                    _local("argsToExpr")(var("args")),
                    _cst(")"),
                ]))),
            ),
            field("project",
                lam("name", _no_sep(list_([
                    _cst("."),
                    _local("nameToExpr")(var("name")),
                ]))),
            ),
            field("slices",
                lam("slices", _no_sep(list_([
                    _cst("["),
                    _local("slicesToExpr")(var("slices")),
                    _cst("]"),
                ]))),
            ),
            field("genexp", lam("_", _cst("[...]"))),
        ],
    )
    return _def(
        "primaryRhsToExpr",
        doc(
            "Serialize a primary RHS",
            lambdas(["rhs"], body),
        ),
    )


def _primary_with_rhs_to_expr():
    body = lets(
        [
            field("prim", _proj("PrimaryWithRhs", "primary", "pwr")),
            field("rhs", _proj("PrimaryWithRhs", "rhs", "pwr")),
        ],
        _no_sep(list_([
            _local("primaryToExpr")(var("prim")),
            _local("primaryRhsToExpr")(var("rhs")),
        ])),
    )
    return _def(
        "primaryWithRhsToExpr",
        doc(
            "Serialize a primary with RHS",
            lambdas(["pwr"], body),
        ),
    )


def _raise_expression_to_expr():
    body = lets(
        [
            field("expr", _proj("RaiseExpression", "expression", "re")),
            field("from_", _proj("RaiseExpression", "from", "re")),
        ],
        serialization_space_sep(Maybes.cat(list_([
            just(_local("expressionToExpr")(var("expr"))),
            Maybes.map(
                lam("f", _space_sep(list_([
                    _cst("from"),
                    _local("expressionToExpr")(var("f")),
                ]))),
                var("from_"),
            ),
        ]))),
    )
    return _def(
        "raiseExpressionToExpr",
        doc(
            "Serialize a raise expression",
            lambdas(["re"], body),
        ),
    )


def _raise_statement_to_expr():
    body = serialization_space_sep(Maybes.cat(list_([
        just(_cst("raise")),
        Maybes.map(_local("raiseExpressionToExpr"), _unwrap("RaiseStatement", "rs")),
    ])))
    return _def(
        "raiseStatementToExpr",
        doc(
            "Serialize a raise statement",
            lambdas(["rs"], body),
        ),
    )


def _relative_import_prefix_to_expr():
    body = cases(
        _ty("RelativeImportPrefix"), var("p"), Nothing(),
        [
            field("dot", constant(_cst("."))),
            field("ellipsis", constant(_cst("..."))),
        ],
    )
    return _def(
        "relativeImportPrefixToExpr",
        doc(
            "Serialize a relative import prefix",
            lambdas(["p"], body),
        ),
    )


def _return_statement_to_expr():
    body = _space_sep(list_([
        _cst("return"),
        _comma_sep_inline(Lists.map(_local("starExpressionToExpr"), _unwrap("ReturnStatement", "rs"))),
    ]))
    return _def(
        "returnStatementToExpr",
        doc(
            "Serialize a return statement",
            lambdas(["rs"], body),
        ),
    )


def _set_to_expr():
    body = serialization_braces_list_adaptive(Lists.map(_local("starNamedExpressionToExpr"), _unwrap("Set", "s")))
    return _def(
        "setToExpr",
        doc(
            "Serialize a Python set",
            lambdas(["s"], body),
        ),
    )


def _shift_expression_to_expr():
    body = _local("sumToExpr")(_proj("ShiftExpression", "rhs", "se"))
    return _def(
        "shiftExpressionToExpr",
        doc(
            "Serialize a shift expression",
            lambdas(["se"], body),
        ),
    )


def _simple_statement_to_expr():
    body = cases(
        _ty("SimpleStatement"), var("ss"), Nothing(),
        [
            field("assignment", lam("a", _local("assignmentToExpr")(var("a")))),
            field("starExpressions",
                lam("es", _newline_sep(Lists.map(_local("starExpressionToExpr"), var("es")))),
            ),
            field("return", lam("r", _local("returnStatementToExpr")(var("r")))),
            field("raise", lam("r", _local("raiseStatementToExpr")(var("r")))),
            field("pass", constant(_cst("pass"))),
            field("break", constant(_cst("break"))),
            field("continue", constant(_cst("continue"))),
            field("import", lam("i", _local("importStatementToExpr")(var("i")))),
            field("typeAlias", lam("t", _local("typeAliasToExpr")(var("t")))),
            field("assert", lam("_", _cst("assert ..."))),
            field("global", lam("_", _cst("global ..."))),
            field("nonlocal", lam("_", _cst("nonlocal ..."))),
            field("del", lam("_", _cst("del ..."))),
        ],
    )
    return _def(
        "simpleStatementToExpr",
        doc(
            "Serialize a simple (single-line) Python statement",
            lambdas(["ss"], body),
        ),
    )


def _simple_type_parameter_to_expr():
    body = _local("nameToExpr")(_proj("SimpleTypeParameter", "name", "stp"))
    return _def(
        "simpleTypeParameterToExpr",
        doc(
            "Serialize a simple type parameter",
            lambdas(["stp"], body),
        ),
    )


def _single_target_to_expr():
    body = cases(
        _ty("SingleTarget"), var("st"), Nothing(),
        [
            field("name", lam("n", _local("nameToExpr")(var("n")))),
            field("parens", lam("_", _cst("(...)"))),
            field("subscriptAttributeTarget", lam("_", _cst("..."))),
        ],
    )
    return _def(
        "singleTargetToExpr",
        doc(
            "Serialize a single target",
            lambdas(["st"], body),
        ),
    )


def _slice_to_expr():
    body = cases(
        _ty("Slice"), var("s"), Nothing(),
        [
            field("named", lam("ne", _local("namedExpressionToExpr")(var("ne")))),
            field("slice_", lam("_", _cst(":"))),
        ],
    )
    return _def(
        "sliceToExpr",
        doc(
            "Serialize a slice",
            lambdas(["s"], body),
        ),
    )


def _slice_or_starred_expression_to_expr():
    body = cases(
        _ty("SliceOrStarredExpression"), var("s"), Nothing(),
        [
            field("slice", lam("sl", _local("sliceToExpr")(var("sl")))),
            field("starred", lam("se", _local("starredExpressionToExpr")(var("se")))),
        ],
    )
    return _def(
        "sliceOrStarredExpressionToExpr",
        doc(
            "Serialize a slice or starred expression",
            lambdas(["s"], body),
        ),
    )


def _slices_to_expr():
    body = lets(
        [
            field("hd", _proj("Slices", "head", "s")),
            field("tl", _proj("Slices", "tail", "s")),
        ],
        _comma_sep_inline(Lists.cons(
            _local("sliceToExpr")(var("hd")),
            Lists.map(_local("sliceOrStarredExpressionToExpr"), var("tl")),
        )),
    )
    return _def(
        "slicesToExpr",
        doc(
            "Serialize slices",
            lambdas(["s"], body),
        ),
    )


def _star_atom_to_expr():
    body = cases(
        _ty("StarAtom"), var("sa"), Nothing(),
        [
            field("name", lam("n", _local("nameToExpr")(var("n")))),
            field("targetWithStarAtom", lam("_", _cst("(...)"))),
            field("starTargetsTupleSeq", lam("_", _cst("(...)"))),
            field("starTargetsListSeq", lam("_", _cst("[...]"))),
        ],
    )
    return _def(
        "starAtomToExpr",
        doc(
            "Serialize a star atom",
            lambdas(["sa"], body),
        ),
    )


def _star_expression_to_expr():
    body = cases(
        _ty("StarExpression"), var("se"), Nothing(),
        [
            field("star",
                lam("bor", _no_sep(list_([
                    _cst("*"),
                    _local("bitwiseOrToExpr")(var("bor")),
                ]))),
            ),
            field("simple", lam("e", _local("expressionToExpr")(var("e")))),
        ],
    )
    return _def(
        "starExpressionToExpr",
        doc(
            "Serialize a star expression",
            lambdas(["se"], body),
        ),
    )


def _star_named_expression_to_expr():
    body = cases(
        _ty("StarNamedExpression"), var("sne"), Nothing(),
        [
            field("star",
                lam("bor", _no_sep(list_([
                    _cst("*"),
                    _local("bitwiseOrToExpr")(var("bor")),
                ]))),
            ),
            field("simple", lam("ne", _local("namedExpressionToExpr")(var("ne")))),
        ],
    )
    return _def(
        "starNamedExpressionToExpr",
        doc(
            "Serialize a star named expression",
            lambdas(["sne"], body),
        ),
    )


def _star_target_to_expr():
    body = cases(
        _ty("StarTarget"), var("st"), Nothing(),
        [
            field("unstarred", lam("t", _local("targetWithStarAtomToExpr")(var("t")))),
            field("starred",
                lam("inner", _no_sep(list_([
                    _cst("*"),
                    _local("starTargetToExpr")(var("inner")),
                ]))),
            ),
        ],
    )
    return _def(
        "starTargetToExpr",
        doc(
            "Serialize a star target",
            lambdas(["st"], body),
        ),
    )


def _starred_expression_to_expr():
    body = _no_sep(list_([
        _cst("*"),
        _local("expressionToExpr")(_unwrap("StarredExpression", "se")),
    ]))
    return _def(
        "starredExpressionToExpr",
        doc(
            "Serialize a starred expression",
            lambdas(["se"], body),
        ),
    )


def _statement_to_expr():
    body = cases(
        _ty("Statement"), var("stmt"), Nothing(),
        [
            field("annotated", lam("a", _local("annotatedStatementToExpr")(var("a")))),
            field("simple",
                lam("ss", _newline_sep(Lists.map(_local("simpleStatementToExpr"), var("ss")))),
            ),
            field("compound", lam("c", _local("compoundStatementToExpr")(var("c")))),
        ],
    )
    return _def(
        "statementToExpr",
        doc(
            "Serialize a Python statement",
            lambdas(["stmt"], body),
        ),
    )


def _string_prefix_to_text():
    body = cases(
        _ty("StringPrefix"), var("p"), Nothing(),
        [
            field("raw", constant(string("r"))),
            field("bytes", constant(string("b"))),
            field("rawBytes", constant(string("rb"))),
            field("unicode", constant(string("u"))),
        ],
    )
    return _def(
        "stringPrefixToText",
        doc(
            "Serialize a Python string prefix to its source-form characters",
            lambdas(["p"], body),
        ),
    )


def _string_to_expr():
    body = lets(
        [
            field("content", _proj("String", "value", "s")),
            field("prefix", Maybes.cases(_proj("String", "prefix", "s"), string(""), _local("stringPrefixToText"))),
            field("style", _proj("String", "quoteStyle", "s")),
        ],
        cases(
            _ty("QuoteStyle"), var("style"), Nothing(),
            [
                field("single",
                    constant(serialization_cst(Strings.cat2(var("prefix"), _local("escapePythonString")(false(), var("content"))))),
                ),
                field("double",
                    constant(serialization_cst(Strings.cat2(var("prefix"), _local("escapePythonString")(true(), var("content"))))),
                ),
                field("tripleSingle",
                    constant(_no_sep(list_([
                        serialization_cst(Strings.cat2(var("prefix"), string("'''"))),
                        serialization_cst(var("content")),
                        serialization_cst(string("'''")),
                    ]))),
                ),
                field("tripleDouble",
                    constant(_no_sep(list_([
                        serialization_cst(Strings.cat2(var("prefix"), string('"""'))),
                        serialization_cst(var("content")),
                        serialization_cst(string('"""')),
                    ]))),
                ),
            ],
        ),
    )
    return _def(
        "stringToExpr",
        doc(
            "Serialize a Python string literal",
            lambdas(["s"], body),
        ),
    )


def _subject_expression_to_expr():
    body = cases(
        _ty("SubjectExpression"), var("se"), Nothing(),
        [
            field("simple", lam("ne", _local("namedExpressionToExpr")(var("ne")))),
            field("tuple", lam("_", _cst("*..."))),
        ],
    )
    return _def(
        "subjectExpressionToExpr",
        doc(
            "Serialize a subject expression",
            lambdas(["se"], body),
        ),
    )


def _sum_to_expr():
    body = _local("termToExpr")(_proj("Sum", "rhs", "s"))
    return _def(
        "sumToExpr",
        doc(
            "Serialize a sum expression",
            lambdas(["s"], body),
        ),
    )


def _t_primary_to_expr():
    body = cases(
        _ty("TPrimary"), var("tp"), Nothing(),
        [
            field("atom", lam("a", _local("atomToExpr")(var("a")))),
            field("primaryAndName", lam("pn", _local("tPrimaryAndNameToExpr")(var("pn")))),
            field("primaryAndSlices", lam("_", _cst("..."))),
            field("primaryAndGenexp", lam("_", _cst("..."))),
            field("primaryAndArguments", lam("_", _cst("..."))),
        ],
    )
    return _def(
        "tPrimaryToExpr",
        doc(
            "Serialize a target-side primary expression",
            lambdas(["tp"], body),
        ),
    )


def _t_primary_and_name_to_expr():
    body = lets(
        [
            field("prim", _proj("TPrimaryAndName", "primary", "pn")),
            field("name_", _proj("TPrimaryAndName", "name", "pn")),
        ],
        _no_sep(list_([
            _local("tPrimaryToExpr")(var("prim")),
            _cst("."),
            _local("nameToExpr")(var("name_")),
        ])),
    )
    return _def(
        "tPrimaryAndNameToExpr",
        doc(
            "Serialize a TPrimaryAndName as primary.name",
            lambdas(["pn"], body),
        ),
    )


def _target_with_star_atom_to_expr():
    body = cases(
        _ty("TargetWithStarAtom"), var("t"), Nothing(),
        [
            field("atom", lam("a", _local("starAtomToExpr")(var("a")))),
            field("project", lam("pn", _local("tPrimaryAndNameToExpr")(var("pn")))),
            field("slices", lam("_", _cst("..."))),
        ],
    )
    return _def(
        "targetWithStarAtomToExpr",
        doc(
            "Serialize a target with star atom",
            lambdas(["t"], body),
        ),
    )


def _term_to_expr():
    body = _local("factorToExpr")(_proj("Term", "rhs", "t"))
    return _def(
        "termToExpr",
        doc(
            "Serialize a term expression",
            lambdas(["t"], body),
        ),
    )


def _tuple_to_expr():
    body = lets(
        [
            field("es", _unwrap("Tuple", "t")),
        ],
        Maybes.from_maybe(
            serialization_paren_list_adaptive(Lists.map(_local("starNamedExpressionToExpr"), var("es"))),
            Maybes.map(
                lam(
                    "firstEs",
                    Logic.if_else(
                        Equality.equal(Lists.length(var("es")), int32(1)),
                        serialization_parens(_no_sep(list_([
                            _local("starNamedExpressionToExpr")(var("firstEs")),
                            _cst(","),
                        ]))),
                        serialization_paren_list_adaptive(Lists.map(_local("starNamedExpressionToExpr"), var("es"))),
                    ),
                ),
                Lists.maybe_head(var("es")),
            ),
        ),
    )
    return _def(
        "tupleToExpr",
        doc(
            "Serialize a Python tuple",
            lambdas(["t"], body),
        ),
    )


def _type_alias_to_expr():
    body = lets(
        [
            field("name", _proj("TypeAlias", "name", "ta")),
            field("tparams", _proj("TypeAlias", "typeParams", "ta")),
            field("expr", _proj("TypeAlias", "expression", "ta")),
            field("alias",
                serialization_no_sep(Maybes.cat(list_([
                    just(_local("nameToExpr")(var("name"))),
                    Logic.if_else(
                        Lists.null(var("tparams")),
                        nothing(),
                        just(serialization_bracket_list(serialization_inline_style, Lists.map(_local("typeParameterToExpr"), var("tparams")))),
                    ),
                ]))),
            ),
        ],
        _space_sep(list_([
            _cst("type"),
            var("alias"),
            _cst("="),
            _local("expressionToExpr")(var("expr")),
        ])),
    )
    return _def(
        "typeAliasToExpr",
        doc(
            "Serialize a type alias",
            lambdas(["ta"], body),
        ),
    )


def _type_parameter_to_expr():
    body = cases(
        _ty("TypeParameter"), var("tp"), Nothing(),
        [
            field("simple", lam("s", _local("simpleTypeParameterToExpr")(var("s")))),
            field("star", lam("_", _cst("*..."))),
            field("doubleStar", lam("_", _cst("**..."))),
        ],
    )
    return _def(
        "typeParameterToExpr",
        doc(
            "Serialize a type parameter",
            lambdas(["tp"], body),
        ),
    )


def _typed_assignment_to_expr():
    body = lets(
        [
            field("lhs", _proj("TypedAssignment", "lhs", "ta")),
            field("typ", _proj("TypedAssignment", "type", "ta")),
            field("rhs", _proj("TypedAssignment", "rhs", "ta")),
        ],
        serialization_space_sep(Maybes.cat(list_([
            just(_no_sep(list_([
                _local("singleTargetToExpr")(var("lhs")),
                _cst(":"),
            ]))),
            just(_local("expressionToExpr")(var("typ"))),
            Maybes.map(_local("annotatedRhsToExpr"), var("rhs")),
        ]))),
    )
    return _def(
        "typedAssignmentToExpr",
        doc(
            "Serialize a typed assignment",
            lambdas(["ta"], body),
        ),
    )


def _untyped_assignment_to_expr():
    body = lets(
        [
            field("targets", _proj("UntypedAssignment", "targets", "ua")),
            field("rhs", _proj("UntypedAssignment", "rhs", "ua")),
        ],
        _space_sep(Lists.concat(list_([
            Lists.map(_local("starTargetToExpr"), var("targets")),
            list_([_local("annotatedRhsToExpr")(var("rhs"))]),
        ]))),
    )
    return _def(
        "untypedAssignmentToExpr",
        doc(
            "Serialize an untyped assignment",
            lambdas(["ua"], body),
        ),
    )


def _value_pattern_to_expr():
    body = _local("attributeToExpr")(_unwrap("ValuePattern", "vp"))
    return _def(
        "valuePatternToExpr",
        doc(
            "Serialize a value pattern",
            lambdas(["vp"], body),
        ),
    )


def _while_statement_to_expr():
    body = lets(
        [
            field("cond", _proj("WhileStatement", "condition", "ws")),
            field("body", _proj("WhileStatement", "body", "ws")),
            field("else_", _proj("WhileStatement", "else", "ws")),
        ],
        serialization_newline_sep(Maybes.cat(list_([
            just(_newline_sep(list_([
                _space_sep(list_([
                    _cst("while"),
                    _no_sep(list_([
                        _local("namedExpressionToExpr")(var("cond")),
                        _cst(":"),
                    ])),
                ])),
                _local("blockToExpr")(var("body")),
            ]))),
            Maybes.map(
                lam("eb", _newline_sep(list_([
                    _cst("else:"),
                    _local("blockToExpr")(var("eb")),
                ]))),
                var("else_"),
            ),
        ]))),
    )
    return _def(
        "whileStatementToExpr",
        doc(
            "Serialize a while statement",
            lambdas(["ws"], body),
        ),
    )


def _escape_python_string():
    body = lets(
        [
            field("replace",
                lambdas(
                    ["old", "new", "str"],
                    Strings.intercalate(var("new"),
                        Strings.split_on(var("old"), var("str"))),
                ),
            ),
            field("s1",
                var("replace")(string("\\"), string("\\\\"), var("s")),
            ),
            field("s2",
                var("replace")(string("\0"), string("\\x00"), var("s1")),
            ),
            field("s3",
                var("replace")(string("\n"), string("\\n"), var("s2")),
            ),
            field("s4",
                var("replace")(string("\t"), string("\\t"), var("s3")),
            ),
            field("s5",
                var("replace")(string("\r"), string("\\r"), var("s4")),
            ),
            field("escaped",
                Logic.if_else(
                    var("doubleQuoted"),
                    var("replace")(string('"'), string('\\"'), var("s5")),
                    var("replace")(string("'"), string("\\'"), var("s5")),
                ),
            ),
            field("quote",
                Logic.if_else(
                    var("doubleQuoted"),
                    string('"'),
                    string("'"),
                ),
            ),
        ],
        Strings.cat2(var("quote"),
            Strings.cat2(var("escaped"), var("quote"))),
    )
    return _def(
        "escapePythonString",
        doc(
            "Escape special characters in a Python string and wrap in quotes",
            lambdas(["doubleQuoted", "s"], body),
        ),
    )


def _python_float_literal_text():
    body = Logic.if_else(
        Equality.equal(var("s"), string("NaN")),
        string("float('nan')"),
        Logic.if_else(
            Equality.equal(var("s"), string("Infinity")),
            string("float('inf')"),
            Logic.if_else(
                Equality.equal(var("s"), string("-Infinity")),
                string("float('-inf')"),
                var("s"),
            ),
        ),
    )
    return _def(
        "pythonFloatLiteralText",
        lambdas(["s"], body),
    )


def _to_python_comments():
    body = Logic.if_else(
        Equality.equal(var("doc_"), string("")),
        string(""),
        Strings.intercalate(
            string("\n"),
            Lists.map(
                lam(
                    "line",
                    Logic.if_else(
                        Equality.equal(var("line"), string("")),
                        string("#"),
                        Strings.cat2(string("# "), var("line")),
                    ),
                ),
                Strings.lines(var("doc_")),
            ),
        ),
    )
    return _def(
        "toPythonComments",
        doc(
            "Convert a doc string to Python comment format. Empty source lines"
            " emit `#` (no trailing space) so blank comment lines don't carry"
            " trailing whitespace into the generated file.",
            lambdas(["doc_"], body),
        ),
    )


def _build_module() -> Module:
    return Module(
        _PLACEHOLDER.name,
        _PLACEHOLDER.metadata,
        _PLACEHOLDER.dependencies,
        (
            to_definition(_annotated_rhs_to_expr()),
            to_definition(_annotated_statement_to_expr()),
            to_definition(_annotation_to_expr()),
            to_definition(_args_to_expr()),
            to_definition(_assignment_to_expr()),
            to_definition(_assignment_expression_to_expr()),
            to_definition(_atom_to_expr()),
            to_definition(_attribute_to_expr()),
            to_definition(_await_primary_to_expr()),
            to_definition(_bitwise_and_to_expr()),
            to_definition(_bitwise_or_to_expr()),
            to_definition(_bitwise_xor_to_expr()),
            to_definition(_block_to_expr()),
            to_definition(_capture_pattern_to_expr()),
            to_definition(_case_block_to_expr()),
            to_definition(_class_definition_to_expr()),
            to_definition(_class_pattern_to_expr()),
            to_definition(_closed_pattern_to_expr()),
            to_definition(_compare_op_bitwise_or_pair_to_expr()),
            to_definition(_compare_op_to_string()),
            to_definition(_comparison_to_expr()),
            to_definition(_compound_statement_to_expr()),
            to_definition(_conditional_to_expr()),
            to_definition(_conjunction_to_expr()),
            to_definition(_decorators_to_expr()),
            to_definition(_dict_to_expr()),
            to_definition(_disjunction_to_expr()),
            to_definition(_dotted_as_name_to_expr()),
            to_definition(_dotted_name_to_expr()),
            to_definition(_double_starred_kvpair_to_expr()),
            to_definition(_expression_to_expr()),
            to_definition(_factor_to_expr()),
            to_definition(_function_def_raw_to_expr()),
            to_definition(_function_definition_to_expr()),
            to_definition(_group_to_expr()),
            to_definition(_guard_to_expr()),
            to_definition(_import_from_to_expr()),
            to_definition(_import_from_as_name_to_expr()),
            to_definition(_import_from_targets_to_expr()),
            to_definition(_import_name_to_expr()),
            to_definition(_import_statement_to_expr()),
            to_definition(_inversion_to_expr()),
            to_definition(_keyword_pattern_to_expr()),
            to_definition(_keyword_patterns_to_expr()),
            to_definition(_kvpair_to_expr()),
            to_definition(_kwarg_to_expr()),
            to_definition(_kwarg_or_double_starred_to_expr()),
            to_definition(_kwarg_or_starred_to_expr()),
            to_definition(_lambda_to_expr()),
            to_definition(_lambda_param_no_default_to_expr()),
            to_definition(_lambda_parameters_to_expr()),
            to_definition(_lambda_star_etc_to_expr()),
            to_definition(_list_to_expr()),
            to_definition(_match_statement_to_expr()),
            to_definition(_module_to_expr()),
            to_definition(_name_to_expr()),
            to_definition(_name_or_attribute_to_expr()),
            to_definition(_named_expression_to_expr()),
            to_definition(_number_to_expr()),
            to_definition(_or_pattern_to_expr()),
            to_definition(_param_to_expr()),
            to_definition(_param_no_default_to_expr()),
            to_definition(_param_no_default_parameters_to_expr()),
            to_definition(_parameters_to_expr()),
            to_definition(_pattern_to_expr()),
            to_definition(_pattern_capture_target_to_expr()),
            to_definition(_patterns_to_expr()),
            to_definition(_pos_arg_to_expr()),
            to_definition(_positional_patterns_to_expr()),
            to_definition(_power_to_expr()),
            to_definition(_primary_to_expr()),
            to_definition(_primary_rhs_to_expr()),
            to_definition(_primary_with_rhs_to_expr()),
            to_definition(_raise_expression_to_expr()),
            to_definition(_raise_statement_to_expr()),
            to_definition(_relative_import_prefix_to_expr()),
            to_definition(_return_statement_to_expr()),
            to_definition(_set_to_expr()),
            to_definition(_shift_expression_to_expr()),
            to_definition(_simple_statement_to_expr()),
            to_definition(_simple_type_parameter_to_expr()),
            to_definition(_single_target_to_expr()),
            to_definition(_slice_to_expr()),
            to_definition(_slice_or_starred_expression_to_expr()),
            to_definition(_slices_to_expr()),
            to_definition(_star_atom_to_expr()),
            to_definition(_star_expression_to_expr()),
            to_definition(_star_named_expression_to_expr()),
            to_definition(_star_target_to_expr()),
            to_definition(_starred_expression_to_expr()),
            to_definition(_statement_to_expr()),
            to_definition(_string_prefix_to_text()),
            to_definition(_string_to_expr()),
            to_definition(_subject_expression_to_expr()),
            to_definition(_sum_to_expr()),
            to_definition(_t_primary_to_expr()),
            to_definition(_t_primary_and_name_to_expr()),
            to_definition(_target_with_star_atom_to_expr()),
            to_definition(_term_to_expr()),
            to_definition(_tuple_to_expr()),
            to_definition(_type_alias_to_expr()),
            to_definition(_type_parameter_to_expr()),
            to_definition(_typed_assignment_to_expr()),
            to_definition(_untyped_assignment_to_expr()),
            to_definition(_value_pattern_to_expr()),
            to_definition(_while_statement_to_expr()),
            to_definition(_escape_python_string()),
            to_definition(_python_float_literal_text()),
            to_definition(_to_python_comments()),
        ),
    )


module_ = _build_module()
