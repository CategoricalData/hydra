"""Python serializer: converts Python AST to concrete syntax (source code).

Mirror of packages/hydra-python/src/main/haskell/Hydra/Sources/Python/Serde.hs.
Serializes the Python syntax model into properly formatted Python source code.
"""

from hydra.core import Name
from hydra.dsl.python import Just, Nothing
from hydra.packaging import Module, Namespace

import hydra.dsl.meta.lib.equality as Equality
import hydra.dsl.meta.lib.lists as Lists
import hydra.dsl.meta.lib.logic as Logic
import hydra.dsl.meta.lib.maybes as Maybes
import hydra.dsl.meta.lib.strings as Strings
import hydra.dsl.meta.phantoms as Phantoms

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

NS = Namespace("hydra.python.serde")

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
#   [Constants.ns, Serialization.ns] L.++ (PySyntax.ns:KernelTypes.kernelTypesNamespaces)
DEPENDENCIES = [
    Namespace("hydra.constants"),
    Namespace("hydra.serialization"),
    Namespace("hydra.python.syntax"),
] + KERNEL_TYPES_NAMESPACES


_PLACEHOLDER = Module(
    Just("Python serializer: converts Python AST to concrete syntax"),
    NS,
    DEPENDENCIES,
    (),
)


def _def(local_name, term):
    return Phantoms.definition_in_module(_PLACEHOLDER, local_name, term)


def _local(local_name: str):
    return Phantoms.var(f"hydra.python.serde.{local_name}")


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


def _proj(type_name: str, field_name: str, var_name: str):
    """Apply a project to a variable: project(TypeName, fieldName) @@ var(varName)."""
    return _ap(
        Phantoms.project(Name(f"hydra.python.syntax.{type_name}"), Name(field_name)),
        Phantoms.var(var_name),
    )


def _unwrap(type_name: str, var_name: str):
    return _ap(
        Phantoms.unwrap(Name(f"hydra.python.syntax.{type_name}")),
        Phantoms.var(var_name),
    )


def _cst(s: str):
    """Serialization.cst @@ string(s)"""
    return _ap(serialization_cst, Phantoms.string(s))


def _space_sep(items_list):
    """Serialization.spaceSep @@ list [items...]"""
    return _ap(serialization_space_sep, items_list)


def _newline_sep(items_list):
    return _ap(serialization_newline_sep, items_list)


def _no_sep(items_list):
    return _ap(serialization_no_sep, items_list)


def _comma_sep_inline(items_list):
    """Serialization.commaSep @@ Serialization.inlineStyle @@ items"""
    return _ap(serialization_comma_sep, serialization_inline_style, items_list)


def _comma_sep_adaptive(items_list):
    return _ap(serialization_comma_sep_adaptive, items_list)


# ----------------------------------------------------------------------
# Type-name shorthand (for Phantoms.cases / Phantoms.project)
# ----------------------------------------------------------------------

def _ty(name: str) -> Name:
    return Name(f"hydra.python.syntax.{name}")


# ----------------------------------------------------------------------
# Definitions (alphabetical)
# ----------------------------------------------------------------------


def _annotated_rhs_to_expr():
    body = _space_sep(Phantoms.list_([
        _cst("="),
        Phantoms.cases(
            _ty("AnnotatedRhs"), Phantoms.var("arhs"), Nothing(),
            [
                Phantoms.field(
                    Name("star"),
                    Phantoms.lam(
                        "ses",
                        _ap(serialization_comma_sep, serialization_inline_style,
                            Lists.map(_local("starExpressionToExpr"), Phantoms.var("ses"))),
                    ),
                ),
                Phantoms.field(
                    Name("yield"),
                    Phantoms.lam("_", _cst("yield ...")),
                ),
            ],
        ),
    ]))
    return _def(
        "annotatedRhsToExpr",
        Phantoms.doc(
            "Serialize an annotated RHS",
            Phantoms.lambdas(["arhs"], body),
        ),
    )


def _annotated_statement_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("doc_"), _proj("AnnotatedStatement", "comment", "as_")),
            Phantoms.field(Name("stmt"), _proj("AnnotatedStatement", "statement", "as_")),
        ],
        _newline_sep(Phantoms.list_([
            _ap(serialization_cst, _ap(_local("toPythonComments"), Phantoms.var("doc_"))),
            _ap(_local("statementToExpr"), Phantoms.var("stmt")),
        ])),
    )
    return _def(
        "annotatedStatementToExpr",
        Phantoms.doc(
            "Serialize an annotated statement (with optional doc comment)",
            Phantoms.lambdas(["as_"], body),
        ),
    )


def _annotation_to_expr():
    body = _space_sep(Phantoms.list_([
        _cst(":"),
        _ap(_local("expressionToExpr"), _unwrap("Annotation", "ann")),
    ]))
    return _def(
        "annotationToExpr",
        Phantoms.doc(
            "Serialize a type annotation",
            Phantoms.lambdas(["ann"], body),
        ),
    )


def _args_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("pos"), _proj("Args", "positional", "args")),
            Phantoms.field(Name("ks"), _proj("Args", "kwargOrStarred", "args")),
            Phantoms.field(Name("kss"), _proj("Args", "kwargOrDoubleStarred", "args")),
        ],
        _comma_sep_adaptive(Lists.concat(Phantoms.list_([
            Lists.map(_local("posArgToExpr"), Phantoms.var("pos")),
            Lists.map(_local("kwargOrStarredToExpr"), Phantoms.var("ks")),
            Lists.map(_local("kwargOrDoubleStarredToExpr"), Phantoms.var("kss")),
        ]))),
    )
    return _def(
        "argsToExpr",
        Phantoms.doc(
            "Serialize function arguments",
            Phantoms.lambdas(["args"], body),
        ),
    )


def _assignment_to_expr():
    body = Phantoms.cases(
        _ty("Assignment"), Phantoms.var("a"), Nothing(),
        [
            Phantoms.field(Name("typed"), Phantoms.lam("t", _ap(_local("typedAssignmentToExpr"), Phantoms.var("t")))),
            Phantoms.field(Name("untyped"), Phantoms.lam("u", _ap(_local("untypedAssignmentToExpr"), Phantoms.var("u")))),
            Phantoms.field(Name("aug"), Phantoms.lam("_", _cst("... += ..."))),
        ],
    )
    return _def(
        "assignmentToExpr",
        Phantoms.doc(
            "Serialize an assignment",
            Phantoms.lambdas(["a"], body),
        ),
    )


def _assignment_expression_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("name"), _proj("AssignmentExpression", "name", "ae")),
            Phantoms.field(Name("expr"), _proj("AssignmentExpression", "expression", "ae")),
        ],
        _space_sep(Phantoms.list_([
            _ap(_local("nameToExpr"), Phantoms.var("name")),
            _cst(":="),
            _ap(_local("expressionToExpr"), Phantoms.var("expr")),
        ])),
    )
    return _def(
        "assignmentExpressionToExpr",
        Phantoms.doc(
            "Serialize an assignment expression (walrus operator)",
            Phantoms.lambdas(["ae"], body),
        ),
    )


def _atom_to_expr():
    body = Phantoms.cases(
        _ty("Atom"), Phantoms.var("atom"), Nothing(),
        [
            Phantoms.field(Name("dict"), Phantoms.lam("d", _ap(_local("dictToExpr"), Phantoms.var("d")))),
            Phantoms.field(Name("dictcomp"), Phantoms.lam("_", _cst("{...}"))),
            Phantoms.field(Name("ellipsis"), Phantoms.constant(_cst("..."))),
            Phantoms.field(Name("false"), Phantoms.constant(_cst("False"))),
            Phantoms.field(Name("genexp"), Phantoms.lam("_", _cst("(...)"))),
            Phantoms.field(Name("group"), Phantoms.lam("g", _ap(_local("groupToExpr"), Phantoms.var("g")))),
            Phantoms.field(Name("list"), Phantoms.lam("l", _ap(_local("listToExpr"), Phantoms.var("l")))),
            Phantoms.field(Name("listcomp"), Phantoms.lam("_", _cst("[...]"))),
            Phantoms.field(Name("name"), Phantoms.lam("n", _ap(_local("nameToExpr"), Phantoms.var("n")))),
            Phantoms.field(Name("none"), Phantoms.constant(_cst("None"))),
            Phantoms.field(Name("number"), Phantoms.lam("n", _ap(_local("numberToExpr"), Phantoms.var("n")))),
            Phantoms.field(Name("set"), Phantoms.lam("s", _ap(_local("setToExpr"), Phantoms.var("s")))),
            Phantoms.field(Name("setcomp"), Phantoms.lam("_", _cst("{...}"))),
            Phantoms.field(Name("string"), Phantoms.lam("s", _ap(_local("stringToExpr"), Phantoms.var("s")))),
            Phantoms.field(Name("true"), Phantoms.constant(_cst("True"))),
            Phantoms.field(Name("tuple"), Phantoms.lam("t", _ap(_local("tupleToExpr"), Phantoms.var("t")))),
        ],
    )
    return _def(
        "atomToExpr",
        Phantoms.doc(
            "Serialize a Python atom (literal or basic expression)",
            Phantoms.lambdas(["atom"], body),
        ),
    )


def _attribute_to_expr():
    body = _ap(
        serialization_dot_sep,
        Lists.map(_local("nameToExpr"), _unwrap("Attribute", "attr")),
    )
    return _def(
        "attributeToExpr",
        Phantoms.doc(
            "Serialize an attribute access",
            Phantoms.lambdas(["attr"], body),
        ),
    )


def _await_primary_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("await_"), _proj("AwaitPrimary", "await", "ap")),
            Phantoms.field(Name("primary"), _proj("AwaitPrimary", "primary", "ap")),
        ],
        Logic.if_else(
            Phantoms.var("await_"),
            _space_sep(Phantoms.list_([
                _cst("await"),
                _ap(_local("primaryToExpr"), Phantoms.var("primary")),
            ])),
            _ap(_local("primaryToExpr"), Phantoms.var("primary")),
        ),
    )
    return _def(
        "awaitPrimaryToExpr",
        Phantoms.doc(
            "Serialize an await primary expression",
            Phantoms.lambdas(["ap"], body),
        ),
    )


def _bitwise_op_body(local_name, type_name, lhs_local, rhs_local, op_str):
    return Phantoms.lets(
        [
            Phantoms.field(Name("lhs"), _proj(type_name, "lhs", lhs_local)),
            Phantoms.field(Name("rhs"), _proj(type_name, "rhs", lhs_local)),
        ],
        _ap(serialization_space_sep, Maybes.cat(Phantoms.list_([
            Maybes.map(
                Phantoms.lam("l", _space_sep(Phantoms.list_([
                    _ap(_local(local_name), Phantoms.var("l")),
                    _cst(op_str),
                ]))),
                Phantoms.var("lhs"),
            ),
            Phantoms.just(_ap(_local(rhs_local), Phantoms.var("rhs"))),
        ]))),
    )


def _bitwise_and_to_expr():
    return _def(
        "bitwiseAndToExpr",
        Phantoms.doc(
            "Serialize a bitwise AND expression",
            Phantoms.lambdas(["band"], _bitwise_op_body(
                "bitwiseAndToExpr", "BitwiseAnd", "band", "shiftExpressionToExpr", "&",
            )),
        ),
    )


def _bitwise_or_to_expr():
    return _def(
        "bitwiseOrToExpr",
        Phantoms.doc(
            "Serialize a bitwise OR expression",
            Phantoms.lambdas(["bor"], _bitwise_op_body(
                "bitwiseOrToExpr", "BitwiseOr", "bor", "bitwiseXorToExpr", "|",
            )),
        ),
    )


def _bitwise_xor_to_expr():
    return _def(
        "bitwiseXorToExpr",
        Phantoms.doc(
            "Serialize a bitwise XOR expression",
            Phantoms.lambdas(["bxor"], _bitwise_op_body(
                "bitwiseXorToExpr", "BitwiseXor", "bxor", "bitwiseAndToExpr", "^",
            )),
        ),
    )


def _block_to_expr():
    body = Phantoms.cases(
        _ty("Block"), Phantoms.var("b"), Nothing(),
        [
            Phantoms.field(
                Name("indented"),
                Phantoms.lam(
                    "groups",
                    _ap(
                        serialization_tab_indent_double_space,
                        Lists.map(
                            Phantoms.lam(
                                "stmts",
                                _newline_sep(Lists.map(_local("statementToExpr"), Phantoms.var("stmts"))),
                            ),
                            Phantoms.var("groups"),
                        ),
                    ),
                ),
            ),
            Phantoms.field(
                Name("simple"),
                Phantoms.lam(
                    "ss",
                    _ap(serialization_semicolon_sep, Lists.map(_local("simpleStatementToExpr"), Phantoms.var("ss"))),
                ),
            ),
        ],
    )
    return _def(
        "blockToExpr",
        Phantoms.doc(
            "Serialize a block",
            Phantoms.lambdas(["b"], body),
        ),
    )


def _capture_pattern_to_expr():
    body = _ap(_local("patternCaptureTargetToExpr"), _unwrap("CapturePattern", "cp"))
    return _def(
        "capturePatternToExpr",
        Phantoms.doc(
            "Serialize a capture pattern",
            Phantoms.lambdas(["cp"], body),
        ),
    )


def _case_block_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("patterns"), _proj("CaseBlock", "patterns", "cb")),
            Phantoms.field(Name("guard"), _proj("CaseBlock", "guard", "cb")),
            Phantoms.field(Name("body"), _proj("CaseBlock", "body", "cb")),
        ],
        _newline_sep(Phantoms.list_([
            _no_sep(Phantoms.list_([
                _ap(serialization_space_sep, Maybes.cat(Phantoms.list_([
                    Phantoms.just(_cst("case")),
                    Phantoms.just(_ap(_local("patternsToExpr"), Phantoms.var("patterns"))),
                    Maybes.map(_local("guardToExpr"), Phantoms.var("guard")),
                ]))),
                _cst(":"),
            ])),
            _ap(_local("blockToExpr"), Phantoms.var("body")),
        ])),
    )
    return _def(
        "caseBlockToExpr",
        Phantoms.doc(
            "Serialize a case block",
            Phantoms.lambdas(["cb"], body),
        ),
    )


def _class_definition_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("decs"), _proj("ClassDefinition", "decorators", "cd")),
            Phantoms.field(Name("name"), _proj("ClassDefinition", "name", "cd")),
            Phantoms.field(Name("args"), _proj("ClassDefinition", "arguments", "cd")),
            Phantoms.field(Name("body"), _proj("ClassDefinition", "body", "cd")),
            Phantoms.field(
                Name("argPart"),
                Maybes.map(
                    Phantoms.lam("a", _no_sep(Phantoms.list_([
                        _cst("("),
                        _ap(_local("argsToExpr"), Phantoms.var("a")),
                        _cst(")"),
                    ]))),
                    Phantoms.var("args"),
                ),
            ),
        ],
        _ap(serialization_newline_sep, Maybes.cat(Phantoms.list_([
            Maybes.map(_local("decoratorsToExpr"), Phantoms.var("decs")),
            Phantoms.just(_ap(serialization_no_sep, Maybes.cat(Phantoms.list_([
                Phantoms.just(_space_sep(Phantoms.list_([
                    _cst("class"),
                    _ap(_local("nameToExpr"), Phantoms.var("name")),
                ]))),
                Phantoms.var("argPart"),
                Phantoms.just(_cst(":")),
            ])))),
            Phantoms.just(_ap(_local("blockToExpr"), Phantoms.var("body"))),
        ]))),
    )
    return _def(
        "classDefinitionToExpr",
        Phantoms.doc(
            "Serialize a class definition",
            Phantoms.lambdas(["cd"], body),
        ),
    )


def _class_pattern_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("noa"), _proj("ClassPattern", "nameOrAttribute", "cp")),
            Phantoms.field(Name("pos"), _proj("ClassPattern", "positionalPatterns", "cp")),
            Phantoms.field(Name("kw"), _proj("ClassPattern", "keywordPatterns", "cp")),
        ],
        _ap(serialization_no_sep, Maybes.cat(Phantoms.list_([
            Phantoms.just(_ap(_local("nameOrAttributeToExpr"), Phantoms.var("noa"))),
            Phantoms.just(_cst("(")),
            Maybes.map(_local("positionalPatternsToExpr"), Phantoms.var("pos")),
            Maybes.map(_local("keywordPatternsToExpr"), Phantoms.var("kw")),
            Phantoms.just(_cst(")")),
        ]))),
    )
    return _def(
        "classPatternToExpr",
        Phantoms.doc(
            "Serialize a class pattern",
            Phantoms.lambdas(["cp"], body),
        ),
    )


def _closed_pattern_to_expr():
    body = Phantoms.cases(
        _ty("ClosedPattern"), Phantoms.var("cp"), Nothing(),
        [
            Phantoms.field(Name("literal"), Phantoms.lam("_", _cst("..."))),
            Phantoms.field(Name("capture"), Phantoms.lam("c", _ap(_local("capturePatternToExpr"), Phantoms.var("c")))),
            Phantoms.field(Name("wildcard"), Phantoms.constant(_cst("_"))),
            Phantoms.field(Name("value"), Phantoms.lam("v", _ap(_local("valuePatternToExpr"), Phantoms.var("v")))),
            Phantoms.field(Name("group"), Phantoms.lam("_", _cst("(...)"))),
            Phantoms.field(Name("sequence"), Phantoms.lam("_", _cst("[...]"))),
            Phantoms.field(Name("mapping"), Phantoms.lam("_", _cst("{...}"))),
            Phantoms.field(Name("class"), Phantoms.lam("c", _ap(_local("classPatternToExpr"), Phantoms.var("c")))),
        ],
    )
    return _def(
        "closedPatternToExpr",
        Phantoms.doc(
            "Serialize a closed pattern",
            Phantoms.lambdas(["cp"], body),
        ),
    )


def _compare_op_bitwise_or_pair_to_expr():
    body = _space_sep(Phantoms.list_([
        _ap(serialization_cst, _ap(_local("compareOpToString"),
            _ap(Phantoms.project(_ty("CompareOpBitwiseOrPair"), Name("operator")), Phantoms.var("pair")))),
        _ap(_local("bitwiseOrToExpr"),
            _ap(Phantoms.project(_ty("CompareOpBitwiseOrPair"), Name("rhs")), Phantoms.var("pair"))),
    ]))
    return _def(
        "compareOpBitwiseOrPairToExpr",
        Phantoms.doc(
            "Serialize a (compare-op, bitwise-or) pair as `<op> <rhs>`",
            Phantoms.lambdas(["pair"], body),
        ),
    )


def _compare_op_to_string():
    body = Phantoms.cases(
        _ty("CompareOp"), Phantoms.var("op"), Nothing(),
        [
            Phantoms.field(Name("eq"), Phantoms.constant(Phantoms.string("=="))),
            Phantoms.field(Name("noteq"), Phantoms.constant(Phantoms.string("!="))),
            Phantoms.field(Name("lte"), Phantoms.constant(Phantoms.string("<="))),
            Phantoms.field(Name("lt"), Phantoms.constant(Phantoms.string("<"))),
            Phantoms.field(Name("gte"), Phantoms.constant(Phantoms.string(">="))),
            Phantoms.field(Name("gt"), Phantoms.constant(Phantoms.string(">"))),
            Phantoms.field(Name("notin"), Phantoms.constant(Phantoms.string("not in"))),
            Phantoms.field(Name("in"), Phantoms.constant(Phantoms.string("in"))),
            Phantoms.field(Name("isnot"), Phantoms.constant(Phantoms.string("is not"))),
            Phantoms.field(Name("is"), Phantoms.constant(Phantoms.string("is"))),
        ],
    )
    return _def(
        "compareOpToString",
        Phantoms.doc(
            "Render a Python comparison operator to its source-code form",
            Phantoms.lambdas(["op"], body),
        ),
    )


def _comparison_to_expr():
    body = _let_chain(
        [
            ("lhs", _proj("Comparison", "lhs", "cmp")),
            ("rhs", _proj("Comparison", "rhs", "cmp")),
        ],
        Logic.if_else(
            Lists.null(Phantoms.var("rhs")),
            _ap(_local("bitwiseOrToExpr"), Phantoms.var("lhs")),
            _space_sep(
                Lists.cons(
                    _ap(_local("bitwiseOrToExpr"), Phantoms.var("lhs")),
                    Lists.map(_local("compareOpBitwiseOrPairToExpr"), Phantoms.var("rhs")),
                ),
            ),
        ),
    )
    return _def(
        "comparisonToExpr",
        Phantoms.doc(
            "Serialize a comparison expression: `<lhs>` if rhs is empty, otherwise `<lhs> <op1> <rhs1> <op2> <rhs2> ...`",
            Phantoms.lambdas(["cmp"], body),
        ),
    )


def _compound_statement_to_expr():
    body = Phantoms.cases(
        _ty("CompoundStatement"), Phantoms.var("cs"), Nothing(),
        [
            Phantoms.field(Name("function"), Phantoms.lam("f", _ap(_local("functionDefinitionToExpr"), Phantoms.var("f")))),
            Phantoms.field(Name("if"), Phantoms.lam("_", _cst("if ..."))),
            Phantoms.field(Name("classDef"), Phantoms.lam("c", _ap(_local("classDefinitionToExpr"), Phantoms.var("c")))),
            Phantoms.field(Name("with"), Phantoms.lam("_", _cst("with ..."))),
            Phantoms.field(Name("for"), Phantoms.lam("_", _cst("for ..."))),
            Phantoms.field(Name("try"), Phantoms.lam("_", _cst("try ..."))),
            Phantoms.field(Name("while"), Phantoms.lam("w", _ap(_local("whileStatementToExpr"), Phantoms.var("w")))),
            Phantoms.field(Name("match"), Phantoms.lam("m", _ap(_local("matchStatementToExpr"), Phantoms.var("m")))),
        ],
    )
    return _def(
        "compoundStatementToExpr",
        Phantoms.doc(
            "Serialize a compound (multi-line) Python statement",
            Phantoms.lambdas(["cs"], body),
        ),
    )


def _conditional_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("body"), _proj("Conditional", "body", "c")),
            Phantoms.field(Name("cond"), _proj("Conditional", "if", "c")),
            Phantoms.field(Name("elseExpr"), _proj("Conditional", "else", "c")),
        ],
        _space_sep(Phantoms.list_([
            _ap(_local("disjunctionToExpr"), Phantoms.var("body")),
            _cst("if"),
            _ap(_local("disjunctionToExpr"), Phantoms.var("cond")),
            _cst("else"),
            _ap(_local("expressionToExpr"), Phantoms.var("elseExpr")),
        ])),
    )
    return _def(
        "conditionalToExpr",
        Phantoms.doc(
            "Serialize a conditional expression (ternary)",
            Phantoms.lambdas(["c"], body),
        ),
    )


def _conjunction_to_expr():
    body = _ap(
        serialization_symbol_sep, Phantoms.string("and"), serialization_inline_style,
        Lists.map(_local("inversionToExpr"), _unwrap("Conjunction", "c")),
    )
    return _def(
        "conjunctionToExpr",
        Phantoms.doc(
            "Serialize a conjunction (and expression)",
            Phantoms.lambdas(["c"], body),
        ),
    )


def _decorators_to_expr():
    body = _ap(
        serialization_newline_sep,
        Lists.map(
            Phantoms.lam(
                "ne",
                _no_sep(Phantoms.list_([
                    _cst("@"),
                    _ap(_local("namedExpressionToExpr"), Phantoms.var("ne")),
                ])),
            ),
            _unwrap("Decorators", "decs"),
        ),
    )
    return _def(
        "decoratorsToExpr",
        Phantoms.doc(
            "Serialize decorators",
            Phantoms.lambdas(["decs"], body),
        ),
    )


def _dict_to_expr():
    body = _ap(
        serialization_curly_braces_list,
        Phantoms.nothing(),
        serialization_half_block_style,
        Lists.map(_local("doubleStarredKvpairToExpr"), _unwrap("Dict", "d")),
    )
    return _def(
        "dictToExpr",
        Phantoms.doc(
            "Serialize a Python dictionary",
            Phantoms.lambdas(["d"], body),
        ),
    )


def _disjunction_to_expr():
    body = _ap(
        serialization_symbol_sep, Phantoms.string("or"), serialization_inline_style,
        Lists.map(_local("conjunctionToExpr"), _unwrap("Disjunction", "d")),
    )
    return _def(
        "disjunctionToExpr",
        Phantoms.doc(
            "Serialize a disjunction (or expression)",
            Phantoms.lambdas(["d"], body),
        ),
    )


def _dotted_as_name_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("name"), _proj("DottedAsName", "name", "dan")),
            Phantoms.field(Name("alias"), _proj("DottedAsName", "as", "dan")),
        ],
        _ap(serialization_space_sep, Maybes.cat(Phantoms.list_([
            Phantoms.just(_ap(_local("dottedNameToExpr"), Phantoms.var("name"))),
            Maybes.map(
                Phantoms.lam("a", _space_sep(Phantoms.list_([
                    _cst("as"),
                    _ap(_local("nameToExpr"), Phantoms.var("a")),
                ]))),
                Phantoms.var("alias"),
            ),
        ]))),
    )
    return _def(
        "dottedAsNameToExpr",
        Phantoms.doc(
            "Serialize a dotted as name",
            Phantoms.lambdas(["dan"], body),
        ),
    )


def _dotted_name_to_expr():
    body = _ap(
        serialization_cst,
        Strings.intercalate(
            Phantoms.string("."),
            Lists.map(
                Phantoms.lam("n", _ap(Phantoms.unwrap(_ty("Name")), Phantoms.var("n"))),
                _unwrap("DottedName", "dn"),
            ),
        ),
    )
    return _def(
        "dottedNameToExpr",
        Phantoms.doc(
            "Serialize a dotted name (e.g., module.submodule)",
            Phantoms.lambdas(["dn"], body),
        ),
    )


def _double_starred_kvpair_to_expr():
    body = Phantoms.cases(
        _ty("DoubleStarredKvpair"), Phantoms.var("dskv"), Nothing(),
        [
            Phantoms.field(Name("pair"), Phantoms.lam("p", _ap(_local("kvpairToExpr"), Phantoms.var("p")))),
            Phantoms.field(Name("starred"), Phantoms.lam("e", _no_sep(Phantoms.list_([
                _cst("**"),
                _ap(_local("bitwiseOrToExpr"), Phantoms.var("e")),
            ])))),
        ],
    )
    return _def(
        "doubleStarredKvpairToExpr",
        Phantoms.doc(
            "Serialize a double-starred key-value pair",
            Phantoms.lambdas(["dskv"], body),
        ),
    )


def _expression_to_expr():
    body = Phantoms.cases(
        _ty("Expression"), Phantoms.var("expr"), Nothing(),
        [
            Phantoms.field(Name("simple"), Phantoms.lam("d", _ap(_local("disjunctionToExpr"), Phantoms.var("d")))),
            Phantoms.field(Name("conditional"), Phantoms.lam("c", _ap(_local("conditionalToExpr"), Phantoms.var("c")))),
            Phantoms.field(Name("lambda"), Phantoms.lam("l", _ap(_local("lambdaToExpr"), Phantoms.var("l")))),
        ],
    )
    return _def(
        "expressionToExpr",
        Phantoms.doc(
            "Serialize a Python expression",
            Phantoms.lambdas(["expr"], body),
        ),
    )


def _factor_to_expr():
    body = Phantoms.cases(
        _ty("Factor"), Phantoms.var("f"), Nothing(),
        [
            Phantoms.field(
                Name("positive"),
                Phantoms.lam("inner", _no_sep(Phantoms.list_([
                    _cst("+"),
                    _ap(_local("factorToExpr"), Phantoms.var("inner")),
                ]))),
            ),
            Phantoms.field(
                Name("negative"),
                Phantoms.lam("inner", _no_sep(Phantoms.list_([
                    _cst("-"),
                    _ap(_local("factorToExpr"), Phantoms.var("inner")),
                ]))),
            ),
            Phantoms.field(
                Name("complement"),
                Phantoms.lam("inner", _no_sep(Phantoms.list_([
                    _cst("~"),
                    _ap(_local("factorToExpr"), Phantoms.var("inner")),
                ]))),
            ),
            Phantoms.field(Name("simple"), Phantoms.lam("p", _ap(_local("powerToExpr"), Phantoms.var("p")))),
        ],
    )
    return _def(
        "factorToExpr",
        Phantoms.doc(
            "Serialize a factor expression",
            Phantoms.lambdas(["f"], body),
        ),
    )


def _function_def_raw_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("async_"), _proj("FunctionDefRaw", "async", "fdr")),
            Phantoms.field(Name("name"), _proj("FunctionDefRaw", "name", "fdr")),
            Phantoms.field(Name("tparams"), _proj("FunctionDefRaw", "typeParams", "fdr")),
            Phantoms.field(Name("params"), _proj("FunctionDefRaw", "params", "fdr")),
            Phantoms.field(Name("retType"), _proj("FunctionDefRaw", "returnType", "fdr")),
            Phantoms.field(Name("block"), _proj("FunctionDefRaw", "block", "fdr")),
            Phantoms.field(
                Name("asyncKw"),
                Logic.if_else(
                    Phantoms.var("async_"),
                    Phantoms.just(_cst("async")),
                    Phantoms.nothing(),
                ),
            ),
            Phantoms.field(
                Name("tparamPart"),
                Logic.if_else(
                    Lists.null(Phantoms.var("tparams")),
                    Phantoms.nothing(),
                    Phantoms.just(_ap(
                        serialization_bracket_list,
                        serialization_inline_style,
                        Lists.map(_local("typeParameterToExpr"), Phantoms.var("tparams")),
                    )),
                ),
            ),
            Phantoms.field(
                Name("paramPart"),
                Maybes.map(_local("parametersToExpr"), Phantoms.var("params")),
            ),
            Phantoms.field(
                Name("retPart"),
                Maybes.map(
                    Phantoms.lam("t", _space_sep(Phantoms.list_([
                        _cst("->"),
                        _ap(_local("expressionToExpr"), Phantoms.var("t")),
                    ]))),
                    Phantoms.var("retType"),
                ),
            ),
        ],
        _newline_sep(Phantoms.list_([
            _no_sep(Phantoms.list_([
                _ap(serialization_space_sep, Maybes.cat(Phantoms.list_([
                    Phantoms.var("asyncKw"),
                    Phantoms.just(_cst("def")),
                    Phantoms.just(_ap(serialization_no_sep, Maybes.cat(Phantoms.list_([
                        Phantoms.just(_ap(_local("nameToExpr"), Phantoms.var("name"))),
                        Phantoms.var("tparamPart"),
                        Phantoms.just(_cst("(")),
                        Phantoms.var("paramPart"),
                        Phantoms.just(_cst(")")),
                    ])))),
                    Phantoms.var("retPart"),
                ]))),
                _cst(":"),
            ])),
            _ap(_local("blockToExpr"), Phantoms.var("block")),
        ])),
    )
    return _def(
        "functionDefRawToExpr",
        Phantoms.doc(
            "Serialize a raw function definition",
            Phantoms.lambdas(["fdr"], body),
        ),
    )


def _function_definition_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("decs"), _proj("FunctionDefinition", "decorators", "fd")),
            Phantoms.field(Name("raw"), _proj("FunctionDefinition", "raw", "fd")),
        ],
        _ap(serialization_newline_sep, Maybes.cat(Phantoms.list_([
            Maybes.map(_local("decoratorsToExpr"), Phantoms.var("decs")),
            Phantoms.just(_ap(_local("functionDefRawToExpr"), Phantoms.var("raw"))),
        ]))),
    )
    return _def(
        "functionDefinitionToExpr",
        Phantoms.doc(
            "Serialize a function definition",
            Phantoms.lambdas(["fd"], body),
        ),
    )


def _group_to_expr():
    body = Phantoms.cases(
        _ty("Group"), Phantoms.var("g"), Nothing(),
        [
            Phantoms.field(Name("expression"), Phantoms.lam("ne", _ap(_local("namedExpressionToExpr"), Phantoms.var("ne")))),
            Phantoms.field(Name("yield"), Phantoms.lam("_", _cst("(yield ...)"))),
        ],
    )
    return _def(
        "groupToExpr",
        Phantoms.doc(
            "Serialize a parenthesized group",
            Phantoms.lambdas(["g"], body),
        ),
    )


def _guard_to_expr():
    body = _space_sep(Phantoms.list_([
        _cst("if"),
        _ap(_local("namedExpressionToExpr"), _unwrap("Guard", "g")),
    ]))
    return _def(
        "guardToExpr",
        Phantoms.doc(
            "Serialize a guard clause",
            Phantoms.lambdas(["g"], body),
        ),
    )


def _import_from_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("prefixes"), _proj("ImportFrom", "prefixes", "if_")),
            Phantoms.field(Name("name"), _proj("ImportFrom", "dottedName", "if_")),
            Phantoms.field(Name("targets"), _proj("ImportFrom", "targets", "if_")),
            Phantoms.field(
                Name("lhs"),
                _ap(serialization_no_sep, Maybes.cat(Lists.concat(Phantoms.list_([
                    Lists.map(
                        Phantoms.lam("p", Phantoms.just(_ap(_local("relativeImportPrefixToExpr"), Phantoms.var("p")))),
                        Phantoms.var("prefixes"),
                    ),
                    Phantoms.list_([Maybes.map(_local("dottedNameToExpr"), Phantoms.var("name"))]),
                ])))),
            ),
        ],
        _space_sep(Phantoms.list_([
            _cst("from"),
            Phantoms.var("lhs"),
            _cst("import"),
            _ap(_local("importFromTargetsToExpr"), Phantoms.var("targets")),
        ])),
    )
    return _def(
        "importFromToExpr",
        Phantoms.doc(
            "Serialize an import from statement",
            Phantoms.lambdas(["if_"], body),
        ),
    )


def _import_from_as_name_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("name"), _proj("ImportFromAsName", "name", "ifan")),
            Phantoms.field(Name("alias"), _proj("ImportFromAsName", "as", "ifan")),
        ],
        Maybes.maybe(
            _ap(_local("nameToExpr"), Phantoms.var("name")),
            Phantoms.lam(
                "a",
                _space_sep(Phantoms.list_([
                    _ap(_local("nameToExpr"), Phantoms.var("name")),
                    _cst("as"),
                    _ap(_local("nameToExpr"), Phantoms.var("a")),
                ])),
            ),
            Phantoms.var("alias"),
        ),
    )
    return _def(
        "importFromAsNameToExpr",
        Phantoms.doc(
            "Serialize an import from as name",
            Phantoms.lambdas(["ifan"], body),
        ),
    )


def _import_from_targets_to_expr():
    body = Phantoms.cases(
        _ty("ImportFromTargets"), Phantoms.var("t"), Nothing(),
        [
            Phantoms.field(
                Name("simple"),
                Phantoms.lam(
                    "names",
                    _comma_sep_inline(Lists.map(_local("importFromAsNameToExpr"), Phantoms.var("names"))),
                ),
            ),
            Phantoms.field(
                Name("parens"),
                Phantoms.lam(
                    "names",
                    _no_sep(Phantoms.list_([
                        _cst("("),
                        _comma_sep_inline(Lists.map(_local("importFromAsNameToExpr"), Phantoms.var("names"))),
                        _cst(")"),
                    ])),
                ),
            ),
            Phantoms.field(Name("star"), Phantoms.constant(_cst("*"))),
        ],
    )
    return _def(
        "importFromTargetsToExpr",
        Phantoms.doc(
            "Serialize import from targets",
            Phantoms.lambdas(["t"], body),
        ),
    )


def _import_name_to_expr():
    body = _space_sep(Phantoms.list_([
        _cst("import"),
        _comma_sep_inline(Lists.map(_local("dottedAsNameToExpr"), _unwrap("ImportName", "in_"))),
    ]))
    return _def(
        "importNameToExpr",
        Phantoms.doc(
            "Serialize an import name",
            Phantoms.lambdas(["in_"], body),
        ),
    )


def _import_statement_to_expr():
    body = Phantoms.cases(
        _ty("ImportStatement"), Phantoms.var("is_"), Nothing(),
        [
            Phantoms.field(Name("name"), Phantoms.lam("n", _ap(_local("importNameToExpr"), Phantoms.var("n")))),
            Phantoms.field(Name("from"), Phantoms.lam("f", _ap(_local("importFromToExpr"), Phantoms.var("f")))),
        ],
    )
    return _def(
        "importStatementToExpr",
        Phantoms.doc(
            "Serialize an import statement",
            Phantoms.lambdas(["is_"], body),
        ),
    )


def _inversion_to_expr():
    body = Phantoms.cases(
        _ty("Inversion"), Phantoms.var("i"), Nothing(),
        [
            Phantoms.field(
                Name("not"),
                Phantoms.lam("other", _space_sep(Phantoms.list_([
                    _cst("not"),
                    _ap(_local("inversionToExpr"), Phantoms.var("other")),
                ]))),
            ),
            Phantoms.field(Name("simple"), Phantoms.lam("c", _ap(_local("comparisonToExpr"), Phantoms.var("c")))),
        ],
    )
    return _def(
        "inversionToExpr",
        Phantoms.doc(
            "Serialize an inversion (not expression)",
            Phantoms.lambdas(["i"], body),
        ),
    )


def _keyword_pattern_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("name"), _proj("KeywordPattern", "name", "kp")),
            Phantoms.field(Name("pat"), _proj("KeywordPattern", "pattern", "kp")),
        ],
        _no_sep(Phantoms.list_([
            _ap(_local("nameToExpr"), Phantoms.var("name")),
            _cst("="),
            _ap(_local("patternToExpr"), Phantoms.var("pat")),
        ])),
    )
    return _def(
        "keywordPatternToExpr",
        Phantoms.doc(
            "Serialize a keyword pattern",
            Phantoms.lambdas(["kp"], body),
        ),
    )


def _keyword_patterns_to_expr():
    body = _comma_sep_inline(Lists.map(_local("keywordPatternToExpr"), _unwrap("KeywordPatterns", "kp")))
    return _def(
        "keywordPatternsToExpr",
        Phantoms.doc(
            "Serialize keyword patterns",
            Phantoms.lambdas(["kp"], body),
        ),
    )


def _kvpair_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("k"), _proj("Kvpair", "key", "kv")),
            Phantoms.field(Name("v"), _proj("Kvpair", "value", "kv")),
        ],
        _space_sep(Phantoms.list_([
            _no_sep(Phantoms.list_([
                _ap(_local("expressionToExpr"), Phantoms.var("k")),
                _cst(":"),
            ])),
            _ap(_local("expressionToExpr"), Phantoms.var("v")),
        ])),
    )
    return _def(
        "kvpairToExpr",
        Phantoms.doc(
            "Serialize a key-value pair",
            Phantoms.lambdas(["kv"], body),
        ),
    )


def _kwarg_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("name"), _proj("Kwarg", "name", "k")),
            Phantoms.field(Name("expr"), _proj("Kwarg", "value", "k")),
        ],
        _no_sep(Phantoms.list_([
            _ap(_local("nameToExpr"), Phantoms.var("name")),
            _cst("="),
            _ap(_local("expressionToExpr"), Phantoms.var("expr")),
        ])),
    )
    return _def(
        "kwargToExpr",
        Phantoms.doc(
            "Serialize a keyword argument",
            Phantoms.lambdas(["k"], body),
        ),
    )


def _kwarg_or_double_starred_to_expr():
    body = Phantoms.cases(
        _ty("KwargOrDoubleStarred"), Phantoms.var("kds"), Nothing(),
        [
            Phantoms.field(Name("kwarg"), Phantoms.lam("k", _ap(_local("kwargToExpr"), Phantoms.var("k")))),
            Phantoms.field(
                Name("doubleStarred"),
                Phantoms.lam("e", _no_sep(Phantoms.list_([
                    _cst("**"),
                    _ap(_local("expressionToExpr"), Phantoms.var("e")),
                ]))),
            ),
        ],
    )
    return _def(
        "kwargOrDoubleStarredToExpr",
        Phantoms.doc(
            "Serialize a kwarg or double starred",
            Phantoms.lambdas(["kds"], body),
        ),
    )


def _kwarg_or_starred_to_expr():
    body = Phantoms.cases(
        _ty("KwargOrStarred"), Phantoms.var("ks"), Nothing(),
        [
            Phantoms.field(Name("kwarg"), Phantoms.lam("k", _ap(_local("kwargToExpr"), Phantoms.var("k")))),
            Phantoms.field(Name("starred"), Phantoms.lam("se", _ap(_local("starredExpressionToExpr"), Phantoms.var("se")))),
        ],
    )
    return _def(
        "kwargOrStarredToExpr",
        Phantoms.doc(
            "Serialize a kwarg or starred",
            Phantoms.lambdas(["ks"], body),
        ),
    )


def _lambda_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("params"), _proj("Lambda", "params", "l")),
            Phantoms.field(Name("body"), _proj("Lambda", "body", "l")),
        ],
        _ap(serialization_parens, _space_sep(Phantoms.list_([
            _cst("lambda"),
            _no_sep(Phantoms.list_([
                _ap(_local("lambdaParametersToExpr"), Phantoms.var("params")),
                _cst(":"),
            ])),
            _ap(_local("expressionToExpr"), Phantoms.var("body")),
        ]))),
    )
    return _def(
        "lambdaToExpr",
        Phantoms.doc(
            "Serialize a lambda expression",
            Phantoms.lambdas(["l"], body),
        ),
    )


def _lambda_param_no_default_to_expr():
    body = _ap(_local("nameToExpr"), _unwrap("LambdaParamNoDefault", "p"))
    return _def(
        "lambdaParamNoDefaultToExpr",
        Phantoms.doc(
            "Serialize a lambda parameter without default",
            Phantoms.lambdas(["p"], body),
        ),
    )


def _lambda_parameters_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("nodef"), _proj("LambdaParameters", "paramNoDefault", "lp")),
        ],
        _comma_sep_inline(Lists.map(_local("lambdaParamNoDefaultToExpr"), Phantoms.var("nodef"))),
    )
    return _def(
        "lambdaParametersToExpr",
        Phantoms.doc(
            "Serialize lambda parameters",
            Phantoms.lambdas(["lp"], body),
        ),
    )


def _lambda_star_etc_to_expr():
    body = Phantoms.cases(
        _ty("LambdaStarEtc"), Phantoms.var("lse"), Nothing(),
        [
            Phantoms.field(Name("paramNoDefault"), Phantoms.lam("p", _ap(_local("lambdaParamNoDefaultToExpr"), Phantoms.var("p")))),
            Phantoms.field(Name("star"), Phantoms.lam("_", _cst("*..."))),
            Phantoms.field(Name("paramMaybeDefault"), Phantoms.lam("_", _cst("..."))),
            Phantoms.field(Name("kwds"), Phantoms.lam("_", _cst("**..."))),
        ],
    )
    return _def(
        "lambdaStarEtcToExpr",
        Phantoms.doc(
            "Serialize lambda star etc",
            Phantoms.lambdas(["lse"], body),
        ),
    )


def _list_to_expr():
    body = _ap(
        serialization_bracket_list_adaptive,
        Lists.map(_local("starNamedExpressionToExpr"), _unwrap("List", "l")),
    )
    return _def(
        "listToExpr",
        Phantoms.doc(
            "Serialize a Python list",
            Phantoms.lambdas(["l"], body),
        ),
    )


def _match_statement_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("subj"), _proj("MatchStatement", "subject", "ms")),
            Phantoms.field(Name("cases"), _proj("MatchStatement", "cases", "ms")),
        ],
        _newline_sep(Phantoms.list_([
            _space_sep(Phantoms.list_([
                _cst("match"),
                _no_sep(Phantoms.list_([
                    _ap(_local("subjectExpressionToExpr"), Phantoms.var("subj")),
                    _cst(":"),
                ])),
            ])),
            _ap(serialization_tab_indent_double_space,
                Lists.map(_local("caseBlockToExpr"), Phantoms.var("cases"))),
        ])),
    )
    return _def(
        "matchStatementToExpr",
        Phantoms.doc(
            "Serialize a match statement",
            Phantoms.lambdas(["ms"], body),
        ),
    )


def _module_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(
                Name("warning"),
                _ap(serialization_cst, _ap(_local("toPythonComments"),
                    Phantoms.var("hydra.constants.warningAutoGeneratedFile"))),  # noqa: E501
            ),
            Phantoms.field(
                Name("groups"),
                Lists.map(
                    Phantoms.lam(
                        "group",
                        _newline_sep(Lists.map(_local("statementToExpr"), Phantoms.var("group"))),
                    ),
                    _unwrap("Module", "mod"),
                ),
            ),
        ],
        _ap(serialization_double_newline_sep,
            Lists.cons(Phantoms.var("warning"), Phantoms.var("groups"))),
    )
    return _def(
        "moduleToExpr",
        Phantoms.doc(
            "Serialize a Python module to an AST expression",
            Phantoms.lambdas(["mod"], body),
        ),
    )


def _name_to_expr():
    body = _ap(serialization_cst, _unwrap("Name", "n"))
    return _def(
        "nameToExpr",
        Phantoms.doc(
            "Serialize a Python name/identifier",
            Phantoms.lambdas(["n"], body),
        ),
    )


def _name_or_attribute_to_expr():
    body = _ap(serialization_dot_sep, Lists.map(_local("nameToExpr"), _unwrap("NameOrAttribute", "noa")))
    return _def(
        "nameOrAttributeToExpr",
        Phantoms.doc(
            "Serialize a name or attribute",
            Phantoms.lambdas(["noa"], body),
        ),
    )


def _named_expression_to_expr():
    body = Phantoms.cases(
        _ty("NamedExpression"), Phantoms.var("ne"), Nothing(),
        [
            Phantoms.field(Name("simple"), Phantoms.lam("e", _ap(_local("expressionToExpr"), Phantoms.var("e")))),
            Phantoms.field(Name("assignment"), Phantoms.lam("ae", _ap(_local("assignmentExpressionToExpr"), Phantoms.var("ae")))),
        ],
    )
    return _def(
        "namedExpressionToExpr",
        Phantoms.doc(
            "Serialize a named expression",
            Phantoms.lambdas(["ne"], body),
        ),
    )


def _number_to_expr():
    body = Phantoms.cases(
        _ty("Number"), Phantoms.var("num"), Nothing(),
        [
            Phantoms.field(
                Name("float"),
                Phantoms.lam("f", _ap(serialization_cst,
                    _ap(_local("pythonFloatLiteralText"),
                        _ap(Phantoms.var("hydra.lib.literals.showBigfloat"), Phantoms.var("f"))))),
            ),
            Phantoms.field(
                Name("integer"),
                Phantoms.lam("i", _ap(serialization_cst,
                    _ap(Phantoms.var("hydra.lib.literals.showBigint"), Phantoms.var("i")))),
            ),
        ],
    )
    return _def(
        "numberToExpr",
        Phantoms.doc(
            "Serialize a Python number literal",
            Phantoms.lambdas(["num"], body),
        ),
    )


def _or_pattern_to_expr():
    body = _ap(
        serialization_symbol_sep, Phantoms.string("|"), serialization_inline_style,
        Lists.map(_local("closedPatternToExpr"), _unwrap("OrPattern", "op")),
    )
    return _def(
        "orPatternToExpr",
        Phantoms.doc(
            "Serialize an or pattern",
            Phantoms.lambdas(["op"], body),
        ),
    )


def _param_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("name"), _proj("Param", "name", "p")),
            Phantoms.field(Name("ann"), _proj("Param", "annotation", "p")),
        ],
        _ap(serialization_no_sep, Maybes.cat(Phantoms.list_([
            Phantoms.just(_ap(_local("nameToExpr"), Phantoms.var("name"))),
            Maybes.map(_local("annotationToExpr"), Phantoms.var("ann")),
        ]))),
    )
    return _def(
        "paramToExpr",
        Phantoms.doc(
            "Serialize a parameter",
            Phantoms.lambdas(["p"], body),
        ),
    )


def _param_no_default_to_expr():
    body = _ap(_local("paramToExpr"), _proj("ParamNoDefault", "param", "pnd"))
    return _def(
        "paramNoDefaultToExpr",
        Phantoms.doc(
            "Serialize a parameter without default",
            Phantoms.lambdas(["pnd"], body),
        ),
    )


def _param_no_default_parameters_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("nodef"), _proj("ParamNoDefaultParameters", "paramNoDefault", "pndp")),
        ],
        _comma_sep_adaptive(Lists.map(_local("paramNoDefaultToExpr"), Phantoms.var("nodef"))),
    )
    return _def(
        "paramNoDefaultParametersToExpr",
        Phantoms.doc(
            "Serialize parameters without defaults",
            Phantoms.lambdas(["pndp"], body),
        ),
    )


def _parameters_to_expr():
    body = Phantoms.cases(
        _ty("Parameters"), Phantoms.var("p"), Nothing(),
        [
            Phantoms.field(Name("paramNoDefault"), Phantoms.lam("pnd", _ap(_local("paramNoDefaultParametersToExpr"), Phantoms.var("pnd")))),
            Phantoms.field(Name("slashNoDefault"), Phantoms.lam("_", _cst("..."))),
            Phantoms.field(Name("slashWithDefault"), Phantoms.lam("_", _cst("..."))),
        ],
    )
    return _def(
        "parametersToExpr",
        Phantoms.doc(
            "Serialize function parameters",
            Phantoms.lambdas(["p"], body),
        ),
    )


def _pattern_to_expr():
    body = Phantoms.cases(
        _ty("Pattern"), Phantoms.var("p"), Nothing(),
        [
            Phantoms.field(Name("or"), Phantoms.lam("op", _ap(_local("orPatternToExpr"), Phantoms.var("op")))),
            Phantoms.field(Name("as"), Phantoms.lam("_", _cst("... as ..."))),
        ],
    )
    return _def(
        "patternToExpr",
        Phantoms.doc(
            "Serialize a pattern",
            Phantoms.lambdas(["p"], body),
        ),
    )


def _pattern_capture_target_to_expr():
    body = _ap(_local("nameToExpr"), _unwrap("PatternCaptureTarget", "pct"))
    return _def(
        "patternCaptureTargetToExpr",
        Phantoms.doc(
            "Serialize a pattern capture target",
            Phantoms.lambdas(["pct"], body),
        ),
    )


def _patterns_to_expr():
    body = Phantoms.cases(
        _ty("Patterns"), Phantoms.var("ps"), Nothing(),
        [
            Phantoms.field(Name("pattern"), Phantoms.lam("p", _ap(_local("patternToExpr"), Phantoms.var("p")))),
            Phantoms.field(Name("sequence"), Phantoms.lam("_", _cst("..."))),
        ],
    )
    return _def(
        "patternsToExpr",
        Phantoms.doc(
            "Serialize patterns",
            Phantoms.lambdas(["ps"], body),
        ),
    )


def _pos_arg_to_expr():
    body = Phantoms.cases(
        _ty("PosArg"), Phantoms.var("pa"), Nothing(),
        [
            Phantoms.field(Name("starred"), Phantoms.lam("se", _ap(_local("starredExpressionToExpr"), Phantoms.var("se")))),
            Phantoms.field(Name("assignment"), Phantoms.lam("ae", _ap(_local("assignmentExpressionToExpr"), Phantoms.var("ae")))),
            Phantoms.field(Name("expression"), Phantoms.lam("e", _ap(_local("expressionToExpr"), Phantoms.var("e")))),
        ],
    )
    return _def(
        "posArgToExpr",
        Phantoms.doc(
            "Serialize a positional argument",
            Phantoms.lambdas(["pa"], body),
        ),
    )


def _positional_patterns_to_expr():
    body = _comma_sep_inline(Lists.map(_local("patternToExpr"), _unwrap("PositionalPatterns", "pp")))
    return _def(
        "positionalPatternsToExpr",
        Phantoms.doc(
            "Serialize positional patterns",
            Phantoms.lambdas(["pp"], body),
        ),
    )


def _power_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("lhs"), _proj("Power", "lhs", "p")),
            Phantoms.field(Name("rhs"), _proj("Power", "rhs", "p")),
        ],
        _ap(serialization_space_sep, Maybes.cat(Phantoms.list_([
            Phantoms.just(_ap(_local("awaitPrimaryToExpr"), Phantoms.var("lhs"))),
            Maybes.map(
                Phantoms.lam("r", _space_sep(Phantoms.list_([
                    _cst("**"),
                    _ap(_local("factorToExpr"), Phantoms.var("r")),
                ]))),
                Phantoms.var("rhs"),
            ),
        ]))),
    )
    return _def(
        "powerToExpr",
        Phantoms.doc(
            "Serialize a power expression",
            Phantoms.lambdas(["p"], body),
        ),
    )


def _primary_to_expr():
    body = Phantoms.cases(
        _ty("Primary"), Phantoms.var("p"), Nothing(),
        [
            Phantoms.field(Name("simple"), Phantoms.lam("a", _ap(_local("atomToExpr"), Phantoms.var("a")))),
            Phantoms.field(Name("compound"), Phantoms.lam("pwr", _ap(_local("primaryWithRhsToExpr"), Phantoms.var("pwr")))),
        ],
    )
    return _def(
        "primaryToExpr",
        Phantoms.doc(
            "Serialize a primary expression",
            Phantoms.lambdas(["p"], body),
        ),
    )


def _primary_rhs_to_expr():
    body = Phantoms.cases(
        _ty("PrimaryRhs"), Phantoms.var("rhs"), Nothing(),
        [
            Phantoms.field(
                Name("call"),
                Phantoms.lam("args", _no_sep(Phantoms.list_([
                    _cst("("),
                    _ap(_local("argsToExpr"), Phantoms.var("args")),
                    _cst(")"),
                ]))),
            ),
            Phantoms.field(
                Name("project"),
                Phantoms.lam("name", _no_sep(Phantoms.list_([
                    _cst("."),
                    _ap(_local("nameToExpr"), Phantoms.var("name")),
                ]))),
            ),
            Phantoms.field(
                Name("slices"),
                Phantoms.lam("slices", _no_sep(Phantoms.list_([
                    _cst("["),
                    _ap(_local("slicesToExpr"), Phantoms.var("slices")),
                    _cst("]"),
                ]))),
            ),
            Phantoms.field(Name("genexp"), Phantoms.lam("_", _cst("[...]"))),
        ],
    )
    return _def(
        "primaryRhsToExpr",
        Phantoms.doc(
            "Serialize a primary RHS",
            Phantoms.lambdas(["rhs"], body),
        ),
    )


def _primary_with_rhs_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("prim"), _proj("PrimaryWithRhs", "primary", "pwr")),
            Phantoms.field(Name("rhs"), _proj("PrimaryWithRhs", "rhs", "pwr")),
        ],
        _no_sep(Phantoms.list_([
            _ap(_local("primaryToExpr"), Phantoms.var("prim")),
            _ap(_local("primaryRhsToExpr"), Phantoms.var("rhs")),
        ])),
    )
    return _def(
        "primaryWithRhsToExpr",
        Phantoms.doc(
            "Serialize a primary with RHS",
            Phantoms.lambdas(["pwr"], body),
        ),
    )


def _raise_expression_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("expr"), _proj("RaiseExpression", "expression", "re")),
            Phantoms.field(Name("from_"), _proj("RaiseExpression", "from", "re")),
        ],
        _ap(serialization_space_sep, Maybes.cat(Phantoms.list_([
            Phantoms.just(_ap(_local("expressionToExpr"), Phantoms.var("expr"))),
            Maybes.map(
                Phantoms.lam("f", _space_sep(Phantoms.list_([
                    _cst("from"),
                    _ap(_local("expressionToExpr"), Phantoms.var("f")),
                ]))),
                Phantoms.var("from_"),
            ),
        ]))),
    )
    return _def(
        "raiseExpressionToExpr",
        Phantoms.doc(
            "Serialize a raise expression",
            Phantoms.lambdas(["re"], body),
        ),
    )


def _raise_statement_to_expr():
    body = _ap(serialization_space_sep, Maybes.cat(Phantoms.list_([
        Phantoms.just(_cst("raise")),
        Maybes.map(_local("raiseExpressionToExpr"), _unwrap("RaiseStatement", "rs")),
    ])))
    return _def(
        "raiseStatementToExpr",
        Phantoms.doc(
            "Serialize a raise statement",
            Phantoms.lambdas(["rs"], body),
        ),
    )


def _relative_import_prefix_to_expr():
    body = Phantoms.cases(
        _ty("RelativeImportPrefix"), Phantoms.var("p"), Nothing(),
        [
            Phantoms.field(Name("dot"), Phantoms.constant(_cst("."))),
            Phantoms.field(Name("ellipsis"), Phantoms.constant(_cst("..."))),
        ],
    )
    return _def(
        "relativeImportPrefixToExpr",
        Phantoms.doc(
            "Serialize a relative import prefix",
            Phantoms.lambdas(["p"], body),
        ),
    )


def _return_statement_to_expr():
    body = _space_sep(Phantoms.list_([
        _cst("return"),
        _comma_sep_inline(Lists.map(_local("starExpressionToExpr"), _unwrap("ReturnStatement", "rs"))),
    ]))
    return _def(
        "returnStatementToExpr",
        Phantoms.doc(
            "Serialize a return statement",
            Phantoms.lambdas(["rs"], body),
        ),
    )


def _set_to_expr():
    body = _ap(
        serialization_braces_list_adaptive,
        Lists.map(_local("starNamedExpressionToExpr"), _unwrap("Set", "s")),
    )
    return _def(
        "setToExpr",
        Phantoms.doc(
            "Serialize a Python set",
            Phantoms.lambdas(["s"], body),
        ),
    )


def _shift_expression_to_expr():
    body = _ap(_local("sumToExpr"), _proj("ShiftExpression", "rhs", "se"))
    return _def(
        "shiftExpressionToExpr",
        Phantoms.doc(
            "Serialize a shift expression",
            Phantoms.lambdas(["se"], body),
        ),
    )


def _simple_statement_to_expr():
    body = Phantoms.cases(
        _ty("SimpleStatement"), Phantoms.var("ss"), Nothing(),
        [
            Phantoms.field(Name("assignment"), Phantoms.lam("a", _ap(_local("assignmentToExpr"), Phantoms.var("a")))),
            Phantoms.field(
                Name("starExpressions"),
                Phantoms.lam("es", _newline_sep(Lists.map(_local("starExpressionToExpr"), Phantoms.var("es")))),
            ),
            Phantoms.field(Name("return"), Phantoms.lam("r", _ap(_local("returnStatementToExpr"), Phantoms.var("r")))),
            Phantoms.field(Name("raise"), Phantoms.lam("r", _ap(_local("raiseStatementToExpr"), Phantoms.var("r")))),
            Phantoms.field(Name("pass"), Phantoms.constant(_cst("pass"))),
            Phantoms.field(Name("break"), Phantoms.constant(_cst("break"))),
            Phantoms.field(Name("continue"), Phantoms.constant(_cst("continue"))),
            Phantoms.field(Name("import"), Phantoms.lam("i", _ap(_local("importStatementToExpr"), Phantoms.var("i")))),
            Phantoms.field(Name("typeAlias"), Phantoms.lam("t", _ap(_local("typeAliasToExpr"), Phantoms.var("t")))),
            Phantoms.field(Name("assert"), Phantoms.lam("_", _cst("assert ..."))),
            Phantoms.field(Name("global"), Phantoms.lam("_", _cst("global ..."))),
            Phantoms.field(Name("nonlocal"), Phantoms.lam("_", _cst("nonlocal ..."))),
            Phantoms.field(Name("del"), Phantoms.lam("_", _cst("del ..."))),
        ],
    )
    return _def(
        "simpleStatementToExpr",
        Phantoms.doc(
            "Serialize a simple (single-line) Python statement",
            Phantoms.lambdas(["ss"], body),
        ),
    )


def _simple_type_parameter_to_expr():
    body = _ap(_local("nameToExpr"), _proj("SimpleTypeParameter", "name", "stp"))
    return _def(
        "simpleTypeParameterToExpr",
        Phantoms.doc(
            "Serialize a simple type parameter",
            Phantoms.lambdas(["stp"], body),
        ),
    )


def _single_target_to_expr():
    body = Phantoms.cases(
        _ty("SingleTarget"), Phantoms.var("st"), Nothing(),
        [
            Phantoms.field(Name("name"), Phantoms.lam("n", _ap(_local("nameToExpr"), Phantoms.var("n")))),
            Phantoms.field(Name("parens"), Phantoms.lam("_", _cst("(...)"))),
            Phantoms.field(Name("subscriptAttributeTarget"), Phantoms.lam("_", _cst("..."))),
        ],
    )
    return _def(
        "singleTargetToExpr",
        Phantoms.doc(
            "Serialize a single target",
            Phantoms.lambdas(["st"], body),
        ),
    )


def _slice_to_expr():
    body = Phantoms.cases(
        _ty("Slice"), Phantoms.var("s"), Nothing(),
        [
            Phantoms.field(Name("named"), Phantoms.lam("ne", _ap(_local("namedExpressionToExpr"), Phantoms.var("ne")))),
            Phantoms.field(Name("slice_"), Phantoms.lam("_", _cst(":"))),
        ],
    )
    return _def(
        "sliceToExpr",
        Phantoms.doc(
            "Serialize a slice",
            Phantoms.lambdas(["s"], body),
        ),
    )


def _slice_or_starred_expression_to_expr():
    body = Phantoms.cases(
        _ty("SliceOrStarredExpression"), Phantoms.var("s"), Nothing(),
        [
            Phantoms.field(Name("slice"), Phantoms.lam("sl", _ap(_local("sliceToExpr"), Phantoms.var("sl")))),
            Phantoms.field(Name("starred"), Phantoms.lam("se", _ap(_local("starredExpressionToExpr"), Phantoms.var("se")))),
        ],
    )
    return _def(
        "sliceOrStarredExpressionToExpr",
        Phantoms.doc(
            "Serialize a slice or starred expression",
            Phantoms.lambdas(["s"], body),
        ),
    )


def _slices_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("hd"), _proj("Slices", "head", "s")),
            Phantoms.field(Name("tl"), _proj("Slices", "tail", "s")),
        ],
        _comma_sep_inline(Lists.cons(
            _ap(_local("sliceToExpr"), Phantoms.var("hd")),
            Lists.map(_local("sliceOrStarredExpressionToExpr"), Phantoms.var("tl")),
        )),
    )
    return _def(
        "slicesToExpr",
        Phantoms.doc(
            "Serialize slices",
            Phantoms.lambdas(["s"], body),
        ),
    )


def _star_atom_to_expr():
    body = Phantoms.cases(
        _ty("StarAtom"), Phantoms.var("sa"), Nothing(),
        [
            Phantoms.field(Name("name"), Phantoms.lam("n", _ap(_local("nameToExpr"), Phantoms.var("n")))),
            Phantoms.field(Name("targetWithStarAtom"), Phantoms.lam("_", _cst("(...)"))),
            Phantoms.field(Name("starTargetsTupleSeq"), Phantoms.lam("_", _cst("(...)"))),
            Phantoms.field(Name("starTargetsListSeq"), Phantoms.lam("_", _cst("[...]"))),
        ],
    )
    return _def(
        "starAtomToExpr",
        Phantoms.doc(
            "Serialize a star atom",
            Phantoms.lambdas(["sa"], body),
        ),
    )


def _star_expression_to_expr():
    body = Phantoms.cases(
        _ty("StarExpression"), Phantoms.var("se"), Nothing(),
        [
            Phantoms.field(
                Name("star"),
                Phantoms.lam("bor", _no_sep(Phantoms.list_([
                    _cst("*"),
                    _ap(_local("bitwiseOrToExpr"), Phantoms.var("bor")),
                ]))),
            ),
            Phantoms.field(Name("simple"), Phantoms.lam("e", _ap(_local("expressionToExpr"), Phantoms.var("e")))),
        ],
    )
    return _def(
        "starExpressionToExpr",
        Phantoms.doc(
            "Serialize a star expression",
            Phantoms.lambdas(["se"], body),
        ),
    )


def _star_named_expression_to_expr():
    body = Phantoms.cases(
        _ty("StarNamedExpression"), Phantoms.var("sne"), Nothing(),
        [
            Phantoms.field(
                Name("star"),
                Phantoms.lam("bor", _no_sep(Phantoms.list_([
                    _cst("*"),
                    _ap(_local("bitwiseOrToExpr"), Phantoms.var("bor")),
                ]))),
            ),
            Phantoms.field(Name("simple"), Phantoms.lam("ne", _ap(_local("namedExpressionToExpr"), Phantoms.var("ne")))),
        ],
    )
    return _def(
        "starNamedExpressionToExpr",
        Phantoms.doc(
            "Serialize a star named expression",
            Phantoms.lambdas(["sne"], body),
        ),
    )


def _star_target_to_expr():
    body = Phantoms.cases(
        _ty("StarTarget"), Phantoms.var("st"), Nothing(),
        [
            Phantoms.field(Name("unstarred"), Phantoms.lam("t", _ap(_local("targetWithStarAtomToExpr"), Phantoms.var("t")))),
            Phantoms.field(
                Name("starred"),
                Phantoms.lam("inner", _no_sep(Phantoms.list_([
                    _cst("*"),
                    _ap(_local("starTargetToExpr"), Phantoms.var("inner")),
                ]))),
            ),
        ],
    )
    return _def(
        "starTargetToExpr",
        Phantoms.doc(
            "Serialize a star target",
            Phantoms.lambdas(["st"], body),
        ),
    )


def _starred_expression_to_expr():
    body = _no_sep(Phantoms.list_([
        _cst("*"),
        _ap(_local("expressionToExpr"), _unwrap("StarredExpression", "se")),
    ]))
    return _def(
        "starredExpressionToExpr",
        Phantoms.doc(
            "Serialize a starred expression",
            Phantoms.lambdas(["se"], body),
        ),
    )


def _statement_to_expr():
    body = Phantoms.cases(
        _ty("Statement"), Phantoms.var("stmt"), Nothing(),
        [
            Phantoms.field(Name("annotated"), Phantoms.lam("a", _ap(_local("annotatedStatementToExpr"), Phantoms.var("a")))),
            Phantoms.field(
                Name("simple"),
                Phantoms.lam("ss", _newline_sep(Lists.map(_local("simpleStatementToExpr"), Phantoms.var("ss")))),
            ),
            Phantoms.field(Name("compound"), Phantoms.lam("c", _ap(_local("compoundStatementToExpr"), Phantoms.var("c")))),
        ],
    )
    return _def(
        "statementToExpr",
        Phantoms.doc(
            "Serialize a Python statement",
            Phantoms.lambdas(["stmt"], body),
        ),
    )


def _string_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("content"), _proj("String", "value", "s")),
            Phantoms.field(Name("style"), _proj("String", "quoteStyle", "s")),
        ],
        Phantoms.cases(
            _ty("QuoteStyle"), Phantoms.var("style"), Nothing(),
            [
                Phantoms.field(
                    Name("single"),
                    Phantoms.constant(_ap(serialization_cst,
                        _ap(_local("escapePythonString"), Phantoms.false(), Phantoms.var("content")))),
                ),
                Phantoms.field(
                    Name("double"),
                    Phantoms.constant(_ap(serialization_cst,
                        _ap(_local("escapePythonString"), Phantoms.true(), Phantoms.var("content")))),
                ),
                Phantoms.field(
                    Name("triple"),
                    Phantoms.constant(_no_sep(Phantoms.list_([
                        _cst('r"""'),
                        _ap(serialization_cst, Phantoms.var("content")),
                        _cst('"""'),
                    ]))),
                ),
            ],
        ),
    )
    return _def(
        "stringToExpr",
        Phantoms.doc(
            "Serialize a Python string literal",
            Phantoms.lambdas(["s"], body),
        ),
    )


def _subject_expression_to_expr():
    body = Phantoms.cases(
        _ty("SubjectExpression"), Phantoms.var("se"), Nothing(),
        [
            Phantoms.field(Name("simple"), Phantoms.lam("ne", _ap(_local("namedExpressionToExpr"), Phantoms.var("ne")))),
            Phantoms.field(Name("tuple"), Phantoms.lam("_", _cst("*..."))),
        ],
    )
    return _def(
        "subjectExpressionToExpr",
        Phantoms.doc(
            "Serialize a subject expression",
            Phantoms.lambdas(["se"], body),
        ),
    )


def _sum_to_expr():
    body = _ap(_local("termToExpr"), _proj("Sum", "rhs", "s"))
    return _def(
        "sumToExpr",
        Phantoms.doc(
            "Serialize a sum expression",
            Phantoms.lambdas(["s"], body),
        ),
    )


def _t_primary_to_expr():
    body = Phantoms.cases(
        _ty("TPrimary"), Phantoms.var("tp"), Nothing(),
        [
            Phantoms.field(Name("atom"), Phantoms.lam("a", _ap(_local("atomToExpr"), Phantoms.var("a")))),
            Phantoms.field(Name("primaryAndName"), Phantoms.lam("pn", _ap(_local("tPrimaryAndNameToExpr"), Phantoms.var("pn")))),
            Phantoms.field(Name("primaryAndSlices"), Phantoms.lam("_", _cst("..."))),
            Phantoms.field(Name("primaryAndGenexp"), Phantoms.lam("_", _cst("..."))),
            Phantoms.field(Name("primaryAndArguments"), Phantoms.lam("_", _cst("..."))),
        ],
    )
    return _def(
        "tPrimaryToExpr",
        Phantoms.doc(
            "Serialize a target-side primary expression",
            Phantoms.lambdas(["tp"], body),
        ),
    )


def _t_primary_and_name_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("prim"), _proj("TPrimaryAndName", "primary", "pn")),
            Phantoms.field(Name("name_"), _proj("TPrimaryAndName", "name", "pn")),
        ],
        _no_sep(Phantoms.list_([
            _ap(_local("tPrimaryToExpr"), Phantoms.var("prim")),
            _cst("."),
            _ap(_local("nameToExpr"), Phantoms.var("name_")),
        ])),
    )
    return _def(
        "tPrimaryAndNameToExpr",
        Phantoms.doc(
            "Serialize a TPrimaryAndName as primary.name",
            Phantoms.lambdas(["pn"], body),
        ),
    )


def _target_with_star_atom_to_expr():
    body = Phantoms.cases(
        _ty("TargetWithStarAtom"), Phantoms.var("t"), Nothing(),
        [
            Phantoms.field(Name("atom"), Phantoms.lam("a", _ap(_local("starAtomToExpr"), Phantoms.var("a")))),
            Phantoms.field(Name("project"), Phantoms.lam("pn", _ap(_local("tPrimaryAndNameToExpr"), Phantoms.var("pn")))),
            Phantoms.field(Name("slices"), Phantoms.lam("_", _cst("..."))),
        ],
    )
    return _def(
        "targetWithStarAtomToExpr",
        Phantoms.doc(
            "Serialize a target with star atom",
            Phantoms.lambdas(["t"], body),
        ),
    )


def _term_to_expr():
    body = _ap(_local("factorToExpr"), _proj("Term", "rhs", "t"))
    return _def(
        "termToExpr",
        Phantoms.doc(
            "Serialize a term expression",
            Phantoms.lambdas(["t"], body),
        ),
    )


def _tuple_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("es"), _unwrap("Tuple", "t")),
        ],
        Maybes.from_maybe(
            _ap(serialization_paren_list_adaptive,
                Lists.map(_local("starNamedExpressionToExpr"), Phantoms.var("es"))),
            Maybes.map(
                Phantoms.lam(
                    "firstEs",
                    Logic.if_else(
                        Equality.equal(Lists.length(Phantoms.var("es")), Phantoms.int32(1)),
                        _ap(serialization_parens, _no_sep(Phantoms.list_([
                            _ap(_local("starNamedExpressionToExpr"), Phantoms.var("firstEs")),
                            _cst(","),
                        ]))),
                        _ap(serialization_paren_list_adaptive,
                            Lists.map(_local("starNamedExpressionToExpr"), Phantoms.var("es"))),
                    ),
                ),
                Lists.maybe_head(Phantoms.var("es")),
            ),
        ),
    )
    return _def(
        "tupleToExpr",
        Phantoms.doc(
            "Serialize a Python tuple",
            Phantoms.lambdas(["t"], body),
        ),
    )


def _type_alias_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("name"), _proj("TypeAlias", "name", "ta")),
            Phantoms.field(Name("tparams"), _proj("TypeAlias", "typeParams", "ta")),
            Phantoms.field(Name("expr"), _proj("TypeAlias", "expression", "ta")),
            Phantoms.field(
                Name("alias"),
                _ap(serialization_no_sep, Maybes.cat(Phantoms.list_([
                    Phantoms.just(_ap(_local("nameToExpr"), Phantoms.var("name"))),
                    Logic.if_else(
                        Lists.null(Phantoms.var("tparams")),
                        Phantoms.nothing(),
                        Phantoms.just(_ap(serialization_bracket_list, serialization_inline_style,
                            Lists.map(_local("typeParameterToExpr"), Phantoms.var("tparams")))),
                    ),
                ]))),
            ),
        ],
        _space_sep(Phantoms.list_([
            _cst("type"),
            Phantoms.var("alias"),
            _cst("="),
            _ap(_local("expressionToExpr"), Phantoms.var("expr")),
        ])),
    )
    return _def(
        "typeAliasToExpr",
        Phantoms.doc(
            "Serialize a type alias",
            Phantoms.lambdas(["ta"], body),
        ),
    )


def _type_parameter_to_expr():
    body = Phantoms.cases(
        _ty("TypeParameter"), Phantoms.var("tp"), Nothing(),
        [
            Phantoms.field(Name("simple"), Phantoms.lam("s", _ap(_local("simpleTypeParameterToExpr"), Phantoms.var("s")))),
            Phantoms.field(Name("star"), Phantoms.lam("_", _cst("*..."))),
            Phantoms.field(Name("doubleStar"), Phantoms.lam("_", _cst("**..."))),
        ],
    )
    return _def(
        "typeParameterToExpr",
        Phantoms.doc(
            "Serialize a type parameter",
            Phantoms.lambdas(["tp"], body),
        ),
    )


def _typed_assignment_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("lhs"), _proj("TypedAssignment", "lhs", "ta")),
            Phantoms.field(Name("typ"), _proj("TypedAssignment", "type", "ta")),
            Phantoms.field(Name("rhs"), _proj("TypedAssignment", "rhs", "ta")),
        ],
        _ap(serialization_space_sep, Maybes.cat(Phantoms.list_([
            Phantoms.just(_no_sep(Phantoms.list_([
                _ap(_local("singleTargetToExpr"), Phantoms.var("lhs")),
                _cst(":"),
            ]))),
            Phantoms.just(_ap(_local("expressionToExpr"), Phantoms.var("typ"))),
            Maybes.map(_local("annotatedRhsToExpr"), Phantoms.var("rhs")),
        ]))),
    )
    return _def(
        "typedAssignmentToExpr",
        Phantoms.doc(
            "Serialize a typed assignment",
            Phantoms.lambdas(["ta"], body),
        ),
    )


def _untyped_assignment_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("targets"), _proj("UntypedAssignment", "targets", "ua")),
            Phantoms.field(Name("rhs"), _proj("UntypedAssignment", "rhs", "ua")),
        ],
        _space_sep(Lists.concat(Phantoms.list_([
            Lists.map(_local("starTargetToExpr"), Phantoms.var("targets")),
            Phantoms.list_([_ap(_local("annotatedRhsToExpr"), Phantoms.var("rhs"))]),
        ]))),
    )
    return _def(
        "untypedAssignmentToExpr",
        Phantoms.doc(
            "Serialize an untyped assignment",
            Phantoms.lambdas(["ua"], body),
        ),
    )


def _value_pattern_to_expr():
    body = _ap(_local("attributeToExpr"), _unwrap("ValuePattern", "vp"))
    return _def(
        "valuePatternToExpr",
        Phantoms.doc(
            "Serialize a value pattern",
            Phantoms.lambdas(["vp"], body),
        ),
    )


def _while_statement_to_expr():
    body = Phantoms.lets(
        [
            Phantoms.field(Name("cond"), _proj("WhileStatement", "condition", "ws")),
            Phantoms.field(Name("body"), _proj("WhileStatement", "body", "ws")),
            Phantoms.field(Name("else_"), _proj("WhileStatement", "else", "ws")),
        ],
        _ap(serialization_newline_sep, Maybes.cat(Phantoms.list_([
            Phantoms.just(_newline_sep(Phantoms.list_([
                _space_sep(Phantoms.list_([
                    _cst("while"),
                    _no_sep(Phantoms.list_([
                        _ap(_local("namedExpressionToExpr"), Phantoms.var("cond")),
                        _cst(":"),
                    ])),
                ])),
                _ap(_local("blockToExpr"), Phantoms.var("body")),
            ]))),
            Maybes.map(
                Phantoms.lam("eb", _newline_sep(Phantoms.list_([
                    _cst("else:"),
                    _ap(_local("blockToExpr"), Phantoms.var("eb")),
                ]))),
                Phantoms.var("else_"),
            ),
        ]))),
    )
    return _def(
        "whileStatementToExpr",
        Phantoms.doc(
            "Serialize a while statement",
            Phantoms.lambdas(["ws"], body),
        ),
    )


def _escape_python_string():
    body = Phantoms.lets(
        [
            Phantoms.field(
                Name("replace"),
                Phantoms.lambdas(
                    ["old", "new", "str"],
                    Strings.intercalate(Phantoms.var("new"),
                        Strings.split_on(Phantoms.var("old"), Phantoms.var("str"))),
                ),
            ),
            Phantoms.field(
                Name("s1"),
                _ap(Phantoms.var("replace"), Phantoms.string("\\"),
                    Phantoms.string("\\\\"), Phantoms.var("s")),
            ),
            Phantoms.field(
                Name("s2"),
                _ap(Phantoms.var("replace"), Phantoms.string("\0"),
                    Phantoms.string("\\x00"), Phantoms.var("s1")),
            ),
            Phantoms.field(
                Name("s3"),
                _ap(Phantoms.var("replace"), Phantoms.string("\n"),
                    Phantoms.string("\\n"), Phantoms.var("s2")),
            ),
            Phantoms.field(
                Name("s4"),
                _ap(Phantoms.var("replace"), Phantoms.string("\t"),
                    Phantoms.string("\\t"), Phantoms.var("s3")),
            ),
            Phantoms.field(
                Name("s5"),
                _ap(Phantoms.var("replace"), Phantoms.string("\r"),
                    Phantoms.string("\\r"), Phantoms.var("s4")),
            ),
            Phantoms.field(
                Name("escaped"),
                Logic.if_else(
                    Phantoms.var("doubleQuoted"),
                    _ap(Phantoms.var("replace"), Phantoms.string('"'),
                        Phantoms.string('\\"'), Phantoms.var("s5")),
                    _ap(Phantoms.var("replace"), Phantoms.string("'"),
                        Phantoms.string("\\'"), Phantoms.var("s5")),
                ),
            ),
            Phantoms.field(
                Name("quote"),
                Logic.if_else(
                    Phantoms.var("doubleQuoted"),
                    Phantoms.string('"'),
                    Phantoms.string("'"),
                ),
            ),
        ],
        Strings.cat2(Phantoms.var("quote"),
            Strings.cat2(Phantoms.var("escaped"), Phantoms.var("quote"))),
    )
    return _def(
        "escapePythonString",
        Phantoms.doc(
            "Escape special characters in a Python string and wrap in quotes",
            Phantoms.lambdas(["doubleQuoted", "s"], body),
        ),
    )


def _python_float_literal_text():
    body = Logic.if_else(
        Equality.equal(Phantoms.var("s"), Phantoms.string("NaN")),
        Phantoms.string("float('nan')"),
        Logic.if_else(
            Equality.equal(Phantoms.var("s"), Phantoms.string("Infinity")),
            Phantoms.string("float('inf')"),
            Logic.if_else(
                Equality.equal(Phantoms.var("s"), Phantoms.string("-Infinity")),
                Phantoms.string("float('-inf')"),
                Phantoms.var("s"),
            ),
        ),
    )
    return _def(
        "pythonFloatLiteralText",
        Phantoms.lambdas(["s"], body),
    )


def _to_python_comments():
    body = Logic.if_else(
        Equality.equal(Phantoms.var("doc_"), Phantoms.string("")),
        Phantoms.string(""),
        Strings.intercalate(
            Phantoms.string("\n"),
            Lists.map(
                Phantoms.lam(
                    "line",
                    Logic.if_else(
                        Equality.equal(Phantoms.var("line"), Phantoms.string("")),
                        Phantoms.string("#"),
                        Strings.cat2(Phantoms.string("# "), Phantoms.var("line")),
                    ),
                ),
                Strings.lines(Phantoms.var("doc_")),
            ),
        ),
    )
    return _def(
        "toPythonComments",
        Phantoms.doc(
            "Convert a doc string to Python comment format. Empty source lines"
            " emit `#` (no trailing space) so blank comment lines don't carry"
            " trailing whitespace into the generated file.",
            Phantoms.lambdas(["doc_"], body),
        ),
    )


def _build_module() -> Module:
    return Module(
        _PLACEHOLDER.description,
        _PLACEHOLDER.namespace,
        _PLACEHOLDER.dependencies,
        (
            Phantoms.to_definition(_annotated_rhs_to_expr()),
            Phantoms.to_definition(_annotated_statement_to_expr()),
            Phantoms.to_definition(_annotation_to_expr()),
            Phantoms.to_definition(_args_to_expr()),
            Phantoms.to_definition(_assignment_to_expr()),
            Phantoms.to_definition(_assignment_expression_to_expr()),
            Phantoms.to_definition(_atom_to_expr()),
            Phantoms.to_definition(_attribute_to_expr()),
            Phantoms.to_definition(_await_primary_to_expr()),
            Phantoms.to_definition(_bitwise_and_to_expr()),
            Phantoms.to_definition(_bitwise_or_to_expr()),
            Phantoms.to_definition(_bitwise_xor_to_expr()),
            Phantoms.to_definition(_block_to_expr()),
            Phantoms.to_definition(_capture_pattern_to_expr()),
            Phantoms.to_definition(_case_block_to_expr()),
            Phantoms.to_definition(_class_definition_to_expr()),
            Phantoms.to_definition(_class_pattern_to_expr()),
            Phantoms.to_definition(_closed_pattern_to_expr()),
            Phantoms.to_definition(_compare_op_bitwise_or_pair_to_expr()),
            Phantoms.to_definition(_compare_op_to_string()),
            Phantoms.to_definition(_comparison_to_expr()),
            Phantoms.to_definition(_compound_statement_to_expr()),
            Phantoms.to_definition(_conditional_to_expr()),
            Phantoms.to_definition(_conjunction_to_expr()),
            Phantoms.to_definition(_decorators_to_expr()),
            Phantoms.to_definition(_dict_to_expr()),
            Phantoms.to_definition(_disjunction_to_expr()),
            Phantoms.to_definition(_dotted_as_name_to_expr()),
            Phantoms.to_definition(_dotted_name_to_expr()),
            Phantoms.to_definition(_double_starred_kvpair_to_expr()),
            Phantoms.to_definition(_expression_to_expr()),
            Phantoms.to_definition(_factor_to_expr()),
            Phantoms.to_definition(_function_def_raw_to_expr()),
            Phantoms.to_definition(_function_definition_to_expr()),
            Phantoms.to_definition(_group_to_expr()),
            Phantoms.to_definition(_guard_to_expr()),
            Phantoms.to_definition(_import_from_to_expr()),
            Phantoms.to_definition(_import_from_as_name_to_expr()),
            Phantoms.to_definition(_import_from_targets_to_expr()),
            Phantoms.to_definition(_import_name_to_expr()),
            Phantoms.to_definition(_import_statement_to_expr()),
            Phantoms.to_definition(_inversion_to_expr()),
            Phantoms.to_definition(_keyword_pattern_to_expr()),
            Phantoms.to_definition(_keyword_patterns_to_expr()),
            Phantoms.to_definition(_kvpair_to_expr()),
            Phantoms.to_definition(_kwarg_to_expr()),
            Phantoms.to_definition(_kwarg_or_double_starred_to_expr()),
            Phantoms.to_definition(_kwarg_or_starred_to_expr()),
            Phantoms.to_definition(_lambda_to_expr()),
            Phantoms.to_definition(_lambda_param_no_default_to_expr()),
            Phantoms.to_definition(_lambda_parameters_to_expr()),
            Phantoms.to_definition(_lambda_star_etc_to_expr()),
            Phantoms.to_definition(_list_to_expr()),
            Phantoms.to_definition(_match_statement_to_expr()),
            Phantoms.to_definition(_module_to_expr()),
            Phantoms.to_definition(_name_to_expr()),
            Phantoms.to_definition(_name_or_attribute_to_expr()),
            Phantoms.to_definition(_named_expression_to_expr()),
            Phantoms.to_definition(_number_to_expr()),
            Phantoms.to_definition(_or_pattern_to_expr()),
            Phantoms.to_definition(_param_to_expr()),
            Phantoms.to_definition(_param_no_default_to_expr()),
            Phantoms.to_definition(_param_no_default_parameters_to_expr()),
            Phantoms.to_definition(_parameters_to_expr()),
            Phantoms.to_definition(_pattern_to_expr()),
            Phantoms.to_definition(_pattern_capture_target_to_expr()),
            Phantoms.to_definition(_patterns_to_expr()),
            Phantoms.to_definition(_pos_arg_to_expr()),
            Phantoms.to_definition(_positional_patterns_to_expr()),
            Phantoms.to_definition(_power_to_expr()),
            Phantoms.to_definition(_primary_to_expr()),
            Phantoms.to_definition(_primary_rhs_to_expr()),
            Phantoms.to_definition(_primary_with_rhs_to_expr()),
            Phantoms.to_definition(_raise_expression_to_expr()),
            Phantoms.to_definition(_raise_statement_to_expr()),
            Phantoms.to_definition(_relative_import_prefix_to_expr()),
            Phantoms.to_definition(_return_statement_to_expr()),
            Phantoms.to_definition(_set_to_expr()),
            Phantoms.to_definition(_shift_expression_to_expr()),
            Phantoms.to_definition(_simple_statement_to_expr()),
            Phantoms.to_definition(_simple_type_parameter_to_expr()),
            Phantoms.to_definition(_single_target_to_expr()),
            Phantoms.to_definition(_slice_to_expr()),
            Phantoms.to_definition(_slice_or_starred_expression_to_expr()),
            Phantoms.to_definition(_slices_to_expr()),
            Phantoms.to_definition(_star_atom_to_expr()),
            Phantoms.to_definition(_star_expression_to_expr()),
            Phantoms.to_definition(_star_named_expression_to_expr()),
            Phantoms.to_definition(_star_target_to_expr()),
            Phantoms.to_definition(_starred_expression_to_expr()),
            Phantoms.to_definition(_statement_to_expr()),
            Phantoms.to_definition(_string_to_expr()),
            Phantoms.to_definition(_subject_expression_to_expr()),
            Phantoms.to_definition(_sum_to_expr()),
            Phantoms.to_definition(_t_primary_to_expr()),
            Phantoms.to_definition(_t_primary_and_name_to_expr()),
            Phantoms.to_definition(_target_with_star_atom_to_expr()),
            Phantoms.to_definition(_term_to_expr()),
            Phantoms.to_definition(_tuple_to_expr()),
            Phantoms.to_definition(_type_alias_to_expr()),
            Phantoms.to_definition(_type_parameter_to_expr()),
            Phantoms.to_definition(_typed_assignment_to_expr()),
            Phantoms.to_definition(_untyped_assignment_to_expr()),
            Phantoms.to_definition(_value_pattern_to_expr()),
            Phantoms.to_definition(_while_statement_to_expr()),
            Phantoms.to_definition(_escape_python_string()),
            Phantoms.to_definition(_python_float_literal_text()),
            Phantoms.to_definition(_to_python_comments()),
        ),
    )


module_ = _build_module()
