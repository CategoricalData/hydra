"""A Python syntax model, based on the Python v3 PEG grammar.

Mirror of packages/hydra-python/src/main/haskell/Hydra/Sources/Python/Syntax.hs.
Retrieved from https://docs.python.org/3/reference/grammar.html on 2024-12-22.
"""

from hydra.core import Name, Type, TypeScheme
from hydra.dsl.python import Just, Nothing
from hydra.packaging import (
    DefinitionType,
    Module,
    Namespace,
    TypeDefinition,
)

import hydra.dsl.annotations as Annotations
import hydra.dsl.types as T


NS = Namespace("hydra.python.syntax")
DEPENDENCIES = [Namespace("hydra.core")]
DESCRIPTION = (
    "A Python syntax model, tracking the Python 3.14 PEG grammar:\n"
    "  https://docs.python.org/3.14/reference/grammar.html"
)


def _py(local: str) -> Type:
    """Reference to another type in this namespace: hydra.python.syntax.<local>."""
    return T.variable(f"hydra.python.syntax.{local}")


def _ne(et: Type) -> Type:
    """nonemptyList helper.

    Mirrors a bug in Haskell's Hydra.Dsl.Annotations.bounded:
    `annotMin t = Y.maybe t (`setMinLength` t) max` should reference `min`
    but uses `max` instead. Result: nonemptyList silently drops the
    minLength annotation. We replicate this for byte-equivalence.
    """
    return T.list_(et)


def _def(local_name: str, typ: Type) -> DefinitionType:
    name = Name(f"{NS.value}.{local_name}")
    ts = TypeScheme((), typ, Nothing())
    return DefinitionType(TypeDefinition(name, ts))


def _record(fields):
    """fields: list of (name_str, Type)."""
    return T.record([T.field(n, t) for (n, t) in fields])


def _union(fields):
    return T.union([T.field(n, t) for (n, t) in fields])


# ----------------------------------------------------------------------
# Type definitions
# Order mirrors Haskell: constructs ++ terminals ++ nonterminals
# ----------------------------------------------------------------------

# constructs (added for Hydra-Python; not from grammar)
_constructs = [
    _def("AnnotatedStatement", _record([
        ("comment", T.string()),
        ("statement", _py("Statement")),
    ])),
    _def("Module", T.wrap(T.list_(_ne(_py("Statement"))))),
    _def("QuoteStyle", T.enum(["single", "double", "tripleSingle", "tripleDouble"])),
    _def("StringPrefix", T.enum(["raw", "bytes", "rawBytes", "unicode"])),
]

# terminals from PEG grammar
_terminals = [
    _def("Name", T.wrap(T.string())),
    _def("Number", _union([
        ("integer", T.bigint()),
        ("float", T.float64()),
        ("imaginary", T.float64()),
    ])),
    _def("String", _record([
        ("value", T.string()),
        ("prefix", T.maybe(_py("StringPrefix"))),
        ("quoteStyle", _py("QuoteStyle")),
    ])),
    _def("TypeComment", T.wrap(T.string())),
]

# nonterminal productions
_nonterminals = [
    _def("File", T.wrap(T.list_(_py("Statement")))),
    _def("Interactive", T.wrap(_py("Statement"))),
    _def("Eval", T.wrap(_ne(_py("Expression")))),
    _def("Statement", _union([
        ("compound", _py("CompoundStatement")),
        ("simple", _ne(_py("SimpleStatement"))),
        ("annotated", _py("AnnotatedStatement")),
    ])),
    _def("SimpleStatement", _union([
        ("assignment", _py("Assignment")),
        ("typeAlias", _py("TypeAlias")),
        ("starExpressions", _ne(_py("StarExpression"))),
        ("return", _py("ReturnStatement")),
        ("import", _py("ImportStatement")),
        ("raise", _py("RaiseStatement")),
        ("pass", T.unit()),
        ("del", _py("DelStatement")),
        ("yield", _py("YieldStatement")),
        ("assert", _py("AssertStatement")),
        ("break", T.unit()),
        ("continue", T.unit()),
        ("global", _ne(_py("Name"))),
        ("nonlocal", _ne(_py("Name"))),
    ])),
    _def("CompoundStatement", _union([
        ("function", _py("FunctionDefinition")),
        ("if", _py("IfStatement")),
        ("classDef", _py("ClassDefinition")),
        ("with", _py("WithStatement")),
        ("for", _py("ForStatement")),
        ("try", _py("TryStatement")),
        ("while", _py("WhileStatement")),
        ("match", _py("MatchStatement")),
    ])),
    _def("Assignment", _union([
        ("typed", _py("TypedAssignment")),
        ("untyped", _py("UntypedAssignment")),
        ("aug", _py("AugAssignment")),
    ])),
    _def("TypedAssignment", _record([
        ("lhs", _py("SingleTarget")),
        ("type", _py("Expression")),
        ("rhs", T.maybe(_py("AnnotatedRhs"))),
    ])),
    _def("UntypedAssignment", _record([
        ("targets", _ne(_py("StarTarget"))),
        ("rhs", _py("AnnotatedRhs")),
        ("typeComment", T.maybe(_py("TypeComment"))),
    ])),
    _def("AugAssignment", _record([
        ("lhs", _py("SingleTarget")),
        ("augassign", _py("AugAssign")),
        ("rhs", _py("AnnotatedRhs")),
    ])),
    _def("AnnotatedRhs", _union([
        ("yield", _py("YieldExpression")),
        ("star", _ne(_py("StarExpression"))),
    ])),
    _def("AugAssign", T.enum([
        "plusEqual", "minusEqual", "timesEqual", "atEqual", "slashEqual",
        "percentEqual", "ampersandEqual", "barEqual", "caretEqual",
        "leftShiftEqual", "rightShiftEqual", "starStarEqual", "doubleSlashEqual",
    ])),
    _def("ReturnStatement", T.wrap(T.list_(_py("StarExpression")))),
    _def("RaiseStatement", T.wrap(T.maybe(_py("RaiseExpression")))),
    _def("RaiseExpression", _record([
        ("expression", _py("Expression")),
        ("from", T.maybe(_py("Expression"))),
    ])),
    _def("DelStatement", T.wrap(_py("DelTargets"))),
    _def("YieldStatement", T.wrap(_py("YieldExpression"))),
    _def("AssertStatement", _record([
        ("expression1", _py("Expression")),
        ("expression2", T.maybe(_py("Expression"))),
    ])),
    _def("ImportStatement", _union([
        ("name", _py("ImportName")),
        ("from", _py("ImportFrom")),
    ])),
    _def("ImportName", T.wrap(_ne(_py("DottedAsName")))),
    _def("ImportFrom", _record([
        ("prefixes", T.list_(_py("RelativeImportPrefix"))),
        ("dottedName", T.maybe(_py("DottedName"))),
        ("targets", _py("ImportFromTargets")),
    ])),
    _def("RelativeImportPrefix", T.enum(["dot", "ellipsis"])),
    _def("ImportFromTargets", _union([
        ("simple", _ne(_py("ImportFromAsName"))),
        ("parens", _ne(_py("ImportFromAsName"))),
        ("star", T.unit()),
    ])),
    _def("ImportFromAsName", _record([
        ("name", _py("Name")),
        ("as", T.maybe(_py("Name"))),
    ])),
    _def("DottedAsName", _record([
        ("name", _py("DottedName")),
        ("as", T.maybe(_py("Name"))),
    ])),
    _def("DottedName", T.wrap(_ne(_py("Name")))),
    _def("Block", _union([
        ("indented", _ne(_ne(_py("Statement")))),
        ("simple", _ne(_py("SimpleStatement"))),
    ])),
    _def("Decorators", T.wrap(_ne(_py("NamedExpression")))),
    _def("ClassDefinition", _record([
        ("decorators", T.maybe(_py("Decorators"))),
        ("name", _py("Name")),
        ("typeParams", T.list_(_py("TypeParameter"))),
        ("arguments", T.maybe(_py("Args"))),
        ("body", _py("Block")),
    ])),
    _def("FunctionDefinition", _record([
        ("decorators", T.maybe(_py("Decorators"))),
        ("raw", _py("FunctionDefRaw")),
    ])),
    _def("FunctionDefRaw", _record([
        ("async", T.boolean()),
        ("name", _py("Name")),
        ("typeParams", T.list_(_py("TypeParameter"))),
        ("params", T.maybe(_py("Parameters"))),
        ("returnType", T.maybe(_py("Expression"))),
        ("funcTypeComment", T.maybe(_py("FuncTypeComment"))),
        ("block", _py("Block")),
    ])),
    _def("Parameters", _union([
        ("slashNoDefault", _py("SlashNoDefaultParameters")),
        ("slashWithDefault", _py("SlashWithDefaultParameters")),
        ("paramNoDefault", _py("ParamNoDefaultParameters")),
        ("paramWithDefault", _py("ParamWithDefaultParameters")),
        ("starEtc", _py("StarEtc")),
    ])),
    _def("SlashNoDefaultParameters", _record([
        ("slash", _py("SlashNoDefault")),
        ("paramNoDefault", T.list_(_py("ParamNoDefault"))),
        ("paramWithDefault", T.list_(_py("ParamWithDefault"))),
        ("starEtc", T.maybe(_py("StarEtc"))),
    ])),
    _def("SlashWithDefaultParameters", _record([
        ("paramNoDefault", T.list_(_py("ParamNoDefault"))),
        ("paramWithDefault", T.list_(_py("ParamWithDefault"))),
        ("starEtc", T.maybe(_py("StarEtc"))),
    ])),
    _def("ParamNoDefaultParameters", _record([
        ("paramNoDefault", _ne(_py("ParamNoDefault"))),
        ("paramWithDefault", T.list_(_py("ParamWithDefault"))),
        ("starEtc", T.maybe(_py("StarEtc"))),
    ])),
    _def("ParamWithDefaultParameters", _record([
        ("paramWithDefault", _ne(_py("ParamWithDefault"))),
        ("starEtc", T.maybe(_py("StarEtc"))),
    ])),
    _def("SlashNoDefault", T.wrap(_ne(_py("ParamNoDefault")))),
    _def("SlashWithDefault", _record([
        ("paramNoDefault", T.list_(_py("ParamNoDefault"))),
        ("paramWithDefault", _ne(_py("ParamWithDefault"))),
    ])),
    _def("StarEtc", _union([
        ("starNoDefault", _py("NoDefaultStarEtc")),
        ("starNoDefaultStarAnnotation", _py("NoDefaultStarAnnotationStarEtc")),
        ("starComma", _py("CommaStarEtc")),
        ("keywords", _py("Keywords")),
    ])),
    _def("NoDefaultStarEtc", _record([
        ("paramNoDefault", _py("ParamNoDefault")),
        ("paramMaybeDefault", T.list_(_py("ParamMaybeDefault"))),
        ("keywords", T.maybe(_py("Keywords"))),
    ])),
    _def("NoDefaultStarAnnotationStarEtc", _record([
        ("paramNoDefaultStarAnnotation", _py("ParamNoDefaultStarAnnotation")),
        ("paramMaybeDefault", T.list_(_py("ParamMaybeDefault"))),
        ("keywords", T.maybe(_py("Keywords"))),
    ])),
    _def("CommaStarEtc", _record([
        ("paramMaybeDefault", _ne(_py("ParamMaybeDefault"))),
        ("keywords", T.maybe(_py("Keywords"))),
    ])),
    _def("Keywords", T.wrap(_py("ParamNoDefault"))),
    _def("ParamNoDefault", _record([
        ("param", _py("Param")),
        ("typeComment", T.maybe(_py("TypeComment"))),
    ])),
    _def("ParamNoDefaultStarAnnotation", _record([
        ("paramStarAnnotation", _py("ParamStarAnnotation")),
        ("typeComment", T.maybe(_py("TypeComment"))),
    ])),
    _def("ParamWithDefault", _record([
        ("param", _py("Param")),
        ("default", _py("Default")),
        ("typeComment", T.maybe(_py("TypeComment"))),
    ])),
    _def("ParamMaybeDefault", _record([
        ("param", _py("Param")),
        ("default", T.maybe(_py("Default"))),
        ("typeComment", T.maybe(_py("TypeComment"))),
    ])),
    _def("Param", _record([
        ("name", _py("Name")),
        ("annotation", T.maybe(_py("Annotation"))),
    ])),
    _def("ParamStarAnnotation", _record([
        ("name", _py("Name")),
        ("annotation", _py("StarAnnotation")),
    ])),
    _def("Annotation", T.wrap(_py("Expression"))),
    _def("StarAnnotation", T.wrap(_py("StarExpression"))),
    _def("Default", T.wrap(_py("Expression"))),
    _def("IfStatement", _record([
        ("condition", _py("NamedExpression")),
        ("body", _py("Block")),
        ("continuation", T.maybe(_py("IfTail"))),
    ])),
    _def("IfTail", _union([
        ("elif", _py("IfStatement")),
        ("else", _py("Block")),
    ])),
    _def("WhileStatement", _record([
        ("condition", _py("NamedExpression")),
        ("body", _py("Block")),
        ("else", T.maybe(_py("Block"))),
    ])),
    _def("ForStatement", _record([
        ("async", T.boolean()),
        ("targets", _ne(_py("StarTarget"))),
        ("expressions", _ne(_py("StarExpression"))),
        ("typeComment", T.maybe(_py("TypeComment"))),
        ("body", _py("Block")),
        ("else", T.maybe(_py("Block"))),
    ])),
    _def("WithStatement", _record([
        ("async", T.boolean()),
        ("items", _ne(_py("WithItem"))),
        ("typeComment", T.maybe(_py("TypeComment"))),
        ("body", _py("Block")),
    ])),
    _def("WithItem", _record([
        ("expression", _py("Expression")),
        ("as", T.maybe(_py("StarTarget"))),
    ])),
    _def("TryStatement", _union([
        ("finally", _py("TryFinallyStatement")),
        ("except", _py("TryExceptStatement")),
        ("exceptStar", _py("TryExceptStarStatement")),
    ])),
    _def("TryFinallyStatement", _record([
        ("body", _py("Block")),
        ("finally", _py("Block")),
    ])),
    _def("TryExceptStatement", _record([
        ("body", _py("Block")),
        ("excepts", _ne(_py("ExceptBlock"))),
        ("else", T.maybe(_py("Block"))),
        ("finally", T.maybe(_py("Block"))),
    ])),
    _def("TryExceptStarStatement", _record([
        ("body", _py("Block")),
        ("excepts", _ne(_py("ExceptStarBlock"))),
        ("else", T.maybe(_py("Block"))),
        ("finally", T.maybe(_py("Block"))),
    ])),
    _def("ExceptBlock", _record([
        ("expression", T.maybe(_py("ExceptExpression"))),
        ("body", _py("Block")),
    ])),
    _def("ExceptExpression", _record([
        ("expression", _py("Expression")),
        ("as", T.maybe(_py("Name"))),
    ])),
    _def("ExceptStarBlock", _record([
        ("expression", _py("Expression")),
        ("as", T.maybe(_py("Name"))),
        ("body", _py("Block")),
    ])),
    _def("MatchStatement", _record([
        ("subject", _py("SubjectExpression")),
        ("cases", _ne(_py("CaseBlock"))),
    ])),
    _def("SubjectExpression", _union([
        ("tuple", _ne(_py("StarNamedExpression"))),
        ("simple", _py("NamedExpression")),
    ])),
    _def("CaseBlock", _record([
        ("patterns", _py("Patterns")),
        ("guard", T.maybe(_py("Guard"))),
        ("body", _py("Block")),
    ])),
    _def("Guard", T.wrap(_py("NamedExpression"))),
    _def("Patterns", _union([
        ("sequence", _py("OpenSequencePattern")),
        ("pattern", _py("Pattern")),
    ])),
    _def("Pattern", _union([
        ("as", _py("AsPattern")),
        ("or", _py("OrPattern")),
    ])),
    _def("AsPattern", _record([
        ("pattern", _py("OrPattern")),
        ("as", _py("PatternCaptureTarget")),
    ])),
    _def("OrPattern", T.wrap(_ne(_py("ClosedPattern")))),
    _def("ClosedPattern", _union([
        ("literal", _py("LiteralExpression")),
        ("capture", _py("CapturePattern")),
        ("wildcard", T.unit()),
        ("value", _py("ValuePattern")),
        ("group", _py("GroupPattern")),
        ("sequence", _py("SequencePattern")),
        ("mapping", _py("MappingPattern")),
        ("class", _py("ClassPattern")),
    ])),
    _def("LiteralExpression", _union([
        ("number", _py("SignedNumber")),
        ("complex", _py("ComplexNumber")),
        ("string", _py("String")),
        ("none", T.unit()),
        ("true", T.unit()),
        ("false", T.unit()),
    ])),
    _def("ComplexNumber", _record([
        ("real", _py("SignedRealNumber")),
        ("plusOrMinus", _py("PlusOrMinus")),
        ("imaginary", _py("ImaginaryNumber")),
    ])),
    _def("PlusOrMinus", T.enum(["plus", "minus"])),
    _def("SignedNumber", _union([
        ("sign", _py("PlusOrMinus")),
        ("number", _py("Number")),
    ])),
    _def("SignedRealNumber", _union([
        ("sign", _py("PlusOrMinus")),
        ("number", _py("RealNumber")),
    ])),
    _def("RealNumber", _union([
        ("integer", T.bigint()),
        ("float", T.float64()),
    ])),
    _def("ImaginaryNumber", T.wrap(T.float64())),
    _def("CapturePattern", T.wrap(_py("PatternCaptureTarget"))),
    _def("PatternCaptureTarget", T.wrap(_py("Name"))),
    _def("ValuePattern", T.wrap(_py("Attribute"))),
    _def("Attribute", T.wrap(_ne(_py("Name")))),
    _def("NameOrAttribute", T.wrap(_ne(_py("Name")))),
    _def("GroupPattern", T.wrap(_py("Pattern"))),
    _def("SequencePattern", _union([
        ("list", T.maybe(_py("MaybeSequencePattern"))),
        ("tuple", T.maybe(_py("OpenSequencePattern"))),
    ])),
    _def("OpenSequencePattern", _record([
        ("head", _py("MaybeStarPattern")),
        ("tail", T.maybe(_py("MaybeSequencePattern"))),
    ])),
    _def("MaybeSequencePattern", T.wrap(_ne(_py("MaybeStarPattern")))),
    _def("MaybeStarPattern", _union([
        ("star", _py("StarPattern")),
        ("pattern", _py("Pattern")),
    ])),
    _def("StarPattern", _union([
        ("capture", _py("PatternCaptureTarget")),
        ("wildcard", T.unit()),
    ])),
    _def("MappingPattern", _record([
        ("items", T.maybe(_py("ItemsPattern"))),
        ("doubleStar", T.maybe(_py("DoubleStarPattern"))),
    ])),
    _def("ItemsPattern", T.wrap(_ne(_py("KeyValuePattern")))),
    _def("KeyValuePattern", _record([
        ("key", _py("LiteralExpressionOrAttribute")),
        ("value", _py("Pattern")),
    ])),
    _def("LiteralExpressionOrAttribute", _union([
        ("literal", _py("LiteralExpression")),
        ("attribute", _py("Attribute")),
    ])),
    _def("DoubleStarPattern", T.wrap(_py("PatternCaptureTarget"))),
    _def("ClassPattern", _record([
        ("nameOrAttribute", _py("NameOrAttribute")),
        ("positionalPatterns", T.maybe(_py("PositionalPatterns"))),
        ("keywordPatterns", T.maybe(_py("KeywordPatterns"))),
    ])),
    _def("PositionalPatterns", T.wrap(_ne(_py("Pattern")))),
    _def("KeywordPatterns", T.wrap(_ne(_py("KeywordPattern")))),
    _def("KeywordPattern", _record([
        ("name", _py("Name")),
        ("pattern", _py("Pattern")),
    ])),
    _def("TypeAlias", _record([
        ("name", _py("Name")),
        ("typeParams", T.list_(_py("TypeParameter"))),
        ("expression", _py("Expression")),
    ])),
    _def("TypeParameter", _union([
        ("simple", _py("SimpleTypeParameter")),
        ("star", _py("StarTypeParameter")),
        ("doubleStar", _py("DoubleStarTypeParameter")),
    ])),
    _def("SimpleTypeParameter", _record([
        ("name", _py("Name")),
        ("bound", T.maybe(_py("Expression"))),
        ("default", T.maybe(_py("Expression"))),
    ])),
    _def("StarTypeParameter", _record([
        ("name", _py("Name")),
        ("default", T.maybe(_py("StarExpression"))),
    ])),
    _def("DoubleStarTypeParameter", _record([
        ("name", _py("Name")),
        ("default", T.maybe(_py("Expression"))),
    ])),
    _def("Expression", _union([
        ("conditional", _py("Conditional")),
        ("simple", _py("Disjunction")),
        ("lambda", _py("Lambda")),
    ])),
    _def("Conditional", _record([
        ("body", _py("Disjunction")),
        ("if", _py("Disjunction")),
        ("else", _py("Expression")),
    ])),
    _def("YieldExpression", _union([
        ("from", _py("Expression")),
        ("simple", T.list_(_py("StarExpression"))),
    ])),
    _def("StarExpression", _union([
        ("star", _py("BitwiseOr")),
        ("simple", _py("Expression")),
    ])),
    _def("StarNamedExpressions", T.wrap(_ne(_py("StarNamedExpression")))),
    _def("StarNamedExpression", _union([
        ("star", _py("BitwiseOr")),
        ("simple", _py("NamedExpression")),
    ])),
    _def("AssignmentExpression", _record([
        ("name", _py("Name")),
        ("expression", _py("Expression")),
    ])),
    _def("NamedExpression", _union([
        ("assignment", _py("AssignmentExpression")),
        ("simple", _py("Expression")),
    ])),
    _def("Disjunction", T.wrap(_ne(_py("Conjunction")))),
    _def("Conjunction", T.wrap(_ne(_py("Inversion")))),
    _def("Inversion", _union([
        ("not", _py("Inversion")),
        ("simple", _py("Comparison")),
    ])),
    _def("Comparison", _record([
        ("lhs", _py("BitwiseOr")),
        ("rhs", T.list_(_py("CompareOpBitwiseOrPair"))),
    ])),
    _def("CompareOpBitwiseOrPair", _record([
        ("operator", _py("CompareOp")),
        ("rhs", _py("BitwiseOr")),
    ])),
    _def("CompareOp", T.enum(["eq", "noteq", "lte", "lt", "gte", "gt", "notin", "in", "isnot", "is"])),
    _def("BitwiseOr", _record([
        ("lhs", T.maybe(_py("BitwiseOr"))),
        ("rhs", _py("BitwiseXor")),
    ])),
    _def("BitwiseXor", _record([
        ("lhs", T.maybe(_py("BitwiseXor"))),
        ("rhs", _py("BitwiseAnd")),
    ])),
    _def("BitwiseAnd", _record([
        ("lhs", T.maybe(_py("BitwiseAnd"))),
        ("rhs", _py("ShiftExpression")),
    ])),
    _def("ShiftExpression", _record([
        ("lhs", T.maybe(_py("ShiftLhs"))),
        ("rhs", _py("Sum")),
    ])),
    _def("ShiftLhs", _record([
        ("operand", _py("ShiftExpression")),
        ("operator", _py("ShiftOp")),
    ])),
    _def("ShiftOp", T.enum(["left", "right"])),
    _def("Sum", _record([
        ("lhs", T.maybe(_py("SumLhs"))),
        ("rhs", _py("Term")),
    ])),
    _def("SumLhs", _record([
        ("operand", _py("Sum")),
        ("operator", _py("SumOp")),
    ])),
    _def("SumOp", T.enum(["add", "sub"])),
    _def("Term", _record([
        ("lhs", T.maybe(_py("TermLhs"))),
        ("rhs", _py("Factor")),
    ])),
    _def("TermLhs", _record([
        ("operand", _py("Term")),
        ("operator", _py("TermOp")),
    ])),
    _def("TermOp", T.enum(["mul", "div", "floordiv", "mod", "matmul"])),
    _def("Factor", _union([
        ("positive", _py("Factor")),
        ("negative", _py("Factor")),
        ("complement", _py("Factor")),
        ("simple", _py("Power")),
    ])),
    _def("Power", _record([
        ("lhs", _py("AwaitPrimary")),
        ("rhs", T.maybe(_py("Factor"))),
    ])),
    _def("AwaitPrimary", _record([
        ("await", T.boolean()),
        ("primary", _py("Primary")),
    ])),
    _def("Primary", _union([
        ("simple", _py("Atom")),
        ("compound", _py("PrimaryWithRhs")),
    ])),
    _def("PrimaryWithRhs", _record([
        ("primary", _py("Primary")),
        ("rhs", _py("PrimaryRhs")),
    ])),
    _def("PrimaryRhs", _union([
        ("project", _py("Name")),
        ("genexp", _py("Genexp")),
        ("call", _py("Args")),
        ("slices", _py("Slices")),
    ])),
    _def("Slices", _record([
        ("head", _py("Slice")),
        ("tail", T.list_(_py("SliceOrStarredExpression"))),
    ])),
    _def("SliceOrStarredExpression", _union([
        ("slice", _py("Slice")),
        ("starred", _py("StarredExpression")),
    ])),
    _def("Slice", _union([
        ("named", _py("NamedExpression")),
        ("slice_", _py("SliceExpression")),
    ])),
    _def("SliceExpression", _record([
        ("start", T.maybe(_py("Expression"))),
        ("stop", T.maybe(_py("Expression"))),
        ("step", T.maybe(_py("Expression"))),
    ])),
    _def("Atom", _union([
        ("name", _py("Name")),
        ("true", T.unit()),
        ("false", T.unit()),
        ("none", T.unit()),
        ("string", _py("String")),
        ("number", _py("Number")),
        ("tuple", _py("Tuple")),
        ("group", _py("Group")),
        ("genexp", _py("Genexp")),
        ("list", _py("List")),
        ("listcomp", _py("Listcomp")),
        ("dict", _py("Dict")),
        ("set", _py("Set")),
        ("dictcomp", _py("Dictcomp")),
        ("setcomp", _py("Setcomp")),
        ("ellipsis", T.unit()),
    ])),
    _def("Group", _union([
        ("yield", _py("YieldExpression")),
        ("expression", _py("NamedExpression")),
    ])),
    _def("Lambda", _record([
        ("params", _py("LambdaParameters")),
        ("body", _py("Expression")),
    ])),
    _def("LambdaParameters", _record([
        ("slashNoDefault", T.maybe(_py("LambdaSlashNoDefault"))),
        ("paramNoDefault", T.list_(_py("LambdaParamNoDefault"))),
        ("paramWithDefault", T.list_(_py("LambdaParamWithDefault"))),
        ("starEtc", T.maybe(_py("LambdaStarEtc"))),
    ])),
    _def("LambdaSlashNoDefault", _record([
        ("parameters", T.list_(_py("LambdaParamNoDefault"))),
    ])),
    _def("LambdaSlashWithDefault", _record([
        ("paramNoDefault", T.list_(_py("LambdaParamNoDefault"))),
        ("paramWithDefault", _ne(_py("LambdaParamWithDefault"))),
    ])),
    _def("LambdaStarEtc", _union([
        ("star", T.boolean()),
        ("paramNoDefault", _py("LambdaParamNoDefault")),
        ("paramMaybeDefault", T.list_(_py("LambdaParamMaybeDefault"))),
        ("kwds", _py("LambdaKwds")),
    ])),
    _def("LambdaKwds", T.wrap(_py("LambdaParamNoDefault"))),
    _def("LambdaParamNoDefault", T.wrap(_py("Name"))),
    _def("LambdaParamWithDefault", _record([
        ("param", _py("Name")),
        ("default", T.maybe(_py("Default"))),
    ])),
    _def("LambdaParamMaybeDefault", _record([
        ("param", _py("Name")),
        ("default", T.maybe(_py("Default"))),
    ])),
    _def("List", T.wrap(T.list_(_py("StarNamedExpression")))),
    _def("Tuple", T.wrap(T.list_(_py("StarNamedExpression")))),
    _def("Set", T.wrap(_ne(_py("StarNamedExpression")))),
    _def("Dict", T.wrap(T.list_(_py("DoubleStarredKvpair")))),
    _def("DoubleStarredKvpair", _union([
        ("starred", _py("BitwiseOr")),
        ("pair", _py("Kvpair")),
    ])),
    _def("Kvpair", _record([
        ("key", _py("Expression")),
        ("value", _py("Expression")),
    ])),
    _def("ForIfClauses", T.wrap(_ne(_py("ForIfClause")))),
    _def("ForIfClause", _record([
        ("async", T.boolean()),
        ("targets", _ne(_py("StarTarget"))),
        ("in", _py("Disjunction")),
        ("ifs", T.list_(_py("Disjunction"))),
    ])),
    _def("Listcomp", _record([
        ("expression", _py("NamedExpression")),
        ("forIfClauses", _py("ForIfClauses")),
    ])),
    _def("Setcomp", _record([
        ("expression", _py("NamedExpression")),
        ("forIfClauses", _py("ForIfClauses")),
    ])),
    _def("Genexp", _record([
        ("head", _py("GenexpHead")),
        ("tail", _py("ForIfClauses")),
    ])),
    _def("GenexpHead", _union([
        ("assignment", _py("AssignmentExpression")),
        ("expression", _py("Expression")),
    ])),
    _def("Dictcomp", _record([
        ("kvpair", _py("Kvpair")),
        ("forIfClauses", _py("ForIfClauses")),
    ])),
    _def("Args", _record([
        ("positional", T.list_(_py("PosArg"))),
        ("kwargOrStarred", T.list_(_py("KwargOrStarred"))),
        ("kwargOrDoubleStarred", T.list_(_py("KwargOrDoubleStarred"))),
    ])),
    _def("PosArg", _union([
        ("starred", _py("StarredExpression")),
        ("assignment", _py("AssignmentExpression")),
        ("expression", _py("Expression")),
    ])),
    _def("StarredExpression", T.wrap(_py("Expression"))),
    _def("KwargOrStarred", _union([
        ("kwarg", _py("Kwarg")),
        ("starred", _py("StarredExpression")),
    ])),
    _def("Kwarg", _record([
        ("name", _py("Name")),
        ("value", _py("Expression")),
    ])),
    _def("KwargOrDoubleStarred", _union([
        ("kwarg", _py("Kwarg")),
        ("doubleStarred", _py("Expression")),
    ])),
    _def("StarTargetsListSeq", T.wrap(_ne(_py("StarTarget")))),
    _def("StarTargetsTupleSeq", T.wrap(_ne(_py("StarTarget")))),
    _def("StarTarget", _union([
        ("starred", _py("StarTarget")),
        ("unstarred", _py("TargetWithStarAtom")),
    ])),
    _def("TargetWithStarAtom", _union([
        ("project", _py("TPrimaryAndName")),
        ("slices", _py("TPrimaryAndSlices")),
        ("atom", _py("StarAtom")),
    ])),
    _def("TPrimaryAndName", _record([
        ("primary", _py("TPrimary")),
        ("name", _py("Name")),
    ])),
    _def("TPrimaryAndSlices", _record([
        ("primary", _py("TPrimary")),
        ("slices", _py("Slices")),
    ])),
    _def("StarAtom", _union([
        ("name", _py("Name")),
        ("targetWithStarAtom", _py("TargetWithStarAtom")),
        ("starTargetsTupleSeq", T.maybe(_py("StarTargetsTupleSeq"))),
        ("starTargetsListSeq", T.maybe(_py("StarTargetsListSeq"))),
    ])),
    _def("SingleTarget", _union([
        ("subscriptAttributeTarget", _py("SingleSubscriptAttributeTarget")),
        ("name", _py("Name")),
        ("parens", _py("SingleTarget")),
    ])),
    _def("SingleSubscriptAttributeTarget", _union([
        ("primaryAndName", _py("TPrimaryAndName")),
        ("primaryAndSlices", _py("TPrimaryAndSlices")),
    ])),
    _def("TPrimary", _union([
        ("primaryAndName", _py("TPrimaryAndName")),
        ("primaryAndSlices", _py("TPrimaryAndSlices")),
        ("primaryAndGenexp", _py("TPrimaryAndGenexp")),
        ("primaryAndArguments", _py("TPrimaryAndArguments")),
        ("atom", _py("Atom")),
    ])),
    _def("TPrimaryAndGenexp", _record([
        ("primary", _py("TPrimary")),
        ("genexp", _py("Genexp")),
    ])),
    _def("TPrimaryAndArguments", _record([
        ("primary", _py("TPrimary")),
        ("arguments", T.maybe(_py("Args"))),
    ])),
    _def("DelTargets", T.wrap(_ne(_py("DelTarget")))),
    _def("DelTarget", _union([
        ("primaryAndName", _py("TPrimaryAndName")),
        ("primaryAndSlices", _py("TPrimaryAndSlices")),
        ("delTAtom", _py("DelTAtom")),
    ])),
    _def("DelTAtom", _union([
        ("name", _py("Name")),
        ("target", _py("DelTarget")),
        ("targets", _py("DelTargets")),
    ])),
    _def("TypeExpression", _union([
        ("expression", _py("Expression")),
        ("starredExpression", _py("Expression")),
        ("doubleStarredExpression", _py("Expression")),
    ])),
    _def("FuncTypeComment", T.wrap(_py("TypeComment"))),
]


# ----------------------------------------------------------------------
# Module assembly
# ----------------------------------------------------------------------

module_ = Module(
    Just(DESCRIPTION),
    NS,
    DEPENDENCIES,
    tuple(_constructs + _terminals + _nonterminals),
)
