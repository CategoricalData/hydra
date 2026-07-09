"""TypedTerm references to functions NOT yet covered by generated DSLs.

Kernel-function references have been replaced by the generated hydra.dsl.* modules
(#467): e.g. hydra.dsl.strip.deannotate_type, hydra.dsl.names.qualify_name.
What remains here are references to this package's OWN modules (hydra.python.*),
which do not yet project generated DSLs; they migrate when DSL generation widens
beyond the kernel (see #467). The last derived-family reference
(hydra.show.core.type) was retired in favor of the typed hydra.refs.show_ref +
hydra.dsl.core.type_type (see #555).

Keep this list minimal -- only add what is actually used and not generated.
"""

from hydra.overlay.python.dsl.meta.phantoms import var
from hydra.typed import TypedTerm
import hydra.dsl.core
import hydra.refs


# hydra.constants

# hydra.formatting

# hydra.names

# hydra.python.utils (used by coder.py)
utils_assignment_statement = var("hydra.python.utils.assignmentStatement")
utils_double_quoted_string = var("hydra.python.utils.doubleQuotedString")
utils_find_namespaces = var("hydra.python.utils.findNamespaces")
utils_function_call = var("hydra.python.utils.functionCall")
utils_indented_block = var("hydra.python.utils.indentedBlock")
utils_primary_with_expression_slices = var("hydra.python.utils.primaryWithExpressionSlices")
utils_primary_with_rhs = var("hydra.python.utils.primaryWithRhs")
utils_project_from_expression = var("hydra.python.utils.projectFromExpression")
utils_py_atom_to_py_expression = var("hydra.python.utils.pyAtomToPyExpression")
utils_py_closed_pattern_to_py_patterns = var("hydra.python.utils.pyClosedPatternToPyPatterns")
utils_py_expression_to_py_primary = var("hydra.python.utils.pyExpressionToPyPrimary")
utils_py_expression_to_py_star_named_expression = var("hydra.python.utils.pyExpressionToPyStarNamedExpression")
utils_py_expressions_to_py_args = var("hydra.python.utils.pyExpressionsToPyArgs")
utils_py_name_to_py_expression = var("hydra.python.utils.pyNameToPyExpression")
utils_py_name_to_py_type_parameter = var("hydra.python.utils.pyNameToPyTypeParameter")
utils_py_primary_to_py_expression = var("hydra.python.utils.pyPrimaryToPyExpression")
utils_py_simple_statement_to_py_statement = var("hydra.python.utils.pySimpleStatementToPyStatement")
utils_string_to_py_expression = var("hydra.python.utils.stringToPyExpression")
utils_py_list = var("hydra.python.utils.pyList")
utils_py_name_to_py_primary = var("hydra.python.utils.pyNameToPyPrimary")
utils_single_quoted_string = var("hydra.python.utils.singleQuotedString")
utils_primary_and_params = var("hydra.python.utils.primaryAndParams")
utils_primary_with_slices = var("hydra.python.utils.primaryWithSlices")
utils_py_expression_to_py_slice = var("hydra.python.utils.pyExpressionToPySlice")
utils_py_primary_to_py_slice = var("hydra.python.utils.pyPrimaryToPySlice")
utils_target_python_version = var("hydra.python.utils.targetPythonVersion")
utils_annotated_expression = var("hydra.python.utils.annotatedExpression")
utils_py_assignment_to_py_statement = var("hydra.python.utils.pyAssignmentToPyStatement")
utils_py_class_definition_to_py_statement = var("hydra.python.utils.pyClassDefinitionToPyStatement")
utils_py_expression_to_py_statement = var("hydra.python.utils.pyExpressionToPyStatement")
utils_triple_quoted_string = var("hydra.python.utils.tripleQuotedString")
utils_unit_variant_methods = var("hydra.python.utils.unitVariantMethods")
utils_annotated_statement = var("hydra.python.utils.annotatedStatement")
utils_dotted_assignment_statement = var("hydra.python.utils.dottedAssignmentStatement")
utils_comment_statement = var("hydra.python.utils.commentStatement")
utils_or_expression = var("hydra.python.utils.orExpression")
utils_py_expression_to_bitwise_or = var("hydra.python.utils.pyExpressionToBitwiseOr")
utils_py_expression_to_disjunction = var("hydra.python.utils.pyExpressionToDisjunction")
utils_raise_assertion_error = var("hydra.python.utils.raiseAssertionError")
utils_raise_type_error = var("hydra.python.utils.raiseTypeError")
utils_return_single = var("hydra.python.utils.returnSingle")
utils_name_and_params = var("hydra.python.utils.nameAndParams")
utils_py_none = var("hydra.python.utils.pyNone")
show_core_type = TypedTerm(hydra.refs.show_ref(hydra.dsl.core.type_type))
names_type_variable_reference = var("hydra.python.names.typeVariableReference")
names_term_variable_reference = var("hydra.python.names.termVariableReference")
names_encode_name_qualified = var("hydra.python.names.encodeNameQualified")

# hydra.annotations
formatting_normalize_comment = var("hydra.formatting.normalizeComment")

utils_type_alias_statement = var("hydra.python.utils.typeAliasStatement")
utils_type_alias_statement310 = var("hydra.python.utils.typeAliasStatement310")
utils_union_type_class_statements310 = var("hydra.python.utils.unionTypeClassStatements310")

# hydra.python.names (used by coder.py)
names_encode_enum_value = var("hydra.python.names.encodeEnumValue")
names_encode_field_name = var("hydra.python.names.encodeFieldName")
names_encode_name = var("hydra.python.names.encodeName")
names_encode_namespace = var("hydra.python.names.encodeNamespace")
names_encode_namespace_with_overrides = var("hydra.python.names.encodeNamespaceWithOverrides")
names_encode_type_variable = var("hydra.python.names.encodeTypeVariable")
names_use_future_annotations = var("hydra.python.names.useFutureAnnotations")
names_variant_name = var("hydra.python.names.variantName")
names_encode_constant_for_type_name = var("hydra.python.names.encodeConstantForTypeName")
names_encode_constant_for_field_name = var("hydra.python.names.encodeConstantForFieldName")

# hydra.predicates (used by coder.py)

# hydra.analysis

# hydra.environment

# hydra.scoping / hydra.strip / hydra.variables

# hydra.serialization (used by serde.py)

# hydra.packaging — these are DSL accessors (generated), not kernel functions.
# Re-export from hydra.dsl.packaging for symmetry with the Haskell DSL imports.
from hydra.dsl.packaging import (
    un_module_name as packaging_un_module_name,
)

# hydra.util — QualifiedName and namespaces_focus moved from hydra.packaging to
# hydra.util in #369 (QualifiedName and Namespaces<n> live in hydra.util).
from hydra.dsl.util import (
    module_names_focus as packaging_namespaces_focus,
    qualified_name_local as packaging_qualified_name_local,
    qualified_name_module_name as packaging_qualified_name_namespace,
)


# Partial-application sites: these functions are referenced with FEWER args than
# their full arity (the rest arrive later in the term), so the arity-saturated
# generated wrappers in hydra.dsl.* cannot express them. Keep as raw refs.
environment_with_lambda_context = var("hydra.environment.withLambdaContext")
environment_with_let_context = var("hydra.environment.withLetContext")
environment_with_type_lambda_context = var("hydra.environment.withTypeLambdaContext")
formatting_convert_case = var("hydra.formatting.convertCase")
formatting_sanitize_with_underscores = var("hydra.formatting.sanitizeWithUnderscores")

# hydra.util CaseConvention enum constants are TTerms (injects), already
# available as constants in hydra.dsl.util. Re-export here for symmetry.
from hydra.dsl.util import (
    case_convention_camel as util_case_convention_camel,
    case_convention_lower_snake as util_case_convention_lower_snake,
    case_convention_upper_snake as util_case_convention_upper_snake,
    case_convention_pascal as util_case_convention_pascal,
)
