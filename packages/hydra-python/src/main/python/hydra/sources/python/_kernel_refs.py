"""TypedTerm references to functions NOT yet covered by generated DSLs.

Kernel-function references have been replaced by the generated hydra.dsl.* modules
(#467): e.g. hydra.dsl.strip.deannotate_type, hydra.dsl.names.qualify_name.
This package's OWN modules (hydra.python.utils, hydra.python.names) are also now
generated (#556): hydra.dsl.python.utils.*, hydra.dsl.python.names.*. What remains
here are derived-family refs (hydra.show.*) not yet covered, and a handful of
permanent partial-application exceptions (see bottom of file) that the
arity-saturated generated wrappers cannot express.

Keep this list minimal -- only add what is actually used and not generated.
"""

from hydra.overlay.python.dsl.meta.phantoms import var
from hydra.dsl.python import utils as _utils
from hydra.dsl.python import names as _names


# hydra.constants

# hydra.formatting

# hydra.names

# hydra.python.utils (used by coder.py; #556 -- generated, was var("hydra.python.utils.*"))
utils_assignment_statement = _utils.assignment_statement
utils_double_quoted_string = _utils.double_quoted_string
utils_find_namespaces = _utils.find_namespaces
utils_function_call = _utils.function_call
utils_indented_block = _utils.indented_block
utils_primary_with_expression_slices = _utils.primary_with_expression_slices
utils_primary_with_rhs = _utils.primary_with_rhs
utils_project_from_expression = _utils.project_from_expression
utils_py_atom_to_py_expression = _utils.py_atom_to_py_expression
utils_py_closed_pattern_to_py_patterns = _utils.py_closed_pattern_to_py_patterns
utils_py_expression_to_py_primary = _utils.py_expression_to_py_primary
utils_py_expression_to_py_star_named_expression = _utils.py_expression_to_py_star_named_expression
utils_py_expressions_to_py_args = _utils.py_expressions_to_py_args
# utils_py_name_to_py_expression: see partial-application/first-class-reference
# exceptions at the bottom of this file (used bare via Lists.map in one place).
# utils_py_name_to_py_type_parameter: see partial-application/first-class-reference
# exceptions at the bottom of this file (used bare via Lists.map, not called directly).
utils_py_primary_to_py_expression = _utils.py_primary_to_py_expression
utils_py_simple_statement_to_py_statement = _utils.py_simple_statement_to_py_statement
utils_string_to_py_expression = _utils.string_to_py_expression
utils_py_list = _utils.py_list
utils_py_name_to_py_primary = _utils.py_name_to_py_primary
utils_single_quoted_string = _utils.single_quoted_string
utils_primary_and_params = _utils.primary_and_params
utils_primary_with_slices = _utils.primary_with_slices
utils_py_expression_to_py_slice = _utils.py_expression_to_py_slice
utils_py_primary_to_py_slice = _utils.py_primary_to_py_slice
utils_target_python_version = _utils.target_python_version()
utils_annotated_expression = _utils.annotated_expression
utils_py_assignment_to_py_statement = _utils.py_assignment_to_py_statement
utils_py_class_definition_to_py_statement = _utils.py_class_definition_to_py_statement
utils_py_expression_to_py_statement = _utils.py_expression_to_py_statement
utils_triple_quoted_string = _utils.triple_quoted_string
utils_unit_variant_methods = _utils.unit_variant_methods
utils_annotated_statement = _utils.annotated_statement
utils_dotted_assignment_statement = _utils.dotted_assignment_statement
utils_comment_statement = _utils.comment_statement
utils_or_expression = _utils.or_expression
utils_py_expression_to_bitwise_or = _utils.py_expression_to_bitwise_or
utils_py_expression_to_disjunction = _utils.py_expression_to_disjunction
utils_raise_assertion_error = _utils.raise_assertion_error
utils_raise_type_error = _utils.raise_type_error
utils_return_single = _utils.return_single
utils_name_and_params = _utils.name_and_params
utils_py_none = _utils.py_none()
show_core_type = var("hydra.show.core.type")
names_type_variable_reference = _names.type_variable_reference
names_term_variable_reference = _names.term_variable_reference
names_encode_name_qualified = _names.encode_name_qualified

# hydra.annotations
formatting_normalize_comment = var("hydra.formatting.normalizeComment")

utils_type_alias_statement = _utils.type_alias_statement
utils_type_alias_statement310 = _utils.type_alias_statement310
utils_union_type_class_statements310 = _utils.union_type_class_statements310

# hydra.python.names (used by coder.py; #556 -- generated, was var("hydra.python.names.*"))
names_encode_enum_value = _names.encode_enum_value
names_encode_field_name = _names.encode_field_name
# names_encode_name: see partial-application exceptions at the bottom of this file
# (line 3399 of coder.py calls it with 3 args, one short of its arity-4 signature).
names_encode_namespace = _names.encode_namespace
# names_encode_namespace_with_overrides, names_encode_type_variable: see
# partial-application/first-class-reference exceptions at the bottom of this file.
names_use_future_annotations = _names.use_future_annotations()
names_variant_name = _names.variant_name
names_encode_constant_for_type_name = _names.encode_constant_for_type_name
names_encode_constant_for_field_name = _names.encode_constant_for_field_name

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


# Partial-application / first-class-reference sites: these functions are either
# referenced with FEWER args than their full arity (the rest arrive later in the
# term) or passed around as bare term-level values (e.g. to Lists.map, compose),
# so the arity-saturated generated wrappers in hydra.dsl.* cannot express them
# (calling one immediately builds a term; there is no unapplied handle to pass
# around). Keep as raw refs -- var(...) returns a TypedTerm whose __call__ builds
# the same application generated wrappers do, so it still works when called too.
environment_with_lambda_context = var("hydra.environment.withLambdaContext")
environment_with_let_context = var("hydra.environment.withLetContext")
environment_with_type_lambda_context = var("hydra.environment.withTypeLambdaContext")
formatting_convert_case = var("hydra.formatting.convertCase")
formatting_sanitize_with_underscores = var("hydra.formatting.sanitizeWithUnderscores")
utils_py_name_to_py_type_parameter = var("hydra.python.utils.pyNameToPyTypeParameter")
names_encode_type_variable = var("hydra.python.names.encodeTypeVariable")
names_encode_namespace_with_overrides = var("hydra.python.names.encodeNamespaceWithOverrides")
names_encode_name = var("hydra.python.names.encodeName")
utils_py_name_to_py_expression = var("hydra.python.utils.pyNameToPyExpression")

# hydra.util CaseConvention enum constants are TTerms (injects), already
# available as constants in hydra.dsl.util. Re-export here for symmetry.
from hydra.dsl.util import (
    case_convention_camel as util_case_convention_camel,
    case_convention_lower_snake as util_case_convention_lower_snake,
    case_convention_upper_snake as util_case_convention_upper_snake,
    case_convention_pascal as util_case_convention_pascal,
)
