# Note: this is an automatically generated file. Do not edit.

r"""A module for tier-0 constants."""

from __future__ import annotations
import hydra.core

# Disable type checking by default, for better performance.
debug_inference = True

# The name used for ignored variables.
ignored_variable = "_"

# Annotation key for type classes.
key_classes = hydra.core.Name("classes")

# Annotation key for debug identifiers.
key_debug_id = hydra.core.Name("debugId")

# Annotation key for deprecated markers.
key_deprecated = hydra.core.Name("deprecated")

# Annotation key for descriptions.
key_description = hydra.core.Name("description")

# Annotation key for exclusions.
key_exclude = hydra.core.Name("exclude")

# A flag which tells the language coders to encode a given encoded type as a term rather than a native type.
key_first_class_type = hydra.core.Name("firstClassType")

# A counter for generating fresh type variable names.
key_fresh_type_variable_count = hydra.core.Name("freshTypeVariableCount")

# Annotation key for maximum length constraints.
key_max_length = hydra.core.Name("maxLength")

# Annotation key for minimum length constraints.
key_min_length = hydra.core.Name("minLength")

# Annotation key for preserving field names during code generation.
key_preserve_field_name = hydra.core.Name("preserveFieldName")

# Annotation key for type information.
key_type = hydra.core.Name("type")

# The maximum value of a 32-bit integer.
max_int32 = 9223372036854775807

# A maximum depth for nested flows. Currently, this is set very high because deep flows are common in type inference over the Hydra kernel.
max_trace_depth = 4000

# A placeholder name for row types as they are being constructed.
placeholder_name = hydra.core.Name("Placeholder")

# A standard warning message for auto-generated files.
warning_auto_generated_file = "Note: this is an automatically generated file. Do not edit."
