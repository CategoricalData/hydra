"""A module for tier-0 constants."""

from __future__ import annotations
import hydra.core

ignored_variable = "_"

key_classes = hydra.core.Name("classes")

key_deprecated = hydra.core.Name("_deprecated")

key_description = hydra.core.Name("description")

key_exclude = hydra.core.Name("exclude")

key_max_length = hydra.core.Name("_maxLength")

key_min_length = hydra.core.Name("_minLength")

key_preserve_field_name = hydra.core.Name("_preserveFieldName")

key_type = hydra.core.Name("type")

# A placeholder name for row types as they are being constructed.
placeholder_name = hydra.core.Name("Placeholder")

max_trace_depth = 50