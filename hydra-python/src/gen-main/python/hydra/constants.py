"""A module for tier-0 constants."""

from __future__ import annotations
import hydra.core

ignoredVariable = "_"

key_classes = hydra.core.Name("classes")

key_deprecated = hydra.core.Name("_deprecated")

key_description = hydra.core.Name("description")

key_exclude = hydra.core.Name("exclude")

key_maxLength = hydra.core.Name("_maxLength")

key_minLength = hydra.core.Name("_minLength")

key_preserveFieldName = hydra.core.Name("_preserveFieldName")

key_type = hydra.core.Name("type")

# A placeholder name for row types as they are being constructed.
placeholderName = hydra.core.Name("Placeholder")

maxTraceDepth = 50