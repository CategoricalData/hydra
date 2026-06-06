"""Language constraints and reserved words for Python 3.

Mirror of packages/hydra-python/src/main/haskell/Hydra/Sources/Python/Language.hs.
Builds the same Module value; serializing it via hydra.dsl.core's module-to-JSON
path produces JSON byte-equivalent to the Haskell-generated language.json.
"""

from hydra.core import Name
from hydra.dsl.python import Given
from hydra.packaging import EntityMetadata, Module, ModuleName

import hydra.dsl.meta.lib.lists as Lists
import hydra.dsl.meta.lib.sets as Sets
from hydra.dsl.meta.phantoms import *  # noqa: F401,F403
import hydra.dsl.core as Core
import hydra.dsl.variants as Variants
import hydra.dsl.coders as Coders


# Namespaces we depend on. Hardcoded here to mirror the Haskell:
#   moduleDependencies = [Lexical.ns] L.++ KernelTypes.kernelTypesNamespaces
# kernelTypesNamespaces is the list of all hydra.* type module namespaces in the
# canonical order from packages/hydra-kernel/.../Hydra/Sources/Kernel/Types/All.hs.
LEXICAL_NS = ModuleName("hydra.lexical")
from hydra.sources.python._source_dsl import KERNEL_TYPES_NAMESPACES, unqualified_dep


def _python_language_term():
    """The body of the pythonLanguage definition (a TypedTerm Language)."""
    return doc(
        "Language constraints for Python 3",
        lets(
            [
                field("literalVariants",
                    Sets.from_list(list_([
                        Variants.literal_variant_binary,
                        Variants.literal_variant_boolean,
                        Variants.literal_variant_decimal,
                        Variants.literal_variant_float,
                        Variants.literal_variant_integer,
                        Variants.literal_variant_string,
                    ])),
                ),
                field("floatTypes",
                    Sets.from_list(list_([
                        Core.float_type_float64,
                    ])),
                ),
                field("integerTypes",
                    Sets.from_list(list_([
                        Core.integer_type_bigint,
                    ])),
                ),
                field("termVariants",
                    Sets.from_list(list_([
                        Variants.term_variant_annotated,
                        Variants.term_variant_application,
                        Variants.term_variant_either,
                        Variants.term_variant_cases,
                        Variants.term_variant_lambda,
                        Variants.term_variant_project,
                        Variants.term_variant_unwrap,
                        Variants.term_variant_let,
                        Variants.term_variant_list,
                        Variants.term_variant_literal,
                        Variants.term_variant_map,
                        Variants.term_variant_optional,
                        Variants.term_variant_pair,
                        Variants.term_variant_record,
                        Variants.term_variant_set,
                        Variants.term_variant_type_application,
                        Variants.term_variant_type_lambda,
                        Variants.term_variant_inject,
                        Variants.term_variant_unit,
                        Variants.term_variant_variable,
                        Variants.term_variant_wrap,
                    ])),
                ),
                field("typeVariants",
                    Sets.from_list(list_([
                        Variants.type_variant_annotated,
                        Variants.type_variant_application,
                        Variants.type_variant_either,
                        Variants.type_variant_function,
                        Variants.type_variant_forall,
                        Variants.type_variant_list,
                        Variants.type_variant_literal,
                        Variants.type_variant_map,
                        Variants.type_variant_optional,
                        Variants.type_variant_pair,
                        Variants.type_variant_record,
                        Variants.type_variant_set,
                        Variants.type_variant_union,
                        Variants.type_variant_unit,
                        Variants.type_variant_variable,
                        Variants.type_variant_void,
                        Variants.type_variant_wrap,
                    ])),
                ),
                field("typePredicate",
                    constant(true()),
                ),
            ],
            Coders.language(
                Coders.language_name2(string("hydra.python")),
                Coders.language_constraints2(
                    var("literalVariants"),
                    var("floatTypes"),
                    var("integerTypes"),
                    var("termVariants"),
                    var("typeVariants"),
                    var("typePredicate"),
                ),
            ),
        ),
    )


def _python_reserved_words_term():
    """The body of the pythonReservedWords definition (a TypedTerm (Set String))."""
    keywords = [
        "False", "None", "True", "and", "as", "assert", "async", "await", "break",
        "class", "continue", "def", "del", "elif", "else", "except", "finally",
        "for", "from", "global", "if", "import", "in", "is", "lambda", "nonlocal",
        "not", "or", "pass", "raise", "return", "try", "while", "with", "yield",
    ]
    builtins = ["range"]
    hydra_specific = ["Node", "FrozenDict"]
    return doc(
        "A set of reserved words in Python",
        lets(
            [
                field("pythonKeywords",
                    doc(
                        "Python keywords, as enumerated at https://docs.python.org/3.13/reference/lexical_analysis.html#keywords",
                        list_([string(s) for s in keywords]),
                    ),
                ),
                field("pythonBuiltInFunctions",
                    doc(
                        "Some additional keywords we reserve in order to avoid collision with built-in functions",
                        list_([string(s) for s in builtins]),
                    ),
                ),
                field("hydraPythonKeywords",
                    doc(
                        "Reserved words which are specific to Hydra-Python",
                        list_([string(s) for s in hydra_specific]),
                    ),
                ),
            ],
            Sets.from_list(Lists.concat(list_([
                var("pythonKeywords"),
                var("pythonBuiltInFunctions"),
                var("hydraPythonKeywords"),
            ]))),
        ),
    )


# Build the module value. We construct definitions first, then the module.
_NS = ModuleName("hydra.python.language")


def _build_module() -> Module:
    """Construct the hydra.python.language Module value."""
    # We need a placeholder module first because definition_in_module needs it.
    # Mirror Haskell pattern: define = definitionInModule module_; build defs; then module_ uses them.
    placeholder = Module(
        _NS,
        Given(EntityMetadata(
            Given("Language constraints and reserved words for Python 3"),
            (),
            (),
            None_())),
        [unqualified_dep(LEXICAL_NS)] + KERNEL_TYPES_NAMESPACES,
        (),  # filled in below
    )
    python_language = definition_in_module(
        placeholder, "pythonLanguage", _python_language_term())
    python_reserved_words = definition_in_module(
        placeholder, "pythonReservedWords", _python_reserved_words_term())
    return Module(
        placeholder.name,
        placeholder.metadata,
        placeholder.dependencies,
        (
            to_definition(python_language),
            to_definition(python_reserved_words),
        ),
    )


module_ = _build_module()
