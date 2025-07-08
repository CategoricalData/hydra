{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Ext.Python.Language where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors                        as Accessors
import qualified Hydra.Dsl.Annotations                      as Anns
import qualified Hydra.Dsl.Ast                              as Ast
import qualified Hydra.Dsl.Coders                           as Coders
import qualified Hydra.Dsl.Compute                          as Compute
import qualified Hydra.Dsl.Core                             as Core
import qualified Hydra.Dsl.Graph                            as Graph
import qualified Hydra.Dsl.Grammar                          as Grammar
import qualified Hydra.Dsl.Lib.Chars                        as Chars
import qualified Hydra.Dsl.Lib.Equality                     as Equality
import qualified Hydra.Dsl.Lib.Flows                        as Flows
import qualified Hydra.Dsl.Lib.Lists                        as Lists
import qualified Hydra.Dsl.Lib.Literals                     as Literals
import qualified Hydra.Dsl.Lib.Logic                        as Logic
import qualified Hydra.Dsl.Lib.Maps                         as Maps
import qualified Hydra.Dsl.Lib.Math                         as Math
import qualified Hydra.Dsl.Lib.Optionals                    as Optionals
import qualified Hydra.Dsl.Lib.Sets                         as Sets
import           Hydra.Dsl.Lib.Strings                      as Strings
import qualified Hydra.Dsl.Mantle                           as Mantle
import qualified Hydra.Dsl.Module                           as Module
import           Hydra.Dsl.Phantoms                         as Phantoms
import qualified Hydra.Dsl.TTerms                           as TTerms
import qualified Hydra.Dsl.TTypes                           as TTypes
import qualified Hydra.Dsl.Tabular                          as Tabular
import qualified Hydra.Dsl.Terms                            as Terms
import qualified Hydra.Dsl.Topology                         as Topology
import qualified Hydra.Dsl.Types                            as Types
import qualified Hydra.Dsl.Typing                           as Typing
import qualified Hydra.Sources.Kernel.Types.All             as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules   as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations     as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Decode.Core     as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Decoding        as Decoding
import qualified Hydra.Sources.Kernel.Terms.Describe.Core   as DescribeCore
import qualified Hydra.Sources.Kernel.Terms.Describe.Mantle as DescribeMantle
import qualified Hydra.Sources.Kernel.Terms.Encode.Core     as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Mantle  as ExtractMantle
import qualified Hydra.Sources.Kernel.Terms.Formatting      as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars        as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference       as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages       as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical         as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals        as Literals
import qualified Hydra.Sources.Kernel.Terms.Monads          as Monads
import qualified Hydra.Sources.Kernel.Terms.Names           as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction       as Reduction
import qualified Hydra.Sources.Kernel.Terms.Rewriting       as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas         as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization   as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors  as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core       as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph      as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Mantle     as ShowMantle
import qualified Hydra.Sources.Kernel.Terms.Show.Typing     as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting         as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution    as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan          as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates       as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification     as Unification
import qualified Hydra.Sources.Kernel.Terms.Variants        as Variants
import           Prelude hiding ((++))
import qualified Data.Int                                   as I
import qualified Data.List                                  as L
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified Data.Maybe                                 as Y


pythonLanguageDefinition :: String -> TTerm a -> TElement a
pythonLanguageDefinition = definitionInModule pythonLanguageModule

pythonLanguageModule :: Module
pythonLanguageModule = Module ns elements
    [KernelTypes.hydraCodersModule, Lexical.hydraLexicalModule]
    [KernelTypes.hydraCoreModule, KernelTypes.hydraGraphModule, KernelTypes.hydraCodersModule] $
    Just "Language constraints and reserved words for Python 3"
  where
    ns = Namespace "hydra.ext.python.language"
    elements = [
      el pythonLanguageDef,
      el pythonReservedWordsDef]

pythonLanguageDef :: TElement Language
pythonLanguageDef = pythonLanguageDefinition "pythonLanguage" $
    doc "Language constraints for Python 3" $
    Coders.language "hydra.ext.python"
      eliminationVariants
      literalVariants
      floatTypes
      functionVariants
      integerTypes
      termVariants
      typeVariants
      typePredicate
  where
      eliminationVariants = [ -- TODO: verify whether all are supported
        EliminationVariantProduct,
        EliminationVariantRecord,
        EliminationVariantUnion,
        EliminationVariantWrap]
      literalVariants = [
        LiteralVariantBinary, -- bytes
        LiteralVariantBoolean, -- bool
        LiteralVariantFloat, -- (see float types)
        LiteralVariantInteger, -- (see integer types)
        LiteralVariantString] -- str
      floatTypes = [FloatTypeFloat64] -- Python has only one floating-point type
      functionVariants = [
        FunctionVariantElimination,
        FunctionVariantLambda,
        FunctionVariantPrimitive]
      integerTypes = [IntegerTypeBigint] -- Python has only one integer type
      termVariants = [ -- TODO: verify whether all are supported
        TermVariantApplication,
        TermVariantFunction,
        TermVariantLet,
        TermVariantList,
        TermVariantLiteral,
        TermVariantMap,
        TermVariantOptional,
        TermVariantProduct,
        TermVariantRecord,
        TermVariantSet,
        TermVariantUnion,
        -- TODO: TermVariantUnit, mapping to Python's None
        TermVariantVariable,
        TermVariantWrap]
      typeVariants = [ -- TODO: verify whether all are supported
        TypeVariantAnnotated,
        TypeVariantApplication,
        TypeVariantFunction,
        TypeVariantForall,
        TypeVariantList,
        TypeVariantLiteral,
        TypeVariantMap,
        TypeVariantOptional,
        TypeVariantProduct,
        TypeVariantRecord,
        TypeVariantSet,
        TypeVariantUnion,
        -- TODO: TypeVariantUnit, mapping to Python's NoneType
        TypeVariantVariable,
        TypeVariantWrap]
      typePredicate = constant true -- TODO: verify whether all are supported

pythonReservedWordsDef :: TElement (S.Set String)
pythonReservedWordsDef = pythonLanguageDefinition "pythonReservedWords" $
  doc "A set of reserved words in Python" $
  lets [
    "pythonKeywords">:
      doc "Python keywords, as enumerated at https://docs.python.org/3.13/reference/lexical_analysis.html#keywords" $
      list [
        "False", "None", "True", "and", "as", "assert", "async", "await", "break", "class", "continue", "def", "del",
        "elif", "else", "except", "finally", "for", "from", "global", "if", "import", "in", "is", "lambda", "nonlocal",
        "not", "or", "pass", "raise", "return", "try", "while", "with", "yield"],
    "hydraPythonKeywords">:
      doc "Reserved words which are specific to Hydra-Python" $
      list ["Node", "FrozenDict"]]
    $ Sets.fromList $ Lists.concat2 (var "pythonKeywords") (var "hydraPythonKeywords")
