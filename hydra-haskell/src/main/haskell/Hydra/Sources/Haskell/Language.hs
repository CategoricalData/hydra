module Hydra.Sources.Haskell.Language where

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


haskellLanguageDefinition :: String -> TTerm a -> TElement a
haskellLanguageDefinition = definitionInModule haskellLanguageModule

haskellLanguageModule :: Module
haskellLanguageModule = Module (Namespace "hydra.ext.haskell.language")
  [el haskellLanguageDef, el reservedWordsDef]
  []
  KernelTypes.kernelTypesModules $
  Just "Language constraints and reserved words for Haskell"

haskellLanguageDef :: TElement Language
haskellLanguageDef = haskellLanguageDefinition "haskellLanguage" $
  doc "Language constraints for Haskell" $ lets [
  "literalVariants">: Sets.fromList $ list [
    Mantle.literalVariantBoolean,
    Mantle.literalVariantFloat,
    Mantle.literalVariantInteger,
    Mantle.literalVariantString],
  "floatTypes">: Sets.fromList $ list [
    -- Bigfloat is excluded for now
    Core.floatTypeFloat32, -- Float
    Core.floatTypeFloat64], -- Double
  "integerTypes">: Sets.fromList $ list [
    Core.integerTypeBigint, -- Integer
    Core.integerTypeInt8, -- Int8
    Core.integerTypeInt16, -- Int16
    Core.integerTypeInt32, -- Int
    Core.integerTypeInt64], -- Int64
  "termVariants">: Sets.fromList $ list [
    Mantle.termVariantApplication,
    Mantle.termVariantFunction,
    Mantle.termVariantLet,
    Mantle.termVariantList,
    Mantle.termVariantLiteral,
    Mantle.termVariantMap,
    Mantle.termVariantOptional,
    Mantle.termVariantProduct,
    Mantle.termVariantRecord,
    Mantle.termVariantSet,
    Mantle.termVariantUnion,
    Mantle.termVariantUnit,
    Mantle.termVariantVariable,
    Mantle.termVariantWrap],
  "typeVariants">: Sets.fromList $ list [
    Mantle.typeVariantAnnotated,
    Mantle.typeVariantApplication,
    Mantle.typeVariantFunction,
    Mantle.typeVariantForall,
    Mantle.typeVariantList,
    Mantle.typeVariantLiteral,
    Mantle.typeVariantMap,
    Mantle.typeVariantOptional,
    Mantle.typeVariantProduct,
    Mantle.typeVariantRecord,
    Mantle.typeVariantSet,
    Mantle.typeVariantUnion,
    Mantle.typeVariantUnit,
    Mantle.typeVariantVariable,
    Mantle.typeVariantWrap],
  "typePredicate">: constant true] $
  Coders.language
    (Coders.languageName $ string "hydra.ext.haskell")
    (Coders.languageConstraints
      (var "eliminationVariants")
      (var "literalVariants")
      (var "floatTypes")
      (var "functionVariants")
      (var "integerTypes")
      (var "termVariants")
      (var "typeVariants")
      (var "typePredicate"))

reservedWordsDef :: TElement (S.Set String)
reservedWordsDef = haskellLanguageDefinition "reservedWords" $
  doc ("Created on 2025-02-28 using GHCi 9.6.6\n\n"
    <> "You can reproduce these lists of symbols by issuing the command `:browse Prelude` in GHCi, pasting the results into\n"
    <> "/tmp/browse_Prelude.txt, and then running the Bash command provided with each list.\n\n"
    <> "See also https://www.haskell.org/onlinereport/standard-prelude.html") $
  lets [
    "keywordSymbols">:
      doc "Haskell's strictly reserved keywords; they cannot be used as identifiers." $
      list $ string <$> [
        "case", "class", "data", "default", "deriving", "do", "else", "forall", "foreign", "if", "import", "in",
        "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where"],
    "reservedSymbols">:
      doc "Hydra uses these symbols in generated code, so we reserve them to avoid conflicts." $
      list $ string <$> [
        "Bool", "Double", "False", "Float", "Int", "Integer", "Just", "Maybe", "Nothing", "Ord", "Show", "String", "True"]]
    $ Sets.fromList $ Lists.concat2 (var "keywordSymbols") (var "reservedSymbols")
