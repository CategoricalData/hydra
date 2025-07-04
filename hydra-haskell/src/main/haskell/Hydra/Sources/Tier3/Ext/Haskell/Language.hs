module Hydra.Sources.Tier3.Ext.Haskell.Language where

-- Standard Tier-3 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors              as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Compute                    as Compute
import qualified Hydra.Dsl.Core                       as Core
import qualified Hydra.Dsl.Graph                      as Graph
import qualified Hydra.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Lib.Equality               as Equality
import qualified Hydra.Dsl.Lib.Flows                  as Flows
import qualified Hydra.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Lib.Literals               as Literals
import qualified Hydra.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Lib.Math                   as Math
import qualified Hydra.Dsl.Lib.Optionals              as Optionals
import           Hydra.Dsl.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Lib.Strings                as Strings
import qualified Hydra.Dsl.Mantle                     as Mantle
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.TTerms                     as TTerms
import qualified Hydra.Dsl.TTypes                     as TTypes
import qualified Hydra.Dsl.Terms                      as Terms
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Types                      as Types
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Sources.Tier1.All              as Tier1
import qualified Hydra.Sources.Tier1.Constants        as Constants
import qualified Hydra.Sources.Tier1.Decode           as Decode
import qualified Hydra.Sources.Tier1.Encode.Core      as EncodeCore
import qualified Hydra.Sources.Tier1.Formatting       as Formatting
import qualified Hydra.Sources.Tier1.Literals         as Literals
import qualified Hydra.Sources.Tier1.Strip            as Strip
import qualified Hydra.Sources.Tier2.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Tier2.Adapt.Modules         as AdaptModules
import qualified Hydra.Sources.Tier2.Annotations      as Annotations
import qualified Hydra.Sources.Tier2.Arity            as Arity
import qualified Hydra.Sources.Tier2.Languages     as Languages
import qualified Hydra.Sources.Tier2.Decode.Core      as DecodeCore
import qualified Hydra.Sources.Tier2.Describe.Core    as DescribeCore
import qualified Hydra.Sources.Tier2.Extract.Core     as ExtractCore
import qualified Hydra.Sources.Tier2.Monads           as Monads
import qualified Hydra.Sources.Tier2.Grammars  as Grammars
import qualified Hydra.Sources.Tier2.Inference        as Inference
import qualified Hydra.Sources.Tier2.Lexical          as Lexical
import qualified Hydra.Sources.Tier2.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Tier2.Qnames           as Qnames
import qualified Hydra.Sources.Tier2.Reduction        as Reduction
import qualified Hydra.Sources.Tier2.Rewriting        as Rewriting
import qualified Hydra.Sources.Tier2.Schemas          as Schemas
import qualified Hydra.Sources.Tier2.Serialization    as Serialization
import qualified Hydra.Sources.Tier2.Show.Accessors   as ShowAccessors
import qualified Hydra.Sources.Tier2.Show.Core        as ShowCore
import qualified Hydra.Sources.Tier2.Sorting          as Sorting
import qualified Hydra.Sources.Tier2.Substitution     as Substitution
import qualified Hydra.Sources.Tier2.Tarjan           as Tarjan
import qualified Hydra.Sources.Tier2.Templating       as Templating
import qualified Hydra.Sources.Tier2.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Tier2.Unification      as Unification
import qualified Hydra.Sources.Tier2.Variants         as Variants
import qualified Data.Int                             as I
import qualified Data.List                            as L
import qualified Data.Map                             as M
import qualified Data.Set                             as S
import qualified Data.Maybe                           as Y


haskellLanguageDefinition :: String -> TTerm a -> TElement a
haskellLanguageDefinition = definitionInModule haskellLanguageModule

haskellLanguageModule :: Module
haskellLanguageModule = Module ns elements
    []
    [Tier1.hydraCodersModule] $
    Just "Language constraints and reserved words for Haskell"
  where
    ns = Namespace "hydra.ext.haskell.language"
    elements = [
      el haskellLanguageDef,
      el reservedWordsDef]

haskellLanguageDef :: TElement Language
haskellLanguageDef = haskellLanguageDefinition "haskellLanguage" $
    doc "Language constraints for Haskell" $
    Coders.language "hydra.ext.haskell"
      eliminationVariants
      literalVariants
      floatTypes
      functionVariants
      integerTypes
      termVariants
      typeVariants
      typePredicate
  where
      literalVariants = [
        LiteralVariantBoolean,
        LiteralVariantFloat,
        LiteralVariantInteger,
        LiteralVariantString]
      floatTypes = [
        -- Bigfloat is excluded for now
        FloatTypeFloat32, -- Float
        FloatTypeFloat64] -- Double
      integerTypes = [
        IntegerTypeBigint, -- Integer
        IntegerTypeInt8, -- Int8
        IntegerTypeInt16, -- Int16
        IntegerTypeInt32, -- Int
        IntegerTypeInt64] -- Int64
      termVariants = [
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
        TermVariantVariable,
        TermVariantWrap]
      typeVariants = [
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
        TypeVariantVariable,
        TypeVariantWrap]
      typePredicate = constant true

reservedWordsDef :: TElement (S.Set String)
reservedWordsDef = haskellLanguageDefinition "reservedWords" $
  doc ("Created on 2025-02-28 using GHCi 9.6.6\n\n"
    ++ "You can reproduce these lists of symbols by issuing the command `:browse Prelude` in GHCi, pasting the results into\n"
    ++ "/tmp/browse_Prelude.txt, and then running the Bash command provided with each list.\n\n"
    ++ "See also https://www.haskell.org/onlinereport/standard-prelude.html") $
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
