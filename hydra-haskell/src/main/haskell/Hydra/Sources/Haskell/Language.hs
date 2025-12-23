module Hydra.Sources.Haskell.Language where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import qualified Hydra.Sources.Kernel.Terms.All             as KernelTerms
import qualified Hydra.Sources.Kernel.Types.All             as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules   as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple    as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations     as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking        as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Decode.Core               as DecodeCore
import qualified Hydra.Sources.Encode.Core                as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util    as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting      as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars        as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference       as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages       as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical         as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals        as Literals
import qualified Hydra.Sources.Kernel.Terms.Monads          as Monads
import qualified Hydra.Sources.Kernel.Terms.Names           as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction       as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect         as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting       as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas         as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization   as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors  as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core       as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph      as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta       as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing     as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting         as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution    as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan          as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates       as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification     as Unification
import           Prelude hiding ((++))
import qualified Data.Int                                   as I
import qualified Data.List                                  as L
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified Data.Maybe                                 as Y


haskellLanguageDefinition :: String -> TTerm a -> TBinding a
haskellLanguageDefinition = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.haskell.language"

module_ :: Module
module_ = Module ns
  [toBinding haskellLanguage, toBinding reservedWords]
  []
  KernelTypes.kernelTypesNamespaces $
  Just "Language constraints and reserved words for Haskell"

haskellLanguage :: TBinding Language
haskellLanguage = haskellLanguageDefinition "haskellLanguage" $
  doc "Language constraints for Haskell" $ lets [
  "eliminationVariants">: Sets.fromList $ list [
    Variants.eliminationVariantRecord,
    Variants.eliminationVariantUnion,
    Variants.eliminationVariantWrap],
  "literalVariants">: Sets.fromList $ list [
    Variants.literalVariantBoolean,
    Variants.literalVariantFloat,
    Variants.literalVariantInteger,
    Variants.literalVariantString],
  "floatTypes">: Sets.fromList $ list [
    Core.floatTypeFloat32, -- Float
    Core.floatTypeFloat64], -- Double
  "functionVariants">: Sets.fromList $ list [
    Variants.functionVariantElimination,
    Variants.functionVariantLambda,
    Variants.functionVariantPrimitive],
  "integerTypes">: Sets.fromList $ list [
    Core.integerTypeBigint, -- Integer
    Core.integerTypeInt8, -- Int8
    Core.integerTypeInt16, -- Int16
    Core.integerTypeInt32, -- Int
    Core.integerTypeInt64], -- Int64
  "termVariants">: Sets.fromList $ list [
    Variants.termVariantAnnotated,
    Variants.termVariantApplication,
    Variants.termVariantEither,
    Variants.termVariantFunction,
    Variants.termVariantLet,
    Variants.termVariantList,
    Variants.termVariantLiteral,
    Variants.termVariantMap,
    Variants.termVariantMaybe,
    Variants.termVariantPair,
    Variants.termVariantRecord,
    Variants.termVariantSet,
    Variants.termVariantUnion,
    Variants.termVariantUnit,
    Variants.termVariantVariable,
    Variants.termVariantWrap],
  "typeVariants">: Sets.fromList $ list [
    Variants.typeVariantAnnotated,
    Variants.typeVariantApplication,
    Variants.typeVariantEither,
    Variants.typeVariantFunction,
    Variants.typeVariantForall,
    Variants.typeVariantList,
    Variants.typeVariantLiteral,
    Variants.typeVariantMap,
    Variants.typeVariantMaybe,
    Variants.typeVariantPair,
    Variants.typeVariantRecord,
    Variants.typeVariantSet,
    Variants.typeVariantUnion,
    Variants.typeVariantUnit,
    Variants.typeVariantVariable,
    Variants.typeVariantWrap],
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

reservedWords :: TBinding (S.Set String)
reservedWords = haskellLanguageDefinition "reservedWords" $
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
