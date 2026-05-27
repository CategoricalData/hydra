-- Note: this is an automatically generated file. Do not edit.
-- | String representations of hydra.variants types

module Hydra.Show.Variants where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Show a term variant as a string
termVariant :: Variants.TermVariant -> String
termVariant x =
    case x of
      Variants.TermVariantAnnotated -> "annotated"
      Variants.TermVariantApplication -> "application"
      Variants.TermVariantCases -> "cases"
      Variants.TermVariantEither -> "either"
      Variants.TermVariantLambda -> "lambda"
      Variants.TermVariantLet -> "let"
      Variants.TermVariantList -> "list"
      Variants.TermVariantLiteral -> "literal"
      Variants.TermVariantMap -> "map"
      Variants.TermVariantMaybe -> "maybe"
      Variants.TermVariantPair -> "pair"
      Variants.TermVariantProject -> "project"
      Variants.TermVariantRecord -> "record"
      Variants.TermVariantSet -> "set"
      Variants.TermVariantTypeLambda -> "typeLambda"
      Variants.TermVariantTypeApplication -> "typeApplication"
      Variants.TermVariantInject -> "inject"
      Variants.TermVariantUnit -> "unit"
      Variants.TermVariantUnwrap -> "unwrap"
      Variants.TermVariantVariable -> "variable"
      Variants.TermVariantWrap -> "wrap"
-- | Show a type variant as a string
typeVariant :: Variants.TypeVariant -> String
typeVariant x =
    case x of
      Variants.TypeVariantAnnotated -> "annotated"
      Variants.TypeVariantApplication -> "application"
      Variants.TypeVariantEither -> "either"
      Variants.TypeVariantForall -> "forall"
      Variants.TypeVariantFunction -> "function"
      Variants.TypeVariantList -> "list"
      Variants.TypeVariantLiteral -> "literal"
      Variants.TypeVariantMap -> "map"
      Variants.TypeVariantMaybe -> "maybe"
      Variants.TypeVariantPair -> "pair"
      Variants.TypeVariantRecord -> "record"
      Variants.TypeVariantSet -> "set"
      Variants.TypeVariantUnion -> "union"
      Variants.TypeVariantUnit -> "unit"
      Variants.TypeVariantVariable -> "variable"
      Variants.TypeVariantVoid -> "void"
      Variants.TypeVariantWrap -> "wrap"
