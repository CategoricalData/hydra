{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Describe.Core where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors              as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Coders                 as Coders
import qualified Hydra.Dsl.Compute                as Compute
import qualified Hydra.Dsl.Core                   as Core
import qualified Hydra.Dsl.Graph                  as Graph
import qualified Hydra.Dsl.Lib.Chars              as Chars
import qualified Hydra.Dsl.Lib.Equality           as Equality
import qualified Hydra.Dsl.Lib.Flows              as Flows
import qualified Hydra.Dsl.Lib.Lists              as Lists
import qualified Hydra.Dsl.Lib.Literals           as Literals
import qualified Hydra.Dsl.Lib.Logic              as Logic
import qualified Hydra.Dsl.Lib.Maps               as Maps
import qualified Hydra.Dsl.Lib.Math               as Math
import qualified Hydra.Dsl.Lib.Optionals          as Optionals
import           Hydra.Dsl.Phantoms               as Phantoms
import qualified Hydra.Dsl.Lib.Sets               as Sets
import           Hydra.Dsl.Lib.Strings            as Strings
import qualified Hydra.Dsl.Mantle                 as Mantle
import qualified Hydra.Dsl.Module                 as Module
import qualified Hydra.Dsl.TTerms                 as TTerms
import qualified Hydra.Dsl.TTypes                 as TTypes
import qualified Hydra.Dsl.Terms                  as Terms
import qualified Hydra.Dsl.Topology               as Topology
import qualified Hydra.Dsl.Types                  as Types
import qualified Hydra.Dsl.Typing                 as Typing
import qualified Hydra.Sources.Tier1.All          as Tier1
import qualified Hydra.Sources.Tier1.Constants    as Constants
import qualified Hydra.Sources.Tier1.Encode.Core as EncodeCore
import qualified Hydra.Sources.Tier1.Decode       as Decode
import qualified Hydra.Sources.Tier1.Formatting   as Formatting
import qualified Hydra.Sources.Tier1.Functions    as Functions
import qualified Hydra.Sources.Tier1.Literals     as Literals
import qualified Hydra.Sources.Tier1.Strip        as Strip
import           Prelude hiding ((++))
import qualified Data.Int                  as I
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

-- Uncomment tier-2 sources as needed
--import qualified Hydra.Sources.Tier2.Adapters as Adapters
--import qualified Hydra.Sources.Tier2.AdapterUtils as AdapterUtils
--import qualified Hydra.Sources.Tier2.Annotations as Annotations
--import qualified Hydra.Sources.Tier2.Arity as Arity
--import qualified Hydra.Sources.Tier2.Decode.Core as DecodeCore
--import qualified Hydra.Sources.Tier2.CoreLanguage as CoreLanguage
--import qualified Hydra.Sources.Tier2.Errors as Errors
--import qualified Hydra.Sources.Tier2.Extract.Core as ExtractCore
--import qualified Hydra.Sources.Tier2.Monads as Monads
--import qualified Hydra.Sources.Tier2.GrammarToModule as GrammarToModule
--import qualified Hydra.Sources.Tier2.Inference as Inference
--import qualified Hydra.Sources.Tier2.Lexical as Lexical
--import qualified Hydra.Sources.Tier2.LiteralAdapters as LiteralAdapters
--import qualified Hydra.Sources.Tier2.Describe.Core as DescribeCore
--import qualified Hydra.Sources.Tier2.Qnames as Qnames
--import qualified Hydra.Sources.Tier2.Reduction as Reduction
--import qualified Hydra.Sources.Tier2.Rewriting as Rewriting
--import qualified Hydra.Sources.Tier2.Schemas as Schemas
--import qualified Hydra.Sources.Tier2.Serialization as Serialization
--import qualified Hydra.Sources.Tier2.Show.Accessors as ShowAccessors
--import qualified Hydra.Sources.Tier2.Show.Core as ShowCore
--import qualified Hydra.Sources.Tier2.Sorting as Sorting
--import qualified Hydra.Sources.Tier2.Substitution as Substitution
--import qualified Hydra.Sources.Tier2.Tarjan as Tarjan
--import qualified Hydra.Sources.Tier2.Templating as Templating
--import qualified Hydra.Sources.Tier2.TermAdapters as TermAdapters
--import qualified Hydra.Sources.Tier2.TermEncoding as TermEncoding
--import qualified Hydra.Sources.Tier2.Unification as Unification
import qualified Hydra.Sources.Tier2.Variants as Variants


describeCoreModule :: Module
describeCoreModule = Module (Namespace "hydra.describe.core") elements
    [Variants.hydraVariantsModule]
    [Tier1.hydraCoreModule, Tier1.hydraMantleModule] $
    Just "Utilities for use in transformations"
  where
   elements = [
     el floatTypeDef,
     el integerTypeDef,
     el literalTypeDef,
     el precisionDef, -- TODO: move out of hydra.describe.core
     el typeDef]

printingDefinition :: String -> TTerm a -> TElement a
printingDefinition = definitionInModule describeCoreModule


floatTypeDef :: TElement (FloatType -> String)
floatTypeDef = printingDefinition "floatType" $
  doc "Display a floating-point type as a string" $
  lambda "t" $ (ref precisionDef <.> ref Variants.floatTypePrecisionDef @@ var "t") ++ string " floating-point number"

integerTypeDef :: TElement (IntegerType -> String)
integerTypeDef = printingDefinition "integerType" $
  doc "Display an integer type as a string" $
  lambda "t" $ (ref precisionDef <.> ref Variants.integerTypePrecisionDef @@ var "t")
    ++ string " integer"

literalTypeDef :: TElement (LiteralType -> String)
literalTypeDef = printingDefinition "literalType" $
  doc "Display a literal type as a string" $
  match _LiteralType Nothing [
    _LiteralType_binary>>: constant $ string "binary string",
    _LiteralType_boolean>>: constant $ string "boolean value",
    _LiteralType_float>>: ref floatTypeDef,
    _LiteralType_integer>>: ref integerTypeDef,
    _LiteralType_string>>: constant $ string "character string"]

precisionDef :: TElement (Precision -> String)
precisionDef = printingDefinition "precision" $
  doc "Display numeric precision as a string" $
  match _Precision Nothing [
    _Precision_arbitrary>>: constant $ string "arbitrary-precision",
    _Precision_bits>>: lambda "bits" $ Literals.showInt32 (var "bits") ++ string "-bit"]

typeDef :: TElement (Type -> String)
typeDef = printingDefinition "type" $
  doc "Display a type as a string" $
  match _Type Nothing [
    _Type_annotated>>: lambda "a" $ string "annotated " ++ (ref typeDef @@
      (project _AnnotatedType _AnnotatedType_subject @@ var "a")),
    _Type_application>>: lambda "at" $ Strings.cat $ list [
      ref typeDef @@ (Core.applicationTypeFunction $ var "at"),
      string " applied to ",
      ref typeDef @@ (Core.applicationTypeArgument $ var "at")],
    _Type_literal>>: ref literalTypeDef,
    _Type_function>>: lambda "ft" $ string "function from "
      ++ (ref typeDef @@ (project _FunctionType _FunctionType_domain @@ var "ft"))
      ++ string " to "
      ++ (ref typeDef @@ (project _FunctionType _FunctionType_codomain @@ var "ft")),
    _Type_forall>>: lambda "fat" $ Strings.cat2 (string "polymorphic ") (ref typeDef @@ (Core.forallTypeBody $ var "fat")),
    _Type_list>>: lambda "t" $ string "list of " ++ (ref typeDef @@ var "t"),
    _Type_map>>: lambda "mt" $ string "map from "
      ++ (ref typeDef @@ (project _MapType _MapType_keys @@ var "mt"))
      ++ string " to "
      ++ (ref typeDef @@ (project _MapType _MapType_values  @@ var "mt")),
    _Type_optional>>: lambda "ot" $ string "optional " ++ (ref typeDef @@ var "ot"),
    _Type_product>>: constant $ string "tuple",
    _Type_record>>: constant $ string "record",
    _Type_set>>: lambda "st" $ string "set of " ++ (ref typeDef @@ var "st"),
    _Type_sum>>: constant $ string "variant tuple",
    _Type_union>>: constant $ string "union",
    _Type_variable>>: constant $ string "instance of a named type",
    _Type_wrap>>: lambda "n" $ string "wrapper for "
      ++ (ref typeDef @@ (project _WrappedType _WrappedType_object @@ var "n"))]
