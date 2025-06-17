{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.AdapterUtils where

-- Standard Tier-2 imports
import qualified Hydra.Dsl.Coders          as Coders
import qualified Hydra.Dsl.Compute         as Compute
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Chars       as Chars
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import           Hydra.Dsl.Phantoms        as Phantoms
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Mantle          as Mantle
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.TTerms          as TTerms
import qualified Hydra.Dsl.TTypes          as TTypes
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier1.All
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

import qualified Hydra.Dsl.Expect as Expect
import Hydra.Sources.Libraries
import Hydra.Sources.Tier2.Qnames
import Hydra.Sources.Tier2.Variants


adapterUtilsDefinition :: String -> TTerm a -> TElement a
adapterUtilsDefinition = definitionInModule hydraAdapterUtilsModule

hydraAdapterUtilsModule :: Module
hydraAdapterUtilsModule = Module (Namespace "hydra.adapterUtils") elements
    [hydraQnamesModule, hydraStripModule, hydraVariantsModule]
    [hydraCodersModule, hydraComputeModule, hydraMantleModule, hydraModuleModule] $
    Just ("Additional adapter utilities, above and beyond the generated ones.")
  where
   elements = [
     el keyTypesDef,
     el bidirectionalDef,
     el chooseAdapterDef,
     el composeCodersDef,
     el encodeDecodeDef,
     el floatTypeIsSupportedDef,
     el idAdapterDef,
     el idCoderDef,
     el integerTypeIsSupportedDef,
     el literalTypeIsSupportedDef,
     el nameToFilePathDef,
     el nameToFilePathNewDef,
     el typeIsSupportedDef,
     el unidirectionalCoderDef]

keyTypesDef :: TElement Name
keyTypesDef = adapterUtilsDefinition "key_types" $
  doc "Key for types in adapter utilities" $
  Core.name' "types"

bidirectionalDef :: TElement ((CoderDirection -> b -> Flow s b) -> Coder s s b b)
bidirectionalDef = adapterUtilsDefinition "bidirectional" $
  doc "Create a bidirectional coder from a direction-aware function" $
  lambda "f" $ Compute.coder (var "f" @@ Coders.coderDirectionEncode) (var "f" @@ Coders.coderDirectionDecode)

chooseAdapterDef :: TElement ((t -> Flow so [SymmetricAdapter si t v]) -> (t -> Bool) -> (t -> String ) -> (t -> String) -> t -> Flow so (SymmetricAdapter si t v))
chooseAdapterDef = adapterUtilsDefinition "chooseAdapter" $
  doc "Choose an appropriate adapter for a type" $
  lambdas ["alts", "supported", "show", "describe", "typ"] $
    Logic.ifElse (var "supported" @@ var "typ")
      (Flows.pure $ Compute.adapter false (var "typ") (var "typ") (ref idCoderDef))
      (Flows.bind (var "alts" @@ var "typ") $
        lambda "raw" $ lets [
          "candidates">: Lists.filter (lambda "adapter" $ var "supported" @@ Compute.adapterTarget (var "adapter")) (var "raw")]
          $ Logic.ifElse (Lists.null $ var "candidates")
              (Flows.fail $ Strings.cat $ list [
                string "no adapters found for ",
                var "describe" @@ var "typ",
                Logic.ifElse (Lists.null $ var "raw")
                  (string "")
                  (Strings.cat $ list [
                    string " (discarded ",
                    Literals.showInt32 $ Lists.length $ var "raw",
                    string " unsupported candidate types: ",
                    Io.showList (var "show") $ Lists.map (unaryFunction Compute.adapterTarget) (var "raw"),
                    string ")"]),
                string ". Original type: ",
                var "show" @@ var "typ"])
              (Flows.pure $ Lists.head $ var "candidates"))

composeCodersDef :: TElement (Coder s s a b -> Coder s s b c -> Coder s s a c)
composeCodersDef = adapterUtilsDefinition "composeCoders" $
  doc "Compose two coders" $
  lambda "c1" $ lambda "c2" $
    Compute.coder
      (lambda "a" $ Flows.bind (Compute.coderEncode (var "c1") @@ var "a") (Compute.coderEncode (var "c2")))
      (lambda "c" $ Flows.bind (Compute.coderDecode (var "c2") @@ var "c") (Compute.coderDecode (var "c1")))

encodeDecodeDef :: TElement (CoderDirection -> Coder s s x x -> x -> Flow s x)
encodeDecodeDef = adapterUtilsDefinition "encodeDecode" $
  doc "Apply coder in the specified direction" $
  lambda "dir" $ lambda "coder" $
    match _CoderDirection Nothing [
      _CoderDirection_encode>>: constant $ Compute.coderEncode (var "coder"),
      _CoderDirection_decode>>: constant $ Compute.coderDecode (var "coder")]
    @@ var "dir"

floatTypeIsSupportedDef :: TElement (LanguageConstraints -> FloatType -> Bool)
floatTypeIsSupportedDef = adapterUtilsDefinition "floatTypeIsSupported" $
  doc "Check if float type is supported by language constraints" $
  lambda "constraints" $ lambda "ft" $
    Sets.member (var "ft") (Coders.languageConstraintsFloatTypes $ var "constraints")

idAdapterDef :: TElement (t -> SymmetricAdapter s t v)
idAdapterDef = adapterUtilsDefinition "idAdapter" $
  doc "Identity adapter" $
  lambda "t" $ Compute.adapter false (var "t") (var "t") (ref idCoderDef)

idCoderDef :: TElement (Coder s s a a)
idCoderDef = adapterUtilsDefinition "idCoder" $
  doc "Identity coder" $
  Compute.coder (unaryFunction Flows.pure) (unaryFunction Flows.pure)

integerTypeIsSupportedDef :: TElement (LanguageConstraints -> IntegerType -> Bool)
integerTypeIsSupportedDef = adapterUtilsDefinition "integerTypeIsSupported" $
  doc "Check if integer type is supported by language constraints" $
  lambda "constraints" $ lambda "it" $
    Sets.member (var "it") (Coders.languageConstraintsIntegerTypes $ var "constraints")

literalTypeIsSupportedDef :: TElement (LanguageConstraints -> LiteralType -> Bool)
literalTypeIsSupportedDef = adapterUtilsDefinition "literalTypeIsSupported" $
  doc "Check if literal type is supported by language constraints" $
  lambda "constraints" $ lambda "at" $
    Logic.and
      (Sets.member (ref literalTypeVariantDef @@ var "at") (Coders.languageConstraintsLiteralVariants $ var "constraints"))
      (match _LiteralType (Just true) [
        _LiteralType_float>>: lambda "ft" $ ref floatTypeIsSupportedDef @@ var "constraints" @@ var "ft",
        _LiteralType_integer>>: lambda "it" $ ref integerTypeIsSupportedDef @@ var "constraints" @@ var "it"]
      @@ var "at")

nameToFilePathDef :: TElement (CaseConvention -> FileExtension -> Name -> FilePath)
nameToFilePathDef = adapterUtilsDefinition "nameToFilePath" $
  doc "Convert name to file path" $
  lambda "caseConv" $ lambda "ext" $ lambda "name" $ lets [
    "qualName">: ref qualifyNameDef @@ var "name",
    "ns">: Module.qualifiedNameNamespace $ var "qualName",
    "local">: Module.qualifiedNameLocal $ var "qualName",
    "prefix">: Optionals.maybe (string "")
      (lambda "n" $ Strings.cat2 (Module.unNamespace $ var "n") (string "/"))
      (var "ns")]
    $ ref namespaceToFilePathDef @@ var "caseConv" @@ var "ext" @@
      (Module.namespace $ Strings.cat2 (var "prefix") (var "local"))

nameToFilePathNewDef :: TElement (CaseConvention -> CaseConvention -> FileExtension -> Name -> FilePath)
nameToFilePathNewDef = adapterUtilsDefinition "nameToFilePathNew" $
  doc "Convert name to file path with different case conventions" $
  lambda "nsConv" $ lambda "localConv" $ lambda "ext" $ lambda "name" $ lets [
    "qualName">: ref qualifyNameDef @@ var "name",
    "ns">: Module.qualifiedNameNamespace $ var "qualName",
    "local">: Module.qualifiedNameLocal $ var "qualName",
    "nsToFilePath">: lambda "ns" $
      Strings.intercalate (string "/") $ Lists.map
        (lambda "part" $ ref convertCaseDef @@ Mantle.caseConventionCamel @@ var "nsConv" @@ var "part")
        (Strings.splitOn (string ".") $ Module.unNamespace $ var "ns"),
    "prefix">: Optionals.maybe (string "")
      (lambda "n" $ Strings.cat2 (var "nsToFilePath" @@ var "n") (string "/"))
      (var "ns"),
    "suffix">: ref convertCaseDef @@ Mantle.caseConventionPascal @@ var "localConv" @@ var "local"]
    $ Strings.cat $ list [var "prefix", var "suffix", string ".", Module.unFileExtension $ var "ext"]

typeIsSupportedDef :: TElement (LanguageConstraints -> Type -> Bool)
typeIsSupportedDef = adapterUtilsDefinition "typeIsSupported" $
  doc "Check if type is supported by language constraints" $
  lambda "constraints" $ lambda "t" $ lets [
    "base">: ref stripTypeDef @@ var "t",
    "isSupportedVariant">: lambda "v" $
      Logic.or
--        (Equality.equal (var "v") Mantle.typeVariantVariable)
        (cases _TypeVariant (var "v") (Just false) [_TypeVariant_variable>>: constant true])
        (Sets.member (var "v") (Coders.languageConstraintsTypeVariants $ var "constraints"))]
    $ Logic.and
        (Coders.languageConstraintsTypes (var "constraints") @@ var "base")
        (Logic.and
          (var "isSupportedVariant" @@ (ref typeVariantDef @@ var "base"))
          (match _Type Nothing [
            _Type_annotated>>: lambda "at" $ ref typeIsSupportedDef @@ var "constraints" @@ Core.annotatedTypeSubject (var "at"),
            _Type_application>>: lambda "app" $
              Logic.and
                (ref typeIsSupportedDef @@ var "constraints" @@ Core.applicationTypeFunction (var "app"))
                (ref typeIsSupportedDef @@ var "constraints" @@ Core.applicationTypeArgument (var "app")),
            _Type_forall>>: lambda "ft" $ ref typeIsSupportedDef @@ var "constraints" @@ Core.forallTypeBody (var "ft"),
            _Type_function>>: lambda "ft" $
              Logic.and
                (ref typeIsSupportedDef @@ var "constraints" @@ Core.functionTypeDomain (var "ft"))
                (ref typeIsSupportedDef @@ var "constraints" @@ Core.functionTypeCodomain (var "ft")),
            _Type_list>>: lambda "lt" $ ref typeIsSupportedDef @@ var "constraints" @@ var "lt",
            _Type_literal>>: lambda "at" $ ref literalTypeIsSupportedDef @@ var "constraints" @@ var "at",
            _Type_map>>: lambda "mt" $
              Logic.and
                (ref typeIsSupportedDef @@ var "constraints" @@ Core.mapTypeKeys (var "mt"))
                (ref typeIsSupportedDef @@ var "constraints" @@ Core.mapTypeValues (var "mt")),
            _Type_optional>>: lambda "ot" $ ref typeIsSupportedDef @@ var "constraints" @@ var "ot",
            _Type_product>>: lambda "types" $
              andAll $ Lists.map (ref typeIsSupportedDef @@ var "constraints") (var "types"),
            _Type_record>>: lambda "rt" $
              andAll $ Lists.map
                (lambda "field" $ ref typeIsSupportedDef @@ var "constraints" @@ Core.fieldTypeType (var "field"))
                (Core.rowTypeFields $ var "rt"),
            _Type_set>>: lambda "st" $ ref typeIsSupportedDef @@ var "constraints" @@ var "st",
            _Type_sum>>: lambda "types" $
              andAll $ Lists.map (ref typeIsSupportedDef @@ var "constraints") (var "types"),
            _Type_union>>: lambda "rt" $
              andAll $ Lists.map
                (lambda "field" $ ref typeIsSupportedDef @@ var "constraints" @@ Core.fieldTypeType (var "field"))
                (Core.rowTypeFields $ var "rt"),
            _Type_wrap>>: lambda "wt" $ ref typeIsSupportedDef @@ var "constraints" @@ Core.wrappedTypeObject (var "wt"),
            _Type_variable>>: constant true]
          @@ var "base"))
  where
    andAll = Lists.foldl (binaryFunction Logic.and) true

unidirectionalCoderDef :: TElement ((a -> Flow s b) -> Coder s s a b)
unidirectionalCoderDef = adapterUtilsDefinition "unidirectionalCoder" $
  doc "Create a unidirectional coder" $
  lambda "m" $
    Compute.coder
      (var "m")
      (constant $ Flows.fail $ string "inbound mapping is unsupported")
