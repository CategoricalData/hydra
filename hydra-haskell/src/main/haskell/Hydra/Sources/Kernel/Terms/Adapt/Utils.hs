
module Hydra.Sources.Kernel.Terms.Adapt.Utils where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  bidirectional, chooseAdapter, composeCoders, encodeDecode, floatTypeIsSupported,
  idAdapter, idCoder, integerTypeIsSupported, literalTypeIsSupported,
  nameToFilePath, typeIsSupported, unidirectionalCoder)
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
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names      as Names
import qualified Hydra.Sources.Kernel.Terms.Reflect    as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting  as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Show.Core  as ShowCore


ns :: Namespace
ns = Namespace "hydra.adapt.utils"

module_ :: Module
module_ = Module ns elements
    [Formatting.ns, Names.ns, Reflect.ns, Rewriting.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just ("Additional adapter utilities, above and beyond the generated ones.")
  where
   elements = [
     toBinding bidirectional,
     toBinding chooseAdapter,
     toBinding composeCoders,
     toBinding encodeDecode,
     toBinding floatTypeIsSupported,
     toBinding idAdapter,
     toBinding idCoder,
     toBinding integerTypeIsSupported,
     toBinding literalTypeIsSupported,
     toBinding nameToFilePath,
     toBinding typeIsSupported,
     toBinding unidirectionalCoder]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

bidirectional :: TBinding ((CoderDirection -> b -> Flow s b) -> Coder s s b b)
bidirectional = define "bidirectional" $
  doc "Create a bidirectional coder from a direction-aware function" $
  "f" ~> Compute.coder (var "f" @@ Coders.coderDirectionEncode) (var "f" @@ Coders.coderDirectionDecode)

chooseAdapter :: TBinding ((t -> Flow so [SymmetricAdapter si t v]) -> (t -> Bool) -> (t -> String ) -> (t -> String) -> t -> Flow so (SymmetricAdapter si t v))
chooseAdapter = define "chooseAdapter" $
  doc "Choose an appropriate adapter for a type" $
  "alts" ~> "supported" ~> "show" ~> "describe" ~> "typ" ~>
  Logic.ifElse (var "supported" @@ var "typ")
    (produce (Compute.adapter false (var "typ") (var "typ") idCoder))
    ("raw" <<~ var "alts" @@ var "typ" $
     "candidates" <~ Lists.filter ("adapter" ~> var "supported" @@ Compute.adapterTarget (var "adapter")) (var "raw") $
     Logic.ifElse (Lists.null (var "candidates"))
       (Flows.fail (Strings.cat (list [
         string "no adapters found for ",
         var "describe" @@ var "typ",
         Logic.ifElse (Lists.null (var "raw"))
           (string "")
           (Strings.cat (list [
             string " (discarded ",
             Literals.showInt32 (Lists.length (var "raw")),
             string " unsupported candidate types: ",
             ShowCore.list_ @@ var "show" @@ (Lists.map (unaryFunction Compute.adapterTarget) (var "raw")),
             string ")"])),
         string ". Original type: ",
         var "show" @@ var "typ"])))
       (produce (Lists.head (var "candidates"))))

composeCoders :: TBinding (Coder s s a b -> Coder s s b c -> Coder s s a c)
composeCoders = define "composeCoders" $
  doc "Compose two coders" $
  "c1" ~> "c2" ~>
  Compute.coder
    ("a" ~> "b1" <<~ Compute.coderEncode (var "c1") @@ var "a" $ Compute.coderEncode (var "c2") @@ var "b1")
    ("c" ~> "b2" <<~ Compute.coderDecode (var "c2") @@ var "c" $ Compute.coderDecode (var "c1") @@ var "b2")

encodeDecode :: TBinding (CoderDirection -> Coder s s x x -> x -> Flow s x)
encodeDecode = define "encodeDecode" $
  doc "Apply coder in the specified direction" $
  "dir" ~> "coder" ~> "term" ~>
  cases _CoderDirection (var "dir")
    Nothing [
    _CoderDirection_encode>>: constant (Compute.coderEncode (var "coder") @@ var "term"),
    _CoderDirection_decode>>: constant (Compute.coderDecode (var "coder") @@ var "term")]

floatTypeIsSupported :: TBinding (LanguageConstraints -> FloatType -> Bool)
floatTypeIsSupported = define "floatTypeIsSupported" $
  doc "Check if float type is supported by language constraints" $
  "constraints" ~> "ft" ~>
  Sets.member (var "ft") (Coders.languageConstraintsFloatTypes (var "constraints"))

idAdapter :: TBinding (t -> SymmetricAdapter s t v)
idAdapter = define "idAdapter" $
  doc "Identity adapter" $
  "t" ~> Compute.adapter false (var "t") (var "t") idCoder

idCoder :: TBinding (Coder s s a a)
idCoder = define "idCoder" $
  doc "Identity coder" $
  Compute.coder (unaryFunction Flows.pure) (unaryFunction Flows.pure)

integerTypeIsSupported :: TBinding (LanguageConstraints -> IntegerType -> Bool)
integerTypeIsSupported = define "integerTypeIsSupported" $
  doc "Check if integer type is supported by language constraints" $
  "constraints" ~> "it" ~>
  Sets.member (var "it") (Coders.languageConstraintsIntegerTypes (var "constraints"))

literalTypeIsSupported :: TBinding (LanguageConstraints -> LiteralType -> Bool)
literalTypeIsSupported = define "literalTypeIsSupported" $
  doc "Check if literal type is supported by language constraints" $
  "constraints" ~> "lt" ~>
  "isSupported" <~ ("lt" ~> cases _LiteralType (var "lt")
    (Just true) [
    _LiteralType_float>>: "ft" ~> floatTypeIsSupported @@ var "constraints" @@ var "ft",
    _LiteralType_integer>>: "it" ~> integerTypeIsSupported @@ var "constraints" @@ var "it"]) $
  Logic.and
    (Sets.member (Reflect.literalTypeVariant @@ var "lt") (Coders.languageConstraintsLiteralVariants (var "constraints")))
    (var "isSupported" @@ var "lt")

nameToFilePath :: TBinding (CaseConvention -> CaseConvention -> FileExtension -> Name -> FilePath)
nameToFilePath = define "nameToFilePath" $
  doc "Convert a name to file path, given case conventions for namespaces and local names, and assuming '/' as the file path separator" $
  "nsConv" ~> "localConv" ~> "ext" ~> "name" ~>
  "qualName" <~ Names.qualifyName @@ var "name" $
  "ns" <~ Module.qualifiedNameNamespace (var "qualName") $
  "local" <~ Module.qualifiedNameLocal (var "qualName") $
  "nsToFilePath" <~ ("ns" ~>
    Strings.intercalate (string "/") (Lists.map
      ("part" ~> Formatting.convertCase @@ Util.caseConventionCamel @@ var "nsConv" @@ var "part")
      (Strings.splitOn (string ".") (Module.unNamespace (var "ns"))))) $
  "prefix" <~ Maybes.maybe (string "")
    ("n" ~> Strings.cat2 (var "nsToFilePath" @@ var "n") (string "/"))
    (var "ns") $
  "suffix" <~ Formatting.convertCase @@ Util.caseConventionPascal @@ var "localConv" @@ var "local" $
  Strings.cat (list [var "prefix", var "suffix", string ".", Module.unFileExtension (var "ext")])

typeIsSupported :: TBinding (LanguageConstraints -> Type -> Bool)
typeIsSupported = define "typeIsSupported" $
  doc "Check if type is supported by language constraints" $
  "constraints" ~> "t" ~>
  "base" <~ Rewriting.deannotateType @@ var "t" $
  "isVariable" <~ ("v" ~> cases _TypeVariant (var "v")
    (Just false) [
    _TypeVariant_variable>>: constant true]) $
  "isSupportedVariant" <~ ("v" ~>
    Logic.or
      (var "isVariable" @@ var "v")
      (Sets.member (var "v") (Coders.languageConstraintsTypeVariants (var "constraints")))) $
  "isSupported" <~ ("base" ~> cases _Type (var "base")
    Nothing [
    _Type_annotated>>: "at" ~> typeIsSupported @@ var "constraints" @@ Core.annotatedTypeBody (var "at"),
    _Type_application>>: "app" ~> Logic.and
      (typeIsSupported @@ var "constraints" @@ Core.applicationTypeFunction (var "app"))
      (typeIsSupported @@ var "constraints" @@ Core.applicationTypeArgument (var "app")),
    _Type_either>>: "et" ~> Logic.and
      (typeIsSupported @@ var "constraints" @@ Core.eitherTypeLeft (var "et"))
      (typeIsSupported @@ var "constraints" @@ Core.eitherTypeRight (var "et")),
    _Type_forall>>: "ft" ~> typeIsSupported @@ var "constraints" @@ Core.forallTypeBody (var "ft"),
    _Type_function>>: "ft" ~> Logic.and
      (typeIsSupported @@ var "constraints" @@ Core.functionTypeDomain (var "ft"))
      (typeIsSupported @@ var "constraints" @@ Core.functionTypeCodomain (var "ft")),
    _Type_list>>: "lt" ~> typeIsSupported @@ var "constraints" @@ var "lt",
    _Type_literal>>: "at" ~> literalTypeIsSupported @@ var "constraints" @@ var "at",
    _Type_map>>: "mt" ~> Logic.and
      (typeIsSupported @@ var "constraints" @@ Core.mapTypeKeys (var "mt"))
      (typeIsSupported @@ var "constraints" @@ Core.mapTypeValues (var "mt")),
    _Type_maybe>>: "ot" ~> typeIsSupported @@ var "constraints" @@ var "ot",
    _Type_pair>>: "pt" ~> Logic.and
      (typeIsSupported @@ var "constraints" @@ Core.pairTypeFirst (var "pt"))
      (typeIsSupported @@ var "constraints" @@ Core.pairTypeSecond (var "pt")),
    _Type_record>>: "rt" ~>
      andAll (Lists.map
        ("field" ~> typeIsSupported @@ var "constraints" @@ Core.fieldTypeType (var "field"))
        (Core.rowTypeFields (var "rt"))),
    _Type_set>>: "st" ~> typeIsSupported @@ var "constraints" @@ var "st",
    _Type_union>>: "rt" ~>
      andAll (Lists.map
        ("field" ~> typeIsSupported @@ var "constraints" @@ Core.fieldTypeType (var "field"))
        (Core.rowTypeFields (var "rt"))),
    _Type_unit>>: constant true,
    _Type_wrap>>: "wt" ~> typeIsSupported @@ var "constraints" @@ Core.wrappedTypeBody (var "wt"),
    _Type_variable>>: constant true]) $
  Logic.and
    (Coders.languageConstraintsTypes (var "constraints") @@ var "base")
    (Logic.and
      (var "isSupportedVariant" @@ (Reflect.typeVariant @@ var "base"))
      (var "isSupported" @@ var "base"))
  where
    andAll = Lists.foldl (binaryFunction Logic.and) true

unidirectionalCoder :: TBinding ((a -> Flow s b) -> Coder s s a b)
unidirectionalCoder = define "unidirectionalCoder" $
  doc "Create a unidirectional coder" $
  "m" ~>
  Compute.coder
    (var "m")
    (constant (Flows.fail (string "inbound mapping is unsupported")))
