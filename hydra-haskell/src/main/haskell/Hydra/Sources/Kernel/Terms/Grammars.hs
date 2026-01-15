
module Hydra.Sources.Kernel.Terms.Grammars where

-- Standard imports for kernel terms modules
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

import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Constants as Constants
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names as Names

import Hydra.Grammar as G


ns :: Namespace
ns = Namespace "hydra.grammars"

module_ :: Module
module_ = Module ns elements
    [Annotations.ns, Formatting.ns, Names.ns]
    kernelTypesNamespaces $
    Just ("A utility for converting a BNF grammar to a Hydra module.")
  where
   elements = [
     toBinding childName,
     toBinding findNames,
     toBinding grammarToModule,
     toBinding isComplex,
     toBinding isNontrivial,
     toBinding makeElements,
     toBinding rawName,
     toBinding simplify,
     toBinding toName,
     toBinding wrapType]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

childName :: TBinding (String -> String -> String)
childName = define "childName" $
  doc "Generate child name" $
  "lname" ~> "n" ~>
  Strings.cat (list [var "lname", string "_", Formatting.capitalize @@ var "n"])

findNames :: TBinding ([G.Pattern] -> [String])
findNames = define "findNames" $
  doc "Find unique names for patterns" $
  "pats" ~>
  "nextName" <~ ("acc" ~> "pat" ~>
    "names" <~ Pairs.first (var "acc") $
    "nameMap" <~ Pairs.second (var "acc") $
    "rn" <~ rawName @@ var "pat" $
    "nameAndIndex" <~ Maybes.maybe
      (pair (var "rn") (int32 1))
      ("i" ~> pair (Strings.cat2 (var "rn") (Literals.showInt32 (Math.add (var "i") (int32 1)))) (Math.add (var "i") (int32 1)))
      (Maps.lookup (var "rn") (var "nameMap")) $
    "nn" <~ Pairs.first (var "nameAndIndex") $
    "ni" <~ Pairs.second (var "nameAndIndex") $
    pair
      (Lists.cons (var "nn") (var "names"))
      (Maps.insert (var "rn") (var "ni") (var "nameMap"))) $
  Lists.reverse (Pairs.first (Lists.foldl (var "nextName") (pair (list ([] :: [TTerm Name])) Maps.empty) (var "pats")))

grammarToModule :: TBinding (Namespace -> G.Grammar -> Maybe String -> Module)
grammarToModule = define "grammarToModule" $
  doc "Convert a BNF grammar to a Hydra module" $
  "ns" ~> "grammar" ~> "desc" ~>
  "prodPairs" <~ Lists.map
    ("prod" ~> pair
      (Grammar.unSymbol (Grammar.productionSymbol (var "prod")))
      (Grammar.productionPattern (var "prod")))
    (Grammar.unGrammar (var "grammar")) $
  "capitalizedNames" <~ Lists.map ("pair" ~> Formatting.capitalize @@ (Pairs.first (var "pair"))) (var "prodPairs") $
  "patterns" <~ Lists.map ("pair" ~> Pairs.second (var "pair")) (var "prodPairs") $
  "elementPairs" <~ Lists.concat (Lists.zipWith
    (makeElements @@ false @@ var "ns")
    (var "capitalizedNames")
    (var "patterns")) $
  "elements" <~ Lists.map
    ("pair" ~>
      "lname" <~ Pairs.first (var "pair") $
      "typ" <~ wrapType @@ (Pairs.second (var "pair")) $
      Annotations.typeElement @@ (toName @@ var "ns" @@ var "lname") @@ var "typ")
    (var "elementPairs") $
  Module.module_ (var "ns") (var "elements") (list ([] :: [TTerm Namespace])) (list ([] :: [TTerm Namespace])) (var "desc")

isComplex :: TBinding (G.Pattern -> Bool)
isComplex = define "isComplex" $
  doc "Check if pattern is complex" $
  "pat" ~> match G._Pattern (Just false) [
    _Pattern_labeled>>: "lp" ~> isComplex @@ (Grammar.labeledPatternPattern (var "lp")),
    _Pattern_sequence>>: "pats" ~> isNontrivial @@ true @@ var "pats",
    _Pattern_alternatives>>: "pats" ~> isNontrivial @@ false @@ var "pats"]
  @@ var "pat"

isNontrivial :: TBinding (Bool -> [G.Pattern] -> Bool)
isNontrivial = define "isNontrivial" $
  doc "Check if patterns are nontrivial" $
  "isRecord" ~> "pats" ~>
  "minPats" <~ simplify @@ var "isRecord" @@ var "pats" $
  "isLabeled" <~ ("p" ~> cases G._Pattern (var "p")
    (Just false) [
    _Pattern_labeled>>: constant true]) $
  Logic.ifElse (Equality.equal (Lists.length (var "minPats")) (int32 1))
    (var "isLabeled" @@ (Lists.head $ var "minPats"))
    true

makeElements :: TBinding (Bool -> Namespace -> String -> G.Pattern -> [(String, Type)])
makeElements = define "makeElements" $
  doc "Create elements from pattern" $
  "omitTrivial" ~> "ns" ~> "lname" ~> "pat" ~>
  "trivial" <~ Logic.ifElse (var "omitTrivial") (list ([] :: [TTerm (String, Type)])) (list [pair (var "lname") MetaTypes.unit]) $

  "descend" <~ ("n" ~> "f" ~> "p" ~>
    "cpairs" <~ makeElements @@ false @@ var "ns" @@ (childName @@ var "lname" @@ var "n") @@ var "p" $
    var "f" @@ Logic.ifElse (isComplex @@ var "p")
      (Lists.cons (pair (var "lname") (Core.typeVariable (toName @@ var "ns" @@ (Pairs.first (Lists.head (var "cpairs")))))) (var "cpairs"))
      (Logic.ifElse (Lists.null (var "cpairs"))
        (list [pair (var "lname") MetaTypes.unit])
        (Lists.cons (pair (var "lname") (Pairs.second (Lists.head (var "cpairs")))) (Lists.tail (var "cpairs"))))) $

  "mod" <~ ("n" ~> "f" ~> "p" ~> var "descend" @@ var "n" @@
    ("pairs" ~> Lists.cons (pair (var "lname") (var "f" @@ (Pairs.second (Lists.head (var "pairs"))))) (Lists.tail (var "pairs"))) @@
    var "p") $

  lets [
    "forPat">: ("pat" ~> match G._Pattern Nothing [
      _Pattern_alternatives>>: "pats" ~> var "forRecordOrUnion" @@ false @@
        ("fields" ~> Core.typeUnion (Core.rowType (Constants.placeholderName) (var "fields"))) @@ var "pats",
      _Pattern_constant>>: constant (var "trivial"),
      _Pattern_ignored>>: constant (list ([] :: [TTerm (String, Type)])),
      _Pattern_labeled>>: "lp" ~> var "forPat" @@ Grammar.labeledPatternPattern (var "lp"),
      _Pattern_nil>>: constant (var "trivial"),
      _Pattern_nonterminal>>: "s" ~> list [pair (var "lname") (Core.typeVariable (
        toName @@ var "ns" @@ Grammar.unSymbol (var "s")))],
      _Pattern_option>>: "p" ~> var "mod" @@ string "Option" @@ (unaryFunction MetaTypes.optional) @@ var "p",
      _Pattern_plus>>: "p" ~> var "mod" @@ string "Elmt" @@ (unaryFunction MetaTypes.list) @@ var "p",
      _Pattern_regex>>: constant (list [pair (var "lname") MetaTypes.string]),
      _Pattern_sequence>>: "pats" ~> var "forRecordOrUnion" @@ true @@
        ("fields" ~> Core.typeRecord (Core.rowType (Constants.placeholderName) (var "fields"))) @@ var "pats",
      _Pattern_star>>: "p" ~> var "mod" @@ string "Elmt" @@ (unaryFunction MetaTypes.list) @@ var "p"]
    @@ var "pat"),

    "forRecordOrUnion">: ("isRecord" ~> "construct" ~> "pats" ~>
      "minPats" <~ simplify @@ var "isRecord" @@ var "pats" $
      "fieldNames" <~ findNames @@ var "minPats" $
      "toField" <~ ("n" ~> "p" ~> var "descend" @@ var "n" @@
        ("pairs" ~> pair (Core.fieldType (Core.name (var "n")) (Pairs.second (Lists.head (var "pairs")))) (Lists.tail (var "pairs"))) @@
        var "p") $
      "fieldPairs" <~ Lists.zipWith (var "toField") (var "fieldNames") (var "minPats") $
      "fields" <~ Lists.map (unaryFunction Pairs.first) (var "fieldPairs") $
      "els" <~ Lists.concat (Lists.map (unaryFunction Pairs.second) (var "fieldPairs")) $
      Logic.ifElse (isNontrivial @@ var "isRecord" @@ var "pats")
        (Lists.cons (pair (var "lname") (var "construct" @@ var "fields")) (var "els"))
        (var "forPat" @@ (Lists.head (var "minPats"))))] $

  var "forPat" @@ var "pat"

rawName :: TBinding (G.Pattern -> String)
rawName = define "rawName" $
  doc "Get raw name from pattern" $
  "pat" ~> match G._Pattern Nothing [
    _Pattern_alternatives>>: constant (string "alts"),
    _Pattern_constant>>: "c" ~> Formatting.capitalize @@ (Formatting.withCharacterAliases @@ (Grammar.unConstant (var "c"))),
    _Pattern_ignored>>: constant (string "ignored"),
    _Pattern_labeled>>: "lp" ~> Grammar.unLabel (Grammar.labeledPatternLabel (var "lp")),
    _Pattern_nil>>: constant (string "none"),
    _Pattern_nonterminal>>: "s" ~> Formatting.capitalize @@ (Grammar.unSymbol (var "s")),
    _Pattern_option>>: "p" ~> Formatting.capitalize @@ (rawName @@ var "p"),
    _Pattern_plus>>: "p" ~> Strings.cat2 (string "listOf") (Formatting.capitalize @@ (rawName @@ var "p")),
    _Pattern_regex>>: constant (string "regex"),
    _Pattern_sequence>>: constant (string "sequence"),
    _Pattern_star>>: "p" ~> Strings.cat2 (string "listOf") (Formatting.capitalize @@ (rawName @@ var "p"))]
  @@ var "pat"

simplify :: TBinding (Bool -> [G.Pattern] -> [G.Pattern])
simplify = define "simplify" $
  doc "Remove trivial patterns from records" $
  "isRecord" ~> "pats" ~>
  "isConstant" <~ ("p" ~> match G._Pattern (Just false) [
    G._Pattern_constant>>: constant true] @@ var "p") $
  Logic.ifElse (var "isRecord")
    (Lists.filter ("p" ~> Logic.not (var "isConstant" @@ var "p")) (var "pats"))
    (var "pats")

toName :: TBinding (Namespace -> String -> Name)
toName = define "toName" $
  doc "Convert local name to qualified name" $
  "ns" ~> "local" ~>
  Names.unqualifyName @@ (Module.qualifiedName (just (var "ns")) (var "local"))

wrapType :: TBinding (Type -> Type)
wrapType = define "wrapType" $
  doc "Wrap a type in a placeholder name, unless it is already a wrapper, record, or union type" $
  "t" ~> cases _Type (var "t")
    (Just (Core.typeWrap (Core.wrappedType (Core.nameLift placeholderName) (var "t")))) [
    _Type_record>>: constant (var "t"),
    _Type_union>>: constant (var "t"),
    _Type_wrap>>: constant (var "t")]
