{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.GrammarToModule where

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
import qualified Hydra.Sources.Tier1.Literals     as Literals
import qualified Hydra.Sources.Tier1.Strip        as Strip
import           Prelude hiding ((++))
import qualified Data.Int                  as I
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

-- Uncomment tier-2 sources as needed
--import qualified Hydra.Sources.Tier2.AdapterUtils as AdapterUtils
--import qualified Hydra.Sources.Tier2.Adapters as Adapters
import qualified Hydra.Sources.Tier2.Annotations as Annotations
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
import qualified Hydra.Sources.Tier2.Qnames as Qnames
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
--import qualified Hydra.Sources.Tier2.Variants as Variants

import Hydra.Grammar as G
import qualified Hydra.Dsl.Grammar as Grammar


grammarToModuleDefinition :: String -> TTerm a -> TElement a
grammarToModuleDefinition = definitionInModule hydraGrammarToModuleModule

hydraGrammarToModuleModule :: Module
hydraGrammarToModuleModule = Module (Namespace "hydra.grammarToModule") elements
    [Annotations.hydraAnnotationsModule, Formatting.hydraFormattingModule, Qnames.hydraQnamesModule]
    [Tier1.hydraGrammarModule, Tier1.hydraComputeModule, Tier1.hydraGraphModule, Tier1.hydraMantleModule, Tier1.hydraModuleModule] $
    Just ("A utility for converting a BNF grammar to a Hydra module.")
  where
   elements = [
     el childNameDef,
     el findNamesDef,
     el grammarToModuleDef,
     el isComplexDef,
     el isNontrivialDef,
     el makeElementsDef,
     el rawNameDef,
     el simplifyDef,
     el toNameDef,
     el wrapTypeDef]

childNameDef :: TElement (String -> String -> String)
childNameDef = grammarToModuleDefinition "childName" $
  doc "Generate child name" $
  lambda "lname" $ lambda "n" $
    Strings.cat $ list [var "lname", string "_", ref Formatting.capitalizeDef @@ var "n"]

findNamesDef :: TElement ([G.Pattern] -> [String])
findNamesDef = grammarToModuleDefinition "findNames" $
  doc "Find unique names for patterns" $
  lambda "pats" $ lets [
    "nextName">: lambda "acc" $ lambda "pat" $ lets [
      "names">: first $ var "acc",
      "nameMap">: second $ var "acc",
      "rn">: ref rawNameDef @@ var "pat",
      "nameAndIndex">: Optionals.maybe
        (pair (var "rn") (int32 1))
        (lambda "i" $ pair (Strings.cat2 (var "rn") (Literals.showInt32 $ Math.add (var "i") (int32 1))) (Math.add (var "i") (int32 1)))
        (Maps.lookup (var "rn") (var "nameMap")),
      "nn">: first $ var "nameAndIndex",
      "ni">: second $ var "nameAndIndex"]
      $ pair
        (Lists.cons (var "nn") (var "names"))
        (Maps.insert (var "rn") (var "ni") (var "nameMap"))]
    $ Lists.reverse $ first $ Lists.foldl (var "nextName") (pair (list []) Maps.empty) (var "pats")

grammarToModuleDef :: TElement (Namespace -> G.Grammar -> Maybe String -> Module)
grammarToModuleDef = grammarToModuleDefinition "grammarToModule" $
  doc "Convert a BNF grammar to a Hydra module" $
  lambda "ns" $ lambda "grammar" $ lambda "desc" $ lets [
    "prodPairs">: Lists.map
      (lambda "prod" $ pair
        (Grammar.unSymbol $ Grammar.productionSymbol $ var "prod")
        (Grammar.productionPattern $ var "prod"))
      (Grammar.unGrammar $ var "grammar"),
    "capitalizedNames">: Lists.map (lambda "pair" $ ref Formatting.capitalizeDef @@ (first $ var "pair")) (var "prodPairs"),
    "patterns">: Lists.map (lambda "pair" $ second $ var "pair") (var "prodPairs"),
    "elementPairs">: Lists.concat $ Lists.zipWith
      (ref makeElementsDef @@ false @@ var "ns")
      (var "capitalizedNames")
      (var "patterns"),
    "elements">: Lists.map
      (lambda "pair" $ lets [
        "lname">: first $ var "pair",
        "typ">: ref wrapTypeDef @@ (second $ var "pair")]
        $ ref Annotations.typeElementDef @@ (ref toNameDef @@ var "ns" @@ var "lname") @@ var "typ")
      (var "elementPairs")]
    $ Module.module_ (var "ns") (var "elements") (list []) (list []) (var "desc")

isComplexDef :: TElement (G.Pattern -> Bool)
isComplexDef = grammarToModuleDefinition "isComplex" $
  doc "Check if pattern is complex" $
  lambda "pat" $ match G._Pattern (Just false) [
    _Pattern_labeled>>: lambda "lp" $ ref isComplexDef @@ (Grammar.labeledPatternPattern $ var "lp"),
    _Pattern_sequence>>: lambda "pats" $ ref isNontrivialDef @@ true @@ var "pats",
    _Pattern_alternatives>>: lambda "pats" $ ref isNontrivialDef @@ false @@ var "pats"]
  @@ var "pat"

isNontrivialDef :: TElement (Bool -> [G.Pattern] -> Bool)
isNontrivialDef = grammarToModuleDefinition "isNontrivial" $
  doc "Check if patterns are nontrivial" $
  lambda "isRecord" $ lambda "pats" $ lets [
    "minPats">: ref simplifyDef @@ var "isRecord" @@ var "pats"]
    $ Logic.ifElse (Equality.equalInt32 (Lists.length $ var "minPats") (int32 1))
        (match G._Pattern (Just false) [
          _Pattern_labeled>>: constant true] @@ Lists.head (var "minPats"))
        true

makeElementsDef :: TElement (Bool -> Namespace -> String -> G.Pattern -> [(String, Type)])
makeElementsDef = grammarToModuleDefinition "makeElements" $
  doc "Create elements from pattern" $
  lambda "omitTrivial" $ lambda "ns" $ lambda "lname" $ lambda "pat" $ lets [
    "trivial">: Logic.ifElse (var "omitTrivial") (list []) (list [pair (var "lname") TTypes.unit]),

    "forRecordOrUnion">: lambda "isRecord" $ lambda "construct" $ lambda "pats" $ lets [
      "minPats">: ref simplifyDef @@ var "isRecord" @@ var "pats",
      "fieldNames">: ref findNamesDef @@ var "minPats",
      "toField">: lambda "n" $ lambda "p" $ var "descend" @@ var "n" @@
        (lambda "pairs" $ pair (Core.fieldType (Core.name $ var "n") (second $ Lists.head $ var "pairs")) (Lists.tail $ var "pairs")) @@
        var "p",
      "fieldPairs">: Lists.zipWith (var "toField") (var "fieldNames") (var "minPats"),
      "fields">: Lists.map (unaryFunction first) (var "fieldPairs"),
      "els">: Lists.concat $ Lists.map (unaryFunction second) (var "fieldPairs")]
      $ Logic.ifElse (ref isNontrivialDef @@ var "isRecord" @@ var "pats")
          (Lists.cons (pair (var "lname") (var "construct" @@ var "fields")) (var "els"))
          (var "forPat" @@ (Lists.head $ var "minPats")),

    "mod">: lambda "n" $ lambda "f" $ lambda "p" $ var "descend" @@ var "n" @@
      (lambda "pairs" $ Lists.cons (pair (var "lname") (var "f" @@ (second $ Lists.head $ var "pairs"))) (Lists.tail $ var "pairs")) @@
      var "p",

    "descend">: lambda "n" $ lambda "f" $ lambda "p" $ lets [
      "cpairs">: ref makeElementsDef @@ false @@ var "ns" @@ (ref childNameDef @@ var "lname" @@ var "n") @@ var "p"]
      $ var "f" @@ Logic.ifElse (ref isComplexDef @@ var "p")
          (Lists.cons (pair (var "lname") (Core.typeVariable $ ref toNameDef @@ var "ns" @@ (first $ Lists.head $ var "cpairs"))) (var "cpairs"))
          (Logic.ifElse (Lists.null $ var "cpairs")
            (list [pair (var "lname") TTypes.unit])
            (Lists.cons (pair (var "lname") (second $ Lists.head $ var "cpairs")) (Lists.tail $ var "cpairs"))),

    "forPat">: lambda "pat" $ match G._Pattern Nothing [
      _Pattern_alternatives>>: lambda "pats" $ var "forRecordOrUnion" @@ false @@
        (lambda "fields" $ Core.typeUnion $ Core.rowType (ref Constants.placeholderNameDef) (var "fields")) @@ var "pats",
      _Pattern_constant>>: constant $ var "trivial",
      _Pattern_ignored>>: constant $ list [],
      _Pattern_labeled>>: lambda "lp" $ var "forPat" @@ Grammar.labeledPatternPattern (var "lp"),
      _Pattern_nil>>: constant $ var "trivial",
      _Pattern_nonterminal>>: lambda "s" $ list [pair (var "lname") $ Core.typeVariable $
        ref toNameDef @@ var "ns" @@ Grammar.unSymbol (var "s")],
      _Pattern_option>>: lambda "p" $ var "mod" @@ string "Option" @@ (unaryFunction TTypes.optional) @@ var "p",
      _Pattern_plus>>: lambda "p" $ var "mod" @@ string "Elmt" @@ (unaryFunction TTypes.list) @@ var "p",
      _Pattern_regex>>: constant $ list [pair (var "lname") TTypes.string],
      _Pattern_sequence>>: lambda "pats" $ var "forRecordOrUnion" @@ true @@
        (lambda "fields" $ Core.typeRecord $ Core.rowType (ref Constants.placeholderNameDef) (var "fields")) @@ var "pats",
      _Pattern_star>>: lambda "p" $ var "mod" @@ string "Elmt" @@ (unaryFunction TTypes.list) @@ var "p"]
    @@ var "pat"]
    $ var "forPat" @@ var "pat"

rawNameDef :: TElement (G.Pattern -> String)
rawNameDef = grammarToModuleDefinition "rawName" $
  doc "Get raw name from pattern" $
  lambda "pat" $ match G._Pattern Nothing [
    _Pattern_alternatives>>: constant $ string "alts",
    _Pattern_constant>>: lambda "c" $ ref Formatting.capitalizeDef @@ (ref Formatting.withCharacterAliasesDef @@ (Grammar.unConstant $ var "c")),
    _Pattern_ignored>>: constant $ string "ignored",
    _Pattern_labeled>>: lambda "lp" $ Grammar.unLabel $ Grammar.labeledPatternLabel $ var "lp",
    _Pattern_nil>>: constant $ string "none",
    _Pattern_nonterminal>>: lambda "s" $ ref Formatting.capitalizeDef @@ (Grammar.unSymbol $ var "s"),
    _Pattern_option>>: lambda "p" $ ref Formatting.capitalizeDef @@ (ref rawNameDef @@ var "p"),
    _Pattern_plus>>: lambda "p" $ Strings.cat2 (string "listOf") (ref Formatting.capitalizeDef @@ (ref rawNameDef @@ var "p")),
    _Pattern_regex>>: constant $ string "regex",
    _Pattern_sequence>>: constant $ string "sequence",
    _Pattern_star>>: lambda "p" $ Strings.cat2 (string "listOf") (ref Formatting.capitalizeDef @@ (ref rawNameDef @@ var "p"))]
  @@ var "pat"

simplifyDef :: TElement (Bool -> [G.Pattern] -> [G.Pattern])
simplifyDef = grammarToModuleDefinition "simplify" $
  doc "Remove trivial patterns from records" $
  lambda "isRecord" $ lambda "pats" $ lets [
    "isConstant">: lambda "p" $ match G._Pattern (Just false) [
      G._Pattern_constant>>: constant true] @@ var "p"]
    $ Logic.ifElse (var "isRecord")
        (Lists.filter (lambda "p" $ Logic.not $ var "isConstant" @@ var "p") (var "pats"))
        (var "pats")

toNameDef :: TElement (Namespace -> String -> Name)
toNameDef = grammarToModuleDefinition "toName" $
  doc "Convert local name to qualified name" $
  lambda "ns" $ lambda "local" $
    ref Qnames.unqualifyNameDef @@ (Module.qualifiedName (just $ var "ns") (var "local"))

wrapTypeDef :: TElement (Type -> Type)
wrapTypeDef = grammarToModuleDefinition "wrapType" $
  doc "Wrap a type in a placeholder name, unless it is already a wrapper, record, or union type" $
  lambda "t" $ cases _Type (var "t") (Just $ Core.typeWrap $ Core.wrappedType (Core.nameLift placeholderName) $ var "t") [
    _Type_record>>: constant $ var "t",
    _Type_union>>: constant $ var "t",
    _Type_wrap>>: constant $ var "t"]
