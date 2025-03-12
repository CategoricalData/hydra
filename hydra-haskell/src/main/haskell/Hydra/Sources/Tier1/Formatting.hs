module Hydra.Sources.Tier1.Formatting where

-- Standard term-level Tier-1 imports
import           Hydra.Dsl.Base          as Base
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Io        as Io
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Sources.Tier0.Core
import           Prelude hiding ((++))
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Dsl.Mantle as Mantle
import Hydra.Sources.Tier1.Mantle
import Hydra.Sources.Libraries


formattingDefinition :: String -> TTerm a -> TElement a
formattingDefinition = definitionInModule hydraFormattingModule

hydraFormattingModule :: Module
hydraFormattingModule = Module (Namespace "hydra.formatting") elements [] [hydraMantleModule] $
    Just "String formatting types and functions."
  where
    elements = [
      el capitalizeDef,
      el convertCaseDef,
      el convertCaseCamelToLowerSnakeDef,
      el convertCaseCamelToUpperSnakeDef,
      el convertCasePascalToUpperSnakeDef,
      el decapitalizeDef,
      el mapFirstLetterDef]

capitalizeDef :: TElement (String -> String)
capitalizeDef = formattingDefinition "capitalize" $
  doc "Capitalize the first letter of a string" $
  ref mapFirstLetterDef @@ primitive _strings_toUpper

convertCaseDef :: TElement (CaseConvention -> CaseConvention -> String -> String)
convertCaseDef = formattingDefinition "convertCase" $
  doc "Convert a string from one case convention to another" $
  lambdas ["from", "to", "original"] $ lets [
    "parts">: lets [
      "byCaps">: lets [
        "splitOnUppercase">: lambda "acc" $ lambda "c" $ Lists.concat2
          (Logic.ifElse (Chars.isUpper $ var "c") emptyParts (list []))
          (Lists.cons (Lists.cons (var "c") (Lists.head $ var "acc")) (Lists.tail $ var "acc"))]
        $ Lists.map (primitive _strings_fromList) $ Lists.foldl (var "splitOnUppercase") emptyParts
          $ Lists.reverse $ Strings.toList (ref decapitalizeDef @@ var "original"),
      "byUnderscores">: Strings.splitOn (string "_") $ var "original"]
      $ (match _CaseConvention Nothing [
        _CaseConvention_camel>>: constant $ var "byCaps",
        _CaseConvention_pascal>>: constant $ var "byCaps",
        _CaseConvention_lowerSnake>>: constant $ var "byUnderscores",
        _CaseConvention_upperSnake>>: constant $ var "byUnderscores"]) @@ var "from"]
    $ (match _CaseConvention Nothing [
      _CaseConvention_camel>>: constant $ ref decapitalizeDef @@ (Strings.cat (Lists.map (ref capitalizeDef <.> primitive _strings_toLower) $ var "parts")),
      _CaseConvention_pascal>>: constant $ Strings.cat (Lists.map (ref capitalizeDef <.> primitive _strings_toLower) $ var "parts"),
      _CaseConvention_lowerSnake>>: constant $ Strings.intercalate (string "_") (Lists.map (primitive _strings_toLower) $ var "parts"),
      _CaseConvention_upperSnake>>: constant $ Strings.intercalate (string "_") (Lists.map (primitive _strings_toUpper) $ var "parts")
      ]) @@ var "to"
  where
    emptyParts = list [list []]

convertCaseCamelToLowerSnakeDef :: TElement (String -> String)
convertCaseCamelToLowerSnakeDef = formattingDefinition "convertCaseCamelToLowerSnake" $
  doc "Convert a string from camel case to lower snake case" $
  ref convertCaseDef @@ Mantle.caseConventionCamel @@ Mantle.caseConventionLowerSnake

convertCaseCamelToUpperSnakeDef :: TElement (String -> String)
convertCaseCamelToUpperSnakeDef = formattingDefinition "convertCaseCamelToUpperSnake" $
  doc "Convert a string from camel case to upper snake case" $
  ref convertCaseDef @@ Mantle.caseConventionCamel @@ Mantle.caseConventionUpperSnake

convertCasePascalToUpperSnakeDef :: TElement (String -> String)
convertCasePascalToUpperSnakeDef = formattingDefinition "convertCasePascalToUpperSnake" $
  doc "Convert a string from pascal case to upper snake case" $
  ref convertCaseDef @@ Mantle.caseConventionPascal @@ Mantle.caseConventionUpperSnake

decapitalizeDef :: TElement (String -> String)
decapitalizeDef = formattingDefinition "decapitalize" $
  doc "Decapitalize the first letter of a string" $
  ref mapFirstLetterDef @@ primitive _strings_toLower

-- TODO: simplify this helper
mapFirstLetterDef :: TElement ((String -> String) -> String -> String)
mapFirstLetterDef = formattingDefinition "mapFirstLetter" $
  doc "A helper which maps the first letter of a string to another string" $
  lambda "mapping" $ lambda "s" $ lets [
    "firstLetter">: var "mapping" @@ (Strings.fromList (Lists.pure (Lists.head $ var "list"))),
    "list">: typed (tList tInt32) $ Strings.toList $ var "s"]
    $ Logic.ifElse
        (Strings.isEmpty $ var "s")
        (var "s")
        (Strings.cat2 (var "firstLetter") (Strings.fromList (Lists.tail $ var "list")))
