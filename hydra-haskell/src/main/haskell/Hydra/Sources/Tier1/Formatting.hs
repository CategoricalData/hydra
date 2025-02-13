module Hydra.Sources.Tier1.Formatting where

-- Standard term-level Tier-1 imports
import           Hydra.Dsl.Base          as Base
import qualified Hydra.Dsl.Core          as Core
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

-- Mixed Tier-1 imports
import Hydra.Dsl.Bootstrap


formattingDefinition :: String -> TTerm a -> TElement a
formattingDefinition = definitionInModule hydraFormattingModule

hydraFormattingModule :: Module
hydraFormattingModule = Module (Namespace "hydra/formatting") elements [] [hydraCoreModule] $
    Just "String formatting types and functions."
  where
    elements = hydraFormattingTypeDefinitions <> [
      el capitalizeDef,
      el decapitalizeDef,
      el mapFirstLetterDef]

hydraFormattingTypeDefinitions :: [Element]
hydraFormattingTypeDefinitions = [
    caseConventionDef,
    charDef]
  where
    def = datatype $ moduleNamespace hydraFormattingModule
    caseConventionDef = def "CaseConvention" $
      Types.enum ["Camel", "Pascal", "LowerSnake", "UpperSnake"]
    charDef = def "Char" Types.int32

capitalizeDef :: TElement (String -> String)
capitalizeDef = formattingDefinition "capitalize" $
  doc "Capitalize the first letter of a string" $
  function tString tString $
  ref mapFirstLetterDef @@ Strings.toUpper

decapitalizeDef :: TElement (String -> String)
decapitalizeDef = formattingDefinition "decapitalize" $
  doc "Decapitalize the first letter of a string" $
  function tString tString $
  ref mapFirstLetterDef @@ Strings.toLower

-- TODO: simplify this helper
mapFirstLetterDef :: TElement ((String -> String) -> String -> String)
mapFirstLetterDef = formattingDefinition "mapFirstLetter" $
  doc "A helper which maps the first letter of a string to another string" $
  function (tFun tString tString) (tFun tString tString) $
  lambda "mapping" $ lambda "s" ((Logic.ifElse
       @@ var "s"
       @@ (Strings.cat2 @@ var "firstLetter" @@ (Strings.fromList @@ (Lists.tail @@ var "list")))
       @@ (Strings.isEmpty @@ var "s"))
    `with` [
      "firstLetter">: var "mapping" @@ (Strings.fromList @@ (Lists.pure @@ (Lists.head @@ var "list"))),
      "list">: typed (tList tInt32) $ Strings.toList @@ var "s"])

