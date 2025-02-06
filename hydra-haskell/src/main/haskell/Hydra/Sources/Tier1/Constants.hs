module Hydra.Sources.Tier1.Constants where

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


constantsDefinition :: String -> TTerm a -> TElement a
constantsDefinition = definitionInModule hydraConstantsModule

hydraConstantsModule :: Module
hydraConstantsModule = Module (Namespace "hydra/constants") elements [] [hydraCoreModule] $
    Just ("A module for tier-0 constants.")
  where
   elements = [
     el ignoredVariableDef,
     el placeholderNameDef,
     el maxTraceDepthDef]

ignoredVariableDef :: TElement String
ignoredVariableDef = constantsDefinition "ignoredVariable" $
  string "_"

placeholderNameDef :: TElement Name
placeholderNameDef = constantsDefinition "placeholderName" $
  doc "A placeholder name for row types as they are being constructed" $
  wrap _Name $ string "Placeholder"

maxTraceDepthDef :: TElement Int
maxTraceDepthDef = constantsDefinition "maxTraceDepth" $ int32 50
