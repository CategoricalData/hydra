module Hydra.Sources.Tier1.Functions where

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


functionsDefinition :: String -> TTerm a -> TElement a
functionsDefinition = definitionInModule hydraFunctionsModule

hydraFunctionsModule :: Module
hydraFunctionsModule = Module (Namespace "hydra.functions") elements [] [hydraCoreModule] $
    Just "Placeholder for utilities dealing with Hydra functions."
  where
    elements = [
      el idDef]

idDef :: TElement (a -> a)
idDef = functionsDefinition "id" $
  doc "The identity function" $
  function tA tA $
  lambda "x" $ var "x"
