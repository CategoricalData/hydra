module Hydra.Sources.Tier0.Functions where

-- Standard term-level Tier-0 imports
import           Hydra.Dsl.Base          as Base
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Types         as Types
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
import           Hydra.Sources.Core
import           Prelude hiding ((++))
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


functionsDefinition :: String -> TTerm a -> TElement a
functionsDefinition = definitionInModule hydraFunctionsModule

hydraFunctionsModule :: Module
hydraFunctionsModule = Module (Namespace "hydra/functions") elements [] [hydraCoreModule] $
    Just "Utility functions."
  where
   elements = [
     el idDef]

idDef :: TElement (a -> a)
idDef = functionsDefinition "id" $
  doc "The identity function" $
  function aT aT $
  lambda "x" $ var "x"
