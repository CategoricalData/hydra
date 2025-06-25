module Hydra.Sources.Tier1.Functions where

-- Standard term-level Tier-1 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import           Hydra.Dsl.Phantoms      as Phantoms
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
    Just "Various general-purpose helper functions."
  where
    elements = [
      el idDef,
      el optionalToListDef]

idDef :: TElement (a -> a)
idDef = functionsDefinition "id" $
  doc "The identity function" $
  lambda "any_" $ var "any_"

optionalToListDef :: TElement (Maybe a -> [a])
optionalToListDef = functionsDefinition "optionalToList" $
  doc "Converts an optional value either to an empty list (if nothing) or a singleton list (if just)." $
  lambda "mx" $ Optionals.maybe (list []) (primitive _lists_pure) (var "mx")
