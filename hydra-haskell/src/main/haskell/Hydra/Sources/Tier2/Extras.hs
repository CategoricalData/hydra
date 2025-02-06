module Hydra.Sources.Tier2.Extras (hydraExtrasModule) where

-- Standard Tier-2 imports
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y
import           Hydra.Dsl.Base            as Base
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier1.All


hydraExtrasDefinition :: String -> TTerm a -> TElement a
hydraExtrasDefinition = definitionInModule hydraExtrasModule

hydraExtrasModule :: Module
hydraExtrasModule = Module (Namespace "hydra/extras") elements
    [hydraGraphModule, hydraMantleModule, hydraComputeModule]
    tier0Modules $
    Just "Basic functions which depend on primitive functions"
  where
    elements = [
      el lookupPrimitiveDef]

lookupPrimitiveDef :: TElement (Graph -> Name -> Maybe Primitive)
lookupPrimitiveDef = hydraExtrasDefinition "lookupPrimitive" $
  function
    graphT
    (Types.function nameT (optionalT primitiveT)) $
  lambda "g" $ lambda "name" $
    apply (Maps.lookup @@ var "name") (Graph.graphPrimitives @@ var "g")
