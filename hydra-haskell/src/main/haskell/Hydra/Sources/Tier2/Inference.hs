module Hydra.Sources.Tier2.Inference where

-- Standard Tier-2 imports
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Chars       as Chars
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import           Hydra.Dsl.Phantoms        as Phantoms
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier1.All hiding (mapDef)
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

import qualified Hydra.Dsl.Typing as Typing


inferenceDefinition :: String -> TTerm a -> TElement a
inferenceDefinition = definitionInModule hydraInferenceModule

hydraInferenceModule :: Module
hydraInferenceModule = Module (Namespace "hydra.inference") elements [] [hydraTypingModule] $
    Just "Type inference following Algorithm W."
  where
   elements = [
     el emptyInferenceContextDef,
     el normalTypeVariableDef]

emptyInferenceContextDef :: TElement InferenceContext
emptyInferenceContextDef = inferenceDefinition "emptyInferenceContext" $
  doc "An empty inference context" $
  Typing.inferenceContext
    (Phantoms.map M.empty)
    (Phantoms.map M.empty)
    (Phantoms.map M.empty)
    false

normalTypeVariableDef :: TElement (Int -> Name)
normalTypeVariableDef = inferenceDefinition "normalTypeVariable" $
  doc "Type variable naming convention follows Haskell: t0, t1, etc." $
  lambda "i" $ Core.name' (Strings.cat2 (string "t") (Literals.showInt32 @@ var "i"))
