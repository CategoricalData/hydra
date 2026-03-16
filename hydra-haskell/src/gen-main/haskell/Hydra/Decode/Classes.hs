-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.classes

module Hydra.Decode.Classes where

import qualified Hydra.Classes as Classes
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

typeClass :: (Graph.Graph -> Core.Term -> Either Error.DecodingError Classes.TypeClass)
typeClass cx raw = (Eithers.either (\err -> Left (Error.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermUnion v0 ->  
    let field = (Core.injectionField v0) 
        fname = (Core.fieldName field)
        fterm = (Core.fieldTerm field)
        variantMap = (Maps.fromList [
                (Core.Name "equality", (\input -> Eithers.map (\t -> Classes.TypeClassEquality) (Helpers.decodeUnit cx input))),
                (Core.Name "ordering", (\input -> Eithers.map (\t -> Classes.TypeClassOrdering) (Helpers.decodeUnit cx input)))])
    in (Maybes.maybe (Left (Error.DecodingError (Strings.cat [
      "no such field ",
      (Core.unName fname),
      " in union"]))) (\f -> f fterm) (Maps.lookup fname variantMap))
  _ -> (Left (Error.DecodingError "expected union"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
