-- | Several functions for stripping annotations from types and terms.

module Hydra.Strip where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

skipAnnotations :: ((x -> Maybe (Core.Annotated x a)) -> x -> x)
skipAnnotations getAnn t =  
  let skip = (\t1 -> (\x -> case x of
          Nothing -> t1
          Just v71 -> (skip (Core.annotatedSubject v71))) (getAnn t1))
  in (skip t)

-- | Strip all annotations from a term
stripTerm :: (Core.Term a -> Core.Term a)
stripTerm x = (skipAnnotations (\x -> case x of
  Core.TermAnnotated v72 -> (Just v72)
  _ -> Nothing) x)

-- | Strip all annotations from a type
stripType :: (Core.Type a -> Core.Type a)
stripType x = (skipAnnotations (\x -> case x of
  Core.TypeAnnotated v73 -> (Just v73)
  _ -> Nothing) x)

-- | Strip any top-level type lambdas from a type, extracting the (possibly nested) type body
stripTypeParameters :: (Core.Type a -> Core.Type a)
stripTypeParameters t = ((\x -> case x of
  Core.TypeLambda v74 -> (stripTypeParameters (Core.lambdaTypeBody v74))
  _ -> t) (stripType t))