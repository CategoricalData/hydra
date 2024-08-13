-- | Several functions for stripping annotations from types and terms.

module Hydra.Strip where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | Strip all annotations from a term, including first-class type annotations
fullyStripTerm :: (Core.Term -> Core.Term)
fullyStripTerm t = ((\x -> case x of
  Core.TermAnnotated v309 -> (fullyStripTerm (Core.annotatedTermSubject v309))
  Core.TermTyped v310 -> (fullyStripTerm (Core.typedTermTerm v310))
  _ -> t) t)

-- | Strip all annotations from a term
stripTerm :: (Core.Term -> Core.Term)
stripTerm t = ((\x -> case x of
  Core.TermAnnotated v311 -> (stripTerm (Core.annotatedTermSubject v311))
  _ -> t) t)

-- | Strip all annotations from a term
stripType :: (Core.Type -> Core.Type)
stripType t = ((\x -> case x of
  Core.TypeAnnotated v312 -> (stripType (Core.annotatedTypeSubject v312))
  _ -> t) t)

-- | Strip any top-level type lambdas from a type, extracting the (possibly nested) type body
stripTypeParameters :: (Core.Type -> Core.Type)
stripTypeParameters t = ((\x -> case x of
  Core.TypeLambda v313 -> (stripTypeParameters (Core.lambdaTypeBody v313))
  _ -> t) (stripType t))