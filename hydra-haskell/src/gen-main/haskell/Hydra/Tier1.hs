-- | A module for all tier-1 functions and constants. These are generated functions and constants which DSL functions and the implementations of primitive functions are allowed to depend upon. Higher tiers of generated code may not be depended upon, as these tiers may themselves need to depend on DSL functions or primitive functions.

module Hydra.Tier1 where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import Data.Int
import Data.List
import Data.Map
import Data.Set

ignoredVariable :: String
ignoredVariable = "_"

-- | A placeholder name for row types as they are being constructed
placeholderName :: Core.Name
placeholderName = (Core.Name "Placeholder")

skipAnnotations :: ((x -> Maybe (Core.Annotated x a)) -> x -> x)
skipAnnotations getAnn t =  
  let skip = (\t1 -> (\x -> case x of
          Nothing -> t1
          Just v -> (skip (Core.annotatedSubject v))) (getAnn t1))
  in (skip t)

-- | Strip all annotations from a term
stripTerm :: (Core.Term a -> Core.Term a)
stripTerm x = (skipAnnotations (\x -> case x of
  Core.TermAnnotated v -> (Just v)
  _ -> Nothing) x)

-- | Strip all annotations from a type
stripType :: (Core.Type a -> Core.Type a)
stripType x = (skipAnnotations (\x -> case x of
  Core.TypeAnnotated v -> (Just v)
  _ -> Nothing) x)

-- | Convert a qualified name to a dot-separated name
unqualifyName :: (Module.QualifiedName -> Core.Name)
unqualifyName qname =  
  let prefix = ((\x -> case x of
          Nothing -> ""
          Just v -> (Strings.cat [
            Module.unNamespace v,
            "."])) (Module.qualifiedNameNamespace qname))
  in (Core.Name (Strings.cat [
    prefix,
    (Module.qualifiedNameLocal qname)]))

emptyTrace :: Compute.Trace
emptyTrace = Compute.Trace {
  Compute.traceStack = [],
  Compute.traceMessages = [],
  Compute.traceOther = Maps.empty}

-- | Check whether a flow succeeds
flowSucceeds :: (s -> Compute.Flow s a -> Bool)
flowSucceeds cx f = (Optionals.isJust (Compute.flowStateValue (Compute.unFlow f cx emptyTrace)))

-- | Get the value of a flow, or a default value if the flow fails
fromFlow :: (a -> s -> Compute.Flow s a -> a)
fromFlow def cx f = ((\x -> case x of
  Nothing -> def
  Just v -> v) (Compute.flowStateValue (Compute.unFlow f cx emptyTrace)))

maxTraceDepth :: Int
maxTraceDepth = 50
