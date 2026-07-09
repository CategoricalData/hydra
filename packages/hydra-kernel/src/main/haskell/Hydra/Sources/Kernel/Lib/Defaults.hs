-- | A module which provides a single term-level map from primitive names to their
-- default (cross-compilable, host-independent) reference implementations.
--
-- This module contains a single binding, defaultImplementations, which maps the
-- Name of each primitive with a declared defaultImplementation (see
-- PrimitiveDefinition.defaultImplementation) to that implementation's Term. Only
-- primitives which actually declare a default implementation are included; the
-- majority of kernel primitives, which rely solely on a per-host native
-- implementation, are omitted.
--
-- By consolidating these terms into a single map, per-host primitive registries
-- can look up and register a default implementation for any primitive without
-- each host needing to duplicate the extraction logic.

module Hydra.Sources.Kernel.Lib.Defaults where

-- Note: non-standard imports; this module is constructed dynamically from other modules.
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Sources.Kernel.Lib.Eithers as Eithers
import qualified Hydra.Sources.Kernel.Lib.Equality as Equality
import qualified Hydra.Sources.Kernel.Lib.Lists as Lists
import qualified Hydra.Sources.Kernel.Lib.Logic as Logic
import qualified Hydra.Sources.Kernel.Lib.Maps as Maps
import qualified Hydra.Sources.Kernel.Lib.Math as Math
import qualified Hydra.Sources.Kernel.Lib.Optionals as Optionals
import qualified Hydra.Sources.Kernel.Lib.Pairs as Pairs
import qualified Hydra.Sources.Kernel.Lib.Sets as Sets

import qualified Data.Map as M
import           Data.Maybe (mapMaybe)


ns :: ModuleName
ns = ModuleName "hydra.lib.defaults"

-- | The primitive-defining hydra.lib.* modules which may contain primitives
-- with declared default implementations.
defaultsSourceModules :: [Module]
defaultsSourceModules = [
  Eithers.module_,
  Equality.module_,
  Lists.module_,
  Logic.module_,
  Maps.module_,
  Math.module_,
  Optionals.module_,
  Pairs.module_,
  Sets.module_]

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = [DefinitionTerm defaultImplementationsDefinition],
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.core", ModuleName "hydra.lib.eithers", ModuleName "hydra.lib.equality", ModuleName "hydra.lib.lists", ModuleName "hydra.lib.logic", ModuleName "hydra.lib.maps", ModuleName "hydra.lib.math", ModuleName "hydra.lib.optionals", ModuleName "hydra.lib.pairs", ModuleName "hydra.lib.sets"],
            moduleMetadata = descriptionMetadata (Just ("A module which provides a single map from primitive names to their"
      ++ " default, cross-compilable reference implementations, for primitives which declare one."))}

defaultImplementationsDefinition :: TermDefinition
defaultImplementationsDefinition = TermDefinition {
    termDefinitionName = Name "hydra.lib.defaults.defaultImplementations",
    termDefinitionMetadata = Nothing,
    termDefinitionBody = defaultImplementationsTerm,
    termDefinitionSignature = Nothing}

-- | Build a Term-level map from Name to Term, by extracting the defaultImplementation
-- field from every PrimitiveDefinition across the defaults source modules. Only
-- primitives with a declared (Just) default implementation are included.
--
-- Each default implementation is a genuinely different-typed lambda term (one may be
-- [a] -> (a -> [b]) -> [b], another may involve Either a b, etc.), so the raw terms
-- cannot be inserted directly as TermMap values: inference on a TermMap unifies all
-- value subterms to a single type, which fails across heterogeneous implementations.
-- Instead, each implementation is reified as term-level data via EncodeCore.term,
-- exactly as typesByNameTerm (Hydra.Sources.Json.Bootstrap) encodes each Type as a
-- uniformly-typed hydra.core.Type term via EncodeCore.type_. This makes every map
-- value uniformly typed as (encoded) hydra.core.Term, sidestepping inference into the
-- implementations' differing executable types.
defaultImplementationsTerm :: Term
defaultImplementationsTerm = TermAnnotated $ AnnotatedTerm {
    annotatedTermBody = TermMap $ M.fromList entries,
    annotatedTermAnnotation = TermMap $ M.fromList [
      (TermVariable (Name "description"), TermLiteral $ LiteralString
        ("A map from primitive name to default (cross-compilable) reference implementation,"
          ++ " for primitives which declare one. Each value is an encoded (reified) term;"
          ++ " decode it to recover the executable implementation."))]}
  where
    entries = mapMaybe defEntry (concatMap moduleDefinitions defaultsSourceModules)
    defEntry def = case def of
      DefinitionPrimitive pd -> case primitiveDefinitionDefaultImplementation pd of
        Just impl -> Just (EncodeCore.name (primitiveDefinitionName pd), EncodeCore.term impl)
        Nothing -> Nothing
      _ -> Nothing
