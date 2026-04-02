-- Note: this is an automatically generated file. Do not edit.

-- | Module dependency namespace analysis

module Hydra.Analysis where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Dependencies as Dependencies
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Predicates as Predicates
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Add names to existing namespaces mapping
addNamesToNamespaces :: (Module.Namespace -> t0) -> S.Set Core.Name -> Module.Namespaces t0 -> Module.Namespaces t0
addNamesToNamespaces encodeNamespace names ns0 =

      let nss = Sets.fromList (Maybes.cat (Lists.map Names.namespaceOf (Sets.toList names)))
          toPair = \ns -> (ns, (encodeNamespace ns))
      in Module.Namespaces {
        Module.namespacesFocus = (Module.namespacesFocus ns0),
        Module.namespacesMapping = (Maps.union (Module.namespacesMapping ns0) (Maps.fromList (Lists.map toPair (Sets.toList nss))))}

-- | Get dependency namespaces from definitions
definitionDependencyNamespaces :: [Module.Definition] -> S.Set Module.Namespace
definitionDependencyNamespaces defs =

      let defNames =
              \def -> case def of
                Module.DefinitionType v0 -> Dependencies.typeDependencyNames True (Module.typeDefinitionType v0)
                Module.DefinitionTerm v0 -> Dependencies.termDependencyNames True True True (Module.termDefinitionTerm v0)
          allNames = Sets.unions (Lists.map defNames defs)
      in (Sets.fromList (Maybes.cat (Lists.map Names.namespaceOf (Sets.toList allNames))))

-- | Find dependency namespaces in all of a set of terms (Either version)
dependencyNamespaces :: Context.Context -> Graph.Graph -> Bool -> Bool -> Bool -> Bool -> [Core.Binding] -> Either (Context.InContext Errors.Error) (S.Set Module.Namespace)
dependencyNamespaces cx graph binds withPrims withNoms withSchema els =

      let depNames =
              \el ->
                let term = Core.bindingTerm el
                    deannotatedTerm = Strip.deannotateTerm term
                    dataNames = Dependencies.termDependencyNames binds withPrims withNoms term
                    schemaNames =
                            Logic.ifElse withSchema (Maybes.maybe Sets.empty (\ts -> Dependencies.typeDependencyNames True (Core.typeSchemeType ts)) (Core.bindingType el)) Sets.empty
                in (Logic.ifElse (Predicates.isEncodedType deannotatedTerm) (Eithers.map (\typ -> Sets.unions [
                  dataNames,
                  schemaNames,
                  (Dependencies.typeDependencyNames True typ)]) (Eithers.bimap (\_wc_e -> Context.InContext {
                  Context.inContextObject = _wc_e,
                  Context.inContextContext = Context.Context {
                    Context.contextTrace = (Lists.cons "dependency namespace (type)" (Context.contextTrace cx)),
                    Context.contextMessages = (Context.contextMessages cx),
                    Context.contextOther = (Context.contextOther cx)}}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _e))) (\_a -> _a) (Core_.type_ graph term)))) (Logic.ifElse (Predicates.isEncodedTerm deannotatedTerm) (Eithers.map (\decodedTerm -> Sets.unions [
                  dataNames,
                  schemaNames,
                  (Dependencies.termDependencyNames binds withPrims withNoms decodedTerm)]) (Eithers.bimap (\_wc_e -> Context.InContext {
                  Context.inContextObject = _wc_e,
                  Context.inContextContext = Context.Context {
                    Context.contextTrace = (Lists.cons "dependency namespace (term)" (Context.contextTrace cx)),
                    Context.contextMessages = (Context.contextMessages cx),
                    Context.contextOther = (Context.contextOther cx)}}) (\_wc_a -> _wc_a) (Eithers.bimap (\_e -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _e))) (\_a -> _a) (Core_.term graph term)))) (Right (Sets.unions [
                  dataNames,
                  schemaNames]))))
      in (Eithers.map (\namesList -> Sets.fromList (Maybes.cat (Lists.map Names.namespaceOf (Sets.toList (Sets.unions namesList))))) (Eithers.mapList depNames els))

-- | Check whether a module contains any binary literal values
moduleContainsBinaryLiterals :: Module.Module -> Bool
moduleContainsBinaryLiterals mod =

      let checkTerm =
              \found -> \term -> Logic.or found (case term of
                Core.TermLiteral v0 -> case v0 of
                  Core.LiteralBinary _ -> True
                  _ -> False
                _ -> False)
          termContainsBinary = \term -> Rewriting.foldOverTerm Coders.TraversalOrderPre checkTerm False term
          defTerms =
                  Maybes.cat (Lists.map (\d -> case d of
                    Module.DefinitionTerm v0 -> Just (Module.termDefinitionTerm v0)
                    _ -> Nothing) (Module.moduleDefinitions mod))
      in (Lists.foldl (\acc -> \t -> Logic.or acc (termContainsBinary t)) False defTerms)

-- | Find dependency namespaces in all elements of a module, excluding the module's own namespace (Either version)
moduleDependencyNamespaces :: Context.Context -> Graph.Graph -> Bool -> Bool -> Bool -> Bool -> Module.Module -> Either (Context.InContext Errors.Error) (S.Set Module.Namespace)
moduleDependencyNamespaces cx graph binds withPrims withNoms withSchema mod =

      let allBindings =
              Maybes.cat (Lists.map (\d -> case d of
                Module.DefinitionType v0 -> Just (Annotations.typeElement (Module.typeDefinitionName v0) (Module.typeDefinitionType v0))
                Module.DefinitionTerm v0 -> Just (Core.Binding {
                  Core.bindingName = (Module.termDefinitionName v0),
                  Core.bindingTerm = (Module.termDefinitionTerm v0),
                  Core.bindingType = (Module.termDefinitionType v0)})
                _ -> Nothing) (Module.moduleDefinitions mod))
      in (Eithers.map (\deps -> Sets.delete (Module.moduleNamespace mod) deps) (dependencyNamespaces cx graph binds withPrims withNoms withSchema allBindings))

-- | Create namespaces mapping for definitions
namespacesForDefinitions :: (Module.Namespace -> t0) -> Module.Namespace -> [Module.Definition] -> Module.Namespaces t0
namespacesForDefinitions encodeNamespace focusNs defs =

      let nss = Sets.delete focusNs (definitionDependencyNamespaces defs)
          toPair = \ns -> (ns, (encodeNamespace ns))
      in Module.Namespaces {
        Module.namespacesFocus = (toPair focusNs),
        Module.namespacesMapping = (Maps.fromList (Lists.map toPair (Sets.toList nss)))}
