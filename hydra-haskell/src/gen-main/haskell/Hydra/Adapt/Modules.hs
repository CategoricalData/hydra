-- Note: this is an automatically generated file. Do not edit.

-- | Entry point for Hydra's adapter (type/term rewriting) framework

module Hydra.Adapt.Modules where

import qualified Hydra.Adapt.Terms as Terms
import qualified Hydra.Adapt.Utils as Utils
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Error as Error
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Given a target language, an encoding function, and a type, adapt and encode the type
adaptTypeToLanguageAndEncode :: (Coders.Language -> (Core.Type -> Either String t0) -> t1 -> Graph.Graph -> Core.Type -> Either String t0)
adaptTypeToLanguageAndEncode lang enc cx g typ =  
  let dflt = (Eithers.bind (adaptTypeToLanguage lang cx g typ) (\adaptedType -> enc adaptedType))
  in ((\x -> case x of
    Core.TypeVariable _ -> (enc typ)
    _ -> dflt) (Rewriting.deannotateType typ))

-- | Given a target language and a source type, find the target type to which the latter will be adapted
adaptTypeToLanguage :: (Coders.Language -> t0 -> Graph.Graph -> Core.Type -> Either String Core.Type)
adaptTypeToLanguage lang cx g typ = (Eithers.map Compute.adapterTarget (languageAdapter lang cx g typ))

-- | Map a Hydra module to a list of type and/or term definitions which have been adapted to the target language
adaptedModuleDefinitions :: (Coders.Language -> Context.Context -> Graph.Graph -> Module.Module -> Either String [Module.Definition])
adaptedModuleDefinitions lang cx graph mod =  
  let els = (Module.moduleElements mod)
  in  
    let adaptersFor = (\types -> Eithers.map (\adapters -> Maps.fromList (Lists.zip types adapters)) (Eithers.mapList (languageAdapter lang cx graph) types))
    in  
      let classify = (\adapters -> \pair ->  
              let el = (Pairs.first pair)
              in  
                let tt = (Pairs.second pair)
                in  
                  let term = (Core.typeApplicationTermBody tt)
                  in  
                    let typ = (Core.typeApplicationTermType tt)
                    in  
                      let name = (Core.bindingName el)
                      in (Logic.ifElse (Annotations.isNativeType el) (Eithers.bind (Eithers.bimap (\e -> Error.unDecodingError e) (\x -> x) (Core_.type_ graph term)) (\coreTyp -> Eithers.bind (adaptTypeToLanguage lang cx graph coreTyp) (\adaptedTyp -> Right (Module.DefinitionType (Module.TypeDefinition {
                        Module.typeDefinitionName = name,
                        Module.typeDefinitionType = adaptedTyp}))))) (Maybes.maybe (Left (Strings.cat2 "no adapter for element " (Core.unName name))) (\adapter -> Eithers.bind (Eithers.bimap (\ic -> Error.unOtherError (Context.inContextObject ic)) (\x -> x) (Compute.coderEncode (Compute.adapterCoder adapter) cx term)) (\adapted -> Right (Module.DefinitionTerm (Module.TermDefinition {
                        Module.termDefinitionName = name,
                        Module.termDefinitionTerm = adapted,
                        Module.termDefinitionType = (Schemas.typeToTypeScheme (Compute.adapterTarget adapter))})))) (Maps.lookup typ adapters))))
      in (Eithers.bind (Eithers.mapList (\_el -> Eithers.bimap (\ic -> Error.unOtherError (Context.inContextObject ic)) (\x -> x) (Schemas.elementAsTypeApplicationTerm cx _el)) els) (\tterms ->  
        let types = (Sets.toList (Sets.fromList (Lists.map (\arg_ -> Rewriting.deannotateType (Core.typeApplicationTermType arg_)) tterms)))
        in (Eithers.bind (adaptersFor types) (\adapters -> Eithers.mapList (classify adapters) (Lists.zip els tterms)))))

-- | Given a target language, a unidirectional last-mile encoding, and a source type, construct a unidirectional adapting coder for terms of that type
constructCoder :: (Coders.Language -> (Context.Context -> Core.Term -> Either (Context.InContext Error.OtherError) t0) -> t1 -> Graph.Graph -> Core.Type -> Either String (Compute.Coder Core.Term t0))
constructCoder lang encodeTerm cx g typ = (Eithers.map (\adapter -> Utils.composeCoders (Compute.adapterCoder adapter) (Utils.unidirectionalCoder encodeTerm)) (languageAdapter lang cx g typ))

-- | Given a target language and a source type, produce an adapter, which rewrites the type and its terms according to the language's constraints
languageAdapter :: (Coders.Language -> t0 -> Graph.Graph -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
languageAdapter lang _cx g typ =  
  let cx0 = Coders.AdapterContext {
          Coders.adapterContextGraph = g,
          Coders.adapterContextLanguage = lang,
          Coders.adapterContextAdapters = Maps.empty}
  in (Terms.termAdapter cx0 typ)

-- | Given a target language, a unidirectional last mile encoding, and an intermediate helper function, transform a given module into a target representation
transformModule :: (Coders.Language -> (Context.Context -> Core.Term -> Either (Context.InContext Error.OtherError) t0) -> (Module.Module -> M.Map Core.Type (Compute.Coder Core.Term t0) -> [(Core.Binding, Core.TypeApplicationTerm)] -> Either String t1) -> Context.Context -> Graph.Graph -> Module.Module -> Either String t1)
transformModule lang encodeTerm createModule cx g mod =  
  let els = (Module.moduleElements mod)
  in (Eithers.bind (Eithers.mapList (\_el -> Eithers.bimap (\ic -> Error.unOtherError (Context.inContextObject ic)) (\x -> x) (Schemas.elementAsTypeApplicationTerm cx _el)) els) (\tterms ->  
    let types = (Lists.nub (Lists.map Core.typeApplicationTermType tterms))
    in (Eithers.bind (Eithers.mapList (constructCoder lang encodeTerm cx g) types) (\cdrs ->  
      let coders = (Maps.fromList (Lists.zip types cdrs))
      in (createModule mod coders (Lists.zip els tterms))))))
