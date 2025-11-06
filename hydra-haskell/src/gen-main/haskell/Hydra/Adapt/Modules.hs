-- | Entry point for Hydra's adapter (type/term rewriting) framework

module Hydra.Adapt.Modules where

import qualified Hydra.Adapt.Terms as Terms
import qualified Hydra.Adapt.Utils as Utils
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Describe.Core as Core__
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Monads as Monads
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

adaptTypeToLanguageAndEncode :: (Coders.Language -> (Core.Type -> Compute.Flow Graph.Graph t0) -> Core.Type -> Compute.Flow Graph.Graph t0)
adaptTypeToLanguageAndEncode lang enc typ =  
  let dflt = (Flows.bind (adaptTypeToLanguage lang typ) (\adaptedType -> enc adaptedType))
  in ((\x -> case x of
    Core.TypeVariable _ -> (enc typ)
    _ -> dflt) (Rewriting.deannotateType typ))

adaptTypeToLanguage :: (Coders.Language -> Core.Type -> Compute.Flow Graph.Graph Core.Type)
adaptTypeToLanguage lang typ = (Flows.bind (languageAdapter lang typ) (\adapter -> Flows.pure (Compute.adapterTarget adapter)))

adaptedModuleDefinitions :: (Coders.Language -> Module.Module -> Compute.Flow Graph.Graph [Module.Definition])
adaptedModuleDefinitions lang mod =  
  let els = (Module.moduleElements mod)
  in  
    let adaptersFor = (\types -> Flows.bind (Flows.mapList (languageAdapter lang) types) (\adapters -> Flows.pure (Maps.fromList (Lists.zip types adapters))))
    in  
      let classify = (\adapters -> \tuple2 ->  
              let el = (fst tuple2)
              in  
                let tt = (snd tuple2)
                in  
                  let term = (Core.typeApplicationTermBody tt)
                  in  
                    let typ = (Core.typeApplicationTermType tt)
                    in  
                      let name = (Core.bindingName el)
                      in (Logic.ifElse (Annotations.isNativeType el) (Flows.bind (Flows.bind (Core_.type_ term) (\coreTyp -> adaptTypeToLanguage lang coreTyp)) (\adaptedTyp -> Flows.pure (Module.DefinitionType (Module.TypeDefinition {
                        Module.typeDefinitionName = name,
                        Module.typeDefinitionType = adaptedTyp})))) (Maybes.maybe (Flows.fail (Strings.cat2 "no adapter for element " (Core.unName name))) (\adapter -> Flows.bind (Compute.coderEncode (Compute.adapterCoder adapter) term) (\adapted -> Flows.pure (Module.DefinitionTerm (Module.TermDefinition {
                        Module.termDefinitionName = name,
                        Module.termDefinitionTerm = adapted,
                        Module.termDefinitionType = (Compute.adapterTarget adapter)})))) (Maps.lookup typ adapters))))
      in (Flows.bind (Lexical.withSchemaContext (Flows.mapList Schemas.elementAsTypeApplicationTerm els)) (\tterms ->  
        let types = (Sets.toList (Sets.fromList (Lists.map (\arg_ -> Rewriting.deannotateType (Core.typeApplicationTermType arg_)) tterms)))
        in (Flows.bind (adaptersFor types) (\adapters -> Flows.mapList (classify adapters) (Lists.zip els tterms)))))

constructCoder :: (Coders.Language -> (Core.Term -> Compute.Flow t0 t1) -> Core.Type -> Compute.Flow Graph.Graph (Compute.Coder t0 t2 Core.Term t1))
constructCoder lang encodeTerm typ = (Monads.withTrace (Strings.cat2 "coder for " (Core__.type_ typ)) (Flows.bind (languageAdapter lang typ) (\adapter -> Flows.pure (Utils.composeCoders (Compute.adapterCoder adapter) (Utils.unidirectionalCoder encodeTerm)))))

languageAdapter :: (Coders.Language -> Core.Type -> Compute.Flow Graph.Graph (Compute.Adapter t0 t1 Core.Type Core.Type Core.Term Core.Term))
languageAdapter lang typ =  
  let getPair = (\typ -> Flows.bind (Terms.termAdapter typ) (\ad -> Flows.bind Monads.getState (\cx -> Flows.pure (ad, cx))))
  in (Flows.bind Monads.getState (\g ->  
    let cx0 = Coders.AdapterContext {
            Coders.adapterContextGraph = g,
            Coders.adapterContextLanguage = lang,
            Coders.adapterContextAdapters = Maps.empty}
    in (Flows.bind (Monads.withState cx0 (getPair typ)) (\result ->  
      let adapter = (fst result)
      in  
        let cx = (snd result)
        in  
          let encode = (\term -> Monads.withState cx (Compute.coderEncode (Compute.adapterCoder adapter) term))
          in  
            let decode = (\term -> Monads.withState cx (Compute.coderDecode (Compute.adapterCoder adapter) term))
            in (Flows.pure (Compute.Adapter {
              Compute.adapterIsLossy = (Compute.adapterIsLossy adapter),
              Compute.adapterSource = (Compute.adapterSource adapter),
              Compute.adapterTarget = (Compute.adapterTarget adapter),
              Compute.adapterCoder = Compute.Coder {
                Compute.coderEncode = encode,
                Compute.coderDecode = decode}}))))))

transformModule :: (Coders.Language -> (Core.Term -> Compute.Flow t0 t1) -> (Module.Module -> M.Map Core.Type (Compute.Coder t0 t2 Core.Term t1) -> [(Core.Binding, Core.TypeApplicationTerm)] -> Compute.Flow Graph.Graph t3) -> Module.Module -> Compute.Flow Graph.Graph t3)
transformModule lang encodeTerm createModule mod =  
  let els = (Module.moduleElements mod)
  in  
    let transform = (Flows.bind (Lexical.withSchemaContext (Flows.mapList Schemas.elementAsTypeApplicationTerm els)) (\tterms ->  
            let types = (Lists.nub (Lists.map Core.typeApplicationTermType tterms))
            in (Flows.bind (Flows.mapList (constructCoder lang encodeTerm) types) (\cdrs ->  
              let coders = (Maps.fromList (Lists.zip types cdrs))
              in (createModule mod coders (Lists.zip els tterms))))))
    in (Monads.withTrace (Strings.cat2 "transform module " (Module.unNamespace (Module.moduleNamespace mod))) transform)
