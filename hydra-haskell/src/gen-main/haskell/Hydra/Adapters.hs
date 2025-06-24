-- | Entry point for Hydra's adapter (type/term rewriting) framework

module Hydra.Adapters where

import qualified Hydra.AdapterUtils as AdapterUtils
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Flows as Flows
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Flows as Flows_
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Describe.Core as DescribeCore
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Strip as Strip
import qualified Hydra.TermAdapters as TermAdapters
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

adaptAndEncodeType :: (Coders.Language -> (Core.Type -> Compute.Flow Graph.Graph t0) -> Core.Type -> Compute.Flow Graph.Graph t0)
adaptAndEncodeType lang enc typ = ((\x -> case x of
  Core.TypeVariable _ -> (enc typ)
  _ -> (Flows_.bind (adaptType lang typ) (\adaptedType -> enc adaptedType))) (Strip.stripType typ))

-- | Given a target language and a source type, find the target type to which the latter will be adapted
adaptType :: (Coders.Language -> Core.Type -> Compute.Flow Graph.Graph Core.Type)
adaptType lang typ = (Flows_.bind (languageAdapter lang typ) (\adapter -> Flows_.pure (Compute.adapterTarget adapter)))

-- | Map a Hydra module to a list of type and/or term definitions which have been adapted to the target language
adaptedModuleDefinitions :: (Coders.Language -> Module.Module -> Compute.Flow Graph.Graph [Module.Definition])
adaptedModuleDefinitions lang mod_ =  
  let els = (Module.moduleElements mod_) 
      adaptersFor = (\types -> Flows_.bind (Flows_.mapList (languageAdapter lang) types) (\adapters -> Flows_.pure (Maps.fromList (Lists.zip types adapters))))
      classify = (\adapters -> \pair ->  
              let el = (fst pair) 
                  tt = (snd pair)
                  term = (Core.typedTermTerm tt)
                  typ = (Core.typedTermType tt)
                  name = (Graph.elementName el)
              in (Logic.ifElse (Annotations.isNativeType el) (Flows_.bind (Flows_.bind (DecodeCore.type_ term) (\coreTyp -> adaptType lang coreTyp)) (\adaptedTyp -> Flows_.pure (Module.DefinitionType (Module.TypeDefinition {
                Module.typeDefinitionName = name,
                Module.typeDefinitionType = adaptedTyp})))) (Optionals.maybe (Flows_.fail (Strings.cat2 "no adapter for element " (Core.unName name))) (\adapter -> Flows_.bind (Compute.coderEncode (Compute.adapterCoder adapter) term) (\adapted -> Flows_.pure (Module.DefinitionTerm (Module.TermDefinition {
                Module.termDefinitionName = name,
                Module.termDefinitionTerm = adapted,
                Module.termDefinitionType = (Compute.adapterTarget adapter)})))) (Maps.lookup typ adapters))))
  in (Flows_.bind (Lexical.withSchemaContext (Flows_.mapList Schemas.elementAsTypedTerm els)) (\tterms ->  
    let types = (Sets.toList (Sets.fromList (Lists.map (\arg_ -> Strip.stripType (Core.typedTermType arg_)) tterms)))
    in (Flows_.bind (adaptersFor types) (\adapters -> Flows_.mapList (classify adapters) (Lists.zip els tterms)))))

constructCoder :: (Coders.Language -> (Core.Term -> Compute.Flow t0 t1) -> Core.Type -> Compute.Flow Graph.Graph (Compute.Coder t0 t2 Core.Term t1))
constructCoder lang encodeTerm typ = (Flows.withTrace (Strings.cat2 "coder for " (DescribeCore.type_ typ)) (Flows_.bind (languageAdapter lang typ) (\adapter -> Flows_.pure (AdapterUtils.composeCoders (Compute.adapterCoder adapter) (AdapterUtils.unidirectionalCoder encodeTerm)))))

languageAdapter :: (Coders.Language -> Core.Type -> Compute.Flow Graph.Graph (Compute.Adapter t0 t1 Core.Type Core.Type Core.Term Core.Term))
languageAdapter lang typ = (Flows_.bind Errors.getState (\g ->  
  let cx0 = Coders.AdapterContext {
          Coders.adapterContextGraph = g,
          Coders.adapterContextLanguage = lang,
          Coders.adapterContextAdapters = Maps.empty}
  in (Flows_.bind (Flows.withState cx0 (Flows_.bind (TermAdapters.termAdapter typ) (\ad -> Flows_.bind Errors.getState (\cx -> Flows_.pure (ad, cx))))) (\result ->  
    let adapter = (fst result) 
        cx = (snd result)
        encode = (\term -> Flows.withState cx (Compute.coderEncode (Compute.adapterCoder adapter) term))
        decode = (\term -> Flows.withState cx (Compute.coderDecode (Compute.adapterCoder adapter) term))
        ac = Compute.Coder {
                Compute.coderEncode = encode,
                Compute.coderDecode = decode}
    in (Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy adapter),
      Compute.adapterSource = (Compute.adapterSource adapter),
      Compute.adapterTarget = (Compute.adapterTarget adapter),
      Compute.adapterCoder = ac}))))))

transformModule :: (Coders.Language -> (Core.Term -> Compute.Flow t0 t1) -> (Module.Module -> M.Map Core.Type (Compute.Coder t0 t3 Core.Term t1) -> [(Graph.Element, Core.TypedTerm)] -> Compute.Flow Graph.Graph t2) -> Module.Module -> Compute.Flow Graph.Graph t2)
transformModule lang encodeTerm createModule mod_ = (Flows.withTrace (Strings.cat2 "transform module " (Module.unNamespace (Module.moduleNamespace mod_))) ( 
  let els = (Module.moduleElements mod_) 
      codersFor = (\types -> Flows_.bind (Flows_.mapList (constructCoder lang encodeTerm) types) (\cdrs -> Flows_.pure (Maps.fromList (Lists.zip types cdrs))))
  in (Flows_.bind (Lexical.withSchemaContext (Flows_.mapList Schemas.elementAsTypedTerm els)) (\tterms ->  
    let types = (Lists.nub (Lists.map Core.typedTermType tterms))
    in (Flows_.bind (codersFor types) (\coders -> createModule mod_ coders (Lists.zip els tterms)))))))
