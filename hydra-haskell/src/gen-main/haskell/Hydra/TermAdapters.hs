-- | Adapter framework for types and terms

module Hydra.TermAdapters where

import qualified Hydra.AdapterUtils as AdapterUtils
import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Describe.Core as Core_
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core__
import qualified Hydra.Flows as Flows
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows_
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.LiteralAdapters as LiteralAdapters
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Show.Core as Core___
import qualified Hydra.Strip as Strip
import qualified Hydra.Variants as Variants
import Prelude hiding  (map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Create an adapter for field types
fieldAdapter :: (Core.FieldType -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.FieldType Core.FieldType Core.Field Core.Field))
fieldAdapter ftyp = (Flows_.bind (termAdapter (Core.fieldTypeType ftyp)) (\ad -> Flows_.pure (Compute.Adapter {
  Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
  Compute.adapterSource = ftyp,
  Compute.adapterTarget = Core.FieldType {
    Core.fieldTypeName = (Core.fieldTypeName ftyp),
    Core.fieldTypeType = (Compute.adapterTarget ad)},
  Compute.adapterCoder = (AdapterUtils.bidirectional (\dir -> \field ->  
    let name = (Core.fieldName field) 
        term = (Core.fieldTerm field)
    in (Flows_.map (\newTerm -> Core.Field {
      Core.fieldName = name,
      Core.fieldTerm = newTerm}) (AdapterUtils.encodeDecode dir (Compute.adapterCoder ad) term))))})))

-- | This function accounts for recursive type definitions
forTypeReference :: (Core.Name -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
forTypeReference name = (Flows.withTrace (Strings.cat2 "adapt named type " (Core.unName name)) ( 
  let lossy = False 
      placeholder = Compute.Adapter {
              Compute.adapterIsLossy = lossy,
              Compute.adapterSource = (Core.TypeVariable name),
              Compute.adapterTarget = (Core.TypeVariable name),
              Compute.adapterCoder = (AdapterUtils.bidirectional (\dir -> \term -> Flows_.bind Errors.getState (\cx ->  
                let adapters = (Coders.adapterContextAdapters cx)
                in (Optionals.maybe (Flows_.fail (Strings.cat2 "no adapter for reference type " (Core.unName name))) (\ad -> AdapterUtils.encodeDecode dir (Compute.adapterCoder ad) term) (Maps.lookup name adapters)))))}
  in (Flows_.bind Errors.getState (\cx ->  
    let adapters = (Coders.adapterContextAdapters cx)
    in (Optionals.maybe ( 
      let newAdapters = (Maps.insert name placeholder adapters) 
          newCx = Coders.AdapterContext {
                  Coders.adapterContextGraph = (Coders.adapterContextGraph cx),
                  Coders.adapterContextLanguage = (Coders.adapterContextLanguage cx),
                  Coders.adapterContextAdapters = newAdapters}
      in (Flows_.bind (Errors.putState newCx) (\_ -> Flows_.bind (withGraphContext (Schemas.resolveType (Core.TypeVariable name))) (\mt -> Optionals.maybe (Flows_.pure (Compute.Adapter {
        Compute.adapterIsLossy = lossy,
        Compute.adapterSource = (Core.TypeVariable name),
        Compute.adapterTarget = (Core.TypeVariable name),
        Compute.adapterCoder = (AdapterUtils.bidirectional (\dir -> \term -> Flows_.pure term))})) (\t -> Flows_.bind (termAdapter t) (\actual ->  
        let finalAdapters = (Maps.insert name actual adapters) 
            finalCx = Coders.AdapterContext {
                    Coders.adapterContextGraph = (Coders.adapterContextGraph cx),
                    Coders.adapterContextLanguage = (Coders.adapterContextLanguage cx),
                    Coders.adapterContextAdapters = finalAdapters}
        in (Flows_.bind (Errors.putState finalCx) (\_ -> Flows_.pure actual)))) mt)))) Flows_.pure (Maps.lookup name adapters))))))

functionProxyName :: Core.Name
functionProxyName = (Core.Name "hydra.core.FunctionProxy")

functionProxyType :: (t0 -> Core.Type)
functionProxyType _ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = functionProxyName,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "wrap"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "record"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "union"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lambda"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "primitive"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))

-- | Convert function types to union types
functionToUnion :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
functionToUnion t = ((\x -> case x of
  Core.TypeFunction v1 ->  
    let dom = (Core.functionTypeDomain v1) 
        cod = (Core.functionTypeCodomain v1)
        unionType = (Flows_.bind (termAdapter dom) (\domAd -> Flows_.pure (Core.TypeUnion (Core.RowType {
                Core.rowTypeTypeName = functionProxyName,
                Core.rowTypeFields = [
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "wrap"),
                    Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "record"),
                    Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "union"),
                    Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "lambda"),
                    Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "primitive"),
                    Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
                  Core.FieldType {
                    Core.fieldTypeName = (Core.Name "variable"),
                    Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))))
        encode = (\ad -> \term ->  
                let strippedTerm = (Strip.fullyStripTerm term)
                in (Compute.coderEncode (Compute.adapterCoder ad) ((\x -> case x of
                  Core.TermFunction v2 -> ((\x -> case x of
                    Core.FunctionElimination v3 -> ((\x -> case x of
                      Core.EliminationWrap v4 -> (Core.TermUnion (Core.Injection {
                        Core.injectionTypeName = functionProxyName,
                        Core.injectionField = Core.Field {
                          Core.fieldName = (Core.Name "wrap"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core.unName v4)))}}))
                      Core.EliminationRecord _ -> (Core.TermUnion (Core.Injection {
                        Core.injectionTypeName = functionProxyName,
                        Core.injectionField = Core.Field {
                          Core.fieldName = (Core.Name "record"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core___.showTerm term)))}}))
                      Core.EliminationUnion _ -> (Core.TermUnion (Core.Injection {
                        Core.injectionTypeName = functionProxyName,
                        Core.injectionField = Core.Field {
                          Core.fieldName = (Core.Name "union"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core___.showTerm term)))}}))) v3)
                    Core.FunctionLambda _ -> (Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = functionProxyName,
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "lambda"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core___.showTerm term)))}}))
                    Core.FunctionPrimitive v3 -> (Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = functionProxyName,
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "primitive"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core.unName v3)))}}))) v2)
                  Core.TermVariable v2 -> (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = functionProxyName,
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "variable"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core.unName v2)))}}))) strippedTerm)))
        decode = (\ad -> \term ->  
                let readFromString = (\term -> Flows_.bind (Core__.string term) (\s -> Optionals.maybe (Flows_.fail (Strings.cat2 "failed to parse term: " s)) Flows_.pure (Core___.readTerm s))) 
                    notFound = (\fname -> Flows_.fail (Strings.cat2 "unexpected field: " (Core.unName fname)))
                    forCases = (\fterm -> withGraphContext (readFromString fterm))
                    forLambda = (\fterm -> withGraphContext (readFromString fterm))
                    forWrapped = (\fterm -> withGraphContext (Flows_.map (\s -> Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name s)))) (Core__.string fterm)))
                    forPrimitive = (\fterm -> withGraphContext (Flows_.map (\s -> Core.TermFunction (Core.FunctionPrimitive (Core.Name s))) (Core__.string fterm)))
                    forProjection = (\fterm -> withGraphContext (readFromString fterm))
                    forVariable = (\fterm -> withGraphContext (Flows_.map (\s -> Core.TermVariable (Core.Name s)) (Core__.string fterm)))
                in (Flows_.bind (Compute.coderDecode (Compute.adapterCoder ad) term) (\injTerm -> Flows_.bind (withGraphContext (Core__.injection functionProxyName injTerm)) (\field ->  
                  let fname = (Core.fieldName field) 
                      fterm = (Core.fieldTerm field)
                  in (Optionals.fromMaybe (notFound fname) (Maps.lookup fname (Maps.fromList [
                    (Core.Name "wrap", (forWrapped fterm)),
                    (Core.Name "record", (forProjection fterm)),
                    (Core.Name "union", (forCases fterm)),
                    (Core.Name "lambda", (forLambda fterm)),
                    (Core.Name "primitive", (forPrimitive fterm)),
                    (Core.Name "variable", (forVariable fterm))])))))))
    in (Flows_.bind unionType (\ut -> Flows_.bind (termAdapter ut) (\ad -> Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Compute.adapterTarget ad),
      Compute.adapterCoder = Compute.Coder {
        Compute.coderEncode = (encode ad),
        Compute.coderDecode = (decode ad)}}))))) t)

-- | Convert forall types to monotypes
lambdaToMonotype :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
lambdaToMonotype t = ((\x -> case x of
  Core.TypeForall v1 ->  
    let body = (Core.forallTypeBody v1)
    in (Flows_.bind (termAdapter body) (\ad -> Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Compute.adapterTarget ad),
      Compute.adapterCoder = (Compute.adapterCoder ad)})))) t)

-- | Convert set types to list types
listToSet :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
listToSet t = ((\x -> case x of
  Core.TypeSet v1 ->  
    let encode = (\ad -> \term -> (\x -> case x of
            Core.TermSet v2 -> (Compute.coderEncode (Compute.adapterCoder ad) (Core.TermList (Sets.toList v2)))) term) 
        decode = (\ad -> \term -> Flows_.bind (Compute.coderDecode (Compute.adapterCoder ad) term) (\listTerm -> (\x -> case x of
                Core.TermList v2 -> (Flows_.pure (Core.TermSet (Sets.fromList v2)))) listTerm))
    in (Flows_.bind (termAdapter (Core.TypeList v1)) (\ad -> Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Compute.adapterTarget ad),
      Compute.adapterCoder = Compute.Coder {
        Compute.coderEncode = (encode ad),
        Compute.coderDecode = (decode ad)}})))) t)

-- | Convert optional types to list types
optionalToList :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
optionalToList t = ((\x -> case x of
  Core.TypeOptional v1 -> (Flows_.bind (termAdapter v1) (\ad ->  
    let encode = (\term -> (\x -> case x of
            Core.TermOptional v2 -> (Optionals.maybe (Flows_.pure (Core.TermList [])) (\r -> Flows_.bind (Compute.coderEncode (Compute.adapterCoder ad) r) (\encoded -> Flows_.pure (Core.TermList [
              encoded]))) v2)) term) 
        decode = (\term -> (\x -> case x of
                Core.TermList v2 -> (Flows_.map (\x -> Core.TermOptional x) (Logic.ifElse (Lists.null v2) (Flows_.pure Nothing) (Flows_.bind (Compute.coderDecode (Compute.adapterCoder ad) (Lists.head v2)) (\decoded -> Flows_.pure (Just decoded)))))) term)
    in (Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = False,
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeList (Compute.adapterTarget ad)),
      Compute.adapterCoder = Compute.Coder {
        Compute.coderEncode = encode,
        Compute.coderDecode = decode}}))))) t)

-- | Pass through application types
passApplication :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passApplication t = ((\x -> case x of
  Core.TypeApplication v1 ->  
    let lhs = (Core.applicationTypeFunction v1) 
        rhs = (Core.applicationTypeArgument v1)
    in (Flows_.bind (termAdapter lhs) (\lhsAd -> Flows_.bind (termAdapter rhs) (\rhsAd -> Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Logic.or (Compute.adapterIsLossy lhsAd) (Compute.adapterIsLossy rhsAd)),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = (Compute.adapterTarget lhsAd),
        Core.applicationTypeArgument = (Compute.adapterTarget rhsAd)})),
      Compute.adapterCoder = (AdapterUtils.bidirectional (\dir -> \term -> AdapterUtils.encodeDecode dir (Compute.adapterCoder lhsAd) term))}))))) t)

-- | Pass through function types with adaptation
passFunction :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passFunction t = ((\x -> case x of
  Core.TypeFunction v1 ->  
    let dom = (Core.functionTypeDomain v1) 
        cod = (Core.functionTypeCodomain v1)
    in (Flows_.bind (termAdapter dom) (\domAd -> Flows_.bind (termAdapter cod) (\codAd -> Flows_.bind ((\x -> case x of
      Core.TypeUnion v2 -> (Flows_.bind (Flows_.mapList (\f -> Flows_.bind (fieldAdapter (Core.FieldType {
        Core.fieldTypeName = (Core.fieldTypeName f),
        Core.fieldTypeType = (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = (Core.fieldTypeType f),
          Core.functionTypeCodomain = cod}))})) (\ad -> Flows_.pure (Core.fieldTypeName f, ad))) (Core.rowTypeFields v2)) (\pairs -> Flows_.pure (Maps.fromList pairs)))
      _ -> (Flows_.pure Maps.empty)) (Strip.stripType dom)) (\caseAds -> Flows_.bind ((\x -> case x of
      Core.TypeOptional v2 -> (Flows_.map Optionals.pure (termAdapter (Core.TypeFunction (Core.FunctionType {
        Core.functionTypeDomain = v2,
        Core.functionTypeCodomain = cod}))))
      _ -> (Flows_.pure Nothing)) (Strip.stripType dom)) (\optionAd ->  
      let lossy = (Logic.or (Compute.adapterIsLossy codAd) (Lists.foldl Logic.or False (Lists.map (\pair -> Compute.adapterIsLossy (snd pair)) (Maps.toList caseAds)))) 
          target = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Compute.adapterTarget domAd),
                  Core.functionTypeCodomain = (Compute.adapterTarget codAd)}))
          getCoder = (\fname -> Optionals.maybe AdapterUtils.idCoder Compute.adapterCoder (Maps.lookup fname caseAds))
      in (Flows_.pure (Compute.Adapter {
        Compute.adapterIsLossy = lossy,
        Compute.adapterSource = t,
        Compute.adapterTarget = target,
        Compute.adapterCoder = (AdapterUtils.bidirectional (\dir -> \term -> (\x -> case x of
          Core.TermFunction v2 -> (Flows_.map (\x -> Core.TermFunction x) ((\x -> case x of
            Core.FunctionElimination v3 -> (Flows_.map (\x -> Core.FunctionElimination x) ((\x -> case x of
              Core.EliminationUnion v4 ->  
                let n = (Core.caseStatementTypeName v4) 
                    def = (Core.caseStatementDefault v4)
                    cases = (Core.caseStatementCases v4)
                in (Flows_.bind (Flows_.mapList (\f -> AdapterUtils.encodeDecode dir (getCoder (Core.fieldName f)) f) cases) (\rcases -> Flows_.bind (Optionals.maybe (Flows_.pure Nothing) (\d -> Flows_.map Optionals.pure (AdapterUtils.encodeDecode dir (Compute.adapterCoder codAd) d)) def) (\rdef -> Flows_.pure (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = n,
                  Core.caseStatementDefault = rdef,
                  Core.caseStatementCases = rcases})))))) v3))
            Core.FunctionLambda v3 ->  
              let var = (Core.lambdaParameter v3) 
                  d = (Core.lambdaDomain v3)
                  body = (Core.lambdaBody v3)
              in (Flows_.bind (AdapterUtils.encodeDecode dir (Compute.adapterCoder codAd) body) (\newBody -> Flows_.pure (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = var,
                Core.lambdaDomain = d,
                Core.lambdaBody = newBody}))))
            Core.FunctionPrimitive v3 -> (Flows_.pure (Core.FunctionPrimitive v3))) v2))
          _ -> (Flows_.pure term)) (Strip.fullyStripTerm term)))})))))))) t)

-- | Pass through forall types
passForall :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passForall t = ((\x -> case x of
  Core.TypeForall v1 ->  
    let v = (Core.forallTypeParameter v1) 
        body = (Core.forallTypeBody v1)
    in (Flows_.bind (termAdapter body) (\ad -> Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeForall (Core.ForallType {
        Core.forallTypeParameter = v,
        Core.forallTypeBody = (Compute.adapterTarget ad)})),
      Compute.adapterCoder = (AdapterUtils.bidirectional (\dir -> \term -> AdapterUtils.encodeDecode dir (Compute.adapterCoder ad) term))})))) t)

-- | Pass through literal types with literal adaptation
passLiteral :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passLiteral t = ((\x -> case x of
  Core.TypeLiteral v1 -> (Flows_.bind (LiteralAdapters.literalAdapter v1) (\ad ->  
    let step = (AdapterUtils.bidirectional (\dir -> \term -> Flows_.bind (withGraphContext (Core__.literal term)) (\l -> Flows_.map (\x -> Core.TermLiteral x) (AdapterUtils.encodeDecode dir (Compute.adapterCoder ad) l))))
    in (Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
      Compute.adapterSource = (Core.TypeLiteral (Compute.adapterSource ad)),
      Compute.adapterTarget = (Core.TypeLiteral (Compute.adapterTarget ad)),
      Compute.adapterCoder = step}))))) t)

-- | Pass through list types
passList :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passList t = ((\x -> case x of
  Core.TypeList v1 -> (Flows_.bind (termAdapter v1) (\ad -> Flows_.pure (Compute.Adapter {
    Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
    Compute.adapterSource = t,
    Compute.adapterTarget = (Core.TypeList (Compute.adapterTarget ad)),
    Compute.adapterCoder = (AdapterUtils.bidirectional (\dir -> \term -> (\x -> case x of
      Core.TermList v2 -> (Flows_.bind (Flows_.mapList (AdapterUtils.encodeDecode dir (Compute.adapterCoder ad)) v2) (\newTerms -> Flows_.pure (Core.TermList newTerms)))) term))})))) t)

-- | Pass through map types
passMap :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passMap t = ((\x -> case x of
  Core.TypeMap v1 ->  
    let kt = (Core.mapTypeKeys v1) 
        vt = (Core.mapTypeValues v1)
    in (Flows_.bind (termAdapter kt) (\kad -> Flows_.bind (termAdapter vt) (\vad -> Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Logic.or (Compute.adapterIsLossy kad) (Compute.adapterIsLossy vad)),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = (Compute.adapterTarget kad),
        Core.mapTypeValues = (Compute.adapterTarget vad)})),
      Compute.adapterCoder = (AdapterUtils.bidirectional (\dir -> \term -> (\x -> case x of
        Core.TermMap v2 -> (Flows_.bind (Flows_.mapList (\pair ->  
          let k = (fst pair) 
              v = (snd pair)
          in (Flows_.bind (AdapterUtils.encodeDecode dir (Compute.adapterCoder kad) k) (\newK -> Flows_.bind (AdapterUtils.encodeDecode dir (Compute.adapterCoder vad) v) (\newV -> Flows_.pure (newK, newV))))) (Maps.toList v2)) (\newPairs -> Flows_.pure (Core.TermMap (Maps.fromList newPairs))))) term))}))))) t)

-- | Pass through optional types
passOptional :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passOptional t = ((\x -> case x of
  Core.TypeOptional v1 ->  
    let mapTerm = (\coder -> \dir -> \term -> Flows_.bind (withGraphContext (Core__.optional Flows_.pure term)) (\opt -> Flows_.bind (Flows_.traverseOptional (AdapterUtils.encodeDecode dir coder) opt) (\newOpt -> Flows_.pure (Core.TermOptional newOpt))))
    in (Flows_.bind (termAdapter v1) (\adapter -> Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy adapter),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeOptional (Compute.adapterTarget adapter)),
      Compute.adapterCoder = (AdapterUtils.bidirectional (mapTerm (Compute.adapterCoder adapter)))})))) t)

-- | Pass through product types
passProduct :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passProduct t = ((\x -> case x of
  Core.TypeProduct v1 -> (Flows_.bind (Flows_.mapList termAdapter v1) (\ads ->  
    let lossy = (Lists.foldl Logic.or False (Lists.map Compute.adapterIsLossy ads))
    in (Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = lossy,
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeProduct (Lists.map Compute.adapterTarget ads)),
      Compute.adapterCoder = (AdapterUtils.bidirectional (\dir -> \term -> (\x -> case x of
        Core.TermProduct v2 -> (Flows_.bind (Flows_.sequence (Lists.zipWith (\term -> \ad -> AdapterUtils.encodeDecode dir (Compute.adapterCoder ad) term) v2 ads)) (\newTuple -> Flows_.pure (Core.TermProduct newTuple)))) term))}))))) t)

-- | Pass through record types
passRecord :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passRecord t = ((\x -> case x of
  Core.TypeRecord v1 -> (Flows_.bind (Flows_.mapList fieldAdapter (Core.rowTypeFields v1)) (\adapters ->  
    let lossy = (Lists.foldl Logic.or False (Lists.map Compute.adapterIsLossy adapters)) 
        sfields_ = (Lists.map Compute.adapterTarget adapters)
    in (Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = lossy,
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.rowTypeTypeName v1),
        Core.rowTypeFields = sfields_})),
      Compute.adapterCoder = (AdapterUtils.bidirectional (\dir -> \term -> (\x -> case x of
        Core.TermRecord v2 ->  
          let dfields = (Core.recordFields v2)
          in (Flows_.bind (Flows_.sequence (Lists.zipWith (\ad -> \f -> AdapterUtils.encodeDecode dir (Compute.adapterCoder ad) f) adapters dfields)) (\newFields -> Flows_.pure (Core.TermRecord (Core.Record {
            Core.recordTypeName = (Core.rowTypeTypeName v1),
            Core.recordFields = newFields}))))) term))}))))) t)

-- | Pass through set types
passSet :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passSet t = ((\x -> case x of
  Core.TypeSet v1 -> (Flows_.bind (termAdapter v1) (\ad -> Flows_.pure (Compute.Adapter {
    Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
    Compute.adapterSource = t,
    Compute.adapterTarget = (Core.TypeSet (Compute.adapterTarget ad)),
    Compute.adapterCoder = (AdapterUtils.bidirectional (\dir -> \term -> (\x -> case x of
      Core.TermSet v2 -> (Flows_.bind (Flows_.mapList (AdapterUtils.encodeDecode dir (Compute.adapterCoder ad)) (Sets.toList v2)) (\newTerms -> Flows_.pure (Core.TermSet (Sets.fromList newTerms))))) term))})))) t)

-- | Pass through sum types
passSum :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passSum t = ((\x -> case x of
  Core.TypeSum v1 -> (Flows_.bind (Flows_.mapList termAdapter v1) (\ads ->  
    let lossy = (Lists.foldl Logic.or False (Lists.map Compute.adapterIsLossy ads))
    in (Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = lossy,
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeSum (Lists.map Compute.adapterTarget ads)),
      Compute.adapterCoder = (AdapterUtils.bidirectional (\dir -> \term -> (\x -> case x of
        Core.TermSum v2 ->  
          let i = (Core.sumIndex v2) 
              n = (Core.sumSize v2)
              term = (Core.sumTerm v2)
          in (Flows_.bind (AdapterUtils.encodeDecode dir (Compute.adapterCoder (Lists.at i ads)) term) (\newTerm -> Flows_.pure (Core.TermSum (Core.Sum {
            Core.sumIndex = i,
            Core.sumSize = n,
            Core.sumTerm = newTerm}))))) term))}))))) t)

-- | Pass through union types
passUnion :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passUnion t = ((\x -> case x of
  Core.TypeUnion v1 ->  
    let sfields = (Core.rowTypeFields v1) 
        tname = (Core.rowTypeTypeName v1)
        getAdapter = (\adaptersMap -> \f -> Optionals.maybe (Flows_.fail (Strings.cat2 "no such field: " (Core.unName (Core.fieldName f)))) Flows_.pure (Maps.lookup (Core.fieldName f) adaptersMap))
    in (Flows_.bind (Flows_.mapList (\f -> Flows_.bind (fieldAdapter f) (\ad -> Flows_.pure (Core.fieldTypeName f, ad))) sfields) (\adapters ->  
      let adaptersMap = (Maps.fromList adapters) 
          lossy = (Lists.foldl Logic.or False (Lists.map (\pair -> Compute.adapterIsLossy (snd pair)) adapters))
          sfields_ = (Lists.map (\pair -> Compute.adapterTarget (snd pair)) adapters)
      in (Flows_.pure (Compute.Adapter {
        Compute.adapterIsLossy = lossy,
        Compute.adapterSource = t,
        Compute.adapterTarget = (Core.TypeUnion (Core.RowType {
          Core.rowTypeTypeName = tname,
          Core.rowTypeFields = sfields_})),
        Compute.adapterCoder = (AdapterUtils.bidirectional (\dir -> \term -> Flows_.bind (withGraphContext (Core__.injection tname term)) (\dfield -> Flows_.bind (getAdapter adaptersMap dfield) (\ad -> Flows_.bind (AdapterUtils.encodeDecode dir (Compute.adapterCoder ad) dfield) (\newField -> Flows_.pure (Core.TermUnion (Core.Injection {
          Core.injectionTypeName = tname,
          Core.injectionField = newField})))))))}))))) t)

-- | Pass through wrapped types
passWrapped :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passWrapped t = ((\x -> case x of
  Core.TypeWrap v1 ->  
    let tname = (Core.wrappedTypeTypeName v1) 
        ot = (Core.wrappedTypeObject v1)
        mapTerm = (\coder -> \dir -> \term -> Flows_.bind (withGraphContext (Core__.wrap tname term)) (\unwrapped -> Flows_.bind (AdapterUtils.encodeDecode dir coder unwrapped) (\newTerm -> Flows_.pure (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = tname,
                Core.wrappedTermObject = newTerm})))))
    in (Flows_.bind (termAdapter ot) (\adapter -> Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy adapter),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeWrap (Core.WrappedType {
        Core.wrappedTypeTypeName = tname,
        Core.wrappedTypeObject = (Compute.adapterTarget adapter)})),
      Compute.adapterCoder = (AdapterUtils.bidirectional (mapTerm (Compute.adapterCoder adapter)))})))) t)

-- | Simplify application types
simplifyApplication :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
simplifyApplication t = ((\x -> case x of
  Core.TypeApplication v1 ->  
    let lhs = (Core.applicationTypeFunction v1)
    in (Flows_.bind (termAdapter lhs) (\ad -> Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = False,
      Compute.adapterSource = t,
      Compute.adapterTarget = (Compute.adapterTarget ad),
      Compute.adapterCoder = (AdapterUtils.bidirectional (\dir -> \term -> AdapterUtils.encodeDecode dir (Compute.adapterCoder ad) term))})))) t)

-- | Create an adapter for any type
termAdapter :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
termAdapter typ =  
  let constraints = (\cx -> Coders.languageConstraints (Coders.adapterContextLanguage cx)) 
      supported = (\cx -> AdapterUtils.typeIsSupported (constraints cx))
      variantIsSupported = (\cx -> \t -> Sets.member (Variants.typeVariant t) (Coders.languageConstraintsTypeVariants (constraints cx)))
      supportedAtTopLevel = (\cx -> \t -> Logic.and (variantIsSupported cx t) (Coders.languageConstraintsTypes (constraints cx) t))
      pass = (\t -> (\x -> case x of
              Mantle.TypeVariantApplication -> [
                passApplication]
              Mantle.TypeVariantForall -> [
                passForall]
              Mantle.TypeVariantFunction -> [
                passFunction]
              Mantle.TypeVariantList -> [
                passList]
              Mantle.TypeVariantLiteral -> [
                passLiteral]
              Mantle.TypeVariantMap -> [
                passMap]
              Mantle.TypeVariantOptional -> [
                passOptional,
                optionalToList]
              Mantle.TypeVariantProduct -> [
                passProduct]
              Mantle.TypeVariantRecord -> [
                passRecord]
              Mantle.TypeVariantSet -> [
                passSet]
              Mantle.TypeVariantSum -> [
                passSum]
              Mantle.TypeVariantUnion -> [
                passUnion]
              Mantle.TypeVariantWrap -> [
                passWrapped]) (Variants.typeVariant (Strip.stripType t)))
      trySubstitution = (\t -> (\x -> case x of
              Mantle.TypeVariantApplication -> [
                simplifyApplication]
              Mantle.TypeVariantFunction -> [
                functionToUnion]
              Mantle.TypeVariantForall -> [
                lambdaToMonotype]
              Mantle.TypeVariantOptional -> [
                optionalToList]
              Mantle.TypeVariantSet -> [
                listToSet]
              Mantle.TypeVariantUnion -> [
                unionToRecord]
              Mantle.TypeVariantWrap -> [
                wrapToUnwrapped]) (Variants.typeVariant t))
      alts = (\cx -> \t -> Flows_.mapList (\c -> c t) (Logic.ifElse (supportedAtTopLevel cx t) (pass t) (trySubstitution t)))
  in ((\x -> case x of
    Core.TypeAnnotated v1 -> (Flows_.bind (termAdapter (Core.annotatedTypeSubject v1)) (\ad -> Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
      Compute.adapterSource = (Compute.adapterSource ad),
      Compute.adapterTarget = (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeSubject = (Compute.adapterTarget ad),
        Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v1)})),
      Compute.adapterCoder = (Compute.adapterCoder ad)})))
    _ -> (Flows.withTrace (Strings.cat2 "adapter for " (Core_.type_ typ)) ((\x -> case x of
      Core.TypeVariable v1 -> (forTypeReference v1)
      _ -> (Flows_.bind Errors.getState (\cx -> AdapterUtils.chooseAdapter (alts cx) (supported cx) Core___.showType Core_.type_ typ))) typ))) typ)

-- | Convert union types to record types
unionToRecord :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
unionToRecord t = ((\x -> case x of
  Core.TypeUnion v1 ->  
    let nm = (Core.rowTypeTypeName v1) 
        sfields = (Core.rowTypeFields v1)
        target = (Core.TypeRecord (unionTypeToRecordType v1))
        toRecordField = (\term -> \fn -> \f ->  
                let fn_ = (Core.fieldTypeName f)
                in Core.Field {
                  Core.fieldName = fn_,
                  Core.fieldTerm = (Core.TermOptional (Logic.ifElse (Equality.equal fn_ fn) (Just term) Nothing))})
        fromRecordFields = (\term -> \term_ -> \t_ -> \fields ->  
                let matches = (Optionals.mapMaybe (\field ->  
                        let fn = (Core.fieldName field) 
                            fterm = (Core.fieldTerm field)
                        in ((\x -> case x of
                          Core.TermOptional v2 -> (Optionals.bind v2 (\t -> Just (Core.Field {
                            Core.fieldName = fn,
                            Core.fieldTerm = t})))) fterm)) fields)
                in (Logic.ifElse (Lists.null matches) (Flows_.fail (Strings.cat [
                  "cannot convert term back to union: ",
                  Core___.showTerm term,
                  " where type = ",
                  Core___.showType t,
                  "    and target type = ",
                  (Core___.showType t_)])) (Flows_.pure (Lists.head matches))))
    in (Flows_.bind (termAdapter target) (\ad -> Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Compute.adapterTarget ad),
      Compute.adapterCoder = Compute.Coder {
        Compute.coderEncode = (\term_ -> Flows_.bind (withGraphContext (Core__.injection (Core.rowTypeTypeName v1) term_)) (\field ->  
          let fn = (Core.fieldName field) 
              term = (Core.fieldTerm field)
          in (Compute.coderEncode (Compute.adapterCoder ad) (Core.TermRecord (Core.Record {
            Core.recordTypeName = nm,
            Core.recordFields = (Lists.map (toRecordField term fn) sfields)}))))),
        Compute.coderDecode = (\term -> Flows_.bind (Compute.coderDecode (Compute.adapterCoder ad) term) (\recTerm -> (\x -> case x of
          Core.TermRecord v2 ->  
            let fields = (Core.recordFields v2)
            in (Flows_.bind (fromRecordFields term (Core.TermRecord (Core.Record {
              Core.recordTypeName = nm,
              Core.recordFields = fields})) (Compute.adapterTarget ad) fields) (\resultField -> Flows_.pure (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = nm,
              Core.injectionField = resultField}))))) recTerm))}})))) t)

-- | Convert a union row type to a record row type
unionTypeToRecordType :: (Core.RowType -> Core.RowType)
unionTypeToRecordType rt =  
  let makeOptional = (\f ->  
          let fn = (Core.fieldTypeName f) 
              ft = (Core.fieldTypeType f)
          in Core.FieldType {
            Core.fieldTypeName = fn,
            Core.fieldTypeType = (Rewriting.mapBeneathTypeAnnotations (\x -> Core.TypeOptional x) ft)})
  in Core.RowType {
    Core.rowTypeTypeName = (Core.rowTypeTypeName rt),
    Core.rowTypeFields = (Lists.map makeOptional (Core.rowTypeFields rt))}

-- | Convert wrapped types to unwrapped types
wrapToUnwrapped :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
wrapToUnwrapped t = ((\x -> case x of
  Core.TypeWrap v1 ->  
    let tname = (Core.wrappedTypeTypeName v1) 
        typ = (Core.wrappedTypeObject v1)
        encode = (\ad -> \term -> Flows_.bind (withGraphContext (Core__.wrap tname term)) (\unwrapped -> Compute.coderEncode (Compute.adapterCoder ad) unwrapped))
        decode = (\ad -> \term -> Flows_.bind (Compute.coderDecode (Compute.adapterCoder ad) term) (\decoded -> Flows_.pure (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = tname,
                Core.wrappedTermObject = decoded}))))
    in (Flows_.bind (termAdapter typ) (\ad -> Flows_.pure (Compute.Adapter {
      Compute.adapterIsLossy = False,
      Compute.adapterSource = t,
      Compute.adapterTarget = (Compute.adapterTarget ad),
      Compute.adapterCoder = Compute.Coder {
        Compute.coderEncode = (encode ad),
        Compute.coderDecode = (decode ad)}})))) t)

withGraphContext :: (Compute.Flow Graph.Graph t0 -> Compute.Flow Coders.AdapterContext t0)
withGraphContext f = (Flows_.bind Errors.getState (\cx -> Flows.withState (Coders.adapterContextGraph cx) f))
