-- | Adapter framework for types and terms

module Hydra.Adapt.Terms where

import qualified Hydra.Adapt.Literals as Literals
import qualified Hydra.Adapt.Utils as Utils
import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Describe.Core as Core_
import qualified Hydra.Extract.Core as Core__
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Monads as Monads
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Show.Core as Core___
import qualified Hydra.Strip as Strip
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Create an adapter for field types
fieldAdapter :: (Core.FieldType -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.FieldType Core.FieldType Core.Field Core.Field))
fieldAdapter ftyp = (Flows.bind (termAdapter (Core.fieldTypeType ftyp)) (\ad -> Flows.pure (Compute.Adapter {
  Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
  Compute.adapterSource = ftyp,
  Compute.adapterTarget = Core.FieldType {
    Core.fieldTypeName = (Core.fieldTypeName ftyp),
    Core.fieldTypeType = (Compute.adapterTarget ad)},
  Compute.adapterCoder = (Utils.bidirectional (\dir -> \field ->  
    let name = (Core.fieldName field) 
        term = (Core.fieldTerm field)
    in (Flows.map (\newTerm -> Core.Field {
      Core.fieldName = name,
      Core.fieldTerm = newTerm}) (Utils.encodeDecode dir (Compute.adapterCoder ad) term))))})))

-- | This function accounts for recursive type definitions
forTypeReference :: (Core.Name -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
forTypeReference name = (Monads.withTrace (Strings.cat2 "adapt named type " (Core.unName name)) ( 
  let lossy = False 
      placeholder = Compute.Adapter {
              Compute.adapterIsLossy = lossy,
              Compute.adapterSource = (Core.TypeVariable name),
              Compute.adapterTarget = (Core.TypeVariable name),
              Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> Flows.bind Monads.getState (\cx ->  
                let adapters = (Coders.adapterContextAdapters cx)
                in (Optionals.maybe (Flows.fail (Strings.cat2 "no adapter for reference type " (Core.unName name))) (\ad -> Utils.encodeDecode dir (Compute.adapterCoder ad) term) (Maps.lookup name adapters)))))}
  in (Flows.bind Monads.getState (\cx ->  
    let adapters = (Coders.adapterContextAdapters cx)
    in (Optionals.maybe ( 
      let newAdapters = (Maps.insert name placeholder adapters) 
          newCx = Coders.AdapterContext {
                  Coders.adapterContextGraph = (Coders.adapterContextGraph cx),
                  Coders.adapterContextLanguage = (Coders.adapterContextLanguage cx),
                  Coders.adapterContextAdapters = newAdapters}
      in (Flows.bind (Monads.putState newCx) (\_ -> Flows.bind (withGraphContext (Schemas.resolveType (Core.TypeVariable name))) (\mt -> Optionals.maybe (Flows.pure (Compute.Adapter {
        Compute.adapterIsLossy = lossy,
        Compute.adapterSource = (Core.TypeVariable name),
        Compute.adapterTarget = (Core.TypeVariable name),
        Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> Flows.pure term))})) (\t -> Flows.bind (termAdapter t) (\actual ->  
        let finalAdapters = (Maps.insert name actual adapters) 
            finalCx = Coders.AdapterContext {
                    Coders.adapterContextGraph = (Coders.adapterContextGraph cx),
                    Coders.adapterContextLanguage = (Coders.adapterContextLanguage cx),
                    Coders.adapterContextAdapters = finalAdapters}
        in (Flows.bind (Monads.putState finalCx) (\_ -> Flows.pure actual)))) mt)))) Flows.pure (Maps.lookup name adapters))))))

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
        unionType = (Flows.bind (termAdapter dom) (\domAd -> Flows.pure (Core.TypeUnion (Core.RowType {
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
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core___.term term)))}}))
                      Core.EliminationUnion _ -> (Core.TermUnion (Core.Injection {
                        Core.injectionTypeName = functionProxyName,
                        Core.injectionField = Core.Field {
                          Core.fieldName = (Core.Name "union"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core___.term term)))}}))) v3)
                    Core.FunctionLambda _ -> (Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = functionProxyName,
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "lambda"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core___.term term)))}}))
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
                let readFromString = (\term -> Flows.bind (Core__.string term) (\s -> Optionals.maybe (Flows.fail (Strings.cat2 "failed to parse term: " s)) Flows.pure (Core___.readTerm s))) 
                    notFound = (\fname -> Flows.fail (Strings.cat2 "unexpected field: " (Core.unName fname)))
                    forCases = (\fterm -> withGraphContext (readFromString fterm))
                    forLambda = (\fterm -> withGraphContext (readFromString fterm))
                    forWrapped = (\fterm -> withGraphContext (Flows.map (\s -> Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name s)))) (Core__.string fterm)))
                    forPrimitive = (\fterm -> withGraphContext (Flows.map (\s -> Core.TermFunction (Core.FunctionPrimitive (Core.Name s))) (Core__.string fterm)))
                    forProjection = (\fterm -> withGraphContext (readFromString fterm))
                    forVariable = (\fterm -> withGraphContext (Flows.map (\s -> Core.TermVariable (Core.Name s)) (Core__.string fterm)))
                in (Flows.bind (Compute.coderDecode (Compute.adapterCoder ad) term) (\injTerm -> Flows.bind (withGraphContext (Core__.injection functionProxyName injTerm)) (\field ->  
                  let fname = (Core.fieldName field) 
                      fterm = (Core.fieldTerm field)
                  in (Optionals.fromMaybe (notFound fname) (Maps.lookup fname (Maps.fromList [
                    (Core.Name "wrap", (forWrapped fterm)),
                    (Core.Name "record", (forProjection fterm)),
                    (Core.Name "union", (forCases fterm)),
                    (Core.Name "lambda", (forLambda fterm)),
                    (Core.Name "primitive", (forPrimitive fterm)),
                    (Core.Name "variable", (forVariable fterm))])))))))
    in (Flows.bind unionType (\ut -> Flows.bind (termAdapter ut) (\ad -> Flows.pure (Compute.Adapter {
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
    in (Flows.bind (termAdapter body) (\ad -> Flows.pure (Compute.Adapter {
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
        decode = (\ad -> \term -> Flows.bind (Compute.coderDecode (Compute.adapterCoder ad) term) (\listTerm -> (\x -> case x of
                Core.TermList v2 -> (Flows.pure (Core.TermSet (Sets.fromList v2)))) listTerm))
    in (Flows.bind (termAdapter (Core.TypeList v1)) (\ad -> Flows.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Compute.adapterTarget ad),
      Compute.adapterCoder = Compute.Coder {
        Compute.coderEncode = (encode ad),
        Compute.coderDecode = (decode ad)}})))) t)

-- | Convert optional types to list types
optionalToList :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
optionalToList t = ((\x -> case x of
  Core.TypeOptional v1 -> (Flows.bind (termAdapter v1) (\ad ->  
    let encode = (\term -> (\x -> case x of
            Core.TermOptional v2 -> (Optionals.maybe (Flows.pure (Core.TermList [])) (\r -> Flows.bind (Compute.coderEncode (Compute.adapterCoder ad) r) (\encoded -> Flows.pure (Core.TermList [
              encoded]))) v2)) term) 
        decode = (\term -> (\x -> case x of
                Core.TermList v2 -> (Flows.map (\x -> Core.TermOptional x) (Logic.ifElse (Lists.null v2) (Flows.pure Nothing) (Flows.bind (Compute.coderDecode (Compute.adapterCoder ad) (Lists.head v2)) (\decoded -> Flows.pure (Just decoded)))))) term)
    in (Flows.pure (Compute.Adapter {
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
    in (Flows.bind (termAdapter lhs) (\lhsAd -> Flows.bind (termAdapter rhs) (\rhsAd -> Flows.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Logic.or (Compute.adapterIsLossy lhsAd) (Compute.adapterIsLossy rhsAd)),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = (Compute.adapterTarget lhsAd),
        Core.applicationTypeArgument = (Compute.adapterTarget rhsAd)})),
      Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> Utils.encodeDecode dir (Compute.adapterCoder lhsAd) term))}))))) t)

-- | Pass through function types with adaptation
passFunction :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passFunction t = ((\x -> case x of
  Core.TypeFunction v1 ->  
    let dom = (Core.functionTypeDomain v1) 
        cod = (Core.functionTypeCodomain v1)
    in (Flows.bind (termAdapter dom) (\domAd -> Flows.bind (termAdapter cod) (\codAd -> Flows.bind ((\x -> case x of
      Core.TypeUnion v2 -> (Flows.bind (Flows.mapList (\f -> Flows.bind (fieldAdapter (Core.FieldType {
        Core.fieldTypeName = (Core.fieldTypeName f),
        Core.fieldTypeType = (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = (Core.fieldTypeType f),
          Core.functionTypeCodomain = cod}))})) (\ad -> Flows.pure (Core.fieldTypeName f, ad))) (Core.rowTypeFields v2)) (\pairs -> Flows.pure (Maps.fromList pairs)))
      _ -> (Flows.pure Maps.empty)) (Strip.stripType dom)) (\caseAds -> Flows.bind ((\x -> case x of
      Core.TypeOptional v2 -> (Flows.map Optionals.pure (termAdapter (Core.TypeFunction (Core.FunctionType {
        Core.functionTypeDomain = v2,
        Core.functionTypeCodomain = cod}))))
      _ -> (Flows.pure Nothing)) (Strip.stripType dom)) (\optionAd ->  
      let lossy = (Logic.or (Compute.adapterIsLossy codAd) (Lists.foldl Logic.or False (Lists.map (\pair -> Compute.adapterIsLossy (snd pair)) (Maps.toList caseAds)))) 
          target = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Compute.adapterTarget domAd),
                  Core.functionTypeCodomain = (Compute.adapterTarget codAd)}))
          getCoder = (\fname -> Optionals.maybe Utils.idCoder Compute.adapterCoder (Maps.lookup fname caseAds))
      in (Flows.pure (Compute.Adapter {
        Compute.adapterIsLossy = lossy,
        Compute.adapterSource = t,
        Compute.adapterTarget = target,
        Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> (\x -> case x of
          Core.TermFunction v2 -> (Flows.map (\x -> Core.TermFunction x) ((\x -> case x of
            Core.FunctionElimination v3 -> (Flows.map (\x -> Core.FunctionElimination x) ((\x -> case x of
              Core.EliminationUnion v4 ->  
                let n = (Core.caseStatementTypeName v4) 
                    def = (Core.caseStatementDefault v4)
                    cases = (Core.caseStatementCases v4)
                in (Flows.bind (Flows.mapList (\f -> Utils.encodeDecode dir (getCoder (Core.fieldName f)) f) cases) (\rcases -> Flows.bind (Optionals.maybe (Flows.pure Nothing) (\d -> Flows.map Optionals.pure (Utils.encodeDecode dir (Compute.adapterCoder codAd) d)) def) (\rdef -> Flows.pure (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = n,
                  Core.caseStatementDefault = rdef,
                  Core.caseStatementCases = rcases})))))) v3))
            Core.FunctionLambda v3 ->  
              let var = (Core.lambdaParameter v3) 
                  d = (Core.lambdaDomain v3)
                  body = (Core.lambdaBody v3)
              in (Flows.bind (Utils.encodeDecode dir (Compute.adapterCoder codAd) body) (\newBody -> Flows.pure (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = var,
                Core.lambdaDomain = d,
                Core.lambdaBody = newBody}))))
            Core.FunctionPrimitive v3 -> (Flows.pure (Core.FunctionPrimitive v3))) v2))
          _ -> (Flows.pure term)) (Strip.fullyStripTerm term)))})))))))) t)

-- | Pass through forall types
passForall :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passForall t = ((\x -> case x of
  Core.TypeForall v1 ->  
    let v = (Core.forallTypeParameter v1) 
        body = (Core.forallTypeBody v1)
    in (Flows.bind (termAdapter body) (\ad -> Flows.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeForall (Core.ForallType {
        Core.forallTypeParameter = v,
        Core.forallTypeBody = (Compute.adapterTarget ad)})),
      Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> Utils.encodeDecode dir (Compute.adapterCoder ad) term))})))) t)

-- | Pass through literal types with literal adaptation
passLiteral :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passLiteral t = ((\x -> case x of
  Core.TypeLiteral v1 -> (Flows.bind (Literals.literalAdapter v1) (\ad ->  
    let step = (Utils.bidirectional (\dir -> \term -> Flows.bind (withGraphContext (Core__.literal term)) (\l -> Flows.map (\x -> Core.TermLiteral x) (Utils.encodeDecode dir (Compute.adapterCoder ad) l))))
    in (Flows.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
      Compute.adapterSource = (Core.TypeLiteral (Compute.adapterSource ad)),
      Compute.adapterTarget = (Core.TypeLiteral (Compute.adapterTarget ad)),
      Compute.adapterCoder = step}))))) t)

-- | Pass through list types
passList :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passList t = ((\x -> case x of
  Core.TypeList v1 -> (Flows.bind (termAdapter v1) (\ad -> Flows.pure (Compute.Adapter {
    Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
    Compute.adapterSource = t,
    Compute.adapterTarget = (Core.TypeList (Compute.adapterTarget ad)),
    Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> (\x -> case x of
      Core.TermList v2 -> (Flows.bind (Flows.mapList (Utils.encodeDecode dir (Compute.adapterCoder ad)) v2) (\newTerms -> Flows.pure (Core.TermList newTerms)))) term))})))) t)

-- | Pass through map types
passMap :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passMap t = ((\x -> case x of
  Core.TypeMap v1 ->  
    let kt = (Core.mapTypeKeys v1) 
        vt = (Core.mapTypeValues v1)
    in (Flows.bind (termAdapter kt) (\kad -> Flows.bind (termAdapter vt) (\vad -> Flows.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Logic.or (Compute.adapterIsLossy kad) (Compute.adapterIsLossy vad)),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = (Compute.adapterTarget kad),
        Core.mapTypeValues = (Compute.adapterTarget vad)})),
      Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> (\x -> case x of
        Core.TermMap v2 -> (Flows.bind (Flows.mapList (\pair ->  
          let k = (fst pair) 
              v = (snd pair)
          in (Flows.bind (Utils.encodeDecode dir (Compute.adapterCoder kad) k) (\newK -> Flows.bind (Utils.encodeDecode dir (Compute.adapterCoder vad) v) (\newV -> Flows.pure (newK, newV))))) (Maps.toList v2)) (\newPairs -> Flows.pure (Core.TermMap (Maps.fromList newPairs))))) term))}))))) t)

-- | Pass through optional types
passOptional :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passOptional t = ((\x -> case x of
  Core.TypeOptional v1 ->  
    let mapTerm = (\coder -> \dir -> \term -> Flows.bind (withGraphContext (Core__.optional Flows.pure term)) (\opt -> Flows.bind (Flows.traverseOptional (Utils.encodeDecode dir coder) opt) (\newOpt -> Flows.pure (Core.TermOptional newOpt))))
    in (Flows.bind (termAdapter v1) (\adapter -> Flows.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy adapter),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeOptional (Compute.adapterTarget adapter)),
      Compute.adapterCoder = (Utils.bidirectional (mapTerm (Compute.adapterCoder adapter)))})))) t)

-- | Pass through product types
passProduct :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passProduct t = ((\x -> case x of
  Core.TypeProduct v1 -> (Flows.bind (Flows.mapList termAdapter v1) (\ads ->  
    let lossy = (Lists.foldl Logic.or False (Lists.map Compute.adapterIsLossy ads))
    in (Flows.pure (Compute.Adapter {
      Compute.adapterIsLossy = lossy,
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeProduct (Lists.map Compute.adapterTarget ads)),
      Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> (\x -> case x of
        Core.TermProduct v2 -> (Flows.bind (Flows.sequence (Lists.zipWith (\term -> \ad -> Utils.encodeDecode dir (Compute.adapterCoder ad) term) v2 ads)) (\newTuple -> Flows.pure (Core.TermProduct newTuple)))) term))}))))) t)

-- | Pass through record types
passRecord :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passRecord t = ((\x -> case x of
  Core.TypeRecord v1 -> (Flows.bind (Flows.mapList fieldAdapter (Core.rowTypeFields v1)) (\adapters ->  
    let lossy = (Lists.foldl Logic.or False (Lists.map Compute.adapterIsLossy adapters)) 
        sfields_ = (Lists.map Compute.adapterTarget adapters)
    in (Flows.pure (Compute.Adapter {
      Compute.adapterIsLossy = lossy,
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.rowTypeTypeName v1),
        Core.rowTypeFields = sfields_})),
      Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> (\x -> case x of
        Core.TermRecord v2 ->  
          let dfields = (Core.recordFields v2)
          in (Flows.bind (Flows.sequence (Lists.zipWith (\ad -> \f -> Utils.encodeDecode dir (Compute.adapterCoder ad) f) adapters dfields)) (\newFields -> Flows.pure (Core.TermRecord (Core.Record {
            Core.recordTypeName = (Core.rowTypeTypeName v1),
            Core.recordFields = newFields}))))) term))}))))) t)

-- | Pass through set types
passSet :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passSet t = ((\x -> case x of
  Core.TypeSet v1 -> (Flows.bind (termAdapter v1) (\ad -> Flows.pure (Compute.Adapter {
    Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
    Compute.adapterSource = t,
    Compute.adapterTarget = (Core.TypeSet (Compute.adapterTarget ad)),
    Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> (\x -> case x of
      Core.TermSet v2 -> (Flows.bind (Flows.mapList (Utils.encodeDecode dir (Compute.adapterCoder ad)) (Sets.toList v2)) (\newTerms -> Flows.pure (Core.TermSet (Sets.fromList newTerms))))) term))})))) t)

-- | Pass through sum types
passSum :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passSum t = ((\x -> case x of
  Core.TypeSum v1 -> (Flows.bind (Flows.mapList termAdapter v1) (\ads ->  
    let lossy = (Lists.foldl Logic.or False (Lists.map Compute.adapterIsLossy ads))
    in (Flows.pure (Compute.Adapter {
      Compute.adapterIsLossy = lossy,
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeSum (Lists.map Compute.adapterTarget ads)),
      Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> (\x -> case x of
        Core.TermSum v2 ->  
          let i = (Core.sumIndex v2) 
              n = (Core.sumSize v2)
              term = (Core.sumTerm v2)
          in (Flows.bind (Utils.encodeDecode dir (Compute.adapterCoder (Lists.at i ads)) term) (\newTerm -> Flows.pure (Core.TermSum (Core.Sum {
            Core.sumIndex = i,
            Core.sumSize = n,
            Core.sumTerm = newTerm}))))) term))}))))) t)

-- | Pass through union types
passUnion :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passUnion t = ((\x -> case x of
  Core.TypeUnion v1 ->  
    let sfields = (Core.rowTypeFields v1) 
        tname = (Core.rowTypeTypeName v1)
        getAdapter = (\adaptersMap -> \f -> Optionals.maybe (Flows.fail (Strings.cat2 "no such field: " (Core.unName (Core.fieldName f)))) Flows.pure (Maps.lookup (Core.fieldName f) adaptersMap))
    in (Flows.bind (Flows.mapList (\f -> Flows.bind (fieldAdapter f) (\ad -> Flows.pure (Core.fieldTypeName f, ad))) sfields) (\adapters ->  
      let adaptersMap = (Maps.fromList adapters) 
          lossy = (Lists.foldl Logic.or False (Lists.map (\pair -> Compute.adapterIsLossy (snd pair)) adapters))
          sfields_ = (Lists.map (\pair -> Compute.adapterTarget (snd pair)) adapters)
      in (Flows.pure (Compute.Adapter {
        Compute.adapterIsLossy = lossy,
        Compute.adapterSource = t,
        Compute.adapterTarget = (Core.TypeUnion (Core.RowType {
          Core.rowTypeTypeName = tname,
          Core.rowTypeFields = sfields_})),
        Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> Flows.bind (withGraphContext (Core__.injection tname term)) (\dfield -> Flows.bind (getAdapter adaptersMap dfield) (\ad -> Flows.bind (Utils.encodeDecode dir (Compute.adapterCoder ad) dfield) (\newField -> Flows.pure (Core.TermUnion (Core.Injection {
          Core.injectionTypeName = tname,
          Core.injectionField = newField})))))))}))))) t)

passUnit :: (t0 -> Compute.Flow t1 (Compute.Adapter t2 t3 Core.Type Core.Type Core.Term Core.Term))
passUnit _ = (Flows.pure (Compute.Adapter {
  Compute.adapterIsLossy = False,
  Compute.adapterSource = Core.TypeUnit,
  Compute.adapterTarget = Core.TypeUnit,
  Compute.adapterCoder = Compute.Coder {
    Compute.coderEncode = (\_ -> Flows.pure Core.TermUnit),
    Compute.coderDecode = (\_ -> Flows.pure Core.TermUnit)}}))

-- | Pass through wrapped types
passWrapped :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passWrapped t = ((\x -> case x of
  Core.TypeWrap v1 ->  
    let tname = (Core.wrappedTypeTypeName v1) 
        ot = (Core.wrappedTypeObject v1)
        mapTerm = (\coder -> \dir -> \term -> Flows.bind (withGraphContext (Core__.wrap tname term)) (\unwrapped -> Flows.bind (Utils.encodeDecode dir coder unwrapped) (\newTerm -> Flows.pure (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = tname,
                Core.wrappedTermObject = newTerm})))))
    in (Flows.bind (termAdapter ot) (\adapter -> Flows.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy adapter),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeWrap (Core.WrappedType {
        Core.wrappedTypeTypeName = tname,
        Core.wrappedTypeObject = (Compute.adapterTarget adapter)})),
      Compute.adapterCoder = (Utils.bidirectional (mapTerm (Compute.adapterCoder adapter)))})))) t)

-- | Simplify application types
simplifyApplication :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
simplifyApplication t = ((\x -> case x of
  Core.TypeApplication v1 ->  
    let lhs = (Core.applicationTypeFunction v1)
    in (Flows.bind (termAdapter lhs) (\ad -> Flows.pure (Compute.Adapter {
      Compute.adapterIsLossy = False,
      Compute.adapterSource = t,
      Compute.adapterTarget = (Compute.adapterTarget ad),
      Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> Utils.encodeDecode dir (Compute.adapterCoder ad) term))})))) t)

-- | Create an adapter for any type
termAdapter :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
termAdapter typ =  
  let constraints = (\cx -> Coders.languageConstraints (Coders.adapterContextLanguage cx)) 
      supported = (\cx -> Utils.typeIsSupported (constraints cx))
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
              Mantle.TypeVariantUnit -> [
                passUnit]
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
              Mantle.TypeVariantUnit -> [
                unitToRecord]
              Mantle.TypeVariantWrap -> [
                wrapToUnwrapped]) (Variants.typeVariant t))
      alts = (\cx -> \t -> Flows.mapList (\c -> c t) (Logic.ifElse (supportedAtTopLevel cx t) (pass t) (trySubstitution t)))
  in ((\x -> case x of
    Core.TypeAnnotated v1 -> (Flows.bind (termAdapter (Core.annotatedTypeSubject v1)) (\ad -> Flows.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
      Compute.adapterSource = (Compute.adapterSource ad),
      Compute.adapterTarget = (Core.TypeAnnotated (Core.AnnotatedType {
        Core.annotatedTypeSubject = (Compute.adapterTarget ad),
        Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v1)})),
      Compute.adapterCoder = (Compute.adapterCoder ad)})))
    _ -> (Monads.withTrace (Strings.cat2 "adapter for " (Core_.type_ typ)) ((\x -> case x of
      Core.TypeVariable v1 -> (forTypeReference v1)
      _ -> (Flows.bind Monads.getState (\cx -> Utils.chooseAdapter (alts cx) (supported cx) Core___.type_ Core_.type_ typ))) typ))) typ)

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
                in (Logic.ifElse (Lists.null matches) (Flows.fail (Strings.cat [
                  "cannot convert term back to union: ",
                  Core___.term term,
                  " where type = ",
                  Core___.type_ t,
                  "    and target type = ",
                  (Core___.type_ t_)])) (Flows.pure (Lists.head matches))))
    in (Flows.bind (termAdapter target) (\ad -> Flows.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Compute.adapterTarget ad),
      Compute.adapterCoder = Compute.Coder {
        Compute.coderEncode = (\term_ -> Flows.bind (withGraphContext (Core__.injection (Core.rowTypeTypeName v1) term_)) (\field ->  
          let fn = (Core.fieldName field) 
              term = (Core.fieldTerm field)
          in (Compute.coderEncode (Compute.adapterCoder ad) (Core.TermRecord (Core.Record {
            Core.recordTypeName = nm,
            Core.recordFields = (Lists.map (toRecordField term fn) sfields)}))))),
        Compute.coderDecode = (\term -> Flows.bind (Compute.coderDecode (Compute.adapterCoder ad) term) (\recTerm -> (\x -> case x of
          Core.TermRecord v2 ->  
            let fields = (Core.recordFields v2)
            in (Flows.bind (fromRecordFields term (Core.TermRecord (Core.Record {
              Core.recordTypeName = nm,
              Core.recordFields = fields})) (Compute.adapterTarget ad) fields) (\resultField -> Flows.pure (Core.TermUnion (Core.Injection {
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

unitToRecord :: (t0 -> Compute.Flow t1 (Compute.Adapter t2 t3 Core.Type Core.Type Core.Term Core.Term))
unitToRecord _ = (Flows.pure (Compute.Adapter {
  Compute.adapterIsLossy = False,
  Compute.adapterSource = Core.TypeUnit,
  Compute.adapterTarget = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "_Unit"),
    Core.rowTypeFields = []})),
  Compute.adapterCoder = Compute.Coder {
    Compute.coderEncode = (\_ -> Flows.pure (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "_Unit"),
      Core.recordFields = []}))),
    Compute.coderDecode = (\_ -> Flows.pure Core.TermUnit)}}))

-- | Convert wrapped types to unwrapped types
wrapToUnwrapped :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
wrapToUnwrapped t = ((\x -> case x of
  Core.TypeWrap v1 ->  
    let tname = (Core.wrappedTypeTypeName v1) 
        typ = (Core.wrappedTypeObject v1)
        encode = (\ad -> \term -> Flows.bind (withGraphContext (Core__.wrap tname term)) (\unwrapped -> Compute.coderEncode (Compute.adapterCoder ad) unwrapped))
        decode = (\ad -> \term -> Flows.bind (Compute.coderDecode (Compute.adapterCoder ad) term) (\decoded -> Flows.pure (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = tname,
                Core.wrappedTermObject = decoded}))))
    in (Flows.bind (termAdapter typ) (\ad -> Flows.pure (Compute.Adapter {
      Compute.adapterIsLossy = False,
      Compute.adapterSource = t,
      Compute.adapterTarget = (Compute.adapterTarget ad),
      Compute.adapterCoder = Compute.Coder {
        Compute.coderEncode = (encode ad),
        Compute.coderDecode = (decode ad)}})))) t)

withGraphContext :: (Compute.Flow Graph.Graph t0 -> Compute.Flow Coders.AdapterContext t0)
withGraphContext f = (Flows.bind Monads.getState (\cx -> Monads.withState (Coders.adapterContextGraph cx) f))
