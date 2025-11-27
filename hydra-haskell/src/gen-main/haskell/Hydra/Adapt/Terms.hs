-- Note: this is an automatically generated file. Do not edit.

-- | Adapter framework for types and terms

module Hydra.Adapt.Terms where

import qualified Hydra.Adapt.Literals as Literals
import qualified Hydra.Adapt.Utils as Utils
import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Monads as Monads
import qualified Hydra.Reflect as Reflect
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Show.Core as Core__
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Create an adapter for field types
fieldAdapter :: (Core.FieldType -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.FieldType Core.FieldType Core.Field Core.Field))
fieldAdapter ftyp =  
  let encdec = (\ad -> \dir -> \field ->  
          let name = (Core.fieldName field)
          in  
            let term = (Core.fieldTerm field)
            in (Flows.bind (Utils.encodeDecode dir (Compute.adapterCoder ad) term) (\newTerm -> Flows.pure (Core.Field {
              Core.fieldName = name,
              Core.fieldTerm = newTerm}))))
  in (Flows.bind (termAdapter (Core.fieldTypeType ftyp)) (\ad -> Flows.pure (Compute.Adapter {
    Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
    Compute.adapterSource = ftyp,
    Compute.adapterTarget = Core.FieldType {
      Core.fieldTypeName = (Core.fieldTypeName ftyp),
      Core.fieldTypeType = (Compute.adapterTarget ad)},
    Compute.adapterCoder = (Utils.bidirectional (encdec ad))})))

-- | This function accounts for recursive type definitions
forTypeReference :: (Core.Name -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
forTypeReference name =  
  let encdec = (\name -> \dir -> \term -> Flows.bind Monads.getState (\cx ->  
          let adapters = (Coders.adapterContextAdapters cx)
          in (Maybes.maybe (Flows.fail (Strings.cat2 "no adapter for reference type " (Core.unName name))) (\ad -> Utils.encodeDecode dir (Compute.adapterCoder ad) term) (Maps.lookup name adapters))))
  in  
    let forType = (\cx -> \adapters -> \t -> Flows.bind (termAdapter t) (\actual ->  
            let finalAdapters = (Maps.insert name actual adapters)
            in  
              let finalCx = Coders.AdapterContext {
                      Coders.adapterContextGraph = (Coders.adapterContextGraph cx),
                      Coders.adapterContextLanguage = (Coders.adapterContextLanguage cx),
                      Coders.adapterContextAdapters = finalAdapters}
              in (Flows.bind (Monads.putState finalCx) (\ignored2 -> Flows.pure actual))))
    in  
      let forMissingAdapter = (\cx -> \lossy -> \adapters -> \placeholder ->  
              let newAdapters = (Maps.insert name placeholder adapters)
              in  
                let newCx = Coders.AdapterContext {
                        Coders.adapterContextGraph = (Coders.adapterContextGraph cx),
                        Coders.adapterContextLanguage = (Coders.adapterContextLanguage cx),
                        Coders.adapterContextAdapters = newAdapters}
                in (Flows.bind (Monads.putState newCx) (\ignored -> Flows.bind (withGraphContext (Schemas.resolveType (Core.TypeVariable name))) (\mt -> Maybes.maybe (Flows.pure (Compute.Adapter {
                  Compute.adapterIsLossy = lossy,
                  Compute.adapterSource = (Core.TypeVariable name),
                  Compute.adapterTarget = (Core.TypeVariable name),
                  Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> Flows.pure term))})) (forType cx adapters) mt))))
      in  
        let flow =  
                let lossy = False
                in  
                  let placeholder = Compute.Adapter {
                          Compute.adapterIsLossy = lossy,
                          Compute.adapterSource = (Core.TypeVariable name),
                          Compute.adapterTarget = (Core.TypeVariable name),
                          Compute.adapterCoder = (Utils.bidirectional (encdec name))}
                  in (Flows.bind Monads.getState (\cx ->  
                    let adapters = (Coders.adapterContextAdapters cx)
                    in (Maybes.maybe (forMissingAdapter cx lossy adapters placeholder) Flows.pure (Maps.lookup name adapters))))
        in (Monads.withTrace (Strings.cat2 "adapt named type " (Core.unName name)) flow)

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
functionToUnion t =  
  let encTerm = (\term -> \strippedTerm -> (\x -> case x of
          Core.TermFunction v1 -> ((\x -> case x of
            Core.FunctionElimination v2 -> ((\x -> case x of
              Core.EliminationWrap v3 -> (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = functionProxyName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "wrap"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core.unName v3)))}}))
              Core.EliminationRecord _ -> (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = functionProxyName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "record"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core__.term term)))}}))
              Core.EliminationUnion _ -> (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = functionProxyName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "union"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core__.term term)))}}))) v2)
            Core.FunctionLambda _ -> (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = functionProxyName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lambda"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core__.term term)))}}))
            Core.FunctionPrimitive v2 -> (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = functionProxyName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "primitive"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core.unName v2)))}}))) v1)
          Core.TermVariable v1 -> (Core.TermUnion (Core.Injection {
            Core.injectionTypeName = functionProxyName,
            Core.injectionField = Core.Field {
              Core.fieldName = (Core.Name "variable"),
              Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core.unName v1)))}}))) strippedTerm)
  in  
    let encode = (\ad -> \term ->  
            let strippedTerm = (Rewriting.deannotateTerm term)
            in (Compute.coderEncode (Compute.adapterCoder ad) (encTerm term strippedTerm)))
    in  
      let readFromString = (\term -> Flows.bind (Core_.string term) (\s -> Maybes.maybe (Flows.fail (Strings.cat2 "failed to parse term: " s)) Flows.pure (Core__.readTerm s)))
      in  
        let decode = (\ad -> \term ->  
                let notFound = (\fname -> Flows.fail (Strings.cat2 "unexpected field: " (Core.unName fname)))
                in  
                  let forCases = (\fterm -> withGraphContext (readFromString fterm))
                  in  
                    let forLambda = (\fterm -> withGraphContext (readFromString fterm))
                    in  
                      let forWrapped = (\fterm -> withGraphContext (Flows.map (\s -> Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name s)))) (Core_.string fterm)))
                      in  
                        let forPrimitive = (\fterm -> withGraphContext (Flows.map (\s -> Core.TermFunction (Core.FunctionPrimitive (Core.Name s))) (Core_.string fterm)))
                        in  
                          let forProjection = (\fterm -> withGraphContext (readFromString fterm))
                          in  
                            let forVariable = (\fterm -> withGraphContext (Flows.map (\s -> Core.TermVariable (Core.Name s)) (Core_.string fterm)))
                            in (Flows.bind (Compute.coderDecode (Compute.adapterCoder ad) term) (\injTerm -> Flows.bind (withGraphContext (Core_.injection functionProxyName injTerm)) (\field ->  
                              let fname = (Core.fieldName field)
                              in  
                                let fterm = (Core.fieldTerm field)
                                in (Maybes.fromMaybe (notFound fname) (Maps.lookup fname (Maps.fromList [
                                  (Core.Name "wrap", (forWrapped fterm)),
                                  (Core.Name "record", (forProjection fterm)),
                                  (Core.Name "union", (forCases fterm)),
                                  (Core.Name "lambda", (forLambda fterm)),
                                  (Core.Name "primitive", (forPrimitive fterm)),
                                  (Core.Name "variable", (forVariable fterm))])))))))
        in ((\x -> case x of
          Core.TypeFunction v1 ->  
            let dom = (Core.functionTypeDomain v1)
            in  
              let cod = (Core.functionTypeCodomain v1)
              in  
                let unionType = (Flows.bind (termAdapter dom) (\domAd -> Flows.pure (Core.TypeUnion (Core.RowType {
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

-- | Convert optional types to list types
maybeToList :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
maybeToList t =  
  let encode = (\ad -> \term -> (\x -> case x of
          Core.TermMaybe v1 -> (Maybes.maybe (Flows.pure (Core.TermList [])) (\r -> Flows.bind (Compute.coderEncode (Compute.adapterCoder ad) r) (\encoded -> Flows.pure (Core.TermList [
            encoded]))) v1)) term)
  in  
    let decode = (\ad -> \term -> (\x -> case x of
            Core.TermList v1 -> (Flows.map (\x -> Core.TermMaybe x) (Logic.ifElse (Lists.null v1) (Flows.pure Nothing) (Flows.bind (Compute.coderDecode (Compute.adapterCoder ad) (Lists.head v1)) (\decoded -> Flows.pure (Just decoded)))))) term)
    in ((\x -> case x of
      Core.TypeMaybe v1 -> (Flows.bind (termAdapter v1) (\ad -> Flows.pure (Compute.Adapter {
        Compute.adapterIsLossy = False,
        Compute.adapterSource = t,
        Compute.adapterTarget = (Core.TypeList (Compute.adapterTarget ad)),
        Compute.adapterCoder = Compute.Coder {
          Compute.coderEncode = (encode ad),
          Compute.coderDecode = (decode ad)}})))) t)

-- | Pass through application types
passApplication :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passApplication t =  
  let forApplicationType = (\at ->  
          let lhs = (Core.applicationTypeFunction at)
          in  
            let rhs = (Core.applicationTypeArgument at)
            in (Flows.bind (termAdapter lhs) (\lhsAd -> Flows.bind (termAdapter rhs) (\rhsAd -> Flows.pure (Compute.Adapter {
              Compute.adapterIsLossy = (Logic.or (Compute.adapterIsLossy lhsAd) (Compute.adapterIsLossy rhsAd)),
              Compute.adapterSource = t,
              Compute.adapterTarget = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Compute.adapterTarget lhsAd),
                Core.applicationTypeArgument = (Compute.adapterTarget rhsAd)})),
              Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> Utils.encodeDecode dir (Compute.adapterCoder lhsAd) term))})))))
  in ((\x -> case x of
    Core.TypeApplication v1 -> (forApplicationType v1)) t)

-- | Pass through either types
passEither :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passEither t =  
  let forEitherType = (\et ->  
          let left = (Core.eitherTypeLeft et)
          in  
            let right = (Core.eitherTypeRight et)
            in (Flows.bind (termAdapter left) (\leftAd -> Flows.bind (termAdapter right) (\rightAd -> Flows.pure (Compute.Adapter {
              Compute.adapterIsLossy = (Logic.or (Compute.adapterIsLossy leftAd) (Compute.adapterIsLossy rightAd)),
              Compute.adapterSource = t,
              Compute.adapterTarget = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Compute.adapterTarget leftAd),
                Core.eitherTypeRight = (Compute.adapterTarget rightAd)})),
              Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> Utils.encodeDecode dir (Compute.adapterCoder leftAd) term))})))))
  in ((\x -> case x of
    Core.TypeEither v1 -> (forEitherType v1)) t)

-- | Pass through function types with adaptation
passFunction :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passFunction t =  
  let toCaseAds = (\dom -> \cod -> (\x -> case x of
          Core.TypeUnion v1 -> (Flows.bind (Flows.mapList (\f -> Flows.bind (fieldAdapter (Core.FieldType {
            Core.fieldTypeName = (Core.fieldTypeName f),
            Core.fieldTypeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.fieldTypeType f),
              Core.functionTypeCodomain = cod}))})) (\ad -> Flows.pure (Core.fieldTypeName f, ad))) (Core.rowTypeFields v1)) (\pairs -> Flows.pure (Maps.fromList pairs)))
          _ -> (Flows.pure Maps.empty)) (Rewriting.deannotateType dom))
  in  
    let toOptionAd = (\dom -> \cod -> (\x -> case x of
            Core.TypeMaybe v1 -> (Flows.map Maybes.pure (termAdapter (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = v1,
              Core.functionTypeCodomain = cod}))))
            _ -> (Flows.pure Nothing)) (Rewriting.deannotateType dom))
    in  
      let getCoder = (\caseAds -> \fname -> Maybes.maybe Utils.idCoder Compute.adapterCoder (Maps.lookup fname caseAds))
      in  
        let forElimination = (\dir -> \codAd -> \caseAds -> \e -> (\x -> case x of
                Core.EliminationUnion v1 ->  
                  let n = (Core.caseStatementTypeName v1)
                  in  
                    let def = (Core.caseStatementDefault v1)
                    in  
                      let cases = (Core.caseStatementCases v1)
                      in (Flows.bind (Flows.mapList (\f -> Utils.encodeDecode dir (getCoder caseAds (Core.fieldName f)) f) cases) (\rcases -> Flows.bind (Maybes.maybe (Flows.pure Nothing) (\d -> Flows.map Maybes.pure (Utils.encodeDecode dir (Compute.adapterCoder codAd) d)) def) (\rdef -> Flows.pure (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = n,
                        Core.caseStatementDefault = rdef,
                        Core.caseStatementCases = rcases})))))) e)
        in  
          let forFunction = (\dir -> \codAd -> \caseAds -> \f -> (\x -> case x of
                  Core.FunctionElimination v1 -> (Flows.map (\x -> Core.FunctionElimination x) (forElimination dir codAd caseAds v1))
                  Core.FunctionLambda v1 ->  
                    let var = (Core.lambdaParameter v1)
                    in  
                      let d = (Core.lambdaDomain v1)
                      in  
                        let body = (Core.lambdaBody v1)
                        in (Flows.bind (Utils.encodeDecode dir (Compute.adapterCoder codAd) body) (\newBody -> Flows.pure (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = var,
                          Core.lambdaDomain = d,
                          Core.lambdaBody = newBody}))))
                  Core.FunctionPrimitive v1 -> (Flows.pure (Core.FunctionPrimitive v1))) f)
          in  
            let encdec = (\codAd -> \caseAds -> \dir -> \term -> (\x -> case x of
                    Core.TermFunction v1 -> (Flows.map (\x -> Core.TermFunction x) (forFunction dir codAd caseAds v1))
                    _ -> (Flows.pure term)) (Rewriting.deannotateTerm term))
            in  
              let forFunctionType = (\ft ->  
                      let dom = (Core.functionTypeDomain ft)
                      in  
                        let cod = (Core.functionTypeCodomain ft)
                        in (Flows.bind (termAdapter dom) (\domAd -> Flows.bind (termAdapter cod) (\codAd -> Flows.bind (toCaseAds dom cod) (\caseAds -> Flows.bind (toOptionAd dom cod) (\optionAd ->  
                          let lossy = (Logic.or (Compute.adapterIsLossy codAd) (Lists.foldl Logic.or False (Lists.map (\pair -> Compute.adapterIsLossy (Pairs.second pair)) (Maps.toList caseAds))))
                          in  
                            let target = (Core.TypeFunction (Core.FunctionType {
                                    Core.functionTypeDomain = (Compute.adapterTarget domAd),
                                    Core.functionTypeCodomain = (Compute.adapterTarget codAd)}))
                            in (Flows.pure (Compute.Adapter {
                              Compute.adapterIsLossy = lossy,
                              Compute.adapterSource = t,
                              Compute.adapterTarget = target,
                              Compute.adapterCoder = (Utils.bidirectional (encdec codAd caseAds))}))))))))
              in ((\x -> case x of
                Core.TypeFunction v1 -> (forFunctionType v1)) t)

-- | Pass through forall types
passForall :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passForall t =  
  let forForallType = (\ft ->  
          let v = (Core.forallTypeParameter ft)
          in  
            let body = (Core.forallTypeBody ft)
            in (Flows.bind (termAdapter body) (\ad -> Flows.pure (Compute.Adapter {
              Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
              Compute.adapterSource = t,
              Compute.adapterTarget = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = v,
                Core.forallTypeBody = (Compute.adapterTarget ad)})),
              Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> Utils.encodeDecode dir (Compute.adapterCoder ad) term))}))))
  in ((\x -> case x of
    Core.TypeForall v1 -> (forForallType v1)) t)

-- | Pass through literal types with literal adaptation
passLiteral :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passLiteral t =  
  let encdec = (\ad -> \dir -> \term -> Flows.bind (withGraphContext (Core_.literal term)) (\l -> Flows.bind (Utils.encodeDecode dir (Compute.adapterCoder ad) l) (\l2 -> Flows.pure (Core.TermLiteral l2))))
  in  
    let forLiteral = (\lt -> Flows.bind (Literals.literalAdapter lt) (\ad ->  
            let step = (Utils.bidirectional (encdec ad))
            in (Flows.pure (Compute.Adapter {
              Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
              Compute.adapterSource = (Core.TypeLiteral (Compute.adapterSource ad)),
              Compute.adapterTarget = (Core.TypeLiteral (Compute.adapterTarget ad)),
              Compute.adapterCoder = step}))))
    in ((\x -> case x of
      Core.TypeLiteral v1 -> (forLiteral v1)) t)

-- | Pass through list types
passList :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passList t =  
  let encdec = (\ad -> \dir -> \term -> (\x -> case x of
          Core.TermList v1 -> (Flows.bind (Flows.mapList (Utils.encodeDecode dir (Compute.adapterCoder ad)) v1) (\newTerms -> Flows.pure (Core.TermList newTerms)))) term)
  in  
    let forListType = (\lt -> Flows.bind (termAdapter lt) (\ad -> Flows.pure (Compute.Adapter {
            Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
            Compute.adapterSource = t,
            Compute.adapterTarget = (Core.TypeList (Compute.adapterTarget ad)),
            Compute.adapterCoder = (Utils.bidirectional (encdec ad))})))
    in ((\x -> case x of
      Core.TypeList v1 -> (forListType v1)) t)

-- | Pass through map types
passMap :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passMap t =  
  let encdec = (\kad -> \vad -> \dir -> \term -> (\x -> case x of
          Core.TermMap v1 -> (Flows.bind (Flows.mapList (\pair ->  
            let k = (Pairs.first pair)
            in  
              let v = (Pairs.second pair)
              in (Flows.bind (Utils.encodeDecode dir (Compute.adapterCoder kad) k) (\newK -> Flows.bind (Utils.encodeDecode dir (Compute.adapterCoder vad) v) (\newV -> Flows.pure (newK, newV))))) (Maps.toList v1)) (\newPairs -> Flows.pure (Core.TermMap (Maps.fromList newPairs))))) term)
  in  
    let forMapType = (\mt ->  
            let kt = (Core.mapTypeKeys mt)
            in  
              let vt = (Core.mapTypeValues mt)
              in (Flows.bind (termAdapter kt) (\kad -> Flows.bind (termAdapter vt) (\vad -> Flows.pure (Compute.Adapter {
                Compute.adapterIsLossy = (Logic.or (Compute.adapterIsLossy kad) (Compute.adapterIsLossy vad)),
                Compute.adapterSource = t,
                Compute.adapterTarget = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Compute.adapterTarget kad),
                  Core.mapTypeValues = (Compute.adapterTarget vad)})),
                Compute.adapterCoder = (Utils.bidirectional (encdec kad vad))})))))
    in ((\x -> case x of
      Core.TypeMap v1 -> (forMapType v1)) t)

-- | Pass through optional types
passOptional :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passOptional t =  
  let mapTerm = (\coder -> \dir -> \term -> Flows.bind (withGraphContext (Core_.maybeTerm Flows.pure term)) (\opt -> Flows.bind (Flows.mapMaybe (Utils.encodeDecode dir coder) opt) (\newOpt -> Flows.pure (Core.TermMaybe newOpt))))
  in ((\x -> case x of
    Core.TypeMaybe v1 -> (Flows.bind (termAdapter v1) (\adapter -> Flows.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy adapter),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeMaybe (Compute.adapterTarget adapter)),
      Compute.adapterCoder = (Utils.bidirectional (mapTerm (Compute.adapterCoder adapter)))})))) t)

-- | Pass through product types
passProduct :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passProduct t =  
  let encdec = (\ads -> \dir -> \term -> (\x -> case x of
          Core.TermProduct v1 -> (Flows.bind (Flows.sequence (Lists.zipWith (\term -> \ad -> Utils.encodeDecode dir (Compute.adapterCoder ad) term) v1 ads)) (\newTuple -> Flows.pure (Core.TermProduct newTuple)))) term)
  in ((\x -> case x of
    Core.TypeProduct v1 -> (Flows.bind (Flows.mapList termAdapter v1) (\ads ->  
      let lossy = (Lists.foldl Logic.or False (Lists.map Compute.adapterIsLossy ads))
      in (Flows.pure (Compute.Adapter {
        Compute.adapterIsLossy = lossy,
        Compute.adapterSource = t,
        Compute.adapterTarget = (Core.TypeProduct (Lists.map Compute.adapterTarget ads)),
        Compute.adapterCoder = (Utils.bidirectional (encdec ads))}))))) t)

-- | Pass through record types
passRecord :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passRecord t =  
  let encdec = (\rt -> \adapters -> \dir -> \term -> (\x -> case x of
          Core.TermRecord v1 ->  
            let dfields = (Core.recordFields v1)
            in (Flows.bind (Flows.sequence (Lists.zipWith (\ad -> \f -> Utils.encodeDecode dir (Compute.adapterCoder ad) f) adapters dfields)) (\newFields -> Flows.pure (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.rowTypeTypeName rt),
              Core.recordFields = newFields}))))) term)
  in  
    let forRecordType = (\rt -> Flows.bind (Flows.mapList fieldAdapter (Core.rowTypeFields rt)) (\adapters ->  
            let lossy = (Lists.foldl Logic.or False (Lists.map Compute.adapterIsLossy adapters))
            in  
              let sfields_ = (Lists.map Compute.adapterTarget adapters)
              in (Flows.pure (Compute.Adapter {
                Compute.adapterIsLossy = lossy,
                Compute.adapterSource = t,
                Compute.adapterTarget = (Core.TypeRecord (Core.RowType {
                  Core.rowTypeTypeName = (Core.rowTypeTypeName rt),
                  Core.rowTypeFields = sfields_})),
                Compute.adapterCoder = (Utils.bidirectional (encdec rt adapters))}))))
    in ((\x -> case x of
      Core.TypeRecord v1 -> (forRecordType v1)) t)

-- | Pass through set types
passSet :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passSet t =  
  let encdec = (\ad -> \dir -> \term -> (\x -> case x of
          Core.TermSet v1 -> (Flows.bind (Flows.mapList (Utils.encodeDecode dir (Compute.adapterCoder ad)) (Sets.toList v1)) (\newTerms -> Flows.pure (Core.TermSet (Sets.fromList newTerms))))) term)
  in ((\x -> case x of
    Core.TypeSet v1 -> (Flows.bind (termAdapter v1) (\ad -> Flows.pure (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeSet (Compute.adapterTarget ad)),
      Compute.adapterCoder = (Utils.bidirectional (encdec ad))})))) t)

-- | Pass through sum types
passSum :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passSum t =  
  let encdec = (\ads -> \dir -> \term -> (\x -> case x of
          Core.TermSum v1 ->  
            let i = (Core.sumIndex v1)
            in  
              let n = (Core.sumSize v1)
              in  
                let term = (Core.sumTerm v1)
                in (Flows.bind (Utils.encodeDecode dir (Compute.adapterCoder (Lists.at i ads)) term) (\newTerm -> Flows.pure (Core.TermSum (Core.Sum {
                  Core.sumIndex = i,
                  Core.sumSize = n,
                  Core.sumTerm = newTerm}))))) term)
  in ((\x -> case x of
    Core.TypeSum v1 -> (Flows.bind (Flows.mapList termAdapter v1) (\ads ->  
      let lossy = (Lists.foldl Logic.or False (Lists.map Compute.adapterIsLossy ads))
      in (Flows.pure (Compute.Adapter {
        Compute.adapterIsLossy = lossy,
        Compute.adapterSource = t,
        Compute.adapterTarget = (Core.TypeSum (Lists.map Compute.adapterTarget ads)),
        Compute.adapterCoder = (Utils.bidirectional (encdec ads))}))))) t)

-- | Pass through union types
passUnion :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
passUnion t = ((\x -> case x of
  Core.TypeUnion v1 ->  
    let sfields = (Core.rowTypeFields v1)
    in  
      let tname = (Core.rowTypeTypeName v1)
      in  
        let getAdapter = (\adaptersMap -> \f -> Maybes.maybe (Flows.fail (Strings.cat2 "no such field: " (Core.unName (Core.fieldName f)))) Flows.pure (Maps.lookup (Core.fieldName f) adaptersMap))
        in (Flows.bind (Flows.mapList (\f -> Flows.bind (fieldAdapter f) (\ad -> Flows.pure (Core.fieldTypeName f, ad))) sfields) (\adapters ->  
          let adaptersMap = (Maps.fromList adapters)
          in  
            let lossy = (Lists.foldl Logic.or False (Lists.map (\pair -> Compute.adapterIsLossy (Pairs.second pair)) adapters))
            in  
              let sfields_ = (Lists.map (\pair -> Compute.adapterTarget (Pairs.second pair)) adapters)
              in (Flows.pure (Compute.Adapter {
                Compute.adapterIsLossy = lossy,
                Compute.adapterSource = t,
                Compute.adapterTarget = (Core.TypeUnion (Core.RowType {
                  Core.rowTypeTypeName = tname,
                  Core.rowTypeFields = sfields_})),
                Compute.adapterCoder = (Utils.bidirectional (\dir -> \term -> Flows.pure term))}))))) t)

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
    in  
      let ot = (Core.wrappedTypeBody v1)
      in  
        let mapTerm = (\coder -> \dir -> \term -> Flows.bind (withGraphContext (Core_.wrap tname term)) (\unwrapped -> Flows.bind (Utils.encodeDecode dir coder unwrapped) (\newTerm -> Flows.pure (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = tname,
                Core.wrappedTermBody = newTerm})))))
        in (Flows.bind (termAdapter ot) (\adapter -> Flows.pure (Compute.Adapter {
          Compute.adapterIsLossy = (Compute.adapterIsLossy adapter),
          Compute.adapterSource = t,
          Compute.adapterTarget = (Core.TypeWrap (Core.WrappedType {
            Core.wrappedTypeTypeName = tname,
            Core.wrappedTypeBody = (Compute.adapterTarget adapter)})),
          Compute.adapterCoder = (Utils.bidirectional (mapTerm (Compute.adapterCoder adapter)))})))) t)

-- | Convert set types to list types
setToList :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
setToList t =  
  let encode = (\ad -> \term -> (\x -> case x of
          Core.TermSet v1 -> (Compute.coderEncode (Compute.adapterCoder ad) (Core.TermList (Sets.toList v1)))) term)
  in  
    let forListTerm = (\t -> (\x -> case x of
            Core.TermList v1 -> (Flows.pure (Core.TermSet (Sets.fromList v1)))) t)
    in  
      let decode = (\ad -> \term -> Flows.bind (Compute.coderDecode (Compute.adapterCoder ad) term) (\listTerm -> forListTerm listTerm))
      in  
        let forSetType = (\st -> Flows.bind (termAdapter (Core.TypeList st)) (\ad -> Flows.pure (Compute.Adapter {
                Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
                Compute.adapterSource = t,
                Compute.adapterTarget = (Compute.adapterTarget ad),
                Compute.adapterCoder = Compute.Coder {
                  Compute.coderEncode = (encode ad),
                  Compute.coderDecode = (decode ad)}})))
        in ((\x -> case x of
          Core.TypeSet v1 -> (forSetType v1)) t)

-- | Simplify application types
simplifyApplication :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
simplifyApplication t =  
  let encdec = (\ad -> \dir -> \term -> Utils.encodeDecode dir (Compute.adapterCoder ad) term)
  in  
    let forApplicationType = (\at ->  
            let lhs = (Core.applicationTypeFunction at)
            in (Flows.bind (termAdapter lhs) (\ad -> Flows.pure (Compute.Adapter {
              Compute.adapterIsLossy = False,
              Compute.adapterSource = t,
              Compute.adapterTarget = (Compute.adapterTarget ad),
              Compute.adapterCoder = (Utils.bidirectional (encdec ad))}))))
    in ((\x -> case x of
      Core.TypeApplication v1 -> (forApplicationType v1)) t)

-- | Create an adapter for any type
termAdapter :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
termAdapter typ =  
  let constraints = (\cx -> Coders.languageConstraints (Coders.adapterContextLanguage cx))
  in  
    let supported = (\cx -> Utils.typeIsSupported (constraints cx))
    in  
      let variantIsSupported = (\cx -> \t -> Sets.member (Reflect.typeVariant t) (Coders.languageConstraintsTypeVariants (constraints cx)))
      in  
        let supportedAtTopLevel = (\cx -> \t -> Logic.and (variantIsSupported cx t) (Coders.languageConstraintsTypes (constraints cx) t))
        in  
          let pass = (\t -> (\x -> case x of
                  Variants.TypeVariantAnnotated -> []
                  Variants.TypeVariantApplication -> [
                    passApplication]
                  Variants.TypeVariantEither -> [
                    passEither]
                  Variants.TypeVariantForall -> [
                    passForall]
                  Variants.TypeVariantFunction -> [
                    passFunction]
                  Variants.TypeVariantList -> [
                    passList]
                  Variants.TypeVariantLiteral -> [
                    passLiteral]
                  Variants.TypeVariantMap -> [
                    passMap]
                  Variants.TypeVariantMaybe -> [
                    passOptional,
                    maybeToList]
                  Variants.TypeVariantPair -> []
                  Variants.TypeVariantProduct -> [
                    passProduct]
                  Variants.TypeVariantRecord -> [
                    passRecord]
                  Variants.TypeVariantSet -> [
                    passSet]
                  Variants.TypeVariantSum -> [
                    passSum]
                  Variants.TypeVariantUnion -> [
                    passUnion]
                  Variants.TypeVariantUnit -> [
                    passUnit]
                  Variants.TypeVariantVariable -> []
                  Variants.TypeVariantWrap -> [
                    passWrapped]) (Reflect.typeVariant (Rewriting.deannotateType t)))
          in  
            let trySubstitution = (\t -> (\x -> case x of
                    Variants.TypeVariantAnnotated -> []
                    Variants.TypeVariantApplication -> [
                      simplifyApplication]
                    Variants.TypeVariantEither -> []
                    Variants.TypeVariantForall -> [
                      lambdaToMonotype]
                    Variants.TypeVariantFunction -> [
                      functionToUnion]
                    Variants.TypeVariantList -> []
                    Variants.TypeVariantLiteral -> []
                    Variants.TypeVariantMap -> []
                    Variants.TypeVariantMaybe -> [
                      maybeToList]
                    Variants.TypeVariantPair -> []
                    Variants.TypeVariantProduct -> []
                    Variants.TypeVariantRecord -> []
                    Variants.TypeVariantSet -> [
                      setToList]
                    Variants.TypeVariantSum -> []
                    Variants.TypeVariantUnion -> [
                      unionToRecord]
                    Variants.TypeVariantUnit -> [
                      unitToRecord]
                    Variants.TypeVariantVariable -> []
                    Variants.TypeVariantWrap -> [
                      wrapToUnwrapped]) (Reflect.typeVariant t))
            in  
              let alts = (\cx -> \t -> Flows.mapList (\c -> c t) (Logic.ifElse (supportedAtTopLevel cx t) (pass t) (trySubstitution t)))
              in  
                let dflt = ((\x -> case x of
                        Core.TypeVariable v1 -> (forTypeReference v1)
                        _ -> (Flows.bind Monads.getState (\cx -> Utils.chooseAdapter (alts cx) (supported cx) Core__.type_ Core__.type_ typ))) typ)
                in ((\x -> case x of
                  Core.TypeAnnotated v1 -> (Flows.bind (termAdapter (Core.annotatedTypeBody v1)) (\ad -> Flows.pure (Compute.Adapter {
                    Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
                    Compute.adapterSource = (Compute.adapterSource ad),
                    Compute.adapterTarget = (Core.TypeAnnotated (Core.AnnotatedType {
                      Core.annotatedTypeBody = (Compute.adapterTarget ad),
                      Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v1)})),
                    Compute.adapterCoder = (Compute.adapterCoder ad)})))
                  _ -> (Monads.withTrace (Strings.cat2 "adapter for " (Core__.type_ typ)) dflt)) typ)

-- | Convert union types to record types
unionToRecord :: (Core.Type -> Compute.Flow Coders.AdapterContext (Compute.Adapter Coders.AdapterContext Coders.AdapterContext Core.Type Core.Type Core.Term Core.Term))
unionToRecord t =  
  let forField = (\field ->  
          let fn = (Core.fieldName field)
          in  
            let fterm = (Core.fieldTerm field)
            in ((\x -> case x of
              Core.TermMaybe v1 -> (Maybes.bind v1 (\t -> Just (Core.Field {
                Core.fieldName = fn,
                Core.fieldTerm = t})))) fterm))
  in  
    let fromRecordFields = (\term -> \term_ -> \t_ -> \fields ->  
            let matches = (Maybes.mapMaybe forField fields)
            in (Logic.ifElse (Lists.null matches) (Flows.fail (Strings.cat [
              "cannot convert term back to union: ",
              Core__.term term,
              " where type = ",
              Core__.type_ t,
              "    and target type = ",
              (Core__.type_ t_)])) (Flows.pure (Lists.head matches))))
    in  
      let forRecTerm = (\nm -> \ad -> \term -> \recTerm -> (\x -> case x of
              Core.TermRecord v1 ->  
                let fields = (Core.recordFields v1)
                in (Flows.bind (fromRecordFields term (Core.TermRecord (Core.Record {
                  Core.recordTypeName = nm,
                  Core.recordFields = fields})) (Compute.adapterTarget ad) fields) (\resultField -> Flows.pure (Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = nm,
                  Core.injectionField = resultField}))))) recTerm)
      in ((\x -> case x of
        Core.TypeUnion v1 ->  
          let nm = (Core.rowTypeTypeName v1)
          in  
            let sfields = (Core.rowTypeFields v1)
            in  
              let target = (Core.TypeRecord (unionTypeToRecordType v1))
              in  
                let toRecordField = (\term -> \fn -> \f ->  
                        let fn_ = (Core.fieldTypeName f)
                        in Core.Field {
                          Core.fieldName = fn_,
                          Core.fieldTerm = (Core.TermMaybe (Logic.ifElse (Equality.equal fn_ fn) (Just term) Nothing))})
                in (Flows.bind (termAdapter target) (\ad -> Flows.pure (Compute.Adapter {
                  Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
                  Compute.adapterSource = t,
                  Compute.adapterTarget = (Compute.adapterTarget ad),
                  Compute.adapterCoder = Compute.Coder {
                    Compute.coderEncode = (\term_ -> Flows.bind (withGraphContext (Core_.injection (Core.rowTypeTypeName v1) term_)) (\field ->  
                      let fn = (Core.fieldName field)
                      in  
                        let term = (Core.fieldTerm field)
                        in (Compute.coderEncode (Compute.adapterCoder ad) (Core.TermRecord (Core.Record {
                          Core.recordTypeName = nm,
                          Core.recordFields = (Lists.map (toRecordField term fn) sfields)}))))),
                    Compute.coderDecode = (\term -> Flows.bind (Compute.coderDecode (Compute.adapterCoder ad) term) (\recTerm -> forRecTerm nm ad term recTerm))}})))) t)

-- | Convert a union row type to a record row type
unionTypeToRecordType :: (Core.RowType -> Core.RowType)
unionTypeToRecordType rt =  
  let makeOptional = (\f ->  
          let fn = (Core.fieldTypeName f)
          in  
            let ft = (Core.fieldTypeType f)
            in Core.FieldType {
              Core.fieldTypeName = fn,
              Core.fieldTypeType = (Rewriting.mapBeneathTypeAnnotations (\x -> Core.TypeMaybe x) ft)})
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
    in  
      let typ = (Core.wrappedTypeBody v1)
      in  
        let encode = (\ad -> \term -> Flows.bind (withGraphContext (Core_.wrap tname term)) (\unwrapped -> Compute.coderEncode (Compute.adapterCoder ad) unwrapped))
        in  
          let decode = (\ad -> \term -> Flows.bind (Compute.coderDecode (Compute.adapterCoder ad) term) (\decoded -> Flows.pure (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = tname,
                  Core.wrappedTermBody = decoded}))))
          in (Flows.bind (termAdapter typ) (\ad -> Flows.pure (Compute.Adapter {
            Compute.adapterIsLossy = False,
            Compute.adapterSource = t,
            Compute.adapterTarget = (Compute.adapterTarget ad),
            Compute.adapterCoder = Compute.Coder {
              Compute.coderEncode = (encode ad),
              Compute.coderDecode = (decode ad)}})))) t)

withGraphContext :: (Compute.Flow Graph.Graph t0 -> Compute.Flow Coders.AdapterContext t0)
withGraphContext f = (Flows.bind Monads.getState (\cx -> Monads.withState (Coders.adapterContextGraph cx) f))
