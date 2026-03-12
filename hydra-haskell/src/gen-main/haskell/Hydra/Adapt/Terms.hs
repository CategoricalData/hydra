-- Note: this is an automatically generated file. Do not edit.

-- | Adapter framework for types and terms

module Hydra.Adapt.Terms where

import qualified Hydra.Adapt.Literals as Literals
import qualified Hydra.Adapt.Utils as Utils
import qualified Hydra.CoderUtils as CoderUtils
import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Reflect as Reflect
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Show.Core as Core__
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Create an adapter for field types
fieldAdapter :: (Coders.AdapterContext -> Core.FieldType -> Either String (Compute.Adapter Core.FieldType Core.FieldType Core.Field Core.Field))
fieldAdapter cx ftyp =  
  let encdec = (\ad -> \dir -> \cx -> \field ->  
          let name = (Core.fieldName field)
          in  
            let term = (Core.fieldTerm field)
            in (Eithers.bind (Utils.encodeDecode dir (Compute.adapterCoder ad) cx term) (\newTerm -> Right (Core.Field {
              Core.fieldName = name,
              Core.fieldTerm = newTerm}))))
  in (Eithers.bind (termAdapter cx (Core.fieldTypeType ftyp)) (\ad -> Right (Compute.Adapter {
    Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
    Compute.adapterSource = ftyp,
    Compute.adapterTarget = Core.FieldType {
      Core.fieldTypeName = (Core.fieldTypeName ftyp),
      Core.fieldTypeType = (Compute.adapterTarget ad)},
    Compute.adapterCoder = (Utils.bidirectional (encdec ad))})))

-- | This function accounts for recursive type definitions
forTypeReference :: (Coders.AdapterContext -> Core.Name -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
forTypeReference cx name =  
  let encdec = (\name -> \adapters0 -> \dir -> \cx -> \term -> Maybes.maybe (Left (Context.InContext {
          Context.inContextObject = (Error.OtherError (Strings.cat2 "no adapter for reference type " (Core.unName name))),
          Context.inContextContext = cx})) (\ad -> Utils.encodeDecode dir (Compute.adapterCoder ad) cx term) (Maps.lookup name adapters0))
  in  
    let forType = (\cx2 -> \adapters0 -> \t -> Eithers.bind (termAdapter cx2 t) (\actual -> Right actual))
    in  
      let forMissingAdapter = (\cx2 -> \lossy -> \adapters0 -> \placeholder ->  
              let newAdapters = (Maps.insert name placeholder adapters0)
              in  
                let newCx = Coders.AdapterContext {
                        Coders.adapterContextGraph = (Coders.adapterContextGraph cx2),
                        Coders.adapterContextLanguage = (Coders.adapterContextLanguage cx2),
                        Coders.adapterContextAdapters = newAdapters}
                in  
                  let mt = (Schemas.resolveType (Coders.adapterContextGraph newCx) (Core.TypeVariable name))
                  in (Maybes.maybe (Right (Compute.Adapter {
                    Compute.adapterIsLossy = lossy,
                    Compute.adapterSource = (Core.TypeVariable name),
                    Compute.adapterTarget = (Core.TypeVariable name),
                    Compute.adapterCoder = (Utils.bidirectional (\dir -> \_cx -> \term -> Right term))})) (forType cx2 adapters0) mt))
      in  
        let lossy = False
        in  
          let adapters = (Coders.adapterContextAdapters cx)
          in  
            let placeholder = Compute.Adapter {
                    Compute.adapterIsLossy = lossy,
                    Compute.adapterSource = (Core.TypeVariable name),
                    Compute.adapterTarget = (Core.TypeVariable name),
                    Compute.adapterCoder = (Utils.bidirectional (encdec name adapters))}
            in (Maybes.maybe (forMissingAdapter cx lossy adapters placeholder) (\x -> Right x) (Maps.lookup name adapters))

functionProxyName :: Core.Name
functionProxyName = (Core.Name "hydra.core.FunctionProxy")

-- | Generate a function proxy type for a given domain type
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
functionToUnion :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
functionToUnion cx t =  
  let encTerm = (\term -> \strippedTerm -> (\x -> case x of
          Core.TermFunction v0 -> ((\x -> case x of
            Core.FunctionElimination v1 -> ((\x -> case x of
              Core.EliminationWrap v2 -> (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = functionProxyName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "wrap"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core.unName v2)))}}))
              Core.EliminationRecord _ -> (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = functionProxyName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "record"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core__.term term)))}}))
              Core.EliminationUnion _ -> (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = functionProxyName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "union"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core__.term term)))}}))) v1)
            Core.FunctionLambda _ -> (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = functionProxyName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lambda"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core__.term term)))}}))
            Core.FunctionPrimitive v1 -> (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = functionProxyName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "primitive"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core.unName v1)))}}))) v0)
          Core.TermVariable v0 -> (Core.TermUnion (Core.Injection {
            Core.injectionTypeName = functionProxyName,
            Core.injectionField = Core.Field {
              Core.fieldName = (Core.Name "variable"),
              Core.fieldTerm = (Core.TermLiteral (Core.LiteralString (Core.unName v0)))}}))) strippedTerm)
  in  
    let encode = (\ad -> \cx -> \term ->  
            let strippedTerm = (Rewriting.deannotateTerm term)
            in (Compute.coderEncode (Compute.adapterCoder ad) cx (encTerm term strippedTerm)))
    in  
      let readFromString = (\cx -> \graph -> \term -> Eithers.bind (Core_.string cx graph term) (\s -> Maybes.maybe (Left (Context.InContext {
              Context.inContextObject = (Error.OtherError (Strings.cat2 "failed to parse term: " s)),
              Context.inContextContext = cx})) (\x -> Right x) (Core__.readTerm s)))
      in  
        let decode = (\graph -> \ad -> \cx -> \term ->  
                let notFound = (\fname -> Left (Context.InContext {
                        Context.inContextObject = (Error.OtherError (Strings.cat2 "unexpected field: " (Core.unName fname))),
                        Context.inContextContext = cx}))
                in  
                  let forCases = (\fterm -> readFromString cx graph fterm)
                  in  
                    let forLambda = (\fterm -> readFromString cx graph fterm)
                    in  
                      let forWrapped = (\fterm -> Eithers.bind (Core_.string cx graph fterm) (\s -> Right (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name s))))))
                      in  
                        let forPrimitive = (\fterm -> Eithers.bind (Core_.string cx graph fterm) (\s -> Right (Core.TermFunction (Core.FunctionPrimitive (Core.Name s)))))
                        in  
                          let forProjection = (\fterm -> readFromString cx graph fterm)
                          in  
                            let forVariable = (\fterm -> Eithers.bind (Core_.string cx graph fterm) (\s -> Right (Core.TermVariable (Core.Name s))))
                            in (Eithers.bind (Compute.coderDecode (Compute.adapterCoder ad) cx term) (\injTerm -> Eithers.bind (Core_.injection cx functionProxyName graph injTerm) (\field ->  
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
          Core.TypeFunction v0 ->  
            let dom = (Core.functionTypeDomain v0)
            in  
              let cod = (Core.functionTypeCodomain v0)
              in  
                let unionType = (Eithers.bind (termAdapter cx dom) (\domAd -> Right (Core.TypeUnion (Core.RowType {
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
                in (Eithers.bind unionType (\ut -> Eithers.bind (termAdapter cx ut) (\ad ->  
                  let graph = (Coders.adapterContextGraph cx)
                  in (Right (Compute.Adapter {
                    Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
                    Compute.adapterSource = t,
                    Compute.adapterTarget = (Compute.adapterTarget ad),
                    Compute.adapterCoder = Compute.Coder {
                      Compute.coderEncode = (encode ad),
                      Compute.coderDecode = (decode graph ad)}})))))) t)

-- | Convert forall types to monotypes
lambdaToMonotype :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
lambdaToMonotype cx t = ((\x -> case x of
  Core.TypeForall v0 ->  
    let body = (Core.forallTypeBody v0)
    in (Eithers.bind (termAdapter cx body) (\ad -> Right (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Compute.adapterTarget ad),
      Compute.adapterCoder = (Compute.adapterCoder ad)})))) t)

-- | Convert optional types to list types
maybeToList :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
maybeToList cx t =  
  let encode = (\ad -> \cx -> \term -> (\x -> case x of
          Core.TermMaybe v0 -> (Maybes.maybe (Right (Core.TermList [])) (\r -> Eithers.bind (Utils.encodeDecode Coders.CoderDirectionEncode (Compute.adapterCoder ad) cx r) (\encoded -> Right (Core.TermList [
            encoded]))) v0)) term)
  in  
    let decode = (\ad -> \cx -> \term -> (\x -> case x of
            Core.TermList v0 -> (Eithers.map (\x -> Core.TermMaybe x) (Logic.ifElse (Lists.null v0) (Right Nothing) (Eithers.bind (Utils.encodeDecode Coders.CoderDirectionDecode (Compute.adapterCoder ad) cx (Lists.head v0)) (\decoded -> Right (Just decoded)))))) term)
    in ((\x -> case x of
      Core.TypeMaybe v0 -> (Eithers.bind (termAdapter cx v0) (\ad -> Right (Compute.Adapter {
        Compute.adapterIsLossy = False,
        Compute.adapterSource = t,
        Compute.adapterTarget = (Core.TypeList (Compute.adapterTarget ad)),
        Compute.adapterCoder = Compute.Coder {
          Compute.coderEncode = (encode ad),
          Compute.coderDecode = (decode ad)}})))) t)

-- | Pass through application types
passApplication :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
passApplication cx t =  
  let forApplicationType = (\at ->  
          let lhs = (Core.applicationTypeFunction at)
          in  
            let rhs = (Core.applicationTypeArgument at)
            in (Eithers.bind (termAdapter cx lhs) (\lhsAd -> Eithers.bind (termAdapter cx rhs) (\rhsAd -> Right (Compute.Adapter {
              Compute.adapterIsLossy = (Logic.or (Compute.adapterIsLossy lhsAd) (Compute.adapterIsLossy rhsAd)),
              Compute.adapterSource = t,
              Compute.adapterTarget = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Compute.adapterTarget lhsAd),
                Core.applicationTypeArgument = (Compute.adapterTarget rhsAd)})),
              Compute.adapterCoder = (Utils.bidirectional (\dir -> \cx -> \term -> Utils.encodeDecode dir (Compute.adapterCoder lhsAd) cx term))})))))
  in ((\x -> case x of
    Core.TypeApplication v0 -> (forApplicationType v0)) t)

-- | Pass through either types
passEither :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
passEither cx t =  
  let forEitherType = (\et ->  
          let left = (Core.eitherTypeLeft et)
          in  
            let right = (Core.eitherTypeRight et)
            in (Eithers.bind (termAdapter cx left) (\leftAd -> Eithers.bind (termAdapter cx right) (\rightAd -> Right (Compute.Adapter {
              Compute.adapterIsLossy = (Logic.or (Compute.adapterIsLossy leftAd) (Compute.adapterIsLossy rightAd)),
              Compute.adapterSource = t,
              Compute.adapterTarget = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Compute.adapterTarget leftAd),
                Core.eitherTypeRight = (Compute.adapterTarget rightAd)})),
              Compute.adapterCoder = (Utils.bidirectional (\dir -> \cx -> \term -> Utils.encodeDecode dir (Compute.adapterCoder leftAd) cx term))})))))
  in ((\x -> case x of
    Core.TypeEither v0 -> (forEitherType v0)) t)

-- | Pass through function types with adaptation
passFunction :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
passFunction cx t =  
  let toCaseAds = (\dom -> \cod -> (\x -> case x of
          Core.TypeUnion v0 -> (Eithers.bind (Eithers.mapList (\f -> Eithers.bind (fieldAdapter cx (Core.FieldType {
            Core.fieldTypeName = (Core.fieldTypeName f),
            Core.fieldTypeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.fieldTypeType f),
              Core.functionTypeCodomain = cod}))})) (\ad -> Right (Core.fieldTypeName f, ad))) (Core.rowTypeFields v0)) (\pairs -> Right (Maps.fromList pairs)))
          _ -> (Right Maps.empty)) (Rewriting.deannotateType dom))
  in  
    let toOptionAd = (\dom -> \cod -> (\x -> case x of
            Core.TypeMaybe v0 -> (Eithers.map Maybes.pure (termAdapter cx (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = v0,
              Core.functionTypeCodomain = cod}))))
            _ -> (Right Nothing)) (Rewriting.deannotateType dom))
    in  
      let getCoder = (\caseAds -> \fname -> Maybes.maybe Utils.idCoder Compute.adapterCoder (Maps.lookup fname caseAds))
      in  
        let forElimination = (\dir -> \cx -> \codAd -> \caseAds -> \e -> (\x -> case x of
                Core.EliminationUnion v0 ->  
                  let n = (Core.caseStatementTypeName v0)
                  in  
                    let def = (Core.caseStatementDefault v0)
                    in  
                      let cases = (Core.caseStatementCases v0)
                      in (Eithers.bind (Eithers.mapList (\f -> Utils.encodeDecode dir (getCoder caseAds (Core.fieldName f)) cx f) cases) (\rcases -> Eithers.bind (Eithers.mapMaybe (\d -> Utils.encodeDecode dir (Compute.adapterCoder codAd) cx d) def) (\rdef -> Right (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = n,
                        Core.caseStatementDefault = rdef,
                        Core.caseStatementCases = rcases})))))) e)
        in  
          let forFunction = (\dir -> \cx -> \codAd -> \caseAds -> \f -> (\x -> case x of
                  Core.FunctionElimination v0 -> (Eithers.map (\x -> Core.FunctionElimination x) (forElimination dir cx codAd caseAds v0))
                  Core.FunctionLambda v0 ->  
                    let var = (Core.lambdaParameter v0)
                    in  
                      let d = (Core.lambdaDomain v0)
                      in  
                        let body = (Core.lambdaBody v0)
                        in (Eithers.bind (Utils.encodeDecode dir (Compute.adapterCoder codAd) cx body) (\newBody -> Right (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = var,
                          Core.lambdaDomain = d,
                          Core.lambdaBody = newBody}))))
                  Core.FunctionPrimitive v0 -> (Right (Core.FunctionPrimitive v0))) f)
          in  
            let encdec = (\codAd -> \caseAds -> \dir -> \cx -> \term -> (\x -> case x of
                    Core.TermFunction v0 -> (Eithers.map (\x -> Core.TermFunction x) (forFunction dir cx codAd caseAds v0))
                    _ -> (Right term)) (Rewriting.deannotateTerm term))
            in  
              let forFunctionType = (\ft ->  
                      let dom = (Core.functionTypeDomain ft)
                      in  
                        let cod = (Core.functionTypeCodomain ft)
                        in (Eithers.bind (termAdapter cx dom) (\domAd -> Eithers.bind (termAdapter cx cod) (\codAd -> Eithers.bind (toCaseAds dom cod) (\caseAds -> Eithers.bind (toOptionAd dom cod) (\optionAd ->  
                          let lossy = (Logic.or (Compute.adapterIsLossy codAd) (Lists.foldl Logic.or False (Lists.map (\pair -> Compute.adapterIsLossy (Pairs.second pair)) (Maps.toList caseAds))))
                          in  
                            let target = (Core.TypeFunction (Core.FunctionType {
                                    Core.functionTypeDomain = (Compute.adapterTarget domAd),
                                    Core.functionTypeCodomain = (Compute.adapterTarget codAd)}))
                            in (Right (Compute.Adapter {
                              Compute.adapterIsLossy = lossy,
                              Compute.adapterSource = t,
                              Compute.adapterTarget = target,
                              Compute.adapterCoder = (Utils.bidirectional (encdec codAd caseAds))}))))))))
              in ((\x -> case x of
                Core.TypeFunction v0 -> (forFunctionType v0)) t)

-- | Pass through forall types
passForall :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
passForall cx t =  
  let forForallType = (\ft ->  
          let v = (Core.forallTypeParameter ft)
          in  
            let body = (Core.forallTypeBody ft)
            in (Eithers.bind (termAdapter cx body) (\ad -> Right (Compute.Adapter {
              Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
              Compute.adapterSource = t,
              Compute.adapterTarget = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = v,
                Core.forallTypeBody = (Compute.adapterTarget ad)})),
              Compute.adapterCoder = (Utils.bidirectional (\dir -> \cx -> \term -> Utils.encodeDecode dir (Compute.adapterCoder ad) cx term))}))))
  in ((\x -> case x of
    Core.TypeForall v0 -> (forForallType v0)) t)

-- | Pass through literal types with literal adaptation
passLiteral :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
passLiteral cx t =  
  let encdec = (\graph -> \ad -> \dir -> \cx -> \term -> Eithers.bind (Core_.literal cx graph term) (\l -> Eithers.bind (Utils.encodeDecode dir (Compute.adapterCoder ad) cx l) (\l2 -> Right (Core.TermLiteral l2))))
  in  
    let forLiteral = (\lt -> Eithers.bind (Literals.literalAdapter cx lt) (\ad ->  
            let step = (Utils.bidirectional (encdec (Coders.adapterContextGraph cx) ad))
            in (Right (Compute.Adapter {
              Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
              Compute.adapterSource = (Core.TypeLiteral (Compute.adapterSource ad)),
              Compute.adapterTarget = (Core.TypeLiteral (Compute.adapterTarget ad)),
              Compute.adapterCoder = step}))))
    in ((\x -> case x of
      Core.TypeLiteral v0 -> (forLiteral v0)) t)

-- | Pass through list types
passList :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
passList cx t =  
  let encdec = (\ad -> \dir -> \cx -> \term -> (\x -> case x of
          Core.TermList v0 -> (Eithers.bind (Eithers.mapList (Utils.encodeDecode dir (Compute.adapterCoder ad) cx) v0) (\newTerms -> Right (Core.TermList newTerms)))) term)
  in  
    let forListType = (\lt -> Eithers.bind (termAdapter cx lt) (\ad -> Right (Compute.Adapter {
            Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
            Compute.adapterSource = t,
            Compute.adapterTarget = (Core.TypeList (Compute.adapterTarget ad)),
            Compute.adapterCoder = (Utils.bidirectional (encdec ad))})))
    in ((\x -> case x of
      Core.TypeList v0 -> (forListType v0)) t)

-- | Pass through map types
passMap :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
passMap cx t =  
  let encdec = (\kad -> \vad -> \dir -> \cx -> \term -> (\x -> case x of
          Core.TermMap v0 -> (Eithers.bind (Eithers.mapList (\pair ->  
            let k = (Pairs.first pair)
            in  
              let v = (Pairs.second pair)
              in (Eithers.bind (Utils.encodeDecode dir (Compute.adapterCoder kad) cx k) (\newK -> Eithers.bind (Utils.encodeDecode dir (Compute.adapterCoder vad) cx v) (\newV -> Right (newK, newV))))) (Maps.toList v0)) (\newPairs -> Right (Core.TermMap (Maps.fromList newPairs))))) term)
  in  
    let forMapType = (\mt ->  
            let kt = (Core.mapTypeKeys mt)
            in  
              let vt = (Core.mapTypeValues mt)
              in (Eithers.bind (termAdapter cx kt) (\kad -> Eithers.bind (termAdapter cx vt) (\vad -> Right (Compute.Adapter {
                Compute.adapterIsLossy = (Logic.or (Compute.adapterIsLossy kad) (Compute.adapterIsLossy vad)),
                Compute.adapterSource = t,
                Compute.adapterTarget = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Compute.adapterTarget kad),
                  Core.mapTypeValues = (Compute.adapterTarget vad)})),
                Compute.adapterCoder = (Utils.bidirectional (encdec kad vad))})))))
    in ((\x -> case x of
      Core.TypeMap v0 -> (forMapType v0)) t)

-- | Pass through optional types
passOptional :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
passOptional cx t =  
  let mapTerm = (\graph -> \coder -> \dir -> \cx -> \term -> Eithers.bind (Core_.maybeTerm cx (\x -> Right x) graph term) (\opt -> Eithers.bind (Eithers.mapMaybe (Utils.encodeDecode dir coder cx) opt) (\newOpt -> Right (Core.TermMaybe newOpt))))
  in ((\x -> case x of
    Core.TypeMaybe v0 -> (Eithers.bind (termAdapter cx v0) (\adapter -> Right (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy adapter),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeMaybe (Compute.adapterTarget adapter)),
      Compute.adapterCoder = (Utils.bidirectional (mapTerm (Coders.adapterContextGraph cx) (Compute.adapterCoder adapter)))})))) t)

-- | Pass through record types
passRecord :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
passRecord cx t =  
  let encdec = (\rt -> \adapters -> \dir -> \cx -> \term -> (\x -> case x of
          Core.TermRecord v0 ->  
            let dfields = (Core.recordFields v0)
            in (Eithers.bind (Eithers.mapList (\p -> Utils.encodeDecode dir (Compute.adapterCoder (Pairs.first p)) cx (Pairs.second p)) (Lists.zip adapters dfields)) (\newFields -> Right (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.rowTypeTypeName rt),
              Core.recordFields = newFields}))))) term)
  in  
    let forRecordType = (\rt -> Eithers.bind (Eithers.mapList (fieldAdapter cx) (Core.rowTypeFields rt)) (\adapters ->  
            let lossy = (Lists.foldl Logic.or False (Lists.map Compute.adapterIsLossy adapters))
            in  
              let sfields_ = (Lists.map Compute.adapterTarget adapters)
              in (Right (Compute.Adapter {
                Compute.adapterIsLossy = lossy,
                Compute.adapterSource = t,
                Compute.adapterTarget = (Core.TypeRecord (Core.RowType {
                  Core.rowTypeTypeName = (Core.rowTypeTypeName rt),
                  Core.rowTypeFields = sfields_})),
                Compute.adapterCoder = (Utils.bidirectional (encdec rt adapters))}))))
    in ((\x -> case x of
      Core.TypeRecord v0 -> (forRecordType v0)) t)

-- | Pass through set types
passSet :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
passSet cx t =  
  let encdec = (\ad -> \dir -> \cx -> \term -> (\x -> case x of
          Core.TermSet v0 -> (Eithers.bind (Eithers.mapList (Utils.encodeDecode dir (Compute.adapterCoder ad) cx) (Sets.toList v0)) (\newTerms -> Right (Core.TermSet (Sets.fromList newTerms))))) term)
  in ((\x -> case x of
    Core.TypeSet v0 -> (Eithers.bind (termAdapter cx v0) (\ad -> Right (Compute.Adapter {
      Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
      Compute.adapterSource = t,
      Compute.adapterTarget = (Core.TypeSet (Compute.adapterTarget ad)),
      Compute.adapterCoder = (Utils.bidirectional (encdec ad))})))) t)

-- | Pass through union types
passUnion :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
passUnion cx t = ((\x -> case x of
  Core.TypeUnion v0 ->  
    let sfields = (Core.rowTypeFields v0)
    in  
      let tname = (Core.rowTypeTypeName v0)
      in (Eithers.bind (Eithers.mapList (\f -> Eithers.bind (fieldAdapter cx f) (\ad -> Right (Core.fieldTypeName f, ad))) sfields) (\adapters ->  
        let adaptersMap = (Maps.fromList adapters)
        in  
          let lossy = (Lists.foldl Logic.or False (Lists.map (\pair -> Compute.adapterIsLossy (Pairs.second pair)) adapters))
          in  
            let sfields_ = (Lists.map (\pair -> Compute.adapterTarget (Pairs.second pair)) adapters)
            in (Right (Compute.Adapter {
              Compute.adapterIsLossy = lossy,
              Compute.adapterSource = t,
              Compute.adapterTarget = (Core.TypeUnion (Core.RowType {
                Core.rowTypeTypeName = tname,
                Core.rowTypeFields = sfields_})),
              Compute.adapterCoder = (Utils.bidirectional (\dir -> \_cx -> \term -> Right term))}))))) t)

-- | Pass through unit types
passUnit :: (t0 -> t1 -> Either t2 (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
passUnit _cx _ = (Right (Compute.Adapter {
  Compute.adapterIsLossy = False,
  Compute.adapterSource = Core.TypeUnit,
  Compute.adapterTarget = Core.TypeUnit,
  Compute.adapterCoder = Compute.Coder {
    Compute.coderEncode = (\_cx -> \_ -> Right Core.TermUnit),
    Compute.coderDecode = (\_cx -> \_ -> Right Core.TermUnit)}}))

-- | Pass through wrapped types
passWrapped :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
passWrapped cx t = ((\x -> case x of
  Core.TypeWrap v0 ->  
    let tname = (Core.wrappedTypeTypeName v0)
    in  
      let ot = (Core.wrappedTypeBody v0)
      in  
        let mapTerm = (\graph -> \coder -> \dir -> \cx -> \term -> Eithers.bind (Core_.wrap cx tname graph term) (\unwrapped -> Eithers.bind (Utils.encodeDecode dir coder cx unwrapped) (\newTerm -> Right (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = tname,
                Core.wrappedTermBody = newTerm})))))
        in (Eithers.bind (termAdapter cx ot) (\adapter -> Right (Compute.Adapter {
          Compute.adapterIsLossy = (Compute.adapterIsLossy adapter),
          Compute.adapterSource = t,
          Compute.adapterTarget = (Core.TypeWrap (Core.WrappedType {
            Core.wrappedTypeTypeName = tname,
            Core.wrappedTypeBody = (Compute.adapterTarget adapter)})),
          Compute.adapterCoder = (Utils.bidirectional (mapTerm (Coders.adapterContextGraph cx) (Compute.adapterCoder adapter)))})))) t)

-- | Convert set types to list types
setToList :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
setToList cx t =  
  let encode = (\ad -> \cx -> \term -> (\x -> case x of
          Core.TermSet v0 -> (Utils.encodeDecode Coders.CoderDirectionEncode (Compute.adapterCoder ad) cx (Core.TermList (Sets.toList v0)))) term)
  in  
    let forListTerm = (\t -> (\x -> case x of
            Core.TermList v0 -> (Right (Core.TermSet (Sets.fromList v0)))) t)
    in  
      let decode = (\ad -> \cx -> \term -> Eithers.bind (Utils.encodeDecode Coders.CoderDirectionDecode (Compute.adapterCoder ad) cx term) (\listTerm -> forListTerm listTerm))
      in  
        let forSetType = (\st -> Eithers.bind (termAdapter cx (Core.TypeList st)) (\ad -> Right (Compute.Adapter {
                Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
                Compute.adapterSource = t,
                Compute.adapterTarget = (Compute.adapterTarget ad),
                Compute.adapterCoder = Compute.Coder {
                  Compute.coderEncode = (encode ad),
                  Compute.coderDecode = (decode ad)}})))
        in ((\x -> case x of
          Core.TypeSet v0 -> (forSetType v0)) t)

-- | Simplify application types
simplifyApplication :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
simplifyApplication cx t =  
  let encdec = (\ad -> \dir -> \cx -> \term -> Utils.encodeDecode dir (Compute.adapterCoder ad) cx term)
  in  
    let forApplicationType = (\at ->  
            let lhs = (Core.applicationTypeFunction at)
            in (Eithers.bind (termAdapter cx lhs) (\ad -> Right (Compute.Adapter {
              Compute.adapterIsLossy = False,
              Compute.adapterSource = t,
              Compute.adapterTarget = (Compute.adapterTarget ad),
              Compute.adapterCoder = (Utils.bidirectional (encdec ad))}))))
    in ((\x -> case x of
      Core.TypeApplication v0 -> (forApplicationType v0)) t)

-- | Create an adapter for any type
termAdapter :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
termAdapter cx typ =  
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
                  Variants.TypeVariantRecord -> [
                    passRecord]
                  Variants.TypeVariantSet -> [
                    passSet]
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
                    Variants.TypeVariantRecord -> []
                    Variants.TypeVariantSet -> [
                      setToList]
                    Variants.TypeVariantUnion -> [
                      unionToRecord]
                    Variants.TypeVariantUnit -> [
                      unitToRecord]
                    Variants.TypeVariantVariable -> []
                    Variants.TypeVariantWrap -> [
                      wrapToUnwrapped]) (Reflect.typeVariant t))
            in  
              let alts = (\cx -> \t -> Eithers.mapList (\c -> c cx t) (Logic.ifElse (supportedAtTopLevel cx t) (pass t) (trySubstitution t)))
              in  
                let dflt = ((\x -> case x of
                        Core.TypeVariable v0 -> (forTypeReference cx v0)
                        _ -> (Utils.chooseAdapter (alts cx) (supported cx) Core__.type_ Core__.type_ typ)) typ)
                in ((\x -> case x of
                  Core.TypeAnnotated v0 -> (Eithers.bind (termAdapter cx (Core.annotatedTypeBody v0)) (\ad -> Right (Compute.Adapter {
                    Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
                    Compute.adapterSource = (Compute.adapterSource ad),
                    Compute.adapterTarget = (Core.TypeAnnotated (Core.AnnotatedType {
                      Core.annotatedTypeBody = (Compute.adapterTarget ad),
                      Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation v0)})),
                    Compute.adapterCoder = (Compute.adapterCoder ad)})))
                  _ -> dflt) typ)

-- | Convert union types to record types
unionToRecord :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
unionToRecord cx t =  
  let forField = (\field ->  
          let fn = (Core.fieldName field)
          in  
            let fterm = (Core.fieldTerm field)
            in ((\x -> case x of
              Core.TermMaybe v0 -> (Maybes.bind v0 (\t -> Just (Core.Field {
                Core.fieldName = fn,
                Core.fieldTerm = t})))) fterm))
  in  
    let fromRecordFields = (\cx -> \term -> \term_ -> \t_ -> \fields ->  
            let matches = (Maybes.mapMaybe forField fields)
            in (Logic.ifElse (Lists.null matches) (Left (Context.InContext {
              Context.inContextObject = (Error.OtherError (Strings.cat [
                "cannot convert term back to union: ",
                (Core__.term term),
                " where type = ",
                (Core__.type_ t),
                "    and target type = ",
                (Core__.type_ t_)])),
              Context.inContextContext = cx})) (Right (Lists.head matches))))
    in  
      let forRecTerm = (\cx -> \nm -> \ad -> \term -> \recTerm -> (\x -> case x of
              Core.TermRecord v0 ->  
                let fields = (Core.recordFields v0)
                in (Eithers.bind (fromRecordFields cx term (Core.TermRecord (Core.Record {
                  Core.recordTypeName = nm,
                  Core.recordFields = fields})) (Compute.adapterTarget ad) fields) (\resultField -> Right (Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = nm,
                  Core.injectionField = resultField}))))) recTerm)
      in ((\x -> case x of
        Core.TypeUnion v0 ->  
          let nm = (Core.rowTypeTypeName v0)
          in  
            let sfields = (Core.rowTypeFields v0)
            in  
              let target = (Core.TypeRecord (CoderUtils.unionTypeToRecordType v0))
              in  
                let toRecordField = (\term -> \fn -> \f ->  
                        let fn_ = (Core.fieldTypeName f)
                        in Core.Field {
                          Core.fieldName = fn_,
                          Core.fieldTerm = (Core.TermMaybe (Logic.ifElse (Equality.equal fn_ fn) (Just term) Nothing))})
                in (Eithers.bind (termAdapter cx target) (\ad ->  
                  let graph = (Coders.adapterContextGraph cx)
                  in (Right (Compute.Adapter {
                    Compute.adapterIsLossy = (Compute.adapterIsLossy ad),
                    Compute.adapterSource = t,
                    Compute.adapterTarget = (Compute.adapterTarget ad),
                    Compute.adapterCoder = Compute.Coder {
                      Compute.coderEncode = (\cx -> \term_ -> Eithers.bind (Core_.injection cx (Core.rowTypeTypeName v0) graph term_) (\field ->  
                        let fn = (Core.fieldName field)
                        in  
                          let term = (Core.fieldTerm field)
                          in (Compute.coderEncode (Compute.adapterCoder ad) cx (Core.TermRecord (Core.Record {
                            Core.recordTypeName = nm,
                            Core.recordFields = (Lists.map (toRecordField term fn) sfields)}))))),
                      Compute.coderDecode = (\cx -> \term -> Eithers.bind (Compute.coderDecode (Compute.adapterCoder ad) cx term) (\recTerm -> forRecTerm cx nm ad term recTerm))}}))))) t)

-- | Convert unit terms to records
unitToRecord :: (t0 -> t1 -> Either t2 (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
unitToRecord _cx _ = (Right (Compute.Adapter {
  Compute.adapterIsLossy = False,
  Compute.adapterSource = Core.TypeUnit,
  Compute.adapterTarget = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "_Unit"),
    Core.rowTypeFields = []})),
  Compute.adapterCoder = Compute.Coder {
    Compute.coderEncode = (\_cx -> \_ -> Right (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "_Unit"),
      Core.recordFields = []}))),
    Compute.coderDecode = (\_cx -> \_ -> Right Core.TermUnit)}}))

-- | Convert wrapped types to unwrapped types
wrapToUnwrapped :: (Coders.AdapterContext -> Core.Type -> Either String (Compute.Adapter Core.Type Core.Type Core.Term Core.Term))
wrapToUnwrapped cx t = ((\x -> case x of
  Core.TypeWrap v0 ->  
    let tname = (Core.wrappedTypeTypeName v0)
    in  
      let typ = (Core.wrappedTypeBody v0)
      in  
        let encode = (\graph -> \ad -> \cx -> \term -> Eithers.bind (Core_.wrap cx tname graph term) (\unwrapped -> Compute.coderEncode (Compute.adapterCoder ad) cx unwrapped))
        in  
          let decode = (\ad -> \cx -> \term -> Eithers.bind (Compute.coderDecode (Compute.adapterCoder ad) cx term) (\decoded -> Right (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = tname,
                  Core.wrappedTermBody = decoded}))))
          in (Eithers.bind (termAdapter cx typ) (\ad -> Right (Compute.Adapter {
            Compute.adapterIsLossy = False,
            Compute.adapterSource = t,
            Compute.adapterTarget = (Compute.adapterTarget ad),
            Compute.adapterCoder = Compute.Coder {
              Compute.coderEncode = (encode (Coders.adapterContextGraph cx) ad),
              Compute.coderDecode = (decode ad)}})))) t)
