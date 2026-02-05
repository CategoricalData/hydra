-- Note: this is an automatically generated file. Do not edit.

-- | JSON encoding and decoding for Hydra terms

module Hydra.Ext.Org.Json.Coder where

import qualified Hydra.Adapt.Modules as Modules
import qualified Hydra.Adapt.Utils as Utils
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Ext.Org.Json.Language as Language
import qualified Hydra.Extract.Core as Core__
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Literals as Literals_
import qualified Hydra.Monads as Monads
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core___
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

jsonCoder :: (Core.Type -> Compute.Flow Graph.Graph (Compute.Coder Graph.Graph t0 Core.Term Model.Value))
jsonCoder typ = (Flows.bind (Modules.languageAdapter Language.jsonLanguage typ) (\adapter -> Flows.bind (termCoder (Compute.adapterTarget adapter)) (\coder -> Flows.pure (Utils.composeCoders (Compute.adapterCoder adapter) coder))))

literalJsonCoder :: (Core.LiteralType -> Compute.Flow t0 (Compute.Coder t1 t2 Core.Literal Model.Value))
literalJsonCoder lt =  
  let decodeBool = (\s -> (\x -> case x of
          Model.ValueBoolean v1 -> (Flows.pure (Core.LiteralBoolean v1))
          _ -> (Monads.unexpected "boolean" (showValue s))) s)
  in  
    let decodeFloat = (\s -> (\x -> case x of
            Model.ValueNumber v1 -> (Flows.pure (Core.LiteralFloat (Core.FloatValueBigfloat v1)))
            _ -> (Monads.unexpected "number" (showValue s))) s)
    in  
      let decodeInteger = (\s -> (\x -> case x of
              Model.ValueNumber v1 ->  
                let bi = (Literals.bigfloatToBigint v1)
                in (Flows.pure (Core.LiteralInteger (Core.IntegerValueBigint bi)))
              _ -> (Monads.unexpected "number" (showValue s))) s)
      in  
        let decodeString = (\s -> (\x -> case x of
                Model.ValueString v1 -> (Flows.pure (Core.LiteralString v1))
                _ -> (Monads.unexpected "string" (showValue s))) s)
        in  
          let encoded = ((\x -> case x of
                  Core.LiteralTypeBoolean -> Compute.Coder {
                    Compute.coderEncode = (\lit -> Flows.bind (Core__.booleanLiteral lit) (\b -> Flows.pure (Model.ValueBoolean b))),
                    Compute.coderDecode = decodeBool}
                  Core.LiteralTypeFloat _ -> Compute.Coder {
                    Compute.coderEncode = (\lit -> Flows.bind (Core__.floatLiteral lit) (\f -> Flows.bind (Core__.bigfloatValue f) (\bf -> Flows.pure (Model.ValueNumber bf)))),
                    Compute.coderDecode = decodeFloat}
                  Core.LiteralTypeInteger _ -> Compute.Coder {
                    Compute.coderEncode = (\lit -> Flows.bind (Core__.integerLiteral lit) (\i -> Flows.bind (Core__.bigintValue i) (\bi -> Flows.pure (Model.ValueNumber (Literals.bigintToBigfloat bi))))),
                    Compute.coderDecode = decodeInteger}
                  Core.LiteralTypeString -> Compute.Coder {
                    Compute.coderEncode = (\lit -> Flows.bind (Core__.stringLiteral lit) (\s -> Flows.pure (Model.ValueString s))),
                    Compute.coderDecode = decodeString}) lt)
          in (Flows.pure encoded)

recordCoder :: (Core.RowType -> Compute.Flow t0 (Compute.Coder Graph.Graph t1 Core.Term Model.Value))
recordCoder rt =  
  let fields = (Core.rowTypeFields rt)
  in  
    let getCoder = (\f -> Flows.bind (termCoder (Core.fieldTypeType f)) (\coder -> Flows.pure (f, coder)))
    in (Flows.bind (Flows.mapList getCoder fields) (\coders -> Flows.pure (Compute.Coder {
      Compute.coderEncode = (encodeRecord coders),
      Compute.coderDecode = (decodeRecord rt coders)})))

encodeRecord :: ([(Core.FieldType, (Compute.Coder Graph.Graph t0 Core.Term Model.Value))] -> Core.Term -> Compute.Flow Graph.Graph Model.Value)
encodeRecord coders term =  
  let stripped = (Rewriting.deannotateTerm term)
  in  
    let matchMaybeTerm = (\fvalue -> \coder_ -> \fname -> \dflt -> (\x -> case x of
            Core.TermMaybe v1 -> (Maybes.maybe (Flows.pure Nothing) (\v -> Flows.bind (Compute.coderEncode coder_ v) (\encoded -> Flows.pure (Just (Core.unName fname, encoded)))) v1)
            _ -> dflt) fvalue)
    in  
      let matchTypeForMaybe = (\ft -> \forMaybe -> \dflt -> (\x -> case x of
              Core.TypeMaybe v1 -> (forMaybe v1)
              _ -> dflt) (Core.fieldTypeType ft))
      in  
        let encodeField = (\coderAndField ->  
                let coder = (Pairs.first coderAndField)
                in  
                  let field = (Pairs.second coderAndField)
                  in  
                    let ft = (Pairs.first coder)
                    in  
                      let coder_ = (Pairs.second coder)
                      in  
                        let fname = (Core.fieldName field)
                        in  
                          let fvalue = (Core.fieldTerm field)
                          in  
                            let forMaybe = (\ot ->  
                                    let dflt = (Flows.bind (Compute.coderEncode coder_ fvalue) (\encoded -> Flows.pure (Just (Core.unName fname, encoded))))
                                    in (matchMaybeTerm fvalue coder_ fname dflt))
                            in  
                              let dflt = (Flows.bind (Compute.coderEncode coder_ fvalue) (\encoded -> Flows.pure (Just (Core.unName fname, encoded))))
                              in (matchTypeForMaybe ft forMaybe dflt))
        in (Flows.bind (Core__.termRecord stripped) (\record ->  
          let fields = (Core.recordFields record)
          in (Flows.bind (Flows.mapList encodeField (Lists.zip coders fields)) (\maybeFields -> Flows.pure (Model.ValueObject (Maps.fromList (Maybes.cat maybeFields)))))))

decodeRecord :: (Core.RowType -> [(Core.FieldType, (Compute.Coder t0 t1 Core.Term Model.Value))] -> Model.Value -> Compute.Flow t1 Core.Term)
decodeRecord rt coders n =  
  let decodeObjectBody = (\m ->  
          let decodeField = (\coder ->  
                  let ft = (Pairs.first coder)
                  in  
                    let coder_ = (Pairs.second coder)
                    in  
                      let fname = (Core.fieldTypeName ft)
                      in  
                        let defaultValue = Model.ValueNull
                        in  
                          let jsonValue = (Maybes.fromMaybe defaultValue (Maps.lookup (Core.unName fname) m))
                          in (Flows.bind (Compute.coderDecode coder_ jsonValue) (\v -> Flows.pure (Core.Field {
                            Core.fieldName = fname,
                            Core.fieldTerm = v}))))
          in (Flows.bind (Flows.mapList decodeField coders) (\fields -> Flows.pure (Core.TermRecord (Core.Record {
            Core.recordTypeName = (Core.rowTypeTypeName rt),
            Core.recordFields = fields})))))
  in  
    let result = ((\x -> case x of
            Model.ValueObject v1 -> (decodeObjectBody v1)
            _ -> (Monads.unexpected "object" (showValue n))) n)
    in result

termCoder :: (Core.Type -> Compute.Flow t0 (Compute.Coder Graph.Graph t1 Core.Term Model.Value))
termCoder typ =  
  let stripped = (Rewriting.deannotateType typ)
  in  
    let encodeLiteral = (\ac -> \term -> (\x -> case x of
            Core.TermLiteral v1 -> (Compute.coderEncode ac v1)
            _ -> (Monads.unexpected "literal term" (Core___.term term))) term)
    in  
      let encodeList = (\lc -> \term -> (\x -> case x of
              Core.TermList v1 -> (Flows.bind (Flows.mapList (Compute.coderEncode lc) v1) (\encodedEls -> Flows.pure (Model.ValueArray encodedEls)))
              _ -> (Monads.unexpected "list term" (Core___.term term))) term)
      in  
        let decodeList = (\lc -> \n -> (\x -> case x of
                Model.ValueArray v1 -> (Flows.bind (Flows.mapList (Compute.coderDecode lc) v1) (\decodedNodes -> Flows.pure (Core.TermList decodedNodes)))
                _ -> (Monads.unexpected "sequence" (showValue n))) n)
        in  
          let matchLiteralString = (\v -> \lit -> (\x -> case x of
                  Core.LiteralString v1 -> v1
                  _ -> (Core___.term v)) lit)
          in  
            let matchTermLiteral = (\v -> (\x -> case x of
                    Core.TermLiteral v1 -> (matchLiteralString v v1)
                    _ -> (Core___.term v)) (Rewriting.deannotateTerm v))
            in  
              let encodeMap = (\encodeEntry -> \term -> (\x -> case x of
                      Core.TermMap v1 -> (Flows.bind (Flows.mapList encodeEntry (Maps.toList v1)) (\entries -> Flows.pure (Model.ValueObject (Maps.fromList entries))))
                      _ -> (Monads.unexpected "map term" (Core___.term term))) term)
              in  
                let decodeMap = (\decodeEntry -> \n -> (\x -> case x of
                        Model.ValueObject v1 -> (Flows.bind (Flows.mapList decodeEntry (Maps.toList v1)) (\entries -> Flows.pure (Core.TermMap (Maps.fromList entries))))
                        _ -> (Monads.unexpected "mapping" (showValue n))) n)
                in  
                  let encodeMaybe = (\maybeElementCoder -> \maybeTerm ->  
                          let strippedMaybeTerm = (Rewriting.deannotateTerm maybeTerm)
                          in ((\x -> case x of
                            Core.TermMaybe v1 -> (Logic.ifElse (Maybes.isNothing v1) (Flows.pure Model.ValueNull) (Flows.bind (Compute.coderEncode maybeElementCoder (Maybes.fromJust v1)) (\encodedInner -> Flows.pure encodedInner)))
                            _ -> (Monads.unexpected "optional term" (Core___.term maybeTerm))) strippedMaybeTerm))
                  in  
                    let decodeMaybe = (\maybeElementCoder -> \jsonVal -> (\x -> case x of
                            Model.ValueNull -> (Flows.pure (Core.TermMaybe Nothing))
                            _ -> (Flows.bind (Compute.coderDecode maybeElementCoder jsonVal) (\decodedInner -> Flows.pure (Core.TermMaybe (Just decodedInner))))) jsonVal)
                    in  
                      let result = ((\x -> case x of
                              Core.TypeLiteral v1 -> (Flows.bind (literalJsonCoder v1) (\ac -> Flows.pure (Compute.Coder {
                                Compute.coderEncode = (encodeLiteral ac),
                                Compute.coderDecode = (\n -> Flows.bind (Compute.coderDecode ac n) (\lit -> Flows.pure (Core.TermLiteral lit)))})))
                              Core.TypeList v1 -> (Flows.bind (termCoder v1) (\lc -> Flows.pure (Compute.Coder {
                                Compute.coderEncode = (encodeList lc),
                                Compute.coderDecode = (decodeList lc)})))
                              Core.TypeMap v1 ->  
                                let kt = (Core.mapTypeKeys v1)
                                in  
                                  let vt = (Core.mapTypeValues v1)
                                  in (Flows.bind (termCoder kt) (\kc -> Flows.bind (termCoder vt) (\vc -> Flows.bind Monads.getState (\cx ->  
                                    let isStringKey = (Equality.equal (Rewriting.deannotateType kt) (Core.TypeLiteral Core.LiteralTypeString))
                                    in  
                                      let toString = (\v -> Logic.ifElse isStringKey (matchTermLiteral v) (Core___.term v))
                                      in  
                                        let fromString = (\s -> Logic.ifElse isStringKey (Core.TermLiteral (Core.LiteralString s)) (readStringStub s))
                                        in  
                                          let encodeEntry = (\kv ->  
                                                  let k = (Pairs.first kv)
                                                  in  
                                                    let v = (Pairs.second kv)
                                                    in (Flows.bind (Compute.coderEncode vc v) (\encodedV -> Flows.pure (toString k, encodedV))))
                                          in  
                                            let decodeEntry = (\kv ->  
                                                    let k = (Pairs.first kv)
                                                    in  
                                                      let v = (Pairs.second kv)
                                                      in (Flows.bind (Compute.coderDecode vc v) (\decodedV -> Flows.pure (fromString k, decodedV))))
                                            in (Flows.pure (Compute.Coder {
                                              Compute.coderEncode = (encodeMap encodeEntry),
                                              Compute.coderDecode = (decodeMap decodeEntry)}))))))
                              Core.TypeMaybe v1 -> (Flows.bind (termCoder v1) (\maybeElementCoder -> Flows.pure (Compute.Coder {
                                Compute.coderEncode = (encodeMaybe maybeElementCoder),
                                Compute.coderDecode = (decodeMaybe maybeElementCoder)})))
                              Core.TypeRecord v1 -> (recordCoder v1)
                              Core.TypeUnit -> (Flows.pure unitCoder)
                              Core.TypeVariable v1 -> (Flows.pure (Compute.Coder {
                                Compute.coderEncode = (\term -> Flows.pure (Model.ValueString (Strings.cat [
                                  "variable '",
                                  (Core.unName v1),
                                  "' for: ",
                                  (Core___.term term)]))),
                                Compute.coderDecode = (\term -> Flows.fail (Strings.cat [
                                  "type variable ",
                                  (Core.unName v1),
                                  " does not support decoding"]))}))
                              _ -> (Flows.fail (Strings.cat [
                                "unsupported type in JSON: ",
                                (Core___.type_ typ)]))) stripped)
                      in result

unitCoder :: (Compute.Coder t0 t1 Core.Term Model.Value)
unitCoder =  
  let encodeUnit = (\term -> (\x -> case x of
          Core.TermUnit -> (Flows.pure Model.ValueNull)
          _ -> (Monads.unexpected "unit" (Core___.term term))) (Rewriting.deannotateTerm term))
  in  
    let decodeUnit = (\n -> (\x -> case x of
            Model.ValueNull -> (Flows.pure Core.TermUnit)
            _ -> (Monads.unexpected "null" (showValue n))) n)
    in Compute.Coder {
      Compute.coderEncode = encodeUnit,
      Compute.coderDecode = decodeUnit}

untypedTermToJson :: (Core.Term -> Compute.Flow t0 Model.Value)
untypedTermToJson term =  
  let unexp = (\msg -> Flows.pure (Model.ValueString (Strings.cat2 "FAIL: " msg)))
  in  
    let asRecord = (\fields -> untypedTermToJson (Core.TermRecord (Core.Record {
            Core.recordTypeName = (Core.Name ""),
            Core.recordFields = fields})))
    in  
      let asVariant = (\name -> \term -> untypedTermToJson (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name ""),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name name),
                Core.fieldTerm = term}})))
      in  
        let matchTermMaybe = (\forTerm -> \t -> (\x -> case x of
                Core.TermMaybe v1 -> (Maybes.maybe (Flows.pure Nothing) forTerm v1)
                _ -> (Flows.map Maybes.pure (untypedTermToJson t))) t)
        in  
          let matchElimination = (\unexp -> \asVariant -> \elm -> (\x -> case x of
                  Core.EliminationRecord v1 -> (asVariant "project" (Core.TermVariable (Core.projectionField v1)))
                  _ -> (unexp (Strings.cat [
                    "unexpected elimination variant: ",
                    (Core___.elimination elm)]))) elm)
          in  
            let matchFunction = (\unexp -> \asRecord -> \asVariant -> \f -> (\x -> case x of
                    Core.FunctionElimination v1 -> (matchElimination unexp asVariant v1)
                    Core.FunctionLambda v1 -> (asRecord [
                      Core.Field {
                        Core.fieldName = (Core.Name "parameter"),
                        Core.fieldTerm = (Core.TermVariable (Core.lambdaParameter v1))},
                      Core.Field {
                        Core.fieldName = (Core.Name "domain"),
                        Core.fieldTerm = (Core.TermMaybe (Maybes.map Core_.type_ (Core.lambdaDomain v1)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "body"),
                        Core.fieldTerm = (Core.lambdaBody v1)}])
                    Core.FunctionPrimitive v1 -> (Flows.pure (Model.ValueString (Core.unName v1)))) f)
            in  
              let matchLiteral = (\lit -> (\x -> case x of
                      Core.LiteralBinary v1 -> (Model.ValueString (Literals.binaryToString v1))
                      Core.LiteralBoolean v1 -> (Model.ValueBoolean v1)
                      Core.LiteralFloat v1 -> (Model.ValueNumber (Literals_.floatValueToBigfloat v1))
                      Core.LiteralInteger v1 ->  
                        let bf = (Literals_.integerValueToBigint v1)
                        in  
                          let f = (Literals.bigintToBigfloat bf)
                          in (Model.ValueNumber f)
                      Core.LiteralString v1 -> (Model.ValueString v1)) lit)
              in  
                let fieldToKeyval = (\f ->  
                        let forTerm = (\t -> matchTermMaybe forTerm t)
                        in (Flows.bind (forTerm (Core.fieldTerm f)) (\mjson -> Flows.pure (Maybes.map (\j -> (Core.unName (Core.fieldName f), j)) mjson))))
                in  
                  let result = ((\x -> case x of
                          Core.TermAnnotated v1 ->  
                            let term1 = (Core.annotatedTermBody v1)
                            in  
                              let ann = (Core.annotatedTermAnnotation v1)
                              in  
                                let encodePair = (\kv ->  
                                        let k = (Core.unName (Pairs.first kv))
                                        in  
                                          let v = (Pairs.second kv)
                                          in (Flows.bind (untypedTermToJson v) (\json -> Flows.pure (k, json))))
                                in (Flows.bind (untypedTermToJson term1) (\json -> Flows.bind (Flows.mapList encodePair (Maps.toList ann)) (\pairs -> Flows.pure (Model.ValueObject (Maps.fromList [
                                  ("term", json),
                                  ("annotations", (Model.ValueObject (Maps.fromList pairs)))])))))
                          Core.TermApplication v1 -> (asRecord [
                            Core.Field {
                              Core.fieldName = (Core.Name "function"),
                              Core.fieldTerm = (Core.applicationFunction v1)},
                            Core.Field {
                              Core.fieldName = (Core.Name "argument"),
                              Core.fieldTerm = (Core.applicationArgument v1)}])
                          Core.TermFunction v1 -> (matchFunction unexp asRecord asVariant v1)
                          Core.TermLet v1 ->  
                            let bindings = (Core.letBindings v1)
                            in  
                              let env = (Core.letBody v1)
                              in  
                                let fromBinding = (\b -> Core.Field {
                                        Core.fieldName = (Core.bindingName b),
                                        Core.fieldTerm = (Core.bindingTerm b)})
                                in (asRecord [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "bindings"),
                                    Core.fieldTerm = (Core.TermRecord (Core.Record {
                                      Core.recordTypeName = (Core.Name ""),
                                      Core.recordFields = (Lists.map fromBinding bindings)}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "environment"),
                                    Core.fieldTerm = env}])
                          Core.TermList v1 -> (Flows.bind (Flows.mapList untypedTermToJson v1) (\jsonTerms -> Flows.pure (Model.ValueArray jsonTerms)))
                          Core.TermLiteral v1 -> (Flows.pure (matchLiteral v1))
                          Core.TermMaybe v1 -> (Maybes.maybe (Flows.pure Model.ValueNull) untypedTermToJson v1)
                          Core.TermRecord v1 ->  
                            let fields = (Core.recordFields v1)
                            in (Flows.bind (Flows.mapList fieldToKeyval fields) (\keyvals -> Flows.pure (Model.ValueObject (Maps.fromList (Maybes.cat keyvals)))))
                          Core.TermSet v1 -> (untypedTermToJson (Core.TermList (Sets.toList v1)))
                          Core.TermTypeLambda v1 -> (asRecord [
                            Core.Field {
                              Core.fieldName = (Core.Name "parameter"),
                              Core.fieldTerm = (Core.TermVariable (Core.typeLambdaParameter v1))},
                            Core.Field {
                              Core.fieldName = (Core.Name "body"),
                              Core.fieldTerm = (Core.typeLambdaBody v1)}])
                          Core.TermTypeApplication v1 -> (asRecord [
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.typeApplicationTermBody v1)},
                            Core.Field {
                              Core.fieldName = (Core.Name "type"),
                              Core.fieldTerm = (Core_.type_ (Core.typeApplicationTermType v1))}])
                          Core.TermUnion v1 ->  
                            let field = (Core.injectionField v1)
                            in (Logic.ifElse (Equality.equal (Core.fieldTerm field) Core.TermUnit) (Flows.pure (Model.ValueString (Core.unName (Core.fieldName field)))) (Flows.bind (fieldToKeyval field) (\mkeyval -> Flows.pure (Model.ValueObject (Maps.fromList (Maybes.maybe [] (\keyval -> [
                              keyval]) mkeyval))))))
                          Core.TermVariable v1 -> (Flows.pure (Model.ValueString (Core.unName v1)))
                          Core.TermWrap v1 -> (untypedTermToJson (Core.wrappedTermBody v1))
                          _ -> (unexp (Strings.cat [
                            "unsupported term variant: ",
                            (Core___.term term)]))) term)
                  in result

-- | Placeholder for reading a string into a term (to be implemented)
readStringStub :: (String -> Core.Term)
readStringStub s = (Core.TermLiteral (Core.LiteralString (Strings.cat2 "TODO: read " s)))

showValue :: (t0 -> String)
showValue value = "TODO: implement showValue"
