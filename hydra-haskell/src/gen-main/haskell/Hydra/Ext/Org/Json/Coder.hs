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
import qualified Hydra.Json as Json
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Literals as Literals_
import qualified Hydra.Monads as Monads
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core___
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

jsonCoder :: (Core.Type -> Compute.Flow Graph.Graph (Compute.Coder Graph.Graph t0 Core.Term Json.Value))
jsonCoder typ = (Flows.bind (Modules.languageAdapter Language.jsonLanguage typ) (\adapter -> Flows.bind (termCoder (Compute.adapterTarget adapter)) (\coder -> Flows.pure (Utils.composeCoders (Compute.adapterCoder adapter) coder))))

literalJsonCoder :: (Core.LiteralType -> Compute.Flow t0 (Compute.Coder t1 t2 Core.Literal Json.Value))
literalJsonCoder lt = (Flows.pure ((\x -> case x of
  Core.LiteralTypeBoolean -> Compute.Coder {
    Compute.coderEncode = (\lit -> Flows.bind (Core__.booleanLiteral lit) (\b -> Flows.pure (Json.ValueBoolean b))),
    Compute.coderDecode = (\s -> (\x -> case x of
      Json.ValueBoolean v2 -> (Flows.pure (Core.LiteralBoolean v2))
      _ -> (Monads.unexpected "boolean" (showValue s))) s)}
  Core.LiteralTypeFloat _ -> Compute.Coder {
    Compute.coderEncode = (\lit -> Flows.bind (Core__.floatLiteral lit) (\f -> Flows.bind (Core__.bigfloatValue f) (\bf -> Flows.pure (Json.ValueNumber bf)))),
    Compute.coderDecode = (\s -> (\x -> case x of
      Json.ValueNumber v2 -> (Flows.pure (Core.LiteralFloat (Core.FloatValueBigfloat v2)))
      _ -> (Monads.unexpected "number" (showValue s))) s)}
  Core.LiteralTypeInteger _ -> Compute.Coder {
    Compute.coderEncode = (\lit -> Flows.bind (Core__.integerLiteral lit) (\i -> Flows.bind (Core__.bigintValue i) (\bi -> Flows.pure (Json.ValueNumber (Literals.bigintToBigfloat bi))))),
    Compute.coderDecode = (\s -> (\x -> case x of
      Json.ValueNumber v2 ->  
        let bi = (Literals.bigfloatToBigint v2)
        in (Flows.pure (Core.LiteralInteger (Core.IntegerValueBigint bi)))
      _ -> (Monads.unexpected "number" (showValue s))) s)}
  Core.LiteralTypeString -> Compute.Coder {
    Compute.coderEncode = (\lit -> Flows.bind (Core__.stringLiteral lit) (\s -> Flows.pure (Json.ValueString s))),
    Compute.coderDecode = (\s -> (\x -> case x of
      Json.ValueString v2 -> (Flows.pure (Core.LiteralString v2))
      _ -> (Monads.unexpected "string" (showValue s))) s)}) lt))

recordCoder :: (Core.RowType -> Compute.Flow t0 (Compute.Coder Graph.Graph t1 Core.Term Json.Value))
recordCoder rt =  
  let fields = (Core.rowTypeFields rt) 
      getCoder = (\f -> Flows.bind (termCoder (Core.fieldTypeType f)) (\coder -> Flows.pure (f, coder)))
  in (Flows.bind (Flows.mapList getCoder fields) (\coders -> Flows.pure (Compute.Coder {
    Compute.coderEncode = (encodeRecord coders),
    Compute.coderDecode = (decodeRecord rt coders)})))

encodeRecord :: ([(Core.FieldType, (Compute.Coder Graph.Graph t0 Core.Term Json.Value))] -> Core.Term -> Compute.Flow Graph.Graph Json.Value)
encodeRecord coders term =  
  let stripped = (Rewriting.deannotateTerm term)
  in (Flows.bind (Core__.termRecord stripped) (\record ->  
    let fields = (Core.recordFields record) 
        encodeField = (\coderAndField ->  
                let coder = (fst coderAndField) 
                    field = (snd coderAndField)
                    ft = (fst coder)
                    coder_ = (snd coder)
                    fname = (Core.fieldName field)
                    fvalue = (Core.fieldTerm field)
                in ((\x -> case x of
                  Core.TypeOptional _ -> ((\x -> case x of
                    Core.TermOptional v2 -> (Optionals.maybe (Flows.pure Nothing) (\v -> Flows.bind (Compute.coderEncode coder_ v) (\encoded -> Flows.pure (Just (Core.unName fname, encoded)))) v2)
                    _ -> (Flows.bind (Compute.coderEncode coder_ fvalue) (\encoded -> Flows.pure (Just (Core.unName fname, encoded))))) fvalue)
                  _ -> (Flows.bind (Compute.coderEncode coder_ fvalue) (\encoded -> Flows.pure (Just (Core.unName fname, encoded))))) (Core.fieldTypeType ft)))
    in (Flows.bind (Flows.mapList encodeField (Lists.zip coders fields)) (\maybeFields -> Flows.pure (Json.ValueObject (Maps.fromList (Optionals.cat maybeFields)))))))

decodeRecord :: (Core.RowType -> [(Core.FieldType, (Compute.Coder t0 t1 Core.Term Json.Value))] -> Json.Value -> Compute.Flow t1 Core.Term)
decodeRecord rt coders n = ((\x -> case x of
  Json.ValueObject v1 ->  
    let decodeField = (\coder ->  
            let ft = (fst coder) 
                coder_ = (snd coder)
                fname = (Core.fieldTypeName ft)
                defaultValue = Json.ValueNull
                jsonValue = (Optionals.fromMaybe defaultValue (Maps.lookup (Core.unName fname) v1))
            in (Flows.bind (Compute.coderDecode coder_ jsonValue) (\v -> Flows.pure (Core.Field {
              Core.fieldName = fname,
              Core.fieldTerm = v}))))
    in (Flows.bind (Flows.mapList decodeField coders) (\fields -> Flows.pure (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.rowTypeTypeName rt),
      Core.recordFields = fields}))))
  _ -> (Monads.unexpected "object" (showValue n))) n)

termCoder :: (Core.Type -> Compute.Flow t0 (Compute.Coder Graph.Graph t1 Core.Term Json.Value))
termCoder typ =  
  let stripped = (Rewriting.deannotateType typ)
  in ((\x -> case x of
    Core.TypeLiteral v1 -> (Flows.bind (literalJsonCoder v1) (\ac -> Flows.pure (Compute.Coder {
      Compute.coderEncode = (\term -> (\x -> case x of
        Core.TermLiteral v2 -> (Compute.coderEncode ac v2)
        _ -> (Monads.unexpected "literal term" (Core___.term term))) term),
      Compute.coderDecode = (\n -> Flows.bind (Compute.coderDecode ac n) (\lit -> Flows.pure (Core.TermLiteral lit)))})))
    Core.TypeList v1 -> (Flows.bind (termCoder v1) (\lc -> Flows.pure (Compute.Coder {
      Compute.coderEncode = (\term -> (\x -> case x of
        Core.TermList v2 -> (Flows.bind (Flows.mapList (Compute.coderEncode lc) v2) (\encodedEls -> Flows.pure (Json.ValueArray encodedEls)))
        _ -> (Monads.unexpected "list term" (Core___.term term))) term),
      Compute.coderDecode = (\n -> (\x -> case x of
        Json.ValueArray v2 -> (Flows.bind (Flows.mapList (Compute.coderDecode lc) v2) (\decodedNodes -> Flows.pure (Core.TermList decodedNodes)))
        _ -> (Monads.unexpected "sequence" (showValue n))) n)})))
    Core.TypeMap v1 ->  
      let kt = (Core.mapTypeKeys v1) 
          vt = (Core.mapTypeValues v1)
      in (Flows.bind (termCoder kt) (\kc -> Flows.bind (termCoder vt) (\vc -> Flows.bind Monads.getState (\cx ->  
        let isStringKey = (Equality.equal (Rewriting.deannotateType kt) (Core.TypeLiteral Core.LiteralTypeString)) 
            toString = (\v -> Logic.ifElse isStringKey ((\x -> case x of
                    Core.TermLiteral v2 -> ((\x -> case x of
                      Core.LiteralString v3 -> v3
                      _ -> (Core___.term v)) v2)
                    _ -> (Core___.term v)) (Rewriting.deannotateTerm v)) (Core___.term v))
            fromString = (\s -> Logic.ifElse isStringKey (Core.TermLiteral (Core.LiteralString s)) (readStringStub s))
            encodeEntry = (\kv ->  
                    let k = (fst kv) 
                        v = (snd kv)
                    in (Flows.bind (Compute.coderEncode vc v) (\encodedV -> Flows.pure (toString k, encodedV))))
            decodeEntry = (\kv ->  
                    let k = (fst kv) 
                        v = (snd kv)
                    in (Flows.bind (Compute.coderDecode vc v) (\decodedV -> Flows.pure (fromString k, decodedV))))
        in (Flows.pure (Compute.Coder {
          Compute.coderEncode = (\term -> (\x -> case x of
            Core.TermMap v2 -> (Flows.bind (Flows.mapList encodeEntry (Maps.toList v2)) (\entries -> Flows.pure (Json.ValueObject (Maps.fromList entries))))
            _ -> (Monads.unexpected "map term" (Core___.term term))) term),
          Compute.coderDecode = (\n -> (\x -> case x of
            Json.ValueObject v2 -> (Flows.bind (Flows.mapList decodeEntry (Maps.toList v2)) (\entries -> Flows.pure (Core.TermMap (Maps.fromList entries))))
            _ -> (Monads.unexpected "mapping" (showValue n))) n)}))))))
    Core.TypeOptional v1 -> (Flows.bind (termCoder v1) (\oc -> Flows.pure (Compute.Coder {
      Compute.coderEncode = (\t ->  
        let stripped = (Rewriting.deannotateTerm t)
        in ((\x -> case x of
          Core.TermOptional v2 -> (Optionals.maybe (Flows.pure Json.ValueNull) (Compute.coderEncode oc) v2)
          _ -> (Monads.unexpected "optional term" (Core___.term t))) stripped)),
      Compute.coderDecode = (\n -> (\x -> case x of
        Json.ValueNull -> (Flows.pure (Core.TermOptional Nothing))
        _ -> (Flows.bind (Compute.coderDecode oc n) (\decoded -> Flows.pure (Core.TermOptional (Just decoded))))) n)})))
    Core.TypeRecord v1 -> (recordCoder v1)
    Core.TypeUnit -> (Flows.pure unitCoder)
    Core.TypeVariable v1 -> (Flows.pure (Compute.Coder {
      Compute.coderEncode = (\term -> Flows.pure (Json.ValueString (Strings.cat [
        "variable '",
        Core.unName v1,
        "' for: ",
        (Core___.term term)]))),
      Compute.coderDecode = (\term -> Flows.fail (Strings.cat [
        "type variable ",
        Core.unName v1,
        " does not support decoding"]))}))
    _ -> (Flows.fail (Strings.cat [
      "unsupported type in JSON: ",
      (Core___.type_ typ)]))) stripped)

unitCoder :: (Compute.Coder t0 t1 Core.Term Json.Value)
unitCoder = Compute.Coder {
  Compute.coderEncode = (\term -> (\x -> case x of
    Core.TermUnit -> (Flows.pure Json.ValueNull)
    _ -> (Monads.unexpected "unit" (Core___.term term))) (Rewriting.deannotateTerm term)),
  Compute.coderDecode = (\n -> (\x -> case x of
    Json.ValueNull -> (Flows.pure Core.TermUnit)
    _ -> (Monads.unexpected "null" (showValue n))) n)}

untypedTermToJson :: (Core.Term -> Compute.Flow t0 Json.Value)
untypedTermToJson term =  
  let unexp = (\msg -> Flows.pure (Json.ValueString (Strings.cat2 "FAIL: " msg))) 
      asRecord = (\fields -> untypedTermToJson (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name ""),
              Core.recordFields = fields})))
      asVariant = (\name -> \term -> untypedTermToJson (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name ""),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name name),
                Core.fieldTerm = term}})))
      fieldToKeyval = (\f ->  
              let forTerm = (\t -> (\x -> case x of
                      Core.TermOptional v1 -> (Optionals.maybe (Flows.pure Nothing) forTerm v1)
                      _ -> (Flows.map Optionals.pure (untypedTermToJson t))) t)
              in (Flows.bind (forTerm (Core.fieldTerm f)) (\mjson -> Flows.pure (Optionals.map (\j -> (Core.unName (Core.fieldName f), j)) mjson))))
  in ((\x -> case x of
    Core.TermAnnotated v1 ->  
      let term1 = (Core.annotatedTermBody v1) 
          ann = (Core.annotatedTermAnnotation v1)
          encodePair = (\kv ->  
                  let k = (Core.unName (fst kv)) 
                      v = (snd kv)
                  in (Flows.bind (untypedTermToJson v) (\json -> Flows.pure (k, json))))
      in (Flows.bind (untypedTermToJson term1) (\json -> Flows.bind (Flows.mapList encodePair (Maps.toList ann)) (\pairs -> Flows.pure (Json.ValueObject (Maps.fromList [
        ("term", json),
        ("annotations", (Json.ValueObject (Maps.fromList pairs)))])))))
    Core.TermApplication v1 -> (asRecord [
      Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Core.applicationFunction v1)},
      Core.Field {
        Core.fieldName = (Core.Name "argument"),
        Core.fieldTerm = (Core.applicationArgument v1)}])
    Core.TermFunction v1 -> ((\x -> case x of
      Core.FunctionElimination v2 -> ((\x -> case x of
        Core.EliminationRecord v3 -> (asVariant "project" (Core.TermVariable (Core.projectionField v3)))
        _ -> (unexp (Strings.cat [
          "unexpected elimination variant: ",
          (Core___.elimination v2)]))) v2)
      Core.FunctionLambda v2 -> (asRecord [
        Core.Field {
          Core.fieldName = (Core.Name "parameter"),
          Core.fieldTerm = (Core.TermVariable (Core.lambdaParameter v2))},
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (Core.TermOptional (Optionals.map Core_.type_ (Core.lambdaDomain v2)))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.lambdaBody v2)}])
      Core.FunctionPrimitive v2 -> (Flows.pure (Json.ValueString (Core.unName v2)))) v1)
    Core.TermLet v1 ->  
      let bindings = (Core.letBindings v1) 
          env = (Core.letBody v1)
          fromBinding = (\b -> Core.Field {
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
    Core.TermList v1 -> (Flows.bind (Flows.mapList untypedTermToJson v1) (\jsonTerms -> Flows.pure (Json.ValueArray jsonTerms)))
    Core.TermLiteral v1 -> (Flows.pure ((\x -> case x of
      Core.LiteralBinary v2 -> (Json.ValueString (Literals.binaryToString v2))
      Core.LiteralBoolean v2 -> (Json.ValueBoolean v2)
      Core.LiteralFloat v2 -> (Json.ValueNumber (Literals_.floatValueToBigfloat v2))
      Core.LiteralInteger v2 ->  
        let bf = (Literals_.integerValueToBigint v2) 
            f = (Literals.bigintToBigfloat bf)
        in (Json.ValueNumber f)
      Core.LiteralString v2 -> (Json.ValueString v2)) v1))
    Core.TermOptional v1 -> (Optionals.maybe (Flows.pure Json.ValueNull) untypedTermToJson v1)
    Core.TermProduct v1 -> (untypedTermToJson (Core.TermList v1))
    Core.TermRecord v1 ->  
      let fields = (Core.recordFields v1)
      in (Flows.bind (Flows.mapList fieldToKeyval fields) (\keyvals -> Flows.pure (Json.ValueObject (Maps.fromList (Optionals.cat keyvals)))))
    Core.TermSet v1 -> (untypedTermToJson (Core.TermList (Sets.toList v1)))
    Core.TermSum v1 -> (asRecord [
      Core.Field {
        Core.fieldName = (Core.Name "index"),
        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Core.sumIndex v1))))},
      Core.Field {
        Core.fieldName = (Core.Name "size"),
        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (Core.sumSize v1))))},
      Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Core.sumTerm v1)}])
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
        Core.fieldTerm = (Core.typedTermTerm v1)},
      Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Core_.type_ (Core.typedTermType v1))}])
    Core.TermUnion v1 ->  
      let field = (Core.injectionField v1)
      in (Logic.ifElse (Equality.equal (Core.fieldTerm field) Core.TermUnit) (Flows.pure (Json.ValueString (Core.unName (Core.fieldName field)))) (Flows.bind (fieldToKeyval field) (\mkeyval -> Flows.pure (Json.ValueObject (Maps.fromList (Optionals.maybe [] (\keyval -> [
        keyval]) mkeyval))))))
    Core.TermVariable v1 -> (Flows.pure (Json.ValueString (Core.unName v1)))
    Core.TermWrap v1 -> (untypedTermToJson (Core.wrappedTermBody v1))
    _ -> (unexp (Strings.cat [
      "unsupported term variant: ",
      (Core___.term term)]))) term)

-- | Placeholder for reading a string into a term (to be implemented)
readStringStub :: (String -> Core.Term)
readStringStub s = (Core.TermLiteral (Core.LiteralString (Strings.cat2 "TODO: read " s)))

showValue :: (t0 -> String)
showValue value = "TODO: implement showValue"
