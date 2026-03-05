-- Note: this is an automatically generated file. Do not edit.

-- | JSON encoding and decoding for Hydra terms

module Hydra.Ext.Org.Json.Coder where

import qualified Hydra.Adapt.Modules as Modules
import qualified Hydra.Adapt.Utils as Utils
import qualified Hydra.Compute as Compute
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Error as Error
import qualified Hydra.Ext.Org.Json.Language as Language
import qualified Hydra.Extract.Core as Core__
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Literals as Literals_
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core___
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Create a JSON coder for a given type
jsonCoder :: (Core.Type -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.OtherError) (Compute.Coder Core.Term Model.Value))
jsonCoder typ cx g =  
  let mkTermCoder = (\t -> termCoder t cx g)
  in (Eithers.bind (Eithers.bimap (\_s -> Context.InContext {
    Context.inContextObject = (Error.OtherError _s),
    Context.inContextContext = cx}) (\_x -> _x) (Modules.languageAdapter Language.jsonLanguage cx g typ)) (\adapter -> Eithers.bind (mkTermCoder (Compute.adapterTarget adapter)) (\coder -> Right (Utils.composeCoders (Compute.adapterCoder adapter) coder))))

-- | Create a JSON coder for literal types
literalJsonCoder :: (Core.LiteralType -> Either t0 (Compute.Coder Core.Literal Model.Value))
literalJsonCoder lt =  
  let decodeBool = (\cx -> \s -> (\x -> case x of
          Model.ValueBoolean v0 -> (Right (Core.LiteralBoolean v0))
          _ -> (Left (Context.InContext {
            Context.inContextObject = (Error.OtherError (Strings.cat [
              "expected boolean, found: ",
              (showValue s)])),
            Context.inContextContext = cx}))) s)
  in  
    let decodeFloat = (\cx -> \s -> (\x -> case x of
            Model.ValueNumber v0 -> (Right (Core.LiteralFloat (Core.FloatValueBigfloat v0)))
            _ -> (Left (Context.InContext {
              Context.inContextObject = (Error.OtherError (Strings.cat [
                "expected number, found: ",
                (showValue s)])),
              Context.inContextContext = cx}))) s)
    in  
      let decodeInteger = (\cx -> \s -> (\x -> case x of
              Model.ValueNumber v0 ->  
                let bi = (Literals.bigfloatToBigint v0)
                in (Right (Core.LiteralInteger (Core.IntegerValueBigint bi)))
              _ -> (Left (Context.InContext {
                Context.inContextObject = (Error.OtherError (Strings.cat [
                  "expected number, found: ",
                  (showValue s)])),
                Context.inContextContext = cx}))) s)
      in  
        let decodeString = (\cx -> \s -> (\x -> case x of
                Model.ValueString v0 -> (Right (Core.LiteralString v0))
                _ -> (Left (Context.InContext {
                  Context.inContextObject = (Error.OtherError (Strings.cat [
                    "expected string, found: ",
                    (showValue s)])),
                  Context.inContextContext = cx}))) s)
        in  
          let encoded = ((\x -> case x of
                  Core.LiteralTypeBoolean -> Compute.Coder {
                    Compute.coderEncode = (\cx -> \lit -> Eithers.bind (Core__.booleanLiteral cx lit) (\b -> Right (Model.ValueBoolean b))),
                    Compute.coderDecode = decodeBool}
                  Core.LiteralTypeFloat _ -> Compute.Coder {
                    Compute.coderEncode = (\cx -> \lit -> Eithers.bind (Core__.floatLiteral cx lit) (\f -> Eithers.bind (Core__.bigfloatValue cx f) (\bf -> Right (Model.ValueNumber bf)))),
                    Compute.coderDecode = decodeFloat}
                  Core.LiteralTypeInteger _ -> Compute.Coder {
                    Compute.coderEncode = (\cx -> \lit -> Eithers.bind (Core__.integerLiteral cx lit) (\i -> Eithers.bind (Core__.bigintValue cx i) (\bi -> Right (Model.ValueNumber (Literals.bigintToBigfloat bi))))),
                    Compute.coderDecode = decodeInteger}
                  Core.LiteralTypeString -> Compute.Coder {
                    Compute.coderEncode = (\cx -> \lit -> Eithers.bind (Core__.stringLiteral cx lit) (\s -> Right (Model.ValueString s))),
                    Compute.coderDecode = decodeString}) lt)
          in (Right encoded)

-- | Create a JSON coder for record types
recordCoder :: (Core.RowType -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.OtherError) (Compute.Coder Core.Term Model.Value))
recordCoder rt cx g =  
  let fields = (Core.rowTypeFields rt)
  in  
    let getCoder = (\f -> Eithers.bind (termCoder (Core.fieldTypeType f) cx g) (\coder -> Right (f, coder)))
    in (Eithers.bind (Eithers.mapList getCoder fields) (\coders -> Right (Compute.Coder {
      Compute.coderEncode = (\cx -> \term -> encodeRecord coders cx g term),
      Compute.coderDecode = (\cx -> \val -> decodeRecord rt coders cx val)})))

-- | Encode a record term to JSON
encodeRecord :: ([(Core.FieldType, (Compute.Coder Core.Term Model.Value))] -> Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.OtherError) Model.Value)
encodeRecord coders cx graph term =  
  let stripped = (Rewriting.deannotateTerm term)
  in  
    let matchMaybeTerm = (\fvalue -> \coder_ -> \fname -> \dflt -> (\x -> case x of
            Core.TermMaybe v0 -> (Maybes.maybe (Right Nothing) (\v -> Eithers.bind (Compute.coderEncode coder_ cx v) (\encoded -> Right (Just (Core.unName fname, encoded)))) v0)
            _ -> dflt) fvalue)
    in  
      let matchTypeForMaybe = (\ft -> \forMaybe -> \dflt -> (\x -> case x of
              Core.TypeMaybe v0 -> (forMaybe v0)
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
                                    let dflt = (Eithers.bind (Compute.coderEncode coder_ cx fvalue) (\encoded -> Right (Just (Core.unName fname, encoded))))
                                    in (matchMaybeTerm fvalue coder_ fname dflt))
                            in  
                              let dflt = (Eithers.bind (Compute.coderEncode coder_ cx fvalue) (\encoded -> Right (Just (Core.unName fname, encoded))))
                              in (matchTypeForMaybe ft forMaybe dflt))
        in (Eithers.bind (Core__.termRecord cx graph stripped) (\record ->  
          let fields = (Core.recordFields record)
          in (Eithers.bind (Eithers.mapList encodeField (Lists.zip coders fields)) (\maybeFields -> Right (Model.ValueObject (Maps.fromList (Maybes.cat maybeFields)))))))

-- | Decode a JSON value to a record term
decodeRecord :: (Core.RowType -> [(Core.FieldType, (Compute.Coder Core.Term Model.Value))] -> Context.Context -> Model.Value -> Either (Context.InContext Error.OtherError) Core.Term)
decodeRecord rt coders cx n =  
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
                          in (Eithers.bind (Compute.coderDecode coder_ cx jsonValue) (\v -> Right (Core.Field {
                            Core.fieldName = fname,
                            Core.fieldTerm = v}))))
          in (Eithers.bind (Eithers.mapList decodeField coders) (\fields -> Right (Core.TermRecord (Core.Record {
            Core.recordTypeName = (Core.rowTypeTypeName rt),
            Core.recordFields = fields})))))
  in ((\x -> case x of
    Model.ValueObject v0 -> (decodeObjectBody v0)
    _ -> (Left (Context.InContext {
      Context.inContextObject = (Error.OtherError (Strings.cat [
        "expected object, found: ",
        (showValue n)])),
      Context.inContextContext = cx}))) n)

-- | Create a JSON coder for term types
termCoder :: (Core.Type -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.OtherError) (Compute.Coder Core.Term Model.Value))
termCoder typ cx g =  
  let stripped = (Rewriting.deannotateType typ)
  in  
    let encodeLiteral = (\ac -> \cx -> \term -> (\x -> case x of
            Core.TermLiteral v0 -> (Compute.coderEncode ac cx v0)
            _ -> (Left (Context.InContext {
              Context.inContextObject = (Error.OtherError (Strings.cat [
                "expected literal term, found: ",
                (Core___.term term)])),
              Context.inContextContext = cx}))) term)
    in  
      let encodeList = (\lc -> \cx -> \term -> (\x -> case x of
              Core.TermList v0 -> (Eithers.bind (Eithers.mapList (\el -> Compute.coderEncode lc cx el) v0) (\encodedEls -> Right (Model.ValueArray encodedEls)))
              _ -> (Left (Context.InContext {
                Context.inContextObject = (Error.OtherError (Strings.cat [
                  "expected list term, found: ",
                  (Core___.term term)])),
                Context.inContextContext = cx}))) term)
      in  
        let decodeList = (\lc -> \cx -> \n -> (\x -> case x of
                Model.ValueArray v0 -> (Eithers.bind (Eithers.mapList (\node -> Compute.coderDecode lc cx node) v0) (\decodedNodes -> Right (Core.TermList decodedNodes)))
                _ -> (Left (Context.InContext {
                  Context.inContextObject = (Error.OtherError (Strings.cat [
                    "expected sequence, found: ",
                    (showValue n)])),
                  Context.inContextContext = cx}))) n)
        in  
          let matchLiteralString = (\v -> \lit -> (\x -> case x of
                  Core.LiteralString v0 -> v0
                  _ -> (Core___.term v)) lit)
          in  
            let matchTermLiteral = (\v -> (\x -> case x of
                    Core.TermLiteral v0 -> (matchLiteralString v v0)
                    _ -> (Core___.term v)) (Rewriting.deannotateTerm v))
            in  
              let encodeMaybe = (\maybeElementCoder -> \cx -> \maybeTerm ->  
                      let strippedMaybeTerm = (Rewriting.deannotateTerm maybeTerm)
                      in ((\x -> case x of
                        Core.TermMaybe v0 -> (Logic.ifElse (Maybes.isNothing v0) (Right Model.ValueNull) (Eithers.bind (Compute.coderEncode maybeElementCoder cx (Maybes.fromJust v0)) (\encodedInner -> Right encodedInner)))
                        _ -> (Left (Context.InContext {
                          Context.inContextObject = (Error.OtherError (Strings.cat [
                            "expected optional term, found: ",
                            (Core___.term maybeTerm)])),
                          Context.inContextContext = cx}))) strippedMaybeTerm))
              in  
                let decodeMaybe = (\maybeElementCoder -> \cx -> \jsonVal -> (\x -> case x of
                        Model.ValueNull -> (Right (Core.TermMaybe Nothing))
                        _ -> (Eithers.bind (Compute.coderDecode maybeElementCoder cx jsonVal) (\decodedInner -> Right (Core.TermMaybe (Just decodedInner))))) jsonVal)
                in  
                  let result = ((\x -> case x of
                          Core.TypeLiteral v0 -> (Eithers.bind (literalJsonCoder v0) (\ac -> Right (Compute.Coder {
                            Compute.coderEncode = (encodeLiteral ac),
                            Compute.coderDecode = (\cx -> \n -> Eithers.bind (Compute.coderDecode ac cx n) (\lit -> Right (Core.TermLiteral lit)))})))
                          Core.TypeList v0 -> (Eithers.bind (termCoder v0 cx g) (\lc -> Right (Compute.Coder {
                            Compute.coderEncode = (encodeList lc),
                            Compute.coderDecode = (decodeList lc)})))
                          Core.TypeMap v0 ->  
                            let kt = (Core.mapTypeKeys v0)
                            in  
                              let vt = (Core.mapTypeValues v0)
                              in (Eithers.bind (termCoder kt cx g) (\kc -> Eithers.bind (termCoder vt cx g) (\vc ->  
                                let isStringKey = (Equality.equal (Rewriting.deannotateType kt) (Core.TypeLiteral Core.LiteralTypeString))
                                in  
                                  let toString = (\v -> Logic.ifElse isStringKey (matchTermLiteral v) (Core___.term v))
                                  in  
                                    let fromString = (\s -> Logic.ifElse isStringKey (Core.TermLiteral (Core.LiteralString s)) (readStringStub s))
                                    in  
                                      let encodeEntry = (\cx -> \kv ->  
                                              let k = (Pairs.first kv)
                                              in  
                                                let v = (Pairs.second kv)
                                                in (Eithers.bind (Compute.coderEncode vc cx v) (\encodedV -> Right (toString k, encodedV))))
                                      in  
                                        let decodeEntry = (\cx -> \kv ->  
                                                let k = (Pairs.first kv)
                                                in  
                                                  let v = (Pairs.second kv)
                                                  in (Eithers.bind (Compute.coderDecode vc cx v) (\decodedV -> Right (fromString k, decodedV))))
                                        in (Right (Compute.Coder {
                                          Compute.coderEncode = (\cx -> \term -> (\x -> case x of
                                            Core.TermMap v1 -> (Eithers.bind (Eithers.mapList (\entry -> encodeEntry cx entry) (Maps.toList v1)) (\entries -> Right (Model.ValueObject (Maps.fromList entries))))
                                            _ -> (Left (Context.InContext {
                                              Context.inContextObject = (Error.OtherError (Strings.cat [
                                                "expected map term, found: ",
                                                (Core___.term term)])),
                                              Context.inContextContext = cx}))) term),
                                          Compute.coderDecode = (\cx -> \n -> (\x -> case x of
                                            Model.ValueObject v1 -> (Eithers.bind (Eithers.mapList (\entry -> decodeEntry cx entry) (Maps.toList v1)) (\entries -> Right (Core.TermMap (Maps.fromList entries))))
                                            _ -> (Left (Context.InContext {
                                              Context.inContextObject = (Error.OtherError (Strings.cat [
                                                "expected mapping, found: ",
                                                (showValue n)])),
                                              Context.inContextContext = cx}))) n)})))))
                          Core.TypeMaybe v0 -> (Eithers.bind (termCoder v0 cx g) (\maybeElementCoder -> Right (Compute.Coder {
                            Compute.coderEncode = (encodeMaybe maybeElementCoder),
                            Compute.coderDecode = (decodeMaybe maybeElementCoder)})))
                          Core.TypeRecord v0 -> (recordCoder v0 cx g)
                          Core.TypeUnit -> (Right unitCoder)
                          Core.TypeVariable v0 -> (Right (Compute.Coder {
                            Compute.coderEncode = (\_cx -> \term -> Right (Model.ValueString (Strings.cat [
                              "variable '",
                              (Core.unName v0),
                              "' for: ",
                              (Core___.term term)]))),
                            Compute.coderDecode = (\cx -> \_term -> Left (Context.InContext {
                              Context.inContextObject = (Error.OtherError (Strings.cat [
                                "type variable ",
                                (Core.unName v0),
                                " does not support decoding"])),
                              Context.inContextContext = cx}))}))
                          _ -> (Left (Context.InContext {
                            Context.inContextObject = (Error.OtherError (Strings.cat [
                              "unsupported type in JSON: ",
                              (Core___.type_ typ)])),
                            Context.inContextContext = cx}))) stripped)
                  in result

-- | JSON coder for unit values
unitCoder :: (Compute.Coder Core.Term Model.Value)
unitCoder = Compute.Coder {
  Compute.coderEncode = encodeUnit,
  Compute.coderDecode = decodeUnit} 
  where 
    encodeUnit = (\cx -> \term -> (\x -> case x of
      Core.TermUnit -> (Right Model.ValueNull)
      _ -> (Left (Context.InContext {
        Context.inContextObject = (Error.OtherError (Strings.cat [
          "expected unit, found: ",
          (Core___.term term)])),
        Context.inContextContext = cx}))) (Rewriting.deannotateTerm term))
    decodeUnit = (\cx -> \n -> (\x -> case x of
      Model.ValueNull -> (Right Core.TermUnit)
      _ -> (Left (Context.InContext {
        Context.inContextObject = (Error.OtherError (Strings.cat [
          "expected null, found: ",
          (showValue n)])),
        Context.inContextContext = cx}))) n)

-- | A simplistic, unidirectional encoding for terms as JSON values. Not type-aware; best used for human consumption.
untypedTermToJson :: (Core.Term -> Either t0 Model.Value)
untypedTermToJson term =  
  let recurse = (\t -> untypedTermToJson t)
  in  
    let unexp = (\msg -> Right (Model.ValueString (Strings.cat2 "FAIL: " msg)))
    in  
      let asRecord = (\fields -> recurse (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name ""),
              Core.recordFields = fields})))
      in  
        let asVariant = (\name -> \term -> recurse (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = (Core.Name ""),
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name name),
                  Core.fieldTerm = term}})))
        in  
          let matchTermMaybe = (\forTerm -> \t -> (\x -> case x of
                  Core.TermMaybe v0 -> (Maybes.maybe (Right Nothing) forTerm v0)
                  _ -> (Eithers.map Maybes.pure (recurse t))) t)
          in  
            let matchElimination = (\unexp -> \asVariant -> \elm -> (\x -> case x of
                    Core.EliminationRecord v0 -> (asVariant "project" (Core.TermVariable (Core.projectionField v0)))
                    _ -> (unexp (Strings.cat [
                      "unexpected elimination variant: ",
                      (Core___.elimination elm)]))) elm)
            in  
              let matchFunction = (\unexp -> \asRecord -> \asVariant -> \f -> (\x -> case x of
                      Core.FunctionElimination v0 -> (matchElimination unexp asVariant v0)
                      Core.FunctionLambda v0 -> (asRecord [
                        Core.Field {
                          Core.fieldName = (Core.Name "parameter"),
                          Core.fieldTerm = (Core.TermVariable (Core.lambdaParameter v0))},
                        Core.Field {
                          Core.fieldName = (Core.Name "domain"),
                          Core.fieldTerm = (Core.TermMaybe (Maybes.map Core_.type_ (Core.lambdaDomain v0)))},
                        Core.Field {
                          Core.fieldName = (Core.Name "body"),
                          Core.fieldTerm = (Core.lambdaBody v0)}])
                      Core.FunctionPrimitive v0 -> (Right (Model.ValueString (Core.unName v0)))) f)
              in  
                let matchLiteral = (\lit -> (\x -> case x of
                        Core.LiteralBinary v0 -> (Model.ValueString (Literals.binaryToString v0))
                        Core.LiteralBoolean v0 -> (Model.ValueBoolean v0)
                        Core.LiteralFloat v0 -> (Model.ValueNumber (Literals_.floatValueToBigfloat v0))
                        Core.LiteralInteger v0 ->  
                          let bf = (Literals_.integerValueToBigint v0)
                          in  
                            let f = (Literals.bigintToBigfloat bf)
                            in (Model.ValueNumber f)
                        Core.LiteralString v0 -> (Model.ValueString v0)) lit)
                in  
                  let fieldToKeyval = (\f ->  
                          let forTerm = (\t -> matchTermMaybe forTerm t)
                          in (Eithers.bind (forTerm (Core.fieldTerm f)) (\mjson -> Right (Maybes.map (\j -> (Core.unName (Core.fieldName f), j)) mjson))))
                  in  
                    let result = ((\x -> case x of
                            Core.TermAnnotated v0 ->  
                              let term1 = (Core.annotatedTermBody v0)
                              in  
                                let ann = (Core.annotatedTermAnnotation v0)
                                in  
                                  let encodePair = (\kv ->  
                                          let k = (Core.unName (Pairs.first kv))
                                          in  
                                            let v = (Pairs.second kv)
                                            in (Eithers.bind (recurse v) (\json -> Right (k, json))))
                                  in (Eithers.bind (recurse term1) (\json -> Eithers.bind (Eithers.mapList encodePair (Maps.toList ann)) (\pairs -> Right (Model.ValueObject (Maps.fromList [
                                    ("term", json),
                                    ("annotations", (Model.ValueObject (Maps.fromList pairs)))])))))
                            Core.TermApplication v0 -> (asRecord [
                              Core.Field {
                                Core.fieldName = (Core.Name "function"),
                                Core.fieldTerm = (Core.applicationFunction v0)},
                              Core.Field {
                                Core.fieldName = (Core.Name "argument"),
                                Core.fieldTerm = (Core.applicationArgument v0)}])
                            Core.TermFunction v0 -> (matchFunction unexp asRecord asVariant v0)
                            Core.TermLet v0 ->  
                              let bindings = (Core.letBindings v0)
                              in  
                                let env = (Core.letBody v0)
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
                            Core.TermList v0 -> (Eithers.bind (Eithers.mapList recurse v0) (\jsonTerms -> Right (Model.ValueArray jsonTerms)))
                            Core.TermLiteral v0 -> (Right (matchLiteral v0))
                            Core.TermMaybe v0 -> (Maybes.maybe (Right Model.ValueNull) recurse v0)
                            Core.TermRecord v0 ->  
                              let fields = (Core.recordFields v0)
                              in (Eithers.bind (Eithers.mapList fieldToKeyval fields) (\keyvals -> Right (Model.ValueObject (Maps.fromList (Maybes.cat keyvals)))))
                            Core.TermSet v0 -> (recurse (Core.TermList (Sets.toList v0)))
                            Core.TermTypeLambda v0 -> (asRecord [
                              Core.Field {
                                Core.fieldName = (Core.Name "parameter"),
                                Core.fieldTerm = (Core.TermVariable (Core.typeLambdaParameter v0))},
                              Core.Field {
                                Core.fieldName = (Core.Name "body"),
                                Core.fieldTerm = (Core.typeLambdaBody v0)}])
                            Core.TermTypeApplication v0 -> (asRecord [
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.typeApplicationTermBody v0)},
                              Core.Field {
                                Core.fieldName = (Core.Name "type"),
                                Core.fieldTerm = (Core_.type_ (Core.typeApplicationTermType v0))}])
                            Core.TermUnion v0 ->  
                              let field = (Core.injectionField v0)
                              in (Logic.ifElse (Equality.equal (Core.fieldTerm field) Core.TermUnit) (Right (Model.ValueString (Core.unName (Core.fieldName field)))) (Eithers.bind (fieldToKeyval field) (\mkeyval -> Right (Model.ValueObject (Maps.fromList (Maybes.maybe [] (\keyval -> [
                                keyval]) mkeyval))))))
                            Core.TermVariable v0 -> (Right (Model.ValueString (Core.unName v0)))
                            Core.TermWrap v0 -> (recurse (Core.wrappedTermBody v0))
                            _ -> (unexp (Strings.cat [
                              "unsupported term variant: ",
                              (Core___.term term)]))) term)
                    in result

-- | Placeholder for reading a string into a term (to be implemented)
readStringStub :: (String -> Core.Term)
readStringStub s = (Core.TermLiteral (Core.LiteralString (Strings.cat2 "TODO: read " s)))

-- | Show a JSON value as a string (placeholder implementation)
showValue :: (t0 -> String)
showValue value = "TODO: implement showValue"
