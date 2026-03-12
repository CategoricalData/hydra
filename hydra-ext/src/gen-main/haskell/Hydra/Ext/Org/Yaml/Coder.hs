-- Note: this is an automatically generated file. Do not edit.

-- | YAML encoding and decoding for Hydra terms

module Hydra.Ext.Org.Yaml.Coder where

import qualified Hydra.Compute as Compute
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Ext.Org.Yaml.Model as Model
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Literals as Literals
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core__
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Create a YAML coder for a given type
yamlCoder :: (Core.Type -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.OtherError) (Compute.Coder Core.Term Model.Node))
yamlCoder typ cx g = (termCoder typ cx g)

-- | Create a YAML coder for literal types
literalYamlCoder :: (Core.LiteralType -> Either t0 (Compute.Coder Core.Literal Model.Scalar))
literalYamlCoder lt =  
  let decodeBool = (\cx -> \s -> (\x -> case x of
          Model.ScalarBool v0 -> (Right (Core.LiteralBoolean v0))
          _ -> (Left (Context.InContext {
            Context.inContextObject = (Error.OtherError (Strings.cat [
              "expected boolean, found scalar"])),
            Context.inContextContext = cx}))) s)
  in  
    let decodeFloat = (\ft -> \cx -> \s -> (\x -> case x of
            Model.ScalarFloat v0 -> (Right (Core.LiteralFloat (Literals.bigfloatToFloatValue ft v0)))
            _ -> (Left (Context.InContext {
              Context.inContextObject = (Error.OtherError (Strings.cat [
                "expected float, found scalar"])),
              Context.inContextContext = cx}))) s)
    in  
      let decodeInteger = (\it -> \cx -> \s -> (\x -> case x of
              Model.ScalarInt v0 -> (Right (Core.LiteralInteger (Literals.bigintToIntegerValue it v0)))
              _ -> (Left (Context.InContext {
                Context.inContextObject = (Error.OtherError (Strings.cat [
                  "expected integer, found scalar"])),
                Context.inContextContext = cx}))) s)
      in  
        let decodeString = (\cx -> \s -> (\x -> case x of
                Model.ScalarStr v0 -> (Right (Core.LiteralString v0))
                _ -> (Left (Context.InContext {
                  Context.inContextObject = (Error.OtherError (Strings.cat [
                    "expected string, found scalar"])),
                  Context.inContextContext = cx}))) s)
        in  
          let encoded = ((\x -> case x of
                  Core.LiteralTypeBoolean -> Compute.Coder {
                    Compute.coderEncode = (\cx -> \lit -> Eithers.bind (Core_.booleanLiteral cx lit) (\b -> Right (Model.ScalarBool b))),
                    Compute.coderDecode = decodeBool}
                  Core.LiteralTypeFloat v0 -> Compute.Coder {
                    Compute.coderEncode = (\cx -> \lit -> Eithers.bind (Core_.floatLiteral cx lit) (\f -> Right (Model.ScalarFloat (Literals.floatValueToBigfloat f)))),
                    Compute.coderDecode = (decodeFloat v0)}
                  Core.LiteralTypeInteger v0 -> Compute.Coder {
                    Compute.coderEncode = (\cx -> \lit -> Eithers.bind (Core_.integerLiteral cx lit) (\i -> Right (Model.ScalarInt (Literals.integerValueToBigint i)))),
                    Compute.coderDecode = (decodeInteger v0)}
                  Core.LiteralTypeString -> Compute.Coder {
                    Compute.coderEncode = (\cx -> \lit -> Eithers.bind (Core_.stringLiteral cx lit) (\s -> Right (Model.ScalarStr s))),
                    Compute.coderDecode = decodeString}) lt)
          in (Right encoded)

-- | Create a YAML coder for record types
recordCoder :: (Core.RowType -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.OtherError) (Compute.Coder Core.Term Model.Node))
recordCoder rt cx g =  
  let fields = (Core.rowTypeFields rt)
  in  
    let getCoder = (\f -> Eithers.bind (termCoder (Core.fieldTypeType f) cx g) (\coder -> Right (f, coder)))
    in (Eithers.bind (Eithers.mapList getCoder fields) (\coders -> Right (Compute.Coder {
      Compute.coderEncode = (\cx -> \term -> encodeRecord coders cx g term),
      Compute.coderDecode = (\cx -> \val -> decodeRecord rt coders cx val)})))

-- | Encode a record term to YAML
encodeRecord :: ([(Core.FieldType, (Compute.Coder Core.Term Model.Node))] -> Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.OtherError) Model.Node)
encodeRecord coders cx graph term =  
  let stripped = (Rewriting.deannotateTerm term)
  in  
    let isMaybeNothing = (\ft -> \fvalue -> (\x -> case x of
            Core.TypeMaybe _ -> ((\x -> case x of
              Core.TermMaybe v1 -> (Maybes.isNothing v1)
              _ -> False) fvalue)
            _ -> False) (Core.fieldTypeType ft))
    in  
      let encodeField = (\coderAndField ->  
              let ftAndCoder = (Pairs.first coderAndField)
              in  
                let field = (Pairs.second coderAndField)
                in  
                  let ft = (Pairs.first ftAndCoder)
                  in  
                    let coder_ = (Pairs.second ftAndCoder)
                    in  
                      let fname = (Core.fieldName field)
                      in  
                        let fvalue = (Core.fieldTerm field)
                        in (Logic.ifElse (isMaybeNothing ft fvalue) (Right Nothing) (Eithers.bind (Compute.coderEncode coder_ cx fvalue) (\encoded -> Right (Just (Model.NodeScalar (Model.ScalarStr (Core.unName fname)), encoded))))))
      in (Eithers.bind (Core_.termRecord cx graph stripped) (\record ->  
        let fields = (Core.recordFields record)
        in (Eithers.bind (Eithers.mapList encodeField (Lists.zip coders fields)) (\maybeFields -> Right (Model.NodeMapping (Maps.fromList (Maybes.cat maybeFields)))))))

-- | Decode a YAML value to a record term
decodeRecord :: (Core.RowType -> [(Core.FieldType, (Compute.Coder Core.Term Model.Node))] -> Context.Context -> Model.Node -> Either (Context.InContext Error.OtherError) Core.Term)
decodeRecord rt coders cx n =  
  let decodeObjectBody = (\m ->  
          let decodeField = (\coder ->  
                  let ft = (Pairs.first coder)
                  in  
                    let coder_ = (Pairs.second coder)
                    in  
                      let fname = (Core.fieldTypeName ft)
                      in  
                        let defaultValue = (Model.NodeScalar Model.ScalarNull)
                        in  
                          let yamlValue = (Maybes.fromMaybe defaultValue (Maps.lookup (Model.NodeScalar (Model.ScalarStr (Core.unName fname))) m))
                          in (Eithers.bind (Compute.coderDecode coder_ cx yamlValue) (\v -> Right (Core.Field {
                            Core.fieldName = fname,
                            Core.fieldTerm = v}))))
          in (Eithers.bind (Eithers.mapList decodeField coders) (\fields -> Right (Core.TermRecord (Core.Record {
            Core.recordTypeName = (Core.rowTypeTypeName rt),
            Core.recordFields = fields})))))
  in ((\x -> case x of
    Model.NodeMapping v0 -> (decodeObjectBody v0)
    _ -> (Left (Context.InContext {
      Context.inContextObject = (Error.OtherError "expected mapping"),
      Context.inContextContext = cx}))) n)

-- | Create a YAML coder for term types
termCoder :: (Core.Type -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.OtherError) (Compute.Coder Core.Term Model.Node))
termCoder typ cx g =  
  let stripped = (Rewriting.deannotateType typ)
  in  
    let encodeLiteral = (\ac -> \cx -> \term -> (\x -> case x of
            Core.TermLiteral v0 -> (Eithers.bind (Compute.coderEncode ac cx v0) (\scalar -> Right (Model.NodeScalar scalar)))
            _ -> (Left (Context.InContext {
              Context.inContextObject = (Error.OtherError (Strings.cat [
                "expected literal term, found: ",
                (Core__.term term)])),
              Context.inContextContext = cx}))) term)
    in  
      let encodeList = (\lc -> \cx -> \term -> (\x -> case x of
              Core.TermList v0 -> (Eithers.bind (Eithers.mapList (\el -> Compute.coderEncode lc cx el) v0) (\encodedEls -> Right (Model.NodeSequence encodedEls)))
              _ -> (Left (Context.InContext {
                Context.inContextObject = (Error.OtherError (Strings.cat [
                  "expected list term, found: ",
                  (Core__.term term)])),
                Context.inContextContext = cx}))) term)
      in  
        let decodeList = (\lc -> \cx -> \n -> (\x -> case x of
                Model.NodeSequence v0 -> (Eithers.bind (Eithers.mapList (\node -> Compute.coderDecode lc cx node) v0) (\decodedNodes -> Right (Core.TermList decodedNodes)))
                _ -> (Left (Context.InContext {
                  Context.inContextObject = (Error.OtherError "expected sequence"),
                  Context.inContextContext = cx}))) n)
        in  
          let encodeMaybe = (\maybeElementCoder -> \cx -> \maybeTerm ->  
                  let strippedMaybeTerm = (Rewriting.deannotateTerm maybeTerm)
                  in ((\x -> case x of
                    Core.TermMaybe v0 -> (Logic.ifElse (Maybes.isNothing v0) (Right (Model.NodeScalar Model.ScalarNull)) (Eithers.bind (Compute.coderEncode maybeElementCoder cx (Maybes.fromJust v0)) (\encodedInner -> Right encodedInner)))
                    _ -> (Left (Context.InContext {
                      Context.inContextObject = (Error.OtherError (Strings.cat [
                        "expected optional term, found: ",
                        (Core__.term maybeTerm)])),
                      Context.inContextContext = cx}))) strippedMaybeTerm))
          in  
            let decodeMaybe = (\maybeElementCoder -> \cx -> \yamlVal -> (\x -> case x of
                    Model.NodeScalar v0 -> ((\x -> case x of
                      Model.ScalarNull -> (Right (Core.TermMaybe Nothing))
                      _ -> (Eithers.bind (Compute.coderDecode maybeElementCoder cx yamlVal) (\decodedInner -> Right (Core.TermMaybe (Just decodedInner))))) v0)
                    _ -> (Eithers.bind (Compute.coderDecode maybeElementCoder cx yamlVal) (\decodedInner -> Right (Core.TermMaybe (Just decodedInner))))) yamlVal)
            in  
              let result = ((\x -> case x of
                      Core.TypeLiteral v0 -> (Eithers.bind (literalYamlCoder v0) (\ac -> Right (Compute.Coder {
                        Compute.coderEncode = (encodeLiteral ac),
                        Compute.coderDecode = (\cx -> \n -> (\x -> case x of
                          Model.NodeScalar v1 -> (Eithers.bind (Compute.coderDecode ac cx v1) (\lit -> Right (Core.TermLiteral lit)))
                          _ -> (Left (Context.InContext {
                            Context.inContextObject = (Error.OtherError "expected scalar node"),
                            Context.inContextContext = cx}))) n)})))
                      Core.TypeList v0 -> (Eithers.bind (termCoder v0 cx g) (\lc -> Right (Compute.Coder {
                        Compute.coderEncode = (encodeList lc),
                        Compute.coderDecode = (decodeList lc)})))
                      Core.TypeMap v0 ->  
                        let kt = (Core.mapTypeKeys v0)
                        in  
                          let vt = (Core.mapTypeValues v0)
                          in (Eithers.bind (termCoder kt cx g) (\kc -> Eithers.bind (termCoder vt cx g) (\vc ->  
                            let encodeEntry = (\cx -> \kv ->  
                                    let k = (Pairs.first kv)
                                    in  
                                      let v = (Pairs.second kv)
                                      in (Eithers.bind (Compute.coderEncode kc cx k) (\encodedK -> Eithers.bind (Compute.coderEncode vc cx v) (\encodedV -> Right (encodedK, encodedV)))))
                            in  
                              let decodeEntry = (\cx -> \kv ->  
                                      let k = (Pairs.first kv)
                                      in  
                                        let v = (Pairs.second kv)
                                        in (Eithers.bind (Compute.coderDecode kc cx k) (\decodedK -> Eithers.bind (Compute.coderDecode vc cx v) (\decodedV -> Right (decodedK, decodedV)))))
                              in (Right (Compute.Coder {
                                Compute.coderEncode = (\cx -> \term -> (\x -> case x of
                                  Core.TermMap v1 -> (Eithers.bind (Eithers.mapList (\entry -> encodeEntry cx entry) (Maps.toList v1)) (\entries -> Right (Model.NodeMapping (Maps.fromList entries))))
                                  _ -> (Left (Context.InContext {
                                    Context.inContextObject = (Error.OtherError (Strings.cat [
                                      "expected map term, found: ",
                                      (Core__.term term)])),
                                    Context.inContextContext = cx}))) term),
                                Compute.coderDecode = (\cx -> \n -> (\x -> case x of
                                  Model.NodeMapping v1 -> (Eithers.bind (Eithers.mapList (\entry -> decodeEntry cx entry) (Maps.toList v1)) (\entries -> Right (Core.TermMap (Maps.fromList entries))))
                                  _ -> (Left (Context.InContext {
                                    Context.inContextObject = (Error.OtherError "expected mapping"),
                                    Context.inContextContext = cx}))) n)})))))
                      Core.TypeMaybe v0 ->  
                        let isNestedMaybe = ((\x -> case x of
                                Core.TypeMaybe _ -> True
                                _ -> False) (Rewriting.deannotateType v0))
                        in (Logic.ifElse isNestedMaybe (Eithers.bind (termCoder (Core.TypeList v0) cx g) (\listCoder -> Right (Compute.Coder {
                          Compute.coderEncode = (\cx -> \maybeTerm ->  
                            let strippedMaybeTerm = (Rewriting.deannotateTerm maybeTerm)
                            in ((\x -> case x of
                              Core.TermMaybe v1 -> (Logic.ifElse (Maybes.isNothing v1) (Compute.coderEncode listCoder cx (Core.TermList [])) (Compute.coderEncode listCoder cx (Core.TermList [
                                Maybes.fromJust v1])))
                              _ -> (Left (Context.InContext {
                                Context.inContextObject = (Error.OtherError (Strings.cat [
                                  "expected optional term, found: ",
                                  (Core__.term maybeTerm)])),
                                Context.inContextContext = cx}))) strippedMaybeTerm)),
                          Compute.coderDecode = (\cx -> \n -> Eithers.bind (Compute.coderDecode listCoder cx n) (\listTerm -> (\x -> case x of
                            Core.TermList v1 -> (Logic.ifElse (Lists.null v1) (Right (Core.TermMaybe Nothing)) (Right (Core.TermMaybe (Just (Lists.head v1)))))
                            _ -> (Left (Context.InContext {
                              Context.inContextObject = (Error.OtherError "expected list term after decoding"),
                              Context.inContextContext = cx}))) listTerm))}))) (Eithers.bind (termCoder v0 cx g) (\maybeElementCoder -> Right (Compute.Coder {
                          Compute.coderEncode = (encodeMaybe maybeElementCoder),
                          Compute.coderDecode = (decodeMaybe maybeElementCoder)}))))
                      Core.TypeRecord v0 -> (recordCoder v0 cx g)
                      Core.TypeSet v0 -> (Eithers.bind (termCoder v0 cx g) (\lc -> Right (Compute.Coder {
                        Compute.coderEncode = (\cx -> \term -> (\x -> case x of
                          Core.TermSet v1 -> (encodeList lc cx (Core.TermList (Sets.toList v1)))
                          _ -> (Left (Context.InContext {
                            Context.inContextObject = (Error.OtherError (Strings.cat [
                              "expected set term, found: ",
                              (Core__.term term)])),
                            Context.inContextContext = cx}))) term),
                        Compute.coderDecode = (\cx -> \n -> Eithers.bind (decodeList lc cx n) (\listTerm -> (\x -> case x of
                          Core.TermList v1 -> (Right (Core.TermSet (Sets.fromList v1)))
                          _ -> (Left (Context.InContext {
                            Context.inContextObject = (Error.OtherError "expected list term after decoding"),
                            Context.inContextContext = cx}))) listTerm))})))
                      Core.TypeUnion v0 ->  
                        let nm = (Core.rowTypeTypeName v0)
                        in  
                          let sfields = (Core.rowTypeFields v0)
                          in (Eithers.bind (Eithers.mapList (\f -> Eithers.bind (termCoder (Core.fieldTypeType f) cx g) (\fc -> Right (Core.unName (Core.fieldTypeName f), fc))) sfields) (\fieldCoders ->  
                            let coderMap = (Maps.fromList fieldCoders)
                            in (Right (Compute.Coder {
                              Compute.coderEncode = (\cx -> \term -> Eithers.bind (Core_.injection cx nm g term) (\field ->  
                                let fn = (Core.fieldName field)
                                in  
                                  let fterm = (Core.fieldTerm field)
                                  in  
                                    let fnStr = (Core.unName fn)
                                    in (Maybes.maybe (Left (Context.InContext {
                                      Context.inContextObject = (Error.OtherError (Strings.cat [
                                        "no coder for union field: ",
                                        fnStr])),
                                      Context.inContextContext = cx})) (\cdr -> Eithers.bind (Compute.coderEncode cdr cx fterm) (\encodedVal -> Right (Model.NodeMapping (Maps.fromList [
                                      (Model.NodeScalar (Model.ScalarStr fnStr), encodedVal)])))) (Maps.lookup fnStr coderMap)))),
                              Compute.coderDecode = (\cx -> \n -> (\x -> case x of
                                Model.NodeMapping v1 ->  
                                  let entries = (Maps.toList v1)
                                  in (Logic.ifElse (Lists.null entries) (Left (Context.InContext {
                                    Context.inContextObject = (Error.OtherError "empty mapping for union"),
                                    Context.inContextContext = cx})) ( 
                                    let entry = (Lists.head entries)
                                    in  
                                      let keyNode = (Pairs.first entry)
                                      in  
                                        let valNode = (Pairs.second entry)
                                        in ((\x -> case x of
                                          Model.NodeScalar v2 -> ((\x -> case x of
                                            Model.ScalarStr v3 -> (Maybes.maybe (Left (Context.InContext {
                                              Context.inContextObject = (Error.OtherError (Strings.cat [
                                                "unknown union field: ",
                                                v3])),
                                              Context.inContextContext = cx})) (\cdr -> Eithers.bind (Compute.coderDecode cdr cx valNode) (\decodedVal -> Right (Core.TermUnion (Core.Injection {
                                              Core.injectionTypeName = nm,
                                              Core.injectionField = Core.Field {
                                                Core.fieldName = (Core.Name v3),
                                                Core.fieldTerm = decodedVal}})))) (Maps.lookup v3 coderMap))
                                            _ -> (Left (Context.InContext {
                                              Context.inContextObject = (Error.OtherError "expected string key in union mapping"),
                                              Context.inContextContext = cx}))) v2)
                                          _ -> (Left (Context.InContext {
                                            Context.inContextObject = (Error.OtherError "expected scalar key in union mapping"),
                                            Context.inContextContext = cx}))) keyNode)))
                                _ -> (Left (Context.InContext {
                                  Context.inContextObject = (Error.OtherError "expected mapping for union"),
                                  Context.inContextContext = cx}))) n)}))))
                      Core.TypeUnit -> (Right unitCoder)
                      Core.TypeWrap v0 ->  
                        let tname = (Core.wrappedTypeTypeName v0)
                        in (Eithers.bind (termCoder (Core.wrappedTypeBody v0) cx g) (\innerCoder -> Right (Compute.Coder {
                          Compute.coderEncode = (\cx -> \term -> Eithers.bind (Core_.wrap cx tname g term) (\inner -> Compute.coderEncode innerCoder cx inner)),
                          Compute.coderDecode = (\cx -> \n -> Eithers.bind (Compute.coderDecode innerCoder cx n) (\decoded -> Right (Core.TermWrap (Core.WrappedTerm {
                            Core.wrappedTermTypeName = tname,
                            Core.wrappedTermBody = decoded}))))})))
                      _ -> (Left (Context.InContext {
                        Context.inContextObject = (Error.OtherError (Strings.cat [
                          "unsupported type in YAML: ",
                          (Core__.type_ typ)])),
                        Context.inContextContext = cx}))) stripped)
              in result

-- | YAML coder for unit values
unitCoder :: (Compute.Coder Core.Term Model.Node)
unitCoder = Compute.Coder {
  Compute.coderEncode = encodeUnit,
  Compute.coderDecode = decodeUnit} 
  where 
    encodeUnit = (\cx -> \term -> (\x -> case x of
      Core.TermUnit -> (Right (Model.NodeScalar Model.ScalarNull))
      _ -> (Left (Context.InContext {
        Context.inContextObject = (Error.OtherError (Strings.cat [
          "expected unit, found: ",
          (Core__.term term)])),
        Context.inContextContext = cx}))) (Rewriting.deannotateTerm term))
    decodeUnit = (\cx -> \n -> (\x -> case x of
      Model.NodeScalar v0 -> ((\x -> case x of
        Model.ScalarNull -> (Right Core.TermUnit)
        _ -> (Left (Context.InContext {
          Context.inContextObject = (Error.OtherError "expected null scalar"),
          Context.inContextContext = cx}))) v0)
      _ -> (Left (Context.InContext {
        Context.inContextObject = (Error.OtherError "expected null"),
        Context.inContextContext = cx}))) n)
