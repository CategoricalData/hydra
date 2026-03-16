-- Note: this is an automatically generated file. Do not edit.

-- | YAML encoding and decoding for Hydra terms

module Hydra.Ext.Org.Yaml.Coder where

import qualified Hydra.Adapt as Adapt
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Ext.Org.Yaml.Language as Language
import qualified Hydra.Ext.Org.Yaml.Model as Model
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core__
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Create a YAML coder for a given type
yamlCoder :: (Core.Type -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) (Util.Coder Core.Term Model.Node))
yamlCoder typ cx g =  
  let mkTermCoder = (\t -> termCoder t cx g)
  in (Eithers.bind (Eithers.bimap (\_s -> Context.InContext {
    Context.inContextObject = (Error.ErrorOther (Error.OtherError _s)),
    Context.inContextContext = cx}) (\_x -> _x) (Adapt.simpleLanguageAdapter Language.yamlLanguage cx g typ)) (\adapter -> Eithers.bind (mkTermCoder (Util.adapterTarget adapter)) (\coder -> Right (Adapt.composeCoders (Util.adapterCoder adapter) coder))))

-- | Create a YAML coder for literal types
literalYamlCoder :: (Core.LiteralType -> Either t0 (Util.Coder Core.Literal Model.Scalar))
literalYamlCoder lt =  
  let decodeBool = (\cx -> \s -> (\x -> case x of
          Model.ScalarBool v0 -> (Right (Core.LiteralBoolean v0))
          _ -> (Left (Context.InContext {
            Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat [
              "expected boolean, found scalar"]))),
            Context.inContextContext = cx}))) s)
  in  
    let decodeFloat = (\cx -> \s -> (\x -> case x of
            Model.ScalarFloat v0 -> (Right (Core.LiteralFloat (Core.FloatValueBigfloat v0)))
            _ -> (Left (Context.InContext {
              Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat [
                "expected float, found scalar"]))),
              Context.inContextContext = cx}))) s)
    in  
      let decodeInteger = (\cx -> \s -> (\x -> case x of
              Model.ScalarInt v0 -> (Right (Core.LiteralInteger (Core.IntegerValueBigint v0)))
              _ -> (Left (Context.InContext {
                Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat [
                  "expected integer, found scalar"]))),
                Context.inContextContext = cx}))) s)
      in  
        let decodeString = (\cx -> \s -> (\x -> case x of
                Model.ScalarStr v0 -> (Right (Core.LiteralString v0))
                _ -> (Left (Context.InContext {
                  Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat [
                    "expected string, found scalar"]))),
                  Context.inContextContext = cx}))) s)
        in  
          let encoded = ((\x -> case x of
                  Core.LiteralTypeBoolean -> Util.Coder {
                    Util.coderEncode = (\cx -> \lit -> Eithers.bind (Core_.booleanLiteral cx lit) (\b -> Right (Model.ScalarBool b))),
                    Util.coderDecode = decodeBool}
                  Core.LiteralTypeFloat _ -> Util.Coder {
                    Util.coderEncode = (\cx -> \lit -> Eithers.bind (Core_.floatLiteral cx lit) (\f -> Eithers.bind (Core_.bigfloatValue cx f) (\bf -> Right (Model.ScalarFloat bf)))),
                    Util.coderDecode = decodeFloat}
                  Core.LiteralTypeInteger _ -> Util.Coder {
                    Util.coderEncode = (\cx -> \lit -> Eithers.bind (Core_.integerLiteral cx lit) (\i -> Eithers.bind (Core_.bigintValue cx i) (\bi -> Right (Model.ScalarInt bi)))),
                    Util.coderDecode = decodeInteger}
                  Core.LiteralTypeString -> Util.Coder {
                    Util.coderEncode = (\cx -> \lit -> Eithers.bind (Core_.stringLiteral cx lit) (\s -> Right (Model.ScalarStr s))),
                    Util.coderDecode = decodeString}) lt)
          in (Right encoded)

-- | Create a YAML coder for record types
recordCoder :: (Core.Name -> [Core.FieldType] -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) (Util.Coder Core.Term Model.Node))
recordCoder tname rt cx g =  
  let getCoder = (\f -> Eithers.bind (termCoder (Core.fieldTypeType f) cx g) (\coder -> Right (f, coder)))
  in (Eithers.bind (Eithers.mapList getCoder rt) (\coders -> Right (Util.Coder {
    Util.coderEncode = (\cx -> \term -> encodeRecord coders cx g term),
    Util.coderDecode = (\cx -> \val -> decodeRecord tname coders cx val)})))

-- | Encode a record term to YAML
encodeRecord :: ([(Core.FieldType, (Util.Coder Core.Term Model.Node))] -> Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) Model.Node)
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
                        in (Logic.ifElse (isMaybeNothing ft fvalue) (Right Nothing) (Eithers.bind (Util.coderEncode coder_ cx fvalue) (\encoded -> Right (Just (Model.NodeScalar (Model.ScalarStr (Core.unName fname)), encoded))))))
      in (Eithers.bind (Core_.termRecord cx graph stripped) (\record ->  
        let fields = (Core.recordFields record)
        in (Eithers.bind (Eithers.mapList encodeField (Lists.zip coders fields)) (\maybeFields -> Right (Model.NodeMapping (Maps.fromList (Maybes.cat maybeFields)))))))

-- | Decode a YAML value to a record term
decodeRecord :: (Core.Name -> [(Core.FieldType, (Util.Coder Core.Term Model.Node))] -> Context.Context -> Model.Node -> Either (Context.InContext Error.Error) Core.Term)
decodeRecord tname coders cx n =  
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
                          in (Eithers.bind (Util.coderDecode coder_ cx yamlValue) (\v -> Right (Core.Field {
                            Core.fieldName = fname,
                            Core.fieldTerm = v}))))
          in (Eithers.bind (Eithers.mapList decodeField coders) (\fields -> Right (Core.TermRecord (Core.Record {
            Core.recordTypeName = tname,
            Core.recordFields = fields})))))
  in ((\x -> case x of
    Model.NodeMapping v0 -> (decodeObjectBody v0)
    _ -> (Left (Context.InContext {
      Context.inContextObject = (Error.ErrorOther (Error.OtherError "expected mapping")),
      Context.inContextContext = cx}))) n)

-- | Create a YAML coder for term types
termCoder :: (Core.Type -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) (Util.Coder Core.Term Model.Node))
termCoder typ cx g =  
  let stripped = (Rewriting.deannotateType typ)
  in  
    let encodeLiteral = (\ac -> \cx -> \term -> (\x -> case x of
            Core.TermLiteral v0 -> (Eithers.bind (Util.coderEncode ac cx v0) (\scalar -> Right (Model.NodeScalar scalar)))
            _ -> (Left (Context.InContext {
              Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat [
                "expected literal term, found: ",
                (Core__.term term)]))),
              Context.inContextContext = cx}))) term)
    in  
      let encodeList = (\lc -> \cx -> \term -> (\x -> case x of
              Core.TermList v0 -> (Eithers.bind (Eithers.mapList (\el -> Util.coderEncode lc cx el) v0) (\encodedEls -> Right (Model.NodeSequence encodedEls)))
              _ -> (Left (Context.InContext {
                Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat [
                  "expected list term, found: ",
                  (Core__.term term)]))),
                Context.inContextContext = cx}))) term)
      in  
        let decodeList = (\lc -> \cx -> \n -> (\x -> case x of
                Model.NodeSequence v0 -> (Eithers.bind (Eithers.mapList (\node -> Util.coderDecode lc cx node) v0) (\decodedNodes -> Right (Core.TermList decodedNodes)))
                _ -> (Left (Context.InContext {
                  Context.inContextObject = (Error.ErrorOther (Error.OtherError "expected sequence")),
                  Context.inContextContext = cx}))) n)
        in  
          let encodeMaybe = (\maybeElementCoder -> \cx -> \maybeTerm ->  
                  let strippedMaybeTerm = (Rewriting.deannotateTerm maybeTerm)
                  in ((\x -> case x of
                    Core.TermMaybe v0 -> (Logic.ifElse (Maybes.isNothing v0) (Right (Model.NodeScalar Model.ScalarNull)) (Eithers.bind (Util.coderEncode maybeElementCoder cx (Maybes.fromJust v0)) (\encodedInner -> Right encodedInner)))
                    _ -> (Left (Context.InContext {
                      Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat [
                        "expected optional term, found: ",
                        (Core__.term maybeTerm)]))),
                      Context.inContextContext = cx}))) strippedMaybeTerm))
          in  
            let decodeMaybe = (\maybeElementCoder -> \cx -> \yamlVal -> (\x -> case x of
                    Model.NodeScalar v0 -> ((\x -> case x of
                      Model.ScalarNull -> (Right (Core.TermMaybe Nothing))
                      _ -> (Eithers.bind (Util.coderDecode maybeElementCoder cx yamlVal) (\decodedInner -> Right (Core.TermMaybe (Just decodedInner))))) v0)
                    _ -> (Eithers.bind (Util.coderDecode maybeElementCoder cx yamlVal) (\decodedInner -> Right (Core.TermMaybe (Just decodedInner))))) yamlVal)
            in  
              let result = ((\x -> case x of
                      Core.TypeLiteral v0 -> (Eithers.bind (literalYamlCoder v0) (\ac -> Right (Util.Coder {
                        Util.coderEncode = (encodeLiteral ac),
                        Util.coderDecode = (\cx -> \n -> (\x -> case x of
                          Model.NodeScalar v1 -> (Eithers.bind (Util.coderDecode ac cx v1) (\lit -> Right (Core.TermLiteral lit)))
                          _ -> (Left (Context.InContext {
                            Context.inContextObject = (Error.ErrorOther (Error.OtherError "expected scalar node")),
                            Context.inContextContext = cx}))) n)})))
                      Core.TypeList v0 -> (Eithers.bind (termCoder v0 cx g) (\lc -> Right (Util.Coder {
                        Util.coderEncode = (encodeList lc),
                        Util.coderDecode = (decodeList lc)})))
                      Core.TypeMap v0 ->  
                        let kt = (Core.mapTypeKeys v0)
                        in  
                          let vt = (Core.mapTypeValues v0)
                          in (Eithers.bind (termCoder kt cx g) (\kc -> Eithers.bind (termCoder vt cx g) (\vc ->  
                            let encodeEntry = (\cx -> \kv ->  
                                    let k = (Pairs.first kv)
                                    in  
                                      let v = (Pairs.second kv)
                                      in (Eithers.bind (Util.coderEncode kc cx k) (\encodedK -> Eithers.bind (Util.coderEncode vc cx v) (\encodedV -> Right (encodedK, encodedV)))))
                            in  
                              let decodeEntry = (\cx -> \kv ->  
                                      let k = (Pairs.first kv)
                                      in  
                                        let v = (Pairs.second kv)
                                        in (Eithers.bind (Util.coderDecode kc cx k) (\decodedK -> Eithers.bind (Util.coderDecode vc cx v) (\decodedV -> Right (decodedK, decodedV)))))
                              in (Right (Util.Coder {
                                Util.coderEncode = (\cx -> \term -> (\x -> case x of
                                  Core.TermMap v1 -> (Eithers.bind (Eithers.mapList (\entry -> encodeEntry cx entry) (Maps.toList v1)) (\entries -> Right (Model.NodeMapping (Maps.fromList entries))))
                                  _ -> (Left (Context.InContext {
                                    Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat [
                                      "expected map term, found: ",
                                      (Core__.term term)]))),
                                    Context.inContextContext = cx}))) term),
                                Util.coderDecode = (\cx -> \n -> (\x -> case x of
                                  Model.NodeMapping v1 -> (Eithers.bind (Eithers.mapList (\entry -> decodeEntry cx entry) (Maps.toList v1)) (\entries -> Right (Core.TermMap (Maps.fromList entries))))
                                  _ -> (Left (Context.InContext {
                                    Context.inContextObject = (Error.ErrorOther (Error.OtherError "expected mapping")),
                                    Context.inContextContext = cx}))) n)})))))
                      Core.TypeMaybe v0 -> (Eithers.bind (termCoder v0 cx g) (\maybeElementCoder -> Right (Util.Coder {
                        Util.coderEncode = (encodeMaybe maybeElementCoder),
                        Util.coderDecode = (decodeMaybe maybeElementCoder)})))
                      Core.TypeRecord v0 -> (recordCoder (Core.Name "yaml") v0 cx g)
                      Core.TypeUnit -> (Right unitCoder)
                      _ -> (Left (Context.InContext {
                        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat [
                          "unsupported type in YAML: ",
                          (Core__.type_ typ)]))),
                        Context.inContextContext = cx}))) stripped)
              in result

-- | YAML coder for unit values
unitCoder :: (Util.Coder Core.Term Model.Node)
unitCoder = Util.Coder {
  Util.coderEncode = encodeUnit,
  Util.coderDecode = decodeUnit} 
  where 
    encodeUnit = (\cx -> \term -> (\x -> case x of
      Core.TermUnit -> (Right (Model.NodeScalar Model.ScalarNull))
      _ -> (Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat [
          "expected unit, found: ",
          (Core__.term term)]))),
        Context.inContextContext = cx}))) (Rewriting.deannotateTerm term))
    decodeUnit = (\cx -> \n -> (\x -> case x of
      Model.NodeScalar v0 -> ((\x -> case x of
        Model.ScalarNull -> (Right Core.TermUnit)
        _ -> (Left (Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError "expected null scalar")),
          Context.inContextContext = cx}))) v0)
      _ -> (Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError "expected null")),
        Context.inContextContext = cx}))) n)
