-- Note: this is an automatically generated file. Do not edit.

-- | YAML encoding and decoding for Hydra terms

module Hydra.Ext.Org.Yaml.Coder where

import qualified Hydra.Adapt as Adapt
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
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
import qualified Hydra.Show.Core as Core__
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Decode a YAML value to a record term
decodeRecord :: Core.Name -> [(Core.FieldType, (Coders.Coder Core.Term Model.Node))] -> Context.Context -> Model.Node -> Either (Context.InContext Errors.Error) Core.Term
decodeRecord tname coders cx n =

      let decodeObjectBody =
              \m ->
                let decodeField =
                        \coder ->
                          let ft = Pairs.first coder
                              coder_ = Pairs.second coder
                              fname = Core.fieldTypeName ft
                              defaultValue = Model.NodeScalar Model.ScalarNull
                              yamlValue = Maybes.fromMaybe defaultValue (Maps.lookup (Model.NodeScalar (Model.ScalarStr (Core.unName fname))) m)
                          in (Eithers.bind (Coders.coderDecode coder_ cx yamlValue) (\v -> Right (Core.Field {
                            Core.fieldName = fname,
                            Core.fieldTerm = v})))
                in (Eithers.bind (Eithers.mapList decodeField coders) (\fields -> Right (Core.TermRecord (Core.Record {
                  Core.recordTypeName = tname,
                  Core.recordFields = fields}))))
      in case n of
        Model.NodeMapping v0 -> decodeObjectBody v0
        _ -> Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "expected mapping")),
          Context.inContextContext = cx})

-- | Encode a record term to YAML
encodeRecord :: [(Core.FieldType, (Coders.Coder Core.Term Model.Node))] -> Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Errors.Error) Model.Node
encodeRecord coders cx graph term =

      let stripped = Strip.deannotateTerm term
          isMaybeNothing =
                  \ft -> \fvalue -> case (Core.fieldTypeType ft) of
                    Core.TypeMaybe _ -> case fvalue of
                      Core.TermMaybe v1 -> Maybes.isNothing v1
                      _ -> False
                    _ -> False
          encodeField =
                  \coderAndField ->
                    let ftAndCoder = Pairs.first coderAndField
                        field = Pairs.second coderAndField
                        ft = Pairs.first ftAndCoder
                        coder_ = Pairs.second ftAndCoder
                        fname = Core.fieldName field
                        fvalue = Core.fieldTerm field
                    in (Logic.ifElse (isMaybeNothing ft fvalue) (Right Nothing) (Eithers.bind (Coders.coderEncode coder_ cx fvalue) (\encoded -> Right (Just (Model.NodeScalar (Model.ScalarStr (Core.unName fname)), encoded)))))
      in (Eithers.bind (Core_.termRecord cx graph stripped) (\record ->
        let fields = Core.recordFields record
        in (Eithers.bind (Eithers.mapList encodeField (Lists.zip coders fields)) (\maybeFields -> Right (Model.NodeMapping (Maps.fromList (Maybes.cat maybeFields)))))))

-- | Create a YAML coder for literal types
literalYamlCoder :: Core.LiteralType -> Either t0 (Coders.Coder Core.Literal Model.Scalar)
literalYamlCoder lt =

      let decodeBool =
              \cx -> \s -> case s of
                Model.ScalarBool v0 -> Right (Core.LiteralBoolean v0)
                _ -> Left (Context.InContext {
                  Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                    "expected boolean, found scalar"]))),
                  Context.inContextContext = cx})
          decodeFloat =
                  \cx -> \s -> case s of
                    Model.ScalarFloat v0 -> Right (Core.LiteralFloat (Core.FloatValueBigfloat v0))
                    _ -> Left (Context.InContext {
                      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                        "expected float, found scalar"]))),
                      Context.inContextContext = cx})
          decodeInteger =
                  \cx -> \s -> case s of
                    Model.ScalarInt v0 -> Right (Core.LiteralInteger (Core.IntegerValueBigint v0))
                    _ -> Left (Context.InContext {
                      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                        "expected integer, found scalar"]))),
                      Context.inContextContext = cx})
          decodeString =
                  \cx -> \s -> case s of
                    Model.ScalarStr v0 -> Right (Core.LiteralString v0)
                    _ -> Left (Context.InContext {
                      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                        "expected string, found scalar"]))),
                      Context.inContextContext = cx})
          encoded =
                  case lt of
                    Core.LiteralTypeBoolean -> Coders.Coder {
                      Coders.coderEncode = (\cx -> \lit -> Eithers.bind (Core_.booleanLiteral cx lit) (\b -> Right (Model.ScalarBool b))),
                      Coders.coderDecode = decodeBool}
                    Core.LiteralTypeFloat _ -> Coders.Coder {
                      Coders.coderEncode = (\cx -> \lit -> Eithers.bind (Core_.floatLiteral cx lit) (\f -> Eithers.bind (Core_.bigfloatValue cx f) (\bf -> Right (Model.ScalarFloat bf)))),
                      Coders.coderDecode = decodeFloat}
                    Core.LiteralTypeInteger _ -> Coders.Coder {
                      Coders.coderEncode = (\cx -> \lit -> Eithers.bind (Core_.integerLiteral cx lit) (\i -> Eithers.bind (Core_.bigintValue cx i) (\bi -> Right (Model.ScalarInt bi)))),
                      Coders.coderDecode = decodeInteger}
                    Core.LiteralTypeString -> Coders.Coder {
                      Coders.coderEncode = (\cx -> \lit -> Eithers.bind (Core_.stringLiteral cx lit) (\s -> Right (Model.ScalarStr s))),
                      Coders.coderDecode = decodeString}
      in (Right encoded)

-- | Create a YAML coder for record types
recordCoder :: Core.Name -> [Core.FieldType] -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) (Coders.Coder Core.Term Model.Node)
recordCoder tname rt cx g =

      let getCoder = \f -> Eithers.bind (termCoder (Core.fieldTypeType f) cx g) (\coder -> Right (f, coder))
      in (Eithers.bind (Eithers.mapList getCoder rt) (\coders -> Right (Coders.Coder {
        Coders.coderEncode = (\cx2 -> \term -> encodeRecord coders cx2 g term),
        Coders.coderDecode = (\cx2 -> \val -> decodeRecord tname coders cx2 val)})))

-- | Create a YAML coder for term types
termCoder :: Core.Type -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) (Coders.Coder Core.Term Model.Node)
termCoder typ cx g =

      let stripped = Strip.deannotateType typ
          encodeLiteral =
                  \ac -> \cx2 -> \term -> case term of
                    Core.TermLiteral v0 -> Eithers.bind (Coders.coderEncode ac cx2 v0) (\scalar -> Right (Model.NodeScalar scalar))
                    _ -> Left (Context.InContext {
                      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                        "expected literal term, found: ",
                        (Core__.term term)]))),
                      Context.inContextContext = cx2})
          encodeList =
                  \lc -> \cx2 -> \term -> case term of
                    Core.TermList v0 -> Eithers.bind (Eithers.mapList (\el -> Coders.coderEncode lc cx2 el) v0) (\encodedEls -> Right (Model.NodeSequence encodedEls))
                    _ -> Left (Context.InContext {
                      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                        "expected list term, found: ",
                        (Core__.term term)]))),
                      Context.inContextContext = cx2})
          decodeList =
                  \lc -> \cx2 -> \n -> case n of
                    Model.NodeSequence v0 -> Eithers.bind (Eithers.mapList (\node -> Coders.coderDecode lc cx2 node) v0) (\decodedNodes -> Right (Core.TermList decodedNodes))
                    _ -> Left (Context.InContext {
                      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "expected sequence")),
                      Context.inContextContext = cx2})
          encodeMaybe =
                  \maybeElementCoder -> \cx2 -> \maybeTerm ->
                    let strippedMaybeTerm = Strip.deannotateTerm maybeTerm
                    in case strippedMaybeTerm of
                      Core.TermMaybe v0 -> Logic.ifElse (Maybes.isNothing v0) (Right (Model.NodeScalar Model.ScalarNull)) (Eithers.bind (Coders.coderEncode maybeElementCoder cx2 (Maybes.fromJust v0)) (\encodedInner -> Right encodedInner))
                      _ -> Left (Context.InContext {
                        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                          "expected optional term, found: ",
                          (Core__.term maybeTerm)]))),
                        Context.inContextContext = cx2})
          decodeMaybe =
                  \maybeElementCoder -> \cx2 -> \yamlVal -> case yamlVal of
                    Model.NodeScalar v0 -> case v0 of
                      Model.ScalarNull -> Right (Core.TermMaybe Nothing)
                      _ -> Eithers.bind (Coders.coderDecode maybeElementCoder cx2 yamlVal) (\decodedInner -> Right (Core.TermMaybe (Just decodedInner)))
                    _ -> Eithers.bind (Coders.coderDecode maybeElementCoder cx2 yamlVal) (\decodedInner -> Right (Core.TermMaybe (Just decodedInner)))
          result =
                  case stripped of
                    Core.TypeLiteral v0 -> Eithers.bind (literalYamlCoder v0) (\ac -> Right (Coders.Coder {
                      Coders.coderEncode = (encodeLiteral ac),
                      Coders.coderDecode = (\cx2 -> \n -> case n of
                        Model.NodeScalar v1 -> Eithers.bind (Coders.coderDecode ac cx2 v1) (\lit -> Right (Core.TermLiteral lit))
                        _ -> Left (Context.InContext {
                          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "expected scalar node")),
                          Context.inContextContext = cx2}))}))
                    Core.TypeList v0 -> Eithers.bind (termCoder v0 cx g) (\lc -> Right (Coders.Coder {
                      Coders.coderEncode = (encodeList lc),
                      Coders.coderDecode = (decodeList lc)}))
                    Core.TypeMap v0 ->
                      let kt = Core.mapTypeKeys v0
                          vt = Core.mapTypeValues v0
                      in (Eithers.bind (termCoder kt cx g) (\kc -> Eithers.bind (termCoder vt cx g) (\vc ->
                        let encodeEntry =
                                \cx2 -> \kv ->
                                  let k = Pairs.first kv
                                      v = Pairs.second kv
                                  in (Eithers.bind (Coders.coderEncode kc cx2 k) (\encodedK -> Eithers.bind (Coders.coderEncode vc cx2 v) (\encodedV -> Right (encodedK, encodedV))))
                            decodeEntry =
                                    \cx2 -> \kv ->
                                      let k = Pairs.first kv
                                          v = Pairs.second kv
                                      in (Eithers.bind (Coders.coderDecode kc cx2 k) (\decodedK -> Eithers.bind (Coders.coderDecode vc cx2 v) (\decodedV -> Right (decodedK, decodedV))))
                        in (Right (Coders.Coder {
                          Coders.coderEncode = (\cx2 -> \term -> case term of
                            Core.TermMap v1 -> Eithers.bind (Eithers.mapList (\entry -> encodeEntry cx2 entry) (Maps.toList v1)) (\entries -> Right (Model.NodeMapping (Maps.fromList entries)))
                            _ -> Left (Context.InContext {
                              Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                                "expected map term, found: ",
                                (Core__.term term)]))),
                              Context.inContextContext = cx2})),
                          Coders.coderDecode = (\cx2 -> \n -> case n of
                            Model.NodeMapping v1 -> Eithers.bind (Eithers.mapList (\entry -> decodeEntry cx2 entry) (Maps.toList v1)) (\entries -> Right (Core.TermMap (Maps.fromList entries)))
                            _ -> Left (Context.InContext {
                              Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "expected mapping")),
                              Context.inContextContext = cx2}))})))))
                    Core.TypeMaybe v0 -> Eithers.bind (termCoder v0 cx g) (\maybeElementCoder -> Right (Coders.Coder {
                      Coders.coderEncode = (encodeMaybe maybeElementCoder),
                      Coders.coderDecode = (decodeMaybe maybeElementCoder)}))
                    Core.TypeRecord v0 -> recordCoder (Core.Name "yaml") v0 cx g
                    Core.TypeUnit -> Right unitCoder
                    _ -> Left (Context.InContext {
                      Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                        "unsupported type in YAML: ",
                        (Core__.type_ typ)]))),
                      Context.inContextContext = cx})
      in result

-- | YAML coder for unit values
unitCoder :: Coders.Coder Core.Term Model.Node
unitCoder =
    Coders.Coder {
      Coders.coderEncode = encodeUnit,
      Coders.coderDecode = decodeUnit}
  where
    encodeUnit =
        \cx -> \term -> case (Strip.deannotateTerm term) of
          Core.TermUnit -> Right (Model.NodeScalar Model.ScalarNull)
          _ -> Left (Context.InContext {
            Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
              "expected unit, found: ",
              (Core__.term term)]))),
            Context.inContextContext = cx})
    decodeUnit =
        \cx -> \n -> case n of
          Model.NodeScalar v0 -> case v0 of
            Model.ScalarNull -> Right Core.TermUnit
            _ -> Left (Context.InContext {
              Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "expected null scalar")),
              Context.inContextContext = cx})
          _ -> Left (Context.InContext {
            Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "expected null")),
            Context.inContextContext = cx})

-- | Create a YAML coder for a given type
yamlCoder :: Core.Type -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) (Coders.Coder Core.Term Model.Node)
yamlCoder typ cx g =

      let mkTermCoder = \t -> termCoder t cx g
      in (Eithers.bind (Eithers.bimap (\_s -> Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError _s)),
        Context.inContextContext = cx}) (\_x -> _x) (Adapt.simpleLanguageAdapter Language.yamlLanguage cx g typ)) (\adapter -> Eithers.bind (mkTermCoder (Coders.adapterTarget adapter)) (\coder -> Right (Adapt.composeCoders (Coders.adapterCoder adapter) coder))))
