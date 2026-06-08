-- Note: this is an automatically generated file. Do not edit.
-- | YAML encoding and decoding for Hydra terms

module Hydra.Yaml.Coder where
import qualified Hydra.Adapt as Adapt
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as JsonModel
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Literals as LibLiterals
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Literals as Literals
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Strip as Strip
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import qualified Hydra.Yaml.Language as Language
import qualified Hydra.Yaml.Model as YamlModel
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Decode a YAML value to a record term
decodeRecord :: Core.Name -> [(Core.FieldType, (Coders.Coder Core.Term YamlModel.Node))] -> Typing.InferenceContext -> YamlModel.Node -> Either Errors.Error Core.Term
decodeRecord tname coders cx n =

      let decodeObjectBody =
              \m ->
                let decodeField =
                        \coder ->
                          let ft = Pairs.first coder
                              coder_ = Pairs.second coder
                              fname = Core.fieldTypeName ft
                              defaultValue = YamlModel.NodeScalar YamlModel.ScalarNull
                              yamlValue = Optionals.fromOptional defaultValue (Maps.lookup (YamlModel.NodeScalar (YamlModel.ScalarStr (Core.unName fname))) m)
                          in (Eithers.bind (Coders.coderDecode coder_ cx yamlValue) (\v -> Right (Core.Field {
                            Core.fieldName = fname,
                            Core.fieldTerm = v})))
                in (Eithers.bind (Eithers.mapList decodeField coders) (\fields -> Right (Core.TermRecord (Core.Record {
                  Core.recordTypeName = tname,
                  Core.recordFields = fields}))))
      in case n of
        YamlModel.NodeMapping v0 -> decodeObjectBody v0
        _ -> Left (Errors.ErrorOther (Errors.OtherError "expected mapping"))
-- | Encode a record term to YAML
encodeRecord :: [(Core.FieldType, (Coders.Coder Core.Term YamlModel.Node))] -> Typing.InferenceContext -> Graph.Graph -> Core.Term -> Either Errors.Error YamlModel.Node
encodeRecord coders cx graph term =

      let stripped = Strip.deannotateTerm term
          isMaybeNothing =
                  \ft -> \fvalue -> case (Core.fieldTypeType ft) of
                    Core.TypeOptional _ -> case fvalue of
                      Core.TermOptional v1 -> Optionals.isNone v1
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
                    in (Logic.ifElse (isMaybeNothing ft fvalue) (Right Nothing) (Eithers.bind (Coders.coderEncode coder_ cx fvalue) (\encoded -> Right (Just (YamlModel.NodeScalar (YamlModel.ScalarStr (Core.unName fname)), encoded)))))
      in (Eithers.bind (ExtractCore.termRecord graph stripped) (\record ->
        let fields = Core.recordFields record
        in (Eithers.bind (Eithers.mapList encodeField (Lists.zip coders fields)) (\maybeFields -> Right (YamlModel.NodeMapping (Maps.fromList (Optionals.cat maybeFields)))))))
-- | Create a YAML coder for literal types
literalYamlCoder :: Core.LiteralType -> Either t0 (Coders.Coder Core.Literal YamlModel.Scalar)
literalYamlCoder lt =

      let decodeBool =
              \cx -> \s -> case s of
                YamlModel.ScalarBool v0 -> Right (Core.LiteralBoolean v0)
                _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                  "expected boolean, found scalar"])))
          decodeDecimal =
                  \cx -> \s -> case s of
                    YamlModel.ScalarDecimal v0 -> Right (Core.LiteralDecimal v0)
                    YamlModel.ScalarFloat v0 -> Right (Core.LiteralDecimal (LibLiterals.float64ToDecimal v0))
                    YamlModel.ScalarInt v0 -> Right (Core.LiteralDecimal (LibLiterals.bigintToDecimal v0))
                    _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                      "expected decimal, found scalar"])))
          decodeFloat =
                  \cx -> \s -> case s of
                    YamlModel.ScalarDecimal v0 -> Right (Core.LiteralFloat (Core.FloatValueFloat64 (LibLiterals.decimalToFloat64 v0)))
                    YamlModel.ScalarFloat v0 -> Right (Core.LiteralFloat (Core.FloatValueFloat64 v0))
                    _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                      "expected float, found scalar"])))
          decodeInteger =
                  \cx -> \s -> case s of
                    YamlModel.ScalarInt v0 -> Right (Core.LiteralInteger (Core.IntegerValueBigint v0))
                    _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                      "expected integer, found scalar"])))
          decodeString =
                  \cx -> \s -> case s of
                    YamlModel.ScalarStr v0 -> Right (Core.LiteralString v0)
                    _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                      "expected string, found scalar"])))
          encoded =
                  case lt of
                    Core.LiteralTypeBoolean -> Coders.Coder {
                      Coders.coderEncode = (\cx -> \lit -> Eithers.bind (ExtractCore.booleanLiteral lit) (\b -> Right (YamlModel.ScalarBool b))),
                      Coders.coderDecode = decodeBool}
                    Core.LiteralTypeDecimal -> Coders.Coder {
                      Coders.coderEncode = (\cx -> \lit -> Eithers.bind (ExtractCore.decimalLiteral lit) (\d -> Right (YamlModel.ScalarDecimal d))),
                      Coders.coderDecode = decodeDecimal}
                    Core.LiteralTypeFloat _ -> Coders.Coder {
                      Coders.coderEncode = (\cx -> \lit -> Eithers.bind (ExtractCore.floatLiteral lit) (\f ->
                        let bf =
                                case f of
                                  Core.FloatValueFloat32 v1 -> LibLiterals.float32ToFloat64 v1
                                  Core.FloatValueFloat64 v1 -> v1
                            shown = LibLiterals.showFloat64 bf
                        in (Logic.ifElse (requiresYamlStringSentinel shown) (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                          "YAML cannot represent float value: ",
                          shown])))) (Right (YamlModel.ScalarFloat bf))))),
                      Coders.coderDecode = decodeFloat}
                    Core.LiteralTypeInteger _ -> Coders.Coder {
                      Coders.coderEncode = (\cx -> \lit -> Eithers.bind (ExtractCore.integerLiteral lit) (\i -> Eithers.bind (ExtractCore.bigintValue i) (\bi -> Right (YamlModel.ScalarInt bi)))),
                      Coders.coderDecode = decodeInteger}
                    Core.LiteralTypeString -> Coders.Coder {
                      Coders.coderEncode = (\cx -> \lit -> Eithers.bind (ExtractCore.stringLiteral lit) (\s -> Right (YamlModel.ScalarStr s))),
                      Coders.coderDecode = decodeString}
      in (Right encoded)
-- | Create a YAML coder for record types
recordCoder :: Core.Name -> [Core.FieldType] -> t0 -> Graph.Graph -> Either Errors.Error (Coders.Coder Core.Term YamlModel.Node)
recordCoder tname rt cx g =

      let getCoder = \f -> Eithers.bind (termCoder (Core.fieldTypeType f) cx g) (\coder -> Right (f, coder))
      in (Eithers.bind (Eithers.mapList getCoder rt) (\coders -> Right (Coders.Coder {
        Coders.coderEncode = (\cx2 -> \term -> encodeRecord coders cx2 g term),
        Coders.coderDecode = (\cx2 -> \val -> decodeRecord tname coders cx2 val)})))
-- | True for IEEE sentinel strings that Hydra YAML cannot represent as a float scalar.
requiresYamlStringSentinel :: String -> Bool
requiresYamlStringSentinel s =
    Logic.or (Equality.equal s "NaN") (Logic.or (Equality.equal s "Infinity") (Logic.or (Equality.equal s "-Infinity") (Equality.equal s "-0.0")))
-- | Create a YAML coder for term types
termCoder :: Core.Type -> t0 -> Graph.Graph -> Either Errors.Error (Coders.Coder Core.Term YamlModel.Node)
termCoder typ cx g =

      let stripped = Strip.deannotateType typ
          encodeLiteral =
                  \ac -> \cx2 -> \term -> case term of
                    Core.TermLiteral v0 -> Eithers.bind (Coders.coderEncode ac cx2 v0) (\scalar -> Right (YamlModel.NodeScalar scalar))
                    _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                      "expected literal term, found: ",
                      (ShowCore.term term)])))
          encodeList =
                  \lc -> \cx2 -> \term -> case term of
                    Core.TermList v0 -> Eithers.bind (Eithers.mapList (\el -> Coders.coderEncode lc cx2 el) v0) (\encodedEls -> Right (YamlModel.NodeSequence encodedEls))
                    _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                      "expected list term, found: ",
                      (ShowCore.term term)])))
          decodeList =
                  \lc -> \cx2 -> \n -> case n of
                    YamlModel.NodeSequence v0 -> Eithers.bind (Eithers.mapList (\node -> Coders.coderDecode lc cx2 node) v0) (\decodedNodes -> Right (Core.TermList decodedNodes))
                    _ -> Left (Errors.ErrorOther (Errors.OtherError "expected sequence"))
          encodeMaybe =
                  \maybeElementCoder -> \cx2 -> \optionalTerm ->
                    let strippedMaybeTerm = Strip.deannotateTerm optionalTerm
                    in case strippedMaybeTerm of
                      Core.TermOptional v0 -> Optionals.cases v0 (Right (YamlModel.NodeScalar YamlModel.ScalarNull)) (\innerTerm -> Eithers.bind (Coders.coderEncode maybeElementCoder cx2 innerTerm) (\encodedInner -> Right encodedInner))
                      _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                        "expected optional term, found: ",
                        (ShowCore.term optionalTerm)])))
          decodeMaybe =
                  \maybeElementCoder -> \cx2 -> \yamlVal -> case yamlVal of
                    YamlModel.NodeScalar v0 -> case v0 of
                      YamlModel.ScalarNull -> Right (Core.TermOptional Nothing)
                      _ -> Eithers.bind (Coders.coderDecode maybeElementCoder cx2 yamlVal) (\decodedInner -> Right (Core.TermOptional (Just decodedInner)))
                    _ -> Eithers.bind (Coders.coderDecode maybeElementCoder cx2 yamlVal) (\decodedInner -> Right (Core.TermOptional (Just decodedInner)))
          result =
                  case stripped of
                    Core.TypeLiteral v0 -> Eithers.bind (literalYamlCoder v0) (\ac -> Right (Coders.Coder {
                      Coders.coderEncode = (encodeLiteral ac),
                      Coders.coderDecode = (\cx2 -> \n -> case n of
                        YamlModel.NodeScalar v1 -> Eithers.bind (Coders.coderDecode ac cx2 v1) (\lit -> Right (Core.TermLiteral lit))
                        _ -> Left (Errors.ErrorOther (Errors.OtherError "expected scalar node")))}))
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
                            Core.TermMap v1 -> Eithers.bind (Eithers.mapList (\entry -> encodeEntry cx2 entry) (Maps.toList v1)) (\entries -> Right (YamlModel.NodeMapping (Maps.fromList entries)))
                            _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                              "expected map term, found: ",
                              (ShowCore.term term)])))),
                          Coders.coderDecode = (\cx2 -> \n -> case n of
                            YamlModel.NodeMapping v1 -> Eithers.bind (Eithers.mapList (\entry -> decodeEntry cx2 entry) (Maps.toList v1)) (\entries -> Right (Core.TermMap (Maps.fromList entries)))
                            _ -> Left (Errors.ErrorOther (Errors.OtherError "expected mapping")))})))))
                    Core.TypeOptional v0 -> Eithers.bind (termCoder v0 cx g) (\maybeElementCoder -> Right (Coders.Coder {
                      Coders.coderEncode = (encodeMaybe maybeElementCoder),
                      Coders.coderDecode = (decodeMaybe maybeElementCoder)}))
                    Core.TypeRecord v0 -> recordCoder (Core.Name "yaml") v0 cx g
                    Core.TypeUnit -> Right unitCoder
                    _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                      "unsupported type in YAML: ",
                      (ShowCore.type_ typ)])))
      in result
-- | YAML coder for unit values
unitCoder :: Coders.Coder Core.Term YamlModel.Node
unitCoder =
    Coders.Coder {
      Coders.coderEncode = encodeUnit,
      Coders.coderDecode = decodeUnit}
  where
    encodeUnit =
        \cx -> \term -> case (Strip.deannotateTerm term) of
          Core.TermUnit -> Right (YamlModel.NodeScalar YamlModel.ScalarNull)
          _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat [
            "expected unit, found: ",
            (ShowCore.term term)])))
    decodeUnit =
        \cx -> \n -> case n of
          YamlModel.NodeScalar v0 -> case v0 of
            YamlModel.ScalarNull -> Right Core.TermUnit
            _ -> Left (Errors.ErrorOther (Errors.OtherError "expected null scalar"))
          _ -> Left (Errors.ErrorOther (Errors.OtherError "expected null"))
-- | Create a YAML coder for a given type
yamlCoder :: Core.Type -> t0 -> Graph.Graph -> Either Errors.Error (Coders.Coder Core.Term YamlModel.Node)
yamlCoder typ cx g =

      let mkTermCoder = \t -> termCoder t cx g
      in (Eithers.bind (Adapt.simpleLanguageAdapter Language.yamlLanguage cx g typ) (\adapter -> Eithers.bind (mkTermCoder (Coders.adapterTarget adapter)) (\coder -> Right (Adapt.composeCoders (Coders.adapterCoder adapter) coder))))
