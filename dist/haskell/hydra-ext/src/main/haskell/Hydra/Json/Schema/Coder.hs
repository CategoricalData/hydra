-- Note: this is an automatically generated file. Do not edit.
-- | JSON Schema code generator: converts Hydra modules to JSON Schema documents

module Hydra.Json.Schema.Coder where
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Core as Core
import qualified Hydra.Dependencies as Dependencies
import qualified Hydra.Environment as Environment
import qualified Hydra.Errors as Errors
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Json.Schema as Schema
import qualified Hydra.Json.Schema.Serde as Serde
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Predicates as Predicates
import qualified Hydra.Reflect as Reflect
import qualified Hydra.Show.Variants as Variants
import qualified Hydra.Strip as Strip
import qualified Hydra.Util as Util
import qualified Hydra.Variables as Variables
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | Construct JSON Schema documents from type definitions
constructModule :: t0 -> Graph.Graph -> t1 -> [Packaging.TypeDefinition] -> Either Errors.Error (M.Map String Schema.Document)
constructModule cx g mod typeDefs =

      let typeBody = \td -> Core.typeSchemeBody (Packaging.typeDefinitionTypeScheme td)
          typeMap = Maps.fromList (Lists.map (\td -> (Packaging.typeDefinitionName td, (typeBody td))) typeDefs)
      in (Eithers.map (\ps -> Maps.fromList ps) (Eithers.mapList (\td -> typeDefToDocument cx g typeMap (Packaging.typeDefinitionName td) (typeBody td)) typeDefs))
-- | Build a single-property record Schema for one branch of an Either oneOf
eitherBranch :: String -> [Schema.Restriction] -> Schema.Schema
eitherBranch label res =
    Schema.Schema [
      Schema.RestrictionType (Schema.TypeSingle Schema.TypeNameObject),
      (Schema.RestrictionObject (Schema.ObjectRestrictionProperties (Maps.singleton (Schema.Keyword label) (Schema.Schema res)))),
      (Schema.RestrictionObject (Schema.ObjectRestrictionRequired [
        Schema.Keyword label])),
      (Schema.RestrictionObject (Schema.ObjectRestrictionAdditionalProperties (Schema.AdditionalItemsAny False)))]
-- | Encode a field type as a JSON Schema keyword-schema pair
encodeField :: t0 -> Graph.Graph -> Core.FieldType -> Either Errors.Error (Schema.Keyword, Schema.Schema)
encodeField cx g ft =

      let name = Core.fieldTypeName ft
          typ = Core.fieldTypeType ft
      in (Eithers.map (\res -> (Schema.Keyword (Core.unName name), (Schema.Schema res))) (typeToExpr cx g False typ))
-- | Encode a Hydra name as a safe identifier string, replacing non-alphanumeric characters with underscores
encodeName :: Core.Name -> String
encodeName name = Formatting.nonAlnumToUnderscores (Core.unName name)
-- | Encode a named type as a list of JSON Schema restrictions with a title
encodeNamedType :: t0 -> Graph.Graph -> Core.Name -> Core.Type -> Either Errors.Error [Schema.Restriction]
encodeNamedType cx g name typ =
    Eithers.map (\res -> Lists.concat [
      [
        Schema.RestrictionTitle (Core.unName name)],
      res]) (typeToExpr cx g False (Strip.deannotateType typ))
-- | Encode a record or union as a list of JSON Schema object restrictions; isUnion adds min/maxProperties=1
encodeRecordOrUnion :: t0 -> Graph.Graph -> Bool -> Bool -> [Core.FieldType] -> Either Errors.Error [Schema.Restriction]
encodeRecordOrUnion cx g optional isUnion fields =
    Eithers.bind (Eithers.mapList (\ft -> encodeField cx g ft) fields) (\props ->
      let objRes = [
            Schema.RestrictionObject (Schema.ObjectRestrictionProperties (Maps.fromList props))]
          reqs =
                  Lists.foldl (\acc -> \f -> Logic.ifElse (isRequiredField f) (Lists.concat2 acc [
                    Schema.Keyword (Core.unName (Core.fieldTypeName f))]) acc) [] fields
          reqRes = Logic.ifElse (Lists.null reqs) [] [
                Schema.RestrictionObject (Schema.ObjectRestrictionRequired reqs)]
          addPropsRes = [
                Schema.RestrictionObject (Schema.ObjectRestrictionAdditionalProperties (Schema.AdditionalItemsAny False))]
          cardRes =
                  Logic.ifElse isUnion [
                    Schema.RestrictionObject (Schema.ObjectRestrictionMinProperties 1),
                    (Schema.RestrictionObject (Schema.ObjectRestrictionMaxProperties 1))] []
      in (Right (Lists.concat [
        jsType optional Schema.TypeNameObject,
        objRes,
        reqRes,
        addPropsRes,
        cardRes])))
-- | Encode a union type, splitting unit-typed (simple) variants into a string-enum branch
encodeUnion :: t0 -> Graph.Graph -> Bool -> [Core.FieldType] -> Either Errors.Error [Schema.Restriction]
encodeUnion cx g optional fields =

      let isSimple = \f -> Predicates.isUnitType (Strip.deannotateType (Core.fieldTypeType f))
          simple = Lists.filter isSimple fields
          nonsimple = Lists.filter (\f -> Logic.not (isSimple f)) fields
      in (Logic.ifElse (Lists.null simple) (encodeRecordOrUnion cx g optional True fields) (Eithers.bind (encodeRecordOrUnion cx g False True nonsimple) (\recRes ->
        let names = Lists.map (\f -> Core.unName (Core.fieldTypeName f)) simple
            simpleSchema =
                    Schema.Schema [
                      Schema.RestrictionType (Schema.TypeSingle Schema.TypeNameString),
                      (Schema.RestrictionMultiple (Schema.MultipleRestrictionEnum (Lists.map (\n -> Model.ValueString n) names)))]
        in (Right [
          Schema.RestrictionMultiple (Schema.MultipleRestrictionOneOf [
            Schema.Schema recRes,
            simpleSchema])]))))
-- | Determine whether a field is required (i.e., not optional/Maybe)
isRequiredField :: Core.FieldType -> Bool
isRequiredField ft =

      let typ = Core.fieldTypeType ft
      in case (Strip.deannotateType typ) of
        Core.TypeMaybe _ -> False
        _ -> True
-- | Build the JSON Schema type-restriction list for a type name, optionally widening to allow null
jsType :: Bool -> Schema.TypeName -> [Schema.Restriction]
jsType optional tname =
    [
      Schema.RestrictionType (Logic.ifElse optional (Schema.TypeMultiple [
        tname,
        Schema.TypeNameNull]) (Schema.TypeSingle tname))]
-- | Map a Hydra literal type to a JSON Schema type name
literalTypeName :: Core.LiteralType -> Schema.TypeName
literalTypeName lt =
    case lt of
      Core.LiteralTypeBinary -> Schema.TypeNameString
      Core.LiteralTypeBoolean -> Schema.TypeNameBoolean
      Core.LiteralTypeFloat _ -> Schema.TypeNameNumber
      Core.LiteralTypeInteger _ -> Schema.TypeNameInteger
      Core.LiteralTypeString -> Schema.TypeNameString
      _ -> Schema.TypeNameString
-- | Convert a Hydra module to a map from file path to JSON Schema document string
moduleToJsonSchema :: t0 -> [Packaging.Definition] -> t1 -> Graph.Graph -> Either Errors.Error (M.Map String String)
moduleToJsonSchema mod defs cx g =

      let partitioned = Environment.partitionDefinitions defs
          typeDefs = Pairs.first partitioned
      in (Eithers.map (\docs -> Maps.map (\doc -> Serde.jsonSchemaDocumentToString doc) docs) (constructModule cx g mod typeDefs))
-- | Compute the JSON Schema output file path for a named type
nameToPath :: Core.Name -> String
nameToPath name =

      let qn = Names.qualifyName name
          mns = Packaging.qualifiedNameModuleName qn
          local = Packaging.qualifiedNameLocal qn
          nsPart = Maybes.maybe "" (\ns -> Strings.cat2 (Packaging.unModuleName ns) ".") mns
      in (Names.namespaceToFilePath Util.CaseConventionCamel (Packaging.FileExtension "json") (Packaging.ModuleName (Strings.cat2 nsPart local)))
-- | Build the JSON Schema restriction list for a pair type
pairRestrictions :: Bool -> [Schema.Restriction] -> [Schema.Restriction] -> [Schema.Restriction]
pairRestrictions optional firstRes secondRes =
    Lists.concat [
      jsType optional Schema.TypeNameObject,
      [
        Schema.RestrictionObject (Schema.ObjectRestrictionProperties (Maps.fromList [
          (Schema.Keyword "first", (Schema.Schema firstRes)),
          (Schema.Keyword "second", (Schema.Schema secondRes))]))],
      [
        Schema.RestrictionObject (Schema.ObjectRestrictionRequired [
          Schema.Keyword "first",
          (Schema.Keyword "second")])],
      [
        Schema.RestrictionObject (Schema.ObjectRestrictionAdditionalProperties (Schema.AdditionalItemsAny False))]]
-- | Create a JSON Schema reference restriction for a named type
referenceRestriction :: Core.Name -> Schema.Restriction
referenceRestriction name =
    Schema.RestrictionReference (Schema.SchemaReference (Strings.cat [
      "#/$defs/",
      (encodeName name)]))
-- | Walk the transitive named-type dependency closure of a root type through typeMap; the visited set guards against cycles in self-/mutually-recursive types
transitiveTypeDeps :: M.Map Core.Name Core.Type -> S.Set Core.Name -> Core.Type -> S.Set Core.Name
transitiveTypeDeps typeMap visited rootType =

      let directDeps = Dependencies.typeDependencyNames True rootType
          step =
                  \acc -> \n -> Logic.ifElse (Sets.member n acc) acc (
                    let acc1 = Sets.insert n acc
                    in (Maybes.maybe acc1 (\t -> transitiveTypeDeps typeMap acc1 t) (Maps.lookup n typeMap)))
      in (Lists.foldl step visited (Sets.toList directDeps))
-- | Build a JSON Schema document for a single named type, with $defs covering its transitive dependencies and short-name substitution applied
typeDefToDocument :: t0 -> Graph.Graph -> M.Map Core.Name Core.Type -> Core.Name -> Core.Type -> Either Errors.Error (String, Schema.Document)
typeDefToDocument cx g typeMap rootName rootType =

      let depNames = Sets.toList (transitiveTypeDeps typeMap Sets.empty rootType)
          allNames = Lists.concat2 [
                rootName] (Lists.filter (\n -> Logic.not (Equality.equal n rootName)) depNames)
          allTypes = Lists.map (\n -> Maybes.fromMaybe (Core.TypeVariable n) (Maps.lookup n typeMap)) allNames
          nameSubst = Dependencies.toShortNames allNames
          types = Lists.map (\t -> Variables.substituteTypeVariables nameSubst t) allTypes
          names = Lists.map (\n -> Maybes.fromMaybe n (Maps.lookup n nameSubst)) allNames
          subRoot = Maybes.fromMaybe rootName (Maps.lookup rootName nameSubst)
          pairs = Lists.zip names types
      in (Eithers.bind (Eithers.mapList (\p -> typeToKeywordSchemaPair cx g (Pairs.first p) (Pairs.second p)) pairs) (\schemas -> Right (
        nameToPath rootName,
        Schema.Document {
          Schema.documentId = Nothing,
          Schema.documentDefinitions = (Just (Maps.fromList schemas)),
          Schema.documentRoot = (Schema.Schema [
            referenceRestriction subRoot])})))
-- | Encode a Hydra type as a list of JSON Schema restrictions
typeToExpr :: t0 -> Graph.Graph -> Bool -> Core.Type -> Either Errors.Error [Schema.Restriction]
typeToExpr cx g optional typ =
    case typ of
      Core.TypeAnnotated _ -> Eithers.bind (typeToExpr cx g optional (Strip.deannotateType typ)) (\res -> Eithers.bind (Annotations.getTypeDescription cx g typ) (\mdesc -> Right (Lists.concat2 (Maybes.maybe [] (\d -> [
        Schema.RestrictionDescription d]) mdesc) res)))
      Core.TypeApplication v0 -> typeToExpr cx g optional (Core.applicationTypeFunction v0)
      Core.TypeEither v0 ->
        let lt = Core.eitherTypeLeft v0
            rt = Core.eitherTypeRight v0
        in (Eithers.bind (typeToExpr cx g False lt) (\leftRes -> Eithers.bind (typeToExpr cx g False rt) (\rightRes -> Right [
          Schema.RestrictionMultiple (Schema.MultipleRestrictionOneOf [
            eitherBranch "left" leftRes,
            (eitherBranch "right" rightRes)])])))
      Core.TypeForall v0 -> typeToExpr cx g optional (Core.forallTypeBody v0)
      Core.TypeList v0 -> Eithers.bind (typeToExpr cx g False v0) (\els -> Right (Lists.concat2 (jsType optional Schema.TypeNameArray) [
        Schema.RestrictionArray (Schema.ArrayRestrictionItems (Schema.ItemsSameItems (Schema.Schema els)))]))
      Core.TypeLiteral v0 -> Right (jsType optional (literalTypeName v0))
      Core.TypeMap v0 ->
        let vt = Core.mapTypeValues v0
        in (Eithers.bind (typeToExpr cx g False vt) (\vRes -> Right (Lists.concat2 (jsType optional Schema.TypeNameObject) [
          Schema.RestrictionObject (Schema.ObjectRestrictionAdditionalProperties (Schema.AdditionalItemsSchema (Schema.Schema vRes)))])))
      Core.TypeMaybe v0 -> typeToExpr cx g True v0
      Core.TypePair v0 ->
        let ft = Core.pairTypeFirst v0
            st = Core.pairTypeSecond v0
        in (Eithers.bind (typeToExpr cx g False ft) (\firstRes -> Eithers.bind (typeToExpr cx g False st) (\secondRes -> Right (pairRestrictions optional firstRes secondRes))))
      Core.TypeRecord v0 -> encodeRecordOrUnion cx g optional False v0
      Core.TypeSet v0 -> Eithers.bind (typeToExpr cx g False v0) (\els -> Right (Lists.concat2 (jsType optional Schema.TypeNameArray) [
        Schema.RestrictionArray (Schema.ArrayRestrictionItems (Schema.ItemsSameItems (Schema.Schema els)))]))
      Core.TypeUnion v0 -> encodeUnion cx g optional v0
      Core.TypeVariable v0 -> Right [
        referenceRestriction v0]
      Core.TypeWrap v0 -> typeToExpr cx g optional v0
      _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "JSON Schema: unsupported type variant: " (Variants.typeVariant (Reflect.typeVariant typ)))))
-- | Build a (Keyword, Schema) pair for a named type, used as a $defs entry
typeToKeywordSchemaPair :: t0 -> Graph.Graph -> Core.Name -> Core.Type -> Either Errors.Error (Schema.Keyword, Schema.Schema)
typeToKeywordSchemaPair cx g name typ =
    Eithers.map (\res -> (Schema.Keyword (encodeName name), (Schema.Schema res))) (encodeNamedType cx g name typ)
