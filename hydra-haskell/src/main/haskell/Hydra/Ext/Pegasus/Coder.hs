module Hydra.Ext.Pegasus.Coder (
  dataGraphToPegasusSchema,
  pegasusDataLanguage,
) where

import Hydra.Adapter
import Hydra.Adapters.Term
import Hydra.Basics
import Hydra.Core
import Hydra.CoreDecoding
import Hydra.CoreLanguage
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Impl.Haskell.Extras
import Hydra.Primitives
import Hydra.Rewriting
import Hydra.Util.Coders
import Hydra.Util.Formatting
import qualified Hydra.Ext.Pegasus.Pdl as PDL
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Impl.Haskell.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


constructModule :: (Default m, Ord m, Read m, Show m)
  => Context m -> Graph m -> M.Map (Type m) (Step (Data m) ()) -> [(Element m, TypedData m)] -> Result PDL.SchemaFile
constructModule cx g coders pairs = do
    let ns = pdlNameForGraph g
    let pkg = Nothing
    let imports = [] -- TODO
    sortedPairs <- case (topologicalSortElements $ fst <$> pairs) of
      Nothing -> fail $ "types form a cycle (unsupported in PDL)"
      Just sorted -> pure $ Y.catMaybes $ fmap (\n -> M.lookup n pairByName) sorted
    schemas <- CM.mapM toSchema sortedPairs
    return $ PDL.SchemaFile ns pkg imports schemas
  where
    pairByName = L.foldl (\m p@(el, tt) -> M.insert (elementName el) p m) M.empty pairs
    aliases = importAliasesForGraph g
    toSchema (el, TypedData typ term) = if typeTerm typ == TypeTermNominal _Type
      then decodeType cx term >>= typeToSchema el
      else fail $ "mapping of non-type elements to PDL is not yet supported: " ++ show typ
    typeToSchema el typ = do
      let qname = pdlNameForElement aliases False $ elementName el
      res <- encodeAdaptedType aliases cx typ
      let ptype = case res of
            Left schema -> PDL.NamedSchema_TypeTyperef schema
            Right t -> t
      let anns = doc $ contextDescriptionOf cx $ dataMeta $ elementData el
      return $ PDL.NamedSchema qname ptype anns

dataGraphToPegasusSchema :: (Default m, Ord m, Read m, Show m) => Context m -> Graph m -> Qualified PDL.SchemaFile
dataGraphToPegasusSchema cx g = dataGraphToExternalModule pegasusDataLanguage (encodeData aliases) constructModule cx g
  where
    aliases = importAliasesForGraph g

doc :: Y.Maybe String -> PDL.Annotations
doc s = PDL.Annotations s False

encodeAdaptedType :: (Default m, Ord m, Read m, Show m)
  => M.Map Name String -> Context m -> Type m
  -> Result (Either PDL.Schema PDL.NamedSchema_Type)
encodeAdaptedType aliases cx typ = do
  let ac = AdapterContext cx hydraCoreLanguage pegasusDataLanguage
  ad <- qualifiedToResult $ termAdapter ac typ
  encodeType aliases cx $ adapterTarget ad

encodeData :: (Default m, Eq m, Ord m, Read m, Show m) => M.Map Name String -> Context m -> Data m -> Result ()
encodeData aliases cx term@(Data expr meta) = do
    fail "not yet implemented"

encodeType :: (Default m, Eq m, Show m) => M.Map Name String -> Context m -> Type m -> Result (Either PDL.Schema PDL.NamedSchema_Type)
encodeType aliases cx typ = case typeTerm typ of
    TypeTermList lt -> Left . PDL.SchemaArray <$> encode lt
    TypeTermLiteral lt -> Left . PDL.SchemaPrimitive <$> case lt of
      LiteralTypeBinary -> pure PDL.PrimitiveTypeBytes
      LiteralTypeBoolean -> pure PDL.PrimitiveTypeBoolean
      LiteralTypeFloat ft -> case ft of
        FloatTypeFloat32 -> pure PDL.PrimitiveTypeFloat
        FloatTypeFloat64 -> pure PDL.PrimitiveTypeDouble
        _ -> fail $ "unexpected floating-point type: " ++ show ft
      LiteralTypeInteger it -> case it of
        IntegerTypeInt32 -> pure PDL.PrimitiveTypeInt
        IntegerTypeInt64 -> pure PDL.PrimitiveTypeLong
        _ -> fail $ "unexpected integer type: " ++ show it
      LiteralTypeString -> pure PDL.PrimitiveTypeString
    TypeTermMap (MapType kt vt) -> Left . PDL.SchemaMap <$> encode vt -- note: we simply assume string as a key type
    TypeTermNominal name -> pure $ Left $ PDL.SchemaNamed $ pdlNameForElement aliases True name
    TypeTermOptional ot -> fail $ "optionals unexpected at top level"
    TypeTermRecord fields -> do
      let includes = []
      rfields <- CM.mapM encodeRecordField fields
      return $ Right $ PDL.NamedSchema_TypeRecord $ PDL.RecordSchema rfields includes
    TypeTermUnion fields -> if isEnum
        then pure . Right . PDL.NamedSchema_TypeEnum $ PDL.EnumSchema $ fmap encodeEnumField fields
        else Left . PDL.SchemaUnion <$> CM.mapM encodeUnionField fields
      where
        isEnum = L.foldl (\b t -> b && t {typeMeta = dflt} == Types.unit) True $ fmap fieldTypeType fields
    _ -> fail $ "unexpected type: " ++ show typ
  where
    encode t = case typeTerm t of
      TypeTermRecord [] -> encode Types.int32 -- special case for the unit type
      _ -> do
        res <- encodeType aliases cx t
        case res of
          Left schema -> pure schema
          Right _ -> fail $ "type resolved to an unsupported nested named schema: " ++ show t
    encodeRecordField (FieldType name typ) = do
      (schema, optional) <- encodePossiblyOptionalType typ
      return PDL.RecordField {
        PDL.recordFieldName = name,
        PDL.recordFieldValue = schema,
        PDL.recordFieldOptional = optional,
        PDL.recordFieldDefault = Nothing,
        PDL.recordFieldAnnotations = anns typ}
    encodeUnionField (FieldType name typ) = do
      (s, optional) <- encodePossiblyOptionalType typ
      let schema = if optional
          then PDL.SchemaUnion (simpleUnionMember <$> [PDL.SchemaNull, s])
          else s
      return PDL.UnionMember {
        PDL.unionMemberAlias = Just name,
        PDL.unionMemberValue = schema,
        PDL.unionMemberAnnotations = anns typ}
    encodeEnumField (FieldType name typ) = PDL.EnumField {
      PDL.enumFieldName = convertCase CaseCamel CaseUpperSnake name,
      PDL.enumFieldAnnotations = anns typ}
    encodePossiblyOptionalType typ = case typeTerm typ of
      TypeTermOptional ot -> do
        t <- encode ot
        return (t, True)
      _ -> do
        t <- encode typ
        return (t, False)
    anns typ = doc $ contextDescriptionOf cx $ typeMeta typ

importAliasesForGraph g = M.empty -- TODO

noAnnotations = PDL.Annotations Nothing False

pdlNameForElement :: M.Map Name String -> Bool -> Name -> PDL.QualifiedName
pdlNameForElement aliases withNs name = PDL.QualifiedName local
    $ if withNs
      then slashesToDots <$> alias
      else Nothing
  where
    (ns, local) = toQname name
    alias = M.lookup ns aliases

pdlNameForGraph :: Graph m -> PDL.Namespace
pdlNameForGraph = slashesToDots . graphName

pegasusDataLanguage :: Language m
pegasusDataLanguage = Language "hydra/ext/pegasus/pdl" $ Language_Constraints {
  languageConstraintsLiteralVariants = S.fromList [
    LiteralVariantBinary,
    LiteralVariantBoolean,
    LiteralVariantFloat,
    LiteralVariantInteger,
    LiteralVariantString],
  languageConstraintsFloatTypes = S.fromList [
    FloatTypeFloat32,
    FloatTypeFloat64],
  languageConstraintsFunctionVariants = S.empty,
  languageConstraintsIntegerTypes = S.fromList [
    IntegerTypeInt32,
    IntegerTypeInt64],
  languageConstraintsDataVariants = S.fromList [
    DataVariantList,
    DataVariantLiteral,
    DataVariantMap,
    DataVariantNominal,
    DataVariantOptional,
    DataVariantRecord,
    DataVariantUnion],
  languageConstraintsTypeVariants = S.fromList [
    TypeVariantElement,
    TypeVariantList,
    TypeVariantLiteral,
    TypeVariantMap,
    TypeVariantNominal,
    TypeVariantOptional,
    TypeVariantRecord,
    TypeVariantUnion],
  languageConstraintsTypes = const True }

simpleUnionMember :: PDL.Schema -> PDL.UnionMember
simpleUnionMember schema = PDL.UnionMember Nothing schema noAnnotations

slashesToDots :: String -> String
slashesToDots = fmap (\c -> if c == '/' then '.' else c)
