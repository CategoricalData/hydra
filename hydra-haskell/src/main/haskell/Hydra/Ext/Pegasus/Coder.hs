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
  => Context m -> Graph m -> M.Map Type (Step (Term m) ()) -> [(Element m, TypedTerm m)] -> Result PDL.SchemaFile
constructModule cx g coders pairs = do
    let ns = pdlNameForGraph g
    let pkg = Nothing
    let imports = [] -- TODO
    schemas <- CM.mapM toSchema pairs
    return $ PDL.SchemaFile ns pkg imports schemas
  where
    aliases = importAliasesForGraph g
    toSchema (el, TypedTerm typ term) = if typ == TypeNominal _Type
      then decodeType cx term >>= typeToSchema el
--      then deref cx term >>= decodeType cx >>= typeToSchema el
      else fail $ "mapping of non-type elements to PDL is not yet supported: " ++ show typ
    typeToSchema el typ = do
      let qname = pdlNameForElement $ elementName el
      res <- encodeAdaptedType aliases cx typ
      let ptype = case res of
            Left schema -> PDL.NamedSchema_TypeTyperef schema
            Right t -> t
      let anns = noAnnotations
      return $ PDL.NamedSchema qname ptype anns

dataGraphToPegasusSchema :: (Default m, Ord m, Read m, Show m) => Context m -> Graph m -> Qualified PDL.SchemaFile
dataGraphToPegasusSchema cx g = dataGraphToExternalModule pegasusDataLanguage (encodeTerm aliases) constructModule cx g
  where
    aliases = importAliasesForGraph g

encodeAdaptedType :: (Default m, Ord m, Read m, Show m)
  => M.Map Name String -> Context m -> Type
  -> Result (Either PDL.Schema PDL.NamedSchema_Type)
encodeAdaptedType aliases cx typ = do
  let ac = AdapterContext cx hydraCoreLanguage pegasusDataLanguage
  ad <- qualifiedToResult $ termAdapter ac typ
  encodeType aliases $ adapterTarget ad

encodeTerm :: (Default m, Eq m, Ord m, Read m, Show m) => M.Map Name String -> Context m -> Term m -> Result ()
encodeTerm aliases cx term@(Term expr meta) = do
    fail "not yet implemented"

encodeType :: M.Map Name String -> Type -> Result (Either PDL.Schema PDL.NamedSchema_Type)
encodeType aliases typ = case typ of
    TypeList lt -> Left . PDL.SchemaArray <$> encode lt
    TypeLiteral lt -> Left . PDL.SchemaPrimitive <$> case lt of
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
    TypeMap (MapType kt vt) -> Left . PDL.SchemaMap <$> encode vt -- note: we simply assume string as a key type
    TypeNominal name -> pure $ Left $ PDL.SchemaNamed $ pdlNameForElement name
    TypeOptional ot -> fail $ "optionals unexpected at top level"
    TypeRecord fields -> do
      let includes = []
      rfields <- CM.mapM encodeRecordField fields
      return $ Right $ PDL.NamedSchema_TypeRecord $ PDL.RecordSchema rfields includes
    TypeUnion fields -> Left . PDL.SchemaUnion <$> CM.mapM encodeUnionField fields
    _ -> fail $ "unexpected type: " ++ show typ
  where
    encode t = case t of
      TypeRecord [] -> encode Types.int32 -- special case for the unit type
      _ -> do
        res <- encodeType aliases t
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
        PDL.recordFieldAnnotations = noAnnotations}
    encodeUnionField (FieldType name typ) = do
      (s, optional) <- encodePossiblyOptionalType typ
      let schema = if optional
          then PDL.SchemaUnion (simpleUnionMember <$> [PDL.SchemaNull, s])
          else s
      return PDL.UnionMember {
        PDL.unionMemberAlias = Just name,
        PDL.unionMemberValue = schema,
        PDL.unionMemberAnnotations = noAnnotations}
    encodePossiblyOptionalType typ = case typ of
      TypeOptional ot -> do
        t <- encode ot
        return (t, True)
      _ -> do
        t <- encode typ
        return (t, False)

importAliasesForGraph g = M.empty -- TODO

noAnnotations = PDL.Annotations Nothing False

pdlNameForElement :: Name -> PDL.QualifiedName
pdlNameForElement name = PDL.QualifiedName local $ Just (slashesToDots ns)
  where
    (ns, local) = toQname name

pdlNameForGraph :: Graph m -> PDL.Namespace
pdlNameForGraph = slashesToDots . graphName

pegasusDataLanguage :: Language
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
  languageConstraintsTermVariants = S.fromList [
    TermVariantList,
    TermVariantLiteral,
    TermVariantMap,
    TermVariantNominal,
    TermVariantOptional,
    TermVariantRecord,
    TermVariantUnion],
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
