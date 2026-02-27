-- Note: this is an automatically generated file. Do not edit.

-- | Rust code generator: converts Hydra type modules to Rust source code

module Hydra.Ext.Rust.Coder where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Ext.Rust.Language as Language
import qualified Hydra.Ext.Rust.Serde as Serde
import qualified Hydra.Ext.Rust.Syntax as Syntax
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Monads as Monads
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

standardDerives :: [String]
standardDerives = [
  "Clone",
  "Debug",
  "PartialEq",
  "Eq",
  "PartialOrd",
  "Ord"]

rustPath :: (String -> Syntax.Type)
rustPath name = (Syntax.TypePath_ (Syntax.TypePath {
  Syntax.typePathGlobal = False,
  Syntax.typePathSegments = [
    Syntax.PathSegment {
      Syntax.pathSegmentName = name,
      Syntax.pathSegmentArguments = Syntax.GenericArgumentsNone}]}))

rustPathSegmented :: ([String] -> Syntax.Type)
rustPathSegmented segs = (Syntax.TypePath_ (Syntax.TypePath {
  Syntax.typePathGlobal = False,
  Syntax.typePathSegments = (Lists.map (\s -> Syntax.PathSegment {
    Syntax.pathSegmentName = s,
    Syntax.pathSegmentArguments = Syntax.GenericArgumentsNone}) segs)}))

rustApply1 :: (String -> Syntax.Type -> Syntax.Type)
rustApply1 name arg = (Syntax.TypePath_ (Syntax.TypePath {
  Syntax.typePathGlobal = False,
  Syntax.typePathSegments = [
    Syntax.PathSegment {
      Syntax.pathSegmentName = name,
      Syntax.pathSegmentArguments = (Syntax.GenericArgumentsAngleBracketed (Syntax.AngleBracketedArgs {
        Syntax.angleBracketedArgsArgs = [
          Syntax.GenericArgType arg]}))}]}))

rustApply2 :: (String -> Syntax.Type -> Syntax.Type -> Syntax.Type)
rustApply2 name arg1 arg2 = (Syntax.TypePath_ (Syntax.TypePath {
  Syntax.typePathGlobal = False,
  Syntax.typePathSegments = [
    Syntax.PathSegment {
      Syntax.pathSegmentName = name,
      Syntax.pathSegmentArguments = (Syntax.GenericArgumentsAngleBracketed (Syntax.AngleBracketedArgs {
        Syntax.angleBracketedArgsArgs = [
          Syntax.GenericArgType arg1,
          (Syntax.GenericArgType arg2)]}))}]}))

rustUnit :: Syntax.Type
rustUnit = Syntax.TypeUnit

encodeLiteralType :: (Core.LiteralType -> Syntax.Type)
encodeLiteralType lt = ((\x -> case x of
  Core.LiteralTypeBinary -> (rustApply1 "Vec" (rustPath "u8"))
  Core.LiteralTypeBoolean -> (rustPath "bool")
  Core.LiteralTypeFloat v1 -> ((\x -> case x of
    Core.FloatTypeBigfloat -> (rustPath "f64")
    Core.FloatTypeFloat32 -> (rustPath "f32")
    Core.FloatTypeFloat64 -> (rustPath "f64")) v1)
  Core.LiteralTypeInteger v1 -> ((\x -> case x of
    Core.IntegerTypeBigint -> (rustPathSegmented [
      "num",
      "BigInt"])
    Core.IntegerTypeInt8 -> (rustPath "i8")
    Core.IntegerTypeInt16 -> (rustPath "i16")
    Core.IntegerTypeInt32 -> (rustPath "i32")
    Core.IntegerTypeInt64 -> (rustPath "i64")
    Core.IntegerTypeUint8 -> (rustPath "u8")
    Core.IntegerTypeUint16 -> (rustPath "u16")
    Core.IntegerTypeUint32 -> (rustPath "u32")
    Core.IntegerTypeUint64 -> (rustPath "u64")) v1)
  Core.LiteralTypeString -> (rustPath "String")) lt)

encodeType :: (Core.Type -> Compute.Flow t0 Syntax.Type)
encodeType t =  
  let typ = (Rewriting.deannotateType t)
  in ((\x -> case x of
    Core.TypeAnnotated v1 -> (encodeType (Core.annotatedTypeBody v1))
    Core.TypeApplication v1 -> (encodeType (Core.applicationTypeFunction v1))
    Core.TypeUnit -> (Flows.pure rustUnit)
    Core.TypeLiteral v1 -> (Flows.pure (encodeLiteralType v1))
    Core.TypeList v1 -> (Flows.map (\enc -> rustApply1 "Vec" enc) (encodeType v1))
    Core.TypeSet v1 -> (Flows.map (\enc -> rustApply1 "BTreeSet" enc) (encodeType v1))
    Core.TypeMap v1 -> (Flows.bind (encodeType (Core.mapTypeKeys v1)) (\kt -> Flows.bind (encodeType (Core.mapTypeValues v1)) (\vt -> Flows.pure (rustApply2 "BTreeMap" kt vt))))
    Core.TypeMaybe v1 -> (Flows.map (\enc -> rustApply1 "Option" enc) (encodeType v1))
    Core.TypeEither v1 -> (Flows.bind (encodeType (Core.eitherTypeLeft v1)) (\lt -> Flows.bind (encodeType (Core.eitherTypeRight v1)) (\rt -> Flows.pure (rustApply2 "Either" lt rt))))
    Core.TypePair v1 -> (Flows.bind (encodeType (Core.pairTypeFirst v1)) (\ft -> Flows.bind (encodeType (Core.pairTypeSecond v1)) (\st -> Flows.pure (Syntax.TypeTuple [
      ft,
      st]))))
    Core.TypeFunction v1 -> (Flows.bind (encodeType (Core.functionTypeDomain v1)) (\dom -> Flows.bind (encodeType (Core.functionTypeCodomain v1)) (\cod -> Flows.pure (rustApply1 "Box" (Syntax.TypeDynTrait [
      Syntax.TypeParamBoundTrait (Syntax.TypePath {
        Syntax.typePathGlobal = False,
        Syntax.typePathSegments = [
          Syntax.PathSegment {
            Syntax.pathSegmentName = "Fn",
            Syntax.pathSegmentArguments = (Syntax.GenericArgumentsParenthesized (Syntax.ParenthesizedArgs {
              Syntax.parenthesizedArgsInputs = [
                dom],
              Syntax.parenthesizedArgsOutput = (Just cod)}))}]})])))))
    Core.TypeRecord v1 -> (Flows.pure (rustPath (Formatting.capitalize (Names.localNameOf (Core.rowTypeTypeName v1)))))
    Core.TypeUnion v1 -> (Flows.pure (rustPath (Formatting.capitalize (Names.localNameOf (Core.rowTypeTypeName v1)))))
    Core.TypeWrap v1 -> (Flows.pure (rustPath (Formatting.capitalize (Names.localNameOf (Core.wrappedTypeTypeName v1)))))
    Core.TypeVariable v1 -> (Flows.pure (rustPath (Formatting.capitalize (Core.unName v1))))
    Core.TypeForall v1 -> (encodeType (Core.forallTypeBody v1))) typ)

encodeStructField :: (Core.FieldType -> Compute.Flow t0 Syntax.StructField)
encodeStructField ft =  
  let fname = (Core.unName (Core.fieldTypeName ft))
  in  
    let ftyp = (Core.fieldTypeType ft)
    in (Flows.bind (encodeType ftyp) (\sftyp -> Flows.pure (Syntax.StructField {
      Syntax.structFieldName = (Formatting.convertCaseCamelToLowerSnake (Formatting.sanitizeWithUnderscores Language.rustReservedWords fname)),
      Syntax.structFieldType = sftyp,
      Syntax.structFieldPublic = True,
      Syntax.structFieldDoc = Nothing})))

encodeEnumVariant :: (Core.FieldType -> Compute.Flow t0 Syntax.EnumVariant)
encodeEnumVariant ft =  
  let fname = (Core.unName (Core.fieldTypeName ft))
  in  
    let ftyp = (Core.fieldTypeType ft)
    in  
      let dtyp = (Rewriting.deannotateType ftyp)
      in  
        let isUnit = ((\x -> case x of
                Core.TypeUnit -> True
                Core.TypeRecord v1 -> (Lists.null (Core.rowTypeFields v1))
                _ -> False) dtyp)
        in (Logic.ifElse isUnit (Flows.pure (Syntax.EnumVariant {
          Syntax.enumVariantName = (Formatting.capitalize fname),
          Syntax.enumVariantBody = Syntax.EnumVariantBodyUnit,
          Syntax.enumVariantDoc = Nothing})) ((\x -> case x of
          Core.TypeRecord v1 -> (Flows.bind (Flows.mapList encodeStructField (Core.rowTypeFields v1)) (\sfields -> Flows.pure (Syntax.EnumVariant {
            Syntax.enumVariantName = (Formatting.capitalize fname),
            Syntax.enumVariantBody = (Syntax.EnumVariantBodyStruct sfields),
            Syntax.enumVariantDoc = Nothing})))
          _ -> (Flows.bind (encodeType ftyp) (\sftyp -> Flows.pure (Syntax.EnumVariant {
            Syntax.enumVariantName = (Formatting.capitalize fname),
            Syntax.enumVariantBody = (Syntax.EnumVariantBodyTuple [
              sftyp]),
            Syntax.enumVariantDoc = Nothing})))) dtyp))

encodeTypeDefinition :: (Module.TypeDefinition -> Compute.Flow t0 Syntax.ItemWithComments)
encodeTypeDefinition tdef =  
  let name = (Module.typeDefinitionName tdef)
  in  
    let typ = (Module.typeDefinitionType tdef)
    in  
      let lname = (Formatting.capitalize (Names.localNameOf name))
      in  
        let freeVars = (Lists.filter (\v -> Lists.null (Lists.tail (Strings.splitOn "." (Core.unName v)))) (Sets.toList (Rewriting.freeVariablesInType typ)))
        in  
          let generics = (Lists.map (\v -> Syntax.GenericParam {
                  Syntax.genericParamName = (Formatting.capitalize (Core.unName v)),
                  Syntax.genericParamBounds = []}) freeVars)
          in  
            let dtyp = (Rewriting.deannotateType typ)
            in (Monads.withTrace (Strings.cat2 "type " (Core.unName name)) (Flows.bind ((\x -> case x of
              Core.TypeRecord v1 -> (Flows.bind (Flows.mapList encodeStructField (Core.rowTypeFields v1)) (\sfields -> Flows.pure (Syntax.ItemStruct (Syntax.StructDef {
                Syntax.structDefName = lname,
                Syntax.structDefGenerics = generics,
                Syntax.structDefWhereClause = Nothing,
                Syntax.structDefBody = (Syntax.StructBodyNamed sfields),
                Syntax.structDefDerives = standardDerives,
                Syntax.structDefPublic = True,
                Syntax.structDefDoc = Nothing}))))
              Core.TypeUnion v1 -> (Flows.bind (Flows.mapList encodeEnumVariant (Core.rowTypeFields v1)) (\variants -> Flows.pure (Syntax.ItemEnum (Syntax.EnumDef {
                Syntax.enumDefName = lname,
                Syntax.enumDefGenerics = generics,
                Syntax.enumDefWhereClause = Nothing,
                Syntax.enumDefVariants = variants,
                Syntax.enumDefDerives = standardDerives,
                Syntax.enumDefPublic = True,
                Syntax.enumDefDoc = Nothing}))))
              Core.TypeWrap v1 -> (Flows.bind (encodeType (Core.wrappedTypeBody v1)) (\styp -> Flows.pure (Syntax.ItemStruct (Syntax.StructDef {
                Syntax.structDefName = lname,
                Syntax.structDefGenerics = generics,
                Syntax.structDefWhereClause = Nothing,
                Syntax.structDefBody = (Syntax.StructBodyTuple [
                  Syntax.TupleField {
                    Syntax.tupleFieldType = styp,
                    Syntax.tupleFieldPublic = True}]),
                Syntax.structDefDerives = standardDerives,
                Syntax.structDefPublic = True,
                Syntax.structDefDoc = Nothing}))))
              _ -> (Flows.bind (encodeType typ) (\styp -> Flows.pure (Syntax.ItemTypeAlias (Syntax.TypeAlias {
                Syntax.typeAliasName = lname,
                Syntax.typeAliasGenerics = generics,
                Syntax.typeAliasType = styp,
                Syntax.typeAliasPublic = True,
                Syntax.typeAliasDoc = Nothing}))))) dtyp) (\item -> Flows.pure (Syntax.ItemWithComments {
              Syntax.itemWithCommentsDoc = Nothing,
              Syntax.itemWithCommentsVisibility = Syntax.VisibilityPublic,
              Syntax.itemWithCommentsItem = item}))))

moduleToRust :: (Module.Module -> [Module.Definition] -> Compute.Flow t0 (M.Map String String))
moduleToRust mod defs = (Monads.withTrace (Strings.cat2 "encode Rust module: " (Module.unNamespace (Module.moduleNamespace mod))) ( 
  let typeDefs = (Pairs.first (Schemas.partitionDefinitions defs))
  in (Flows.bind (Flows.mapList encodeTypeDefinition typeDefs) (\items ->  
    let crate = Syntax.Crate {
            Syntax.crateItems = items}
    in  
      let code = (Serialization.printExpr (Serialization.parenthesize (Serde.crateToExpr crate)))
      in  
        let filePath = (Names.namespaceToFilePath Util.CaseConventionLowerSnake (Module.FileExtension "rs") (Module.moduleNamespace mod))
        in (Flows.pure (Maps.singleton filePath code))))))
