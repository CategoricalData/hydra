module Hydra.Ext.Staging.Graphql.Serde (exprDocument) where

import Hydra.Kernel
import Hydra.Formatting
import Hydra.Serialization
import qualified Hydra.Ast as CT
import qualified Hydra.Ext.Org.Graphql.Syntax as G

import qualified Data.List as L
import qualified Data.Maybe as Y


commentDelim = cst "\"\"\"" :: CT.Expr

exprDefinition :: G.Definition -> CT.Expr
exprDefinition def = case def of
  G.DefinitionExecutable _ -> unsup def
  G.DefinitionTypeSystem de -> exprTypeSystemDefinitionOrExtension de

exprDescription :: G.Description -> CT.Expr
exprDescription desc = newlineSep [
  commentDelim,
  cst $ G.unStringValue $ G.unDescription desc,
  commentDelim]

exprDocument :: G.Document -> CT.Expr
exprDocument d = doubleNewlineSep (exprDefinition <$> G.unDocument d)

exprEnumTypeDefinition :: G.EnumTypeDefinition -> CT.Expr
exprEnumTypeDefinition def = withDescription (G.enumTypeDefinitionDescription def) $
    spaceSep [cst "enum", exprName (G.enumTypeDefinitionName def),
      curlyBracesList Nothing fullBlockStyle valuesExpr]
  where
    valuesExpr = case G.enumTypeDefinitionEnumValuesDefinition def of
      Nothing -> []
      Just values -> exprEnumValueDefinition <$> G.unEnumValuesDefinition values

exprEnumValue :: G.EnumValue -> CT.Expr
exprEnumValue = exprName . G.unEnumValue

exprEnumValueDefinition :: G.EnumValueDefinition -> CT.Expr
exprEnumValueDefinition def = withDescription (G.enumValueDefinitionDescription def) $
  exprEnumValue $ G.enumValueDefinitionEnumValue def

exprFieldDefinition :: G.FieldDefinition -> CT.Expr
exprFieldDefinition def = withDescription (G.fieldDefinitionDescription def) $
    spaceSep [namePart, typePart]
  where
    namePart = noSep[exprName (G.fieldDefinitionName def), cst ":"]
    typePart = exprType $ G.fieldDefinitionType def

exprListType :: G.ListType -> CT.Expr
exprListType lt = noSep[cst "[", exprType $ G.unListType lt, cst "]"]

exprName :: G.Name -> CT.Expr
exprName = cst . G.unName

exprNamedType :: G.NamedType -> CT.Expr
exprNamedType = exprName . G.unNamedType

exprNonNullType :: G.NonNullType -> CT.Expr
exprNonNullType nnt = noSep [typeExpr, cst "!"]
  where
    typeExpr = case nnt of
      G.NonNullTypeNamed nt -> exprNamedType nt
      G.NonNullTypeList lt -> exprListType lt

exprObjectTypeDefinition :: G.ObjectTypeDefinition -> CT.Expr
exprObjectTypeDefinition def = withDescription (G.objectTypeDefinitionDescription def) $
    spaceSep [cst "type", exprName (G.objectTypeDefinitionName def),
      curlyBracesList Nothing fullBlockStyle fieldsExpr]
  where
    fieldsExpr = case G.objectTypeDefinitionFieldsDefinition def of
      Nothing -> []
      Just fields -> exprFieldDefinition <$> G.unFieldsDefinition fields

exprType :: G.Type -> CT.Expr
exprType typ = case typ of
  G.TypeNamed nt -> exprNamedType nt
  G.TypeList lt -> exprListType lt
  G.TypeNonNull nnt -> exprNonNullType nnt

exprTypeDefinition :: G.TypeDefinition -> CT.Expr
exprTypeDefinition def = case def of
  G.TypeDefinitionScalar _ -> unsup def
  G.TypeDefinitionObject od -> exprObjectTypeDefinition od
  G.TypeDefinitionInterface _ -> unsup def
  G.TypeDefinitionUnion _ -> unsup def
  G.TypeDefinitionEnum ed -> exprEnumTypeDefinition ed
  G.TypeDefinitionInputObject _ -> unsup def

exprTypeSystemDefinition :: G.TypeSystemDefinition -> CT.Expr
exprTypeSystemDefinition def = case def of
  G.TypeSystemDefinitionSchema _ -> unsup def
  G.TypeSystemDefinitionType dt -> exprTypeDefinition dt
  G.TypeSystemDefinitionDirective _ -> unsup def

exprTypeSystemDefinitionOrExtension :: G.TypeSystemDefinitionOrExtension -> CT.Expr
exprTypeSystemDefinitionOrExtension de = case de of
  G.TypeSystemDefinitionOrExtensionDefinition d -> exprTypeSystemDefinition d
  G.TypeSystemDefinitionOrExtensionExtension _ -> unsup de

unsup :: Show x => x -> CT.Expr
unsup obj = cst $ "Unsupported: " ++ show obj

withDescription :: Maybe G.Description -> CT.Expr -> CT.Expr
withDescription mdesc expr = newlineSep $ Y.catMaybes [exprDescription <$> mdesc, Y.Just expr]
