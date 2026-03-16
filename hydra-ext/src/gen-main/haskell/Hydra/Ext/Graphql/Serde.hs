-- Note: this is an automatically generated file. Do not edit.

-- | Serialization functions for converting GraphQL AST to abstract expressions

module Hydra.Ext.Graphql.Serde where

import qualified Hydra.Ast as Ast
import qualified Hydra.Ext.Org.Graphql.Syntax as Syntax
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Convert a GraphQL definition to an expression
exprDefinition :: Syntax.Definition -> Ast.Expr
exprDefinition def =
    case def of
      Syntax.DefinitionExecutable _ -> Serialization.cst "Unsupported: executable definition"
      Syntax.DefinitionTypeSystem v0 -> exprTypeSystemDefinitionOrExtension v0

-- | Convert a GraphQL description to a triple-quoted comment
exprDescription :: Syntax.Description -> Ast.Expr
exprDescription desc =
     
      let delim = Serialization.cst "\"\"\"" 
          text = Syntax.unStringValue (Syntax.unDescription desc)
      in (Serialization.newlineSep [
        delim,
        (Serialization.cst text),
        delim])

-- | Convert a GraphQL document to an expression
exprDocument :: Syntax.Document -> Ast.Expr
exprDocument d = Serialization.doubleNewlineSep (Lists.map exprDefinition (Syntax.unDocument d))

-- | Convert a GraphQL enum type definition to an expression
exprEnumTypeDefinition :: Syntax.EnumTypeDefinition -> Ast.Expr
exprEnumTypeDefinition def =
     
      let desc = Syntax.enumTypeDefinitionDescription def 
          name = Syntax.enumTypeDefinitionName def
          values = Syntax.enumTypeDefinitionEnumValuesDefinition def
          valuesExpr = Maybes.maybe [] (\vs -> Lists.map exprEnumValueDefinition (Syntax.unEnumValuesDefinition vs)) values
      in (withDescription desc (Serialization.spaceSep [
        Serialization.cst "enum",
        (exprName name),
        (Serialization.curlyBracesList Nothing Serialization.fullBlockStyle valuesExpr)]))

-- | Convert a GraphQL enum value to an expression
exprEnumValue :: Syntax.EnumValue -> Ast.Expr
exprEnumValue ev = exprName (Syntax.unEnumValue ev)

-- | Convert a GraphQL enum value definition to an expression
exprEnumValueDefinition :: Syntax.EnumValueDefinition -> Ast.Expr
exprEnumValueDefinition def =
     
      let desc = Syntax.enumValueDefinitionDescription def 
          ev = Syntax.enumValueDefinitionEnumValue def
      in (withDescription desc (exprEnumValue ev))

-- | Convert a GraphQL field definition to an expression
exprFieldDefinition :: Syntax.FieldDefinition -> Ast.Expr
exprFieldDefinition def =
     
      let desc = Syntax.fieldDefinitionDescription def 
          name = Syntax.fieldDefinitionName def
          typ = Syntax.fieldDefinitionType def
          namePart =
                  Serialization.noSep [
                    exprName name,
                    (Serialization.cst ":")]
          typePart = exprType typ
      in (withDescription desc (Serialization.spaceSep [
        namePart,
        typePart]))

-- | Convert a GraphQL list type to an expression
exprListType :: Syntax.ListType -> Ast.Expr
exprListType lt =
    Serialization.noSep [
      Serialization.cst "[",
      (exprType (Syntax.unListType lt)),
      (Serialization.cst "]")]

-- | Convert a GraphQL name to an expression
exprName :: Syntax.Name -> Ast.Expr
exprName n = Serialization.cst (Syntax.unName n)

-- | Convert a GraphQL named type to an expression
exprNamedType :: Syntax.NamedType -> Ast.Expr
exprNamedType nt = exprName (Syntax.unNamedType nt)

-- | Convert a GraphQL non-null type to an expression
exprNonNullType :: Syntax.NonNullType -> Ast.Expr
exprNonNullType nnt =
     
      let typeExpr =
              case nnt of
                Syntax.NonNullTypeNamed v0 -> exprNamedType v0
                Syntax.NonNullTypeList v0 -> exprListType v0
      in (Serialization.noSep [
        typeExpr,
        (Serialization.cst "!")])

-- | Convert a GraphQL object type definition to an expression
exprObjectTypeDefinition :: Syntax.ObjectTypeDefinition -> Ast.Expr
exprObjectTypeDefinition def =
     
      let desc = Syntax.objectTypeDefinitionDescription def 
          name = Syntax.objectTypeDefinitionName def
          fields = Syntax.objectTypeDefinitionFieldsDefinition def
          fieldsExpr = Maybes.maybe [] (\fs -> Lists.map exprFieldDefinition (Syntax.unFieldsDefinition fs)) fields
      in (withDescription desc (Serialization.spaceSep [
        Serialization.cst "type",
        (exprName name),
        (Serialization.curlyBracesList Nothing Serialization.fullBlockStyle fieldsExpr)]))

-- | Convert a GraphQL type to an expression
exprType :: Syntax.Type -> Ast.Expr
exprType typ =
    case typ of
      Syntax.TypeNamed v0 -> exprNamedType v0
      Syntax.TypeList v0 -> exprListType v0
      Syntax.TypeNonNull v0 -> exprNonNullType v0

-- | Convert a GraphQL type definition to an expression
exprTypeDefinition :: Syntax.TypeDefinition -> Ast.Expr
exprTypeDefinition def =
    case def of
      Syntax.TypeDefinitionScalar _ -> Serialization.cst "Unsupported: scalar type definition"
      Syntax.TypeDefinitionObject v0 -> exprObjectTypeDefinition v0
      Syntax.TypeDefinitionInterface _ -> Serialization.cst "Unsupported: interface type definition"
      Syntax.TypeDefinitionUnion _ -> Serialization.cst "Unsupported: union type definition"
      Syntax.TypeDefinitionEnum v0 -> exprEnumTypeDefinition v0
      Syntax.TypeDefinitionInputObject _ -> Serialization.cst "Unsupported: input object type definition"

-- | Convert a GraphQL type system definition to an expression
exprTypeSystemDefinition :: Syntax.TypeSystemDefinition -> Ast.Expr
exprTypeSystemDefinition def =
    case def of
      Syntax.TypeSystemDefinitionSchema _ -> Serialization.cst "Unsupported: schema definition"
      Syntax.TypeSystemDefinitionType v0 -> exprTypeDefinition v0
      Syntax.TypeSystemDefinitionDirective _ -> Serialization.cst "Unsupported: directive definition"

-- | Convert a GraphQL type system definition or extension to an expression
exprTypeSystemDefinitionOrExtension :: Syntax.TypeSystemDefinitionOrExtension -> Ast.Expr
exprTypeSystemDefinitionOrExtension de =
    case de of
      Syntax.TypeSystemDefinitionOrExtensionDefinition v0 -> exprTypeSystemDefinition v0
      Syntax.TypeSystemDefinitionOrExtensionExtension _ -> Serialization.cst "Unsupported: type system extension"

-- | Prepend an optional description to an expression
withDescription :: Maybe Syntax.Description -> Ast.Expr -> Ast.Expr
withDescription mdesc expr =
    Serialization.newlineSep (Maybes.cat [
      Maybes.map exprDescription mdesc,
      (Maybes.pure expr)])
