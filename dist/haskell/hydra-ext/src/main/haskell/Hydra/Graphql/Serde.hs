-- Note: this is an automatically generated file. Do not edit.
-- | Serialization functions for converting GraphQL AST to abstract expressions

module Hydra.Graphql.Serde where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Graphql.Syntax as Syntax
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Convert a GraphQL definition to an expression
definitionToExpr :: Syntax.Definition -> Ast.Expr
definitionToExpr def =
    case def of
      Syntax.DefinitionExecutable _ -> Serialization.cst "Unsupported: executable definition"
      Syntax.DefinitionTypeSystem v0 -> typeSystemDefinitionOrExtensionToExpr v0
-- | Convert a GraphQL description to a triple-quoted comment
descriptionToExpr :: Syntax.Description -> Ast.Expr
descriptionToExpr desc =

      let delim = Serialization.cst "\"\"\""
          text = Syntax.unStringValue (Syntax.unDescription desc)
      in (Serialization.newlineSep [
        delim,
        (Serialization.cst text),
        delim])
-- | Convert a GraphQL document to an expression
documentToExpr :: Syntax.Document -> Ast.Expr
documentToExpr d = Serialization.doubleNewlineSep (Lists.map definitionToExpr (Syntax.unDocument d))
-- | Convert a GraphQL enum type definition to an expression
enumTypeDefinitionToExpr :: Syntax.EnumTypeDefinition -> Ast.Expr
enumTypeDefinitionToExpr def =

      let desc = Syntax.enumTypeDefinitionDescription def
          name = Syntax.enumTypeDefinitionName def
          values = Syntax.enumTypeDefinitionEnumValuesDefinition def
          valuesExpr = Optionals.cases values [] (\vs -> Lists.map enumValueDefinitionToExpr (Syntax.unEnumValuesDefinition vs))
      in (withDescription desc (Serialization.spaceSep [
        Serialization.cst "enum",
        (nameToExpr name),
        (Serialization.curlyBracesList Nothing Serialization.fullBlockStyle valuesExpr)]))
-- | Convert a GraphQL enum value definition to an expression
enumValueDefinitionToExpr :: Syntax.EnumValueDefinition -> Ast.Expr
enumValueDefinitionToExpr def =

      let desc = Syntax.enumValueDefinitionDescription def
          ev = Syntax.enumValueDefinitionEnumValue def
      in (withDescription desc (enumValueToExpr ev))
-- | Convert a GraphQL enum value to an expression
enumValueToExpr :: Syntax.EnumValue -> Ast.Expr
enumValueToExpr ev = nameToExpr (Syntax.unEnumValue ev)
-- | Convert a GraphQL field definition to an expression
fieldDefinitionToExpr :: Syntax.FieldDefinition -> Ast.Expr
fieldDefinitionToExpr def =

      let desc = Syntax.fieldDefinitionDescription def
          name = Syntax.fieldDefinitionName def
          typ = Syntax.fieldDefinitionType def
          namePart =
                  Serialization.noSep [
                    nameToExpr name,
                    (Serialization.cst ":")]
          typePart = typeToExpr typ
      in (withDescription desc (Serialization.spaceSep [
        namePart,
        typePart]))
-- | Convert a GraphQL list type to an expression
listTypeToExpr :: Syntax.ListType -> Ast.Expr
listTypeToExpr lt =
    Serialization.noSep [
      Serialization.cst "[",
      (typeToExpr (Syntax.unListType lt)),
      (Serialization.cst "]")]
-- | Convert a GraphQL name to an expression
nameToExpr :: Syntax.Name -> Ast.Expr
nameToExpr n = Serialization.cst (Syntax.unName n)
-- | Convert a GraphQL named type to an expression
namedTypeToExpr :: Syntax.NamedType -> Ast.Expr
namedTypeToExpr nt = nameToExpr (Syntax.unNamedType nt)
-- | Convert a GraphQL non-null type to an expression
nonNullTypeToExpr :: Syntax.NonNullType -> Ast.Expr
nonNullTypeToExpr nnt =

      let typeExpr =
              case nnt of
                Syntax.NonNullTypeNamed v0 -> namedTypeToExpr v0
                Syntax.NonNullTypeList v0 -> listTypeToExpr v0
      in (Serialization.noSep [
        typeExpr,
        (Serialization.cst "!")])
-- | Convert a GraphQL object type definition to an expression
objectTypeDefinitionToExpr :: Syntax.ObjectTypeDefinition -> Ast.Expr
objectTypeDefinitionToExpr def =

      let desc = Syntax.objectTypeDefinitionDescription def
          name = Syntax.objectTypeDefinitionName def
          fields = Syntax.objectTypeDefinitionFieldsDefinition def
          fieldsExpr = Optionals.cases fields [] (\fs -> Lists.map fieldDefinitionToExpr (Syntax.unFieldsDefinition fs))
      in (withDescription desc (Serialization.spaceSep [
        Serialization.cst "type",
        (nameToExpr name),
        (Serialization.curlyBracesList Nothing Serialization.fullBlockStyle fieldsExpr)]))
-- | Convert a GraphQL type definition to an expression
typeDefinitionToExpr :: Syntax.TypeDefinition -> Ast.Expr
typeDefinitionToExpr def =
    case def of
      Syntax.TypeDefinitionScalar _ -> Serialization.cst "Unsupported: scalar type definition"
      Syntax.TypeDefinitionObject v0 -> objectTypeDefinitionToExpr v0
      Syntax.TypeDefinitionInterface _ -> Serialization.cst "Unsupported: interface type definition"
      Syntax.TypeDefinitionUnion _ -> Serialization.cst "Unsupported: union type definition"
      Syntax.TypeDefinitionEnum v0 -> enumTypeDefinitionToExpr v0
      Syntax.TypeDefinitionInputObject _ -> Serialization.cst "Unsupported: input object type definition"
-- | Convert a GraphQL type system definition or extension to an expression
typeSystemDefinitionOrExtensionToExpr :: Syntax.TypeSystemDefinitionOrExtension -> Ast.Expr
typeSystemDefinitionOrExtensionToExpr de =
    case de of
      Syntax.TypeSystemDefinitionOrExtensionDefinition v0 -> typeSystemDefinitionToExpr v0
      Syntax.TypeSystemDefinitionOrExtensionExtension _ -> Serialization.cst "Unsupported: type system extension"
-- | Convert a GraphQL type system definition to an expression
typeSystemDefinitionToExpr :: Syntax.TypeSystemDefinition -> Ast.Expr
typeSystemDefinitionToExpr def =
    case def of
      Syntax.TypeSystemDefinitionSchema _ -> Serialization.cst "Unsupported: schema definition"
      Syntax.TypeSystemDefinitionType v0 -> typeDefinitionToExpr v0
      Syntax.TypeSystemDefinitionDirective _ -> Serialization.cst "Unsupported: directive definition"
-- | Convert a GraphQL type to an expression
typeToExpr :: Syntax.Type -> Ast.Expr
typeToExpr typ =
    case typ of
      Syntax.TypeNamed v0 -> namedTypeToExpr v0
      Syntax.TypeList v0 -> listTypeToExpr v0
      Syntax.TypeNonNull v0 -> nonNullTypeToExpr v0
-- | Prepend an optional description to an expression
withDescription :: Maybe Syntax.Description -> Ast.Expr -> Ast.Expr
withDescription mdesc expr =
    Serialization.newlineSep (Optionals.cat [
      Optionals.map descriptionToExpr mdesc,
      (Optionals.pure expr)])
