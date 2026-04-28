module Hydra.Sources.Graphql.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Strip          as Strip
import qualified Hydra.Sources.Kernel.Terms.Predicates    as Predicates
import qualified Hydra.Sources.Kernel.Terms.Environment   as Environment
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants  as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import qualified Hydra.Graphql.Syntax as G
import qualified Hydra.Sources.Graphql.Syntax as GraphqlSyntax
import qualified Hydra.Sources.Graphql.Language as GraphqlLanguage
import qualified Hydra.Sources.Graphql.Serde as GraphqlSerde
import qualified Hydra.Dsl.Meta.Context as Ctx
import qualified Hydra.Dsl.Errors as Error


ns :: Namespace
ns = Namespace "hydra.graphql.coder"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [Formatting.ns, Names.ns, Strip.ns, Environment.ns, Predicates.ns, Annotations.ns, Serialization.ns,
      moduleNamespace GraphqlLanguage.module_,
      GraphqlSerde.ns],
            moduleTypeDependencies = (moduleNamespace GraphqlSyntax.module_:KernelTypes.kernelTypesNamespaces),
            moduleDescription = Just "GraphQL code generator: converts Hydra modules to GraphQL schema definitions"}
  where
    definitions = [
      toDefinition descriptionFromType,
      toDefinition encodeEnumFieldName,
      toDefinition encodeEnumFieldType,
      toDefinition encodeFieldName,
      toDefinition encodeFieldType,
      toDefinition encodeLiteralType,
      toDefinition encodeNamedType,
      toDefinition encodeType,
      toDefinition encodeTypeDefinition,
      toDefinition encodeTypeName,
      toDefinition encodeUnionFieldType,
      toDefinition moduleToGraphql,
      toDefinition sanitize]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- | Helper: find namespace prefixes from type definitions
findPrefixes :: TTerm (Namespace -> [TypeDefinition] -> M.Map Namespace String)
findPrefixes = lambda "modNs" $ lambda "tdefs" $ lets [
  "namespaces">: (Lists.nub :: TTerm [Namespace] -> TTerm [Namespace]) $ Maybes.cat $ Lists.map
    (lambda "td" $ Names.namespaceOf @@ (Packaging.typeDefinitionName $ var "td"))
    (var "tdefs")] $
  Maps.fromList $ Lists.map
    (lambda "ns_" $ pair (var "ns_")
      (Logic.ifElse (Equality.equal (var "ns_") (var "modNs"))
        (string "")
        (Strings.cat2 (Formatting.sanitizeWithUnderscores @@ Sets.empty @@ (Packaging.unNamespace $ var "ns_")) (string "_"))))
    (var "namespaces")

-- | Helper: wrap a type in a record with a single "value" field
wrapAsRecord :: TTerm Name -> TTerm Context -> TTerm Graph -> TTerm (M.Map Namespace String) -> TTerm Type -> TTerm (Either Error G.TypeDefinition)
wrapAsRecord name cx g prefixes innerTyp =
  encodeNamedType @@ cx @@ g @@ prefixes @@ name @@
    (inject _Type _Type_record $ list [
      Core.fieldType (Core.name $ string "value") innerTyp])

-- | Get the description from a type as a GraphQL Description
descriptionFromType :: TTermDefinition (Context -> Graph -> Type -> Either Error (Maybe G.Description))
descriptionFromType = define "descriptionFromType" $
  "cx" ~> "g" ~> lambda "typ" $
    Eithers.map
      (lambda "mval" $ Maybes.map
        (lambda "s" $ wrap G._Description (wrap G._StringValue (var "s")))
        (var "mval"))
      (Annotations.getTypeDescription @@ var "cx" @@ var "g" @@ var "typ")

-- | Encode a field name to a GraphQL EnumValue
encodeEnumFieldName :: TTermDefinition (Name -> G.EnumValue)
encodeEnumFieldName = define "encodeEnumFieldName" $
  lambda "name" $ wrap G._EnumValue (wrap G._Name (sanitize @@ (Core.unName $ var "name")))

-- | Encode an enum field type to a GraphQL EnumValueDefinition
encodeEnumFieldType :: TTermDefinition (Context -> Graph -> FieldType -> Either Error G.EnumValueDefinition)
encodeEnumFieldType = define "encodeEnumFieldType" $
  "cx" ~> "g" ~> lambda "ft" $
    "desc" <<~ (descriptionFromType @@ var "cx" @@ var "g" @@ (Core.fieldTypeType $ var "ft")) $
    right (record G._EnumValueDefinition [
      G._EnumValueDefinition_Description>>: var "desc",
      G._EnumValueDefinition_EnumValue>>: (encodeEnumFieldName @@ (Core.fieldTypeName $ var "ft")),
      G._EnumValueDefinition_Directives>>: nothing])

-- | Encode a field name to a GraphQL Name
encodeFieldName :: TTermDefinition (Name -> G.Name)
encodeFieldName = define "encodeFieldName" $
  lambda "name" $ wrap G._Name (sanitize @@ (Core.unName $ var "name"))

-- | Encode a field type to a GraphQL FieldDefinition
encodeFieldType :: TTermDefinition (Context -> Graph -> M.Map Namespace String -> FieldType -> Either Error G.FieldDefinition)
encodeFieldType = define "encodeFieldType" $
  "cx" ~> "g" ~> lambda "prefixes" $ lambda "ft" $
    "gtype" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "prefixes" @@ (Core.fieldTypeType $ var "ft")) $
    "desc" <<~ (descriptionFromType @@ var "cx" @@ var "g" @@ (Core.fieldTypeType $ var "ft")) $
    right (record G._FieldDefinition [
      G._FieldDefinition_Description>>: var "desc",
      G._FieldDefinition_Name>>: (encodeFieldName @@ (Core.fieldTypeName $ var "ft")),
      G._FieldDefinition_ArgumentsDefinition>>: nothing,
      G._FieldDefinition_Type>>: var "gtype",
      G._FieldDefinition_Directives>>: nothing])

-- | Encode a literal type to a GraphQL NamedType
encodeLiteralType :: TTermDefinition (Context -> LiteralType -> Either Error G.NamedType)
encodeLiteralType = define "encodeLiteralType" $
  "cx" ~> lambda "lt" $
    cases _LiteralType (var "lt")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError $ Strings.cat2 (string "Expected GraphQL-compatible literal type, found: ") (ShowCore.literalType @@ var "lt")) (var "cx")) [
      _LiteralType_boolean>>: constant $
        right (wrap G._NamedType (wrap G._Name (string "Boolean"))),
      _LiteralType_float>>: lambda "ft_" $
        cases _FloatType (var "ft_")
          (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError $ Strings.cat2 (string "Expected 64-bit float type, found: ") (ShowCore.floatType @@ var "ft_")) (var "cx")) [
          _FloatType_float64>>: constant $
            right (wrap G._NamedType (wrap G._Name (string "Float")))],
      _LiteralType_integer>>: lambda "it_" $
        cases _IntegerType (var "it_")
          (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError $ Strings.cat2 (string "Expected 32-bit signed integer type, found: ") (ShowCore.integerType @@ var "it_")) (var "cx")) [
          _IntegerType_int32>>: constant $
            right (wrap G._NamedType (wrap G._Name (string "Int")))],
      _LiteralType_string>>: constant $
        right (wrap G._NamedType (wrap G._Name (string "String")))]

-- | Encode a named type to a GraphQL type definition.
encodeNamedType :: TTermDefinition (Context -> Graph -> M.Map Namespace String -> Name -> Type -> Either Error G.TypeDefinition)
encodeNamedType = define "encodeNamedType" $
  "cx" ~> "g" ~> lambda "prefixes" $ lambda "name" $ lambda "typ" $
    cases _Type (Strip.deannotateType @@ var "typ")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError $ Strings.cat2 (string "Expected record or union type, found: ") (ShowCore.type_ @@ var "typ")) (var "cx")) [
      _Type_record>>: lambda "rt" $
        "gfields" <<~ (Eithers.mapList (lambda "f" $ encodeFieldType @@ var "cx" @@ var "g" @@ var "prefixes" @@ var "f") (var "rt")) $
        "desc" <<~ (descriptionFromType @@ var "cx" @@ var "g" @@ var "typ") $
        right (inject G._TypeDefinition G._TypeDefinition_object $ record G._ObjectTypeDefinition [
          G._ObjectTypeDefinition_Description>>: var "desc",
          G._ObjectTypeDefinition_Name>>: (encodeTypeName @@ var "prefixes" @@ var "name"),
          G._ObjectTypeDefinition_ImplementsInterfaces>>: nothing,
          G._ObjectTypeDefinition_Directives>>: nothing,
          G._ObjectTypeDefinition_FieldsDefinition>>: just (wrap G._FieldsDefinition (var "gfields"))]),
      _Type_union>>: lambda "rt" $
        Logic.ifElse (Predicates.isEnumRowType @@ var "rt")
          -- Pure enum: all variants are unit-typed
          ("values" <<~ (Eithers.mapList (lambda "f" $ encodeEnumFieldType @@ var "cx" @@ var "g" @@ var "f") (var "rt")) $
           "desc" <<~ (descriptionFromType @@ var "cx" @@ var "g" @@ var "typ") $
           right (inject G._TypeDefinition G._TypeDefinition_enum $ record G._EnumTypeDefinition [
             G._EnumTypeDefinition_Description>>: var "desc",
             G._EnumTypeDefinition_Name>>: (encodeTypeName @@ var "prefixes" @@ var "name"),
             G._EnumTypeDefinition_Directives>>: nothing,
             G._EnumTypeDefinition_EnumValuesDefinition>>: just (wrap G._EnumValuesDefinition (var "values"))]))
          -- Data-carrying union: encode as object type with nullable fields (one per variant)
          ("gfields" <<~ (Eithers.mapList (lambda "f" $ encodeUnionFieldType @@ var "cx" @@ var "g" @@ var "prefixes" @@ var "f") (var "rt")) $
           "desc" <<~ (descriptionFromType @@ var "cx" @@ var "g" @@ var "typ") $
           right (inject G._TypeDefinition G._TypeDefinition_object $ record G._ObjectTypeDefinition [
             G._ObjectTypeDefinition_Description>>: var "desc",
             G._ObjectTypeDefinition_Name>>: (encodeTypeName @@ var "prefixes" @@ var "name"),
             G._ObjectTypeDefinition_ImplementsInterfaces>>: nothing,
             G._ObjectTypeDefinition_Directives>>: nothing,
             G._ObjectTypeDefinition_FieldsDefinition>>: just (wrap G._FieldsDefinition (var "gfields"))])),
      _Type_either>>: lambda "et" $
        encodeNamedType @@ var "cx" @@ var "g" @@ var "prefixes" @@ var "name" @@
          (inject _Type _Type_record $ list [
            Core.fieldType (Core.name $ string "left") (MetaTypes.optional (project _EitherType _EitherType_left @@ var "et")),
            Core.fieldType (Core.name $ string "right") (MetaTypes.optional (project _EitherType _EitherType_right @@ var "et"))]),
      _Type_pair>>: lambda "pt" $
        encodeNamedType @@ var "cx" @@ var "g" @@ var "prefixes" @@ var "name" @@
          (inject _Type _Type_record $ list [
            Core.fieldType (Core.name $ string "first") (project _PairType _PairType_first @@ var "pt"),
            Core.fieldType (Core.name $ string "second") (project _PairType _PairType_second @@ var "pt")]),
      _Type_list>>: lambda "lt_" $
        wrapAsRecord (var "name") (var "cx") (var "g") (var "prefixes") (inject _Type _Type_list (var "lt_")),
      _Type_set>>: lambda "st" $
        wrapAsRecord (var "name") (var "cx") (var "g") (var "prefixes") (inject _Type _Type_list (var "st")),
      _Type_map>>: lambda "mt" $
        encodeNamedType @@ var "cx" @@ var "g" @@ var "prefixes" @@ var "name" @@
          (inject _Type _Type_record $ list [
            Core.fieldType (Core.name $ string "key") (Core.mapTypeKeys (var "mt")),
            Core.fieldType (Core.name $ string "value") (Core.mapTypeValues (var "mt"))]),
      _Type_literal>>: lambda "lt_" $
        wrapAsRecord (var "name") (var "cx") (var "g") (var "prefixes") (inject _Type _Type_literal (var "lt_")),
      _Type_variable>>: lambda "vn" $
        wrapAsRecord (var "name") (var "cx") (var "g") (var "prefixes") (inject _Type _Type_variable (var "vn")),
      _Type_wrap>>: lambda "wt" $
        wrapAsRecord (var "name") (var "cx") (var "g") (var "prefixes") (var "wt"),
      _Type_unit>>: constant $
        wrapAsRecord (var "name") (var "cx") (var "g") (var "prefixes") (inject _Type _Type_literal (inject _LiteralType _LiteralType_boolean unit)),
      -- Forall: strip the quantifier and encode the body type
      _Type_forall>>: lambda "ft" $
        encodeNamedType @@ var "cx" @@ var "g" @@ var "prefixes" @@ var "name" @@ (Core.forallTypeBody (var "ft")),
      -- Type application: use the function type (strip the argument)
      _Type_application>>: lambda "at" $
        encodeNamedType @@ var "cx" @@ var "g" @@ var "prefixes" @@ var "name" @@ (Core.applicationTypeFunction (var "at")),
      -- Function types: encode as a record with domain and codomain fields
      _Type_function>>: lambda "ft" $
        encodeNamedType @@ var "cx" @@ var "g" @@ var "prefixes" @@ var "name" @@
          (inject _Type _Type_record $ list [
            Core.fieldType (Core.name $ string "domain") (Core.functionTypeDomain (var "ft")),
            Core.fieldType (Core.name $ string "codomain") (Core.functionTypeCodomain (var "ft"))])]

-- | Encode a Hydra type as a GraphQL type reference
encodeType :: TTermDefinition (Context -> Graph -> M.Map Namespace String -> Type -> Either Error G.Type)
encodeType = define "encodeType" $
  "cx" ~> "g" ~> lambda "prefixes" $ lambda "typ" $
    cases _Type (Strip.deannotateType @@ var "typ")
      (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError $ Strings.cat2 (string "Expected GraphQL-compatible type, found: ") (ShowCore.type_ @@ var "typ")) (var "cx")) [
      _Type_maybe>>: lambda "et" $
        cases _Type (Strip.deannotateType @@ var "et")
          (Just $ Ctx.failInContext (Error.errorOther $ Error.otherError $ Strings.cat2 (string "Expected GraphQL-compatible type, found: ") (ShowCore.type_ @@ var "et")) (var "cx")) [
          _Type_list>>: lambda "et2" $
            Eithers.map (lambda "gt" $ inject G._Type G._Type_list (wrap G._ListType (var "gt")))
              (encodeType @@ var "cx" @@ var "g" @@ var "prefixes" @@ var "et2"),
          _Type_set>>: lambda "st" $
            Eithers.map (lambda "gt" $ inject G._Type G._Type_list (wrap G._ListType (var "gt")))
              (encodeType @@ var "cx" @@ var "g" @@ var "prefixes" @@ var "st"),
          _Type_map>>: lambda "mt" $
            Eithers.map (lambda "gt" $ inject G._Type G._Type_list (wrap G._ListType (var "gt")))
              (encodeType @@ var "cx" @@ var "g" @@ var "prefixes" @@ (Core.mapTypeValues (var "mt"))),
          _Type_literal>>: lambda "lt_" $
            Eithers.map (lambda "nt" $ inject G._Type G._Type_named (var "nt"))
              (encodeLiteralType @@ var "cx" @@ var "lt_"),
          _Type_pair>>: constant $
            right (inject G._Type G._Type_named (wrap G._NamedType (encodeTypeName @@ var "prefixes" @@ (Core.name $ string "hydra.util.Pair")))),
          _Type_either>>: constant $
            right (inject G._Type G._Type_named (wrap G._NamedType (encodeTypeName @@ var "prefixes" @@ (Core.name $ string "hydra.util.Either")))),
          _Type_record>>: constant $
            Ctx.failInContext (Error.errorOther $ Error.otherError (string "unexpected anonymous record type")) (var "cx"),
          _Type_union>>: constant $
            Ctx.failInContext (Error.errorOther $ Error.otherError (string "unexpected anonymous union type")) (var "cx"),
          _Type_wrap>>: lambda "wt" $
            encodeType @@ var "cx" @@ var "g" @@ var "prefixes" @@ (MetaTypes.optional (var "wt")),
          _Type_variable>>: lambda "n" $
            right (inject G._Type G._Type_named (wrap G._NamedType (encodeTypeName @@ var "prefixes" @@ (var "n")))),
          _Type_forall>>: lambda "ft" $
            encodeType @@ var "cx" @@ var "g" @@ var "prefixes" @@ (MetaTypes.optional (Core.forallTypeBody (var "ft"))),
          _Type_application>>: lambda "at" $
            encodeType @@ var "cx" @@ var "g" @@ var "prefixes" @@ (MetaTypes.optional (Core.applicationTypeFunction (var "at"))),
          _Type_function>>: constant $
            right (inject G._Type G._Type_named (wrap G._NamedType (wrap G._Name (string "String")))),
          _Type_unit>>: constant $
            right (inject G._Type G._Type_named (wrap G._NamedType (wrap G._Name (string "Boolean"))))],
      -- Non-optional types become non-null
      _Type_list>>: lambda "et" $
        Eithers.map (lambda "gt" $ inject G._Type G._Type_nonNull (inject G._NonNullType G._NonNullType_list (wrap G._ListType (var "gt"))))
          (encodeType @@ var "cx" @@ var "g" @@ var "prefixes" @@ var "et"),
      _Type_set>>: lambda "st" $
        Eithers.map (lambda "gt" $ inject G._Type G._Type_nonNull (inject G._NonNullType G._NonNullType_list (wrap G._ListType (var "gt"))))
          (encodeType @@ var "cx" @@ var "g" @@ var "prefixes" @@ var "st"),
      _Type_map>>: lambda "mt" $
        Eithers.map (lambda "gt" $ inject G._Type G._Type_nonNull (inject G._NonNullType G._NonNullType_list (wrap G._ListType (var "gt"))))
          (encodeType @@ var "cx" @@ var "g" @@ var "prefixes" @@ (Core.mapTypeValues (var "mt"))),
      _Type_literal>>: lambda "lt_" $
        Eithers.map (lambda "nt" $ inject G._Type G._Type_nonNull (inject G._NonNullType G._NonNullType_named (var "nt")))
          (encodeLiteralType @@ var "cx" @@ var "lt_"),
      _Type_pair>>: constant $
        right (inject G._Type G._Type_nonNull (inject G._NonNullType G._NonNullType_named
          (wrap G._NamedType (encodeTypeName @@ var "prefixes" @@ (Core.name $ string "hydra.util.Pair"))))),
      _Type_either>>: constant $
        right (inject G._Type G._Type_nonNull (inject G._NonNullType G._NonNullType_named
          (wrap G._NamedType (encodeTypeName @@ var "prefixes" @@ (Core.name $ string "hydra.util.Either"))))),
      _Type_record>>: constant $
        Ctx.failInContext (Error.errorOther $ Error.otherError (string "unexpected anonymous record type")) (var "cx"),
      _Type_union>>: constant $
        Ctx.failInContext (Error.errorOther $ Error.otherError (string "unexpected anonymous union type")) (var "cx"),
      _Type_variable>>: lambda "n" $
        right (inject G._Type G._Type_nonNull (inject G._NonNullType G._NonNullType_named
          (wrap G._NamedType (encodeTypeName @@ var "prefixes" @@ (var "n"))))),
      _Type_wrap>>: lambda "wt" $
        encodeType @@ var "cx" @@ var "g" @@ var "prefixes" @@ var "wt",
      _Type_forall>>: lambda "ft" $
        encodeType @@ var "cx" @@ var "g" @@ var "prefixes" @@ (Core.forallTypeBody (var "ft")),
      _Type_application>>: lambda "at" $
        encodeType @@ var "cx" @@ var "g" @@ var "prefixes" @@ (Core.applicationTypeFunction (var "at")),
      _Type_function>>: lambda "ft" $
        right (inject G._Type G._Type_nonNull (inject G._NonNullType G._NonNullType_named
          (wrap G._NamedType (wrap G._Name (string "String"))))),
      _Type_unit>>: constant $
        right (inject G._Type G._Type_nonNull (inject G._NonNullType G._NonNullType_named
          (wrap G._NamedType (wrap G._Name (string "Boolean")))))]

-- | Encode a TypeDefinition to a GraphQL TypeDefinition
encodeTypeDefinition :: TTermDefinition (Context -> Graph -> M.Map Namespace String -> TypeDefinition -> Either Error G.TypeDefinition)
encodeTypeDefinition = define "encodeTypeDefinition" $
  "cx" ~> "g" ~> lambda "prefixes" $ lambda "tdef" $
    encodeNamedType @@ var "cx" @@ var "g" @@ var "prefixes"
      @@ (Packaging.typeDefinitionName $ var "tdef")
      @@ (Core.typeSchemeBody $ Packaging.typeDefinitionTypeScheme $ var "tdef")

-- | Encode a Hydra Name as a GraphQL Name with namespace prefix
encodeTypeName :: TTermDefinition (M.Map Namespace String -> Name -> G.Name)
encodeTypeName = define "encodeTypeName" $
  lambda "prefixes" $ lambda "name" $ lets [
    "qualName">: Names.qualifyName @@ var "name",
    "local">: Packaging.qualifiedNameLocal (var "qualName"),
    "mns">: Packaging.qualifiedNameNamespace (var "qualName"),
    "prefix">: Maybes.maybe (string "")
      (lambda "ns_" $ Maybes.maybe (string "") ("p" ~> var "p") (Maps.lookup (var "ns_") (var "prefixes")))
      (var "mns")] $
    wrap G._Name (Strings.cat2 (var "prefix") (sanitize @@ var "local"))

-- | Encode a union variant field type to a nullable GraphQL FieldDefinition.
-- Unit-typed variants become Boolean fields; data-carrying variants use their actual type, made nullable.
encodeUnionFieldType :: TTermDefinition (Context -> Graph -> M.Map Namespace String -> FieldType -> Either Error G.FieldDefinition)
encodeUnionFieldType = define "encodeUnionFieldType" $
  "cx" ~> "g" ~> lambda "prefixes" $ lambda "ft" $ lets [
    "innerType">: Core.fieldTypeType $ var "ft",
    "isUnit">: Predicates.isUnitType @@ (Strip.deannotateType @@ var "innerType"),
    -- Unit variants use nullable Boolean; data-carrying variants use Maybe<innerType>
    "effectiveType">: Logic.ifElse (var "isUnit")
      (MetaTypes.optional (inject _Type _Type_literal (inject _LiteralType _LiteralType_boolean unit)))
      (MetaTypes.optional (var "innerType"))] $
    "gtype" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "prefixes" @@ var "effectiveType") $
    "desc" <<~ (descriptionFromType @@ var "cx" @@ var "g" @@ var "innerType") $
    right (record G._FieldDefinition [
      G._FieldDefinition_Description>>: var "desc",
      G._FieldDefinition_Name>>: (encodeFieldName @@ (Core.fieldTypeName $ var "ft")),
      G._FieldDefinition_ArgumentsDefinition>>: nothing,
      G._FieldDefinition_Type>>: var "gtype",
      G._FieldDefinition_Directives>>: nothing])

-- | Top-level entry point: convert a module to GraphQL schema files.
moduleToGraphql :: TTermDefinition (Module -> [Definition] -> Context -> Graph -> Either Error (M.Map FilePath String))
moduleToGraphql = define "moduleToGraphql" $
  lambda "mod" $ lambda "defs" $
    "cx" ~> "g" ~> lets [
    "partitioned">: Environment.partitionDefinitions @@ var "defs",
    "typeDefs">: Pairs.first (var "partitioned"),
    "prefixes">: findPrefixes @@ Packaging.moduleNamespace (var "mod") @@ var "typeDefs",
    "filePath">: Names.namespaceToFilePath @@ Util.caseConventionCamel @@ (wrap _FileExtension (string "graphql")) @@ Packaging.moduleNamespace (var "mod")] $
    "gtdefs" <<~ (Eithers.mapList (lambda "td" $ encodeTypeDefinition @@ var "cx" @@ var "g" @@ var "prefixes" @@ var "td") (var "typeDefs")) $
    right (Maps.fromList $ Lists.pure $ pair (var "filePath")
      (Serialization.printExpr @@ (Serialization.parenthesize @@
        (GraphqlSerde.exprDocument @@ (wrap G._Document
          (Lists.map
            (lambda "gtdef" $
              inject G._Definition G._Definition_typeSystem
                (inject G._TypeSystemDefinitionOrExtension G._TypeSystemDefinitionOrExtension_definition
                  (inject G._TypeSystemDefinition G._TypeSystemDefinition_type (var "gtdef"))))
            (var "gtdefs")))))))

-- | Sanitize a string for use as a GraphQL identifier
sanitize :: TTermDefinition (String -> String)
sanitize = define "sanitize" $
  lambda "s" $ Formatting.sanitizeWithUnderscores @@ GraphqlLanguage.graphqlReservedWords @@ var "s"
