-- Note: this file was created with the help of a large language model. It requires further human review.

module Hydra.Ext.Sources.Cpp.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Errors                      as Error
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.CoderUtils                  as CoderUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Serialization  as SerializationSource
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import           Prelude hiding ((++))

import qualified Data.Map as M
import qualified Data.Set as S

-- Additional imports for Cpp AST
import qualified Hydra.Ext.Cpp.Syntax as Cpp
import qualified Hydra.Ext.Sources.Cpp.Serde as CppSerde
import qualified Hydra.Ext.Sources.Cpp.Syntax as CppSyntax
import qualified Hydra.Ext.Sources.Cpp.Language as CppLanguageSource


def :: String -> TTerm a -> TTermDefinition a
def = definitionInModule module_


-- | An empty list term, avoiding ambiguous type variable issues with 'list []'
emptyList :: TTerm [a]
emptyList = TTerm $ TermList []

ns :: Namespace
ns = Namespace "hydra.ext.cpp.coder"

module_ :: Module
module_ = Module ns elements
    [moduleNamespace CppLanguageSource.module_,
      CppSerde.ns,
      Formatting.ns, Names.ns, Rewriting.ns, Schemas.ns, Lexical.ns,
      ShowCore.ns, Annotations.ns, Sorting.ns, SerializationSource.ns,
      moduleNamespace DecodeCore.module_, moduleNamespace EncodeCore.module_]
    (CppSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "C++ code generator: converts Hydra modules to C++ header files"
  where
    elements = [
      -- Cpp AST helper functions
      toDefinition cppClassDeclaration,
      toDefinition cppEnumDeclaration,
      toDefinition cppEnumForwardDeclaration,
      toDefinition memberSpecificationPublic,
      toDefinition memberSpecificationProtected,
      toDefinition createTemplateType,
      toDefinition createIdentifierExpr,
      toDefinition createLiteralBoolExpr,
      toDefinition createLiteralIntExpr,
      toDefinition createFunctionCallExpr,
      toDefinition createHeaderFile,
      toDefinition createConstructorBody,
      toDefinition constParameter,
      toDefinition unnamedParameter,
      toDefinition toConstType,
      toDefinition createTypeIdNameCall,
      toDefinition createThrowStmt,
      -- Expression chain helpers
      toDefinition cppPrimaryExpressionToCppExpression,
      toDefinition cppUnaryExpressionToCppExpression,
      toDefinition cppPostfixExpressionToCppExpression,
      toDefinition cppUnaryExpressionToCppLogicalOrExpression,
      -- Naming functions
      toDefinition encodeName,
      toDefinition encodeNamespace,
      toDefinition encodeFieldName,
      toDefinition encodeEnumValue,
      toDefinition sanitizeCppName,
      toDefinition className,
      toDefinition variantName,
      toDefinition visitorName,
      toDefinition partialVisitorName,
      toDefinition fwdHeaderName,
      toDefinition namespaceDecl,
      toDefinition createTypeReference,
      -- Type encoding
      toDefinition encodeLiteralType,
      toDefinition encodeType,
      toDefinition encodeForallType,
      toDefinition encodeFunctionType,
      toDefinition encodeApplicationType,
      toDefinition encodeTypeAlias,
      toDefinition encodeTypeDefinition,
      -- Struct/union/enum encoding
      toDefinition encodeFieldType,
      toDefinition encodeRecordType,
      toDefinition encodeUnionType,
      toDefinition encodeEnumType,
      toDefinition encodeVariantType,
      toDefinition encodeWrappedType,
      -- Visitor pattern
      toDefinition createVisitorInterface,
      toDefinition createPartialVisitorInterface,
      toDefinition createUnionBaseClass,
      toDefinition createVariantClass,
      toDefinition createAcceptImplementation,
      toDefinition generateForwardDeclarations,
      toDefinition createLessThanOperator,
      -- File generation
      toDefinition serializeHeaderFile,
      toDefinition bindingNameToFilePath,
      toDefinition findIncludes,
      toDefinition findTypeDependencies,
      toDefinition gatherMetadata,
      toDefinition isStdContainerType,
      toDefinition isStructType,
      toDefinition isTemplateType,
      -- Module entry point
      toDefinition generateTypeFile,
      toDefinition generateTypeFiles,
      toDefinition moduleToCpp]


-- ============================================================================
-- Cpp AST helper functions
-- ============================================================================

-- | Create a class declaration
cppClassDeclaration :: TTermDefinition (String -> [Cpp.BaseSpecifier] -> Maybe Cpp.ClassBody -> Cpp.Declaration)
cppClassDeclaration = def "cppClassDeclaration" $
  lambda "name" $ lambda "baseSpecs" $ lambda "mbody" $
    inject Cpp._Declaration Cpp._Declaration_class $
      record Cpp._ClassDeclaration [
        Cpp._ClassDeclaration_specifier>>:
          record Cpp._ClassSpecifier [
            Cpp._ClassSpecifier_key>>: inject Cpp._ClassKey Cpp._ClassKey_class unit,
            Cpp._ClassSpecifier_name>>: var "name",
            Cpp._ClassSpecifier_inheritance>>: var "baseSpecs"],
        Cpp._ClassDeclaration_body>>: var "mbody"]

-- | Create an enum class declaration
cppEnumDeclaration :: TTermDefinition (String -> Maybe Cpp.ClassBody -> Cpp.Declaration)
cppEnumDeclaration = def "cppEnumDeclaration" $
  lambda "name" $ lambda "mbody" $
    inject Cpp._Declaration Cpp._Declaration_class $
      record Cpp._ClassDeclaration [
        Cpp._ClassDeclaration_specifier>>:
          record Cpp._ClassSpecifier [
            Cpp._ClassSpecifier_key>>: inject Cpp._ClassKey Cpp._ClassKey_enumClass unit,
            Cpp._ClassSpecifier_name>>: var "name",
            Cpp._ClassSpecifier_inheritance>>: list ([] :: [TTerm Cpp.BaseSpecifier])],
        Cpp._ClassDeclaration_body>>: var "mbody"]

-- | Create an enum forward declaration (no body)
cppEnumForwardDeclaration :: TTermDefinition (String -> Cpp.Declaration)
cppEnumForwardDeclaration = def "cppEnumForwardDeclaration" $
  lambda "name" $
    cppEnumDeclaration @@ var "name" @@ nothing

-- | Public access label
memberSpecificationPublic :: TTermDefinition Cpp.MemberSpecification
memberSpecificationPublic = def "memberSpecificationPublic" $
  inject Cpp._MemberSpecification Cpp._MemberSpecification_accessLabel $
    inject Cpp._AccessSpecifier Cpp._AccessSpecifier_public unit

-- | Protected access label
memberSpecificationProtected :: TTermDefinition Cpp.MemberSpecification
memberSpecificationProtected = def "memberSpecificationProtected" $
  inject Cpp._MemberSpecification Cpp._MemberSpecification_accessLabel $
    inject Cpp._AccessSpecifier Cpp._AccessSpecifier_protected unit

-- | Create a template type (e.g., std::vector<T>)
createTemplateType :: TTermDefinition (String -> [Cpp.TypeExpression] -> Cpp.TypeExpression)
createTemplateType = def "createTemplateType" $
  lambda "name" $ lambda "args" $
    inject Cpp._TypeExpression Cpp._TypeExpression_template $
      record Cpp._TemplateType [
        Cpp._TemplateType_name>>: var "name",
        Cpp._TemplateType_arguments>>:
          Lists.map (lambda "a" $
            inject Cpp._TemplateArgument Cpp._TemplateArgument_type (var "a"))
          (var "args")]

-- | Create an identifier expression
createIdentifierExpr :: TTermDefinition (String -> Cpp.Expression)
createIdentifierExpr = def "createIdentifierExpr" $
  lambda "name" $
    cppPrimaryExpressionToCppExpression @@
      (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (var "name"))

-- | Create a boolean literal expression
createLiteralBoolExpr :: TTermDefinition (Bool -> Cpp.Expression)
createLiteralBoolExpr = def "createLiteralBoolExpr" $
  lambda "val" $
    cppPrimaryExpressionToCppExpression @@
      (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_literal $
        inject Cpp._Literal Cpp._Literal_boolean $
          wrap Cpp._BooleanLiteral (var "val"))

-- | Create an integer literal expression
createLiteralIntExpr :: TTermDefinition (Int -> Cpp.Expression)
createLiteralIntExpr = def "createLiteralIntExpr" $
  lambda "val" $
    cppPrimaryExpressionToCppExpression @@
      (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_literal $
        inject Cpp._Literal Cpp._Literal_integer $
          inject Cpp._IntegerLiteral Cpp._IntegerLiteral_decimal (var "val"))

-- | Create a function call expression
createFunctionCallExpr :: TTermDefinition (String -> [Cpp.Expression] -> Cpp.Expression)
createFunctionCallExpr = def "createFunctionCallExpr" $
  lambda "funcName" $ lambda "args" $
    cppPostfixExpressionToCppExpression @@
      (inject Cpp._PostfixExpression Cpp._PostfixExpression_functionCall $
        record Cpp._FunctionCallOperation [
          Cpp._FunctionCallOperation_function>>:
            inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
              inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (var "funcName"),
          Cpp._FunctionCallOperation_arguments>>: var "args"])

-- | Create a header file with pragma once, includes, and declarations
createHeaderFile :: TTermDefinition ([Cpp.IncludeDirective] -> [Cpp.Declaration] -> Cpp.Program)
createHeaderFile = def "createHeaderFile" $
  lambda "includes" $ lambda "decls" $
    record Cpp._Program [
      Cpp._Program_preprocessorDirectives>>: list [
        inject Cpp._PreprocessorDirective Cpp._PreprocessorDirective_pragma $
          record Cpp._PragmaDirective [Cpp._PragmaDirective_content>>: string "once"]],
      Cpp._Program_includes>>: var "includes",
      Cpp._Program_declarations>>: var "decls"]

-- | Create function body: default if no params, empty compound otherwise
createConstructorBody :: TTermDefinition ([Cpp.Parameter] -> Cpp.FunctionBody)
createConstructorBody = def "createConstructorBody" $
  lambda "params" $
    Logic.ifElse
      (Lists.null (var "params"))
      (inject Cpp._FunctionBody Cpp._FunctionBody_default unit)
      (inject Cpp._FunctionBody Cpp._FunctionBody_compound $
        wrap Cpp._CompoundStatement (list ([] :: [TTerm Cpp.Statement])))

-- | Create a const reference parameter
constParameter :: TTermDefinition (String -> Cpp.TypeExpression -> Cpp.Parameter)
constParameter = def "constParameter" $
  lambda "name" $ lambda "typ" $
    record Cpp._Parameter [
      Cpp._Parameter_type>>:
        inject Cpp._TypeExpression Cpp._TypeExpression_qualified $
          record Cpp._QualifiedType [
            Cpp._QualifiedType_baseType>>:
              inject Cpp._TypeExpression Cpp._TypeExpression_qualified $
                record Cpp._QualifiedType [
                  Cpp._QualifiedType_baseType>>: var "typ",
                  Cpp._QualifiedType_qualifier>>: inject Cpp._TypeQualifier Cpp._TypeQualifier_const unit],
            Cpp._QualifiedType_qualifier>>: inject Cpp._TypeQualifier Cpp._TypeQualifier_lvalueRef unit],
      Cpp._Parameter_name>>: var "name",
      Cpp._Parameter_unnamed>>: boolean False,
      Cpp._Parameter_defaultValue>>: nothing]

-- | Create an unnamed parameter (for operator overloads, etc.)
unnamedParameter :: TTermDefinition (String -> Cpp.TypeExpression -> Cpp.Parameter)
unnamedParameter = def "unnamedParameter" $
  lambda "name" $ lambda "typ" $
    record Cpp._Parameter [
      Cpp._Parameter_type>>: var "typ",
      Cpp._Parameter_name>>: var "name",
      Cpp._Parameter_unnamed>>: boolean True,
      Cpp._Parameter_defaultValue>>: nothing]

-- | Wrap a type expression with const qualifier
toConstType :: TTermDefinition (Cpp.TypeExpression -> Cpp.TypeExpression)
toConstType = def "toConstType" $
  lambda "baseType" $
    inject Cpp._TypeExpression Cpp._TypeExpression_qualified $
      record Cpp._QualifiedType [
        Cpp._QualifiedType_baseType>>: var "baseType",
        Cpp._QualifiedType_qualifier>>: inject Cpp._TypeQualifier Cpp._TypeQualifier_const unit]

-- | Create a typeid(*this).name() call expression
createTypeIdNameCall :: TTermDefinition Cpp.Expression
createTypeIdNameCall = def "createTypeIdNameCall" $
  cppPostfixExpressionToCppExpression @@
    (inject Cpp._PostfixExpression Cpp._PostfixExpression_functionCall $
      record Cpp._FunctionCallOperation [
        Cpp._FunctionCallOperation_function>>:
          inject Cpp._PostfixExpression Cpp._PostfixExpression_memberAccess $
            record Cpp._MemberAccessOperation [
              Cpp._MemberAccessOperation_object>>:
                inject Cpp._PostfixExpression Cpp._PostfixExpression_functionCall $
                  record Cpp._FunctionCallOperation [
                    Cpp._FunctionCallOperation_function>>:
                      inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
                        inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (string "typeid"),
                    Cpp._FunctionCallOperation_arguments>>: list [
                      cppPrimaryExpressionToCppExpression @@
                        (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_parenthesized $
                          cppPostfixExpressionToCppExpression @@
                            (inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
                              inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (string "*this")))]],
              Cpp._MemberAccessOperation_member>>: string "name"],
        Cpp._FunctionCallOperation_arguments>>: list ([] :: [TTerm Cpp.Expression])])

-- | Create a throw statement
createThrowStmt :: TTermDefinition (String -> Cpp.Expression -> Cpp.Statement)
createThrowStmt = def "createThrowStmt" $
  lambda "exceptionType" $ lambda "arg" $
    inject Cpp._Statement Cpp._Statement_jump $
      inject Cpp._JumpStatement Cpp._JumpStatement_throw $
        createFunctionCallExpr @@ var "exceptionType" @@ list [var "arg"]


-- ============================================================================
-- Expression chain helpers
-- ============================================================================

-- | Convert a PrimaryExpression to a full Expression
cppPrimaryExpressionToCppExpression :: TTermDefinition (Cpp.PrimaryExpression -> Cpp.Expression)
cppPrimaryExpressionToCppExpression = def "cppPrimaryExpressionToCppExpression" $
  lambda "prim" $
    cppPostfixExpressionToCppExpression @@
      (inject Cpp._PostfixExpression Cpp._PostfixExpression_primary (var "prim"))

-- | Convert a UnaryExpression to a full Expression
cppUnaryExpressionToCppExpression :: TTermDefinition (Cpp.UnaryExpression -> Cpp.Expression)
cppUnaryExpressionToCppExpression = def "cppUnaryExpressionToCppExpression" $
  lambda "ue" $
    inject Cpp._Expression Cpp._Expression_assignment $
      inject Cpp._AssignmentExpression Cpp._AssignmentExpression_conditional $
        inject Cpp._ConditionalExpression Cpp._ConditionalExpression_logicalOr $
          cppUnaryExpressionToCppLogicalOrExpression @@ var "ue"

-- | Convert a PostfixExpression to a full Expression
cppPostfixExpressionToCppExpression :: TTermDefinition (Cpp.PostfixExpression -> Cpp.Expression)
cppPostfixExpressionToCppExpression = def "cppPostfixExpressionToCppExpression" $
  lambda "pe" $
    cppUnaryExpressionToCppExpression @@
      (inject Cpp._UnaryExpression Cpp._UnaryExpression_postfix (var "pe"))

-- | Convert a UnaryExpression to a LogicalOrExpression
cppUnaryExpressionToCppLogicalOrExpression :: TTermDefinition (Cpp.UnaryExpression -> Cpp.LogicalOrExpression)
cppUnaryExpressionToCppLogicalOrExpression = def "cppUnaryExpressionToCppLogicalOrExpression" $
  lambda "ue" $
    inject Cpp._LogicalOrExpression Cpp._LogicalOrExpression_logicalAnd $
      inject Cpp._LogicalAndExpression Cpp._LogicalAndExpression_inclusiveOr $
        inject Cpp._InclusiveOrExpression Cpp._InclusiveOrExpression_exclusiveOr $
          inject Cpp._ExclusiveOrExpression Cpp._ExclusiveOrExpression_and $
            inject Cpp._AndExpression Cpp._AndExpression_equality $
              inject Cpp._EqualityExpression Cpp._EqualityExpression_relational $
                inject Cpp._RelationalExpression Cpp._RelationalExpression_shift $
                  inject Cpp._ShiftExpression Cpp._ShiftExpression_additive $
                    inject Cpp._AdditiveExpression Cpp._AdditiveExpression_multiplicative $
                      inject Cpp._MultiplicativeExpression Cpp._MultiplicativeExpression_unary (var "ue")


-- ============================================================================
-- Naming functions
-- ============================================================================

-- | Encode a name with specified convention and optional qualification
encodeName :: TTermDefinition (Bool -> CaseConvention -> x -> Name -> String)
encodeName = def "encodeName" $
  doc "Encode a name with a specified case convention, optionally qualified" $
  lambda "isQualified" $ lambda "conv" $ lambda "env" $ lambda "name" $
    -- Simplified: use local name with sanitization
    sanitizeCppName @@ (Names.localNameOf @@ var "name")

-- | Encode a namespace as a C++ namespace string (e.g., "hydra.ext.cpp" -> "hydra::ext::cpp")
encodeNamespace :: TTermDefinition (Namespace -> String)
encodeNamespace = def "encodeNamespace" $
  lambda "ns" $
    Strings.intercalate (string "::")
      (Lists.map
        (lambda "seg" $ Formatting.convertCaseCamelToLowerSnake @@ var "seg")
        (Strings.splitOn (string ".") (Module.unNamespace (var "ns"))))

-- | Encode a field name in lower_snake_case
encodeFieldName :: TTermDefinition (Name -> String)
encodeFieldName = def "encodeFieldName" $
  lambda "fname" $
    sanitizeCppName @@ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (var "fname"))

-- | Encode an enum value in UPPER_SNAKE_CASE
encodeEnumValue :: TTermDefinition (Name -> String)
encodeEnumValue = def "encodeEnumValue" $
  lambda "fname" $
    sanitizeCppName @@ (Formatting.convertCaseCamelToUpperSnake @@ Core.unName (var "fname"))

-- | Sanitize a name to be valid in C++
sanitizeCppName :: TTermDefinition (String -> String)
sanitizeCppName = def "sanitizeCppName" $
  lambda "name" $
    Formatting.sanitizeWithUnderscores @@ (asTerm CppLanguageSource.cppReservedWords) @@ var "name"

-- | Get the class name from a fully qualified Name
className :: TTermDefinition (Name -> String)
className = def "className" $
  lambda "name" $
    sanitizeCppName @@ (Names.localNameOf @@ var "name")

-- | Construct a variant class name (e.g., "MyType" + "field" -> "MyTypeField")
variantName :: TTermDefinition (Name -> Name -> String)
variantName = def "variantName" $
  lambda "tname" $ lambda "fname" $
    sanitizeCppName @@ (
      (Names.localNameOf @@ var "tname")
      ++ (Formatting.capitalize @@ Core.unName (var "fname")))

-- | Construct a visitor interface name
visitorName :: TTermDefinition (Name -> String)
visitorName = def "visitorName" $
  lambda "name" $
    sanitizeCppName @@ ((Names.localNameOf @@ var "name") ++ string "Visitor")

-- | Construct a partial visitor interface name
partialVisitorName :: TTermDefinition (Name -> String)
partialVisitorName = def "partialVisitorName" $
  lambda "name" $
    sanitizeCppName @@ ((Names.localNameOf @@ var "name") ++ string "PartialVisitor")

-- | Construct the forward-declaration header name for a namespace
fwdHeaderName :: TTermDefinition (Namespace -> Name)
fwdHeaderName = def "fwdHeaderName" $
  lambda "ns" $
    Names.unqualifyName @@
      (record _QualifiedName [
        _QualifiedName_namespace>>: just (var "ns"),
        _QualifiedName_local>>: string "Fwd"])

-- | Create a namespace declaration wrapping declarations
namespaceDecl :: TTermDefinition (Namespace -> [Cpp.Declaration] -> Cpp.Declaration)
namespaceDecl = def "namespaceDecl" $
  lambda "ns" $ lambda "decls" $
    inject Cpp._Declaration Cpp._Declaration_namespace $
      record Cpp._NamespaceDeclaration [
        Cpp._NamespaceDeclaration_name>>: encodeNamespace @@ var "ns",
        Cpp._NamespaceDeclaration_declarations>>: var "decls"]

-- | Create a type reference, optionally as a shared_ptr for struct types
createTypeReference :: TTermDefinition (Bool -> Name -> Cpp.TypeExpression)
createTypeReference = def "createTypeReference" $
  lambda "isPointer" $ lambda "name" $
    Logic.ifElse (var "isPointer")
      (createTemplateType @@ string "std::shared_ptr" @@
        list [toConstType @@ (inject Cpp._TypeExpression Cpp._TypeExpression_basic $
          inject Cpp._BasicType Cpp._BasicType_named (sanitizeCppName @@ (Names.localNameOf @@ var "name")))])
      (inject Cpp._TypeExpression Cpp._TypeExpression_basic $
        inject Cpp._BasicType Cpp._BasicType_named (sanitizeCppName @@ (Names.localNameOf @@ var "name")))


-- ============================================================================
-- Type encoding
-- ============================================================================

-- | Encode a literal type as a C++ type expression
encodeLiteralType :: TTermDefinition (LiteralType -> Cpp.TypeExpression)
encodeLiteralType = def "encodeLiteralType" $
  lambda "lt" $
    inject Cpp._TypeExpression Cpp._TypeExpression_basic $
      cases _LiteralType (var "lt") Nothing [
        _LiteralType_binary>>: constant $
          inject Cpp._BasicType Cpp._BasicType_char unit,
        _LiteralType_boolean>>: constant $
          inject Cpp._BasicType Cpp._BasicType_bool unit,
        _LiteralType_float>>: lambda "ft" $
          cases _FloatType (var "ft") (Just $ inject Cpp._BasicType Cpp._BasicType_double unit) [
            _FloatType_float32>>: constant $ inject Cpp._BasicType Cpp._BasicType_float unit,
            _FloatType_float64>>: constant $ inject Cpp._BasicType Cpp._BasicType_double unit],
        _LiteralType_integer>>: lambda "it" $
          cases _IntegerType (var "it") (Just $ inject Cpp._BasicType Cpp._BasicType_int unit) [
            _IntegerType_bigint>>: constant $ inject Cpp._BasicType Cpp._BasicType_int unit,
            _IntegerType_int8>>: constant $ inject Cpp._BasicType Cpp._BasicType_char unit,
            _IntegerType_int16>>: constant $ inject Cpp._BasicType Cpp._BasicType_named (string "int16_t"),
            _IntegerType_int32>>: constant $ inject Cpp._BasicType Cpp._BasicType_int unit,
            _IntegerType_int64>>: constant $ inject Cpp._BasicType Cpp._BasicType_named (string "int64_t")],
        _LiteralType_string>>: constant $
          inject Cpp._BasicType Cpp._BasicType_string unit]

-- | Encode a Hydra type as a C++ type expression
encodeType :: TTermDefinition (Context -> Graph -> Type -> Either (InContext Error) Cpp.TypeExpression)
encodeType = def "encodeType" $
  "cx" ~> "g" ~> lambda "typ" $
    "t" <~ (Rewriting.deannotateType @@ var "typ") $
    cases _Type (var "t") (Just $
      Ctx.failInContext (Error.errorOther $ Error.otherError $ string "Unsupported type") (var "cx"))
    [_Type_application>>: lambda "at" $
       encodeApplicationType @@ var "cx" @@ var "g" @@ var "at",
     _Type_either>>: lambda "et" $
       "lt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.eitherTypeLeft (var "et")) $
       "rt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.eitherTypeRight (var "et")) $
         right (toConstType @@ (createTemplateType @@ string "std::variant" @@ list [var "lt", var "rt"])),
     _Type_function>>: lambda "ft" $
       encodeFunctionType @@ var "cx" @@ var "g" @@ var "ft",
     _Type_forall>>: lambda "lt" $
       encodeForallType @@ var "cx" @@ var "g" @@ var "lt",
     _Type_list>>: lambda "et" $
       Eithers.map (lambda "enc" $ toConstType @@ (createTemplateType @@ string "std::vector" @@ list [var "enc"]))
         (encodeType @@ var "cx" @@ var "g" @@ var "et"),
     _Type_map>>: lambda "mt" $
       "kt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.mapTypeKeys (var "mt")) $
       "vt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.mapTypeValues (var "mt")) $
         right (toConstType @@ (createTemplateType @@ string "std::map" @@ list [var "kt", var "vt"])),
     _Type_literal>>: lambda "lt" $
       right (encodeLiteralType @@ var "lt"),
     _Type_maybe>>: lambda "et" $
       Eithers.map (lambda "enc" $ toConstType @@ (createTemplateType @@ string "std::optional" @@ list [var "enc"]))
         (encodeType @@ var "cx" @@ var "g" @@ var "et"),
     _Type_pair>>: lambda "pt" $
       "ft" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.pairTypeFirst (var "pt")) $
       "st" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.pairTypeSecond (var "pt")) $
         right (toConstType @@ (createTemplateType @@ string "std::pair" @@ list [var "ft", var "st"])),
     _Type_record>>: lambda "rt" $
       Ctx.failInContext (Error.errorOther $ Error.otherError (string "unexpected anonymous record type")) (var "cx"),
     _Type_set>>: lambda "et" $
       Eithers.map (lambda "enc" $ toConstType @@ (createTemplateType @@ string "std::set" @@ list [var "enc"]))
         (encodeType @@ var "cx" @@ var "g" @@ var "et"),
     _Type_union>>: lambda "rt" $
       Ctx.failInContext (Error.errorOther $ Error.otherError (string "unexpected anonymous union type")) (var "cx"),
     _Type_variable>>: lambda "name" $
       right (inject Cpp._TypeExpression Cpp._TypeExpression_basic $
         inject Cpp._BasicType Cpp._BasicType_named (sanitizeCppName @@ Core.unName (var "name"))),
     _Type_wrap>>: lambda "wt" $
       Ctx.failInContext (Error.errorOther $ Error.otherError (string "unexpected anonymous wrapped type")) (var "cx"),
     _Type_unit>>: constant $
       right (createTemplateType @@ string "std::tuple" @@ list ([] :: [TTerm Cpp.TypeExpression]))]

-- | Encode a forall type (strip the quantifier)
encodeForallType :: TTermDefinition (Context -> Graph -> ForallType -> Either (InContext Error) Cpp.TypeExpression)
encodeForallType = def "encodeForallType" $
  "cx" ~> "g" ~> lambda "lt" $
    encodeType @@ var "cx" @@ var "g" @@ Core.forallTypeBody (var "lt")

-- | Encode a function type as std::function<R(Args...)>
encodeFunctionType :: TTermDefinition (Context -> Graph -> FunctionType -> Either (InContext Error) Cpp.TypeExpression)
encodeFunctionType = def "encodeFunctionType" $
  "cx" ~> "g" ~> lambda "ft" $
    "dom" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.functionTypeDomain (var "ft")) $
    "cod" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.functionTypeCodomain (var "ft")) $
      right (inject Cpp._TypeExpression Cpp._TypeExpression_function $
        record Cpp._FunctionType [
          Cpp._FunctionType_returnType>>: var "cod",
          Cpp._FunctionType_parameters>>: list [
            record Cpp._Parameter [
              Cpp._Parameter_type>>: var "dom",
              Cpp._Parameter_name>>: string "",
              Cpp._Parameter_unnamed>>: boolean False,
              Cpp._Parameter_defaultValue>>: nothing]]])

-- | Encode a type application (template instantiation)
encodeApplicationType :: TTermDefinition (Context -> Graph -> ApplicationType -> Either (InContext Error) Cpp.TypeExpression)
encodeApplicationType = def "encodeApplicationType" $
  "cx" ~> "g" ~> lambda "at" $
    "body" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.applicationTypeFunction (var "at")) $
    "arg" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.applicationTypeArgument (var "at")) $
      right (createTemplateType @@ string "TODO_template" @@ list [var "body", var "arg"])

-- | Encode a type as a typedef / using declaration
encodeTypeAlias :: TTermDefinition (Context -> Graph -> Name -> Type -> Maybe String -> Either (InContext Error) Cpp.Declaration)
encodeTypeAlias = def "encodeTypeAlias" $
  "cx" ~> "g" ~> lambda "name" $ lambda "typ" $ lambda "comment" $
    "cppType" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "typ") $
      right (inject Cpp._Declaration Cpp._Declaration_typedef $
        record Cpp._TypedefDeclaration [
          Cpp._TypedefDeclaration_name>>: className @@ var "name",
          Cpp._TypedefDeclaration_type>>: var "cppType",
          Cpp._TypedefDeclaration_isUsing>>: boolean True])

-- | Encode a top-level type definition (dispatches to record/union/wrap)
encodeTypeDefinition :: TTermDefinition (Context -> Graph -> Name -> Type -> Either (InContext Error) [Cpp.Declaration])
encodeTypeDefinition = def "encodeTypeDefinition" $
  "cx" ~> "g" ~> lambda "name" $ lambda "typ" $
    "t" <~ (Rewriting.deannotateType @@ var "typ") $
    cases _Type (var "t") (Just $
      Ctx.failInContext (Error.errorOther $ Error.otherError $ string "unexpected type in definition: " ++ (ShowCore.type_ @@ var "typ")) (var "cx"))
    [_Type_forall>>: lambda "fa" $
       encodeTypeDefinition @@ var "cx" @@ var "g" @@ var "name" @@ Core.forallTypeBody (var "fa"),
     _Type_record>>: lambda "rt" $
       encodeRecordType @@ var "cx" @@ var "g" @@ var "name" @@ var "rt" @@ nothing,
     _Type_union>>: lambda "rt" $
       encodeUnionType @@ var "cx" @@ var "g" @@ var "name" @@ var "rt" @@ nothing,
     _Type_wrap>>: lambda "wt" $
       encodeWrappedType @@ var "cx" @@ var "g" @@ var "name" @@ var "wt" @@ nothing]


-- ============================================================================
-- Struct/union/enum encoding
-- ============================================================================

-- | Encode a field type as a VariableDeclaration
encodeFieldType :: TTermDefinition (Bool -> FieldType -> Context -> Graph -> Either (InContext Error) Cpp.VariableDeclaration)
encodeFieldType = def "encodeFieldType" $
  lambda "isParameter" $ lambda "ft" $ "cx" ~> lambda "g" $
    "fname" <~ Core.fieldTypeName (var "ft") $
    "ftype" <~ Core.fieldTypeType (var "ft") $
    "cppType" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "ftype") $
      right (record Cpp._VariableDeclaration [
        Cpp._VariableDeclaration_type>>: just (var "cppType"),
        Cpp._VariableDeclaration_name>>: encodeFieldName @@ var "fname",
        Cpp._VariableDeclaration_initializer>>: nothing,
        Cpp._VariableDeclaration_isAuto>>: boolean False])

-- | Encode a record type as a C++ class with fields and constructor
encodeRecordType :: TTermDefinition (Context -> Graph -> Name -> [FieldType] -> Maybe String -> Either (InContext Error) [Cpp.Declaration])
encodeRecordType = def "encodeRecordType" $
  "cx" ~> "g" ~> lambda "name" $ lambda "rt" $ lambda "comment" $
    "cppFields" <<~ (Eithers.mapList (lambda "f" $ encodeFieldType @@ boolean False @@ var "f" @@ var "cx" @@ var "g") (var "rt")) $
    "constructorParams" <<~ (Eithers.mapList (lambda "f" $ encodeFieldType @@ boolean True @@ var "f" @@ var "cx" @@ var "g") (var "rt")) $
      let classDecl = cppClassDeclaration @@ (className @@ var "name") @@ list ([] :: [TTerm Cpp.BaseSpecifier]) @@
            (just (wrap Cpp._ClassBody (
              Lists.concat (list [
                list [asTerm memberSpecificationPublic],
                Lists.map (lambda "field" $
                  inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
                    inject Cpp._MemberDeclaration Cpp._MemberDeclaration_variable (var "field"))
                  (var "cppFields"),
                list [inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
                  inject Cpp._MemberDeclaration Cpp._MemberDeclaration_constructor $
                    record Cpp._ConstructorDeclaration [
                      Cpp._ConstructorDeclaration_name>>: className @@ var "name",
                      Cpp._ConstructorDeclaration_parameters>>:
                        Lists.map (lambda "p" $
                          record Cpp._Parameter [
                            Cpp._Parameter_type>>: Maybes.fromMaybe
                              (inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                                inject Cpp._BasicType Cpp._BasicType_int unit)
                              ((project Cpp._VariableDeclaration Cpp._VariableDeclaration_type @@ var "p")),
                            Cpp._Parameter_name>>: (project Cpp._VariableDeclaration Cpp._VariableDeclaration_name @@ var "p"),
                            Cpp._Parameter_unnamed>>: boolean False,
                            Cpp._Parameter_defaultValue>>: nothing])
                          (var "constructorParams"),
                      Cpp._ConstructorDeclaration_initializers>>:
                        Lists.map (lambda "field" $
                          record Cpp._MemInitializer [
                            Cpp._MemInitializer_name>>: (project Cpp._VariableDeclaration Cpp._VariableDeclaration_name @@ var "field"),
                            Cpp._MemInitializer_arguments>>: list [
                              createIdentifierExpr @@ (project Cpp._VariableDeclaration Cpp._VariableDeclaration_name @@ var "field")]])
                          (var "cppFields"),
                      Cpp._ConstructorDeclaration_body>>: createConstructorBody @@ var "constructorParams"]]]
              ))))
          ltOp = createLessThanOperator @@ var "name" @@ var "rt"
      in right (list [classDecl, ltOp])

-- | Encode a union type (dispatches to enum or variant based on content)
encodeUnionType :: TTermDefinition (Context -> Graph -> Name -> [FieldType] -> Maybe String -> Either (InContext Error) [Cpp.Declaration])
encodeUnionType = def "encodeUnionType" $
  "cx" ~> "g" ~> lambda "name" $ lambda "rt" $ lambda "comment" $
    Logic.ifElse (Schemas.isEnumRowType @@ var "rt")
      (encodeEnumType @@ var "cx" @@ var "g" @@ var "name" @@ var "rt" @@ var "comment")
      (encodeVariantType @@ var "cx" @@ var "g" @@ var "name" @@ var "rt" @@ var "comment")

-- | Encode an enum type as a C++ enum class
encodeEnumType :: TTermDefinition (Context -> Graph -> Name -> [FieldType] -> Maybe String -> Either (InContext Error) [Cpp.Declaration])
encodeEnumType = def "encodeEnumType" $
  "cx" ~> "g" ~> lambda "name" $ lambda "tfields" $ lambda "comment" $
    right (list [
      cppEnumDeclaration @@ (className @@ var "name") @@
        (just (wrap Cpp._ClassBody (
          Lists.map (lambda "ft" $
            inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
              inject Cpp._MemberDeclaration Cpp._MemberDeclaration_variable $
                record Cpp._VariableDeclaration [
                  Cpp._VariableDeclaration_type>>: nothing,
                  Cpp._VariableDeclaration_name>>: encodeEnumValue @@ Core.fieldTypeName (var "ft"),
                  Cpp._VariableDeclaration_initializer>>: nothing,
                  Cpp._VariableDeclaration_isAuto>>: boolean False])
          (var "tfields"))))])

-- | Encode a variant (tagged union) type as a class hierarchy with visitor pattern
encodeVariantType :: TTermDefinition (Context -> Graph -> Name -> [FieldType] -> Maybe String -> Either (InContext Error) [Cpp.Declaration])
encodeVariantType = def "encodeVariantType" $
  "cx" ~> "g" ~> lambda "name" $ lambda "variants" $ lambda "comment" $
    "variantClasses" <<~ (Eithers.mapList
      (lambda "v" $ createVariantClass @@ var "cx" @@ var "g" @@ var "name" @@ var "name" @@ var "v")
      (var "variants")) $
      right (Lists.concat (list [
        generateForwardDeclarations @@ var "name" @@ var "variants",
        list [createVisitorInterface @@ var "name" @@ var "variants"],
        list [createUnionBaseClass @@ var "name" @@ var "variants"],
        var "variantClasses",
        list [createPartialVisitorInterface @@ var "name" @@ var "variants"],
        list [createAcceptImplementation @@ var "name" @@ var "variants"]]))

-- | Encode a wrapped type as a single-field record
encodeWrappedType :: TTermDefinition (Context -> Graph -> Name -> Type -> Maybe String -> Either (InContext Error) [Cpp.Declaration])
encodeWrappedType = def "encodeWrappedType" $
  "cx" ~> "g" ~> lambda "name" $ lambda "typ" $ lambda "comment" $
    encodeRecordType @@ var "cx" @@ var "g" @@ var "name"
      @@ list [
            record _FieldType [
              _FieldType_name>>: wrap _Name (string "value"),
              _FieldType_type>>: var "typ"]]
      @@ var "comment"


-- ============================================================================
-- Visitor pattern
-- ============================================================================

-- | Create a visitor interface with pure virtual visit methods
createVisitorInterface :: TTermDefinition (Name -> [FieldType] -> Cpp.Declaration)
createVisitorInterface = def "createVisitorInterface" $
  lambda "tname" $ lambda "variants" $
    inject Cpp._Declaration Cpp._Declaration_template $
      record Cpp._TemplateDeclaration [
        Cpp._TemplateDeclaration_inline>>: boolean False,
        Cpp._TemplateDeclaration_parameters>>: list [string "typename R"],
        Cpp._TemplateDeclaration_declaration>>:
          cppClassDeclaration @@ (visitorName @@ var "tname") @@ list ([] :: [TTerm Cpp.BaseSpecifier]) @@
            (just (wrap Cpp._ClassBody (
              Lists.concat (list [
                list [asTerm memberSpecificationPublic],
                Lists.map (lambda "ft" $
                  "fname" <~ Core.fieldTypeName (var "ft") $
                  inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
                    inject Cpp._MemberDeclaration Cpp._MemberDeclaration_function $
                      record Cpp._FunctionDeclaration [
                        Cpp._FunctionDeclaration_prefixSpecifiers>>: list [
                          inject Cpp._FunctionSpecifierPrefix Cpp._FunctionSpecifierPrefix_virtual unit],
                        Cpp._FunctionDeclaration_returnType>>:
                          inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                            inject Cpp._BasicType Cpp._BasicType_named (string "R"),
                        Cpp._FunctionDeclaration_name>>: string "visit",
                        Cpp._FunctionDeclaration_parameters>>: list [
                          record Cpp._Parameter [
                            Cpp._Parameter_type>>:
                              inject Cpp._TypeExpression Cpp._TypeExpression_qualified $
                                record Cpp._QualifiedType [
                                  Cpp._QualifiedType_baseType>>:
                                    inject Cpp._TypeExpression Cpp._TypeExpression_qualified $
                                      record Cpp._QualifiedType [
                                        Cpp._QualifiedType_baseType>>:
                                          inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                                            inject Cpp._BasicType Cpp._BasicType_named (variantName @@ var "tname" @@ var "fname"),
                                        Cpp._QualifiedType_qualifier>>: inject Cpp._TypeQualifier Cpp._TypeQualifier_const unit],
                                  Cpp._QualifiedType_qualifier>>: inject Cpp._TypeQualifier Cpp._TypeQualifier_lvalueRef unit],
                            Cpp._Parameter_name>>: string "value",
                            Cpp._Parameter_unnamed>>: boolean False,
                            Cpp._Parameter_defaultValue>>: nothing]],
                        Cpp._FunctionDeclaration_suffixSpecifiers>>: list ([] :: [TTerm Cpp.FunctionSpecifierSuffix]),
                        Cpp._FunctionDeclaration_body>>: inject Cpp._FunctionBody Cpp._FunctionBody_pure unit])
                (var "variants"),
                list [inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
                  inject Cpp._MemberDeclaration Cpp._MemberDeclaration_destructor $
                    record Cpp._DestructorDeclaration [
                      Cpp._DestructorDeclaration_prefixSpecifiers>>: list [
                        inject Cpp._FunctionSpecifierPrefix Cpp._FunctionSpecifierPrefix_virtual unit],
                      Cpp._DestructorDeclaration_name>>: visitorName @@ var "tname",
                      Cpp._DestructorDeclaration_suffixSpecifiers>>: list ([] :: [TTerm Cpp.FunctionSpecifierSuffix]),
                      Cpp._DestructorDeclaration_body>>: inject Cpp._FunctionBody Cpp._FunctionBody_default unit]]]))))]

-- | Create a partial visitor interface with default visit methods that delegate to otherwise()
createPartialVisitorInterface :: TTermDefinition (Name -> [FieldType] -> Cpp.Declaration)
createPartialVisitorInterface = def "createPartialVisitorInterface" $
  lambda "tname" $ lambda "variants" $
    inject Cpp._Declaration Cpp._Declaration_template $
      record Cpp._TemplateDeclaration [
        Cpp._TemplateDeclaration_inline>>: boolean False,
        Cpp._TemplateDeclaration_parameters>>: list [string "typename R"],
        Cpp._TemplateDeclaration_declaration>>:
          cppClassDeclaration @@ (partialVisitorName @@ var "tname")
            @@ list [record Cpp._BaseSpecifier [
                 Cpp._BaseSpecifier_access>>: inject Cpp._AccessSpecifier Cpp._AccessSpecifier_public unit,
                 Cpp._BaseSpecifier_name>>: (visitorName @@ var "tname") ++ string "<R>"]]
            @@ (just (wrap Cpp._ClassBody (
              Lists.concat (list [
                list [asTerm memberSpecificationPublic],
                -- otherwise() pure virtual method
                list [inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
                  inject Cpp._MemberDeclaration Cpp._MemberDeclaration_function $
                    record Cpp._FunctionDeclaration [
                      Cpp._FunctionDeclaration_prefixSpecifiers>>: list [
                        inject Cpp._FunctionSpecifierPrefix Cpp._FunctionSpecifierPrefix_virtual unit],
                      Cpp._FunctionDeclaration_returnType>>:
                        inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                          inject Cpp._BasicType Cpp._BasicType_named (string "R"),
                      Cpp._FunctionDeclaration_name>>: string "otherwise",
                      Cpp._FunctionDeclaration_parameters>>: list [
                        constParameter @@ string "value" @@
                          (inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                            inject Cpp._BasicType Cpp._BasicType_named (className @@ var "tname"))],
                      Cpp._FunctionDeclaration_suffixSpecifiers>>: list [
                        inject Cpp._FunctionSpecifierSuffix Cpp._FunctionSpecifierSuffix_const unit],
                      Cpp._FunctionDeclaration_body>>: inject Cpp._FunctionBody Cpp._FunctionBody_pure unit]],
                -- Default visit methods delegating to otherwise
                Lists.map (lambda "ft" $
                  "fname" <~ Core.fieldTypeName (var "ft") $
                  inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
                    inject Cpp._MemberDeclaration Cpp._MemberDeclaration_function $
                      record Cpp._FunctionDeclaration [
                        Cpp._FunctionDeclaration_prefixSpecifiers>>: list ([] :: [TTerm Cpp.FunctionSpecifierPrefix]),
                        Cpp._FunctionDeclaration_returnType>>:
                          inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                            inject Cpp._BasicType Cpp._BasicType_named (string "R"),
                        Cpp._FunctionDeclaration_name>>: string "visit",
                        Cpp._FunctionDeclaration_parameters>>: list [
                          constParameter @@ string "value" @@
                            (inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                              inject Cpp._BasicType Cpp._BasicType_named (variantName @@ var "tname" @@ var "fname"))],
                        Cpp._FunctionDeclaration_suffixSpecifiers>>: list [
                          inject Cpp._FunctionSpecifierSuffix Cpp._FunctionSpecifierSuffix_override unit],
                        Cpp._FunctionDeclaration_body>>:
                          inject Cpp._FunctionBody Cpp._FunctionBody_compound $
                            wrap Cpp._CompoundStatement (list [
                              inject Cpp._Statement Cpp._Statement_jump $
                                inject Cpp._JumpStatement Cpp._JumpStatement_returnValue $
                                  createFunctionCallExpr @@ string "otherwise" @@ list [
                                    createIdentifierExpr @@ string "value"]])])
                (var "variants")]))))]


-- | Create the union base class with protected constructor, virtual destructor, and accept method
createUnionBaseClass :: TTermDefinition (Name -> [FieldType] -> Cpp.Declaration)
createUnionBaseClass = def "createUnionBaseClass" $
  lambda "name" $ lambda "variants" $
    cppClassDeclaration @@ (className @@ var "name") @@ list ([] :: [TTerm Cpp.BaseSpecifier]) @@
      (just (wrap Cpp._ClassBody (list [
        asTerm memberSpecificationProtected,
        -- Protected constructor
        inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
          inject Cpp._MemberDeclaration Cpp._MemberDeclaration_constructor $
            record Cpp._ConstructorDeclaration [
              Cpp._ConstructorDeclaration_name>>: className @@ var "name",
              Cpp._ConstructorDeclaration_parameters>>: list ([] :: [TTerm Cpp.Parameter]),
              Cpp._ConstructorDeclaration_initializers>>: list ([] :: [TTerm Cpp.MemInitializer]),
              Cpp._ConstructorDeclaration_body>>: inject Cpp._FunctionBody Cpp._FunctionBody_default unit],
        asTerm memberSpecificationPublic,
        -- Virtual destructor
        inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
          inject Cpp._MemberDeclaration Cpp._MemberDeclaration_destructor $
            record Cpp._DestructorDeclaration [
              Cpp._DestructorDeclaration_prefixSpecifiers>>: list [
                inject Cpp._FunctionSpecifierPrefix Cpp._FunctionSpecifierPrefix_virtual unit],
              Cpp._DestructorDeclaration_name>>: className @@ var "name",
              Cpp._DestructorDeclaration_suffixSpecifiers>>: list ([] :: [TTerm Cpp.FunctionSpecifierSuffix]),
              Cpp._DestructorDeclaration_body>>: inject Cpp._FunctionBody Cpp._FunctionBody_default unit],
        -- Template accept method
        inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
          inject Cpp._MemberDeclaration Cpp._MemberDeclaration_template $
            record Cpp._TemplateDeclaration [
              Cpp._TemplateDeclaration_inline>>: boolean False,
              Cpp._TemplateDeclaration_parameters>>: list [string "typename R"],
              Cpp._TemplateDeclaration_declaration>>:
                inject Cpp._Declaration Cpp._Declaration_function $
                  record Cpp._FunctionDeclaration [
                    Cpp._FunctionDeclaration_prefixSpecifiers>>: list ([] :: [TTerm Cpp.FunctionSpecifierPrefix]),
                    Cpp._FunctionDeclaration_returnType>>:
                      inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                        inject Cpp._BasicType Cpp._BasicType_named (string "R"),
                    Cpp._FunctionDeclaration_name>>: string "accept",
                    Cpp._FunctionDeclaration_parameters>>: list [
                      record Cpp._Parameter [
                        Cpp._Parameter_type>>:
                          inject Cpp._TypeExpression Cpp._TypeExpression_qualified $
                            record Cpp._QualifiedType [
                              Cpp._QualifiedType_baseType>>:
                                inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                                  inject Cpp._BasicType Cpp._BasicType_named (
                                    (visitorName @@ var "name") ++ string "<R>"),
                              Cpp._QualifiedType_qualifier>>: inject Cpp._TypeQualifier Cpp._TypeQualifier_lvalueRef unit],
                        Cpp._Parameter_name>>: string "visitor",
                        Cpp._Parameter_unnamed>>: boolean False,
                        Cpp._Parameter_defaultValue>>: nothing]],
                    Cpp._FunctionDeclaration_suffixSpecifiers>>: list [
                      inject Cpp._FunctionSpecifierSuffix Cpp._FunctionSpecifierSuffix_const unit],
                    Cpp._FunctionDeclaration_body>>: inject Cpp._FunctionBody Cpp._FunctionBody_declaration unit]]])))

-- | Create a variant subclass (one branch of a union type)
createVariantClass :: TTermDefinition (Context -> Graph -> Name -> Name -> FieldType -> Either (InContext Error) Cpp.Declaration)
createVariantClass = def "createVariantClass" $
  "cx" ~> "g" ~> lambda "tname" $ lambda "parentClass" $ lambda "ft" $
    "fname" <~ Core.fieldTypeName (var "ft") $
    "variantType" <~ Core.fieldTypeType (var "ft") $
    "hasValue" <~ Logic.not (Schemas.isUnitType @@ var "variantType") $
    "valueField" <~ Logic.ifElse (var "hasValue")
      (Eithers.map (lambda "cppType" $
        list [inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
          inject Cpp._MemberDeclaration Cpp._MemberDeclaration_variable $
            record Cpp._VariableDeclaration [
              Cpp._VariableDeclaration_type>>: just (var "cppType"),
              Cpp._VariableDeclaration_name>>: string "value",
              Cpp._VariableDeclaration_initializer>>: nothing,
              Cpp._VariableDeclaration_isAuto>>: boolean False]])
        (encodeType @@ var "cx" @@ var "g" @@ (Rewriting.deannotateType @@ var "variantType")))
      (right (list ([] :: [TTerm Cpp.MemberSpecification]))) $
    "constructorParams" <~ Logic.ifElse (var "hasValue")
      (Eithers.map (lambda "paramType" $
        list [record Cpp._Parameter [
          Cpp._Parameter_type>>: var "paramType",
          Cpp._Parameter_name>>: string "value",
          Cpp._Parameter_unnamed>>: boolean False,
          Cpp._Parameter_defaultValue>>: nothing]])
        (encodeType @@ var "cx" @@ var "g" @@ (Rewriting.deannotateType @@ var "variantType")))
      (right (list ([] :: [TTerm Cpp.Parameter]))) $
    "vFields" <<~ (var "valueField") $
    "vParams" <<~ (var "constructorParams") $
    "initList" <~ Logic.ifElse (var "hasValue")
      (list [record Cpp._MemInitializer [
        Cpp._MemInitializer_name>>: string "value",
        Cpp._MemInitializer_arguments>>: list [createIdentifierExpr @@ string "value"]]])
      (list ([] :: [TTerm Cpp.MemInitializer])) $
      right (cppClassDeclaration
        @@ (variantName @@ var "tname" @@ var "fname")
        @@ list [record Cpp._BaseSpecifier [
             Cpp._BaseSpecifier_access>>: inject Cpp._AccessSpecifier Cpp._AccessSpecifier_public unit,
             Cpp._BaseSpecifier_name>>: className @@ var "parentClass"]]
        @@ (just (wrap Cpp._ClassBody (
          Lists.concat (list [
            list [asTerm memberSpecificationPublic],
            var "vFields",
            list [inject Cpp._MemberSpecification Cpp._MemberSpecification_member $
              inject Cpp._MemberDeclaration Cpp._MemberDeclaration_constructor $
                record Cpp._ConstructorDeclaration [
                  Cpp._ConstructorDeclaration_name>>: variantName @@ var "tname" @@ var "fname",
                  Cpp._ConstructorDeclaration_parameters>>: var "vParams",
                  Cpp._ConstructorDeclaration_initializers>>: var "initList",
                  Cpp._ConstructorDeclaration_body>>: createConstructorBody @@ var "vParams"]]])))))

-- | Create the accept() method implementation using dynamic_cast chain
createAcceptImplementation :: TTermDefinition (Name -> [FieldType] -> Cpp.Declaration)
createAcceptImplementation = def "createAcceptImplementation" $
  lambda "tname" $ lambda "variants" $
    inject Cpp._Declaration Cpp._Declaration_template $
      record Cpp._TemplateDeclaration [
        Cpp._TemplateDeclaration_inline>>: boolean False,
        Cpp._TemplateDeclaration_parameters>>: list [string "typename R"],
        Cpp._TemplateDeclaration_declaration>>:
          inject Cpp._Declaration Cpp._Declaration_function $
            record Cpp._FunctionDeclaration [
              Cpp._FunctionDeclaration_prefixSpecifiers>>: list ([] :: [TTerm Cpp.FunctionSpecifierPrefix]),
              Cpp._FunctionDeclaration_returnType>>:
                inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                  inject Cpp._BasicType Cpp._BasicType_named (string "R"),
              Cpp._FunctionDeclaration_name>>: (className @@ var "tname") ++ string "::accept",
              Cpp._FunctionDeclaration_parameters>>: list [
                record Cpp._Parameter [
                  Cpp._Parameter_type>>:
                    inject Cpp._TypeExpression Cpp._TypeExpression_qualified $
                      record Cpp._QualifiedType [
                        Cpp._QualifiedType_baseType>>:
                          inject Cpp._TypeExpression Cpp._TypeExpression_basic $
                            inject Cpp._BasicType Cpp._BasicType_named (
                              (visitorName @@ var "tname") ++ string "<R>"),
                        Cpp._QualifiedType_qualifier>>: inject Cpp._TypeQualifier Cpp._TypeQualifier_lvalueRef unit],
                  Cpp._Parameter_name>>: string "visitor",
                  Cpp._Parameter_unnamed>>: boolean False,
                  Cpp._Parameter_defaultValue>>: nothing]],
              Cpp._FunctionDeclaration_suffixSpecifiers>>: list [
                inject Cpp._FunctionSpecifierSuffix Cpp._FunctionSpecifierSuffix_const unit],
              Cpp._FunctionDeclaration_body>>:
                inject Cpp._FunctionBody Cpp._FunctionBody_compound $
                  wrap Cpp._CompoundStatement (
                    -- Generate dynamic_cast if-else chain for each variant
                    -- Each variant checks dynamic_cast and calls visitor.visit(*ptr)
                    -- The last branch throws std::runtime_error with typeid name
                    Lists.map (lambda "ft" $
                      "fname" <~ Core.fieldTypeName (var "ft") $
                      -- Create an if statement with dynamic_cast and visitor.visit call
                      inject Cpp._Statement Cpp._Statement_selection $
                        record Cpp._SelectionStatement [
                          Cpp._SelectionStatement_condition>>:
                            -- auto ptr = dynamic_cast<const VariantName*>(this)
                            inject Cpp._Expression Cpp._Expression_assignment $
                              inject Cpp._AssignmentExpression Cpp._AssignmentExpression_assignment $
                                record Cpp._ExplicitAssignment [
                                  Cpp._ExplicitAssignment_left>>:
                                    cppUnaryExpressionToCppLogicalOrExpression @@
                                      (inject Cpp._UnaryExpression Cpp._UnaryExpression_postfix $
                                        inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
                                          inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (string "auto ptr")),
                                  Cpp._ExplicitAssignment_op>>:
                                    inject Cpp._AssignmentOperator Cpp._AssignmentOperator_assign unit,
                                  Cpp._ExplicitAssignment_right>>:
                                    inject Cpp._AssignmentExpression Cpp._AssignmentExpression_conditional $
                                      inject Cpp._ConditionalExpression Cpp._ConditionalExpression_logicalOr $
                                        cppUnaryExpressionToCppLogicalOrExpression @@
                                          (inject Cpp._UnaryExpression Cpp._UnaryExpression_postfix $
                                            inject Cpp._PostfixExpression Cpp._PostfixExpression_functionCall $
                                              record Cpp._FunctionCallOperation [
                                                Cpp._FunctionCallOperation_function>>:
                                                  inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
                                                    inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (
                                                      string "dynamic_cast<const " ++ (variantName @@ var "tname" @@ var "fname") ++ string "*>"),
                                                Cpp._FunctionCallOperation_arguments>>: list [
                                                  cppPrimaryExpressionToCppExpression @@
                                                    (inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (string "this"))]])],
                          Cpp._SelectionStatement_thenBranch>>:
                            inject Cpp._Statement Cpp._Statement_compound $
                              wrap Cpp._CompoundStatement (list [
                                inject Cpp._Statement Cpp._Statement_jump $
                                  inject Cpp._JumpStatement Cpp._JumpStatement_returnValue $
                                    cppPostfixExpressionToCppExpression @@
                                      (inject Cpp._PostfixExpression Cpp._PostfixExpression_functionCall $
                                        record Cpp._FunctionCallOperation [
                                          Cpp._FunctionCallOperation_function>>:
                                            inject Cpp._PostfixExpression Cpp._PostfixExpression_memberAccess $
                                              record Cpp._MemberAccessOperation [
                                                Cpp._MemberAccessOperation_object>>:
                                                  inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
                                                    inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (string "visitor"),
                                                Cpp._MemberAccessOperation_member>>: string "visit"],
                                          Cpp._FunctionCallOperation_arguments>>: list [
                                            cppUnaryExpressionToCppExpression @@
                                              (inject Cpp._UnaryExpression Cpp._UnaryExpression_unaryOp $
                                                record Cpp._UnaryOperation [
                                                  Cpp._UnaryOperation_operator>>:
                                                    inject Cpp._UnaryOperator Cpp._UnaryOperator_dereference unit,
                                                  Cpp._UnaryOperation_operand>>:
                                                    inject Cpp._UnaryExpression Cpp._UnaryExpression_postfix $
                                                      inject Cpp._PostfixExpression Cpp._PostfixExpression_primary $
                                                        inject Cpp._PrimaryExpression Cpp._PrimaryExpression_identifier (string "ptr")])]])]),
                          Cpp._SelectionStatement_elseBranch>>:
                            just (createThrowStmt @@ string "std::runtime_error" @@ (asTerm createTypeIdNameCall))])
                    (var "variants"))]]

-- | Generate forward declarations for all variant subclasses
generateForwardDeclarations :: TTermDefinition (Name -> [FieldType] -> [Cpp.Declaration])
generateForwardDeclarations = def "generateForwardDeclarations" $
  lambda "tname" $ lambda "fields" $
    Lists.map (lambda "ft" $
      cppClassDeclaration @@ (variantName @@ var "tname" @@ Core.fieldTypeName (var "ft")) @@ list ([] :: [TTerm Cpp.BaseSpecifier]) @@ nothing)
    (var "fields")

-- | Create a less-than operator for a record type (trivial/placeholder implementation)
createLessThanOperator :: TTermDefinition (Name -> [FieldType] -> Cpp.Declaration)
createLessThanOperator = def "createLessThanOperator" $
  lambda "typeName" $ lambda "fields" $
    inject Cpp._Declaration Cpp._Declaration_function $
      record Cpp._FunctionDeclaration [
        Cpp._FunctionDeclaration_prefixSpecifiers>>: list [
          inject Cpp._FunctionSpecifierPrefix Cpp._FunctionSpecifierPrefix_inline unit],
        Cpp._FunctionDeclaration_returnType>>:
          inject Cpp._TypeExpression Cpp._TypeExpression_basic $
            inject Cpp._BasicType Cpp._BasicType_bool unit,
        Cpp._FunctionDeclaration_name>>: string "operator<",
        Cpp._FunctionDeclaration_parameters>>: list [
          unnamedParameter @@ string "lhs" @@
            (inject Cpp._TypeExpression Cpp._TypeExpression_basic $
              inject Cpp._BasicType Cpp._BasicType_named (className @@ var "typeName")),
          unnamedParameter @@ string "rhs" @@
            (inject Cpp._TypeExpression Cpp._TypeExpression_basic $
              inject Cpp._BasicType Cpp._BasicType_named (className @@ var "typeName"))],
        Cpp._FunctionDeclaration_suffixSpecifiers>>: list ([] :: [TTerm Cpp.FunctionSpecifierSuffix]),
        Cpp._FunctionDeclaration_body>>:
          inject Cpp._FunctionBody Cpp._FunctionBody_compound $
            wrap Cpp._CompoundStatement (list [
              inject Cpp._Statement Cpp._Statement_jump $
                inject Cpp._JumpStatement Cpp._JumpStatement_returnValue $
                  createLiteralBoolExpr @@ boolean False])]


-- ============================================================================
-- File generation
-- ============================================================================

-- | Serialize a header file from name, includes, and declarations
serializeHeaderFile :: TTermDefinition (Name -> [Cpp.IncludeDirective] -> [Cpp.Declaration] -> (FilePath, String))
serializeHeaderFile = def "serializeHeaderFile" $
  lambda "name" $ lambda "includes" $ lambda "decls" $
    pair
      (bindingNameToFilePath @@ var "name")
      (SerializationSource.printExpr @@
        (SerializationSource.parenthesize @@
          (TTerm (TermVariable (Name "hydra.ext.cpp.serde.encodeProgram"))
            @@ (createHeaderFile @@ var "includes" @@ var "decls"))))

-- | Convert a binding name to a file path
bindingNameToFilePath :: TTermDefinition (Name -> FilePath)
bindingNameToFilePath = def "bindingNameToFilePath" $
  lambda "name" $
    CoderUtils.nameToFilePath
      @@ (inject _CaseConvention _CaseConvention_lowerSnake unit)
      @@ (inject _CaseConvention _CaseConvention_lowerSnake unit)
      @@ wrap _FileExtension (string "h")
      @@ var "name"

-- | Find includes for a set of type definitions
findIncludes :: TTermDefinition (Bool -> Namespace -> [TypeDefinition] -> [Cpp.IncludeDirective])
findIncludes = def "findIncludes" $
  lambda "withFwd" $ lambda "ns" $ lambda "defs" $
    -- System includes based on metadata, plus domain includes for cross-namespace dependencies
    -- For simplicity, include the standard set that the staging code conditionally includes
    Lists.concat (list [
      list [
        record Cpp._IncludeDirective [
          Cpp._IncludeDirective_name>>: string "memory",
          Cpp._IncludeDirective_isSystem>>: boolean True],
        record Cpp._IncludeDirective [
          Cpp._IncludeDirective_name>>: string "stdexcept",
          Cpp._IncludeDirective_isSystem>>: boolean True]],
      -- Domain includes from cross-namespace type dependencies
      Lists.map (lambda "depName" $
        record Cpp._IncludeDirective [
          Cpp._IncludeDirective_name>>: bindingNameToFilePath @@ var "depName",
          Cpp._IncludeDirective_isSystem>>: boolean False])
        (findTypeDependencies @@ var "ns" @@ var "defs"),
      -- Fwd include if needed
      Logic.ifElse (var "withFwd")
        (list [record Cpp._IncludeDirective [
          Cpp._IncludeDirective_name>>: bindingNameToFilePath @@ (fwdHeaderName @@ var "ns"),
          Cpp._IncludeDirective_isSystem>>: boolean False]])
        (list ([] :: [TTerm Cpp.IncludeDirective]))])

-- | Find type dependencies that are in other namespaces
findTypeDependencies :: TTermDefinition (Namespace -> [TypeDefinition] -> [Name])
findTypeDependencies = def "findTypeDependencies" $
  lambda "ns" $ lambda "defs" $
    Lists.filter
      (lambda "n" $
        Logic.not (Equality.equal
          (Maybes.map (unaryFunction Module.unNamespace) (Names.namespaceOf @@ var "n"))
          (just (Module.unNamespace (var "ns")))))
      (Sets.toList (Lists.foldl
        (lambda "acc" $ lambda "d" $
          Sets.union (var "acc") (Rewriting.typeDependencyNames @@ boolean True @@ Module.typeDefinitionType (var "d")))
        (Sets.empty)
        (var "defs")))

-- | Gather metadata from definitions (simplified: always include common headers)
gatherMetadata :: TTermDefinition ([Definition] -> Bool)
gatherMetadata = def "gatherMetadata" $
  lambda "defs" $ boolean True

-- | Check whether a type maps to an STL container type
isStdContainerType :: TTermDefinition (Type -> Bool)
isStdContainerType = def "isStdContainerType" $
  lambda "typ" $
    "t" <~ (Rewriting.deannotateType @@ var "typ") $
    cases _Type (var "t") (Just $ boolean False)
    [_Type_application>>: lambda "at" $
       isStdContainerType @@ Core.applicationTypeFunction (var "at"),
     _Type_list>>: constant $ boolean True,
     _Type_map>>: constant $ boolean True,
     _Type_maybe>>: constant $ boolean True,
     _Type_set>>: constant $ boolean True]

-- | Check whether a type is a struct type (not a literal and not an enum)
isStructType :: TTermDefinition (Type -> Bool)
isStructType = def "isStructType" $
  lambda "rawType" $
    "t" <~ (Schemas.fullyStripType @@ var "rawType") $
    "isLiteral" <~ cases _Type (var "t") (Just $ boolean False)
      [_Type_literal>>: constant $ boolean True] $
    Logic.and
      (Logic.not (var "isLiteral"))
      (Logic.not (Schemas.isEnumType @@ var "rawType"))

-- | Check whether a type maps to a C++ template type (string or STL container)
isTemplateType :: TTermDefinition (Type -> Bool)
isTemplateType = def "isTemplateType" $
  lambda "typ" $
    "t" <~ (Rewriting.deannotateType @@ var "typ") $
    Logic.or
      (cases _Type (var "t") (Just $ boolean False)
        [_Type_literal>>: lambda "lt" $
          cases _LiteralType (var "lt") (Just $ boolean False)
            [_LiteralType_string>>: constant $ boolean True]])
      (isStdContainerType @@ var "typ")


-- ============================================================================
-- Module entry point
-- ============================================================================

-- | Generate a single type header file
generateTypeFile :: TTermDefinition (Namespace -> TypeDefinition -> Context -> Graph -> Either (InContext Error) (FilePath, String))
generateTypeFile = def "generateTypeFile" $
  lambda "ns" $ lambda "def_" $ "cx" ~> lambda "g" $
    "name" <~ Module.typeDefinitionName (var "def_") $
    "typ" <~ Module.typeDefinitionType (var "def_") $
    "decls" <<~ (encodeTypeDefinition @@ var "cx" @@ var "g" @@ var "name" @@ var "typ") $
    "includes" <~ (findIncludes @@ boolean True @@ var "ns" @@ list [var "def_"]) $
      right (serializeHeaderFile @@ var "name" @@ var "includes"
        @@ list [namespaceDecl @@ var "ns" @@ var "decls"])

-- | Generate all type header files for a module (fwd file + individual class files)
generateTypeFiles :: TTermDefinition (Namespace -> [TypeDefinition] -> Context -> Graph -> Either (InContext Error) [(FilePath, String)])
generateTypeFiles = def "generateTypeFiles" $
  lambda "ns" $ lambda "defs" $ "cx" ~> lambda "g" $
    "classFiles" <<~ (Eithers.mapList
      (lambda "d" $ generateTypeFile @@ var "ns" @@ var "d" @@ var "cx" @@ var "g")
      (var "defs")) $
      right (var "classFiles")

-- | Convert a module to C++ code files (entry point)
moduleToCpp :: TTermDefinition (Module -> [Definition] -> Context -> Graph -> Either (InContext Error) (M.Map FilePath String))
moduleToCpp = def "moduleToCpp" $
  lambda "mod" $ lambda "defs" $ "cx" ~> lambda "g" $
    "ns" <~ Module.moduleNamespace (var "mod") $
    "typeDefs" <~ Pairs.first (Schemas.partitionDefinitions @@ var "defs") $
    "typeFiles" <<~ (generateTypeFiles @@ var "ns" @@ var "typeDefs" @@ var "cx" @@ var "g") $
      right (Maps.fromList (var "typeFiles"))
