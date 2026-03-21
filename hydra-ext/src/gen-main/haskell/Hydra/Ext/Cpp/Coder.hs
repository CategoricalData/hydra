-- Note: this is an automatically generated file. Do not edit.

-- | C++ code generator: converts Hydra modules to C++ header files

module Hydra.Ext.Cpp.Coder where

import qualified Hydra.CoderUtils as CoderUtils
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Ext.Cpp.Language as Language
import qualified Hydra.Ext.Cpp.Serde as Serde
import qualified Hydra.Ext.Cpp.Syntax as Syntax
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

cppClassDeclaration :: String -> [Syntax.BaseSpecifier] -> Maybe Syntax.ClassBody -> Syntax.Declaration
cppClassDeclaration name baseSpecs mbody =
    Syntax.DeclarationClass (Syntax.ClassDeclaration {
      Syntax.classDeclarationSpecifier = Syntax.ClassSpecifier {
        Syntax.classSpecifierKey = Syntax.ClassKeyClass,
        Syntax.classSpecifierName = name,
        Syntax.classSpecifierInheritance = baseSpecs},
      Syntax.classDeclarationBody = mbody})

cppEnumDeclaration :: String -> Maybe Syntax.ClassBody -> Syntax.Declaration
cppEnumDeclaration name mbody =
    Syntax.DeclarationClass (Syntax.ClassDeclaration {
      Syntax.classDeclarationSpecifier = Syntax.ClassSpecifier {
        Syntax.classSpecifierKey = Syntax.ClassKeyEnumClass,
        Syntax.classSpecifierName = name,
        Syntax.classSpecifierInheritance = []},
      Syntax.classDeclarationBody = mbody})

cppEnumForwardDeclaration :: String -> Syntax.Declaration
cppEnumForwardDeclaration name = cppEnumDeclaration name Nothing

memberSpecificationPublic :: Syntax.MemberSpecification
memberSpecificationPublic = Syntax.MemberSpecificationAccessLabel Syntax.AccessSpecifierPublic

memberSpecificationProtected :: Syntax.MemberSpecification
memberSpecificationProtected = Syntax.MemberSpecificationAccessLabel Syntax.AccessSpecifierProtected

createTemplateType :: String -> [Syntax.TypeExpression] -> Syntax.TypeExpression
createTemplateType name args =
    Syntax.TypeExpressionTemplate (Syntax.TemplateType {
      Syntax.templateTypeName = name,
      Syntax.templateTypeArguments = (Lists.map (\a -> Syntax.TemplateArgumentType a) args)})

createIdentifierExpr :: String -> Syntax.Expression
createIdentifierExpr name = cppPrimaryExpressionToCppExpression (Syntax.PrimaryExpressionIdentifier name)

createLiteralBoolExpr :: Bool -> Syntax.Expression
createLiteralBoolExpr val =
    cppPrimaryExpressionToCppExpression (Syntax.PrimaryExpressionLiteral (Syntax.LiteralBoolean (Syntax.BooleanLiteral val)))

createLiteralIntExpr :: Integer -> Syntax.Expression
createLiteralIntExpr val =
    cppPrimaryExpressionToCppExpression (Syntax.PrimaryExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteralDecimal val)))

createFunctionCallExpr :: String -> [Syntax.Expression] -> Syntax.Expression
createFunctionCallExpr funcName args =
    cppPostfixExpressionToCppExpression (Syntax.PostfixExpressionFunctionCall (Syntax.FunctionCallOperation {
      Syntax.functionCallOperationFunction = (Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier funcName)),
      Syntax.functionCallOperationArguments = args}))

createHeaderFile :: [Syntax.IncludeDirective] -> [Syntax.Declaration] -> Syntax.Program
createHeaderFile includes decls =
    Syntax.Program {
      Syntax.programPreprocessorDirectives = [
        Syntax.PreprocessorDirectivePragma (Syntax.PragmaDirective {
          Syntax.pragmaDirectiveContent = "once"})],
      Syntax.programIncludes = includes,
      Syntax.programDeclarations = decls}

createConstructorBody :: [t0] -> Syntax.FunctionBody
createConstructorBody params =
    Logic.ifElse (Lists.null params) Syntax.FunctionBodyDefault (Syntax.FunctionBodyCompound (Syntax.CompoundStatement []))

constParameter :: String -> Syntax.TypeExpression -> Syntax.Parameter
constParameter name typ =
    Syntax.Parameter {
      Syntax.parameterType = (Syntax.TypeExpressionQualified (Syntax.QualifiedType {
        Syntax.qualifiedTypeBaseType = (Syntax.TypeExpressionQualified (Syntax.QualifiedType {
          Syntax.qualifiedTypeBaseType = typ,
          Syntax.qualifiedTypeQualifier = Syntax.TypeQualifierConst})),
        Syntax.qualifiedTypeQualifier = Syntax.TypeQualifierLvalueRef})),
      Syntax.parameterName = name,
      Syntax.parameterUnnamed = False,
      Syntax.parameterDefaultValue = Nothing}

unnamedParameter :: String -> Syntax.TypeExpression -> Syntax.Parameter
unnamedParameter name typ =
    Syntax.Parameter {
      Syntax.parameterType = typ,
      Syntax.parameterName = name,
      Syntax.parameterUnnamed = True,
      Syntax.parameterDefaultValue = Nothing}

toConstType :: Syntax.TypeExpression -> Syntax.TypeExpression
toConstType baseType =
    Syntax.TypeExpressionQualified (Syntax.QualifiedType {
      Syntax.qualifiedTypeBaseType = baseType,
      Syntax.qualifiedTypeQualifier = Syntax.TypeQualifierConst})

createTypeIdNameCall :: Syntax.Expression
createTypeIdNameCall =
    cppPostfixExpressionToCppExpression (Syntax.PostfixExpressionFunctionCall (Syntax.FunctionCallOperation {
      Syntax.functionCallOperationFunction = (Syntax.PostfixExpressionMemberAccess (Syntax.MemberAccessOperation {
        Syntax.memberAccessOperationObject = (Syntax.PostfixExpressionFunctionCall (Syntax.FunctionCallOperation {
          Syntax.functionCallOperationFunction = (Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "typeid")),
          Syntax.functionCallOperationArguments = [
            cppPrimaryExpressionToCppExpression (Syntax.PrimaryExpressionParenthesized (cppPostfixExpressionToCppExpression (Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "*this"))))]})),
        Syntax.memberAccessOperationMember = "name"})),
      Syntax.functionCallOperationArguments = []}))

createThrowStmt :: String -> Syntax.Expression -> Syntax.Statement
createThrowStmt exceptionType arg =
    Syntax.StatementJump (Syntax.JumpStatementThrow (createFunctionCallExpr exceptionType [
      arg]))

cppPrimaryExpressionToCppExpression :: Syntax.PrimaryExpression -> Syntax.Expression
cppPrimaryExpressionToCppExpression prim = cppPostfixExpressionToCppExpression (Syntax.PostfixExpressionPrimary prim)

cppUnaryExpressionToCppExpression :: Syntax.UnaryExpression -> Syntax.Expression
cppUnaryExpressionToCppExpression ue =
    Syntax.ExpressionAssignment (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionLogicalOr (cppUnaryExpressionToCppLogicalOrExpression ue)))

cppPostfixExpressionToCppExpression :: Syntax.PostfixExpression -> Syntax.Expression
cppPostfixExpressionToCppExpression pe = cppUnaryExpressionToCppExpression (Syntax.UnaryExpressionPostfix pe)

cppUnaryExpressionToCppLogicalOrExpression :: Syntax.UnaryExpression -> Syntax.LogicalOrExpression
cppUnaryExpressionToCppLogicalOrExpression ue =
    Syntax.LogicalOrExpressionLogicalAnd (Syntax.LogicalAndExpressionInclusiveOr (Syntax.InclusiveOrExpressionExclusiveOr (Syntax.ExclusiveOrExpressionAnd (Syntax.AndExpressionEquality (Syntax.EqualityExpressionRelational (Syntax.RelationalExpressionShift (Syntax.ShiftExpressionAdditive (Syntax.AdditiveExpressionMultiplicative (Syntax.MultiplicativeExpressionUnary ue)))))))))

-- | Encode a name with a specified case convention, optionally qualified
encodeName :: t0 -> t1 -> t2 -> Core.Name -> String
encodeName isQualified conv env name = sanitizeCppName (Names.localNameOf name)

encodeNamespace :: Module.Namespace -> String
encodeNamespace ns =
    Strings.intercalate "::" (Lists.map (\seg -> Formatting.convertCaseCamelToLowerSnake seg) (Strings.splitOn "." (Module.unNamespace ns)))

encodeFieldName :: Core.Name -> String
encodeFieldName fname = sanitizeCppName (Formatting.convertCaseCamelToLowerSnake (Core.unName fname))

encodeEnumValue :: Core.Name -> String
encodeEnumValue fname = sanitizeCppName (Formatting.convertCaseCamelToUpperSnake (Core.unName fname))

sanitizeCppName :: String -> String
sanitizeCppName name = Formatting.sanitizeWithUnderscores Language.cppReservedWords name

className :: Core.Name -> String
className name = sanitizeCppName (Names.localNameOf name)

variantName :: Core.Name -> Core.Name -> String
variantName tname fname = sanitizeCppName (Strings.cat2 (Names.localNameOf tname) (Formatting.capitalize (Core.unName fname)))

visitorName :: Core.Name -> String
visitorName name = sanitizeCppName (Strings.cat2 (Names.localNameOf name) "Visitor")

partialVisitorName :: Core.Name -> String
partialVisitorName name = sanitizeCppName (Strings.cat2 (Names.localNameOf name) "PartialVisitor")

fwdHeaderName :: Module.Namespace -> Core.Name
fwdHeaderName ns =
    Names.unqualifyName (Module.QualifiedName {
      Module.qualifiedNameNamespace = (Just ns),
      Module.qualifiedNameLocal = "Fwd"})

namespaceDecl :: Module.Namespace -> [Syntax.Declaration] -> Syntax.Declaration
namespaceDecl ns decls =
    Syntax.DeclarationNamespace (Syntax.NamespaceDeclaration {
      Syntax.namespaceDeclarationName = (encodeNamespace ns),
      Syntax.namespaceDeclarationDeclarations = decls})

createTypeReference :: Bool -> Core.Name -> Syntax.TypeExpression
createTypeReference isPointer name =
    Logic.ifElse isPointer (createTemplateType "std::shared_ptr" [
      toConstType (Syntax.TypeExpressionBasic (Syntax.BasicTypeNamed (sanitizeCppName (Names.localNameOf name))))]) (Syntax.TypeExpressionBasic (Syntax.BasicTypeNamed (sanitizeCppName (Names.localNameOf name))))

encodeLiteralType :: Core.LiteralType -> Syntax.TypeExpression
encodeLiteralType lt =
    Syntax.TypeExpressionBasic (case lt of
      Core.LiteralTypeBinary -> Syntax.BasicTypeChar
      Core.LiteralTypeBoolean -> Syntax.BasicTypeBool
      Core.LiteralTypeFloat v0 -> case v0 of
        Core.FloatTypeFloat32 -> Syntax.BasicTypeFloat
        Core.FloatTypeFloat64 -> Syntax.BasicTypeDouble
        _ -> Syntax.BasicTypeDouble
      Core.LiteralTypeInteger v0 -> case v0 of
        Core.IntegerTypeBigint -> Syntax.BasicTypeInt
        Core.IntegerTypeInt8 -> Syntax.BasicTypeChar
        Core.IntegerTypeInt16 -> Syntax.BasicTypeNamed "int16_t"
        Core.IntegerTypeInt32 -> Syntax.BasicTypeInt
        Core.IntegerTypeInt64 -> Syntax.BasicTypeNamed "int64_t"
        _ -> Syntax.BasicTypeInt
      Core.LiteralTypeString -> Syntax.BasicTypeString)

encodeType :: Context.Context -> t0 -> Core.Type -> Either (Context.InContext Errors.Error) Syntax.TypeExpression
encodeType cx g typ =

      let t = Rewriting.deannotateType typ
      in case t of
        Core.TypeApplication v0 -> encodeApplicationType cx g v0
        Core.TypeEither v0 -> Eithers.bind (encodeType cx g (Core.eitherTypeLeft v0)) (\lt -> Eithers.bind (encodeType cx g (Core.eitherTypeRight v0)) (\rt -> Right (toConstType (createTemplateType "std::variant" [
          lt,
          rt]))))
        Core.TypeFunction v0 -> encodeFunctionType cx g v0
        Core.TypeForall v0 -> encodeForallType cx g v0
        Core.TypeList v0 -> Eithers.map (\enc -> toConstType (createTemplateType "std::vector" [
          enc])) (encodeType cx g v0)
        Core.TypeMap v0 -> Eithers.bind (encodeType cx g (Core.mapTypeKeys v0)) (\kt -> Eithers.bind (encodeType cx g (Core.mapTypeValues v0)) (\vt -> Right (toConstType (createTemplateType "std::map" [
          kt,
          vt]))))
        Core.TypeLiteral v0 -> Right (encodeLiteralType v0)
        Core.TypeMaybe v0 -> Eithers.map (\enc -> toConstType (createTemplateType "std::optional" [
          enc])) (encodeType cx g v0)
        Core.TypePair v0 -> Eithers.bind (encodeType cx g (Core.pairTypeFirst v0)) (\ft -> Eithers.bind (encodeType cx g (Core.pairTypeSecond v0)) (\st -> Right (toConstType (createTemplateType "std::pair" [
          ft,
          st]))))
        Core.TypeRecord _ -> Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "unexpected anonymous record type")),
          Context.inContextContext = cx})
        Core.TypeSet v0 -> Eithers.map (\enc -> toConstType (createTemplateType "std::set" [
          enc])) (encodeType cx g v0)
        Core.TypeUnion _ -> Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "unexpected anonymous union type")),
          Context.inContextContext = cx})
        Core.TypeVariable v0 -> Right (Syntax.TypeExpressionBasic (Syntax.BasicTypeNamed (sanitizeCppName (Core.unName v0))))
        Core.TypeWrap _ -> Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "unexpected anonymous wrapped type")),
          Context.inContextContext = cx})
        Core.TypeUnit -> Right (createTemplateType "std::tuple" [])
        _ -> Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError "Unsupported type")),
          Context.inContextContext = cx})

encodeForallType :: Context.Context -> t0 -> Core.ForallType -> Either (Context.InContext Errors.Error) Syntax.TypeExpression
encodeForallType cx g lt = encodeType cx g (Core.forallTypeBody lt)

encodeFunctionType :: Context.Context -> t0 -> Core.FunctionType -> Either (Context.InContext Errors.Error) Syntax.TypeExpression
encodeFunctionType cx g ft =
    Eithers.bind (encodeType cx g (Core.functionTypeDomain ft)) (\dom -> Eithers.bind (encodeType cx g (Core.functionTypeCodomain ft)) (\cod -> Right (Syntax.TypeExpressionFunction (Syntax.FunctionType {
      Syntax.functionTypeReturnType = cod,
      Syntax.functionTypeParameters = [
        Syntax.Parameter {
          Syntax.parameterType = dom,
          Syntax.parameterName = "",
          Syntax.parameterUnnamed = False,
          Syntax.parameterDefaultValue = Nothing}]}))))

encodeApplicationType :: Context.Context -> t0 -> Core.ApplicationType -> Either (Context.InContext Errors.Error) Syntax.TypeExpression
encodeApplicationType cx g at =
    Eithers.bind (encodeType cx g (Core.applicationTypeFunction at)) (\body -> Eithers.bind (encodeType cx g (Core.applicationTypeArgument at)) (\arg -> Right (createTemplateType "TODO_template" [
      body,
      arg])))

encodeTypeAlias :: Context.Context -> t0 -> Core.Name -> Core.Type -> t1 -> Either (Context.InContext Errors.Error) Syntax.Declaration
encodeTypeAlias cx g name typ comment =
    Eithers.bind (encodeType cx g typ) (\cppType -> Right (Syntax.DeclarationTypedef (Syntax.TypedefDeclaration {
      Syntax.typedefDeclarationName = (className name),
      Syntax.typedefDeclarationType = cppType,
      Syntax.typedefDeclarationIsUsing = True})))

encodeTypeDefinition :: Context.Context -> t0 -> Core.Name -> Core.Type -> Either (Context.InContext Errors.Error) [Syntax.Declaration]
encodeTypeDefinition cx g name typ =

      let t = Rewriting.deannotateType typ
      in case t of
        Core.TypeForall v0 -> encodeTypeDefinition cx g name (Core.forallTypeBody v0)
        Core.TypeRecord v0 -> encodeRecordType cx g name v0 Nothing
        Core.TypeUnion v0 -> encodeUnionType cx g name v0 Nothing
        Core.TypeWrap v0 -> encodeWrappedType cx g name v0 Nothing
        _ -> Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "unexpected type in definition: " (Core_.type_ typ)))),
          Context.inContextContext = cx})

encodeFieldType :: t0 -> Core.FieldType -> Context.Context -> t1 -> Either (Context.InContext Errors.Error) Syntax.VariableDeclaration
encodeFieldType isParameter ft cx g =

      let fname = Core.fieldTypeName ft
          ftype = Core.fieldTypeType ft
      in (Eithers.bind (encodeType cx g ftype) (\cppType -> Right (Syntax.VariableDeclaration {
        Syntax.variableDeclarationType = (Just cppType),
        Syntax.variableDeclarationName = (encodeFieldName fname),
        Syntax.variableDeclarationInitializer = Nothing,
        Syntax.variableDeclarationIsAuto = False})))

encodeRecordType :: Context.Context -> t0 -> Core.Name -> [Core.FieldType] -> t1 -> Either (Context.InContext Errors.Error) [Syntax.Declaration]
encodeRecordType cx g name rt comment =
    Eithers.bind (Eithers.mapList (\f -> encodeFieldType False f cx g) rt) (\cppFields -> Eithers.bind (Eithers.mapList (\f -> encodeFieldType True f cx g) rt) (\constructorParams -> Right [
      cppClassDeclaration (className name) [] (Just (Syntax.ClassBody (Lists.concat [
        [
          memberSpecificationPublic],
        (Lists.map (\field -> Syntax.MemberSpecificationMember (Syntax.MemberDeclarationVariable field)) cppFields),
        [
          Syntax.MemberSpecificationMember (Syntax.MemberDeclarationConstructor (Syntax.ConstructorDeclaration {
            Syntax.constructorDeclarationName = (className name),
            Syntax.constructorDeclarationParameters = (Lists.map (\p -> Syntax.Parameter {
              Syntax.parameterType = (Maybes.fromMaybe (Syntax.TypeExpressionBasic Syntax.BasicTypeInt) (Syntax.variableDeclarationType p)),
              Syntax.parameterName = (Syntax.variableDeclarationName p),
              Syntax.parameterUnnamed = False,
              Syntax.parameterDefaultValue = Nothing}) constructorParams),
            Syntax.constructorDeclarationInitializers = (Lists.map (\field -> Syntax.MemInitializer {
              Syntax.memInitializerName = (Syntax.variableDeclarationName field),
              Syntax.memInitializerArguments = [
                createIdentifierExpr (Syntax.variableDeclarationName field)]}) cppFields),
            Syntax.constructorDeclarationBody = (createConstructorBody constructorParams)}))]]))),
      (createLessThanOperator name rt)]))

encodeUnionType :: Context.Context -> t0 -> Core.Name -> [Core.FieldType] -> t1 -> Either (Context.InContext Errors.Error) [Syntax.Declaration]
encodeUnionType cx g name rt comment =
    Logic.ifElse (Schemas.isEnumRowType rt) (encodeEnumType cx g name rt comment) (encodeVariantType cx g name rt comment)

encodeEnumType :: t0 -> t1 -> Core.Name -> [Core.FieldType] -> t2 -> Either t3 [Syntax.Declaration]
encodeEnumType cx g name tfields comment =
    Right [
      cppEnumDeclaration (className name) (Just (Syntax.ClassBody (Lists.map (\ft -> Syntax.MemberSpecificationMember (Syntax.MemberDeclarationVariable (Syntax.VariableDeclaration {
        Syntax.variableDeclarationType = Nothing,
        Syntax.variableDeclarationName = (encodeEnumValue (Core.fieldTypeName ft)),
        Syntax.variableDeclarationInitializer = Nothing,
        Syntax.variableDeclarationIsAuto = False}))) tfields)))]

encodeVariantType :: Context.Context -> t0 -> Core.Name -> [Core.FieldType] -> t1 -> Either (Context.InContext Errors.Error) [Syntax.Declaration]
encodeVariantType cx g name variants comment =
    Eithers.bind (Eithers.mapList (\v -> createVariantClass cx g name name v) variants) (\variantClasses -> Right (Lists.concat [
      generateForwardDeclarations name variants,
      [
        createVisitorInterface name variants],
      [
        createUnionBaseClass name variants],
      variantClasses,
      [
        createPartialVisitorInterface name variants],
      [
        createAcceptImplementation name variants]]))

encodeWrappedType :: Context.Context -> t0 -> Core.Name -> Core.Type -> t1 -> Either (Context.InContext Errors.Error) [Syntax.Declaration]
encodeWrappedType cx g name typ comment =
    encodeRecordType cx g name [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "value"),
        Core.fieldTypeType = typ}] comment

createVisitorInterface :: Core.Name -> [Core.FieldType] -> Syntax.Declaration
createVisitorInterface tname variants =
    Syntax.DeclarationTemplate (Syntax.TemplateDeclaration {
      Syntax.templateDeclarationInline = False,
      Syntax.templateDeclarationParameters = [
        "typename R"],
      Syntax.templateDeclarationDeclaration = (cppClassDeclaration (visitorName tname) [] (Just (Syntax.ClassBody (Lists.concat [
        [
          memberSpecificationPublic],
        (Lists.map (\ft ->
          let fname = Core.fieldTypeName ft
          in (Syntax.MemberSpecificationMember (Syntax.MemberDeclarationFunction (Syntax.FunctionDeclaration {
            Syntax.functionDeclarationPrefixSpecifiers = [
              Syntax.FunctionSpecifierPrefixVirtual],
            Syntax.functionDeclarationReturnType = (Syntax.TypeExpressionBasic (Syntax.BasicTypeNamed "R")),
            Syntax.functionDeclarationName = "visit",
            Syntax.functionDeclarationParameters = [
              Syntax.Parameter {
                Syntax.parameterType = (Syntax.TypeExpressionQualified (Syntax.QualifiedType {
                  Syntax.qualifiedTypeBaseType = (Syntax.TypeExpressionQualified (Syntax.QualifiedType {
                    Syntax.qualifiedTypeBaseType = (Syntax.TypeExpressionBasic (Syntax.BasicTypeNamed (variantName tname fname))),
                    Syntax.qualifiedTypeQualifier = Syntax.TypeQualifierConst})),
                  Syntax.qualifiedTypeQualifier = Syntax.TypeQualifierLvalueRef})),
                Syntax.parameterName = "value",
                Syntax.parameterUnnamed = False,
                Syntax.parameterDefaultValue = Nothing}],
            Syntax.functionDeclarationSuffixSpecifiers = [],
            Syntax.functionDeclarationBody = Syntax.FunctionBodyPure})))) variants),
        [
          Syntax.MemberSpecificationMember (Syntax.MemberDeclarationDestructor (Syntax.DestructorDeclaration {
            Syntax.destructorDeclarationPrefixSpecifiers = [
              Syntax.FunctionSpecifierPrefixVirtual],
            Syntax.destructorDeclarationName = (visitorName tname),
            Syntax.destructorDeclarationSuffixSpecifiers = [],
            Syntax.destructorDeclarationBody = Syntax.FunctionBodyDefault}))]]))))})

createPartialVisitorInterface :: Core.Name -> [Core.FieldType] -> Syntax.Declaration
createPartialVisitorInterface tname variants =
    Syntax.DeclarationTemplate (Syntax.TemplateDeclaration {
      Syntax.templateDeclarationInline = False,
      Syntax.templateDeclarationParameters = [
        "typename R"],
      Syntax.templateDeclarationDeclaration = (cppClassDeclaration (partialVisitorName tname) [
        Syntax.BaseSpecifier {
          Syntax.baseSpecifierAccess = Syntax.AccessSpecifierPublic,
          Syntax.baseSpecifierName = (Strings.cat2 (visitorName tname) "<R>")}] (Just (Syntax.ClassBody (Lists.concat [
        [
          memberSpecificationPublic],
        [
          Syntax.MemberSpecificationMember (Syntax.MemberDeclarationFunction (Syntax.FunctionDeclaration {
            Syntax.functionDeclarationPrefixSpecifiers = [
              Syntax.FunctionSpecifierPrefixVirtual],
            Syntax.functionDeclarationReturnType = (Syntax.TypeExpressionBasic (Syntax.BasicTypeNamed "R")),
            Syntax.functionDeclarationName = "otherwise",
            Syntax.functionDeclarationParameters = [
              constParameter "value" (Syntax.TypeExpressionBasic (Syntax.BasicTypeNamed (className tname)))],
            Syntax.functionDeclarationSuffixSpecifiers = [
              Syntax.FunctionSpecifierSuffixConst],
            Syntax.functionDeclarationBody = Syntax.FunctionBodyPure}))],
        (Lists.map (\ft ->
          let fname = Core.fieldTypeName ft
          in (Syntax.MemberSpecificationMember (Syntax.MemberDeclarationFunction (Syntax.FunctionDeclaration {
            Syntax.functionDeclarationPrefixSpecifiers = [],
            Syntax.functionDeclarationReturnType = (Syntax.TypeExpressionBasic (Syntax.BasicTypeNamed "R")),
            Syntax.functionDeclarationName = "visit",
            Syntax.functionDeclarationParameters = [
              constParameter "value" (Syntax.TypeExpressionBasic (Syntax.BasicTypeNamed (variantName tname fname)))],
            Syntax.functionDeclarationSuffixSpecifiers = [
              Syntax.FunctionSpecifierSuffixOverride],
            Syntax.functionDeclarationBody = (Syntax.FunctionBodyCompound (Syntax.CompoundStatement [
              Syntax.StatementJump (Syntax.JumpStatementReturnValue (createFunctionCallExpr "otherwise" [
                createIdentifierExpr "value"]))]))})))) variants)]))))})

createUnionBaseClass :: Core.Name -> t0 -> Syntax.Declaration
createUnionBaseClass name variants =
    cppClassDeclaration (className name) [] (Just (Syntax.ClassBody [
      memberSpecificationProtected,
      (Syntax.MemberSpecificationMember (Syntax.MemberDeclarationConstructor (Syntax.ConstructorDeclaration {
        Syntax.constructorDeclarationName = (className name),
        Syntax.constructorDeclarationParameters = [],
        Syntax.constructorDeclarationInitializers = [],
        Syntax.constructorDeclarationBody = Syntax.FunctionBodyDefault}))),
      memberSpecificationPublic,
      (Syntax.MemberSpecificationMember (Syntax.MemberDeclarationDestructor (Syntax.DestructorDeclaration {
        Syntax.destructorDeclarationPrefixSpecifiers = [
          Syntax.FunctionSpecifierPrefixVirtual],
        Syntax.destructorDeclarationName = (className name),
        Syntax.destructorDeclarationSuffixSpecifiers = [],
        Syntax.destructorDeclarationBody = Syntax.FunctionBodyDefault}))),
      (Syntax.MemberSpecificationMember (Syntax.MemberDeclarationTemplate (Syntax.TemplateDeclaration {
        Syntax.templateDeclarationInline = False,
        Syntax.templateDeclarationParameters = [
          "typename R"],
        Syntax.templateDeclarationDeclaration = (Syntax.DeclarationFunction (Syntax.FunctionDeclaration {
          Syntax.functionDeclarationPrefixSpecifiers = [],
          Syntax.functionDeclarationReturnType = (Syntax.TypeExpressionBasic (Syntax.BasicTypeNamed "R")),
          Syntax.functionDeclarationName = "accept",
          Syntax.functionDeclarationParameters = [
            Syntax.Parameter {
              Syntax.parameterType = (Syntax.TypeExpressionQualified (Syntax.QualifiedType {
                Syntax.qualifiedTypeBaseType = (Syntax.TypeExpressionBasic (Syntax.BasicTypeNamed (Strings.cat2 (visitorName name) "<R>"))),
                Syntax.qualifiedTypeQualifier = Syntax.TypeQualifierLvalueRef})),
              Syntax.parameterName = "visitor",
              Syntax.parameterUnnamed = False,
              Syntax.parameterDefaultValue = Nothing}],
          Syntax.functionDeclarationSuffixSpecifiers = [
            Syntax.FunctionSpecifierSuffixConst],
          Syntax.functionDeclarationBody = Syntax.FunctionBodyDeclaration}))})))]))

createVariantClass :: Context.Context -> t0 -> Core.Name -> Core.Name -> Core.FieldType -> Either (Context.InContext Errors.Error) Syntax.Declaration
createVariantClass cx g tname parentClass ft =

      let fname = Core.fieldTypeName ft
          variantType = Core.fieldTypeType ft
          hasValue = Logic.not (Schemas.isUnitType variantType)
          valueField =
                  Logic.ifElse hasValue (Eithers.map (\cppType -> [
                    Syntax.MemberSpecificationMember (Syntax.MemberDeclarationVariable (Syntax.VariableDeclaration {
                      Syntax.variableDeclarationType = (Just cppType),
                      Syntax.variableDeclarationName = "value",
                      Syntax.variableDeclarationInitializer = Nothing,
                      Syntax.variableDeclarationIsAuto = False}))]) (encodeType cx g (Rewriting.deannotateType variantType))) (Right [])
          constructorParams =
                  Logic.ifElse hasValue (Eithers.map (\paramType -> [
                    Syntax.Parameter {
                      Syntax.parameterType = paramType,
                      Syntax.parameterName = "value",
                      Syntax.parameterUnnamed = False,
                      Syntax.parameterDefaultValue = Nothing}]) (encodeType cx g (Rewriting.deannotateType variantType))) (Right [])
      in (Eithers.bind valueField (\vFields -> Eithers.bind constructorParams (\vParams ->
        let initList =
                Logic.ifElse hasValue [
                  Syntax.MemInitializer {
                    Syntax.memInitializerName = "value",
                    Syntax.memInitializerArguments = [
                      createIdentifierExpr "value"]}] []
        in (Right (cppClassDeclaration (variantName tname fname) [
          Syntax.BaseSpecifier {
            Syntax.baseSpecifierAccess = Syntax.AccessSpecifierPublic,
            Syntax.baseSpecifierName = (className parentClass)}] (Just (Syntax.ClassBody (Lists.concat [
          [
            memberSpecificationPublic],
          vFields,
          [
            Syntax.MemberSpecificationMember (Syntax.MemberDeclarationConstructor (Syntax.ConstructorDeclaration {
              Syntax.constructorDeclarationName = (variantName tname fname),
              Syntax.constructorDeclarationParameters = vParams,
              Syntax.constructorDeclarationInitializers = initList,
              Syntax.constructorDeclarationBody = (createConstructorBody vParams)}))]]))))))))

createAcceptImplementation :: Core.Name -> [Core.FieldType] -> Syntax.Declaration
createAcceptImplementation tname variants =
    Syntax.DeclarationTemplate (Syntax.TemplateDeclaration {
      Syntax.templateDeclarationInline = False,
      Syntax.templateDeclarationParameters = [
        "typename R"],
      Syntax.templateDeclarationDeclaration = (Syntax.DeclarationFunction (Syntax.FunctionDeclaration {
        Syntax.functionDeclarationPrefixSpecifiers = [],
        Syntax.functionDeclarationReturnType = (Syntax.TypeExpressionBasic (Syntax.BasicTypeNamed "R")),
        Syntax.functionDeclarationName = (Strings.cat2 (className tname) "::accept"),
        Syntax.functionDeclarationParameters = [
          Syntax.Parameter {
            Syntax.parameterType = (Syntax.TypeExpressionQualified (Syntax.QualifiedType {
              Syntax.qualifiedTypeBaseType = (Syntax.TypeExpressionBasic (Syntax.BasicTypeNamed (Strings.cat2 (visitorName tname) "<R>"))),
              Syntax.qualifiedTypeQualifier = Syntax.TypeQualifierLvalueRef})),
            Syntax.parameterName = "visitor",
            Syntax.parameterUnnamed = False,
            Syntax.parameterDefaultValue = Nothing}],
        Syntax.functionDeclarationSuffixSpecifiers = [
          Syntax.FunctionSpecifierSuffixConst],
        Syntax.functionDeclarationBody = (Syntax.FunctionBodyCompound (Syntax.CompoundStatement (Lists.map (\ft ->
          let fname = Core.fieldTypeName ft
          in (Syntax.StatementSelection (Syntax.SelectionStatement {
            Syntax.selectionStatementCondition = (Syntax.ExpressionAssignment (Syntax.AssignmentExpressionAssignment (Syntax.ExplicitAssignment {
              Syntax.explicitAssignmentLeft = (cppUnaryExpressionToCppLogicalOrExpression (Syntax.UnaryExpressionPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "auto ptr")))),
              Syntax.explicitAssignmentOp = Syntax.AssignmentOperatorAssign,
              Syntax.explicitAssignmentRight = (Syntax.AssignmentExpressionConditional (Syntax.ConditionalExpressionLogicalOr (cppUnaryExpressionToCppLogicalOrExpression (Syntax.UnaryExpressionPostfix (Syntax.PostfixExpressionFunctionCall (Syntax.FunctionCallOperation {
                Syntax.functionCallOperationFunction = (Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier (Strings.cat2 (Strings.cat2 "dynamic_cast<const " (variantName tname fname)) "*>"))),
                Syntax.functionCallOperationArguments = [
                  cppPrimaryExpressionToCppExpression (Syntax.PrimaryExpressionIdentifier "this")]}))))))}))),
            Syntax.selectionStatementThenBranch = (Syntax.StatementCompound (Syntax.CompoundStatement [
              Syntax.StatementJump (Syntax.JumpStatementReturnValue (cppPostfixExpressionToCppExpression (Syntax.PostfixExpressionFunctionCall (Syntax.FunctionCallOperation {
                Syntax.functionCallOperationFunction = (Syntax.PostfixExpressionMemberAccess (Syntax.MemberAccessOperation {
                  Syntax.memberAccessOperationObject = (Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "visitor")),
                  Syntax.memberAccessOperationMember = "visit"})),
                Syntax.functionCallOperationArguments = [
                  cppUnaryExpressionToCppExpression (Syntax.UnaryExpressionUnaryOp (Syntax.UnaryOperation {
                    Syntax.unaryOperationOperator = Syntax.UnaryOperatorDereference,
                    Syntax.unaryOperationOperand = (Syntax.UnaryExpressionPostfix (Syntax.PostfixExpressionPrimary (Syntax.PrimaryExpressionIdentifier "ptr")))}))]}))))])),
            Syntax.selectionStatementElseBranch = (Just (createThrowStmt "std::runtime_error" createTypeIdNameCall))}))) variants)))}))})

generateForwardDeclarations :: Core.Name -> [Core.FieldType] -> [Syntax.Declaration]
generateForwardDeclarations tname fields =
    Lists.map (\ft -> cppClassDeclaration (variantName tname (Core.fieldTypeName ft)) [] Nothing) fields

createLessThanOperator :: Core.Name -> t0 -> Syntax.Declaration
createLessThanOperator typeName fields =
    Syntax.DeclarationFunction (Syntax.FunctionDeclaration {
      Syntax.functionDeclarationPrefixSpecifiers = [
        Syntax.FunctionSpecifierPrefixInline],
      Syntax.functionDeclarationReturnType = (Syntax.TypeExpressionBasic Syntax.BasicTypeBool),
      Syntax.functionDeclarationName = "operator<",
      Syntax.functionDeclarationParameters = [
        unnamedParameter "lhs" (Syntax.TypeExpressionBasic (Syntax.BasicTypeNamed (className typeName))),
        (unnamedParameter "rhs" (Syntax.TypeExpressionBasic (Syntax.BasicTypeNamed (className typeName))))],
      Syntax.functionDeclarationSuffixSpecifiers = [],
      Syntax.functionDeclarationBody = (Syntax.FunctionBodyCompound (Syntax.CompoundStatement [
        Syntax.StatementJump (Syntax.JumpStatementReturnValue (createLiteralBoolExpr False))]))})

serializeHeaderFile :: Core.Name -> [Syntax.IncludeDirective] -> [Syntax.Declaration] -> (String, String)
serializeHeaderFile name includes decls =
    (bindingNameToFilePath name, (Serialization.printExpr (Serialization.parenthesize (Serde.encodeProgram (createHeaderFile includes decls)))))

bindingNameToFilePath :: Core.Name -> String
bindingNameToFilePath name =
    CoderUtils.nameToFilePath Util.CaseConventionLowerSnake Util.CaseConventionLowerSnake (Module.FileExtension "h") name

findIncludes :: Bool -> Module.Namespace -> [Module.TypeDefinition] -> [Syntax.IncludeDirective]
findIncludes withFwd ns defs =
    Lists.concat [
      [
        Syntax.IncludeDirective {
          Syntax.includeDirectiveName = "memory",
          Syntax.includeDirectiveIsSystem = True},
        Syntax.IncludeDirective {
          Syntax.includeDirectiveName = "stdexcept",
          Syntax.includeDirectiveIsSystem = True}],
      (Lists.map (\depName -> Syntax.IncludeDirective {
        Syntax.includeDirectiveName = (bindingNameToFilePath depName),
        Syntax.includeDirectiveIsSystem = False}) (findTypeDependencies ns defs)),
      (Logic.ifElse withFwd [
        Syntax.IncludeDirective {
          Syntax.includeDirectiveName = (bindingNameToFilePath (fwdHeaderName ns)),
          Syntax.includeDirectiveIsSystem = False}] [])]

findTypeDependencies :: Module.Namespace -> [Module.TypeDefinition] -> [Core.Name]
findTypeDependencies ns defs =
    Lists.filter (\n -> Logic.not (Equality.equal (Maybes.map Module.unNamespace (Names.namespaceOf n)) (Just (Module.unNamespace ns)))) (Sets.toList (Lists.foldl (\acc -> \d -> Sets.union acc (Rewriting.typeDependencyNames True (Module.typeDefinitionType d))) Sets.empty defs))

gatherMetadata :: t0 -> Bool
gatherMetadata defs = True

isStdContainerType :: Core.Type -> Bool
isStdContainerType typ =

      let t = Rewriting.deannotateType typ
      in case t of
        Core.TypeApplication v0 -> isStdContainerType (Core.applicationTypeFunction v0)
        Core.TypeList _ -> True
        Core.TypeMap _ -> True
        Core.TypeMaybe _ -> True
        Core.TypeSet _ -> True
        _ -> False

isStructType :: Core.Type -> Bool
isStructType rawType =

      let t = Schemas.fullyStripType rawType
          isLiteral =
                  case t of
                    Core.TypeLiteral _ -> True
                    _ -> False
      in (Logic.and (Logic.not isLiteral) (Logic.not (Schemas.isEnumType rawType)))

isTemplateType :: Core.Type -> Bool
isTemplateType typ =

      let t = Rewriting.deannotateType typ
      in (Logic.or (case t of
        Core.TypeLiteral v0 -> case v0 of
          Core.LiteralTypeString -> True
          _ -> False
        _ -> False) (isStdContainerType typ))

generateTypeFile :: Module.Namespace -> Module.TypeDefinition -> Context.Context -> t0 -> Either (Context.InContext Errors.Error) (String, String)
generateTypeFile ns def_ cx g =

      let name = Module.typeDefinitionName def_
          typ = Module.typeDefinitionType def_
      in (Eithers.bind (encodeTypeDefinition cx g name typ) (\decls ->
        let includes = findIncludes True ns [
              def_]
        in (Right (serializeHeaderFile name includes [
          namespaceDecl ns decls]))))

generateTypeFiles :: Module.Namespace -> [Module.TypeDefinition] -> Context.Context -> t0 -> Either (Context.InContext Errors.Error) [(String, String)]
generateTypeFiles ns defs cx g =
    Eithers.bind (Eithers.mapList (\d -> generateTypeFile ns d cx g) defs) (\classFiles -> Right classFiles)

moduleToCpp :: Module.Module -> [Module.Definition] -> Context.Context -> t0 -> Either (Context.InContext Errors.Error) (M.Map String String)
moduleToCpp mod defs cx g =

      let ns = Module.moduleNamespace mod
          typeDefs = Pairs.first (Schemas.partitionDefinitions defs)
      in (Eithers.bind (generateTypeFiles ns typeDefs cx g) (\typeFiles -> Right (Maps.fromList typeFiles)))
