-- Note: this file was created with the help of a large language model. It requires further human review.

module Hydra.Ext.Cpp.Coder (moduleToCpp) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Hydra.Dsl.ShorthandTypes
import Hydra.Dsl.Terms
import Hydra.Ext.Cpp.Language
import Hydra.Ext.Cpp.Names
import Hydra.Ext.Cpp.Utils
import qualified Hydra.Ext.Cpp.Serde as CppSer
import qualified Hydra.Ext.Cpp.Syntax as Cpp
import Hydra.Kernel
import Hydra.Lib.Io
import qualified Hydra.Lib.Strings as Strings
import Hydra.Staging.Adapters
import Hydra.Staging.Formatting
import Hydra.Staging.Serialization
import qualified Hydra.Decode as Decode

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Data.Set as S
import qualified Text.Read as TR

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Switch to control whether to use std::variant or class hierarchy for union types
-- Set to False to use class hierarchy with visitor pattern (recommended for labeled unions)
-- Set to True to use std::variant approach (more compact but less type-safe for labeled unions)
useStdVariants :: Bool
useStdVariants = False

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

-- | Temporary metadata which is used to create the header section of a C++ file
data CppModuleMetadata = CppModuleMetadata {
  cppModuleMetadataTypeVariables :: S.Set Name,
  cppModuleMetadataUsesVariant :: Bool,
  cppModuleMetadataUsesOptional :: Bool,
  cppModuleMetadataUsesVector :: Bool,
  cppModuleMetadataUsesMap :: Bool,
  cppModuleMetadataUsesSet :: Bool,
  cppModuleMetadataUsesString :: Bool,
  cppModuleMetadataUsesMemory :: Bool,
  cppModuleMetadataUsesFunctional :: Bool,
  cppModuleMetadataUsesTypeTraits :: Bool,
  cppModuleMetadataUsesIOStream :: Bool,
  cppModuleMetadataUsesAlgorithm :: Bool
}

--------------------------------------------------------------------------------
-- Top-Level Entry Points
--------------------------------------------------------------------------------

-- | Convert a module to C++ code files
moduleToCpp :: Module -> Flow Graph (M.Map FilePath String)
moduleToCpp mod = do
  program <- encodeModule mod

  let header = printExpr $ parenthesize $ CppSer.encodeProgram program
  let implContent = generateImplementationFile mod

  let headerPath = namespaceToFilePath CaseConventionLowerSnake (FileExtension "h") $ moduleNamespace mod
  let implPath = namespaceToFilePath CaseConventionLowerSnake (FileExtension "cpp") $ moduleNamespace mod

  return $ M.fromList [
    (headerPath, header),
    (implPath, implContent)]

-- | Generate implementation file with template specializations and implementation details
generateImplementationFile :: Module -> String
generateImplementationFile mod =
  "// Generated C++ implementation file\n\n"
  ++ "#include \"" ++ namespaceToFilePath CaseConventionLowerSnake (FileExtension "h") (moduleNamespace mod) ++ "\"\n\n"
  ++ "namespace " ++ unNamespace (moduleNamespace mod) ++ " {\n\n"
  ++ "// Add implementation-specific code here\n\n"
  ++ "} // namespace " ++ unNamespace (moduleNamespace mod) ++ "\n"

--------------------------------------------------------------------------------
-- Encoding Functions (Alphabetical Order)
--------------------------------------------------------------------------------

encodeApplicationType :: CppEnvironment -> ApplicationType -> Flow Graph Cpp.TypeExpression
encodeApplicationType env at = do
  cppBody <- encodeType env body
  cppArgs <- CM.mapM (encodeType env) args
  return $ createTemplateTypeFromBase cppBody cppArgs
  where
    (body, args) = gatherParams (TypeApplication at) []

    gatherParams t ps = case stripType t of
      TypeApplication (ApplicationType lhs rhs) -> gatherParams lhs (rhs:ps)
      _ -> (t, ps)

    createTemplateTypeFromBase base args = case base of
      Cpp.TypeExpressionBasic (Cpp.BasicTypeNamed name) -> createTemplateType name args
      _ -> error "Non-named type in template application"

encodeClassHierarchyUnion :: CppEnvironment -> Name -> [FieldType] -> Maybe String -> Flow Graph [Cpp.Declaration]
encodeClassHierarchyUnion env name variants comment = do
  baseClass <- createUnionBaseClass env name variants
  let visitorInterface = createVisitorInterface env name variants
  let partialVisitorInterface = createPartialVisitorInterface env name variants
  variantClasses <- CM.mapM (createVariantClass env name name) variants
  return $ [baseClass, visitorInterface, partialVisitorInterface] ++ variantClasses

encodeEnumType :: CppEnvironment -> Name -> [FieldType] -> Maybe String -> Flow Graph [Cpp.Declaration]
encodeEnumType env name tfields _comment = return [
  Cpp.DeclarationClass $
    Cpp.ClassDeclaration
      (Cpp.ClassSpecifier Cpp.ClassKeyEnumClass (encodeName False CaseConventionPascal env name) [])
      (Just $ Cpp.ClassBody enumFields)
  ]
  where
    enumFields = [Cpp.MemberSpecificationMember $
                    Cpp.MemberDeclarationVariable $
                      Cpp.VariableDeclaration
                        Nothing
                        (encodeEnumValue env fname)
                        (Just $ createLiteralIntExpr idx)
                        False
                  | (FieldType fname _, idx) <- zip tfields [0..]]

encodeFieldType :: CppEnvironment -> Bool -> FieldType -> Flow Graph Cpp.VariableDeclaration
encodeFieldType env isParameter (FieldType fname ftype) = do
  _comment <- getTypeDescription ftype
  cppType <- encodeType env ftype
  let finalType = if isParameter then parameterType cppType else fieldType cppType
  return $ Cpp.VariableDeclaration
    (Just finalType)
    (encodeFieldName env fname)
    Nothing
    False
  where
    isBasicType = typeVariant (stripType ftype) == TypeVariantLiteral
    isTemplateType = isStdContainer ftype

    fieldType typ =
      if isBasicType then typ
      else Cpp.TypeExpressionQualified $ Cpp.QualifiedType typ Cpp.TypeQualifierConst

    parameterType typ
      | isBasicType = typ
      | isTemplateType = typ
      | otherwise = Cpp.TypeExpressionQualified $
                      Cpp.QualifiedType
                        (Cpp.TypeExpressionQualified $
                          Cpp.QualifiedType typ Cpp.TypeQualifierConst)
                        Cpp.TypeQualifierLvalueRef

    isStdContainer typ = case stripType typ of
      TypeApplication (ApplicationType lhs _) -> isStdContainer lhs
      TypeList _ -> True
      TypeMap _ -> True
      TypeOptional _ -> True
      TypeSet _ -> True
      _ -> False

encodeForallType :: CppEnvironment -> ForallType -> Flow Graph Cpp.TypeExpression
encodeForallType env lt = do
  cppBody <- encodeType env body
  return cppBody
  where
    (body, _) = gatherParams (TypeForall lt) []

    gatherParams t ps = case stripType t of
      TypeForall (ForallType name body) -> gatherParams body (name:ps)
      _ -> (t, L.reverse ps)

encodeFunctionType :: CppEnvironment -> FunctionType -> Flow Graph Cpp.TypeExpression
encodeFunctionType env ft = do
  cppDoms <- CM.mapM encode doms
  cppCod <- encode cod
  return $ Cpp.TypeExpressionFunction $ Cpp.FunctionType cppCod (paramFromType <$> cppDoms)
  where
    encode = encodeType env
    (doms, cod) = gatherParams [] ft

    gatherParams rdoms (FunctionType dom cod) = case stripType cod of
      TypeFunction ft2 -> gatherParams (dom:rdoms) ft2
      _ -> (L.reverse (dom:rdoms), cod)

    paramFromType t = Cpp.Parameter t "" Nothing

encodeLiteralType :: LiteralType -> Flow Graph Cpp.TypeExpression
encodeLiteralType lt = do
  basicType <- findType
  return $ Cpp.TypeExpressionBasic basicType
  where
    findType = case lt of
      LiteralTypeBinary -> pure Cpp.BasicTypeChar
      LiteralTypeBoolean -> pure Cpp.BasicTypeBool
      LiteralTypeFloat ft -> case ft of
        FloatTypeFloat64 -> pure Cpp.BasicTypeDouble
        FloatTypeFloat32 -> pure Cpp.BasicTypeFloat
        _ -> pure Cpp.BasicTypeDouble
      LiteralTypeInteger it -> case it of
        IntegerTypeBigint -> pure Cpp.BasicTypeInt
        IntegerTypeInt8 -> pure Cpp.BasicTypeChar
        IntegerTypeInt16 -> pure $ Cpp.BasicTypeNamed "int16_t"
        IntegerTypeInt32 -> pure Cpp.BasicTypeInt
        IntegerTypeInt64 -> pure $ Cpp.BasicTypeNamed "int64_t"
        _ -> pure Cpp.BasicTypeInt
      LiteralTypeString -> pure Cpp.BasicTypeString

encodeModule :: Module -> Flow Graph Cpp.Program
encodeModule mod = do
  defs <- adaptedModuleDefinitions cppLanguage mod
  let namespaces = namespacesForDefinitions False encodeNamespace (moduleNamespace mod) defs
  let env = CppEnvironment {
    cppEnvironmentNamespaces = namespaces,
    cppEnvironmentBoundTypeVariables = ([], M.empty)}
  defDecls <- reorderedTypeDecls env defs
  let meta = gatherMetadata defs
  let includeDirectives = includes namespaces meta
  return $ Cpp.Program includeDirectives (wrapWithNamespace (moduleNamespace mod) defDecls)
  where
    wrapWithNamespace ns decls = [
      Cpp.DeclarationNamespace $
        Cpp.NamespaceDeclaration (encodeNamespace ns) decls]

    includes namespaces meta = fixedIncludes ++ conditionalIncludes ++ domainIncludes
      where
        fixedIncludes = [
          Cpp.IncludeDirective "string" True,
          Cpp.IncludeDirective "cstdint" True,
          Cpp.IncludeDirective "stdexcept" True]

        conditionalIncludes = Y.catMaybes [
          if cppModuleMetadataUsesVector meta then Just (Cpp.IncludeDirective "vector" True) else Nothing,
          if cppModuleMetadataUsesMap meta then Just (Cpp.IncludeDirective "map" True) else Nothing,
          if cppModuleMetadataUsesSet meta then Just (Cpp.IncludeDirective "set" True) else Nothing,
          if cppModuleMetadataUsesVariant meta then Just (Cpp.IncludeDirective "variant" True) else Nothing,
          if cppModuleMetadataUsesOptional meta then Just (Cpp.IncludeDirective "optional" True) else Nothing,
          if cppModuleMetadataUsesMemory meta then Just (Cpp.IncludeDirective "memory" True) else Nothing,
          if cppModuleMetadataUsesFunctional meta then Just (Cpp.IncludeDirective "functional" True) else Nothing,
          if cppModuleMetadataUsesTypeTraits meta then Just (Cpp.IncludeDirective "type_traits" True) else Nothing]

        domainIncludes = toCppInclude <$> names
          where
            names = L.sort $ M.elems $ namespacesMapping namespaces
            toCppInclude ns =
              Cpp.IncludeDirective (namespaceToHeaderPath ns) False

            namespaceToHeaderPath ns =
              L.intercalate "/" (convertCase CaseConventionCamel CaseConventionLowerSnake <$> Strings.splitOn "::" ns) ++ ".h"

    reorderedTypeDecls env defs = do
      decls <- CM.mapM encode typeDefs
      let declMap = M.fromList $ L.zip (typeDefinitionName <$> typeDefs) decls
      return $ L.concat (componentDecls declMap <$> typeComponents)
      where
        typeDefs = Y.catMaybes (toTypeDef <$> defs)

        toTypeDef def = case def of
          DefinitionType d -> Just d
          _ -> Nothing

        typeDefMap = M.fromList $ L.zip (typeDefinitionName <$> typeDefs) typeDefs

        encode (TypeDefinition name typ) = withTrace ("type element " ++ unName name) $ do
          comment <- fmap normalizeComment <$> getTypeDescription typ
          encodeTypeDefinition env name typ comment

        typeComponents = topologicalSortComponents (toAdjPair <$> typeDefs)

        toAdjPair (TypeDefinition name typ) = (name, S.toList $ typeDependencyNames True False typ)

        componentDecls declMap comp = forwardDecls ++ typeDecls
          where
            forwardDecls = if L.length comp < 2 then [] else toForwardDecl <$> structDefs
              where
                toForwardDecl (TypeDefinition name _) =
                  Cpp.DeclarationClass $
                    Cpp.ClassDeclaration
                      (Cpp.ClassSpecifier Cpp.ClassKeyStruct (encodeName False CaseConventionPascal env name) [])
                      Nothing

            typeDecls = L.concat $ Y.catMaybes $ fmap (\d -> M.lookup (typeDefinitionName d) declMap) reorderedDefs

            reorderedDefs = otherDefs ++ usingDefs ++ structDefs

            defs = Y.catMaybes $ fmap (\n -> M.lookup n typeDefMap) comp

            (usingDefs, nonUsingDefs) = L.partition isUnion defs

            isUnion (TypeDefinition _ typ) =
              isEnumType typ ||
              (useStdVariants && typeVariant (stripType typ) == TypeVariantUnion)

            (structDefs, otherDefs) = L.partition (isStructType . typeDefinitionType) nonUsingDefs

encodeRecordType :: CppEnvironment -> Name -> RowType -> Maybe String -> Flow Graph Cpp.Declaration
encodeRecordType env name (RowType _ tfields) _comment = do
  cppFields <- CM.mapM (encodeFieldType env False) tfields
  constructorParams <- createParameters tfields
  return $ Cpp.DeclarationClass $
    Cpp.ClassDeclaration
      (Cpp.ClassSpecifier Cpp.ClassKeyStruct (encodeName False CaseConventionPascal env name) [])
      (Just $ Cpp.ClassBody (publicSection cppFields ++ createStructMethods cppFields constructorParams))
  where
    publicSection fields = [Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationVariable field | field <- fields]

    createStructMethods fields params = [
      Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationConstructor $
        Cpp.ConstructorDeclaration
          (encodeName False CaseConventionPascal env name)
          params
          [Cpp.MemInitializer
             (Cpp.variableDeclarationName field)
             [createIdentifierExpr $ Cpp.variableDeclarationName field]
           | field <- fields]
          (Cpp.CompoundStatement [])]

    createParameters fields = do
      let fieldNames = [fname | FieldType fname _ <- fields]
      CM.zipWithM createParam fieldNames fields

    createParam fieldName (FieldType _ ftype) = do
      paramDecl <- encodeFieldType env True (FieldType fieldName ftype)
      return $ Cpp.Parameter
        (Y.fromJust $ Cpp.variableDeclarationType paramDecl)
        (Cpp.variableDeclarationName paramDecl)
        Nothing

encodeStdVariantUnion :: CppEnvironment -> Name -> [FieldType] -> Maybe String -> Flow Graph [Cpp.Declaration]
encodeStdVariantUnion env name variants _comment = do
  taggedTypes <- CM.mapM createTaggedType variants
  variantTypes <- CM.mapM (encodeType env . fieldTypeType) variants
  let variantTypedef = Cpp.DeclarationTypedef $
        Cpp.TypedefDeclaration
          (encodeName False CaseConventionPascal env name)
          (createTemplateType "std::variant" variantTypes)
          True
  let visitorHelpers = createVariantVisitors env name variants
  return $ [tagEnum, variantTypedef] ++ taggedTypes ++ visitorHelpers
  where
    tagName = encodeName False CaseConventionPascal env name ++ "Tag"

    tagEnum = Cpp.DeclarationClass $
      Cpp.ClassDeclaration
        (Cpp.ClassSpecifier Cpp.ClassKeyEnumClass tagName [])
        (Just $ Cpp.ClassBody [
          Cpp.MemberSpecificationMember $
            Cpp.MemberDeclarationVariable $
              Cpp.VariableDeclaration
                Nothing
                (encodeEnumValue env variantName)
                (Just $ createLiteralIntExpr idx)
                False
          | (FieldType variantName _, idx) <- zip variants [0..]])

    createTaggedType (FieldType variantName variantType) = do
      wrappedType <- encodeType env variantType
      return $ Cpp.DeclarationClass $
        Cpp.ClassDeclaration
          (Cpp.ClassSpecifier Cpp.ClassKeyStruct (encodeName False CaseConventionPascal env variantName) [])
          (Just $ Cpp.ClassBody [
            Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationVariable $
              Cpp.VariableDeclaration (Just wrappedType) "value" Nothing False,
            Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationVariable $
              Cpp.VariableDeclaration
                (Just $ Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed tagName)
                "tag"
                (Just $ createEnumAccessExpr tagName (encodeEnumValue env variantName))
                False,
            Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationConstructor $
              Cpp.ConstructorDeclaration
                (encodeName False CaseConventionPascal env variantName)
                [Cpp.Parameter wrappedType "val" Nothing]
                [Cpp.MemInitializer "value" [createIdentifierExpr "val"]]
                (Cpp.CompoundStatement [])
          ])

    createVariantVisitors env name _variants = [
      Cpp.DeclarationFunction $
        Cpp.FunctionDeclaration
          []
          (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed "R")
          ("visit" ++ encodeName False CaseConventionPascal env name)
          [Cpp.Parameter
            (Cpp.TypeExpressionQualified $
              Cpp.QualifiedType
                (Cpp.TypeExpressionQualified $
                  Cpp.QualifiedType
                    (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed $
                      encodeName False CaseConventionPascal env name)
                    Cpp.TypeQualifierConst)
                Cpp.TypeQualifierLvalueRef)
            "variant"
            Nothing,
           Cpp.Parameter
            (Cpp.TypeExpressionTemplate $
              Cpp.TemplateType "std::function"
                [Cpp.TemplateArgumentType $
                  Cpp.TypeExpressionFunction $
                    Cpp.FunctionType
                      (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed "R")
                      [Cpp.Parameter
                        (Cpp.TypeExpressionQualified $
                          Cpp.QualifiedType
                            (Cpp.TypeExpressionQualified $
                              Cpp.QualifiedType
                                (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed $
                                  encodeName False CaseConventionPascal env name)
                                Cpp.TypeQualifierConst)
                            Cpp.TypeQualifierLvalueRef)
                        ""
                        Nothing]])
            "visitor"
            Nothing]
          []
          (Cpp.FunctionBodyCompound $ Cpp.CompoundStatement [
            Cpp.StatementJump $ Cpp.JumpStatementReturnValue $
              createFunctionCallExpr
                "std::visit"
                [createLambdaExpr [Cpp.Parameter Cpp.TypeExpressionAuto "v" Nothing]
                  (createFunctionCallExpr "visitor" [createIdentifierExpr "v"]),
                 createIdentifierExpr "variant"]
          ])]

encodeType :: CppEnvironment -> Type -> Flow Graph Cpp.TypeExpression
encodeType env typ = case stripType typ of
  TypeApplication at -> encodeApplicationType env at
  TypeFunction ft -> encodeFunctionType env ft
  TypeForall lt -> encodeForallType env lt
  TypeList et -> createTemplateType "std::vector" <$> ((:[]) <$> encode et)
  TypeMap (MapType kt vt) -> createTemplateType "std::map" <$> sequence [encode kt, encode vt]
  TypeLiteral lt -> encodeLiteralType lt
  TypeOptional et -> createTemplateType "std::optional" <$> ((:[]) <$> encode et)
  TypeRecord rt -> typeref typ (rowTypeTypeName rt)
  TypeSet et -> createTemplateType "std::set" <$> ((:[]) <$> encode et)
  TypeUnion rt -> typeref typ (rowTypeTypeName rt)
  TypeVariable name -> (elementTerm <$> requireElement name) >>= coreDecodeType >>= \t -> typeref t name
  TypeWrap (WrappedType name _) -> typeref typ name
  _ -> fail $ "Unsupported type: " ++ show (stripType typ)
  where
    encode = encodeType env
    typeref t name = pure $ createTypeReference (isStructType t) env name

encodeTypeAlias :: CppEnvironment -> Name -> Type -> Maybe String -> Flow Graph Cpp.Declaration
encodeTypeAlias env name typ _comment = do
  cppType <- encodeType env typ
  return $ Cpp.DeclarationTypedef $
    Cpp.TypedefDeclaration
      (encodeName False CaseConventionPascal env name)
      cppType
      True

encodeTypeDefinition :: CppEnvironment -> Name -> Type -> Maybe String -> Flow Graph [Cpp.Declaration]
encodeTypeDefinition env name typ comment = encode env typ
  where
    encode env typ = case stripType typ of
      TypeForall (ForallType var body) ->
        encode env' body
        where
          (tparamList, tparamMap) = cppEnvironmentBoundTypeVariables env
          env' = env { cppEnvironmentBoundTypeVariables = (tparamList ++ [var], M.insert var (unName var) tparamMap)}
      TypeRecord rt -> pure <$> encodeRecordType env name rt comment
      TypeUnion rt -> encodeUnionType env name rt comment
      TypeWrap (WrappedType _ t) -> pure <$> encodeWrappedType env name t comment
      _ -> pure <$> encodeTypeAlias env name typ comment

encodeUnionType :: CppEnvironment -> Name -> RowType -> Maybe String -> Flow Graph [Cpp.Declaration]
encodeUnionType env name rt comment =
  if isEnumRowType rt
    then encodeEnumType env name (rowTypeFields rt) comment
    else if useStdVariants
      then encodeStdVariantUnion env name (rowTypeFields rt) comment
      else encodeClassHierarchyUnion env name (rowTypeFields rt) comment

encodeWrappedType :: CppEnvironment -> Name -> Type -> Maybe String -> Flow Graph Cpp.Declaration
encodeWrappedType env name typ _comment = do
  cppType <- encodeType env typ
  let className = encodeName False CaseConventionPascal env name
  return $ Cpp.DeclarationClass $
    Cpp.ClassDeclaration
      (Cpp.ClassSpecifier Cpp.ClassKeyStruct className [])
      (Just $ Cpp.ClassBody [
        Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationVariable $
          Cpp.VariableDeclaration (Just cppType) "value" Nothing False,
        Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationConstructor $
          Cpp.ConstructorDeclaration
            className
            [Cpp.Parameter cppType "v" Nothing]
            [Cpp.MemInitializer "value" [createIdentifierExpr "v"]]
            (Cpp.CompoundStatement []),
        Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationFunction $
          Cpp.FunctionDeclaration
            []
            cppType
            "getValue"
            []
            [Cpp.FunctionSpecifierSuffixConst]
            (Cpp.FunctionBodyCompound $ Cpp.CompoundStatement [
              Cpp.StatementJump $ Cpp.JumpStatementReturnValue $ createIdentifierExpr "value"])])

--------------------------------------------------------------------------------
-- Helper Functions (Alphabetical Order)
--------------------------------------------------------------------------------

createFactoryMethods :: CppEnvironment -> Name -> [FieldType] -> [Cpp.MemberSpecification]
createFactoryMethods env name variants =
  [Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationFunction $
    Cpp.FunctionDeclaration
      [Cpp.FunctionSpecifierPrefixStatic]
      (Cpp.TypeExpressionTemplate $
        Cpp.TemplateType "std::unique_ptr"
          [Cpp.TemplateArgumentType $
            Cpp.TypeExpressionBasic $
              Cpp.BasicTypeNamed $ encodeName False CaseConventionPascal env name])
      ("make" ++ encodeName False CaseConventionPascal env variantName)
      (if isUnitType variantType
       then []
       else [Cpp.Parameter paramType "value" Nothing])
      []
      (Cpp.FunctionBodyCompound $ Cpp.CompoundStatement [
        Cpp.StatementJump $ Cpp.JumpStatementReturnValue $
          createFunctionCallExpr
            ("std::make_unique<" ++ encodeName False CaseConventionPascal env variantName ++ ">")
            (if isUnitType variantType
             then []
             else [createIdentifierExpr "value"])])
  | FieldType variantName variantType <- variants,
    let paramType = if isUnitType variantType
                    then Cpp.TypeExpressionBasic Cpp.BasicTypeVoid
                    else Cpp.TypeExpressionQualified $
                           Cpp.QualifiedType
                             (Cpp.TypeExpressionQualified $
                               Cpp.QualifiedType
                                 (encodeType' env variantType)
                                 Cpp.TypeQualifierConst)
                             Cpp.TypeQualifierLvalueRef]
  where
    encodeType' :: CppEnvironment -> Type -> Cpp.TypeExpression
    encodeType' env typ = case stripType typ of
      TypeLiteral lt -> case lt of
        LiteralTypeBoolean -> Cpp.TypeExpressionBasic Cpp.BasicTypeBool
        LiteralTypeFloat FloatTypeFloat32 -> Cpp.TypeExpressionBasic Cpp.BasicTypeFloat
        LiteralTypeFloat _ -> Cpp.TypeExpressionBasic Cpp.BasicTypeDouble
        LiteralTypeInteger IntegerTypeInt32 -> Cpp.TypeExpressionBasic Cpp.BasicTypeInt
        LiteralTypeInteger _ -> Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed "int64_t"
        LiteralTypeString -> Cpp.TypeExpressionBasic Cpp.BasicTypeString
        _ -> Cpp.TypeExpressionBasic Cpp.BasicTypeVoid
      _ -> Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed $ encodeName False CaseConventionPascal env name

createPartialVisitorInterface :: CppEnvironment -> Name -> [FieldType] -> Cpp.Declaration
createPartialVisitorInterface env name variants = Cpp.DeclarationTemplate $
  Cpp.TemplateDeclaration False ["typename R"] $
    Cpp.DeclarationClass $
      Cpp.ClassDeclaration
        (Cpp.ClassSpecifier
          Cpp.ClassKeyClass
          (encodeName False CaseConventionPascal env name ++ "PartialVisitor")
          [Cpp.BaseSpecifier Cpp.AccessSpecifierPublic (encodeName False CaseConventionPascal env name ++ "Visitor<R>")])
        (Just $ Cpp.ClassBody (otherwiseMethod : defaultVisitMethods))
  where
    otherwiseMethod = Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationFunction $
      Cpp.FunctionDeclaration
        [Cpp.FunctionSpecifierPrefixVirtual]
        (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed "R")
        "otherwise"
        [Cpp.Parameter
          (Cpp.TypeExpressionQualified $
            Cpp.QualifiedType
              (Cpp.TypeExpressionQualified $
                Cpp.QualifiedType
                  (Cpp.TypeExpressionBasic $
                    Cpp.BasicTypeNamed $
                      encodeName False CaseConventionPascal env name)
                  Cpp.TypeQualifierConst)
              Cpp.TypeQualifierLvalueRef)
          "variant"
          Nothing]
        []
        (Cpp.FunctionBodyCompound $ Cpp.CompoundStatement [
          Cpp.StatementExpression $
            createFunctionCallExpr
              "throw std::runtime_error"
              [createLiteralStringExpr "Non-exhaustive patterns when matching: unhandled case"]])

    defaultVisitMethods = [
      Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationFunction $
        Cpp.FunctionDeclaration
          [Cpp.FunctionSpecifierPrefixVirtual]
          (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed "R")
          ("visit" ++ encodeName False CaseConventionPascal env variantName)
          [Cpp.Parameter
            (Cpp.TypeExpressionQualified $
              Cpp.QualifiedType
                (Cpp.TypeExpressionQualified $
                  Cpp.QualifiedType
                    (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed $
                      encodeName False CaseConventionPascal env variantName)
                    Cpp.TypeQualifierConst)
                Cpp.TypeQualifierLvalueRef)
            "variant"
            Nothing]
          [Cpp.FunctionSpecifierSuffixOverride]
          (Cpp.FunctionBodyCompound $ Cpp.CompoundStatement [
            Cpp.StatementJump $
              Cpp.JumpStatementReturnValue $
                createFunctionCallExpr
                  "otherwise"
                  [createCastExpr
                    (Cpp.TypeExpressionQualified $
                      Cpp.QualifiedType
                        (Cpp.TypeExpressionQualified $
                          Cpp.QualifiedType
                            (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed $
                              encodeName False CaseConventionPascal env name)
                            Cpp.TypeQualifierConst)
                        Cpp.TypeQualifierLvalueRef)
                    createVariantExpr]
          ])
      | FieldType variantName _ <- variants]

createUnionBaseClass :: CppEnvironment -> Name -> [FieldType] -> Flow Graph Cpp.Declaration
createUnionBaseClass env name variants = do
  let className = encodeName False CaseConventionPascal env name

  let privateConstructor = Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationConstructor $
        Cpp.ConstructorDeclaration
          className
          []
          []
          (Cpp.CompoundStatement [])

  let virtualDestructor = Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationDestructor $
        Cpp.DestructorDeclaration
          [Cpp.FunctionSpecifierPrefixVirtual]
          className
          []
          (Cpp.CompoundStatement [])

  let acceptMethod = Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationFunction $
        Cpp.FunctionDeclaration
          [Cpp.FunctionSpecifierPrefixVirtual]
          (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed "R")
          "accept"
          [Cpp.Parameter
            (Cpp.TypeExpressionQualified $
              Cpp.QualifiedType
                (Cpp.TypeExpressionQualified $
                  Cpp.QualifiedType
                    (Cpp.TypeExpressionBasic $
                      Cpp.BasicTypeNamed $ className ++ "Visitor<R>")
                    Cpp.TypeQualifierConst)
                Cpp.TypeQualifierLvalueRef)
            "visitor"
            Nothing]
          [Cpp.FunctionSpecifierSuffixPure]
          Cpp.FunctionBodyDeclaration

  let factoryMethods = createFactoryMethods env name variants

  return $ Cpp.DeclarationTemplate $ Cpp.TemplateDeclaration False ["typename R"] $
    Cpp.DeclarationClass $ Cpp.ClassDeclaration
      (Cpp.ClassSpecifier Cpp.ClassKeyClass className [])
      (Just $ Cpp.ClassBody (
        [ Cpp.MemberSpecificationAccessLabel Cpp.AccessSpecifierPrivate
        , privateConstructor
        , Cpp.MemberSpecificationAccessLabel Cpp.AccessSpecifierPublic
        , virtualDestructor
        , acceptMethod
        ] ++ factoryMethods))

createVariantClass :: CppEnvironment -> Name -> Name -> FieldType -> Flow Graph Cpp.Declaration
createVariantClass env parentName parentClass (FieldType variantName variantType) = do
  let hasValue = not (isUnitType variantType)

  valueField <- if hasValue
                then do
                  strippedType <- encodeType env (stripType variantType)
                  return [Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationVariable $
                    Cpp.VariableDeclaration (Just strippedType) "value" Nothing False]
                else return []

  constructorParams <- if hasValue
                       then do
                         paramType <- encodeType env (stripType variantType)
                         return [Cpp.Parameter paramType "value" Nothing]
                       else return []

  let constructorInitList = if hasValue
                            then [Cpp.MemInitializer "value" [createIdentifierExpr "value"]]
                            else []

  let constructor = Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationConstructor $
        Cpp.ConstructorDeclaration
          (encodeName False CaseConventionPascal env variantName)
          constructorParams
          constructorInitList
          (Cpp.CompoundStatement [])

  let visitorType = Cpp.TypeExpressionTemplate $
                      Cpp.TemplateType
                        (encodeName False CaseConventionPascal env parentName ++ "Visitor")
                        [Cpp.TemplateArgumentType $
                          Cpp.TypeExpressionBasic $
                          Cpp.BasicTypeNamed "R"]

  let constVisitorType = Cpp.TypeExpressionQualified $
                           Cpp.QualifiedType visitorType Cpp.TypeQualifierConst

  let visitorParam = Cpp.Parameter
                       (Cpp.TypeExpressionQualified $
                         Cpp.QualifiedType constVisitorType Cpp.TypeQualifierLvalueRef)
                       "visitor"
                       Nothing

  let acceptMethod = Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationFunction $
        Cpp.FunctionDeclaration
          [Cpp.FunctionSpecifierPrefixVirtual]
          (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed "R")
          "accept"
          [visitorParam]
          [Cpp.FunctionSpecifierSuffixConst, Cpp.FunctionSpecifierSuffixPure]
          Cpp.FunctionBodyDeclaration

  let baseClass = Cpp.BaseSpecifier Cpp.AccessSpecifierPublic $
                    encodeName False CaseConventionPascal env parentClass

  return $ Cpp.DeclarationClass $
    Cpp.ClassDeclaration
      (Cpp.ClassSpecifier Cpp.ClassKeyClass (encodeName False CaseConventionPascal env variantName) [baseClass])
      (Just $ Cpp.ClassBody (valueField ++ [constructor, acceptMethod]))

createVisitorInterface :: CppEnvironment -> Name -> [FieldType] -> Cpp.Declaration
createVisitorInterface env name variants = Cpp.DeclarationTemplate $
  Cpp.TemplateDeclaration False ["typename R"] $
    Cpp.DeclarationClass $
      Cpp.ClassDeclaration
        (Cpp.ClassSpecifier
          Cpp.ClassKeyClass
          (encodeName False CaseConventionPascal env name ++ "Visitor")
          [])
        (Just $ Cpp.ClassBody (visitMethods ++ [virtualDestructor]))
  where
    visitMethods = [
      Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationFunction $
        Cpp.FunctionDeclaration
          [Cpp.FunctionSpecifierPrefixVirtual]
          (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed "R")
          ("visit" ++ encodeName False CaseConventionPascal env variantName)
          [Cpp.Parameter
            (Cpp.TypeExpressionQualified $
              Cpp.QualifiedType
                (Cpp.TypeExpressionQualified $
                  Cpp.QualifiedType
                    (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed $
                      encodeName False CaseConventionPascal env variantName)
                    Cpp.TypeQualifierConst)
                Cpp.TypeQualifierLvalueRef)
            "variant"
            Nothing]
          [Cpp.FunctionSpecifierSuffixConst, Cpp.FunctionSpecifierSuffixPure]
          (Cpp.FunctionBodyCompound $ Cpp.CompoundStatement [])
      | FieldType variantName _ <- variants]

    virtualDestructor = Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationDestructor $
      Cpp.DestructorDeclaration
        []
        (encodeName False CaseConventionPascal env name ++ "Visitor")
        []
        (Cpp.CompoundStatement [])

gatherMetadata :: [Definition] -> CppModuleMetadata
gatherMetadata defs = L.foldl addDef start defs
  where
    start = CppModuleMetadata {
      cppModuleMetadataTypeVariables = S.empty,
      cppModuleMetadataUsesVariant = False,
      cppModuleMetadataUsesOptional = False,
      cppModuleMetadataUsesVector = False,
      cppModuleMetadataUsesMap = False,
      cppModuleMetadataUsesSet = False,
      cppModuleMetadataUsesString = False,
      cppModuleMetadataUsesMemory = False,
      cppModuleMetadataUsesFunctional = False,
      cppModuleMetadataUsesTypeTraits = True,
      cppModuleMetadataUsesIOStream = False,
      cppModuleMetadataUsesAlgorithm = False}

    addDef meta def = case def of
      DefinitionTerm (TermDefinition _ term typ) ->
        foldOverTerm TraversalOrderPre extendMetaForTerm (extendMetaForType meta typ) term
      DefinitionType (TypeDefinition _ typ) ->
        foldOverType TraversalOrderPre extendMetaForType meta typ

    extendMetaForTerm meta t = case t of
      TermLet (Let bindings _) -> L.foldl forBinding meta bindings
        where
          forBinding m (LetBinding _ _ mts) = case mts of
            Nothing -> m
            Just ts -> extendMetaForType m $ typeSchemeType ts
      TermMap _ -> meta {cppModuleMetadataUsesMap = True}
      TermList _ -> meta {cppModuleMetadataUsesVector = True, cppModuleMetadataUsesAlgorithm = True}
      TermSet _ -> meta {cppModuleMetadataUsesSet = True, cppModuleMetadataUsesAlgorithm = True}
      TermLiteral (LiteralString _) -> meta {cppModuleMetadataUsesString = True}
      TermFunction _ -> meta {cppModuleMetadataUsesFunctional = True}
      TermApplication _ -> meta {cppModuleMetadataUsesFunctional = True}
      TermOptional _ -> meta {cppModuleMetadataUsesOptional = True}
      _ -> meta

    extendMetaForType meta typ = case stripType typ of
      TypeFunction _ -> meta {cppModuleMetadataUsesFunctional = True}
      TypeForall (ForallType _ body) ->
        meta {cppModuleMetadataTypeVariables = S.union (cppModuleMetadataTypeVariables meta) (freeVariablesInType body)}
      TypeList _ -> meta {cppModuleMetadataUsesVector = True, cppModuleMetadataUsesAlgorithm = True}
      TypeMap _ -> meta {cppModuleMetadataUsesMap = True}
      TypeLiteral lt -> case lt of
        LiteralTypeString -> meta {cppModuleMetadataUsesString = True}
        LiteralTypeFloat _ -> meta {cppModuleMetadataUsesIOStream = True}
        _ -> meta
      TypeOptional _ -> meta {cppModuleMetadataUsesOptional = True}
      TypeRecord _ -> meta {cppModuleMetadataUsesMemory = True}
      TypeSet _ -> meta {cppModuleMetadataUsesSet = True}
      TypeUnion rt ->
        if isEnumRowType rt
          then meta
          else if useStdVariants
            then meta {cppModuleMetadataUsesVariant = True, cppModuleMetadataUsesFunctional = True}
            else meta {cppModuleMetadataUsesMemory = True, cppModuleMetadataUsesFunctional = True}
      TypeVariable name -> meta {cppModuleMetadataTypeVariables = S.insert name (cppModuleMetadataTypeVariables meta)}
      TypeWrap _ -> meta {cppModuleMetadataUsesMemory = True}
      TypeApplication _ -> meta
      _ -> meta

isStructType :: Type -> Bool
isStructType typ =
  typeVariant (fullyStripType typ) == TypeVariantRecord ||
  (not useStdVariants && not (isEnumType typ) && typeVariant (fullyStripType typ) == TypeVariantUnion)
