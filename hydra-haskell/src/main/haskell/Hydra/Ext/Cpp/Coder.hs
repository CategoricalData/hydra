-- Note: this file was created with the help of a large language model. It requires further human review.

module Hydra.Ext.Cpp.Coder (moduleToCpp) where

import Hydra.Kernel
import Hydra.Staging.Adapters
import Hydra.Ext.Cpp.Language
import Hydra.Dsl.Terms
import Hydra.Staging.Serialization
import qualified Hydra.Ext.Cpp.Syntax as Cpp
import Hydra.Ext.Cpp.Names
import Hydra.Ext.Cpp.Utils
import qualified Hydra.Ext.Cpp.Serde as CppSer
import qualified Hydra.Dsl.Types as Types
import Hydra.Dsl.ShorthandTypes
import Hydra.Lib.Io
import Hydra.Staging.Formatting
import qualified Hydra.Decode as Decode

import qualified Control.Monad as CM
import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Set   as S
import qualified Data.Maybe as Y
import qualified Text.Read  as TR


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
  cppModuleMetadataUsesAlgorithm :: Bool}

-- | Convert a module to C++ code files
moduleToCpp :: Module -> Flow Graph (M.Map FilePath String)
moduleToCpp mod = do
  program <- encodeModule mod

  -- Generate header file
  let header = printExpr $ parenthesize $ CppSer.encodeProgram program

  -- Generate implementation file with template specializations
  let implContent = generateImplementationFile mod

  -- Set file paths for header and implementation files
  let headerPath = namespaceToFilePath CaseConventionLowerSnake (FileExtension "h") $ moduleNamespace mod
  let implPath = namespaceToFilePath CaseConventionLowerSnake (FileExtension "cpp") $ moduleNamespace mod

  -- Return both header and implementation files
  return $ M.fromList [
    (headerPath, header),
    (implPath, implContent)]

-- | Generate implementation file with template specializations and implementation details
generateImplementationFile :: Module -> String
generateImplementationFile mod =
  "// Generated C++ implementation file\n\n" ++
  "#include \"" ++ (namespaceToFilePath CaseConventionLowerSnake (FileExtension "h") $ moduleNamespace mod) ++ "\"\n\n" ++
  "namespace " ++ unNamespace (moduleNamespace mod) ++ " {\n\n" ++
  "// Add implementation-specific code here\n\n" ++
  "} // namespace " ++ unNamespace (moduleNamespace mod) ++ "\n"

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
      Cpp.TypeExpressionBasic (Cpp.BasicTypeNamed name) ->
        createTemplateType name args
      _ -> error "Non-named type in template application"

encodeDefinition :: CppEnvironment -> Definition -> Flow Graph [Cpp.Declaration]
encodeDefinition env def = case def of
  DefinitionTerm name term typ -> withTrace ("data element " ++ unName name) $ do
    fail "term-level encoding is not yet supported"
  DefinitionType name typ -> withTrace ("type element " ++ unName name) $ do
    comment <- fmap normalizeComment <$> getTypeDescription typ
    encodeTypeDefinition env name typ comment

encodeEnumType :: CppEnvironment -> Name -> [FieldType] -> Maybe String -> Flow Graph [Cpp.Declaration]
encodeEnumType env name tfields comment = do
  return [Cpp.DeclarationClass $
    Cpp.ClassDeclaration
      (Cpp.ClassSpecifier Cpp.ClassKeyClass (encodeName False CaseConventionPascal env name) [])
      (Cpp.ClassBody $ [Cpp.MemberSpecificationAccessLabel Cpp.AccessSpecifierPublic] ++ enumFields ++ enumMethods)]
  where
    enumFields = [Cpp.MemberSpecificationMember $
                    Cpp.MemberDeclarationVariable $
                      Cpp.VariableDeclaration
                        (Cpp.TypeExpressionBasic $ Cpp.BasicTypeInt)
                        (encodeEnumValue env fname)
                        (Just $ createLiteralIntExpr idx)
                        False
                | (FieldType fname _, idx) <- zip tfields [0..]]

    enumMethods = [
      -- Create a toString method for enums
      Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationFunction $
        Cpp.FunctionDeclaration
          (Cpp.TypeExpressionBasic Cpp.BasicTypeString)
          "toString"
          [createParameter (Cpp.TypeExpressionBasic $ Cpp.BasicTypeInt) "value" Nothing]
          [Cpp.FunctionSpecifierStatic]
          (Cpp.FunctionBodyCompound $ createCompoundStmt [
            createSwitchStatement
          ])]

    createSwitchStatement = Cpp.StatementSelection $ Cpp.SelectionStatement
      (createIdentifierExpr "value")
      (Cpp.StatementCompound $ createCompoundStmt switchCases)
      Nothing

    switchCases = enumCases ++ [defaultCase]
      where
        enumCases = [
          Cpp.StatementLabeled (Cpp.LabeledStatement
            ("case " ++ show idx)
            (Cpp.StatementJump $ Cpp.JumpStatementReturnValue $
              createLiteralStringExpr (unName fname)))
          | (FieldType fname _, idx) <- zip tfields [0..]]

        defaultCase = Cpp.StatementLabeled (Cpp.LabeledStatement
          "default"
          (Cpp.StatementJump $ Cpp.JumpStatementReturnValue $
            createLiteralStringExpr "Unknown"))

encodeFieldType :: CppEnvironment -> FieldType -> Flow Graph Cpp.VariableDeclaration
encodeFieldType env (FieldType fname ftype) = do
  comment <- getTypeDescription ftype
  cppType <- encodeType env ftype
  return $ Cpp.VariableDeclaration
    cppType
    (encodeFieldName env fname)
    Nothing
    False

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
    paramFromType t = createParameter t "" Nothing

encodeForallType :: CppEnvironment -> ForallType -> Flow Graph Cpp.TypeExpression
encodeForallType env lt = do
    cppBody <- encodeType env body
    return cppBody  -- C++ handles type variables differently, so we just return the body type
  where
    (body, _) = gatherParams (TypeForall lt) []
    gatherParams t ps = case stripType t of
      TypeForall (ForallType name body) -> gatherParams body (name:ps)
      _ -> (t, L.reverse ps)

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
    let namespaces = namespacesForDefinitions encodeNamespace (moduleNamespace mod) defs
    let env = CppEnvironment {
              cppEnvironmentNamespaces = namespaces,
              cppEnvironmentBoundTypeVariables = ([], M.empty)}
    defDecls <- L.concat <$> (CM.mapM (encodeDefinition env) defs)
    let meta = gatherMetadata defs
    let includeDirectives = includes meta
    return $ Cpp.Program includeDirectives (wrapWithNamespace (moduleNamespace mod) defDecls)
  where
    wrapWithNamespace ns decls = [Cpp.DeclarationNamespace $
      Cpp.NamespaceDeclaration (unNamespace ns) decls]

    includes meta = addVersionInclude $ standardIncludes ++ containerIncludes
      where
        addVersionInclude includes =
          Cpp.IncludeDirective "version" True : includes

        standardIncludes = [
          Cpp.IncludeDirective "iostream" True,
          Cpp.IncludeDirective "string" True,
          Cpp.IncludeDirective "cstdint" True]

        containerIncludes = Y.catMaybes [
          if cppModuleMetadataUsesVector meta then Just (Cpp.IncludeDirective "vector" True) else Nothing,
          if cppModuleMetadataUsesMap meta then Just (Cpp.IncludeDirective "map" True) else Nothing,
          if cppModuleMetadataUsesSet meta then Just (Cpp.IncludeDirective "set" True) else Nothing,
          if cppModuleMetadataUsesVariant meta then Just (Cpp.IncludeDirective "variant" True) else Nothing,
          if cppModuleMetadataUsesOptional meta then Just (Cpp.IncludeDirective "optional" True) else Nothing,
          if cppModuleMetadataUsesMemory meta then Just (Cpp.IncludeDirective "memory" True) else Nothing,
          if cppModuleMetadataUsesFunctional meta then Just (Cpp.IncludeDirective "functional" True) else Nothing,
          if cppModuleMetadataUsesTypeTraits meta then Just (Cpp.IncludeDirective "type_traits" True) else Nothing]

encodeRecordType :: CppEnvironment -> Name -> RowType -> Maybe String -> Flow Graph Cpp.Declaration
encodeRecordType env name (RowType _ tfields) comment = do
    cppFields <- CM.mapM (encodeFieldType env) tfields
    return $ Cpp.DeclarationClass $
      Cpp.ClassDeclaration
        (Cpp.ClassSpecifier Cpp.ClassKeyStruct (encodeName False CaseConventionPascal env name) [])
        (Cpp.ClassBody $ publicSection cppFields ++ createStructMethods cppFields)
  where
    publicSection cppFields = [
      Cpp.MemberSpecificationAccessLabel Cpp.AccessSpecifierPublic] ++
      [Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationVariable field | field <- cppFields]

    -- Add default constructor, destructor, copy/move operations if needed
    createStructMethods fields = [
      Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationConstructor $
        Cpp.ConstructorDeclaration
          (encodeName False CaseConventionPascal env name)
          []  -- No parameters for default constructor
          []  -- No initializers
          (createCompoundStmt []),

      Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationConstructor $
        Cpp.ConstructorDeclaration
          (encodeName False CaseConventionPascal env name)
          [createParameter (createConstRefType $ createBasicType $ encodeName False CaseConventionPascal env name) "other" Nothing]
          [Cpp.MemInitializer (fieldVarName field) [createMemberAccessExpr (createIdentifierExpr "other") (fieldVarName field)]
            | field <- fields]
          (createCompoundStmt [])]
      where
        fieldVarName (Cpp.VariableDeclaration _ name _ _) = name

encodeType :: CppEnvironment -> Type -> Flow Graph Cpp.TypeExpression
encodeType env typ = case stripType typ of
    TypeApplication at -> encodeApplicationType env at
    TypeFunction ft -> encodeFunctionType env ft
    TypeForall lt -> encodeForallType env lt
    TypeList et -> do
      elemType <- encode et
      return $ createTemplateType "std::vector" [elemType]
    TypeMap (MapType kt vt) -> do
      keyType <- encode kt
      valueType <- encode vt
      return $ createTemplateType "std::map" [keyType, valueType]
    TypeLiteral lt -> encodeLiteralType lt
    TypeOptional et -> do
      elemType <- encode et
      return $ createTemplateType "std::optional" [elemType]
    TypeRecord rt -> pure $ createBasicType $ unName $ rowTypeTypeName rt
    TypeSet et -> do
      elemType <- encode et
      return $ createTemplateType "std::set" [elemType]
    TypeUnion rt -> pure $ createBasicType $ unName $ rowTypeTypeName rt
    TypeVariable name -> pure $ createBasicType $ unName name
    TypeWrap (WrappedType name _) -> pure $ createBasicType $ unName name
    _ -> dflt
  where
    encode = encodeType env
    dflt = fail $ "Unsupported type: " ++ show (stripType typ)

encodeTypeDefinition :: CppEnvironment -> Name -> Type -> Maybe String -> Flow Graph [Cpp.Declaration]
encodeTypeDefinition env name typ comment = encode env typ
  where
    encode env typ = case stripType typ of
      TypeForall (ForallType var body) -> encode newEnv body
        where
          (tparamList, tparamMap) = cppEnvironmentBoundTypeVariables env
          newEnv = env {cppEnvironmentBoundTypeVariables = (tparamList ++ [var], M.insert var (unName var) tparamMap)}
      TypeRecord rt -> single <$> encodeRecordType env name rt comment
      TypeUnion rt -> encodeUnionType env name rt comment
      TypeWrap (WrappedType _ t) -> single <$> encodeWrappedType env name t comment
      t -> single <$> encodeTypeAlias env name t comment
    single decl = [decl]

encodeTypeAlias :: CppEnvironment -> Name -> Type -> Maybe String -> Flow Graph Cpp.Declaration
encodeTypeAlias env name typ comment = do
  cppType <- encodeType env typ
  return $ Cpp.DeclarationTypedef $
    Cpp.TypedefDeclaration
      (encodeName False CaseConventionPascal env name)
      cppType
      True -- Use "using" syntax instead of "typedef"

encodeUnionType :: CppEnvironment -> Name -> RowType -> Maybe String -> Flow Graph [Cpp.Declaration]
encodeUnionType env name rt@(RowType _ tfields) comment =
  if isEnumType rt
    then encodeEnumType env name tfields comment
    else encodeVariantType env name tfields comment

-- | Process a variant type (std::variant)
encodeVariantType :: CppEnvironment -> Name -> [FieldType] -> Maybe String -> Flow Graph [Cpp.Declaration]
encodeVariantType env name tfields comment = do
  -- Encode each field type
  variantTypes <- CM.mapM (encodeType env . fieldType) tfields

  -- Create a typedef declaration for the variant
  let typedefDecl = Cpp.DeclarationTypedef $
        Cpp.TypedefDeclaration
          (encodeName False CaseConventionPascal env name)
          (createTemplateType "std::variant" variantTypes)
          True  -- Use "using" syntax instead of "typedef"

  -- Create helper function to identify the active type in a variant
  let helperFunc = Cpp.DeclarationFunction $
        Cpp.FunctionDeclaration
          (Cpp.TypeExpressionBasic Cpp.BasicTypeString)
          ("get" ++ encodeName False CaseConventionPascal env name ++ "Type")
          [Cpp.Parameter
            (createConstRefType $ createBasicType $ encodeName False CaseConventionPascal env name)
            "value"
            Nothing]
          []
          (Cpp.FunctionBodyCompound $ Cpp.CompoundStatement [
            Cpp.StatementJump $ Cpp.JumpStatementReturnValue $
              createFunctionCallExpr "std::visit" [
                createLambdaExpr,
                createIdentifierExpr "value"
              ]
          ])

  return [typedefDecl, helperFunc]
  where
    fieldType (FieldType _ ft) = ft

    -- Helper to create a template type like std::variant<T1, T2, ...>
    createTemplateType name args =
      Cpp.TypeExpressionTemplate $
        Cpp.TemplateType name [Cpp.TemplateArgumentType a | a <- args]

encodeWrappedType :: CppEnvironment -> Name -> Type -> Maybe String -> Flow Graph Cpp.Declaration
encodeWrappedType env name typ comment = do
  cppType <- encodeType env typ
  let className = encodeName False CaseConventionPascal env name
  return $ Cpp.DeclarationClass $
    Cpp.ClassDeclaration
      (Cpp.ClassSpecifier Cpp.ClassKeyStruct className [])
      (Cpp.ClassBody $ [Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationVariable $
        Cpp.VariableDeclaration cppType "value" Nothing False,

       -- Simple constructor
       Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationConstructor $
        Cpp.ConstructorDeclaration
          className
          [Cpp.Parameter cppType "v" Nothing]
          [Cpp.MemInitializer "value" [createIdentifierExpr "v"]]
          (Cpp.CompoundStatement []),

       -- Simple getter
       Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationFunction $
        Cpp.FunctionDeclaration
          cppType
          "getValue"
          []
          [Cpp.FunctionSpecifierConst]
          (Cpp.FunctionBodyCompound $ Cpp.CompoundStatement [
            Cpp.StatementJump $ Cpp.JumpStatementReturnValue $ createIdentifierExpr "value"
          ])])

-- | Gather metadata about used types to determine required includes
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
      cppModuleMetadataUsesTypeTraits = False,
      cppModuleMetadataUsesIOStream = False,
      cppModuleMetadataUsesAlgorithm = False}

    addDef meta def = case def of
      DefinitionTerm _ term typ -> foldOverTerm TraversalOrderPre extendMetaForTerm (extendMetaForType meta typ) term
      DefinitionType _ typ -> foldOverType TraversalOrderPre extendMetaForType meta typ

    extendMetaForTerm meta t = case t of
      TermLet (Let bindings _) -> L.foldl forBinding meta bindings
        where
          forBinding meta (LetBinding _ _ mts) = case mts of
            Nothing -> meta
            Just ts -> extendMetaForType meta $ typeSchemeType ts
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
      TypeForall (ForallType _ body) -> meta {
        cppModuleMetadataUsesTypeTraits = True,
        cppModuleMetadataTypeVariables = S.union (cppModuleMetadataTypeVariables meta) (freeVariablesInType body)}
      TypeList _ -> meta {cppModuleMetadataUsesVector = True, cppModuleMetadataUsesAlgorithm = True}
      TypeMap _ -> meta {cppModuleMetadataUsesMap = True}
      TypeLiteral lt -> case lt of
        LiteralTypeString -> meta {cppModuleMetadataUsesString = True}
        LiteralTypeFloat _ -> meta {cppModuleMetadataUsesIOStream = True}
        _ -> meta
      TypeOptional _ -> meta {cppModuleMetadataUsesOptional = True}
      TypeRecord (RowType _ fields) -> L.foldl (checkFieldTypes) meta fields
      TypeSet _ -> meta {cppModuleMetadataUsesSet = True}
      TypeUnion rt -> if isEnumType rt
        then meta
        else meta {
          cppModuleMetadataUsesVariant = True,
          cppModuleMetadataUsesTypeTraits = True,
          cppModuleMetadataUsesFunctional = True}
      TypeVariable name -> meta {
        cppModuleMetadataUsesTypeTraits = True,
        cppModuleMetadataTypeVariables = S.insert name (cppModuleMetadataTypeVariables meta)}
      TypeWrap _ -> meta {cppModuleMetadataUsesMemory = True}
      TypeApplication _ -> meta {cppModuleMetadataUsesTypeTraits = True}
      _ -> meta

    -- Check each field in a record type for additional metadata
    checkFieldTypes meta (FieldType _ ft) = extendMetaForType meta ft

    -- Check if a row type is an enum (all unit fields)
    isEnumType (RowType _ fields) = all isUnitField fields
      where isUnitField (FieldType _ ft) = isUnitType (stripType ft)
