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
  DefinitionTerm _ -> fail "term-level encoding is not yet supported"
  DefinitionType (TypeDefinition name typ) -> withTrace ("type element " ++ unName name) $ do
    comment <- fmap normalizeComment <$> getTypeDescription typ
    encodeTypeDefinition env name typ comment

encodeEnumType :: CppEnvironment -> Name -> [FieldType] -> Maybe String -> Flow Graph [Cpp.Declaration]
encodeEnumType env name tfields comment = do
  return [Cpp.DeclarationClass $
    Cpp.ClassDeclaration
      (Cpp.ClassSpecifier Cpp.ClassKeyEnumClass (encodeName False CaseConventionPascal env name) [])
      (Just $ Cpp.ClassBody enumFields)]
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
    comment <- getTypeDescription ftype
    cppType <- encodeType env ftype

    -- Apply different type modifiers based on whether this is a parameter or field
    let finalType = if isParameter
                    then parameterType cppType
                    else fieldType cppType

    return $ Cpp.VariableDeclaration
      (Just finalType)
      (encodeFieldName env fname)
      Nothing
      False
  where
    isBasicType = typeVariant (stripType ftype) == TypeVariantLiteral
    isTemplateType = isStdContainer ftype

    -- For fields: Apply const to non-basic types
    fieldType typ = if isBasicType
                    then typ
                    else Cpp.TypeExpressionQualified $ Cpp.QualifiedType typ Cpp.TypeQualifierConst

    -- For parameters: Apply different rules based on type
    parameterType typ
      | isBasicType = typ                               -- Basic types by value
      | isTemplateType = typ                           -- Template types by value (for std::move)
      | otherwise = Cpp.TypeExpressionQualified $     -- Other complex types by const reference
                     Cpp.QualifiedType
                       (Cpp.TypeExpressionQualified $
                         Cpp.QualifiedType typ Cpp.TypeQualifierConst)
                       Cpp.TypeQualifierLvalueRef

    -- Check if type is a std container like optional, vector, etc.
    isStdContainer typ = case stripType typ of
      TypeApplication (ApplicationType lhs _) -> isStdContainer lhs
      TypeList _ -> True
      TypeMap _ -> True
      TypeOptional _ -> True
      TypeSet _ -> True
      _ -> False

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
    let forwardDecls = forwardDeclarationsForDefs env defs
    defDecls <- L.concat <$> (CM.mapM (encodeDefinition env) defs)
    let meta = gatherMetadata defs
    let includeDirectives = includes meta
    return $ Cpp.Program includeDirectives (wrapWithNamespace (moduleNamespace mod) (forwardDecls ++ defDecls))
  where
    wrapWithNamespace ns decls = [Cpp.DeclarationNamespace $
      Cpp.NamespaceDeclaration (encodeNamespace ns) decls]

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

    forwardDeclarationsForDefs env defs = Y.catMaybes (toDecl <$> defs)
      where
        toDecl def = case def of
          DefinitionType (TypeDefinition name typ) -> case stripType typ of
            TypeRecord _ -> Just $ Cpp.DeclarationClass $
                  Cpp.ClassDeclaration
                    (Cpp.ClassSpecifier Cpp.ClassKeyStruct (encodeName False CaseConventionPascal env name) [])
                    Nothing
            _ -> Nothing
          _ -> Nothing

encodeRecordType :: CppEnvironment -> Name -> RowType -> Maybe String -> Flow Graph Cpp.Declaration
encodeRecordType env name (RowType _ tfields) comment = do
    -- Get field declarations for member variables
    cppFields <- CM.mapM (encodeFieldType env False) tfields

    -- Generate constructor parameters in the monadic context
    constructorParams <- createParameters tfields

    return $ Cpp.DeclarationClass $
      Cpp.ClassDeclaration
        (Cpp.ClassSpecifier Cpp.ClassKeyStruct (encodeName False CaseConventionPascal env name) [])
        (Just $ Cpp.ClassBody $ publicSection cppFields ++ createStructMethods cppFields constructorParams)
  where
    publicSection cppFields =
      [Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationVariable field | field <- cppFields]

    createStructMethods fields params = [
      Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationConstructor $
        Cpp.ConstructorDeclaration
          (encodeName False CaseConventionPascal env name)
          params  -- Use pre-computed parameters
          [Cpp.MemInitializer
             (Cpp.variableDeclarationName field)
             [createInitializerExpr field (Cpp.variableDeclarationName field)]
           | field <- fields]
          (Cpp.CompoundStatement [])]
      where
        -- Create initializer expression with std::move for template types
        createInitializerExpr field paramName =
          if isTemplateType field
            then createFunctionCallExpr "std::move" [createIdentifierExpr paramName]  -- Use std::move
            else createIdentifierExpr paramName  -- Use direct reference

        -- Check if a field has a template type
        isTemplateType (Cpp.VariableDeclaration (Just typ) _ _ _) =
          case typ of
            Cpp.TypeExpressionQualified (Cpp.QualifiedType innerType _) -> isTemplateTypeExpr innerType
            _ -> isTemplateTypeExpr typ

        isTemplateTypeExpr typ = case typ of
          Cpp.TypeExpressionTemplate _ -> True  -- Template types like std::optional<T>
          _ -> False

    createParameters origFields = do
      let fieldNames = [fname | FieldType fname _ <- origFields]
      CM.zipWithM createParam fieldNames origFields

    createParam fieldName (FieldType _ ftype) = do
      paramDecl <- encodeFieldType env True (FieldType fieldName ftype)
      return $ Cpp.Parameter
        (variableDeclarationType paramDecl)
        (variableDeclarationName paramDecl)
        Nothing

    fieldVarName (Cpp.VariableDeclaration _ name _ _) = name

    variableDeclarationType (Cpp.VariableDeclaration (Just typ) _ _ _) = typ

    variableDeclarationName (Cpp.VariableDeclaration _ name _ _) = name

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
    TypeRecord rt -> pure $ createTypeReference env $ rowTypeTypeName rt
    TypeSet et -> do
      elemType <- encode et
      return $ createTemplateType "std::set" [elemType]
    TypeUnion rt -> pure $ createTypeReference env $ rowTypeTypeName rt
    TypeVariable name -> pure $ createTypeReference env name
    TypeWrap (WrappedType name _) -> pure $ createTypeReference env name
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
  variantTypes <- CM.mapM (encodeType env . fieldTypeType) tfields

  -- Create a typedef declaration for the variant
  let typedefDecl = Cpp.DeclarationTypedef $
        Cpp.TypedefDeclaration
          (encodeName False CaseConventionPascal env name)
          (createTemplateType "std::variant" variantTypes)
          True  -- Use "using" syntax instead of "typedef"

  return [typedefDecl]
  where
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
      (Just $ Cpp.ClassBody $ [Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationVariable $
        Cpp.VariableDeclaration (Just cppType) "value" Nothing False,

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
      DefinitionTerm (TermDefinition _ term typ) -> foldOverTerm TraversalOrderPre extendMetaForTerm (extendMetaForType meta typ) term
      DefinitionType (TypeDefinition _ typ) -> foldOverType TraversalOrderPre extendMetaForType meta typ

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
