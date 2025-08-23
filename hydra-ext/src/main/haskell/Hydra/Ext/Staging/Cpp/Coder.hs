-- Note: this file was created with the help of a large language model. It requires further human review.

module Hydra.Ext.Staging.Cpp.Coder (moduleToCpp) where

import Hydra.Kernel
import Hydra.Dsl.ShorthandTypes
import Hydra.Dsl.Terms
import Hydra.Ext.Cpp.Language
import Hydra.Ext.Staging.Cpp.Names
import Hydra.Ext.Staging.Cpp.Utils
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Ext.Staging.Cpp.Serde as CppSer
import qualified Hydra.Ext.Cpp.Syntax as Cpp
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Lib.Strings as Strings
import Hydra.Adapt.Modules
import Hydra.Formatting

import qualified Control.Monad as CM
import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y
import qualified Data.Set as S
import qualified Text.Read as TR

--------------------------------------------------------------------------------
-- Datatypes

-- | Temporary metadata which is used to create the header section of a C++ file
data CppModuleMetadata = CppModuleMetadata {
  cppModuleMetadataTypeVariables :: S.Set Name,
  cppModuleMetadataUsesMap :: Bool,
  cppModuleMetadataUsesOptional :: Bool,
  cppModuleMetadataUsesSet :: Bool,
  cppModuleMetadataUsesString :: Bool,
  cppModuleMetadataUsesTuple :: Bool,
  cppModuleMetadataUsesTypeinfo :: Bool,
  cppModuleMetadataUsesVector :: Bool}

--------------------------------------------------------------------------------
-- Entry point

-- | Convert a module to C++ code files
moduleToCpp :: Module -> Flow Graph (M.Map FilePath String)
moduleToCpp mod = do
    defs <- adaptedModuleDefinitions cppLanguage mod
    let namespaces = namespacesForDefinitions encodeNamespace (moduleNamespace mod) defs
    let env = CppEnvironment {
      cppEnvironmentNamespaces = namespaces,
      cppEnvironmentBoundTypeVariables = ([], M.empty)}

    let (typeDefs, termDefs) = E.partitionEithers $ fmap toEither defs

    typeFiles <- generateTypeFiles env ns typeDefs

    return $ M.fromList typeFiles -- TODO: also generate a term-level *.cpp file if nonempty
  where
    ns = moduleNamespace mod
    toEither d = case d of
      DefinitionType t -> Left t
      DefinitionTerm t -> Right t

generateTypeFile :: CppEnvironment -> TypeDefinition -> Flow Graph (FilePath, String)
generateTypeFile env def@(TypeDefinition name typ) = withTrace ("type definition " ++ show (unName name)) $ do
    decls <- encodeTypeDefinition env name typ
    return $ serializeHeaderFile name includes [namespaceDecl ns decls]
  where
    ns = Y.fromJust $ namespaceOf name
    includes = findIncludes True ns [def]

generateTypeFiles :: CppEnvironment -> Namespace -> [TypeDefinition] -> Flow Graph [(FilePath, String)]
generateTypeFiles env ns defs = do
    fwdFile <- createFwdFile
    classFiles <- CM.mapM (generateTypeFile env) classDefs
    return (fwdFile:classFiles)
  where
    (usingDefs, classDefs) = L.partition (isUsingDef . typeDefinitionType) defs
      where
        isUsingDef typ = case deannotateType typ of
          TypeForall (ForallType _ body) -> isUsingDef body
          TypeRecord _ -> False
          TypeUnion _ -> False
          TypeWrap _ -> False
          _ -> True
    createFwdFile = do
        usingDecls <- CM.mapM usingDecl sortedUsingDefs
        return $ serializeHeaderFile (fwdHeaderName ns) includes [
          namespaceDecl ns $ (fmap classDecl sortedClassDefs) ++ usingDecls]
      where
        includes = findIncludes False ns usingDefs
        usingDecl (TypeDefinition name typ) = do
          comment <- fmap normalizeComment <$> getTypeDescription typ
          encodeTypeAlias env name typ comment
        classDecl (TypeDefinition name typ) = if isEnumType typ
          then cppEnumForwardDeclaration $ localNameOf name
          else cppClassDeclaration (localNameOf name) [] Nothing
        sortedClassDefs = sortAlphabetical classDefs
        sortedUsingDefs = if noChains
            then sortAlphabetical usingDefs
            -- Note: with a little more effort, it should be possible to have an approximation of
            --       alphabetical order for the using definitions, even when some of them are chained.
            else Y.catMaybes $ fmap (\n -> M.lookup n defByName) $ L.concat comps
          where
            names = S.fromList $ fmap typeDefinitionName defs
            defByName = M.fromList $ fmap (\def -> (typeDefinitionName def, def)) usingDefs
            toPair def = (
              typeDefinitionName def,
              L.filter (\n -> S.member n names) $
                S.toList $ typeDependencyNames True $ typeDefinitionType def)
            pairs = fmap toPair usingDefs
            comps = topologicalSortComponents pairs
            noChains = L.foldl (\b (_, outs) -> b && L.null outs) True pairs
        sortAlphabetical defs = M.elems $ M.fromList $ fmap (\def -> (typeDefinitionName def, def)) defs


--------------------------------------------------------------------------------
-- Encoding functions

encodeApplicationType :: CppEnvironment -> ApplicationType -> Flow Graph Cpp.TypeExpression
encodeApplicationType env at = do
  cppBody <- encodeType env body
  cppArgs <- CM.mapM (encodeType env) args
  return $ createTemplateTypeFromBase cppBody cppArgs
  where
    (body, args) = gatherParams (TypeApplication at) []

    gatherParams t ps = case deannotateType t of
      TypeApplication (ApplicationType lhs rhs) -> gatherParams lhs (rhs:ps)
      _ -> (t, ps)

    createTemplateTypeFromBase base args = case base of
      Cpp.TypeExpressionBasic (Cpp.BasicTypeNamed name) -> createTemplateType name args
      _ -> error "Non-named type in template application"

encodeEnumType :: CppEnvironment -> Name -> [FieldType] -> Maybe String -> Flow Graph [Cpp.Declaration]
encodeEnumType env name tfields _comment = return [
    cppEnumDeclaration (encodeName False CaseConventionPascal env name)
      $ Just $ Cpp.ClassBody enumFields]
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
    isBasicType = typeVariant (deannotateType ftype) == TypeVariantLiteral
    isContainerType = isStdContainerType ftype

    fieldType typ =
      if isBasicType || isContainerType then typ
      else Cpp.TypeExpressionQualified $ Cpp.QualifiedType typ Cpp.TypeQualifierConst

    parameterType typ
      | isBasicType = typ
      | isContainerType = typ
      | otherwise = Cpp.TypeExpressionQualified $
                      Cpp.QualifiedType
                        (Cpp.TypeExpressionQualified $
                          Cpp.QualifiedType typ Cpp.TypeQualifierConst)
                        Cpp.TypeQualifierLvalueRef

encodeForallType :: CppEnvironment -> ForallType -> Flow Graph Cpp.TypeExpression
encodeForallType env lt = do
  cppBody <- encodeType env body
  return cppBody
  where
    (body, _) = gatherParams (TypeForall lt) []

    gatherParams t ps = case deannotateType t of
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

    gatherParams rdoms (FunctionType dom cod) = case deannotateType cod of
      TypeFunction ft2 -> gatherParams (dom:rdoms) ft2
      _ -> (L.reverse (dom:rdoms), cod)

    paramFromType t = Cpp.Parameter t "" False Nothing

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

encodeRecordType :: CppEnvironment -> Name -> RowType -> Maybe String -> Flow Graph [Cpp.Declaration]
encodeRecordType env name (RowType _ tfields) _comment = do
    cppFields <- CM.mapM (encodeFieldType env False) tfields
    constructorParams <- createParameters tfields
    return [cppClassDeclaration (className name) [] $
      Just $ Cpp.ClassBody ([memberSpecificationPublic] ++ fieldDecls cppFields ++ constructor cppFields constructorParams),
      createLessThanOperator env name tfields]
  where
    fieldDecls fields = [Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationVariable field | field <- fields]

    constructor fields params = [
      Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationConstructor $
        Cpp.ConstructorDeclaration
          (encodeName False CaseConventionPascal env name)
          params
          [Cpp.MemInitializer
             (Cpp.variableDeclarationName field)
             [createIdentifierExpr $ Cpp.variableDeclarationName field]
           | field <- fields]
          (createConstructorBody params)]

    createParameters fields = do
      let fieldNames = [fname | FieldType fname _ <- fields]
      CM.zipWithM createParam fieldNames fields

    createParam fieldName (FieldType _ ftype) = do
      paramDecl <- encodeFieldType env True (FieldType fieldName ftype)
      return $ Cpp.Parameter
        (Y.fromJust $ Cpp.variableDeclarationType paramDecl)
        (Cpp.variableDeclarationName paramDecl)
        False
        Nothing

encodeType :: CppEnvironment -> Type -> Flow Graph Cpp.TypeExpression
encodeType env typ = case deannotateType typ of
    TypeApplication at -> encodeApplicationType env at
    TypeFunction ft -> encodeFunctionType env ft
    TypeForall lt -> encodeForallType env lt
    TypeList et -> toConstType <$> (createTemplateType "std::vector" <$> ((:[]) <$> encode et))
    TypeMap (MapType kt vt) -> toConstType <$> (createTemplateType "std::map" <$> sequence [encode kt, encode vt])
    TypeLiteral lt -> encodeLiteralType lt
    TypeOptional et -> toConstType <$> (createTemplateType "std::optional" <$> ((:[]) <$> encode et))
    TypeRecord rt -> typeref typ (rowTypeTypeName rt)
    TypeSet et -> toConstType <$> (createTemplateType "std::set" <$> ((:[]) <$> encode et))
    TypeUnion rt -> typeref typ (rowTypeTypeName rt)
    TypeVariable name -> (bindingTerm <$> requireElement name) >>= DecodeCore.type_ >>= \t -> typeref t name
    TypeWrap (WrappedType name _) -> typeref typ name
    _ -> fail $ "Unsupported type: " ++ show (deannotateType typ)
  where
    encode = encodeType env
    typeref t name = pure $ if EncodeCore.isUnitType t
      then createTemplateType "std::tuple" []
      else createTypeReference (isStructType t) env name

encodeTypeAlias :: CppEnvironment -> Name -> Type -> Maybe String -> Flow Graph Cpp.Declaration
encodeTypeAlias env name typ comment = do
  cppType <- encodeType env typ
  return $ Cpp.DeclarationTypedef $
    Cpp.TypedefDeclaration
      (encodeName False CaseConventionPascal env name)
      cppType
      True

encodeTypeDefinition :: CppEnvironment -> Name -> Type -> Flow Graph [Cpp.Declaration]
encodeTypeDefinition env name typ = do
    comment <- fmap normalizeComment <$> getTypeDescription typ
    encode env typ comment
  where
    -- TODO: use the comment
    encode env typ comment = case deannotateType typ of
      TypeForall (ForallType var body) ->
        encode env2 body comment
        where
          (tparamList, tparamMap) = cppEnvironmentBoundTypeVariables env
          env2 = env { cppEnvironmentBoundTypeVariables = (tparamList ++ [var], M.insert var (unName var) tparamMap)}
      TypeRecord rt -> encodeRecordType env name rt comment
      TypeUnion rt -> encodeUnionType env name rt comment
      TypeWrap (WrappedType _ t) -> encodeWrappedType env name t comment
      _ -> fail $ "unexpected type in definition: " ++ ShowCore.type_ typ

encodeUnionType :: CppEnvironment -> Name -> RowType -> Maybe String -> Flow Graph [Cpp.Declaration]
encodeUnionType env name rt comment = if isEnumRowType rt
  then encodeEnumType env name (rowTypeFields rt) comment
  else encodeVariantType env name (rowTypeFields rt) comment

encodeVariantType env name variants comment = do
    variantClasses <- CM.mapM (createVariantClass env name name) variants
    return $ forwardDecls ++ [visitorInterface, baseClass] ++ variantClasses ++ [partialVisitorInterface, acceptImpl]
  where
    forwardDecls = generateForwardDeclarations env name variants
    baseClass = createUnionBaseClass env name variants
    visitorInterface = createVisitorInterface env name variants
    partialVisitorInterface = createPartialVisitorInterface env name variants
    acceptImpl = createAcceptImplementation env name variants

encodeWrappedType :: CppEnvironment -> Name -> Type -> Maybe String -> Flow Graph [Cpp.Declaration]
encodeWrappedType env name typ comment = encodeRecordType env name rt comment
  where
    rt = RowType name [FieldType (Name "value") typ]

--------------------------------------------------------------------------------
-- Helper functions

createAcceptImplementation :: CppEnvironment -> Name -> [FieldType] -> Cpp.Declaration
createAcceptImplementation env tname variants = Cpp.DeclarationTemplate $
    Cpp.TemplateDeclaration False ["typename R"] $
      Cpp.DeclarationFunction $
        Cpp.FunctionDeclaration
          []
          (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed "R")
          (encodeName False CaseConventionPascal env tname ++ "::accept")
          [Cpp.Parameter
            (Cpp.TypeExpressionQualified $
              Cpp.QualifiedType
                (Cpp.TypeExpressionBasic $
                  Cpp.BasicTypeNamed $ visitorName tname ++ "<R>")
                Cpp.TypeQualifierLvalueRef)
            "visitor"
            False
            Nothing]
          [Cpp.FunctionSpecifierSuffixConst]
          (Cpp.FunctionBodyCompound $ Cpp.CompoundStatement $ generateDynamicCasts variants)
  where
    generateDynamicCasts [] = []
    generateDynamicCasts (FieldType fname _:rest) =
      let -- Create a workaround for template function call using existing AST
          -- We'll represent dynamic_cast<T>(expr) as a regular function call with the template part in the function name
          templateFunction = "dynamic_cast<const " ++ variantName tname fname ++ "*>"

          dynamicCastExpr = Cpp.ExpressionAssignment $ Cpp.AssignmentExpressionAssignment $
            Cpp.ExplicitAssignment
              (cppUnaryExpressionToCppLogicalOrExpression $ Cpp.UnaryExpressionPostfix $
                Cpp.PostfixExpressionPrimary $ Cpp.PrimaryExpressionIdentifier "auto ptr")
              Cpp.AssignmentOperatorAssign
              (Cpp.AssignmentExpressionConditional $ Cpp.ConditionalExpressionLogicalOr $
                cppUnaryExpressionToCppLogicalOrExpression $ Cpp.UnaryExpressionPostfix $
                  Cpp.PostfixExpressionFunctionCall $ Cpp.FunctionCallOperation
                    (Cpp.PostfixExpressionPrimary $ Cpp.PrimaryExpressionIdentifier templateFunction)
                    [cppPrimaryExpressionToCppExpression $ Cpp.PrimaryExpressionIdentifier "this"])

          returnVisitorCall = Cpp.StatementJump $ Cpp.JumpStatementReturnValue $
            cppPostfixExpressionToCppExpression $ Cpp.PostfixExpressionFunctionCall $ Cpp.FunctionCallOperation
              (Cpp.PostfixExpressionMemberAccess $ Cpp.MemberAccessOperation
                (Cpp.PostfixExpressionPrimary $ Cpp.PrimaryExpressionIdentifier "visitor")
                "visit")
              [cppUnaryExpressionToCppExpression $ Cpp.UnaryExpressionUnaryOp $ Cpp.UnaryOperation
                Cpp.UnaryOperatorDereference
                (Cpp.UnaryExpressionPostfix $ Cpp.PostfixExpressionPrimary $
                  Cpp.PrimaryExpressionIdentifier "ptr")]

          elseClause = if null rest
                       then Just throwStatement
                       else Just $ head $ generateDynamicCasts rest
      in
      [Cpp.StatementSelection $ Cpp.SelectionStatement
         dynamicCastExpr
         (Cpp.StatementCompound $ Cpp.CompoundStatement [returnVisitorCall])
         elseClause]

    throwStatement = createThrowStmt "std::runtime_error" createTypeIdNameCall

createLessThanOperator :: CppEnvironment -> Name -> [FieldType] -> Cpp.Declaration
createLessThanOperator env typeName fields = Cpp.DeclarationFunction $
  Cpp.FunctionDeclaration
    [Cpp.FunctionSpecifierPrefixInline]
    (Cpp.TypeExpressionBasic Cpp.BasicTypeBool)
    ("operator<")
    [unnamedParameter "lhs" $ Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed $ encodeName False CaseConventionPascal env typeName,
     unnamedParameter "rhs" $ Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed $ encodeName False CaseConventionPascal env typeName]
    []
    (Cpp.FunctionBodyCompound $ Cpp.CompoundStatement $ generateComparisonLogic fields)
  where
    -- TODO: trivial (and incorrect) implementation
    generateComparisonLogic fields =
      [Cpp.StatementJump $ Cpp.JumpStatementReturnValue $ createLiteralBoolExpr False]

createPartialVisitorInterface :: CppEnvironment -> Name -> [FieldType] -> Cpp.Declaration
createPartialVisitorInterface env tname variants = Cpp.DeclarationTemplate $
    Cpp.TemplateDeclaration False ["typename R"] $
      cppClassDeclaration (partialVisitorName tname)
        [Cpp.BaseSpecifier Cpp.AccessSpecifierPublic (visitorName tname ++ "<R>")] $
        Just $ Cpp.ClassBody ([memberSpecificationPublic, otherwiseMethod] ++ defaultVisitMethods)
  where
    otherwiseMethod = Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationFunction $
      Cpp.FunctionDeclaration
        [Cpp.FunctionSpecifierPrefixVirtual]
        (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed "R")
        "otherwise"
        [constParameter "value" $ Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed $ className tname]
        [Cpp.FunctionSpecifierSuffixConst]
        Cpp.FunctionBodyPure

    defaultVisitMethods = [
      Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationFunction $
        Cpp.FunctionDeclaration
          []
          (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed "R")
          "visit"
          [constParameter "value" $ Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed $ variantName tname fname]
          [Cpp.FunctionSpecifierSuffixOverride]
          (Cpp.FunctionBodyCompound $ Cpp.CompoundStatement [
            Cpp.StatementJump $
              Cpp.JumpStatementReturnValue $
                createFunctionCallExpr
                  "otherwise"
                  [createIdentifierExpr "value"]])
      | FieldType fname _ <- variants]

createUnionBaseClass :: CppEnvironment -> Name -> [FieldType] -> Cpp.Declaration
createUnionBaseClass env name variants = cppClassDeclaration className [] $ Just $ Cpp.ClassBody [
    memberSpecificationProtected,
    constructor,
    memberSpecificationPublic,
    virtualDestructor,
    acceptMethod]
  where
    className = encodeName False CaseConventionPascal env name
    constructor = Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationConstructor $
      Cpp.ConstructorDeclaration
        className
        []
        []
        Cpp.FunctionBodyDefault
    virtualDestructor = Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationDestructor $
      Cpp.DestructorDeclaration
        [Cpp.FunctionSpecifierPrefixVirtual]
        className
        []
        Cpp.FunctionBodyDefault
    acceptMethod = Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationTemplate $
      Cpp.TemplateDeclaration False ["typename R"] $ Cpp.DeclarationFunction $
        Cpp.FunctionDeclaration
          []
          (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed "R")
          "accept"
          [Cpp.Parameter
            (Cpp.TypeExpressionQualified $
              Cpp.QualifiedType
                (Cpp.TypeExpressionBasic $
                  Cpp.BasicTypeNamed $ visitorName name ++ "<R>")
                Cpp.TypeQualifierLvalueRef)
            "visitor"
            False
            Nothing]
          [Cpp.FunctionSpecifierSuffixConst]
          Cpp.FunctionBodyDeclaration

createVariantClass :: CppEnvironment -> Name -> Name -> FieldType -> Flow Graph Cpp.Declaration
createVariantClass env tname parentClass (FieldType fname variantType) = do
    valueField <- if hasValue
      then do
        cppType <- encodeType env (deannotateType variantType)
        return [Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationVariable $
                  Cpp.VariableDeclaration (Just cppType) "value" Nothing False]
      else return []

    constructorParams <- if hasValue
      then do
        paramType <- encodeType env (deannotateType variantType)
        return [Cpp.Parameter paramType "value" False Nothing]
      else return []

    let constructorInitList = if hasValue && isTemplateType variantType
                              then [Cpp.MemInitializer "value" [createFunctionCallExpr "std::move" [createIdentifierExpr "value"]]]
                              else if hasValue
                                   then [Cpp.MemInitializer "value" [createIdentifierExpr "value"]]
                                   else []
    let constructor = Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationConstructor $
          Cpp.ConstructorDeclaration
            (variantName tname fname)
            constructorParams
            constructorInitList
            (createConstructorBody constructorParams)
    let baseClass = Cpp.BaseSpecifier Cpp.AccessSpecifierPublic $ encodeName False CaseConventionPascal env parentClass

    return $ cppClassDeclaration
      (variantName tname fname)
      [baseClass]
      (Just $ Cpp.ClassBody ([memberSpecificationPublic] ++ valueField ++ [constructor]))
  where
    hasValue = not (EncodeCore.isUnitType variantType)

createVisitorInterface :: CppEnvironment -> Name -> [FieldType] -> Cpp.Declaration
createVisitorInterface env tname variants = Cpp.DeclarationTemplate $
    Cpp.TemplateDeclaration False ["typename R"] $
      cppClassDeclaration
        (visitorName tname)
        []
        (Just $ Cpp.ClassBody (visitMethods ++ [virtualDestructor]))
  where
    visitMethods = [memberSpecificationPublic] ++ [
      Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationFunction $
        Cpp.FunctionDeclaration
          [Cpp.FunctionSpecifierPrefixVirtual]
          (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed "R")
          "visit"
          [Cpp.Parameter
            (Cpp.TypeExpressionQualified $
              Cpp.QualifiedType
                (Cpp.TypeExpressionQualified $
                  Cpp.QualifiedType
                    (Cpp.TypeExpressionBasic $ Cpp.BasicTypeNamed $ variantName tname fname)
                    Cpp.TypeQualifierConst)
                Cpp.TypeQualifierLvalueRef)
            "value"
            False
            Nothing]
          []
          Cpp.FunctionBodyPure
      | FieldType fname _ <- variants]

    virtualDestructor = Cpp.MemberSpecificationMember $ Cpp.MemberDeclarationDestructor $
      Cpp.DestructorDeclaration
        [Cpp.FunctionSpecifierPrefixVirtual]
        (visitorName tname)
        []
        Cpp.FunctionBodyDefault

bindingNameToFilePath :: Name -> FilePath
bindingNameToFilePath = nameToFilePath CaseConventionLowerSnake CaseConventionLowerSnake (FileExtension "h")

findIncludes :: Bool -> Namespace -> [TypeDefinition] -> [Cpp.IncludeDirective]
findIncludes withFwd ns defs = systemIncludes ++ domainIncludes
  where
    meta = gatherMetadata (DefinitionType <$> defs)
    systemIncludes = Y.catMaybes [
      if cppModuleMetadataUsesMap meta then Just (Cpp.IncludeDirective "map" True) else Nothing,
      if cppModuleMetadataUsesOptional meta then Just (Cpp.IncludeDirective "optional" True) else Nothing,
      if cppModuleMetadataUsesSet meta then Just (Cpp.IncludeDirective "set" True) else Nothing,
      if cppModuleMetadataUsesString meta then Just (Cpp.IncludeDirective "string" True) else Nothing,
      if cppModuleMetadataUsesTuple meta then Just (Cpp.IncludeDirective "tuple" True) else Nothing,
      if cppModuleMetadataUsesTypeinfo meta then Just (Cpp.IncludeDirective "typeinfo" True) else Nothing,
      if cppModuleMetadataUsesVector meta then Just (Cpp.IncludeDirective "vector" True) else Nothing,
      -- TODO: consider making these conditional as well
      Just (Cpp.IncludeDirective "memory" True),
      Just (Cpp.IncludeDirective "stdexcept" True)]
    domainIncludes = typeIncludes ++ dslIncludes ++ fwdIncludes
      where
        typeIncludes = toInclude <$> importDeps
        toInclude name = Cpp.IncludeDirective (bindingNameToFilePath name) False
        dslIncludes = [] -- These will be needed if/when DSL functions are used.for type construction.
        fwdIncludes = if withFwd then [toInclude $ fwdHeaderName ns] else []
    importDeps = findTypeDependencies ns defs

findTypeDependencies :: Namespace -> [TypeDefinition] -> [Name]
findTypeDependencies ns defs = L.filter (\n -> namespaceOf n /= Just ns) $ S.toList $ L.foldl S.union S.empty $
  fmap (typeDependencyNames True . typeDefinitionType) defs

gatherMetadata :: [Definition] -> CppModuleMetadata
gatherMetadata defs = L.foldl addDef start defs
  where
    start = CppModuleMetadata {
      cppModuleMetadataTypeVariables = S.empty,
      cppModuleMetadataUsesMap = False,
      cppModuleMetadataUsesOptional = False,
      cppModuleMetadataUsesSet = False,
      cppModuleMetadataUsesString = False,
      cppModuleMetadataUsesTuple = False,
      cppModuleMetadataUsesTypeinfo = False,
      cppModuleMetadataUsesVector = False}

    addDef meta def = case def of
      DefinitionTerm (TermDefinition _ term typ) ->
        foldOverTerm TraversalOrderPre extendMetaForTerm (extendMetaForType meta typ) term
      DefinitionType (TypeDefinition _ typ) ->
        foldOverType TraversalOrderPre extendMetaForType meta typ

    extendMetaForTerm meta t = case t of
      TermLet (Let bindings _) -> L.foldl forBinding meta bindings
        where
          forBinding m (Binding _ _ mts) = case mts of
            Nothing -> m
            Just ts -> extendMetaForType m $ typeSchemeType ts
      TermMap _ -> meta {cppModuleMetadataUsesMap = True}
      TermList _ -> meta {cppModuleMetadataUsesVector = True}
      TermSet _ -> meta {cppModuleMetadataUsesSet = True}
      TermLiteral (LiteralString _) -> meta {cppModuleMetadataUsesString = True}
      TermOptional _ -> meta {cppModuleMetadataUsesOptional = True}
      _ -> meta

    extendMetaForType meta typ = case deannotateType typ of
      TypeForall (ForallType _ body) ->
        meta {cppModuleMetadataTypeVariables = S.union (cppModuleMetadataTypeVariables meta) (freeVariablesInType body)}
      TypeList _ -> meta {cppModuleMetadataUsesVector = True}
      TypeMap _ -> meta {cppModuleMetadataUsesMap = True}
      TypeLiteral lt -> case lt of
        LiteralTypeString -> meta {cppModuleMetadataUsesString = True}
        _ -> meta
      TypeOptional _ -> meta {cppModuleMetadataUsesOptional = True}
      TypeRecord rt -> meta
      TypeSet _ -> meta {cppModuleMetadataUsesSet = True}
      TypeUnion _ -> meta {cppModuleMetadataUsesTypeinfo = True}
      TypeUnit -> meta {cppModuleMetadataUsesTuple = True}
      TypeVariable name -> meta {cppModuleMetadataTypeVariables = S.insert name (cppModuleMetadataTypeVariables meta)}
      TypeApplication _ -> meta
      _ -> meta

generateForwardDeclarations :: CppEnvironment -> Name -> [FieldType] -> [Cpp.Declaration]
generateForwardDeclarations env tname fields = fmap declare variantNames
  where
    variantNames = fmap (variantName tname . fieldTypeName) fields
    declare name = cppClassDeclaration name [] Nothing

isStdContainerType :: Type -> Bool
isStdContainerType typ = case deannotateType typ of
  TypeApplication (ApplicationType lhs _) -> isStdContainerType lhs
  TypeList _ -> True
  TypeMap _ -> True
  TypeOptional _ -> True
  TypeSet _ -> True
  _ -> False

isStructType :: Type -> Bool
isStructType rawType = var /= TypeVariantLiteral && not (isEnumType rawType)
  where
    var = typeVariant $ fullyStripType rawType

isTemplateType :: Type -> Bool
isTemplateType typ = case deannotateType typ of
  TypeLiteral LiteralTypeString -> True
  _ -> isStdContainerType typ

parameterType :: CppEnvironment -> Type -> Flow Graph Cpp.TypeExpression
parameterType env typ = do
  encoded <- encodeType env typ
  return $
    Cpp.TypeExpressionQualified $
      Cpp.QualifiedType
        (Cpp.TypeExpressionQualified $
          Cpp.QualifiedType encoded Cpp.TypeQualifierConst)
        Cpp.TypeQualifierLvalueRef

serializeHeaderFile :: Name -> [Cpp.IncludeDirective] -> [Cpp.Declaration] -> (FilePath, String)
serializeHeaderFile name includes decls = (
  bindingNameToFilePath name,
  printExpr $ parenthesize $ CppSer.encodeProgram $ createHeaderFile includes decls)
