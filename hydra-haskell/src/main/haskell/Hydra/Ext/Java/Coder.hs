module Hydra.Ext.Java.Coder (
  moduleToJavaCompilationUnit,
  javaLanguage,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Extras
import Hydra.CoreDecoding
import Hydra.Ext.Java.Utils
import Hydra.Ext.Java.Language
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import qualified Hydra.Ext.Java.Syntax as Java
import Hydra.Util.Coders
import Hydra.Util.Formatting

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M


listsAsArrays :: Bool
listsAsArrays = False

moduleToJavaCompilationUnit :: (Default m, Ord m, Read m, Show m) => Context m -> Graph m -> Qualified Java.CompilationUnit
moduleToJavaCompilationUnit cx g = dataGraphToExternalModule javaLanguage (encodeData aliases) constructModule cx g
  where
    aliases = importAliasesForGraph g

constructModule :: (Default m, Ord m, Show m)
  => Context m -> Graph m -> M.Map (Type m) (Step (Data m) Java.Block) -> [(Element m, TypedData m)]
  -> Result Java.CompilationUnit
constructModule cx g coders pairs = do
    let pkg = javaPackageDeclaration $ graphName g
    let imports = []
    typeDecls <- CM.mapM (toTypeDeclaration aliases cx) typePairs
    dataDecls <- CM.mapM (toDataDeclaration aliases cx) dataPairs
    let types = typeDecls -- TODO
    return $ Java.CompilationUnitOrdinary $ Java.OrdinaryCompilationUnit (Just pkg) imports types
  where
    isTypePair = isType . typedDataType . snd
    typePairs = L.filter isTypePair pairs
    dataPairs = [] -- TODO   L.filter (not . isTypePair) pairs
    aliases = importAliasesForGraph g

toTypeDeclaration :: (Default m, Ord m, Show m)
  => M.Map GraphName Java.PackageName -> Context m -> (Element m, TypedData m) -> Result Java.TypeDeclaration
toTypeDeclaration aliases cx (el, TypedData _ term) = do
    t <- decodeType cx term
    cd <- toClassDecl (elementName el) t
    return $ Java.TypeDeclarationClass cd
  where
    toClassDecl elName t = case typeTerm t of
      TypeTermNominal name -> return $ javaClassDeclaration aliases elName topMods (Just name) []
      TypeTermRecord fields -> do
          memberVars <- CM.mapM toMemberVar fields
          let hashCode = []  :: [Java.ClassBodyDeclaration]-- TODO
          withMethods <- if L.length fields > 1
            then CM.mapM toWithMethod fields
            else pure []
          cons <- constructor
          let bodyDecls = memberVars ++ [cons, equalsMethod] ++ hashCode ++ withMethods :: [Java.ClassBodyDeclaration]
          return $ javaClassDeclaration aliases elName topMods Nothing bodyDecls
        where
          constructor = do
            params <- CM.mapM fieldToFormalParam fields
            let stmts = Java.BlockStatementStatement . toAssignStmt . fieldTypeName <$> fields
            return $ makeConstructor aliases elName False params stmts

          fieldArgs = fieldNameToJavaExpression . fieldTypeName <$> fields

          toMemberVar (FieldType fname ft) = do
            let mods = [Java.FieldModifierPublic, Java.FieldModifierFinal]
            jt <- encodeType aliases ft
            let var = fieldNameToJavaVariableDeclarator fname
            return $ javaMemberField mods jt var

          toWithMethod field = do
            let mods = [Java.MethodModifierPublic]
            let methodName = "with" ++ capitalize (unFieldName $ fieldTypeName field)
            param <- fieldToFormalParam field
            let anns = [] -- TODO
            let result = referenceTypeToResult $ nameToJavaReferenceType aliases False elName
            let returnStmt = Java.BlockStatementStatement $ javaReturnStatement $ Just $ javaConstructorCall elName fieldArgs
            return $ methodDeclaration mods [] anns methodName [param] result (Just [returnStmt])

          fieldToFormalParam (FieldType fname ft) = do
            jt <- encodeType aliases ft
            return $ javaTypeToJavaFormalParameter jt fname

          equalsMethod = methodDeclaration mods [] anns "equals" [param] result $
              Just [instanceOfStmt,
                castStmt,
                returnStmt]
            where
              anns = [overrideAnnotation]
              mods = [Java.MethodModifierPublic]
              param = javaTypeToJavaFormalParameter (javaRefType [] Nothing "Object") (FieldName otherName)
              result = javaTypeToResult javaBooleanType
              otherName = "other"
              tmpName = "o"
              instanceOfStmt = Java.BlockStatementStatement $ Java.StatementIfThen $ Java.IfThenStatement cond ret
                where
                  cond = javaUnaryExpressionToJavaExpression $
                      Java.UnaryExpressionOther $
                      Java.UnaryExpressionNotPlusMinusNot $
                      javaRelationalExpressionToJavaUnaryExpression $
                      javaInstanceOf other parent
                    where
                      other = javaIdentifierToJavaRelationalExpression $ Java.Identifier otherName
                      parent = nameToJavaReferenceType aliases False elName

                  ret = javaReturnStatement $ Just $ javaBoolean False
              castStmt = variableDeclarationStatement aliases elName id rhs
                where
                  id = Java.Identifier tmpName
                  rhs = javaUnaryExpressionToJavaExpression $ Java.UnaryExpressionOther $
                    Java.UnaryExpressionNotPlusMinusCast $ javaCastExpression aliases elName $ Java.Identifier otherName
                                  
              returnStmt = Java.BlockStatementStatement $ javaReturnStatement $ Just $ javaBoolean False -- TODO; placeholder

{-
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NumberEsc)) {
        return false;
    }
    NumberEsc o = (NumberEsc) other;
    return integer.equals(o.integer)
        && fraction.equals(o.fraction)
        && exponent.equals(o.exponent);
  }

  @Override
  public int hashCode() {
    return 2 * integer.hashCode()
        + 3 * fraction.hashCode()
        + 5 * exponent.hashCode();
  }
-}

      TypeTermUnion fields -> do
          variantClasses <- CM.mapM (fmap augmentVariantClass . unionFieldClass) fields
          let variantDecls = Java.ClassBodyDeclarationClassMember . Java.ClassMemberDeclarationClass <$> variantClasses
          let bodyDecls = [privateConstructor, toAcceptMethod True] ++ variantDecls
          let mods = topMods ++ [Java.ClassModifierAbstract]
          return $ javaClassDeclaration aliases elName mods Nothing bodyDecls
        where
          privateConstructor = makeConstructor aliases elName True [] []
          visitor = () -- TODO
          partialVisitor = () -- TODO
          unionFieldClass (FieldType (FieldName fname) ftype) = do
            let rtype = Types.record $ if isUnit ftype then [] else [FieldType (FieldName "value") ftype]
            toClassDecl (fromQname (graphNameOf elName) $ capitalize fname) rtype
          augmentVariantClass (Java.ClassDeclarationNormal cd) = Java.ClassDeclarationNormal $ cd {
              Java.normalClassDeclarationModifiers = [Java.ClassModifierPublic, Java.ClassModifierStatic, Java.ClassModifierFinal],
              Java.normalClassDeclarationExtends = Just $ nameToJavaClassType aliases True elName,
              Java.normalClassDeclarationBody = newBody (Java.normalClassDeclarationBody cd)}
            where
              newBody (Java.ClassBody decls) = Java.ClassBody $ decls ++ [toAcceptMethod False]
          isUnit t = typeTerm t  == TypeTermRecord []

      TypeTermUniversal (UniversalType (TypeVariable v) body) -> do
        (Java.ClassDeclarationNormal cd) <- toClassDecl elName body
        return $ Java.ClassDeclarationNormal $ cd {
          Java.normalClassDeclarationParameters = addParameter v (Java.normalClassDeclarationParameters cd)}
      _ -> fail $ "unexpected type: " ++ show t
    addParameter v params = params ++ [javaTypeParameter v]

    topMods = [Java.ClassModifierPublic]

{-
  public static final class StringEsc extends Value {
    public final String string;

    /**
     * Constructs an immutable StringEsc object
     */
    public StringEsc(String string) {
      this.string = string;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StringEsc)) {
          return false;
      }
      StringEsc o = (StringEsc) other;
      return string.equals(o.string);
    }

    @Override
    public int hashCode() {
      return 2 * string.hashCode();
    }
  }
-}

-- | Transform a given type into a type which can be used as the basis for a Java class
toDeclarationType :: Default m => Type m -> Result (Type m)
toDeclarationType t = case typeTerm t of
  TypeTermNominal _ -> pure t
  TypeTermRecord _ -> pure t
  TypeTermUnion _ -> pure t
  TypeTermUniversal (UniversalType v body) -> do
    b <- toDeclarationType body
    return t {typeTerm = TypeTermUniversal $ UniversalType v b}
  TypeTermVariable _ -> fail "unexpected type variable"
  _ -> do
    let rt = Types.record [Types.field "value" t]
    return t {typeTerm = typeTerm rt}

toDataDeclaration :: M.Map GraphName Java.PackageName -> Context m -> (a, TypedData m) -> Result a
toDataDeclaration aliases cx (el, TypedData typ term) = do
  fail "not implemented" -- TODO

encodeData :: (Default m, Eq m, Ord m, Read m, Show m)
  => M.Map GraphName Java.PackageName -> Context m -> Data m -> Result Java.Block
encodeData aliases cx term@(Data expr meta) = do
  return $ javaStatementsToBlock [javaEmptyStatement] -- TODO

-- Note: we use Java object types everywhere, rather than primitive types, as the latter cannot be used
--       to build function types, parameterized types, etc.
encodeLiteralType :: LiteralType -> Result Java.Type
encodeLiteralType lt = case lt of
    LiteralTypeBoolean -> simple "Boolean"
    LiteralTypeFloat ft -> case ft of
      FloatTypeFloat32 -> simple "Float"
      FloatTypeFloat64 -> simple "Double"
      _ -> fail $ "unexpected float type: " ++ show ft
    LiteralTypeInteger it -> case it of
      IntegerTypeBigint -> pure $ javaRefType [] (Just $ javaPackageName ["java", "math"]) "BigInteger"
      IntegerTypeInt16 -> simple "Short"
      IntegerTypeInt32 -> simple "Integer"
      IntegerTypeInt64 -> simple "Long"
      IntegerTypeUint8 -> simple "Byte"
      IntegerTypeUint16 -> simple "Char"
      _ -> fail $ "unexpected integer type: " ++ show it
    LiteralTypeString -> simple "String"
    _ -> fail $ "unexpected literal type: " ++ show lt
  where
    simple n = pure $ javaRefType [] Nothing n

encodeType :: Show m => M.Map GraphName Java.PackageName -> Type m -> Result Java.Type
encodeType aliases t = case typeTerm t of
  TypeTermElement et -> encode et -- Elements are simply unboxed
  TypeTermFunction (FunctionType dom cod) -> do
    jdom <- encode dom >>= asJavaReferenceType
    jcod <- encode cod >>= asJavaReferenceType
    return $ javaRefType [jdom, jcod] javaUtilPackageName "Function"
  TypeTermList et -> do
    jet <- encode et
    if listsAsArrays
      then toJavaArrayType jet
      else do
        rt <- asJavaReferenceType jet
        return $ javaRefType [rt] javaUtilPackageName "List"
  TypeTermLiteral lt -> encodeLiteralType lt
  TypeTermMap (MapType kt vt) -> do
    jkt <- encode kt >>= asJavaReferenceType
    jvt <- encode vt >>= asJavaReferenceType
    return $ javaRefType [jkt, jvt] javaUtilPackageName "Map"
  TypeTermNominal name -> pure $ Java.TypeReference $ nameToJavaReferenceType aliases True name
  TypeTermOptional ot -> do
    jot <- encode ot >>= asJavaReferenceType
    return $ javaRefType [jot] javaUtilPackageName "Optional"
  TypeTermSet st -> do
    jst <- encode st >>= asJavaReferenceType
    return $ javaRefType [jst] javaUtilPackageName "Set"
  TypeTermUniversal (UniversalType (TypeVariable v) body) -> do
    jbody <- encode body
    addJavaTypeParameter (javaTypeVariable v) jbody
  TypeTermVariable (TypeVariable v) -> pure $ Java.TypeReference $ javaTypeVariable v
  TypeTermRecord [] -> return $ javaRefType [] javaLangPackageName "Void"
  -- Note: record (other than unit) and union types should not appear at this level
  _ -> fail $ "can't encode unsupported type in Java: " ++ show t
  where
    encode = encodeType aliases
