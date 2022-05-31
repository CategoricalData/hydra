module Hydra.Ext.Java.Coder (
  moduleToJavaCompilationUnit,
  javaLanguage,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Extras
import Hydra.CoreDecoding
import Hydra.Ext.Java.CoderUtils
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
    cd <- toClassDecl t
    return $ Java.TypeDeclarationClass $ Java.ClassDeclarationNormal cd
  where
    toClassDecl t = case typeTerm t of
      TypeTermNominal name -> return $ classDecl (Just name) []
      TypeTermRecord fields -> do
          memberVars <- CM.mapM toMemberVar fields
          let cons = []  :: [Java.ClassBodyDeclaration]-- TODO
          let eq = []  :: [Java.ClassBodyDeclaration]-- TODO
          let hashCode = []  :: [Java.ClassBodyDeclaration]-- TODO
          withMethods <- CM.mapM toWithMethod fields
          let bodyDecls = memberVars ++ cons ++ eq ++ hashCode ++ withMethods :: [Java.ClassBodyDeclaration]
          return $ classDecl Nothing bodyDecls
        where
          toMemberVar (FieldType fname ft) = do
            let mods = [Java.FieldModifierPublic, Java.FieldModifierFinal]
            jt <- encodeType aliases ft
            let var = Java.VariableDeclarator (fieldNameToJavaVariableDeclaratorId fname) Nothing
            return $ Java.ClassBodyDeclarationClassMember $ Java.ClassMemberDeclarationField $
              Java.FieldDeclaration mods (Java.UnannType jt) [var]
          toWithMethod (FieldType fname@(FieldName fn) ft) = do
            jt <- encodeType aliases ft
            let mods = [Java.MethodModifierPublic]
            let methodName = Java.Identifier $ "with" ++ capitalize fn
            let recParam = Nothing
            let argType = Java.UnannType jt
            let argId = fieldNameToJavaVariableDeclaratorId fname
            let formalParams = [Java.FormalParameterSimple $ Java.FormalParameter_Simple [] argType argId]
            let params = []
            let anns = [] -- TODO
            let result = Java.ResultType $ Java.UnannType $ Java.TypeReference $
                  nameToJavaReferenceType aliases False elName
            let decl = Java.MethodDeclarator methodName recParam formalParams
            let mthrows = Nothing
            let header = Java.MethodHeader params anns result decl mthrows
            let cit = javaConstructorName elName
            let consCall = Java.UnqualifiedClassInstanceCreationExpression [] cit [] Nothing

            let relEx = Java.RelationalExpressionSimple $
                  Java.ShiftExpressionUnary $
                  Java.AdditiveExpressionUnary $
                  Java.MultiplicativeExpressionUnary $
                  Java.UnaryExpressionOther $
                  Java.UnaryExpressionNotPlusMinusPostfix $
                  Java.PostfixExpressionPrimary $
                  Java.PrimaryNoNewArray $ Java.PrimaryNoNewArrayClassInstance $
                  Java.ClassInstanceCreationExpression Nothing consCall

            let andEx = Java.AndExpression [Java.EqualityExpressionUnary relEx]
            let exOrEx = Java.ExclusiveOrExpression [andEx]
            let incOrEx = Java.InclusiveOrExpression [exOrEx]
            let condAndEx = Java.ConditionalAndExpression [incOrEx]
            let ex = Java.ExpressionAssignment $ Java.AssignmentExpressionConditional $
                     Java.ConditionalExpressionSimple $ Java.ConditionalOrExpression [condAndEx]
            let returnStmt = javaReturnStatement $ Just ex
            let body = Java.MethodBody $ javaStatementsToBlock [returnStmt]
            return $ Java.ClassBodyDeclarationClassMember $ Java.ClassMemberDeclarationMethod $
              Java.MethodDeclaration mods header body
      {-
        public NumberEsc withInteger(Long integer) {
          return new NumberEsc(integer, fraction, exponent);
        }

      -}
      TypeTermUnion fields -> do
        let bodyDecls = [] -- TODO
        return $ classDecl Nothing bodyDecls
      TypeTermUniversal (UniversalType (TypeVariable v) body) -> do
        cd <- toClassDecl body
        return cd {Java.normalClassDeclarationParameters = addParameter v (Java.normalClassDeclarationParameters cd)}
      _ -> fail $ "unexpected type: " ++ show t
    elName = elementName el
    addParameter v params = params ++ [Java.TypeParameter [] (javaTypeIdentifier v) Nothing]
    javaDeclName = javaTypeIdentifier (localNameOf elName)
    classDecl supname bodyDecls = Java.NormalClassDeclaration {
      Java.normalClassDeclarationModifiers = [Java.ClassModifierPublic],
      Java.normalClassDeclarationIdentifier = javaDeclName,
      Java.normalClassDeclarationParameters = [],
      Java.normalClassDeclarationExtends = fmap (nameToJavaClassType aliases True) supname,
      Java.normalClassDeclarationImplements = [],
      Java.normalClassDeclarationBody = Java.ClassBody bodyDecls}

-- Transform a given type into a type which can be used as the basis for a Java class
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
  -- Note: record and union types should not appear at this level
  _ -> fail $ "can't encode unsupported type in Java: " ++ show t
  where
    encode = encodeType aliases
