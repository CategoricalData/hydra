-- Note: this is an automatically generated file. Do not edit.

-- | Java code generator: converts Hydra modules to Java source code

module Hydra.Ext.Java.Coder where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Arity as Arity
import qualified Hydra.CoderUtils as CoderUtils
import qualified Hydra.Constants as Constants
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Encode.Core as Core__
import qualified Hydra.Error as Error
import qualified Hydra.Ext.Java.Helpers as Helpers
import qualified Hydra.Ext.Java.Language as Language
import qualified Hydra.Ext.Java.Names as Names
import qualified Hydra.Ext.Java.Serde as Serde
import qualified Hydra.Ext.Java.Syntax as Syntax
import qualified Hydra.Ext.Java.Utils as Utils
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names_
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Show.Core as Core___
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

java8Features :: Helpers.JavaFeatures
java8Features = Helpers.JavaFeatures {
  Helpers.javaFeaturesSupportsDiamondOperator = False}

java11Features :: Helpers.JavaFeatures
java11Features = Helpers.JavaFeatures {
  Helpers.javaFeaturesSupportsDiamondOperator = True}

javaFeatures :: Helpers.JavaFeatures
javaFeatures = java11Features

classModsPublic :: [Syntax.ClassModifier]
classModsPublic = [
  Syntax.ClassModifierPublic]

noComment :: (Syntax.ClassBodyDeclaration -> Syntax.ClassBodyDeclarationWithComments)
noComment decl = Syntax.ClassBodyDeclarationWithComments {
  Syntax.classBodyDeclarationWithCommentsValue = decl,
  Syntax.classBodyDeclarationWithCommentsComments = Nothing}

typeArgsOrDiamond :: ([Syntax.TypeArgument] -> Syntax.TypeArgumentsOrDiamond)
typeArgsOrDiamond args = (Logic.ifElse (Helpers.javaFeaturesSupportsDiamondOperator javaFeatures) Syntax.TypeArgumentsOrDiamondDiamond (Syntax.TypeArgumentsOrDiamondArguments args))

bindingNameToFilePath :: (Core.Name -> String)
bindingNameToFilePath name =  
  let qn = (Names_.qualifyName name) 
      ns_ = (Module.qualifiedNameNamespace qn)
      local = (Module.qualifiedNameLocal qn)
      sanitized = (Formatting.sanitizeWithUnderscores Language.reservedWords local)
      unq = (Names_.unqualifyName (Module.QualifiedName {
              Module.qualifiedNameNamespace = ns_,
              Module.qualifiedNameLocal = sanitized}))
  in (CoderUtils.nameToFilePath Util.CaseConventionCamel Util.CaseConventionPascal (Module.FileExtension "java") unq)

javaIdentifierToString :: (Syntax.Identifier -> String)
javaIdentifierToString id = (Syntax.unIdentifier id)

boundTypeVariables :: (Core.Type -> [Core.Name])
boundTypeVariables typ = ((\x -> case x of
  Core.TypeAnnotated v0 -> (boundTypeVariables (Core.annotatedTypeBody v0))
  Core.TypeForall v0 -> (Lists.cons (Core.forallTypeParameter v0) (boundTypeVariables (Core.forallTypeBody v0)))
  _ -> []) typ)

extractTypeApplicationArgs :: (Core.Type -> [Core.Type])
extractTypeApplicationArgs typ = (Lists.reverse (extractTypeApplicationArgs_go typ))

extractTypeApplicationArgs_go :: (Core.Type -> [Core.Type])
extractTypeApplicationArgs_go t = ((\x -> case x of
  Core.TypeApplication v0 -> (Lists.cons (Core.applicationTypeArgument v0) (extractTypeApplicationArgs_go (Core.applicationTypeFunction v0)))
  _ -> []) t)

javaTypeParametersForType :: (Core.Type -> [Syntax.TypeParameter])
javaTypeParametersForType typ =  
  let toParam = (\name -> Utils.javaTypeParameter (Formatting.capitalize (Core.unName name))) 
      boundVars = (javaTypeParametersForType_bvars typ)
      freeVars = (Lists.filter (\v -> isLambdaBoundVariable v) (Sets.toList (Rewriting.freeVariablesInType typ)))
      vars = (Lists.nub (Lists.concat2 boundVars freeVars))
  in (Lists.map toParam vars)

javaTypeParametersForType_bvars :: (Core.Type -> [Core.Name])
javaTypeParametersForType_bvars t = ((\x -> case x of
  Core.TypeForall v0 -> (Lists.cons (Core.forallTypeParameter v0) (javaTypeParametersForType_bvars (Core.forallTypeBody v0)))
  _ -> []) t)

javaTypeArgumentsForType :: (Core.Type -> [Syntax.TypeArgument])
javaTypeArgumentsForType typ = (Lists.reverse (Lists.map Utils.typeParameterToTypeArgument (javaTypeParametersForType typ)))

isLambdaBoundVariable :: (Core.Name -> Bool)
isLambdaBoundVariable name =  
  let v = (Core.unName name)
  in (Equality.lte (Strings.length v) 4)

isLocalVariable :: (Core.Name -> Bool)
isLocalVariable name = (Maybes.isNothing (Module.qualifiedNameNamespace (Names_.qualifyName name)))

serializableTypes :: (Bool -> [Syntax.InterfaceType])
serializableTypes isSer =  
  let javaSerializableType = (Syntax.InterfaceType (Syntax.ClassType {
          Syntax.classTypeAnnotations = [],
          Syntax.classTypeQualifier = Syntax.ClassTypeQualifierNone,
          Syntax.classTypeIdentifier = (Utils.javaTypeIdentifier "Serializable"),
          Syntax.classTypeArguments = []}))
  in (Logic.ifElse isSer [
    javaSerializableType] [])

encodeLiteralType :: (Core.LiteralType -> t0 -> t1 -> Either t2 Syntax.Type)
encodeLiteralType lt cx g = ((\x -> case x of
  Core.LiteralTypeBinary -> (Right (Syntax.TypeReference (Syntax.ReferenceTypeArray (Syntax.ArrayType {
    Syntax.arrayTypeDims = (Syntax.Dims [
      []]),
    Syntax.arrayTypeVariant = (Syntax.ArrayType_VariantPrimitive (Syntax.PrimitiveTypeWithAnnotations {
      Syntax.primitiveTypeWithAnnotationsType = (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeByte)),
      Syntax.primitiveTypeWithAnnotationsAnnotations = []}))}))))
  Core.LiteralTypeBoolean -> (encodeLiteralType_simple "Boolean" cx g)
  Core.LiteralTypeFloat v0 -> ((\x -> case x of
    Core.FloatTypeBigfloat -> (Right (Utils.javaRefType [] (Just (Names.javaPackageName [
      "java",
      "math"])) "BigDecimal"))
    Core.FloatTypeFloat32 -> (encodeLiteralType_simple "Float" cx g)
    Core.FloatTypeFloat64 -> (encodeLiteralType_simple "Double" cx g)) v0)
  Core.LiteralTypeInteger v0 -> ((\x -> case x of
    Core.IntegerTypeBigint -> (Right (Utils.javaRefType [] (Just (Names.javaPackageName [
      "java",
      "math"])) "BigInteger"))
    Core.IntegerTypeInt8 -> (encodeLiteralType_simple "Byte" cx g)
    Core.IntegerTypeInt16 -> (encodeLiteralType_simple "Short" cx g)
    Core.IntegerTypeInt32 -> (encodeLiteralType_simple "Integer" cx g)
    Core.IntegerTypeInt64 -> (encodeLiteralType_simple "Long" cx g)
    Core.IntegerTypeUint8 -> (encodeLiteralType_simple "Short" cx g)
    Core.IntegerTypeUint16 -> (encodeLiteralType_simple "Character" cx g)
    Core.IntegerTypeUint32 -> (encodeLiteralType_simple "Long" cx g)
    Core.IntegerTypeUint64 -> (Right (Utils.javaRefType [] (Just (Names.javaPackageName [
      "java",
      "math"])) "BigInteger"))) v0)
  Core.LiteralTypeString -> (encodeLiteralType_simple "String" cx g)) lt)

encodeLiteralType_simple :: (String -> t0 -> t1 -> Either t2 Syntax.Type)
encodeLiteralType_simple n cx g = (Right (Utils.javaRefType [] Nothing n))

elementsClassName :: (Module.Namespace -> String)
elementsClassName ns =  
  let nsStr = (Module.unNamespace ns) 
      parts = (Strings.splitOn "." nsStr)
  in (Formatting.sanitizeWithUnderscores Language.reservedWords (Formatting.capitalize (Lists.last parts)))

isRecursiveVariable :: (Helpers.Aliases -> Core.Name -> Bool)
isRecursiveVariable aliases name = (Sets.member name (Helpers.aliasesRecursiveVars aliases))

interfaceTypes :: (Bool -> Helpers.Aliases -> [Syntax.TypeParameter] -> Core.Name -> [Syntax.InterfaceType])
interfaceTypes isSer aliases tparams elName =  
  let javaSerializableType = (Syntax.InterfaceType (Syntax.ClassType {
          Syntax.classTypeAnnotations = [],
          Syntax.classTypeQualifier = Syntax.ClassTypeQualifierNone,
          Syntax.classTypeIdentifier = (Utils.javaTypeIdentifier "Serializable"),
          Syntax.classTypeArguments = []})) 
      selfTypeArg = (Syntax.TypeArgumentReference (Utils.nameToJavaReferenceType aliases False (Lists.map (\tp_ -> Utils.typeParameterToTypeArgument tp_) tparams) elName Nothing))
      javaComparableType = (Syntax.InterfaceType (Syntax.ClassType {
              Syntax.classTypeAnnotations = [],
              Syntax.classTypeQualifier = Syntax.ClassTypeQualifierNone,
              Syntax.classTypeIdentifier = (Utils.javaTypeIdentifier "Comparable"),
              Syntax.classTypeArguments = [
                selfTypeArg]}))
  in (Logic.ifElse isSer [
    javaSerializableType,
    javaComparableType] [])

isNonComparableType :: (Core.Type -> Bool)
isNonComparableType typ = ((\x -> case x of
  Core.TypeList _ -> True
  Core.TypeSet _ -> True
  Core.TypeMap _ -> True
  Core.TypeMaybe _ -> True
  Core.TypeEither _ -> True
  Core.TypeFunction _ -> True
  Core.TypeUnit -> True
  Core.TypeLiteral v0 -> ((\x -> case x of
    Core.LiteralTypeBinary -> True
    _ -> False) v0)
  Core.TypeForall v0 -> (isNonComparableType (Core.forallTypeBody v0))
  _ -> False) (Rewriting.deannotateType typ))

isBinaryType :: (Core.Type -> Bool)
isBinaryType typ = ((\x -> case x of
  Core.TypeLiteral v0 -> ((\x -> case x of
    Core.LiteralTypeBinary -> True
    _ -> False) v0)
  _ -> False) (Rewriting.deannotateType typ))

isBigNumericType :: (Core.Type -> Bool)
isBigNumericType typ = ((\x -> case x of
  Core.TypeLiteral v0 -> ((\x -> case x of
    Core.LiteralTypeFloat v1 -> ((\x -> case x of
      Core.FloatTypeBigfloat -> True
      _ -> False) v1)
    Core.LiteralTypeInteger v1 -> ((\x -> case x of
      Core.IntegerTypeBigint -> True
      _ -> False) v1)
    _ -> False) v0)
  _ -> False) (Rewriting.deannotateType typ))

innerClassRef :: (Helpers.Aliases -> Core.Name -> String -> Syntax.Identifier)
innerClassRef aliases name local =  
  let id = (Syntax.unIdentifier (Utils.nameToJavaName aliases name))
  in (Syntax.Identifier (Strings.cat2 (Strings.cat2 id ".") local))

peelExpectedTypes :: (M.Map Core.Name Core.Type -> Int -> Core.Type -> [Core.Type])
peelExpectedTypes subst n t = (Logic.ifElse (Equality.equal n 0) [] ((\x -> case x of
  Core.TypeFunction v0 -> (Lists.cons (applySubstFull subst (Core.functionTypeDomain v0)) (peelExpectedTypes subst (Math.sub n 1) (Core.functionTypeCodomain v0)))
  _ -> []) (Rewriting.deannotateType t)))

applySubstFull :: (M.Map Core.Name Core.Type -> Core.Type -> Core.Type)
applySubstFull s t = ((\x -> case x of
  Core.TypeVariable v0 -> (Maps.findWithDefault t v0 s)
  Core.TypeFunction v0 -> (Core.TypeFunction (Core.FunctionType {
    Core.functionTypeDomain = (applySubstFull s (Core.functionTypeDomain v0)),
    Core.functionTypeCodomain = (applySubstFull s (Core.functionTypeCodomain v0))}))
  Core.TypeApplication v0 -> (Core.TypeApplication (Core.ApplicationType {
    Core.applicationTypeFunction = (applySubstFull s (Core.applicationTypeFunction v0)),
    Core.applicationTypeArgument = (applySubstFull s (Core.applicationTypeArgument v0))}))
  Core.TypeList v0 -> (Core.TypeList (applySubstFull s v0))
  Core.TypeSet v0 -> (Core.TypeSet (applySubstFull s v0))
  Core.TypeMaybe v0 -> (Core.TypeMaybe (applySubstFull s v0))
  Core.TypeMap v0 -> (Core.TypeMap (Core.MapType {
    Core.mapTypeKeys = (applySubstFull s (Core.mapTypeKeys v0)),
    Core.mapTypeValues = (applySubstFull s (Core.mapTypeValues v0))}))
  Core.TypePair v0 -> (Core.TypePair (Core.PairType {
    Core.pairTypeFirst = (applySubstFull s (Core.pairTypeFirst v0)),
    Core.pairTypeSecond = (applySubstFull s (Core.pairTypeSecond v0))}))
  Core.TypeEither v0 -> (Core.TypeEither (Core.EitherType {
    Core.eitherTypeLeft = (applySubstFull s (Core.eitherTypeLeft v0)),
    Core.eitherTypeRight = (applySubstFull s (Core.eitherTypeRight v0))}))
  Core.TypeForall v0 -> (Core.TypeForall (Core.ForallType {
    Core.forallTypeParameter = (Core.forallTypeParameter v0),
    Core.forallTypeBody = (applySubstFull (Maps.delete (Core.forallTypeParameter v0) s) (Core.forallTypeBody v0))}))
  _ -> t) (Rewriting.deannotateType t))

collectForallParams :: (Core.Type -> [Core.Name])
collectForallParams t = ((\x -> case x of
  Core.TypeForall v0 -> (Lists.cons (Core.forallTypeParameter v0) (collectForallParams (Core.forallTypeBody v0)))
  _ -> []) (Rewriting.deannotateType t))

stripForalls :: (Core.Type -> Core.Type)
stripForalls t = ((\x -> case x of
  Core.TypeForall v0 -> (stripForalls (Core.forallTypeBody v0))
  _ -> t) (Rewriting.deannotateType t))

collectTypeVars :: (Core.Type -> S.Set Core.Name)
collectTypeVars typ = (collectTypeVars_go (Rewriting.deannotateType typ))

collectTypeVars_go :: (Core.Type -> S.Set Core.Name)
collectTypeVars_go t = ((\x -> case x of
  Core.TypeVariable v0 -> (Sets.singleton v0)
  Core.TypeFunction v0 -> (Sets.union (collectTypeVars_go (Rewriting.deannotateType (Core.functionTypeDomain v0))) (collectTypeVars_go (Rewriting.deannotateType (Core.functionTypeCodomain v0))))
  Core.TypeApplication v0 -> (Sets.union (collectTypeVars_go (Rewriting.deannotateType (Core.applicationTypeFunction v0))) (collectTypeVars_go (Rewriting.deannotateType (Core.applicationTypeArgument v0))))
  Core.TypeList v0 -> (collectTypeVars_go (Rewriting.deannotateType v0))
  Core.TypeSet v0 -> (collectTypeVars_go (Rewriting.deannotateType v0))
  Core.TypeMaybe v0 -> (collectTypeVars_go (Rewriting.deannotateType v0))
  Core.TypeMap v0 -> (Sets.union (collectTypeVars_go (Rewriting.deannotateType (Core.mapTypeKeys v0))) (collectTypeVars_go (Rewriting.deannotateType (Core.mapTypeValues v0))))
  Core.TypePair v0 -> (Sets.union (collectTypeVars_go (Rewriting.deannotateType (Core.pairTypeFirst v0))) (collectTypeVars_go (Rewriting.deannotateType (Core.pairTypeSecond v0))))
  Core.TypeEither v0 -> (Sets.union (collectTypeVars_go (Rewriting.deannotateType (Core.eitherTypeLeft v0))) (collectTypeVars_go (Rewriting.deannotateType (Core.eitherTypeRight v0))))
  Core.TypeForall v0 -> (collectTypeVars_go (Rewriting.deannotateType (Core.forallTypeBody v0)))
  _ -> Sets.empty) t)

substituteTypeVarsWithTypes :: (M.Map Core.Name Core.Type -> Core.Type -> Core.Type)
substituteTypeVarsWithTypes subst t = (substituteTypeVarsWithTypes_go subst (Rewriting.deannotateType t))

substituteTypeVarsWithTypes_go :: (M.Map Core.Name Core.Type -> Core.Type -> Core.Type)
substituteTypeVarsWithTypes_go subst t = ((\x -> case x of
  Core.TypeVariable v0 -> (Maybes.cases (Maps.lookup v0 subst) t (\rep -> rep))
  Core.TypeFunction v0 -> (Core.TypeFunction (Core.FunctionType {
    Core.functionTypeDomain = (substituteTypeVarsWithTypes_go subst (Core.functionTypeDomain v0)),
    Core.functionTypeCodomain = (substituteTypeVarsWithTypes_go subst (Core.functionTypeCodomain v0))}))
  Core.TypeApplication v0 -> (Core.TypeApplication (Core.ApplicationType {
    Core.applicationTypeFunction = (substituteTypeVarsWithTypes_go subst (Core.applicationTypeFunction v0)),
    Core.applicationTypeArgument = (substituteTypeVarsWithTypes_go subst (Core.applicationTypeArgument v0))}))
  Core.TypeList v0 -> (Core.TypeList (substituteTypeVarsWithTypes_go subst v0))
  Core.TypeSet v0 -> (Core.TypeSet (substituteTypeVarsWithTypes_go subst v0))
  Core.TypeMaybe v0 -> (Core.TypeMaybe (substituteTypeVarsWithTypes_go subst v0))
  Core.TypeMap v0 -> (Core.TypeMap (Core.MapType {
    Core.mapTypeKeys = (substituteTypeVarsWithTypes_go subst (Core.mapTypeKeys v0)),
    Core.mapTypeValues = (substituteTypeVarsWithTypes_go subst (Core.mapTypeValues v0))}))
  Core.TypePair v0 -> (Core.TypePair (Core.PairType {
    Core.pairTypeFirst = (substituteTypeVarsWithTypes_go subst (Core.pairTypeFirst v0)),
    Core.pairTypeSecond = (substituteTypeVarsWithTypes_go subst (Core.pairTypeSecond v0))}))
  Core.TypeEither v0 -> (Core.TypeEither (Core.EitherType {
    Core.eitherTypeLeft = (substituteTypeVarsWithTypes_go subst (Core.eitherTypeLeft v0)),
    Core.eitherTypeRight = (substituteTypeVarsWithTypes_go subst (Core.eitherTypeRight v0))}))
  Core.TypeForall v0 -> (Core.TypeForall (Core.ForallType {
    Core.forallTypeParameter = (Core.forallTypeParameter v0),
    Core.forallTypeBody = (substituteTypeVarsWithTypes_go subst (Core.forallTypeBody v0))}))
  _ -> t) (Rewriting.deannotateType t))

addComment :: (Syntax.ClassBodyDeclaration -> Core.FieldType -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.ClassBodyDeclarationWithComments)
addComment decl field cx g = (Eithers.map (\c -> Syntax.ClassBodyDeclarationWithComments {
  Syntax.classBodyDeclarationWithCommentsValue = decl,
  Syntax.classBodyDeclarationWithCommentsComments = c}) (CoderUtils.commentsFromFieldType cx g field))

insertBranchVar :: (Core.Name -> Helpers.JavaEnvironment -> Helpers.JavaEnvironment)
insertBranchVar name env =  
  let aliases = (Helpers.javaEnvironmentAliases env)
  in Helpers.JavaEnvironment {
    Helpers.javaEnvironmentAliases = Helpers.Aliases {
      Helpers.aliasesCurrentNamespace = (Helpers.aliasesCurrentNamespace aliases),
      Helpers.aliasesPackages = (Helpers.aliasesPackages aliases),
      Helpers.aliasesBranchVars = (Sets.insert name (Helpers.aliasesBranchVars aliases)),
      Helpers.aliasesRecursiveVars = (Helpers.aliasesRecursiveVars aliases),
      Helpers.aliasesInScopeTypeParams = (Helpers.aliasesInScopeTypeParams aliases),
      Helpers.aliasesPolymorphicLocals = (Helpers.aliasesPolymorphicLocals aliases),
      Helpers.aliasesInScopeJavaVars = (Helpers.aliasesInScopeJavaVars aliases),
      Helpers.aliasesVarRenames = (Helpers.aliasesVarRenames aliases),
      Helpers.aliasesLambdaVars = (Helpers.aliasesLambdaVars aliases),
      Helpers.aliasesTypeVarSubst = (Helpers.aliasesTypeVarSubst aliases),
      Helpers.aliasesTrustedTypeVars = (Helpers.aliasesTrustedTypeVars aliases),
      Helpers.aliasesMethodCodomain = (Helpers.aliasesMethodCodomain aliases),
      Helpers.aliasesThunkedVars = (Helpers.aliasesThunkedVars aliases)},
    Helpers.javaEnvironmentGraph = (Helpers.javaEnvironmentGraph env)}

getCodomain :: (M.Map Core.Name Core.Term -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Core.Type)
getCodomain ann cx g = (Eithers.map (\ft -> Core.functionTypeCodomain ft) (getFunctionType ann cx g))

getFunctionType :: (M.Map Core.Name Core.Term -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Core.FunctionType)
getFunctionType ann cx g = (Eithers.bind (Eithers.bimap (\_de -> Context.InContext {
  Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unDecodingError _de))),
  Context.inContextContext = cx}) (\_a -> _a) (Annotations.getType g ann)) (\mt -> Maybes.cases mt (Left (Context.InContext {
  Context.inContextObject = (Error.ErrorOther (Error.OtherError "type annotation is required for function and elimination terms in Java")),
  Context.inContextContext = cx})) (\t -> (\x -> case x of
  Core.TypeFunction v0 -> (Right v0)
  _ -> (Left (Context.InContext {
    Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "expected function type, got: " (Core___.type_ t)))),
    Context.inContextContext = cx}))) t)))

wrapLazyArguments :: (Core.Name -> [Syntax.Expression] -> ([Syntax.Expression], (Maybe String)))
wrapLazyArguments name args = (Logic.ifElse (Logic.and (Equality.equal name (Core.Name "hydra.lib.logic.ifElse")) (Equality.equal (Lists.length args) 3)) ([
  Lists.at 0 args,
  (wrapInSupplierLambda (Lists.at 1 args)),
  (wrapInSupplierLambda (Lists.at 2 args))], (Just "lazy")) (Logic.ifElse (Logic.and (Equality.equal name (Core.Name "hydra.lib.maybes.maybe")) (Equality.equal (Lists.length args) 3)) ([
  wrapInSupplierLambda (Lists.at 0 args),
  (Lists.at 1 args),
  (Lists.at 2 args)], (Just "applyLazy")) (Logic.ifElse (Logic.and (Equality.equal name (Core.Name "hydra.lib.maybes.cases")) (Equality.equal (Lists.length args) 3)) ([
  Lists.at 0 args,
  (wrapInSupplierLambda (Lists.at 1 args)),
  (Lists.at 2 args)], (Just "applyLazy")) (Logic.ifElse (Logic.and (Equality.equal name (Core.Name "hydra.lib.maps.findWithDefault")) (Equality.equal (Lists.length args) 3)) ([
  wrapInSupplierLambda (Lists.at 0 args),
  (Lists.at 1 args),
  (Lists.at 2 args)], (Just "applyLazy")) (Logic.ifElse (Logic.and (Logic.or (Equality.equal name (Core.Name "hydra.lib.maybes.fromMaybe")) (Logic.or (Equality.equal name (Core.Name "hydra.lib.eithers.fromLeft")) (Equality.equal name (Core.Name "hydra.lib.eithers.fromRight")))) (Equality.equal (Lists.length args) 2)) ([
  wrapInSupplierLambda (Lists.at 0 args),
  (Lists.at 1 args)], (Just "applyLazy")) (args, Nothing))))))

wrapInSupplierLambda :: (Syntax.Expression -> Syntax.Expression)
wrapInSupplierLambda expr = (Syntax.ExpressionLambda (Syntax.LambdaExpression {
  Syntax.lambdaExpressionParameters = (Syntax.LambdaParametersTuple []),
  Syntax.lambdaExpressionBody = (Syntax.LambdaBodyExpression expr)}))

elementJavaIdentifier :: (Bool -> Bool -> Helpers.Aliases -> Core.Name -> Syntax.Identifier)
elementJavaIdentifier isPrim isMethod aliases name =  
  let qn = (Names_.qualifyName name) 
      ns_ = (Module.qualifiedNameNamespace qn)
      local = (Module.qualifiedNameLocal qn)
      sep = (Logic.ifElse isMethod "::" ".")
  in (Logic.ifElse isPrim (Syntax.Identifier (Strings.cat2 (Strings.cat2 (elementJavaIdentifier_qualify aliases ns_ (Formatting.capitalize local)) ".") Names.applyMethodName)) (Maybes.cases ns_ (Syntax.Identifier (Utils.sanitizeJavaName local)) (\n -> Syntax.Identifier (Strings.cat2 (Strings.cat2 (elementJavaIdentifier_qualify aliases (Just n) (elementsClassName n)) sep) (Utils.sanitizeJavaName local)))))

elementJavaIdentifier_qualify :: (Helpers.Aliases -> Maybe Module.Namespace -> String -> String)
elementJavaIdentifier_qualify aliases mns s = (Syntax.unIdentifier (Utils.nameToJavaName aliases (Names_.unqualifyName (Module.QualifiedName {
  Module.qualifiedNameNamespace = mns,
  Module.qualifiedNameLocal = s}))))

isLambdaBoundIn :: (Core.Name -> S.Set Core.Name -> Bool)
isLambdaBoundIn name lambdaVars = (Logic.or (Sets.member name lambdaVars) (Logic.or (Logic.and (isLambdaBoundIn_isQualified name) (Maybes.isJust (Lists.find (\lv -> Logic.and (isLambdaBoundIn_isQualified lv) (Equality.equal (Names_.localNameOf lv) (Names_.localNameOf name))) (Sets.toList lambdaVars)))) (Logic.and (Logic.not (isLambdaBoundIn_isQualified name)) (Sets.member (Core.Name (Names_.localNameOf name)) lambdaVars))))

isLambdaBoundIn_isQualified :: (Core.Name -> Bool)
isLambdaBoundIn_isQualified n = (Maybes.isJust (Module.qualifiedNameNamespace (Names_.qualifyName n)))

findMatchingLambdaVar :: (Core.Name -> S.Set Core.Name -> Core.Name)
findMatchingLambdaVar name lambdaVars = (Logic.ifElse (Sets.member name lambdaVars) name (Logic.ifElse (isLambdaBoundIn_isQualified name) (Maybes.fromMaybe name (Lists.find (\lv -> Logic.and (isLambdaBoundIn_isQualified lv) (Equality.equal (Names_.localNameOf lv) (Names_.localNameOf name))) (Sets.toList lambdaVars))) (Logic.ifElse (Sets.member (Core.Name (Names_.localNameOf name)) lambdaVars) (Core.Name (Names_.localNameOf name)) name)))

constructElementsInterface :: (Module.Module -> [Syntax.InterfaceMemberDeclaration] -> (Core.Name, Syntax.CompilationUnit))
constructElementsInterface mod members =  
  let pkg = (Utils.javaPackageDeclaration (Module.moduleNamespace mod)) 
      mods = [
              Syntax.InterfaceModifierPublic]
      className = (elementsClassName (Module.moduleNamespace mod))
      elName = (Names_.unqualifyName (Module.QualifiedName {
              Module.qualifiedNameNamespace = (Just (Module.moduleNamespace mod)),
              Module.qualifiedNameLocal = className}))
      body = (Syntax.InterfaceBody members)
      itf = (Syntax.TypeDeclarationInterface (Syntax.InterfaceDeclarationNormalInterface (Syntax.NormalInterfaceDeclaration {
              Syntax.normalInterfaceDeclarationModifiers = mods,
              Syntax.normalInterfaceDeclarationIdentifier = (Utils.javaTypeIdentifier className),
              Syntax.normalInterfaceDeclarationParameters = [],
              Syntax.normalInterfaceDeclarationExtends = [],
              Syntax.normalInterfaceDeclarationBody = body})))
      decl = Syntax.TypeDeclarationWithComments {
              Syntax.typeDeclarationWithCommentsValue = itf,
              Syntax.typeDeclarationWithCommentsComments = (Module.moduleDescription mod)}
  in (elName, (Syntax.CompilationUnitOrdinary (Syntax.OrdinaryCompilationUnit {
    Syntax.ordinaryCompilationUnitPackage = (Just pkg),
    Syntax.ordinaryCompilationUnitImports = [],
    Syntax.ordinaryCompilationUnitTypes = [
      decl]})))

splitConstantInitializer :: (Syntax.InterfaceMemberDeclaration -> [Syntax.InterfaceMemberDeclaration])
splitConstantInitializer member = ((\x -> case x of
  Syntax.InterfaceMemberDeclarationConstant v0 -> (Lists.bind (Syntax.constantDeclarationVariables v0) (splitConstantInitializer_splitVar (Syntax.constantDeclarationModifiers v0) (Syntax.constantDeclarationType v0)))
  _ -> [
    member]) member)

splitConstantInitializer_splitVar :: ([Syntax.ConstantModifier] -> Syntax.UnannType -> Syntax.VariableDeclarator -> [Syntax.InterfaceMemberDeclaration])
splitConstantInitializer_splitVar mods utype vd =  
  let vid = (Syntax.variableDeclaratorId vd) 
      mInit = (Syntax.variableDeclaratorInitializer vd)
  in (Maybes.cases mInit [
    Syntax.InterfaceMemberDeclarationConstant (Syntax.ConstantDeclaration {
      Syntax.constantDeclarationModifiers = mods,
      Syntax.constantDeclarationType = utype,
      Syntax.constantDeclarationVariables = [
        vd]})] (\init_ -> (\x -> case x of
    Syntax.VariableInitializerExpression v0 ->  
      let varName = (javaIdentifierToString (Syntax.variableDeclaratorIdIdentifier vid)) 
          helperName = (Strings.cat2 "_init_" varName)
          callExpr = (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocation Nothing (Syntax.Identifier helperName) []))
          field = (Syntax.InterfaceMemberDeclarationConstant (Syntax.ConstantDeclaration {
                  Syntax.constantDeclarationModifiers = mods,
                  Syntax.constantDeclarationType = utype,
                  Syntax.constantDeclarationVariables = [
                    Syntax.VariableDeclarator {
                      Syntax.variableDeclaratorId = vid,
                      Syntax.variableDeclaratorInitializer = (Just (Syntax.VariableInitializerExpression callExpr))}]}))
          returnSt = (Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just v0)))
          resultType = (Syntax.ResultType utype)
          helper = (Utils.interfaceMethodDeclaration [
                  Syntax.InterfaceMethodModifierStatic,
                  Syntax.InterfaceMethodModifierPrivate] [] helperName [] resultType (Just [
                  returnSt]))
      in [
        field,
        helper]
    _ -> [
      Syntax.InterfaceMemberDeclarationConstant (Syntax.ConstantDeclaration {
        Syntax.constantDeclarationModifiers = mods,
        Syntax.constantDeclarationType = utype,
        Syntax.constantDeclarationVariables = [
          vd]})]) init_))

isUnresolvedInferenceVar :: (Core.Name -> Bool)
isUnresolvedInferenceVar name =  
  let chars = (Strings.toList (Core.unName name))
  in (Logic.ifElse (Lists.null chars) False (Logic.ifElse (Logic.not (Equality.equal (Lists.head chars) 116)) False ( 
    let rest = (Lists.tail chars)
    in (Logic.and (Logic.not (Lists.null rest)) (Lists.null (Lists.filter (\c -> Logic.not (isUnresolvedInferenceVar_isDigit c)) rest))))))

isUnresolvedInferenceVar_isDigit :: (Int -> Bool)
isUnresolvedInferenceVar_isDigit c = (Logic.and (Equality.gte c 48) (Equality.lte c 57))

classifyDataTerm :: (Core.TypeScheme -> Core.Term -> Helpers.JavaSymbolClass)
classifyDataTerm ts term = (Logic.ifElse (Rewriting.isLambda term) ( 
  let n = (classifyDataTerm_countLambdaParams term)
  in (Logic.ifElse (Equality.gt n 1) (Helpers.JavaSymbolClassHoistedLambda n) Helpers.JavaSymbolClassUnaryFunction)) ( 
  let hasTypeParams = (Logic.not (Lists.null (Core.typeSchemeVariables ts)))
  in (Logic.ifElse hasTypeParams ( 
    let n2 = (classifyDataTerm_countLambdaParams (classifyDataTerm_stripTypeLambdas term))
    in (Logic.ifElse (Equality.gt n2 0) (Helpers.JavaSymbolClassHoistedLambda n2) Helpers.JavaSymbolClassNullaryFunction)) Helpers.JavaSymbolClassNullaryFunction)))

classifyDataTerm_countLambdaParams :: (Core.Term -> Int)
classifyDataTerm_countLambdaParams t = ((\x -> case x of
  Core.TermFunction v0 -> ((\x -> case x of
    Core.FunctionLambda v1 -> (Math.add 1 (classifyDataTerm_countLambdaParams (Core.lambdaBody v1)))
    _ -> 0) v0)
  Core.TermLet v0 -> (classifyDataTerm_countLambdaParams (Core.letBody v0))
  _ -> 0) (Rewriting.deannotateTerm t))

classifyDataTerm_stripTypeLambdas :: (Core.Term -> Core.Term)
classifyDataTerm_stripTypeLambdas t = ((\x -> case x of
  Core.TermTypeLambda v0 -> (classifyDataTerm_stripTypeLambdas (Core.typeLambdaBody v0))
  _ -> t) (Rewriting.deannotateTerm t))

classifyDataReference :: (Core.Name -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Helpers.JavaSymbolClass)
classifyDataReference name cx g = (Eithers.bind (Right (Lexical.dereferenceElement g name)) (\mel -> Maybes.cases mel (Right Helpers.JavaSymbolClassLocalVariable) (\el -> Maybes.cases (Core.bindingType el) (Left (Context.InContext {
  Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "no type scheme for element " (Core.unName (Core.bindingName el))))),
  Context.inContextContext = cx})) (\ts -> Right (classifyDataTerm ts (Core.bindingTerm el))))))

encodeType :: (Helpers.Aliases -> S.Set Core.Name -> Core.Type -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.Type)
encodeType aliases boundVars t cx g =  
  let inScopeTypeParams = (Helpers.aliasesInScopeTypeParams aliases)
  in  
    let typeVarSubst = (Helpers.aliasesTypeVarSubst aliases)
    in ((\x -> case x of
      Core.TypeApplication v0 -> (Eithers.bind (encodeType aliases boundVars (Core.applicationTypeFunction v0) cx g) (\jlhs -> Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.applicationTypeArgument v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jrhs -> Utils.addJavaTypeParameter jrhs jlhs cx)))
      Core.TypeFunction v0 -> (Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.functionTypeDomain v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jdom -> Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.functionTypeCodomain v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jcod -> Right (Utils.javaRefType [
        jdom,
        jcod] Names.javaUtilFunctionPackageName "Function"))))
      Core.TypeForall v0 -> (Eithers.bind (encodeType aliases (Sets.insert (Core.forallTypeParameter v0) boundVars) (Core.forallTypeBody v0) cx g) (\jbody -> Utils.addJavaTypeParameter (Utils.javaTypeVariable (Core.unName (Core.forallTypeParameter v0))) jbody cx))
      Core.TypeList v0 -> (Eithers.bind (encodeType aliases boundVars v0 cx g) (\jet -> Eithers.bind (Eithers.bind (Right jet) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\rt -> Right (Utils.javaRefType [
        rt] Names.hydraUtilPackageName "ConsList"))))
      Core.TypeLiteral v0 -> (encodeLiteralType v0 cx g)
      Core.TypeEither v0 -> (Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.eitherTypeLeft v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jlt -> Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.eitherTypeRight v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jrt -> Right (Utils.javaRefType [
        jlt,
        jrt] Names.hydraUtilPackageName "Either"))))
      Core.TypeMap v0 -> (Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.mapTypeKeys v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jkt -> Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.mapTypeValues v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jvt -> Right (Utils.javaRefType [
        jkt,
        jvt] Names.hydraUtilPackageName "PersistentMap"))))
      Core.TypePair v0 -> (Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.pairTypeFirst v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jfirst -> Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.pairTypeSecond v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jsecond -> Right (Utils.javaRefType [
        jfirst,
        jsecond] Names.hydraUtilPackageName "Pair"))))
      Core.TypeUnit -> (Right (Utils.javaRefType [] Names.javaLangPackageName "Void"))
      Core.TypeRecord v0 -> (Logic.ifElse (Logic.and (Equality.equal (Core.rowTypeTypeName v0) (Core.Name "hydra.core.Unit")) (Lists.null (Core.rowTypeFields v0))) (Right (Utils.javaRefType [] Names.javaLangPackageName "Void")) (Right (Syntax.TypeReference (Utils.nameToJavaReferenceType aliases True (javaTypeArgumentsForType t) (Core.rowTypeTypeName v0) Nothing))))
      Core.TypeMaybe v0 -> (Eithers.bind (Eithers.bind (encodeType aliases boundVars v0 cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jot -> Right (Utils.javaRefType [
        jot] Names.hydraUtilPackageName "Maybe")))
      Core.TypeSet v0 -> (Eithers.bind (Eithers.bind (encodeType aliases boundVars v0 cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jst -> Right (Utils.javaRefType [
        jst] Names.hydraUtilPackageName "PersistentSet")))
      Core.TypeUnion v0 -> (Right (Syntax.TypeReference (Utils.nameToJavaReferenceType aliases True (javaTypeArgumentsForType t) (Core.rowTypeTypeName v0) Nothing)))
      Core.TypeVariable v0 ->  
        let name = (Maybes.fromMaybe v0 (Maps.lookup v0 typeVarSubst))
        in (Eithers.bind (encodeType_resolveIfTypedef aliases boundVars inScopeTypeParams name cx g) (\resolved -> Maybes.cases resolved (Right (Logic.ifElse (Logic.or (Sets.member name boundVars) (Sets.member name inScopeTypeParams)) (Syntax.TypeReference (Utils.javaTypeVariable (Core.unName name))) (Logic.ifElse (isLambdaBoundVariable name) (Syntax.TypeReference (Utils.javaTypeVariable (Core.unName name))) (Logic.ifElse (isUnresolvedInferenceVar name) (Syntax.TypeReference (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass (Utils.javaClassType [] Names.javaLangPackageName "Object")))) (Syntax.TypeReference (Utils.nameToJavaReferenceType aliases True [] name Nothing)))))) (\resolvedType -> encodeType aliases boundVars resolvedType cx g)))
      Core.TypeWrap v0 -> (Right (Syntax.TypeReference (Utils.nameToJavaReferenceType aliases True [] (Core.wrappedTypeTypeName v0) Nothing)))
      _ -> (Left (Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "can't encode unsupported type in Java: " (Core___.type_ t)))),
        Context.inContextContext = cx}))) (Rewriting.deannotateType t))

encodeType_resolveIfTypedef :: (t0 -> S.Set Core.Name -> S.Set Core.Name -> Core.Name -> t1 -> Graph.Graph -> Either t2 (Maybe Core.Type))
encodeType_resolveIfTypedef aliases boundVars inScopeTypeParams name cx g = (Logic.ifElse (Logic.or (Sets.member name boundVars) (Sets.member name inScopeTypeParams)) (Right Nothing) (Logic.ifElse (isLambdaBoundVariable name) (Right Nothing) ( 
  let schemaTypes = (Graph.graphSchemaTypes g)
  in (Maybes.cases (Maps.lookup name schemaTypes) (Right Nothing) (\ts -> Logic.ifElse (Logic.not (Lists.null (Core.typeSchemeVariables ts))) (Right Nothing) ((\x -> case x of
    Core.TypeRecord _ -> (Right Nothing)
    Core.TypeUnion _ -> (Right Nothing)
    Core.TypeWrap _ -> (Right Nothing)
    _ -> (Right (Just (Core.typeSchemeType ts)))) (Rewriting.deannotateType (Core.typeSchemeType ts))))))))

javaTypeArgumentsForNamedType :: (Core.Name -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) [Syntax.TypeArgument])
javaTypeArgumentsForNamedType tname cx g = (Eithers.bind (Schemas.requireType cx g tname) (\typ -> Right (Lists.map (\tp_ -> Utils.typeParameterToTypeArgument tp_) (javaTypeParametersForType typ))))

encodeLiteral :: (Core.Literal -> Syntax.Expression)
encodeLiteral lit = ((\x -> case x of
  Core.LiteralBinary v0 ->  
    let byteValues = (Literals.binaryToBytes v0)
    in (Utils.javaArrayCreation Utils.javaBytePrimitiveType (Just (Utils.javaArrayInitializer (Lists.map (\w -> Utils.javaLiteralToJavaExpression (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.int32ToBigint w)))) byteValues))))
  Core.LiteralBoolean v0 -> (encodeLiteral_litExp (Utils.javaBoolean v0))
  Core.LiteralFloat v0 -> (encodeLiteral_encodeFloat v0)
  Core.LiteralInteger v0 -> (encodeLiteral_encodeInteger v0)
  Core.LiteralString v0 -> (encodeLiteral_litExp (Utils.javaString v0))) lit)

encodeLiteral_litExp :: (Syntax.Literal -> Syntax.Expression)
encodeLiteral_litExp l = (Utils.javaLiteralToJavaExpression l)

encodeLiteral_primCast :: (Syntax.PrimitiveType -> Syntax.Expression -> Syntax.Expression)
encodeLiteral_primCast pt expr = (Utils.javaCastExpressionToJavaExpression (Utils.javaCastPrimitive pt (Utils.javaExpressionToJavaUnaryExpression expr)))

encodeLiteral_encodeFloat :: (Core.FloatValue -> Syntax.Expression)
encodeLiteral_encodeFloat f = ((\x -> case x of
  Core.FloatValueBigfloat v0 -> (Utils.javaConstructorCall (Utils.javaConstructorName (Syntax.Identifier "java.math.BigDecimal") Nothing) [
    encodeLiteral (Core.LiteralString (Literals.showBigfloat v0))] Nothing)
  Core.FloatValueFloat32 v0 -> (encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeFloatingPoint Syntax.FloatingPointTypeFloat)) (encodeLiteral_litExp (Syntax.LiteralFloatingPoint (Syntax.FloatingPointLiteral (Literals.float32ToBigfloat v0)))))
  Core.FloatValueFloat64 v0 -> (encodeLiteral_litExp (Syntax.LiteralFloatingPoint (Syntax.FloatingPointLiteral (Literals.float64ToBigfloat v0))))) f)

encodeLiteral_encodeInteger :: (Core.IntegerValue -> Syntax.Expression)
encodeLiteral_encodeInteger i = ((\x -> case x of
  Core.IntegerValueBigint v0 -> (Utils.javaConstructorCall (Utils.javaConstructorName (Syntax.Identifier "java.math.BigInteger") Nothing) [
    encodeLiteral (Core.LiteralString (Literals.showBigint v0))] Nothing)
  Core.IntegerValueInt8 v0 -> (encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeByte)) (encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.int8ToBigint v0)))))
  Core.IntegerValueInt16 v0 -> (encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeShort)) (encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.int16ToBigint v0)))))
  Core.IntegerValueInt32 v0 -> (encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.int32ToBigint v0))))
  Core.IntegerValueInt64 v0 -> (encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeLong)) (encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.int64ToBigint v0)))))
  Core.IntegerValueUint8 v0 -> (encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeShort)) (encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.uint8ToBigint v0)))))
  Core.IntegerValueUint16 v0 -> (encodeLiteral_litExp (Syntax.LiteralCharacter v0))
  Core.IntegerValueUint32 v0 -> (encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeLong)) (encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.uint32ToBigint v0)))))
  Core.IntegerValueUint64 v0 -> (Utils.javaConstructorCall (Utils.javaConstructorName (Syntax.Identifier "java.math.BigInteger") Nothing) [
    encodeLiteral (Core.LiteralString (Literals.showBigint (Literals.uint64ToBigint v0)))] Nothing)) i)

fieldTypeToFormalParam :: (Helpers.Aliases -> Core.FieldType -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.FormalParameter)
fieldTypeToFormalParam aliases ft cx g = (Eithers.bind (encodeType aliases Sets.empty (Core.fieldTypeType ft) cx g) (\jt -> Right (Utils.javaTypeToJavaFormalParameter jt (Core.fieldTypeName ft))))

applyCastIfSafe :: (Helpers.Aliases -> Core.Type -> Syntax.Expression -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.Expression)
applyCastIfSafe aliases castType expr cx g =  
  let trusted = (Helpers.aliasesTrustedTypeVars aliases)
  in  
    let inScope = (Helpers.aliasesInScopeTypeParams aliases)
    in  
      let castVars = (collectTypeVars castType)
      in  
        let javaTypeVars = (Sets.fromList (Lists.filter (\v -> Logic.or (Sets.member v inScope) (isLambdaBoundVariable v)) (Sets.toList castVars)))
        in  
          let isSafe = (Logic.or (Sets.null trusted) (Logic.or (Sets.null javaTypeVars) (Sets.null (Sets.difference javaTypeVars trusted))))
          in (Logic.ifElse isSafe (Eithers.bind (encodeType aliases Sets.empty castType cx g) (\jtype -> Eithers.bind (Utils.javaTypeToJavaReferenceType jtype cx) (\rt -> Right (Utils.javaCastExpressionToJavaExpression (Utils.javaCastExpression rt (Utils.javaExpressionToJavaUnaryExpression expr)))))) (Right expr))

encodeVariable :: (Helpers.JavaEnvironment -> Core.Name -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.Expression)
encodeVariable env name cx g =  
  let aliases = (Helpers.javaEnvironmentAliases env)
  in  
    let resolvedName = (Utils.lookupJavaVarName aliases name)
    in  
      let jid = (Utils.javaIdentifier (Core.unName resolvedName))
      in (Logic.ifElse (Sets.member name (Helpers.aliasesBranchVars aliases)) (Right (Utils.javaFieldAccessToJavaExpression (Syntax.FieldAccess {
        Syntax.fieldAccessQualifier = (Syntax.FieldAccess_QualifierPrimary (Utils.javaExpressionToJavaPrimary (Utils.javaIdentifierToJavaExpression jid))),
        Syntax.fieldAccessIdentifier = (Utils.javaIdentifier Names.valueFieldName)}))) (Logic.ifElse (Logic.and (Equality.equal name (Core.Name (Strings.cat [
        Names.instanceName,
        "_",
        Names.valueFieldName]))) (isRecursiveVariable aliases name)) ( 
        let instanceExpr = (Utils.javaIdentifierToJavaExpression (Utils.javaIdentifier Names.instanceName))
        in (Right (Utils.javaFieldAccessToJavaExpression (Syntax.FieldAccess {
          Syntax.fieldAccessQualifier = (Syntax.FieldAccess_QualifierPrimary (Utils.javaExpressionToJavaPrimary instanceExpr)),
          Syntax.fieldAccessIdentifier = (Utils.javaIdentifier Names.valueFieldName)})))) (Logic.ifElse (Logic.and (isRecursiveVariable aliases name) (Logic.not (isLambdaBoundIn name (Helpers.aliasesLambdaVars aliases)))) (Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocation (Just (Left (Syntax.ExpressionName {
        Syntax.expressionNameQualifier = Nothing,
        Syntax.expressionNameIdentifier = jid}))) (Syntax.Identifier Names.getMethodName) []))) (Logic.ifElse (Logic.and (Sets.member name (Helpers.aliasesThunkedVars aliases)) (Logic.not (isLambdaBoundIn name (Helpers.aliasesLambdaVars aliases)))) (Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocation (Just (Left (Syntax.ExpressionName {
        Syntax.expressionNameQualifier = Nothing,
        Syntax.expressionNameIdentifier = jid}))) (Syntax.Identifier Names.getMethodName) []))) (Logic.ifElse (isLambdaBoundIn name (Helpers.aliasesLambdaVars aliases)) ( 
        let actualName = (findMatchingLambdaVar name (Helpers.aliasesLambdaVars aliases))
        in  
          let resolvedActual = (Utils.lookupJavaVarName aliases actualName)
          in (Right (Utils.javaIdentifierToJavaExpression (Utils.variableToJavaIdentifier resolvedActual)))) (Logic.ifElse (Sets.member name (Helpers.aliasesInScopeJavaVars aliases)) (Right (Utils.javaIdentifierToJavaExpression (elementJavaIdentifier False False aliases resolvedName))) (Eithers.bind (classifyDataReference name cx g) (\cls -> (\x -> case x of
        Helpers.JavaSymbolClassHoistedLambda v0 -> (encodeVariable_hoistedLambdaCase aliases name v0 cx g)
        Helpers.JavaSymbolClassLocalVariable -> (Right (Utils.javaIdentifierToJavaExpression (elementJavaIdentifier False False aliases resolvedName)))
        Helpers.JavaSymbolClassConstant -> (Right (Utils.javaIdentifierToJavaExpression (elementJavaIdentifier False False aliases name)))
        Helpers.JavaSymbolClassNullaryFunction -> (Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocation Nothing (elementJavaIdentifier False False aliases name) [])))
        Helpers.JavaSymbolClassUnaryFunction -> (Right (Utils.javaIdentifierToJavaExpression (elementJavaIdentifier False True aliases name)))) cls))))))))

encodeVariable_buildCurried :: ([Core.Name] -> Syntax.Expression -> Syntax.Expression)
encodeVariable_buildCurried params inner = (Logic.ifElse (Lists.null params) inner (Utils.javaLambda (Lists.head params) (encodeVariable_buildCurried (Lists.tail params) inner)))

encodeVariable_hoistedLambdaCase :: (Helpers.Aliases -> Core.Name -> Int -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.Expression)
encodeVariable_hoistedLambdaCase aliases name arity cx g =  
  let paramNames = (Lists.map (\i -> Core.Name (Strings.cat2 "p" (Literals.showInt32 i))) (Math.range 0 (Math.sub arity 1)))
  in  
    let paramExprs = (Lists.map (\pn -> Utils.javaIdentifierToJavaExpression (Utils.variableToJavaIdentifier pn)) paramNames)
    in  
      let call = (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocation Nothing (elementJavaIdentifier False False aliases name) paramExprs))
      in  
        let lam = (encodeVariable_buildCurried paramNames call)
        in (Eithers.bind (Right (Lexical.dereferenceElement g name)) (\mel -> Maybes.cases mel (Right lam) (\el -> Maybes.cases (Core.bindingType el) (Right lam) (\ts ->  
          let typ = (Core.typeSchemeType ts)
          in (Eithers.bind (encodeType aliases Sets.empty typ cx g) (\jtype -> Eithers.bind (Utils.javaTypeToJavaReferenceType jtype cx) (\rt -> Right (Utils.javaCastExpressionToJavaExpression (Utils.javaCastExpression rt (Utils.javaExpressionToJavaUnaryExpression lam))))))))))

encodeNullaryConstant :: (Helpers.JavaEnvironment -> Core.Type -> Core.Function -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.Expression)
encodeNullaryConstant env typ fun cx g =  
  let aliases = (Helpers.javaEnvironmentAliases env)
  in ((\x -> case x of
    Core.FunctionPrimitive v0 -> (Eithers.bind (encodeNullaryConstant_typeArgsFromReturnType aliases typ cx g) (\targs -> Logic.ifElse (Lists.null targs) ( 
      let header = (Syntax.MethodInvocation_HeaderSimple (Syntax.MethodName (elementJavaIdentifier True False aliases v0)))
      in (Right (Utils.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
        Syntax.methodInvocationHeader = header,
        Syntax.methodInvocationArguments = []})))) ( 
      let fullName = (Syntax.unIdentifier (elementJavaIdentifier True False aliases v0))
      in  
        let parts = (Strings.splitOn "." fullName)
        in  
          let className = (Syntax.Identifier (Strings.intercalate "." (Lists.init parts)))
          in  
            let methodName = (Syntax.Identifier (Lists.last parts))
            in (Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs className methodName targs []))))))
    _ -> (Left (Context.InContext {
      Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "unexpected " (Strings.cat2 "nullary function" (Strings.cat2 " in " (Core___.function fun)))))),
      Context.inContextContext = cx}))) fun)

encodeNullaryConstant_typeArgsFromReturnType :: (Helpers.Aliases -> Core.Type -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) [Syntax.TypeArgument])
encodeNullaryConstant_typeArgsFromReturnType aliases t cx g = ((\x -> case x of
  Core.TypeSet v0 -> (Eithers.bind (encodeType aliases Sets.empty v0 cx g) (\jst -> Eithers.bind (Utils.javaTypeToJavaReferenceType jst cx) (\rt -> Right [
    Syntax.TypeArgumentReference rt])))
  Core.TypeList v0 -> (Eithers.bind (encodeType aliases Sets.empty v0 cx g) (\jlt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jlt cx) (\rt -> Right [
    Syntax.TypeArgumentReference rt])))
  Core.TypeMaybe v0 -> (Eithers.bind (encodeType aliases Sets.empty v0 cx g) (\jmt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jmt cx) (\rt -> Right [
    Syntax.TypeArgumentReference rt])))
  Core.TypeMap v0 -> (Eithers.bind (encodeType aliases Sets.empty (Core.mapTypeKeys v0) cx g) (\jkt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jkt cx) (\rk -> Eithers.bind (encodeType aliases Sets.empty (Core.mapTypeValues v0) cx g) (\jvt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jvt cx) (\rv -> Right [
    Syntax.TypeArgumentReference rk,
    (Syntax.TypeArgumentReference rv)])))))
  _ -> (Right [])) (Rewriting.deannotateType t))

buildTypeVarSubst :: (S.Set Core.Name -> Core.Type -> Core.Type -> M.Map Core.Name Core.Name)
buildTypeVarSubst schemeVarSet freshTyp canonTyp = (buildTypeVarSubst_go schemeVarSet (Rewriting.deannotateType freshTyp) (Rewriting.deannotateType canonTyp))

buildTypeVarSubst_go :: (S.Set Core.Name -> Core.Type -> Core.Type -> M.Map Core.Name Core.Name)
buildTypeVarSubst_go svs ft ct =  
  let goSub = (\a -> \b -> buildTypeVarSubst_go svs (Rewriting.deannotateType a) (Rewriting.deannotateType b))
  in ((\x -> case x of
    Core.TypeVariable v0 -> ((\x -> case x of
      Core.TypeVariable v1 -> (Logic.ifElse (Logic.and (Logic.not (Equality.equal v0 v1)) (Sets.member v1 svs)) (Maps.singleton v0 v1) Maps.empty)
      _ -> Maps.empty) ct)
    Core.TypeFunction v0 -> ((\x -> case x of
      Core.TypeFunction v1 -> (Maps.union (goSub (Core.functionTypeDomain v0) (Core.functionTypeDomain v1)) (goSub (Core.functionTypeCodomain v0) (Core.functionTypeCodomain v1)))
      _ -> Maps.empty) ct)
    Core.TypeApplication v0 -> ((\x -> case x of
      Core.TypeApplication v1 -> (Maps.union (goSub (Core.applicationTypeFunction v0) (Core.applicationTypeFunction v1)) (goSub (Core.applicationTypeArgument v0) (Core.applicationTypeArgument v1)))
      _ -> Maps.empty) ct)
    Core.TypeList v0 -> ((\x -> case x of
      Core.TypeList v1 -> (goSub v0 v1)
      _ -> Maps.empty) ct)
    Core.TypeSet v0 -> ((\x -> case x of
      Core.TypeSet v1 -> (goSub v0 v1)
      _ -> Maps.empty) ct)
    Core.TypeMaybe v0 -> ((\x -> case x of
      Core.TypeMaybe v1 -> (goSub v0 v1)
      _ -> Maps.empty) ct)
    Core.TypeMap v0 -> ((\x -> case x of
      Core.TypeMap v1 -> (Maps.union (goSub (Core.mapTypeKeys v0) (Core.mapTypeKeys v1)) (goSub (Core.mapTypeValues v0) (Core.mapTypeValues v1)))
      _ -> Maps.empty) ct)
    Core.TypePair v0 -> ((\x -> case x of
      Core.TypePair v1 -> (Maps.union (goSub (Core.pairTypeFirst v0) (Core.pairTypeFirst v1)) (goSub (Core.pairTypeSecond v0) (Core.pairTypeSecond v1)))
      _ -> Maps.empty) ct)
    Core.TypeEither v0 -> ((\x -> case x of
      Core.TypeEither v1 -> (Maps.union (goSub (Core.eitherTypeLeft v0) (Core.eitherTypeLeft v1)) (goSub (Core.eitherTypeRight v0) (Core.eitherTypeRight v1)))
      _ -> Maps.empty) ct)
    Core.TypeForall v0 -> ((\x -> case x of
      Core.TypeForall v1 -> (goSub (Core.forallTypeBody v0) (Core.forallTypeBody v1))
      _ -> (buildTypeVarSubst_go svs (Rewriting.deannotateType (Core.forallTypeBody v0)) ct)) ct)
    _ -> ((\x -> case x of
      Core.TypeForall v0 -> (buildTypeVarSubst_go svs ft (Rewriting.deannotateType (Core.forallTypeBody v0)))
      _ -> Maps.empty) ct)) ft)

buildTypeSubst :: (S.Set Core.Name -> Core.Type -> Core.Type -> M.Map Core.Name Core.Type)
buildTypeSubst schemeVarSet schemeType actualType = (buildTypeSubst_go schemeVarSet (Rewriting.deannotateType schemeType) (Rewriting.deannotateType actualType))

buildTypeSubst_go :: (S.Set Core.Name -> Core.Type -> Core.Type -> M.Map Core.Name Core.Type)
buildTypeSubst_go svs st at =  
  let goSub = (\a -> \b -> buildTypeSubst_go svs (Rewriting.deannotateType a) (Rewriting.deannotateType b))
  in ((\x -> case x of
    Core.TypeVariable v0 -> (Logic.ifElse (Sets.member v0 svs) (Maps.singleton v0 at) Maps.empty)
    Core.TypeFunction v0 -> ((\x -> case x of
      Core.TypeFunction v1 -> (Maps.union (goSub (Core.functionTypeDomain v0) (Core.functionTypeDomain v1)) (goSub (Core.functionTypeCodomain v0) (Core.functionTypeCodomain v1)))
      _ -> Maps.empty) at)
    Core.TypeApplication v0 -> ((\x -> case x of
      Core.TypeApplication v1 -> (Maps.union (goSub (Core.applicationTypeFunction v0) (Core.applicationTypeFunction v1)) (goSub (Core.applicationTypeArgument v0) (Core.applicationTypeArgument v1)))
      _ -> Maps.empty) at)
    Core.TypeList v0 -> ((\x -> case x of
      Core.TypeList v1 -> (goSub v0 v1)
      _ -> Maps.empty) at)
    Core.TypeSet v0 -> ((\x -> case x of
      Core.TypeSet v1 -> (goSub v0 v1)
      _ -> Maps.empty) at)
    Core.TypeMaybe v0 -> ((\x -> case x of
      Core.TypeMaybe v1 -> (goSub v0 v1)
      _ -> Maps.empty) at)
    Core.TypeMap v0 -> ((\x -> case x of
      Core.TypeMap v1 -> (Maps.union (goSub (Core.mapTypeKeys v0) (Core.mapTypeKeys v1)) (goSub (Core.mapTypeValues v0) (Core.mapTypeValues v1)))
      _ -> Maps.empty) at)
    Core.TypePair v0 -> ((\x -> case x of
      Core.TypePair v1 -> (Maps.union (goSub (Core.pairTypeFirst v0) (Core.pairTypeFirst v1)) (goSub (Core.pairTypeSecond v0) (Core.pairTypeSecond v1)))
      _ -> Maps.empty) at)
    Core.TypeEither v0 -> ((\x -> case x of
      Core.TypeEither v1 -> (Maps.union (goSub (Core.eitherTypeLeft v0) (Core.eitherTypeLeft v1)) (goSub (Core.eitherTypeRight v0) (Core.eitherTypeRight v1)))
      _ -> Maps.empty) at)
    Core.TypeForall v0 -> ((\x -> case x of
      Core.TypeForall v1 -> (goSub (Core.forallTypeBody v0) (Core.forallTypeBody v1))
      _ -> (goSub (Core.forallTypeBody v0) at)) at)
    _ -> Maps.empty) st)

javaEnvGetGraph :: (Helpers.JavaEnvironment -> Graph.Graph)
javaEnvGetGraph env = (Helpers.javaEnvironmentGraph env)

javaEnvSetGraph :: (Graph.Graph -> Helpers.JavaEnvironment -> Helpers.JavaEnvironment)
javaEnvSetGraph g env = Helpers.JavaEnvironment {
  Helpers.javaEnvironmentAliases = (Helpers.javaEnvironmentAliases env),
  Helpers.javaEnvironmentGraph = g}

analyzeJavaFunction :: (Helpers.JavaEnvironment -> Core.Term -> Context.Context -> t0 -> Either t1 (Typing.FunctionStructure Helpers.JavaEnvironment))
analyzeJavaFunction env term cx g = (CoderUtils.analyzeFunctionTerm cx javaEnvGetGraph javaEnvSetGraph env term)

withLambda :: (Helpers.JavaEnvironment -> Core.Lambda -> (Helpers.JavaEnvironment -> t0) -> t0)
withLambda env lam k = (Schemas.withLambdaContext javaEnvGetGraph javaEnvSetGraph env lam (\env1 ->  
  let aliases = (Helpers.javaEnvironmentAliases env1)
  in  
    let aliases2 = Helpers.Aliases {
            Helpers.aliasesCurrentNamespace = (Helpers.aliasesCurrentNamespace aliases),
            Helpers.aliasesPackages = (Helpers.aliasesPackages aliases),
            Helpers.aliasesBranchVars = (Helpers.aliasesBranchVars aliases),
            Helpers.aliasesRecursiveVars = (Helpers.aliasesRecursiveVars aliases),
            Helpers.aliasesInScopeTypeParams = (Helpers.aliasesInScopeTypeParams aliases),
            Helpers.aliasesPolymorphicLocals = (Helpers.aliasesPolymorphicLocals aliases),
            Helpers.aliasesInScopeJavaVars = (Helpers.aliasesInScopeJavaVars aliases),
            Helpers.aliasesVarRenames = (Helpers.aliasesVarRenames aliases),
            Helpers.aliasesLambdaVars = (Sets.insert (Core.lambdaParameter lam) (Helpers.aliasesLambdaVars aliases)),
            Helpers.aliasesTypeVarSubst = (Helpers.aliasesTypeVarSubst aliases),
            Helpers.aliasesTrustedTypeVars = (Helpers.aliasesTrustedTypeVars aliases),
            Helpers.aliasesMethodCodomain = (Helpers.aliasesMethodCodomain aliases),
            Helpers.aliasesThunkedVars = (Helpers.aliasesThunkedVars aliases)}
    in  
      let env2 = Helpers.JavaEnvironment {
              Helpers.javaEnvironmentAliases = aliases2,
              Helpers.javaEnvironmentGraph = (Helpers.javaEnvironmentGraph env1)}
      in (k env2)))

withTypeLambda :: (Helpers.JavaEnvironment -> Core.TypeLambda -> (Helpers.JavaEnvironment -> t0) -> t0)
withTypeLambda = (Schemas.withTypeLambdaContext javaEnvGetGraph javaEnvSetGraph)

propagateType :: (Core.Type -> Core.Term -> Core.Term)
propagateType typ term =  
  let setTypeAnn = (\t -> Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ typ)) t)
  in ((\x -> case x of
    Core.TermFunction v0 -> ((\x -> case x of
      Core.FunctionLambda _ ->  
        let annotated = (setTypeAnn term)
        in ((\x -> case x of
          Core.TypeFunction v2 -> (propagateType_propagateIntoLambda (Core.functionTypeCodomain v2) annotated)
          _ -> annotated) (Rewriting.deannotateType typ))
      _ -> (setTypeAnn term)) v0)
    Core.TermLet v0 -> (setTypeAnn (propagateType_rebuildLet term (Core.letBindings v0) (propagateType typ (Core.letBody v0))))
    _ -> (setTypeAnn term)) (Rewriting.deannotateTerm term))

propagateType_propagateIntoLambda :: (Core.Type -> Core.Term -> Core.Term)
propagateType_propagateIntoLambda cod t = ((\x -> case x of
  Core.TermAnnotated v0 -> (Core.TermAnnotated (Core.AnnotatedTerm {
    Core.annotatedTermBody = (propagateType_propagateIntoLambda cod (Core.annotatedTermBody v0)),
    Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)}))
  Core.TermFunction v0 -> ((\x -> case x of
    Core.FunctionLambda v1 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.lambdaParameter v1),
      Core.lambdaDomain = (Core.lambdaDomain v1),
      Core.lambdaBody = (propagateType cod (Core.lambdaBody v1))})))
    _ -> t) v0)
  _ -> t) t)

propagateType_rebuildLet :: (Core.Term -> [Core.Binding] -> Core.Term -> Core.Term)
propagateType_rebuildLet t bindings newBody = ((\x -> case x of
  Core.TermAnnotated v0 -> (Core.TermAnnotated (Core.AnnotatedTerm {
    Core.annotatedTermBody = (propagateType_rebuildLet (Core.annotatedTermBody v0) bindings newBody),
    Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)}))
  Core.TermLet _ -> (Core.TermLet (Core.Let {
    Core.letBindings = bindings,
    Core.letBody = newBody}))
  _ -> t) t)

flattenBindings :: ([Core.Binding] -> [Core.Binding])
flattenBindings bindings = (Lists.bind bindings (\b -> (\x -> case x of
  Core.TermLet v0 -> (Lists.concat2 (flattenBindings (Core.letBindings v0)) [
    Core.Binding {
      Core.bindingName = (Core.bindingName b),
      Core.bindingTerm = (Core.letBody v0),
      Core.bindingType = (Core.bindingType b)}])
  _ -> [
    b]) (Rewriting.deannotateTerm (Core.bindingTerm b))))

dedupBindings :: (S.Set Core.Name -> [Core.Binding] -> [Core.Binding])
dedupBindings inScope bs = (Logic.ifElse (Lists.null bs) [] ( 
  let b = (Lists.head bs)
  in  
    let rest = (Lists.tail bs)
    in  
      let name = (Core.bindingName b)
      in (Logic.ifElse (Sets.member name inScope) ( 
        let newName = (freshJavaName name inScope)
        in  
          let subst = (Maps.singleton name newName)
          in  
            let rest2 = (Lists.map (\b2 -> Core.Binding {
                    Core.bindingName = (Core.bindingName b2),
                    Core.bindingTerm = (Rewriting.substituteVariables subst (Core.bindingTerm b2)),
                    Core.bindingType = (Core.bindingType b2)}) rest)
            in (Lists.cons (Core.Binding {
              Core.bindingName = newName,
              Core.bindingTerm = (Core.bindingTerm b),
              Core.bindingType = (Core.bindingType b)}) (dedupBindings (Sets.insert newName inScope) rest2))) (Lists.cons b (dedupBindings (Sets.insert name inScope) rest)))))

freshJavaName :: (Core.Name -> S.Set Core.Name -> Core.Name)
freshJavaName base avoid = (freshJavaName_go base avoid 2)

freshJavaName_go :: (Core.Name -> S.Set Core.Name -> Int -> Core.Name)
freshJavaName_go base avoid i =  
  let candidate = (Core.Name (Strings.cat2 (Core.unName base) (Literals.showInt32 i)))
  in (Logic.ifElse (Sets.member candidate avoid) (freshJavaName_go base avoid (Math.add i 1)) candidate)

needsThunking :: (Core.Term -> Bool)
needsThunking t = ((\x -> case x of
  Core.TermLet _ -> True
  Core.TermTypeApplication _ -> True
  Core.TermTypeLambda _ -> True
  _ -> (Lists.foldl (\b -> \st -> Logic.or b (needsThunking st)) False (Rewriting.subterms t))) (Rewriting.deannotateTerm t))

bindingIsFunctionType :: (Core.Binding -> Bool)
bindingIsFunctionType b = (Maybes.maybe ((\x -> case x of
  Core.TermFunction _ -> True
  _ -> False) (Rewriting.deannotateTerm (Core.bindingTerm b))) (\ts -> (\x -> case x of
  Core.TypeFunction _ -> True
  Core.TypeForall v0 -> ((\x -> case x of
    Core.TypeFunction _ -> True
    _ -> False) (Rewriting.deannotateType (Core.forallTypeBody v0)))
  _ -> False) (Rewriting.deannotateType (Core.typeSchemeType ts))) (Core.bindingType b))

decodeTypeFromTerm :: (Core.Term -> Maybe Core.Type)
decodeTypeFromTerm term = ((\x -> case x of
  Core.TermUnion v0 -> (Logic.ifElse (Equality.equal (Core.injectionTypeName v0) (Core.Name "hydra.core.Type")) ( 
    let fname = (Core.unName (Core.fieldName (Core.injectionField v0)))
    in  
      let fterm = (Core.fieldTerm (Core.injectionField v0))
      in (Logic.ifElse (Equality.equal fname "variable") ((\x -> case x of
        Core.TermWrap v1 -> ((\x -> case x of
          Core.TermLiteral v2 -> ((\x -> case x of
            Core.LiteralString v3 -> (Just (Core.TypeVariable (Core.Name v3)))
            _ -> Nothing) v2)
          _ -> Nothing) (Core.wrappedTermBody v1))
        _ -> Nothing) fterm) (Logic.ifElse (Equality.equal fname "annotated") ((\x -> case x of
        Core.TermRecord v1 -> (Maybes.bind (Lists.safeHead (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.Name "body")) (Core.recordFields v1))) (\bodyField -> decodeTypeFromTerm (Core.fieldTerm bodyField)))
        _ -> Nothing) fterm) (Logic.ifElse (Equality.equal fname "application") ((\x -> case x of
        Core.TermRecord v1 -> (Maybes.bind (Lists.safeHead (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.Name "function")) (Core.recordFields v1))) (\funcField -> Maybes.bind (decodeTypeFromTerm (Core.fieldTerm funcField)) (\func -> Maybes.bind (Lists.safeHead (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.Name "argument")) (Core.recordFields v1))) (\argField -> Maybes.map (\arg -> Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = func,
          Core.applicationTypeArgument = arg})) (decodeTypeFromTerm (Core.fieldTerm argField))))))
        _ -> Nothing) fterm) (Logic.ifElse (Equality.equal fname "function") ((\x -> case x of
        Core.TermRecord v1 -> (Maybes.bind (Lists.safeHead (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.Name "domain")) (Core.recordFields v1))) (\domField -> Maybes.bind (decodeTypeFromTerm (Core.fieldTerm domField)) (\dom -> Maybes.bind (Lists.safeHead (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.Name "codomain")) (Core.recordFields v1))) (\codField -> Maybes.map (\cod -> Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = dom,
          Core.functionTypeCodomain = cod})) (decodeTypeFromTerm (Core.fieldTerm codField))))))
        _ -> Nothing) fterm) (Logic.ifElse (Equality.equal fname "literal") ((\x -> case x of
        Core.TermUnion v1 -> (Logic.ifElse (Equality.equal (Core.unName (Core.fieldName (Core.injectionField v1))) "string") (Just (Core.TypeLiteral Core.LiteralTypeString)) Nothing)
        _ -> Nothing) fterm) Nothing)))))) Nothing)
  _ -> Nothing) (Rewriting.deannotateTerm term))

tryInferFunctionType :: (Core.Function -> Maybe Core.Type)
tryInferFunctionType fun = ((\x -> case x of
  Core.FunctionLambda v0 -> (Maybes.bind (Core.lambdaDomain v0) (\dom ->  
    let mCod = ((\x -> case x of
            Core.TermAnnotated v1 -> (Maybes.bind (Maps.lookup Constants.key_type (Core.annotatedTermAnnotation v1)) (\typeTerm -> decodeTypeFromTerm typeTerm))
            Core.TermFunction v1 -> (tryInferFunctionType v1)
            _ -> Nothing) (Core.lambdaBody v0))
    in (Maybes.map (\cod -> Core.TypeFunction (Core.FunctionType {
      Core.functionTypeDomain = dom,
      Core.functionTypeCodomain = cod})) mCod)))
  _ -> Nothing) fun)

collectTypeApps :: (Core.Term -> [Core.Type] -> (Core.Term, [Core.Type]))
collectTypeApps t acc = ((\x -> case x of
  Core.TermTypeApplication v0 -> (collectTypeApps (Core.typeApplicationTermBody v0) (Lists.cons (Core.typeApplicationTermType v0) acc))
  _ -> (Rewriting.deannotateTerm t, acc)) (Rewriting.deannotateTerm t))

collectTypeApps0 :: (Core.Term -> [Core.Type] -> (Core.Term, [Core.Type]))
collectTypeApps0 t acc = ((\x -> case x of
  Core.TermTypeApplication v0 -> (collectTypeApps0 (Core.typeApplicationTermBody v0) (Lists.cons (Core.typeApplicationTermType v0) acc))
  _ -> (t, acc)) (Rewriting.deannotateTerm t))

countFunctionParams :: (Core.Type -> Int)
countFunctionParams t = ((\x -> case x of
  Core.TypeFunction v0 -> (Math.add 1 (countFunctionParams (Core.functionTypeCodomain v0)))
  _ -> 0) (Rewriting.deannotateType t))

peelDomainTypes :: (Int -> Core.Type -> ([Core.Type], Core.Type))
peelDomainTypes n t = (Logic.ifElse (Equality.lte n 0) ([], t) ((\x -> case x of
  Core.TypeFunction v0 ->  
    let rest = (peelDomainTypes (Math.sub n 1) (Core.functionTypeCodomain v0))
    in (Lists.cons (Core.functionTypeDomain v0) (Pairs.first rest), (Pairs.second rest))
  _ -> ([], t)) (Rewriting.deannotateType t)))

unwrapReturnType :: (Core.Type -> Core.Type)
unwrapReturnType t = ((\x -> case x of
  Core.TypeFunction v0 -> (unwrapReturnType (Core.functionTypeCodomain v0))
  Core.TypeApplication v0 -> (unwrapReturnType (Core.applicationTypeArgument v0))
  _ -> t) (Rewriting.deannotateType t))

findPairFirst :: (Core.Type -> Maybe Core.Name)
findPairFirst t = ((\x -> case x of
  Core.TypePair v0 -> ((\x -> case x of
    Core.TypeVariable v1 -> (Just v1)
    _ -> Nothing) (Rewriting.deannotateType (Core.pairTypeFirst v0)))
  _ -> Nothing) (Rewriting.deannotateType t))

extractInOutPair :: (Core.Type -> [(Core.Name, Core.Name)])
extractInOutPair t = ((\x -> case x of
  Core.TypeFunction v0 -> ((\x -> case x of
    Core.TypeVariable v1 ->  
      let retType = (unwrapReturnType (Core.functionTypeCodomain v0))
      in ((\x -> case x of
        Core.TypePair v2 -> ((\x -> case x of
          Core.TypeVariable v3 -> [
            (v1, v3)]
          _ -> []) (Rewriting.deannotateType (Core.pairTypeFirst v2)))
        _ -> []) (Rewriting.deannotateType retType))
    _ -> []) (Rewriting.deannotateType (Core.functionTypeDomain v0)))
  _ -> []) (Rewriting.deannotateType t))

extractDirectReturn :: (S.Set Core.Name -> Core.Type -> [(Core.Name, Core.Name)])
extractDirectReturn tparamSet t = (extractDirectReturn_go tparamSet t)

extractDirectReturn_go :: (S.Set Core.Name -> Core.Type -> [(Core.Name, Core.Name)])
extractDirectReturn_go tparamSet t = ((\x -> case x of
  Core.TypeFunction v0 ->  
    let dom = (Rewriting.deannotateType (Core.functionTypeDomain v0))
    in  
      let cod = (Core.functionTypeCodomain v0)
      in ((\x -> case x of
        Core.TypeVariable v1 -> (Logic.ifElse (Sets.member v1 tparamSet) ((\x -> case x of
          Core.TypeFunction v2 ->  
            let midArg = (Rewriting.deannotateType (Core.functionTypeDomain v2))
            in  
              let retPart = (Rewriting.deannotateType (Core.functionTypeCodomain v2))
              in ((\x -> case x of
                Core.TypeVariable v3 -> (Logic.ifElse (Sets.member v3 tparamSet) [] ((\x -> case x of
                  Core.TypeVariable v4 -> (Logic.ifElse (Sets.member v4 tparamSet) [
                    (v1, v4)] [])
                  _ -> []) retPart))
                _ -> ((\x -> case x of
                  Core.TypeVariable v3 -> (Logic.ifElse (Sets.member v3 tparamSet) [
                    (v1, v3)] [])
                  _ -> []) retPart)) midArg)
          _ -> []) (Rewriting.deannotateType cod)) (extractDirectReturn_go tparamSet cod))
        _ -> (extractDirectReturn_go tparamSet cod)) dom)
  _ -> []) (Rewriting.deannotateType t))

nameMapToTypeMap :: Ord t0 => (M.Map t0 Core.Name -> M.Map t0 Core.Type)
nameMapToTypeMap m = (Maps.map (\v -> Core.TypeVariable v) m)

groupPairsByFirst :: Ord t0 => ([(t0, t1)] -> M.Map t0 [t1])
groupPairsByFirst pairs = (Lists.foldl (\m -> \p ->  
  let k = (Pairs.first p)
  in  
    let v = (Pairs.second p)
    in (Maps.alter (\mv -> Maybes.maybe (Just [
      v]) (\vs -> Just (Lists.concat2 vs [
      v])) mv) k m)) Maps.empty pairs)

selfRefSubstitution :: (Eq t0, Ord t0) => (M.Map t0 [t0] -> M.Map t0 t0)
selfRefSubstitution grouped = (Lists.foldl (\subst -> \entry -> selfRefSubstitution_processGroup subst (Pairs.first entry) (Pairs.second entry)) Maps.empty (Maps.toList grouped))

selfRefSubstitution_processGroup :: (Eq t0, Ord t0) => (M.Map t0 t0 -> t0 -> [t0] -> M.Map t0 t0)
selfRefSubstitution_processGroup subst inVar outVars = (Logic.ifElse (Lists.elem inVar outVars) (Lists.foldl (\s -> \v -> Logic.ifElse (Equality.equal v inVar) s (Maps.insert v inVar s)) subst outVars) subst)

directRefSubstitution :: (Eq t0, Ord t0) => (S.Set t0 -> Maybe t0 -> M.Map t0 [t0] -> M.Map t0 t0)
directRefSubstitution directInputVars codVar grouped = (Lists.foldl (\subst -> \entry -> directRefSubstitution_processGroup directInputVars codVar subst (Pairs.first entry) (Pairs.second entry)) Maps.empty (Maps.toList grouped))

directRefSubstitution_processGroup :: (Eq t0, Ord t0) => (S.Set t0 -> Maybe t0 -> M.Map t0 t0 -> t0 -> [t0] -> M.Map t0 t0)
directRefSubstitution_processGroup directInputVars codVar subst inVar outVars =  
  let selfRefCount = (Lists.length (Lists.filter (\v -> Equality.equal v inVar) outVars))
  in  
    let nonSelfVars = (Lists.filter (\v -> Logic.not (Equality.equal v inVar)) outVars)
    in  
      let safeNonSelfVars = (Lists.filter (\v -> Logic.and (Logic.not (Sets.member v directInputVars)) (Logic.not (Equality.equal (Just v) codVar))) nonSelfVars)
      in (Logic.ifElse (Logic.and (Equality.gte selfRefCount 2) (Logic.not (Lists.null safeNonSelfVars))) (Lists.foldl (\s -> \v -> Maps.insert v inVar s) subst safeNonSelfVars) subst)

findSelfRefVar :: (Eq t0, Ord t0) => (M.Map t0 [t0] -> Maybe t0)
findSelfRefVar grouped =  
  let selfRefs = (Lists.filter (\entry -> Lists.elem (Pairs.first entry) (Pairs.second entry)) (Maps.toList grouped))
  in (Logic.ifElse (Lists.null selfRefs) Nothing (Just (Pairs.first (Lists.head selfRefs))))

detectAccumulatorUnification :: ([Core.Type] -> Core.Type -> [Core.Name] -> M.Map Core.Name Core.Type)
detectAccumulatorUnification doms cod tparams =  
  let tparamSet = (Sets.fromList tparams)
  in  
    let allPairs = (Lists.bind doms (\d -> extractInOutPair d))
    in  
      let groupedByInput = (groupPairsByFirst allPairs)
      in  
        let selfRefSubst = (selfRefSubstitution groupedByInput)
        in  
          let directPairs = (Lists.bind doms (\d -> extractDirectReturn tparamSet d))
          in  
            let groupedDirect = (groupPairsByFirst directPairs)
            in  
              let directInputVars = (Sets.fromList (Lists.map (\p -> Pairs.first p) directPairs))
              in  
                let codVar = ((\x -> case x of
                        Core.TypeVariable v0 -> (Just v0)
                        _ -> Nothing) (Rewriting.deannotateType cod))
                in  
                  let directRefSubst = (directRefSubstitution directInputVars codVar groupedDirect)
                  in  
                    let codSubst = (Maybes.maybe Maps.empty (\cv -> Logic.ifElse (Maps.member cv selfRefSubst) Maps.empty (Maybes.maybe Maps.empty (\refVar -> Logic.ifElse (Equality.equal cv refVar) Maps.empty (Maps.singleton cv refVar)) (findSelfRefVar groupedByInput))) (findPairFirst cod))
                    in  
                      let domVars = (Sets.fromList (Lists.bind doms (\d -> Sets.toList (collectTypeVars d))))
                      in  
                        let danglingSubst = (Maybes.maybe Maps.empty (\cv -> Logic.ifElse (Sets.member cv domVars) Maps.empty (Maybes.maybe Maps.empty (\refVar -> Maps.singleton cv (Core.TypeVariable refVar)) (findSelfRefVar groupedByInput))) (findPairFirst cod))
                        in (Maps.union (Maps.union (Maps.union (nameMapToTypeMap selfRefSubst) (nameMapToTypeMap codSubst)) danglingSubst) (nameMapToTypeMap directRefSubst))

typesMatch :: (Core.Type -> Core.Type -> Bool)
typesMatch a b = ((\x -> case x of
  Core.TypeVariable v0 -> ((\x -> case x of
    Core.TypeVariable v1 -> (Equality.equal v0 v1)
    _ -> True) b)
  Core.TypeWrap v0 -> ((\x -> case x of
    Core.TypeWrap v1 -> (Equality.equal (Core.wrappedTypeTypeName v0) (Core.wrappedTypeTypeName v1))
    _ -> True) b)
  _ -> True) a)

isSimpleName :: (Core.Name -> Bool)
isSimpleName name = (Equality.equal (Lists.length (Strings.splitOn "." (Core.unName name))) 1)

filterPhantomTypeArgs :: (Core.Name -> [Core.Type] -> t0 -> Graph.Graph -> Either t1 [Core.Type])
filterPhantomTypeArgs calleeName allTypeArgs cx g = (Eithers.bind (Right (Lexical.dereferenceElement g calleeName)) (\mel -> Maybes.cases mel (Right allTypeArgs) (\el -> Maybes.cases (Core.bindingType el) (Right allTypeArgs) (\ts ->  
  let schemeVars = (Lists.filter (\v -> isSimpleName v) (Core.typeSchemeVariables ts))
  in  
    let schemeTypeVars = (collectTypeVars (Core.typeSchemeType ts))
    in  
      let schemeType = (Core.typeSchemeType ts)
      in  
        let nParams = (countFunctionParams schemeType)
        in  
          let peeled = (peelDomainTypes nParams schemeType)
          in  
            let calleeDoms = (Pairs.first peeled)
            in  
              let calleeCod = (Pairs.second peeled)
              in  
                let overgenSubst = (detectAccumulatorUnification calleeDoms calleeCod schemeVars)
                in  
                  let keepFlags = (Lists.map (\v -> Logic.and (Sets.member v schemeTypeVars) (Logic.not (Maps.member v overgenSubst))) schemeVars)
                  in (Logic.ifElse (Logic.not (Equality.equal (Lists.length schemeVars) (Lists.length allTypeArgs))) (Right allTypeArgs) (Right (filterPhantomTypeArgs_filterAndApply allTypeArgs keepFlags overgenSubst)))))))

filterPhantomTypeArgs_filterAndApply :: ([Core.Type] -> [Bool] -> M.Map Core.Name Core.Type -> [Core.Type])
filterPhantomTypeArgs_filterAndApply allTypeArgs keepFlags overgenSubst =  
  let filtered = (Lists.map (\p -> Pairs.first p) (Lists.filter (\p -> Pairs.second p) (Lists.zip allTypeArgs keepFlags)))
  in (Logic.ifElse (Logic.not (Maps.null overgenSubst)) (Lists.map (\t -> substituteTypeVarsWithTypes overgenSubst t) filtered) filtered)

filterByFlags :: ([t0] -> [Bool] -> [t0])
filterByFlags xs flags = (Lists.map (\p -> Pairs.first p) (Lists.filter (\p -> Pairs.second p) (Lists.zip xs flags)))

applySubstSimple :: (M.Map Core.Name Core.Type -> Core.Type -> Core.Type)
applySubstSimple subst t = ((\x -> case x of
  Core.TypeVariable v0 -> (Maps.findWithDefault t v0 subst)
  _ -> t) (Rewriting.deannotateType t))

buildArgSubst :: (S.Set Core.Name -> [Core.Type] -> [t0] -> M.Map Core.Name t0)
buildArgSubst schemeVarSet schemeDoms argTypes = (Maps.fromList (Lists.bind (Lists.zip schemeDoms argTypes) (\p ->  
  let sdom = (Pairs.first p)
  in  
    let argType = (Pairs.second p)
    in ((\x -> case x of
      Core.TypeVariable v0 -> (Logic.ifElse (Sets.member v0 schemeVarSet) [
        (v0, argType)] [])
      _ -> []) (Rewriting.deannotateType sdom)))))

resolveTypeApps :: ([Core.Name] -> [Core.Type] -> M.Map Core.Name Core.Type -> [Core.Type])
resolveTypeApps schemeVars fallbackTypeApps argSubst =  
  let resolvedVars = (Sets.fromList (Maps.keys argSubst))
  in  
    let unresolvedVars = (Lists.filter (\v -> Logic.not (Sets.member v resolvedVars)) schemeVars)
    in  
      let usedTypes = (Sets.fromList (Maps.elems argSubst))
      in  
        let unusedIrTypes = (Lists.filter (\t -> Logic.not (Sets.member t usedTypes)) fallbackTypeApps)
        in  
          let remainingSubst = (Maps.fromList (Lists.zip unresolvedVars unusedIrTypes))
          in  
            let fullSubst = (Maps.union argSubst remainingSubst)
            in (Lists.map (\v -> Maps.findWithDefault (Core.TypeVariable v) v fullSubst) schemeVars)

correctTypeAppsWithArgs :: ([Core.Name] -> [Core.Type] -> Core.Type -> [Core.Term] -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) [Core.Type])
correctTypeAppsWithArgs schemeVars fallbackTypeApps schemeType args cx g =  
  let schemeVarSet = (Sets.fromList schemeVars)
  in  
    let irSubst = (Maps.fromList (Lists.zip schemeVars fallbackTypeApps))
    in  
      let peeled = (peelDomainTypes (Lists.length args) schemeType)
      in  
        let schemeDoms = (Pairs.first peeled)
        in (Eithers.bind (Eithers.mapList (\arg -> Eithers.bimap (\_de -> Context.InContext {
          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unDecodingError _de))),
          Context.inContextContext = cx}) (\_a -> _a) (Annotations.getType g (Annotations.termAnnotationInternal arg))) args) (\mArgTypes -> Logic.ifElse (Logic.not (Lists.null (Lists.filter (\m -> Maybes.isNothing m) mArgTypes))) (Right fallbackTypeApps) ( 
          let argTypes = (Lists.bind mArgTypes (\m -> Maybes.cases m [] (\x -> Lists.pure x)))
          in  
            let irDoms = (Lists.map (\d -> applySubstSimple irSubst d) schemeDoms)
            in  
              let domsMatch = (Lists.null (Lists.filter (\p -> Logic.not (typesMatch (Rewriting.deannotateType (Pairs.first p)) (Rewriting.deannotateType (Pairs.second p)))) (Lists.zip irDoms argTypes)))
              in (Logic.ifElse domsMatch (Right fallbackTypeApps) (Right (resolveTypeApps schemeVars fallbackTypeApps (buildArgSubst schemeVarSet schemeDoms argTypes)))))))

correctTypeApps :: (t0 -> Core.Name -> [Core.Term] -> [Core.Type] -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) [Core.Type])
correctTypeApps gr name args fallbackTypeApps cx g = (Eithers.bind (Right (Lexical.dereferenceElement g name)) (\mel -> Maybes.cases mel (Right fallbackTypeApps) (\el -> Maybes.cases (Core.bindingType el) (Right fallbackTypeApps) (\ts ->  
  let schemeType = (Core.typeSchemeType ts)
  in  
    let allSchemeVars = (Lists.filter (\v -> isSimpleName v) (Core.typeSchemeVariables ts))
    in  
      let schemeTypeVars = (collectTypeVars schemeType)
      in  
        let usedFlags = (Lists.map (\v -> Sets.member v schemeTypeVars) allSchemeVars)
        in  
          let usedSchemeVars = (filterByFlags allSchemeVars usedFlags)
          in  
            let nParams = (countFunctionParams schemeType)
            in  
              let peeled = (peelDomainTypes nParams schemeType)
              in  
                let calleeDoms = (Pairs.first peeled)
                in  
                  let calleeCod = (Pairs.second peeled)
                  in  
                    let overgenSubst = (detectAccumulatorUnification calleeDoms calleeCod usedSchemeVars)
                    in  
                      let keepFlags = (Lists.map (\v -> Logic.and (Sets.member v schemeTypeVars) (Logic.not (Maps.member v overgenSubst))) allSchemeVars)
                      in  
                        let schemeVars = (filterByFlags allSchemeVars keepFlags)
                        in  
                          let filteredFallback0 = (Logic.ifElse (Equality.equal (Lists.length allSchemeVars) (Lists.length fallbackTypeApps)) (filterByFlags fallbackTypeApps keepFlags) fallbackTypeApps)
                          in  
                            let filteredFallback = (Logic.ifElse (Maps.null overgenSubst) filteredFallback0 (Lists.map (\t -> substituteTypeVarsWithTypes overgenSubst t) filteredFallback0))
                            in (Logic.ifElse (Logic.or (Lists.null schemeVars) (Logic.not (Equality.equal (Lists.length schemeVars) (Lists.length filteredFallback)))) (Right filteredFallback) (correctTypeAppsWithArgs schemeVars filteredFallback schemeType args cx g))))))

buildSubstFromAnnotations_go :: (S.Set Core.Name -> Graph.Graph -> Core.Term -> M.Map Core.Name Core.Name)
buildSubstFromAnnotations_go schemeVarSet g term = ((\x -> case x of
  Core.TermAnnotated v0 ->  
    let body = (Core.annotatedTermBody v0)
    in  
      let anns = (Core.annotatedTermAnnotation v0)
      in  
        let bodySubst = (buildSubstFromAnnotations_go schemeVarSet g body)
        in  
          let annSubst = (Maybes.cases (Maps.lookup Constants.key_type anns) Maps.empty (\typeTerm -> Eithers.either (\_ -> Maps.empty) (\annType -> (\x -> case x of
                  Core.TermFunction v1 -> ((\x -> case x of
                    Core.FunctionLambda v2 -> (Maybes.cases (Core.lambdaDomain v2) Maps.empty (\dom -> (\x -> case x of
                      Core.TypeFunction v3 -> (buildTypeVarSubst schemeVarSet (Core.functionTypeDomain v3) dom)
                      _ -> Maps.empty) (Rewriting.deannotateType annType)))
                    _ -> Maps.empty) v1)
                  _ -> Maps.empty) (Rewriting.deannotateTerm body)) (Core_.type_ g typeTerm)))
          in (Maps.union annSubst bodySubst)
  Core.TermApplication v0 -> (Maps.union (buildSubstFromAnnotations_go schemeVarSet g (Core.applicationFunction v0)) (buildSubstFromAnnotations_go schemeVarSet g (Core.applicationArgument v0)))
  Core.TermFunction v0 -> ((\x -> case x of
    Core.FunctionLambda v1 -> (buildSubstFromAnnotations_go schemeVarSet g (Core.lambdaBody v1))
    Core.FunctionElimination v1 -> ((\x -> case x of
      Core.EliminationUnion v2 ->  
        let defSubst = (Maybes.cases (Core.caseStatementDefault v2) Maps.empty (\d -> buildSubstFromAnnotations_go schemeVarSet g d))
        in  
          let caseSubsts = (Lists.foldl (\acc -> \fld -> Maps.union acc (buildSubstFromAnnotations_go schemeVarSet g (Core.fieldTerm fld))) Maps.empty (Core.caseStatementCases v2))
          in (Maps.union defSubst caseSubsts)
      _ -> Maps.empty) v1)
    _ -> Maps.empty) v0)
  Core.TermLet v0 ->  
    let bindingSubst = (Lists.foldl (\acc -> \b -> Maps.union acc (buildSubstFromAnnotations_go schemeVarSet g (Core.bindingTerm b))) Maps.empty (Core.letBindings v0))
    in (Maps.union bindingSubst (buildSubstFromAnnotations_go schemeVarSet g (Core.letBody v0)))
  Core.TermList v0 -> (Lists.foldl (\acc -> \t -> Maps.union acc (buildSubstFromAnnotations_go schemeVarSet g t)) Maps.empty v0)
  Core.TermMaybe v0 -> (Maybes.cases v0 Maps.empty (\t -> buildSubstFromAnnotations_go schemeVarSet g t))
  Core.TermPair v0 -> (Maps.union (buildSubstFromAnnotations_go schemeVarSet g (Pairs.first v0)) (buildSubstFromAnnotations_go schemeVarSet g (Pairs.second v0)))
  Core.TermRecord v0 -> (Lists.foldl (\acc -> \fld -> Maps.union acc (buildSubstFromAnnotations_go schemeVarSet g (Core.fieldTerm fld))) Maps.empty (Core.recordFields v0))
  Core.TermSet v0 -> (Lists.foldl (\acc -> \t -> Maps.union acc (buildSubstFromAnnotations_go schemeVarSet g t)) Maps.empty (Sets.toList v0))
  Core.TermTypeApplication v0 -> (buildSubstFromAnnotations_go schemeVarSet g (Core.typeApplicationTermBody v0))
  Core.TermTypeLambda v0 -> (buildSubstFromAnnotations_go schemeVarSet g (Core.typeLambdaBody v0))
  Core.TermEither v0 -> (Eithers.either (\t -> buildSubstFromAnnotations_go schemeVarSet g t) (\t -> buildSubstFromAnnotations_go schemeVarSet g t) v0)
  _ -> Maps.empty) term)

buildSubstFromAnnotations :: (S.Set Core.Name -> Core.Term -> t0 -> Graph.Graph -> Either t1 (M.Map Core.Name Core.Name))
buildSubstFromAnnotations schemeVarSet term cx g = (Right (buildSubstFromAnnotations_go schemeVarSet g term))

applyOvergenSubstToTermAnnotations_go :: (M.Map Core.Name Core.Type -> Graph.Graph -> Core.Term -> Core.Term)
applyOvergenSubstToTermAnnotations_go subst cx term = ((\x -> case x of
  Core.TermAnnotated v0 ->  
    let inner = (Core.annotatedTermBody v0)
    in  
      let ann = (Core.annotatedTermAnnotation v0)
      in  
        let ann_ = (Maybes.cases (Maps.lookup Constants.key_type ann) ann (\typeTerm -> Eithers.either (\_ -> ann) (\t ->  
                let t_ = (substituteTypeVarsWithTypes subst t)
                in (Maps.insert Constants.key_type (Core__.type_ t_) ann)) (Core_.type_ cx typeTerm)))
        in (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (applyOvergenSubstToTermAnnotations_go subst cx inner),
          Core.annotatedTermAnnotation = ann_}))
  Core.TermApplication v0 -> (Core.TermApplication (Core.Application {
    Core.applicationFunction = (applyOvergenSubstToTermAnnotations_go subst cx (Core.applicationFunction v0)),
    Core.applicationArgument = (applyOvergenSubstToTermAnnotations_go subst cx (Core.applicationArgument v0))}))
  Core.TermFunction v0 -> ((\x -> case x of
    Core.FunctionLambda v1 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.lambdaParameter v1),
      Core.lambdaDomain = (Maybes.map (\d -> substituteTypeVarsWithTypes subst d) (Core.lambdaDomain v1)),
      Core.lambdaBody = (applyOvergenSubstToTermAnnotations_go subst cx (Core.lambdaBody v1))})))
    Core.FunctionElimination v1 -> ((\x -> case x of
      Core.EliminationUnion v2 -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
        Core.caseStatementTypeName = (Core.caseStatementTypeName v2),
        Core.caseStatementDefault = (Maybes.map (\d -> applyOvergenSubstToTermAnnotations_go subst cx d) (Core.caseStatementDefault v2)),
        Core.caseStatementCases = (Lists.map (\fld -> Core.Field {
          Core.fieldName = (Core.fieldName fld),
          Core.fieldTerm = (applyOvergenSubstToTermAnnotations_go subst cx (Core.fieldTerm fld))}) (Core.caseStatementCases v2))}))))
      _ -> term) v1)
    _ -> term) v0)
  Core.TermLet v0 -> (Core.TermLet (Core.Let {
    Core.letBindings = (Lists.map (\b -> Core.Binding {
      Core.bindingName = (Core.bindingName b),
      Core.bindingTerm = (applyOvergenSubstToTermAnnotations_go subst cx (Core.bindingTerm b)),
      Core.bindingType = (Core.bindingType b)}) (Core.letBindings v0)),
    Core.letBody = (applyOvergenSubstToTermAnnotations_go subst cx (Core.letBody v0))}))
  Core.TermTypeApplication v0 -> (Core.TermTypeApplication (Core.TypeApplicationTerm {
    Core.typeApplicationTermBody = (applyOvergenSubstToTermAnnotations_go subst cx (Core.typeApplicationTermBody v0)),
    Core.typeApplicationTermType = (substituteTypeVarsWithTypes subst (Core.typeApplicationTermType v0))}))
  Core.TermTypeLambda v0 -> (Core.TermTypeLambda (Core.TypeLambda {
    Core.typeLambdaParameter = (Core.typeLambdaParameter v0),
    Core.typeLambdaBody = (applyOvergenSubstToTermAnnotations_go subst cx (Core.typeLambdaBody v0))}))
  _ -> term) term)

applyOvergenSubstToTermAnnotations :: (M.Map Core.Name Core.Type -> Core.Term -> t0 -> Graph.Graph -> Either t1 Core.Term)
applyOvergenSubstToTermAnnotations subst term0 cx g = (Right (applyOvergenSubstToTermAnnotations_go subst g term0))

javaComparableRefType :: Syntax.ReferenceType
javaComparableRefType = (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass (Syntax.ClassType {
  Syntax.classTypeAnnotations = [],
  Syntax.classTypeQualifier = Syntax.ClassTypeQualifierNone,
  Syntax.classTypeIdentifier = (Utils.javaTypeIdentifier "Comparable"),
  Syntax.classTypeArguments = []})))

comparableCompareExpr :: (String -> String -> Syntax.Expression)
comparableCompareExpr otherVar fname =  
  let arg = (Utils.javaExpressionNameToJavaExpression (Utils.fieldExpression (Utils.javaIdentifier otherVar) (Utils.javaIdentifier fname))) 
      castVar = (Syntax.MethodInvocation_VariantPrimary (Utils.javaExpressionToJavaPrimary (Utils.javaCastExpressionToJavaExpression (Utils.javaCastExpression javaComparableRefType (Utils.javaIdentifierToJavaUnaryExpression (Syntax.Identifier (Utils.sanitizeJavaName fname)))))))
      header = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
              Syntax.methodInvocation_ComplexVariant = castVar,
              Syntax.methodInvocation_ComplexTypeArguments = [],
              Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier Names.compareToMethodName)}))
  in (Utils.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
    Syntax.methodInvocationHeader = header,
    Syntax.methodInvocationArguments = [
      arg]}))

arraysCompareExpr :: (String -> String -> Syntax.Expression)
arraysCompareExpr otherVar fname =  
  let header = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
          Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantType (Utils.javaTypeName (Syntax.Identifier "java.util.Arrays"))),
          Syntax.methodInvocation_ComplexTypeArguments = [],
          Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier "compare")})) 
      arg1 = (Utils.javaExpressionNameToJavaExpression (Syntax.ExpressionName {
              Syntax.expressionNameQualifier = Nothing,
              Syntax.expressionNameIdentifier = (Syntax.Identifier (Utils.sanitizeJavaName fname))}))
      arg2 = (Utils.javaExpressionNameToJavaExpression (Utils.fieldExpression (Utils.javaIdentifier otherVar) (Utils.javaIdentifier fname)))
  in (Utils.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
    Syntax.methodInvocationHeader = header,
    Syntax.methodInvocationArguments = [
      arg1,
      arg2]}))

hashCodeCompareExpr :: (String -> String -> Syntax.Expression)
hashCodeCompareExpr otherVar fname =  
  let header = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
          Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantType (Utils.javaTypeName (Syntax.Identifier "Integer"))),
          Syntax.methodInvocation_ComplexTypeArguments = [],
          Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier "compare")})) 
      thisHashCode = (Utils.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
              Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
                Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantExpression (Syntax.ExpressionName {
                  Syntax.expressionNameQualifier = Nothing,
                  Syntax.expressionNameIdentifier = (Syntax.Identifier (Utils.sanitizeJavaName fname))})),
                Syntax.methodInvocation_ComplexTypeArguments = [],
                Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier Names.hashCodeMethodName)})),
              Syntax.methodInvocationArguments = []}))
      otherHashCode = (Utils.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
              Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
                Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantExpression (Utils.fieldExpression (Utils.javaIdentifier otherVar) (Utils.javaIdentifier fname))),
                Syntax.methodInvocation_ComplexTypeArguments = [],
                Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier Names.hashCodeMethodName)})),
              Syntax.methodInvocationArguments = []}))
  in (Utils.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
    Syntax.methodInvocationHeader = header,
    Syntax.methodInvocationArguments = [
      thisHashCode,
      otherHashCode]}))

compareFieldExpr :: (String -> Core.FieldType -> Syntax.Expression)
compareFieldExpr otherVar ft =  
  let fname = (Core.unName (Core.fieldTypeName ft))
  in  
    let ftype = (Core.fieldTypeType ft)
    in (Logic.ifElse (isBinaryType ftype) (arraysCompareExpr otherVar fname) (Logic.ifElse (isNonComparableType ftype) (hashCodeCompareExpr otherVar fname) (comparableCompareExpr otherVar fname)))

cmpNotZeroExpr :: Syntax.Expression
cmpNotZeroExpr = (Utils.javaEqualityExpressionToJavaExpression (Syntax.EqualityExpressionNotEqual (Syntax.EqualityExpression_Binary {
  Syntax.equalityExpression_BinaryLhs = lhs,
  Syntax.equalityExpression_BinaryRhs = rhs}))) 
  where 
    lhs = (Utils.javaRelationalExpressionToJavaEqualityExpression (Utils.javaPostfixExpressionToJavaRelationalExpression (Syntax.PostfixExpressionName (Syntax.ExpressionName {
      Syntax.expressionNameQualifier = Nothing,
      Syntax.expressionNameIdentifier = (Utils.javaIdentifier "cmp")}))))
    rhs = (Utils.javaPostfixExpressionToJavaRelationalExpression (Syntax.PostfixExpressionPrimary (Utils.javaLiteralToJavaPrimary (Utils.javaInt 0))))

cmpDeclStatement :: (t0 -> Syntax.BlockStatement)
cmpDeclStatement aliases = (Utils.variableDeclarationStatement aliases Utils.javaIntType (Utils.javaIdentifier "cmp") (Utils.javaIntExpression 0))

compareAndReturnStmts :: (String -> Core.FieldType -> [Syntax.BlockStatement])
compareAndReturnStmts otherVar f = [
  Syntax.BlockStatementStatement (Utils.javaAssignmentStatement (Syntax.LeftHandSideExpressionName (Syntax.ExpressionName {
    Syntax.expressionNameQualifier = Nothing,
    Syntax.expressionNameIdentifier = (Utils.javaIdentifier "cmp")})) (compareFieldExpr otherVar f)),
  (Syntax.BlockStatementStatement (Syntax.StatementIfThen (Syntax.IfThenStatement {
    Syntax.ifThenStatementExpression = cmpNotZeroExpr,
    Syntax.ifThenStatementStatement = (Utils.javaReturnStatement (Just (Utils.javaExpressionNameToJavaExpression (Syntax.ExpressionName {
      Syntax.expressionNameQualifier = Nothing,
      Syntax.expressionNameIdentifier = (Utils.javaIdentifier "cmp")}))))})))]

compareToBody :: (t0 -> String -> [Core.FieldType] -> [Syntax.BlockStatement])
compareToBody aliases otherVar fields = (Logic.ifElse (Lists.null fields) [
  Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just (Utils.javaIntExpression 0)))] (Logic.ifElse (Equality.equal (Lists.length fields) 1) [
  Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just (compareFieldExpr otherVar (Lists.head fields))))] (Lists.concat2 [
  cmpDeclStatement aliases] (Lists.concat2 (Lists.concat (Lists.map (\f -> compareAndReturnStmts otherVar f) (Lists.init fields))) [
  Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just (compareFieldExpr otherVar (Lists.last fields))))]))))

tagCompareExpr :: Syntax.Expression
tagCompareExpr = (Utils.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
  Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
    Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantPrimary (Utils.javaMethodInvocationToJavaPrimary thisGetName)),
    Syntax.methodInvocation_ComplexTypeArguments = [],
    Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier Names.compareToMethodName)})),
  Syntax.methodInvocationArguments = [
    Utils.javaMethodInvocationToJavaExpression otherGetName]})) 
  where 
    thisGetClass = Syntax.MethodInvocation {
      Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
        Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantPrimary (Utils.javaExpressionToJavaPrimary Utils.javaThis)),
        Syntax.methodInvocation_ComplexTypeArguments = [],
        Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier "getClass")})),
      Syntax.methodInvocationArguments = []}
    thisGetName = Syntax.MethodInvocation {
      Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
        Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantPrimary (Utils.javaMethodInvocationToJavaPrimary thisGetClass)),
        Syntax.methodInvocation_ComplexTypeArguments = [],
        Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier "getName")})),
      Syntax.methodInvocationArguments = []}
    otherGetClass = Syntax.MethodInvocation {
      Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
        Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantExpression (Syntax.ExpressionName {
          Syntax.expressionNameQualifier = Nothing,
          Syntax.expressionNameIdentifier = (Syntax.Identifier Names.otherInstanceName)})),
        Syntax.methodInvocation_ComplexTypeArguments = [],
        Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier "getClass")})),
      Syntax.methodInvocationArguments = []}
    otherGetName = Syntax.MethodInvocation {
      Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
        Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantPrimary (Utils.javaMethodInvocationToJavaPrimary otherGetClass)),
        Syntax.methodInvocation_ComplexTypeArguments = [],
        Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier "getName")})),
      Syntax.methodInvocationArguments = []}

tagCmpNotZeroExpr :: Syntax.Expression
tagCmpNotZeroExpr = (Utils.javaEqualityExpressionToJavaExpression (Syntax.EqualityExpressionNotEqual (Syntax.EqualityExpression_Binary {
  Syntax.equalityExpression_BinaryLhs = lhs,
  Syntax.equalityExpression_BinaryRhs = rhs}))) 
  where 
    lhs = (Utils.javaRelationalExpressionToJavaEqualityExpression (Utils.javaPostfixExpressionToJavaRelationalExpression (Syntax.PostfixExpressionName (Syntax.ExpressionName {
      Syntax.expressionNameQualifier = Nothing,
      Syntax.expressionNameIdentifier = (Utils.javaIdentifier "tagCmp")}))))
    rhs = (Utils.javaPostfixExpressionToJavaRelationalExpression (Syntax.PostfixExpressionPrimary (Utils.javaLiteralToJavaPrimary (Utils.javaInt 0))))

recordCompareToMethod :: (Helpers.Aliases -> t0 -> Core.Name -> [Core.FieldType] -> Syntax.ClassBodyDeclaration)
recordCompareToMethod aliases tparams elName fields =  
  let anns = [
          Utils.overrideAnnotation,
          Utils.suppressWarningsUncheckedAnnotation] 
      mods = [
              Syntax.MethodModifierPublic]
      param = (Utils.javaTypeToJavaFormalParameter (Utils.javaTypeFromTypeName aliases elName) (Core.Name Names.otherInstanceName))
      result = (Utils.javaTypeToJavaResult Utils.javaIntType)
  in (Utils.methodDeclaration mods [] anns Names.compareToMethodName [
    param] result (Just (compareToBody aliases Names.otherInstanceName fields)))

variantCompareToMethod :: (Helpers.Aliases -> t0 -> Core.Name -> Core.Name -> [Core.FieldType] -> Syntax.ClassBodyDeclaration)
variantCompareToMethod aliases tparams parentName variantName fields =  
  let anns = [
          Utils.overrideAnnotation,
          Utils.suppressWarningsUncheckedAnnotation] 
      mods = [
              Syntax.MethodModifierPublic]
      param = (Utils.javaTypeToJavaFormalParameter (Utils.javaTypeFromTypeName aliases parentName) (Core.Name Names.otherInstanceName))
      result = (Utils.javaTypeToJavaResult Utils.javaIntType)
      varTmpName = "o"
      tagDeclStmt = (Utils.variableDeclarationStatement aliases Utils.javaIntType (Utils.javaIdentifier "tagCmp") tagCompareExpr)
      tagReturnStmt = (Syntax.BlockStatementStatement (Syntax.StatementIfThen (Syntax.IfThenStatement {
              Syntax.ifThenStatementExpression = tagCmpNotZeroExpr,
              Syntax.ifThenStatementStatement = (Utils.javaReturnStatement (Just (Utils.javaExpressionNameToJavaExpression (Syntax.ExpressionName {
                Syntax.expressionNameQualifier = Nothing,
                Syntax.expressionNameIdentifier = (Utils.javaIdentifier "tagCmp")}))))})))
      variantJavaType = (Utils.javaTypeFromTypeName aliases variantName)
      castOtherExpr = (Utils.javaCastExpressionToJavaExpression (Utils.javaCastExpression (Utils.nameToJavaReferenceType aliases False [] variantName Nothing) (Utils.javaIdentifierToJavaUnaryExpression (Syntax.Identifier Names.otherInstanceName))))
      castDeclStmt = (Utils.variableDeclarationStatement aliases variantJavaType (Utils.javaIdentifier varTmpName) castOtherExpr)
      emptyReturn = [
              Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just (Utils.javaIntExpression 0)))]
      valueCompareStmt = (Logic.ifElse (Lists.null fields) emptyReturn (Lists.concat2 [
              castDeclStmt] (compareToBody aliases varTmpName fields)))
      body = (Lists.concat2 [
              tagDeclStmt,
              tagReturnStmt] valueCompareStmt)
  in (Utils.methodDeclaration mods [] anns Names.compareToMethodName [
    param] result (Just body))

recordMemberVar :: (Helpers.Aliases -> Core.FieldType -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.ClassBodyDeclaration)
recordMemberVar aliases ft cx g =  
  let mods = [
          Syntax.FieldModifierPublic,
          Syntax.FieldModifierFinal] 
      fname = (Core.fieldTypeName ft)
      ftype = (Core.fieldTypeType ft)
  in (Eithers.bind (encodeType aliases Sets.empty ftype cx g) (\jt -> Right (Utils.javaMemberField mods jt (Utils.fieldNameToJavaVariableDeclarator fname))))

recordWithMethod :: (Helpers.Aliases -> Core.Name -> [Core.FieldType] -> Core.FieldType -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.ClassBodyDeclaration)
recordWithMethod aliases elName fields field cx g =  
  let mods = [
          Syntax.MethodModifierPublic] 
      anns = []
      methodName = (Strings.cat2 "with" (Formatting.nonAlnumToUnderscores (Formatting.capitalize (Core.unName (Core.fieldTypeName field)))))
      result = (Utils.referenceTypeToResult (Utils.nameToJavaReferenceType aliases False [] elName Nothing))
      consId = (Syntax.Identifier (Utils.sanitizeJavaName (Names_.localNameOf elName)))
      fieldArgs = (Lists.map (\f -> Utils.fieldNameToJavaExpression (Core.fieldTypeName f)) fields)
      returnStmt = (Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just (Utils.javaConstructorCall (Utils.javaConstructorName consId Nothing) fieldArgs Nothing))))
  in (Eithers.bind (fieldTypeToFormalParam aliases field cx g) (\param -> Right (Utils.methodDeclaration mods [] anns methodName [
    param] result (Just [
    returnStmt]))))

recordConstructor :: (Helpers.Aliases -> Core.Name -> [Core.FieldType] -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.ClassBodyDeclaration)
recordConstructor aliases elName fields cx g =  
  let assignStmts = (Lists.map (\f -> Syntax.BlockStatementStatement (Utils.toAssignStmt (Core.fieldTypeName f))) fields)
  in (Eithers.bind (Eithers.mapList (\f -> fieldTypeToFormalParam aliases f cx g) fields) (\params -> Right (Utils.makeConstructor aliases elName False params assignStmts)))

eqClause :: (String -> Core.FieldType -> Syntax.InclusiveOrExpression)
eqClause tmpName ft =  
  let fname = (Core.unName (Core.fieldTypeName ft)) 
      ftype = (Core.fieldTypeType ft)
  in (Logic.ifElse (isBinaryType ftype) (arraysEqualsClause tmpName fname) (Logic.ifElse (isBigNumericType ftype) (compareToZeroClause tmpName fname) (equalsClause tmpName fname)))

equalsClause :: (String -> String -> Syntax.InclusiveOrExpression)
equalsClause tmpName fname =  
  let thisArg = (Utils.javaExpressionNameToJavaExpression (Utils.fieldExpression (Syntax.Identifier "this") (Utils.javaIdentifier fname))) 
      otherArg = (Utils.javaExpressionNameToJavaExpression (Utils.fieldExpression (Utils.javaIdentifier tmpName) (Utils.javaIdentifier fname)))
      header = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
              Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantType (Utils.javaTypeName (Syntax.Identifier "java.util.Objects"))),
              Syntax.methodInvocation_ComplexTypeArguments = [],
              Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier Names.equalsMethodName)}))
  in (Utils.javaPostfixExpressionToJavaInclusiveOrExpression (Utils.javaMethodInvocationToJavaPostfixExpression (Syntax.MethodInvocation {
    Syntax.methodInvocationHeader = header,
    Syntax.methodInvocationArguments = [
      thisArg,
      otherArg]})))

arraysEqualsClause :: (String -> String -> Syntax.InclusiveOrExpression)
arraysEqualsClause tmpName fname =  
  let thisArg = (Utils.javaExpressionNameToJavaExpression (Utils.fieldExpression (Syntax.Identifier "this") (Utils.javaIdentifier fname))) 
      otherArg = (Utils.javaExpressionNameToJavaExpression (Utils.fieldExpression (Utils.javaIdentifier tmpName) (Utils.javaIdentifier fname)))
      header = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
              Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantType (Utils.javaTypeName (Syntax.Identifier "java.util.Arrays"))),
              Syntax.methodInvocation_ComplexTypeArguments = [],
              Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier Names.equalsMethodName)}))
  in (Utils.javaPostfixExpressionToJavaInclusiveOrExpression (Utils.javaMethodInvocationToJavaPostfixExpression (Syntax.MethodInvocation {
    Syntax.methodInvocationHeader = header,
    Syntax.methodInvocationArguments = [
      thisArg,
      otherArg]})))

compareToZeroClause :: (String -> String -> Syntax.InclusiveOrExpression)
compareToZeroClause tmpName fname =  
  let compareToArg = (Utils.javaExpressionNameToJavaExpression (Utils.fieldExpression (Utils.javaIdentifier tmpName) (Utils.javaIdentifier fname))) 
      compareToVar = (Syntax.MethodInvocation_VariantExpression (Utils.fieldExpression (Syntax.Identifier "this") (Utils.javaIdentifier fname)))
      compareToHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
              Syntax.methodInvocation_ComplexVariant = compareToVar,
              Syntax.methodInvocation_ComplexTypeArguments = [],
              Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier Names.compareToMethodName)}))
      lhs = (Utils.javaRelationalExpressionToJavaEqualityExpression (Utils.javaPostfixExpressionToJavaRelationalExpression (Utils.javaMethodInvocationToJavaPostfixExpression (Syntax.MethodInvocation {
              Syntax.methodInvocationHeader = compareToHeader,
              Syntax.methodInvocationArguments = [
                compareToArg]}))))
      rhs = (Utils.javaPostfixExpressionToJavaRelationalExpression (Syntax.PostfixExpressionPrimary (Utils.javaLiteralToJavaPrimary (Utils.javaInt 0))))
  in (Utils.javaEqualityExpressionToJavaInclusiveOrExpression (Syntax.EqualityExpressionEqual (Syntax.EqualityExpression_Binary {
    Syntax.equalityExpression_BinaryLhs = lhs,
    Syntax.equalityExpression_BinaryRhs = rhs})))

recordEqualsMethod :: (Helpers.Aliases -> Core.Name -> [Core.FieldType] -> Syntax.ClassBodyDeclaration)
recordEqualsMethod aliases elName fields =  
  let anns = [
          Utils.overrideAnnotation] 
      mods = [
              Syntax.MethodModifierPublic]
      param = (Utils.javaTypeToJavaFormalParameter (Utils.javaRefType [] Nothing "Object") (Core.Name Names.otherInstanceName))
      result = (Utils.javaTypeToJavaResult Utils.javaBooleanType)
      tmpName = "o"
      instanceOfStmt = (Syntax.BlockStatementStatement (Syntax.StatementIfThen (Syntax.IfThenStatement {
              Syntax.ifThenStatementExpression = (Utils.javaUnaryExpressionToJavaExpression (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusNot (Utils.javaRelationalExpressionToJavaUnaryExpression (Utils.javaInstanceOf (Utils.javaIdentifierToJavaRelationalExpression (Utils.javaIdentifier Names.otherInstanceName)) (Utils.nameToJavaReferenceType aliases False [] elName Nothing)))))),
              Syntax.ifThenStatementStatement = (Utils.javaReturnStatement (Just (Utils.javaBooleanExpression False)))})))
      castStmt = (Utils.variableDeclarationStatement aliases (Utils.javaTypeFromTypeName aliases elName) (Utils.javaIdentifier tmpName) (Utils.javaCastExpressionToJavaExpression (Utils.javaCastExpression (Utils.nameToJavaReferenceType aliases False [] elName Nothing) (Utils.javaIdentifierToJavaUnaryExpression (Syntax.Identifier (Utils.sanitizeJavaName Names.otherInstanceName))))))
      returnAllFieldsEqual = (Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just (Logic.ifElse (Lists.null fields) (Utils.javaBooleanExpression True) (Utils.javaConditionalAndExpressionToJavaExpression (Syntax.ConditionalAndExpression (Lists.map (\f -> eqClause tmpName f) fields)))))))
  in (Utils.methodDeclaration mods [] anns Names.equalsMethodName [
    param] result (Just [
    instanceOfStmt,
    castStmt,
    returnAllFieldsEqual]))

hashCodeMultPair :: (Integer -> Core.Name -> Syntax.MultiplicativeExpression)
hashCodeMultPair i fname =  
  let fnameStr = (Core.unName fname) 
      lhs = (Syntax.MultiplicativeExpressionUnary (Utils.javaPrimaryToJavaUnaryExpression (Utils.javaLiteralToJavaPrimary (Utils.javaInt i))))
      rhs = (Utils.javaPostfixExpressionToJavaUnaryExpression (Utils.javaMethodInvocationToJavaPostfixExpression (Syntax.MethodInvocation {
              Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
                Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantType (Utils.javaTypeName (Syntax.Identifier "java.util.Objects"))),
                Syntax.methodInvocation_ComplexTypeArguments = [],
                Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier Names.hashCodeMethodName)})),
              Syntax.methodInvocationArguments = [
                Utils.javaExpressionNameToJavaExpression (Syntax.ExpressionName {
                  Syntax.expressionNameQualifier = Nothing,
                  Syntax.expressionNameIdentifier = (Syntax.Identifier (Utils.sanitizeJavaName fnameStr))})]})))
  in (Syntax.MultiplicativeExpressionTimes (Syntax.MultiplicativeExpression_Binary {
    Syntax.multiplicativeExpression_BinaryLhs = lhs,
    Syntax.multiplicativeExpression_BinaryRhs = rhs}))

first20Primes :: [Integer]
first20Primes = [
  2,
  3,
  5,
  7,
  11,
  13,
  17,
  19,
  23,
  29,
  31,
  37,
  41,
  43,
  47,
  53,
  59,
  61,
  67,
  71]

recordHashCodeMethod :: ([Core.FieldType] -> Syntax.ClassBodyDeclaration)
recordHashCodeMethod fields =  
  let anns = [
          Utils.overrideAnnotation] 
      mods = [
              Syntax.MethodModifierPublic]
      result = (Utils.javaTypeToJavaResult Utils.javaIntType)
      returnSum = (Syntax.BlockStatementStatement (Logic.ifElse (Lists.null fields) (Utils.javaReturnStatement (Just (Utils.javaIntExpression 0))) (Utils.javaReturnStatement (Just (Utils.javaAdditiveExpressionToJavaExpression (Utils.addExpressions (Lists.zipWith hashCodeMultPair first20Primes (Lists.map (\f -> Core.fieldTypeName f) fields))))))))
  in (Utils.methodDeclaration mods [] anns Names.hashCodeMethodName [] result (Just [
    returnSum]))

constantDecl :: (String -> Helpers.Aliases -> Core.Name -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.ClassBodyDeclarationWithComments)
constantDecl javaName aliases name cx g =  
  let mods = [
          Syntax.FieldModifierPublic,
          Syntax.FieldModifierStatic,
          Syntax.FieldModifierFinal] 
      nameName = (Utils.nameToJavaName aliases (Core.Name "hydra.core.Name"))
  in  
    let env = Helpers.JavaEnvironment {
            Helpers.javaEnvironmentAliases = aliases,
            Helpers.javaEnvironmentGraph = g}
    in (Eithers.bind (encodeType aliases Sets.empty (Core.TypeVariable (Core.Name "hydra.core.Name")) cx g) (\jt -> Eithers.bind (encodeTerm env (Core.TermLiteral (Core.LiteralString (Core.unName name))) cx g) (\arg ->  
      let init = (Syntax.VariableInitializerExpression (Utils.javaConstructorCall (Utils.javaConstructorName nameName Nothing) [
              arg] Nothing))
      in  
        let var = (Utils.javaVariableDeclarator (Syntax.Identifier javaName) (Just init))
        in (Right (noComment (Utils.javaMemberField mods jt var))))))

constantDeclForFieldType :: (Helpers.Aliases -> Core.FieldType -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.ClassBodyDeclarationWithComments)
constantDeclForFieldType aliases ftyp cx g =  
  let name = (Core.fieldTypeName ftyp) 
      javaName = (Formatting.nonAlnumToUnderscores (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionUpperSnake (Core.unName name)))
  in (constantDecl javaName aliases name cx g)

constantDeclForTypeName :: (Helpers.Aliases -> Core.Name -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.ClassBodyDeclarationWithComments)
constantDeclForTypeName aliases name cx g = (constantDecl "TYPE_" aliases name cx g)

declarationForRecordType :: (Bool -> Bool -> Helpers.Aliases -> [Syntax.TypeParameter] -> Core.Name -> [Core.FieldType] -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.ClassDeclaration)
declarationForRecordType isInner isSer aliases tparams elName fields cx g = (declarationForRecordType_ isInner isSer aliases tparams elName Nothing fields cx g)

declarationForRecordType_ :: (Bool -> Bool -> Helpers.Aliases -> [Syntax.TypeParameter] -> Core.Name -> Maybe Core.Name -> [Core.FieldType] -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.ClassDeclaration)
declarationForRecordType_ isInner isSer aliases tparams elName parentName fields cx g = (Eithers.bind (Eithers.mapList (\f -> recordMemberVar aliases f cx g) fields) (\memberVars -> Eithers.bind (Eithers.mapList (\p -> addComment (Pairs.first p) (Pairs.second p) cx g) (Lists.zip memberVars fields)) (\memberVars_ -> Eithers.bind (Logic.ifElse (Equality.gt (Lists.length fields) 1) (Eithers.mapList (\f -> recordWithMethod aliases elName fields f cx g) fields) (Right [])) (\withMethods -> Eithers.bind (recordConstructor aliases elName fields cx g) (\cons -> Eithers.bind (Logic.ifElse isInner (Right []) (Eithers.bind (constantDeclForTypeName aliases elName cx g) (\d -> Eithers.bind (Eithers.mapList (\f -> constantDeclForFieldType aliases f cx g) fields) (\dfields -> Right (Lists.cons d dfields))))) (\tn ->  
  let comparableMethods = (Maybes.cases parentName (Logic.ifElse (Logic.and (Logic.not isInner) isSer) [
          recordCompareToMethod aliases tparams elName fields] []) (\pn -> Logic.ifElse isSer [
          variantCompareToMethod aliases tparams pn elName fields] []))
  in  
    let bodyDecls = (Lists.concat2 tn (Lists.concat2 memberVars_ (Lists.map (\x -> noComment x) (Lists.concat2 [
            cons,
            (recordEqualsMethod aliases elName fields),
            (recordHashCodeMethod fields)] (Lists.concat2 comparableMethods withMethods)))))
    in  
      let ifaces = (Logic.ifElse isInner (serializableTypes isSer) (interfaceTypes isSer aliases tparams elName))
      in (Right (Utils.javaClassDeclaration aliases tparams elName classModsPublic Nothing ifaces bodyDecls))))))))

takeTypeArgs :: (String -> Int -> [Syntax.Type] -> Context.Context -> t0 -> Either (Context.InContext Error.Error) [Syntax.TypeArgument])
takeTypeArgs label n tyapps cx g = (Logic.ifElse (Equality.lt (Lists.length tyapps) n) (Left (Context.InContext {
  Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat [
    "needed type arguments for ",
    label,
    ", found too few"]))),
  Context.inContextContext = cx})) (Eithers.mapList (\jt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jt cx) (\rt -> Right (Syntax.TypeArgumentReference rt))) (Lists.take n tyapps)))

isFieldUnitType :: (Core.Name -> Core.Name -> t0 -> Graph.Graph -> Either t1 Bool)
isFieldUnitType typeName fieldName cx g =  
  let schemaTypes = (Graph.graphSchemaTypes g)
  in (Maybes.cases (Maps.lookup typeName schemaTypes) (Right False) (\ts -> (\x -> case x of
    Core.TypeUnion v0 -> (Right (Maybes.cases (Lists.find (\ft -> Equality.equal (Core.fieldTypeName ft) fieldName) (Core.rowTypeFields v0)) False (\ft -> Schemas.isUnitType (Rewriting.deannotateType (Core.fieldTypeType ft)))))
    _ -> (Right False)) (Rewriting.deannotateType (Core.typeSchemeType ts))))

encodeTerm :: (Helpers.JavaEnvironment -> Core.Term -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.Expression)
encodeTerm env term cx g = (encodeTermInternal env [] [] term cx g)

encodeTermInternal :: (Helpers.JavaEnvironment -> [M.Map Core.Name Core.Term] -> [Syntax.Type] -> Core.Term -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.Expression)
encodeTermInternal env anns tyapps term cx g0 =  
  let aliases = (Helpers.javaEnvironmentAliases env) 
      g = (Helpers.javaEnvironmentGraph env)
      encode = (\t -> encodeTerm env t cx g)
  in ((\x -> case x of
    Core.TermAnnotated v0 -> (encodeTermInternal env (Lists.cons (Core.annotatedTermAnnotation v0) anns) tyapps (Core.annotatedTermBody v0) cx g)
    Core.TermApplication v0 -> (encodeApplication env v0 cx g)
    Core.TermEither v0 -> (Eithers.bind (takeTypeArgs "either" 2 tyapps cx g) (\targs ->  
      let combinedAnns = (Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns)
      in (Eithers.bind (Eithers.bimap (\_de -> Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unDecodingError _de))),
        Context.inContextContext = cx}) (\_a -> _a) (Annotations.getType g combinedAnns)) (\mEitherType ->  
        let branchTypes = (Maybes.bind mEitherType (\etyp -> (\x -> case x of
                Core.TypeEither v1 -> (Just (Core.eitherTypeLeft v1, (Core.eitherTypeRight v1)))
                _ -> Nothing) (Rewriting.deannotateType etyp)))
        in  
          let encodeWithType = (\branchType -> \t1 ->  
                  let annotated = (Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ branchType)) t1)
                  in (encodeTermInternal env anns [] annotated cx g))
          in (Eithers.either (\term1 -> Eithers.bind (Maybes.cases branchTypes (encode term1) (\bt -> encodeWithType (Pairs.first bt) term1)) (\expr -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs (Syntax.Identifier "hydra.util.Either") (Syntax.Identifier "left") targs [
            expr])))) (\term1 -> Eithers.bind (Maybes.cases branchTypes (encode term1) (\bt -> encodeWithType (Pairs.second bt) term1)) (\expr -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs (Syntax.Identifier "hydra.util.Either") (Syntax.Identifier "right") targs [
            expr])))) v0)))))
    Core.TermFunction v0 ->  
      let combinedAnns = (Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns)
      in (Eithers.bind (Eithers.bimap (\_de -> Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unDecodingError _de))),
        Context.inContextContext = cx}) (\_a -> _a) (Annotations.getType g combinedAnns)) (\mt -> Eithers.bind (Maybes.cases mt (Maybes.cases (tryInferFunctionType v0) (CoderUtils.typeOfTerm cx g term) (\inferredType -> Right inferredType)) (\t -> Right t)) (\typ -> (\x -> case x of
        Core.TypeFunction v1 -> (encodeFunction env (Core.functionTypeDomain v1) (Core.functionTypeCodomain v1) v0 cx g)
        _ -> (encodeNullaryConstant env typ v0 cx g)) (Rewriting.deannotateType typ))))
    Core.TermLet v0 ->  
      let bindings = (Core.letBindings v0)
      in  
        let body = (Core.letBody v0)
        in (Logic.ifElse (Lists.null bindings) (encodeTermInternal env anns [] body cx g) (Eithers.bind (bindingsToStatements env bindings cx g) (\bindResult ->  
          let bindingStmts = (Pairs.first bindResult)
          in  
            let env2 = (Pairs.second bindResult)
            in (Eithers.bind (encodeTermInternal env2 anns [] body cx g) (\jbody ->  
              let returnSt = (Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just jbody)))
              in  
                let block = (Syntax.Block (Lists.concat2 bindingStmts [
                        returnSt]))
                in  
                  let nullaryLambda = (Syntax.ExpressionLambda (Syntax.LambdaExpression {
                          Syntax.lambdaExpressionParameters = (Syntax.LambdaParametersTuple []),
                          Syntax.lambdaExpressionBody = (Syntax.LambdaBodyBlock block)}))
                  in  
                    let combinedAnns = (Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns)
                    in  
                      let g2 = (Helpers.javaEnvironmentGraph env2)
                      in  
                        let aliases2 = (Helpers.javaEnvironmentAliases env2)
                        in (Eithers.bind (Eithers.bimap (\_de -> Context.InContext {
                          Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unDecodingError _de))),
                          Context.inContextContext = cx}) (\_a -> _a) (Annotations.getType g combinedAnns)) (\mt -> Eithers.bind (Maybes.cases mt (CoderUtils.typeOfTerm cx g2 body) (\t -> Right t)) (\letType -> Eithers.bind (encodeType aliases2 Sets.empty letType cx g) (\jLetType -> Eithers.bind (Utils.javaTypeToJavaReferenceType jLetType cx) (\rt ->  
                          let supplierRt = (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass (Utils.javaClassType [
                                  rt] Names.javaUtilFunctionPackageName "Supplier")))
                          in  
                            let castExpr = (Utils.javaCastExpressionToJavaExpression (Utils.javaCastExpression supplierRt (Utils.javaExpressionToJavaUnaryExpression nullaryLambda)))
                            in (Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocation (Just (Right (Utils.javaExpressionToJavaPrimary castExpr))) (Syntax.Identifier "get") [])))))))))))))
    Core.TermList v0 -> (Eithers.bind (Eithers.mapList encode v0) (\jels -> Eithers.bind (Logic.ifElse (Lists.null jels) (takeTypeArgs "list" 1 tyapps cx g) (Right [])) (\targs -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs (Syntax.Identifier "hydra.util.ConsList") (Syntax.Identifier "of") targs jels)))))
    Core.TermLiteral v0 -> (Right (encodeLiteral v0))
    Core.TermMap v0 -> (Eithers.bind (Eithers.mapList encode (Maps.keys v0)) (\jkeys -> Eithers.bind (Eithers.mapList encode (Maps.elems v0)) (\jvals ->  
      let pairExprs = (Lists.map (\kv -> Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStatic (Syntax.Identifier "hydra.util.PersistentMap") (Syntax.Identifier "entry") [
              Pairs.first kv,
              (Pairs.second kv)])) (Lists.zip jkeys jvals))
      in (Eithers.bind (Logic.ifElse (Maps.null v0) (takeTypeArgs "map" 2 tyapps cx g) (Right [])) (\targs -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs (Syntax.Identifier "hydra.util.PersistentMap") (Syntax.Identifier "ofEntries") targs pairExprs)))))))
    Core.TermMaybe v0 -> (Maybes.cases v0 (Eithers.bind (takeTypeArgs "maybe" 1 tyapps cx g) (\targs -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs (Syntax.Identifier "hydra.util.Maybe") (Syntax.Identifier "nothing") targs [])))) (\term1 -> Eithers.bind (encode term1) (\expr -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStatic (Syntax.Identifier "hydra.util.Maybe") (Syntax.Identifier "just") [
      expr])))))
    Core.TermPair v0 -> (Eithers.bind (encode (Pairs.first v0)) (\jterm1 -> Eithers.bind (encode (Pairs.second v0)) (\jterm2 -> Eithers.bind (Logic.ifElse (Lists.null tyapps) (Right Nothing) (Eithers.bind (Eithers.mapList (\jt -> Utils.javaTypeToJavaReferenceType jt cx) tyapps) (\rts -> Right (Just (Syntax.TypeArgumentsOrDiamondArguments (Lists.map (\rt -> Syntax.TypeArgumentReference rt) rts)))))) (\mtargs -> Right (Utils.javaConstructorCall (Utils.javaConstructorName (Syntax.Identifier "hydra.util.Pair") mtargs) [
      jterm1,
      jterm2] Nothing)))))
    Core.TermRecord v0 ->  
      let recName = (Core.recordTypeName v0)
      in  
        let mRecordType = (Eithers.either (\_ -> Nothing) (\t -> Just t) (Schemas.requireType cx g recName))
        in  
          let strippedRecTyp = (Maybes.map (\recTyp -> stripForalls (Rewriting.deannotateType recTyp)) mRecordType)
          in  
            let mFieldTypeMap = (Maybes.bind strippedRecTyp (\bodyTyp -> (\x -> case x of
                    Core.TypeRecord v1 -> (Just (Maps.fromList (Lists.map (\ft -> (Core.fieldTypeName ft, (Core.fieldTypeType ft))) (Core.rowTypeFields v1))))
                    _ -> Nothing) bodyTyp))
            in  
              let combinedAnnsRec = (Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns)
              in (Eithers.bind (Eithers.bimap (\_de -> Context.InContext {
                Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unDecodingError _de))),
                Context.inContextContext = cx}) (\_a -> _a) (Annotations.getType g combinedAnnsRec)) (\mAnnotType ->  
                let mTypeSubst = (Maybes.bind mAnnotType (\annTyp -> Maybes.bind mRecordType (\recTyp ->  
                        let args = (extractTypeApplicationArgs (Rewriting.deannotateType annTyp))
                        in  
                          let params = (collectForallParams (Rewriting.deannotateType recTyp))
                          in (Logic.ifElse (Logic.or (Lists.null args) (Logic.not (Equality.equal (Lists.length args) (Lists.length params)))) Nothing (Just (Maps.fromList (Lists.zip params args)))))))
                in  
                  let encodeField = (\fld -> Maybes.cases mFieldTypeMap (encode (Core.fieldTerm fld)) (\ftmap ->  
                          let mftyp = (Maps.lookup (Core.fieldName fld) ftmap)
                          in (Maybes.cases mftyp (encode (Core.fieldTerm fld)) (\ftyp ->  
                            let resolvedType = (Maybes.cases mTypeSubst ftyp (\subst -> applySubstFull subst ftyp))
                            in  
                              let annotatedFieldTerm = (Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ resolvedType)) (Core.fieldTerm fld))
                              in (encodeTermInternal env anns [] annotatedFieldTerm cx g)))))
                  in (Eithers.bind (Eithers.mapList encodeField (Core.recordFields v0)) (\fieldExprs ->  
                    let consId = (Utils.nameToJavaName aliases recName)
                    in (Eithers.bind (Logic.ifElse (Logic.not (Lists.null tyapps)) (Eithers.bind (Eithers.mapList (\jt -> Utils.javaTypeToJavaReferenceType jt cx) tyapps) (\rts -> Right (Just (Syntax.TypeArgumentsOrDiamondArguments (Lists.map (\rt -> Syntax.TypeArgumentReference rt) rts))))) ( 
                      let combinedAnns = (Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns)
                      in (Eithers.bind (Eithers.bimap (\_de -> Context.InContext {
                        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unDecodingError _de))),
                        Context.inContextContext = cx}) (\_a -> _a) (Annotations.getType g combinedAnns)) (\mtyp -> Maybes.cases mtyp (Right Nothing) (\annTyp ->  
                        let typeArgs = (extractTypeApplicationArgs (Rewriting.deannotateType annTyp))
                        in (Logic.ifElse (Lists.null typeArgs) (Right Nothing) (Eithers.bind (Eithers.mapList (\t -> Eithers.bind (encodeType aliases Sets.empty t cx g) (\jt -> Utils.javaTypeToJavaReferenceType jt cx)) typeArgs) (\jTypeArgs -> Right (Just (Syntax.TypeArgumentsOrDiamondArguments (Lists.map (\rt -> Syntax.TypeArgumentReference rt) jTypeArgs))))))))))) (\mtargs -> Right (Utils.javaConstructorCall (Utils.javaConstructorName consId mtargs) fieldExprs Nothing)))))))
    Core.TermSet v0 ->  
      let slist = (Sets.toList v0)
      in (Eithers.bind (Eithers.mapList encode slist) (\jels -> Eithers.bind (Logic.ifElse (Sets.null v0) (takeTypeArgs "set" 1 tyapps cx g) (Right [])) (\targs -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs (Syntax.Identifier "hydra.util.PersistentSet") (Syntax.Identifier "of") targs jels)))))
    Core.TermTypeLambda v0 -> (withTypeLambda env v0 (\env2 ->  
      let combinedAnns = (Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns)
      in (Eithers.bind (Eithers.bimap (\_de -> Context.InContext {
        Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unDecodingError _de))),
        Context.inContextContext = cx}) (\_a -> _a) (Annotations.getType g combinedAnns)) (\mtyp ->  
        let annotatedBody = (Maybes.cases mtyp (Core.typeLambdaBody v0) (\t -> (\x -> case x of
                Core.TypeForall v1 -> (Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ (Core.forallTypeBody v1))) (Core.typeLambdaBody v0))
                _ -> (Core.typeLambdaBody v0)) t))
        in (encodeTerm env2 annotatedBody cx g)))))
    Core.TermUnion v0 ->  
      let injTypeName = (Core.injectionTypeName v0)
      in  
        let injField = (Core.injectionField v0)
        in  
          let injFieldName = (Core.fieldName injField)
          in  
            let injFieldTerm = (Core.fieldTerm injField)
            in  
              let typeId = (Syntax.unIdentifier (Utils.nameToJavaName aliases injTypeName))
              in  
                let consId = (Syntax.Identifier (Strings.cat [
                        typeId,
                        ".",
                        (Utils.sanitizeJavaName (Formatting.capitalize (Core.unName injFieldName)))]))
                in (Eithers.bind (isFieldUnitType injTypeName injFieldName cx g) (\fieldIsUnit -> Eithers.bind (Logic.ifElse (Logic.or (Schemas.isUnitTerm (Rewriting.deannotateTerm injFieldTerm)) fieldIsUnit) (Right []) (Eithers.bind (encode injFieldTerm) (\ex -> Right [
                  ex]))) (\args -> Right (Utils.javaConstructorCall (Utils.javaConstructorName consId Nothing) args Nothing))))
    Core.TermVariable v0 -> (encodeVariable env v0 cx g)
    Core.TermUnit -> (Right (Utils.javaLiteralToJavaExpression Syntax.LiteralNull))
    Core.TermWrap v0 -> (Eithers.bind (encode (Core.wrappedTermBody v0)) (\jarg -> Right (Utils.javaConstructorCall (Utils.javaConstructorName (Utils.nameToJavaName aliases (Core.wrappedTermTypeName v0)) Nothing) [
      jarg] Nothing)))
    Core.TermTypeApplication v0 ->  
      let atyp = (Core.typeApplicationTermType v0)
      in  
        let body = (Core.typeApplicationTermBody v0)
        in (Eithers.bind (encodeType aliases Sets.empty atyp cx g) (\jatyp ->  
          let combinedAnns = (Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns)
          in (Eithers.bind (Eithers.bimap (\_de -> Context.InContext {
            Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unDecodingError _de))),
            Context.inContextContext = cx}) (\_a -> _a) (Annotations.getType g combinedAnns)) (\mtyp -> Eithers.bind (Maybes.cases mtyp (CoderUtils.typeOfTerm cx g term) (\t -> Right t)) (\typ ->  
            let collected0 = (collectTypeApps0 body [
                    atyp])
            in  
              let innermostBody0 = (Pairs.first collected0)
              in  
                let allTypeArgs0 = (Pairs.second collected0)
                in (Eithers.bind (correctCastType innermostBody0 allTypeArgs0 typ cx g) (\correctedTyp ->  
                  let collected = (collectTypeApps body [
                          atyp])
                  in  
                    let innermostBody = (Pairs.first collected)
                    in  
                      let allTypeArgs = (Pairs.second collected)
                      in ((\x -> case x of
                        Core.TermVariable v1 -> (Eithers.bind (classifyDataReference v1 cx g) (\cls -> typeAppNullaryOrHoisted env aliases anns tyapps jatyp body correctedTyp v1 cls allTypeArgs cx g))
                        Core.TermEither v1 -> (Logic.ifElse (Equality.equal (Lists.length allTypeArgs) 2) ( 
                          let eitherBranchTypes = (Lists.head allTypeArgs, (Lists.head (Lists.tail allTypeArgs)))
                          in (Eithers.bind (Eithers.mapList (\t -> Eithers.bind (encodeType aliases Sets.empty t cx g) (\jt -> Utils.javaTypeToJavaReferenceType jt cx)) allTypeArgs) (\jTypeArgs ->  
                            let eitherTargs = (Lists.map (\rt -> Syntax.TypeArgumentReference rt) jTypeArgs)
                            in  
                              let encodeEitherBranch = (\branchType -> \t1 ->  
                                      let annotated = (Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ branchType)) t1)
                                      in (encodeTermInternal env anns [] annotated cx g))
                              in (Eithers.either (\term1 -> Eithers.bind (encodeEitherBranch (Pairs.first eitherBranchTypes) term1) (\expr -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs (Syntax.Identifier "hydra.util.Either") (Syntax.Identifier "left") eitherTargs [
                                expr])))) (\term1 -> Eithers.bind (encodeEitherBranch (Pairs.second eitherBranchTypes) term1) (\expr -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs (Syntax.Identifier "hydra.util.Either") (Syntax.Identifier "right") eitherTargs [
                                expr])))) v1)))) (typeAppFallbackCast env aliases anns tyapps jatyp body correctedTyp cx g))
                        _ -> (typeAppFallbackCast env aliases anns tyapps jatyp body correctedTyp cx g)) innermostBody))))))))
    _ -> (Right (encodeLiteral (Core.LiteralString "Unimplemented term variant")))) term)

annotateLambdaArgs :: (Core.Name -> [Core.Type] -> [Core.Term] -> t0 -> Graph.Graph -> Either t1 [Core.Term])
annotateLambdaArgs cname tApps argTerms cx g = (Logic.ifElse (Lists.null tApps) (Right argTerms) (Eithers.bind (Eithers.bind (Right (Lexical.dereferenceElement g cname)) (\mel -> Maybes.cases mel (Right (Maybes.map (\prim -> Graph.primitiveType prim) (Maps.lookup cname (Graph.graphPrimitives g)))) (\el -> Right (Core.bindingType el)))) (\mts -> Maybes.cases mts (Right argTerms) (\ts ->  
  let schemeType = (Core.typeSchemeType ts)
  in  
    let schemeTypeVars = (collectTypeVars schemeType)
    in  
      let schemeVars = (Lists.filter (\v -> Sets.member v schemeTypeVars) (Core.typeSchemeVariables ts))
      in (Logic.ifElse (Logic.or (Lists.null schemeVars) (Logic.not (Equality.equal (Lists.length schemeVars) (Lists.length tApps)))) (Right argTerms) ( 
        let subst = (Maps.fromList (Lists.zip schemeVars tApps))
        in  
          let expectedTypes = (peelExpectedTypes subst (Lists.length argTerms) schemeType)
          in (Right (Lists.zipWith (\arg -> \mExpected -> propagateType mExpected arg) argTerms (Lists.concat2 expectedTypes (Lists.replicate (Lists.length argTerms) (Core.TypeVariable (Core.Name "unused"))))))))))))

applyJavaArg :: (Syntax.Expression -> Syntax.Expression -> Syntax.Expression)
applyJavaArg expr jarg = (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocation (Just (Right (Utils.javaExpressionToJavaPrimary expr))) (Syntax.Identifier Names.applyMethodName) [
  jarg]))

encodeApplication :: (Helpers.JavaEnvironment -> Core.Application -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.Expression)
encodeApplication env app cx g0 =  
  let aliases = (Helpers.javaEnvironmentAliases env)
  in  
    let g = (Helpers.javaEnvironmentGraph env)
    in  
      let gathered = (CoderUtils.gatherArgsWithTypeApps (Core.TermApplication app) [] [])
      in  
        let fun = (Pairs.first gathered)
        in  
          let args = (Pairs.first (Pairs.second gathered))
          in  
            let typeApps = (Pairs.second (Pairs.second gathered))
            in (Eithers.bind (Eithers.bimap (\_de -> Context.InContext {
              Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unDecodingError _de))),
              Context.inContextContext = cx}) (\_a -> _a) (Annotations.getType g (Annotations.termAnnotationInternal fun))) (\mfunTyp -> Eithers.bind (Maybes.cases mfunTyp (CoderUtils.typeOfTerm cx g fun) (\t -> Right t)) (\funTyp ->  
              let arity = (Arity.typeArity funTyp)
              in  
                let deannotatedFun = (Rewriting.deannotateTerm fun)
                in  
                  let calleeName = ((\x -> case x of
                          Core.TermFunction v0 -> ((\x -> case x of
                            Core.FunctionPrimitive v1 -> (Just v1)
                            _ -> Nothing) v0)
                          Core.TermVariable v0 -> (Just v0)
                          _ -> Nothing) deannotatedFun)
                  in (Eithers.bind (Maybes.cases calleeName (Right args) (\cname -> annotateLambdaArgs cname typeApps args cx g)) (\annotatedArgs -> (\x -> case x of
                    Core.TermFunction v0 -> ((\x -> case x of
                      Core.FunctionPrimitive v1 ->  
                        let hargs = (Lists.take arity annotatedArgs)
                        in  
                          let rargs = (Lists.drop arity annotatedArgs)
                          in (Eithers.bind (functionCall env True v1 hargs [] cx g) (\initialCall -> Eithers.foldl (\acc -> \h -> Eithers.bind (encodeTerm env h cx g) (\jarg -> Right (applyJavaArg acc jarg))) initialCall rargs))
                      _ -> (encodeApplication_fallback env aliases g typeApps (Core.applicationFunction app) (Core.applicationArgument app) cx g)) v0)
                    Core.TermVariable v0 -> (Logic.ifElse (Logic.and (isRecursiveVariable aliases v0) (Logic.not (isLambdaBoundIn v0 (Helpers.aliasesLambdaVars aliases)))) (encodeApplication_fallback env aliases g typeApps (Core.applicationFunction app) (Core.applicationArgument app) cx g) (Eithers.bind (classifyDataReference v0 cx g) (\symClass ->  
                      let methodArity = ((\x -> case x of
                              Helpers.JavaSymbolClassHoistedLambda v1 -> v1
                              _ -> arity) symClass)
                      in  
                        let hargs = (Lists.take methodArity annotatedArgs)
                        in  
                          let rargs = (Lists.drop methodArity annotatedArgs)
                          in  
                            let trusted = (Helpers.aliasesTrustedTypeVars aliases)
                            in  
                              let inScope = (Helpers.aliasesInScopeTypeParams aliases)
                              in  
                                let filteredTypeApps = (Logic.ifElse (Logic.or (Sets.null trusted) (Sets.null inScope)) [] ( 
                                        let allVars = (Sets.unions (Lists.map (\t -> collectTypeVars t) typeApps))
                                        in (Logic.ifElse (Logic.not (Sets.null (Sets.difference allVars inScope))) [] (Logic.ifElse (Sets.null (Sets.difference allVars trusted)) typeApps []))))
                                in (Eithers.bind (Logic.ifElse (Lists.null filteredTypeApps) (Right []) (correctTypeApps g v0 hargs filteredTypeApps cx g)) (\safeTypeApps -> Eithers.bind (filterPhantomTypeArgs v0 safeTypeApps cx g) (\finalTypeApps -> Eithers.bind (functionCall env False v0 hargs finalTypeApps cx g) (\initialCall -> Eithers.foldl (\acc -> \h -> Eithers.bind (encodeTerm env h cx g) (\jarg -> Right (applyJavaArg acc jarg))) initialCall rargs)))))))
                    _ -> (encodeApplication_fallback env aliases g typeApps (Core.applicationFunction app) (Core.applicationArgument app) cx g)) deannotatedFun)))))

encodeApplication_fallback :: (Helpers.JavaEnvironment -> Helpers.Aliases -> Graph.Graph -> [Core.Type] -> Core.Term -> Core.Term -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.Expression)
encodeApplication_fallback env aliases gr typeApps lhs rhs cx g = (Eithers.bind (Eithers.bimap (\_de -> Context.InContext {
  Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unDecodingError _de))),
  Context.inContextContext = cx}) (\_a -> _a) (Annotations.getType g (Annotations.termAnnotationInternal lhs))) (\mt -> Eithers.bind (Maybes.cases mt (CoderUtils.typeOfTerm cx g lhs) (\typ -> Right typ)) (\t -> (\x -> case x of
  Core.TypeFunction v0 ->  
    let dom = (Core.functionTypeDomain v0)
    in  
      let cod = (Core.functionTypeCodomain v0)
      in ((\x -> case x of
        Core.TermFunction v1 -> ((\x -> case x of
          Core.FunctionElimination v2 -> (Eithers.bind (encodeTerm env rhs cx g) (\jarg -> Eithers.bind (Logic.ifElse (Logic.not (Lists.null (javaTypeArgumentsForType dom))) (Right dom) (Eithers.bind (Eithers.bimap (\_de -> Context.InContext {
            Context.inContextObject = (Error.ErrorOther (Error.OtherError (Error.unDecodingError _de))),
            Context.inContextContext = cx}) (\_a -> _a) (Annotations.getType g (Annotations.termAnnotationInternal rhs))) (\mrt -> Maybes.cases mrt (Eithers.bind (CoderUtils.typeOfTerm cx g rhs) (\rt -> Right (Logic.ifElse (Logic.not (Lists.null (javaTypeArgumentsForType rt))) rt dom))) (\rt -> Right (Logic.ifElse (Logic.not (Lists.null (javaTypeArgumentsForType rt))) rt dom))))) (\enrichedDom -> encodeElimination env (Just jarg) enrichedDom cod v2 cx g)))
          _ -> (Eithers.bind (encodeTerm env lhs cx g) (\jfun -> Eithers.bind (encodeTerm env rhs cx g) (\jarg -> Right (applyJavaArg jfun jarg))))) v1)
        _ -> (Eithers.bind (encodeTerm env lhs cx g) (\jfun -> Eithers.bind (encodeTerm env rhs cx g) (\jarg -> Right (applyJavaArg jfun jarg))))) (Rewriting.deannotateTerm lhs))
  _ -> (Eithers.bind (encodeTerm env lhs cx g) (\jfun -> Eithers.bind (encodeTerm env rhs cx g) (\jarg -> Right (applyJavaArg jfun jarg))))) (Rewriting.deannotateTypeParameters (Rewriting.deannotateType t)))))

functionCall :: (Helpers.JavaEnvironment -> Bool -> Core.Name -> [Core.Term] -> [Core.Type] -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.Expression)
functionCall env isPrim name args typeApps cx g =  
  let aliases = (Helpers.javaEnvironmentAliases env)
  in  
    let isLambdaBound = (isLambdaBoundIn name (Helpers.aliasesLambdaVars aliases))
    in (Eithers.bind (Eithers.mapList (\arg -> encodeTerm env arg cx g) args) (\jargs0 ->  
      let wrapResult = (wrapLazyArguments name jargs0)
      in  
        let jargs = (Pairs.first wrapResult)
        in  
          let mMethodOverride = (Pairs.second wrapResult)
          in (Logic.ifElse (Logic.or (isLocalVariable name) isLambdaBound) (Eithers.bind (encodeVariable env name cx g) (\baseExpr -> Right (Lists.foldl (\acc -> \jarg -> applyJavaArg acc jarg) baseExpr jargs))) ( 
            let overrideMethodName = (\jid -> Maybes.cases mMethodOverride jid (\m ->  
                    let s = (Syntax.unIdentifier jid)
                    in (Syntax.Identifier (Strings.cat2 (Strings.fromList (Lists.take (Math.sub (Strings.length s) (Strings.length Names.applyMethodName)) (Strings.toList s))) m))))
            in (Logic.ifElse (Lists.null typeApps) ( 
              let header = (Syntax.MethodInvocation_HeaderSimple (Syntax.MethodName (overrideMethodName (elementJavaIdentifier isPrim False aliases name))))
              in (Right (Utils.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
                Syntax.methodInvocationHeader = header,
                Syntax.methodInvocationArguments = jargs})))) ( 
              let qn = (Names_.qualifyName name)
              in  
                let mns = (Module.qualifiedNameNamespace qn)
                in  
                  let localName = (Module.qualifiedNameLocal qn)
                  in (Maybes.cases mns ( 
                    let header = (Syntax.MethodInvocation_HeaderSimple (Syntax.MethodName (overrideMethodName (elementJavaIdentifier isPrim False aliases name))))
                    in (Right (Utils.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
                      Syntax.methodInvocationHeader = header,
                      Syntax.methodInvocationArguments = jargs})))) (\ns_ ->  
                    let classId = (Utils.nameToJavaName aliases (Names_.unqualifyName (Module.QualifiedName {
                            Module.qualifiedNameNamespace = (Just ns_),
                            Module.qualifiedNameLocal = (elementsClassName ns_)})))
                    in  
                      let methodId = (Logic.ifElse isPrim (overrideMethodName (Syntax.Identifier (Strings.cat2 (Syntax.unIdentifier (Utils.nameToJavaName aliases (Names_.unqualifyName (Module.QualifiedName {
                              Module.qualifiedNameNamespace = (Just ns_),
                              Module.qualifiedNameLocal = (Formatting.capitalize localName)})))) (Strings.cat2 "." Names.applyMethodName)))) (Syntax.Identifier (Utils.sanitizeJavaName localName)))
                      in (Eithers.bind (Eithers.mapList (\t -> Eithers.bind (encodeType aliases Sets.empty t cx g) (\jt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jt cx) (\rt -> Right (Syntax.TypeArgumentReference rt)))) typeApps) (\jTypeArgs -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs classId methodId jTypeArgs jargs))))))))))))

buildCurriedLambda :: ([Core.Name] -> Syntax.Expression -> Syntax.Expression)
buildCurriedLambda params inner = (Lists.foldl (\acc -> \p -> Utils.javaLambda p acc) inner (Lists.reverse params))

encodeFunction :: (Helpers.JavaEnvironment -> Core.Type -> Core.Type -> Core.Function -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.Expression)
encodeFunction env dom cod fun cx g =  
  let aliases = (Helpers.javaEnvironmentAliases env)
  in ((\x -> case x of
    Core.FunctionElimination v0 -> (encodeElimination env Nothing dom cod v0 cx g)
    Core.FunctionLambda v0 -> (withLambda env v0 (\env2 ->  
      let lambdaVar = (Core.lambdaParameter v0)
      in  
        let body = (Core.lambdaBody v0)
        in ((\x -> case x of
          Core.TermFunction v1 -> ((\x -> case x of
            Core.FunctionLambda v2 -> ((\x -> case x of
              Core.TypeFunction v3 ->  
                let dom2 = (Core.functionTypeDomain v3)
                in  
                  let cod2 = (Core.functionTypeCodomain v3)
                  in (Eithers.bind (encodeFunction env2 dom2 cod2 (Core.FunctionLambda v2) cx g) (\innerJavaLambda ->  
                    let lam1 = (Utils.javaLambda lambdaVar innerJavaLambda)
                    in (applyCastIfSafe aliases (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = dom,
                      Core.functionTypeCodomain = cod})) lam1 cx g)))
              _ -> (Left (Context.InContext {
                Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "expected function type for lambda body, but got: " (Core___.type_ cod)))),
                Context.inContextContext = cx}))) (Rewriting.deannotateType cod))
            _ -> (Eithers.bind (analyzeJavaFunction env2 body cx g) (\fs ->  
              let bindings = (Typing.functionStructureBindings fs)
              in  
                let innerBody = (Typing.functionStructureBody fs)
                in  
                  let env3 = (Typing.functionStructureEnvironment fs)
                  in (Eithers.bind (bindingsToStatements env3 bindings cx g) (\bindResult ->  
                    let bindingStmts = (Pairs.first bindResult)
                    in  
                      let env4 = (Pairs.second bindResult)
                      in (Eithers.bind (encodeTerm env4 innerBody cx g) (\jbody ->  
                        let lam1 = (Logic.ifElse (Lists.null bindings) (Utils.javaLambda lambdaVar jbody) ( 
                                let returnSt = (Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just jbody)))
                                in (Utils.javaLambdaFromBlock lambdaVar (Syntax.Block (Lists.concat2 bindingStmts [
                                  returnSt])))))
                        in (applyCastIfSafe aliases (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = dom,
                          Core.functionTypeCodomain = cod})) lam1 cx g)))))))) v1)
          _ -> (Eithers.bind (analyzeJavaFunction env2 body cx g) (\fs ->  
            let bindings = (Typing.functionStructureBindings fs)
            in  
              let innerBody = (Typing.functionStructureBody fs)
              in  
                let env3 = (Typing.functionStructureEnvironment fs)
                in (Eithers.bind (bindingsToStatements env3 bindings cx g) (\bindResult ->  
                  let bindingStmts = (Pairs.first bindResult)
                  in  
                    let env4 = (Pairs.second bindResult)
                    in (Eithers.bind (encodeTerm env4 innerBody cx g) (\jbody ->  
                      let lam1 = (Logic.ifElse (Lists.null bindings) (Utils.javaLambda lambdaVar jbody) ( 
                              let returnSt = (Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just jbody)))
                              in (Utils.javaLambdaFromBlock lambdaVar (Syntax.Block (Lists.concat2 bindingStmts [
                                returnSt])))))
                      in (applyCastIfSafe aliases (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = dom,
                        Core.functionTypeCodomain = cod})) lam1 cx g)))))))) (Rewriting.deannotateTerm body))))
    Core.FunctionPrimitive v0 ->  
      let classWithApply = (Syntax.unIdentifier (elementJavaIdentifier True False aliases v0))
      in  
        let suffix = (Strings.cat2 "." Names.applyMethodName)
        in  
          let className = (Strings.fromList (Lists.take (Math.sub (Strings.length classWithApply) (Strings.length suffix)) (Strings.toList classWithApply)))
          in  
            let arity = (Arity.typeArity (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = dom,
                    Core.functionTypeCodomain = cod})))
            in (Logic.ifElse (Equality.lte arity 1) (Right (Utils.javaIdentifierToJavaExpression (Syntax.Identifier (Strings.cat [
              className,
              "::",
              Names.applyMethodName])))) ( 
              let paramNames = (Lists.map (\i -> Core.Name (Strings.cat2 "p" (Literals.showInt32 i))) (Math.range 0 (Math.sub arity 1)))
              in  
                let paramExprs = (Lists.map (\p -> Utils.javaIdentifierToJavaExpression (Utils.variableToJavaIdentifier p)) paramNames)
                in  
                  let classId = (Syntax.Identifier className)
                  in  
                    let call = (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStatic classId (Syntax.Identifier Names.applyMethodName) paramExprs))
                    in  
                      let curried = (buildCurriedLambda paramNames call)
                      in (Eithers.bind (encodeType aliases Sets.empty (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = dom,
                        Core.functionTypeCodomain = cod})) cx g) (\jtype -> Eithers.bind (Utils.javaTypeToJavaReferenceType jtype cx) (\rt -> Right (Utils.javaCastExpressionToJavaExpression (Utils.javaCastExpression rt (Utils.javaExpressionToJavaUnaryExpression curried))))))))
    _ -> (Right (encodeLiteral (Core.LiteralString (Strings.cat2 "Unimplemented function variant: " (Core___.function fun)))))) fun)

extractArgType :: (t0 -> Core.Type -> Core.Type)
extractArgType _lhs typ = ((\x -> case x of
  Core.TypeApplication v0 -> ((\x -> case x of
    Core.TypeApplication _ -> (Core.applicationTypeArgument v0)
    _ -> typ) (Core.applicationTypeFunction v0))
  _ -> typ) typ)

annotateBodyWithCod :: (Core.Type -> Core.Term -> Core.Term)
annotateBodyWithCod typ term =  
  let setAnn = (\t -> Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ typ)) t)
  in ((\x -> case x of
    Core.TermTypeApplication _ -> (setAnn term)
    Core.TermApplication v0 ->  
      let lhs = (Core.applicationFunction v0)
      in  
        let rhs = (Core.applicationArgument v0)
        in  
          let annotatedRhs = ((\x -> case x of
                  Core.TermTypeApplication _ -> (annotateBodyWithCod (extractArgType lhs typ) rhs)
                  _ -> rhs) (Rewriting.deannotateTerm rhs))
          in (setAnn (Core.TermApplication (Core.Application {
            Core.applicationFunction = lhs,
            Core.applicationArgument = annotatedRhs})))
    _ -> (setAnn term)) (Rewriting.deannotateTerm term))

domTypeArgs :: (Helpers.Aliases -> Core.Type -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) [Syntax.TypeArgument])
domTypeArgs aliases d cx g =  
  let args = (extractTypeApplicationArgs (Rewriting.deannotateType d))
  in (Logic.ifElse (Logic.not (Lists.null args)) (Eithers.mapList (\t -> Eithers.bind (encodeType aliases Sets.empty t cx g) (\jt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jt cx) (\rt -> Right (Syntax.TypeArgumentReference rt)))) args) (Right (javaTypeArgumentsForType d)))

otherwiseBranch :: (Helpers.JavaEnvironment -> Helpers.Aliases -> Core.Type -> Core.Type -> Core.Name -> Syntax.Type -> [Syntax.TypeArgument] -> Core.Term -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.ClassBodyDeclarationWithComments)
otherwiseBranch env aliases dom cod tname jcod targs d cx g =  
  let jdom = (Syntax.TypeReference (Utils.nameToJavaReferenceType aliases True targs tname Nothing))
  in  
    let mods = [
            Syntax.MethodModifierPublic]
    in  
      let anns = [
              Utils.overrideAnnotation]
      in  
        let param = (Utils.javaTypeToJavaFormalParameter jdom (Core.Name "instance"))
        in  
          let result = (Syntax.ResultType (Syntax.UnannType jcod))
          in (Eithers.bind (analyzeJavaFunction env d cx g) (\fs ->  
            let bindings = (Typing.functionStructureBindings fs)
            in  
              let rawBody = (Typing.functionStructureBody fs)
              in  
                let innerBody = (annotateBodyWithCod cod rawBody)
                in  
                  let env2 = (Typing.functionStructureEnvironment fs)
                  in (Eithers.bind (bindingsToStatements env2 bindings cx g) (\bindResult ->  
                    let bindingStmts = (Pairs.first bindResult)
                    in  
                      let env3 = (Pairs.second bindResult)
                      in (Eithers.bind (encodeTerm env3 innerBody cx g) (\jret ->  
                        let returnStmt = (Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just jret)))
                        in  
                          let allStmts = (Lists.concat2 bindingStmts [
                                  returnStmt])
                          in (Right (noComment (Utils.methodDeclaration mods [] anns Names.otherwiseMethodName [
                            param] result (Just allStmts))))))))))

visitBranch :: (Helpers.JavaEnvironment -> Helpers.Aliases -> Core.Type -> Core.Name -> Syntax.Type -> [Syntax.TypeArgument] -> Core.Field -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.ClassBodyDeclarationWithComments)
visitBranch env aliases dom tname jcod targs field cx g =  
  let jdom = (Syntax.TypeReference (Utils.nameToJavaReferenceType aliases True targs tname (Just (Formatting.capitalize (Core.unName (Core.fieldName field))))))
  in  
    let mods = [
            Syntax.MethodModifierPublic]
    in  
      let anns = [
              Utils.overrideAnnotation]
      in  
        let result = (Syntax.ResultType (Syntax.UnannType jcod))
        in ((\x -> case x of
          Core.TermFunction v0 -> ((\x -> case x of
            Core.FunctionLambda v1 -> (withLambda env v1 (\env2 ->  
              let lambdaParam = (Core.lambdaParameter v1)
              in  
                let body = (Core.lambdaBody v1)
                in  
                  let env3 = (insertBranchVar lambdaParam env2)
                  in (Eithers.bind (analyzeJavaFunction env3 body cx g) (\fs ->  
                    let bindings = (Typing.functionStructureBindings fs)
                    in  
                      let innerBody = (Typing.functionStructureBody fs)
                      in  
                        let env4 = (Typing.functionStructureEnvironment fs)
                        in (Eithers.bind (bindingsToStatements env4 bindings cx g) (\bindResult ->  
                          let bindingStmts = (Pairs.first bindResult)
                          in  
                            let env5 = (Pairs.second bindResult)
                            in (Eithers.bind (encodeTerm env5 innerBody cx g) (\jret ->  
                              let param = (Utils.javaTypeToJavaFormalParameter jdom lambdaParam)
                              in  
                                let returnStmt = (Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just jret)))
                                in  
                                  let allStmts = (Lists.concat2 bindingStmts [
                                          returnStmt])
                                  in (Right (noComment (Utils.methodDeclaration mods [] anns Names.visitMethodName [
                                    param] result (Just allStmts))))))))))))
            _ -> (Left (Context.InContext {
              Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "visitBranch: field term is not a lambda: " (Core___.term (Core.fieldTerm field))))),
              Context.inContextContext = cx}))) v0)
          _ -> (Left (Context.InContext {
            Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "visitBranch: field term is not a lambda: " (Core___.term (Core.fieldTerm field))))),
            Context.inContextContext = cx}))) (Rewriting.deannotateTerm (Core.fieldTerm field)))

encodeElimination :: (Helpers.JavaEnvironment -> Maybe Syntax.Expression -> Core.Type -> Core.Type -> Core.Elimination -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.Expression)
encodeElimination env marg dom cod elm cx g =  
  let aliases = (Helpers.javaEnvironmentAliases env)
  in ((\x -> case x of
    Core.EliminationRecord v0 ->  
      let fname = (Core.projectionField v0)
      in (Eithers.bind (encodeType aliases Sets.empty dom cx g) (\jdom0 -> Eithers.bind (Utils.javaTypeToJavaReferenceType jdom0 cx) (\jdomr -> Maybes.cases marg ( 
        let projVar = (Core.Name "projected")
        in  
          let jbody = (Utils.javaExpressionNameToJavaExpression (Utils.fieldExpression (Utils.variableToJavaIdentifier projVar) (Utils.javaIdentifier (Core.unName fname))))
          in (Right (Utils.javaLambda projVar jbody))) (\jarg ->  
        let qual = (Syntax.FieldAccess_QualifierPrimary (Utils.javaExpressionToJavaPrimary jarg))
        in (Right (Utils.javaFieldAccessToJavaExpression (Syntax.FieldAccess {
          Syntax.fieldAccessQualifier = qual,
          Syntax.fieldAccessIdentifier = (Utils.javaIdentifier (Core.unName fname))})))))))
    Core.EliminationUnion v0 ->  
      let tname = (Core.caseStatementTypeName v0)
      in  
        let def_ = (Core.caseStatementDefault v0)
        in  
          let fields = (Core.caseStatementCases v0)
          in (Maybes.cases marg ( 
            let uVar = (Core.Name "u")
            in  
              let typedLambda = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = uVar,
                      Core.lambdaDomain = (Just dom),
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination elm)),
                        Core.applicationArgument = (Core.TermVariable uVar)}))})))
              in (encodeTerm env typedLambda cx g)) (\jarg ->  
            let prim = (Utils.javaExpressionToJavaPrimary jarg)
            in  
              let consId = (innerClassRef aliases tname Names.partialVisitorName)
              in (Eithers.bind (encodeType aliases Sets.empty cod cx g) (\jcod -> Eithers.bind (Utils.javaTypeToJavaReferenceType jcod cx) (\rt -> Eithers.bind (domTypeArgs aliases dom cx g) (\domArgs ->  
                let targs = (typeArgsOrDiamond (Lists.concat2 domArgs [
                        Syntax.TypeArgumentReference rt]))
                in (Eithers.bind (Maybes.cases def_ (Right []) (\d -> Eithers.bind (otherwiseBranch env aliases dom cod tname jcod domArgs d cx g) (\b -> Right [
                  b]))) (\otherwiseBranches -> Eithers.bind (Eithers.mapList (\f -> visitBranch env aliases dom tname jcod domArgs f cx g) fields) (\visitBranches ->  
                  let body = (Syntax.ClassBody (Lists.concat2 otherwiseBranches visitBranches))
                  in  
                    let visitor = (Utils.javaConstructorCall (Utils.javaConstructorName consId (Just targs)) [] (Just body))
                    in (Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocation (Just (Right prim)) (Syntax.Identifier Names.acceptMethodName) [
                      visitor]))))))))))))
    Core.EliminationWrap _ ->  
      let withArg = (\ja -> Utils.javaFieldAccessToJavaExpression (Syntax.FieldAccess {
              Syntax.fieldAccessQualifier = (Syntax.FieldAccess_QualifierPrimary (Utils.javaExpressionToJavaPrimary ja)),
              Syntax.fieldAccessIdentifier = (Utils.javaIdentifier Names.valueFieldName)}))
      in (Right (Maybes.cases marg ( 
        let wVar = (Core.Name "wrapped")
        in  
          let wArg = (Utils.javaIdentifierToJavaExpression (Utils.variableToJavaIdentifier wVar))
          in (Utils.javaLambda wVar (withArg wArg))) (\jarg -> withArg jarg)))
    _ -> (Left (Context.InContext {
      Context.inContextObject = (Error.ErrorOther (Error.OtherError (Strings.cat2 "unexpected " (Strings.cat2 "elimination case" (Strings.cat2 " in " "encodeElimination"))))),
      Context.inContextContext = cx}))) elm)

toDeclInit :: (Helpers.Aliases -> Graph.Graph -> S.Set Core.Name -> [Core.Binding] -> Core.Name -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) (Maybe Syntax.BlockStatement))
toDeclInit aliasesExt gExt recursiveVars flatBindings name cx g = (Logic.ifElse (Sets.member name recursiveVars) ( 
  let binding = (Lists.head (Lists.filter (\b -> Equality.equal (Core.bindingName b) name) flatBindings))
  in  
    let value = (Core.bindingTerm binding)
    in (Eithers.bind (Maybes.cases (Core.bindingType binding) (CoderUtils.typeOfTerm cx gExt value) (\ts -> Right (Core.typeSchemeType ts))) (\typ -> Eithers.bind (encodeType aliasesExt Sets.empty typ cx g) (\jtype ->  
      let id = (Utils.variableToJavaIdentifier name)
      in  
        let arid = (Syntax.Identifier "java.util.concurrent.atomic.AtomicReference")
        in  
          let aid = Syntax.AnnotatedIdentifier {
                  Syntax.annotatedIdentifierAnnotations = [],
                  Syntax.annotatedIdentifierIdentifier = arid}
          in (Eithers.bind (Utils.javaTypeToJavaReferenceType jtype cx) (\rt ->  
            let targs = (typeArgsOrDiamond [
                    Syntax.TypeArgumentReference rt])
            in  
              let ci = Syntax.ClassOrInterfaceTypeToInstantiate {
                      Syntax.classOrInterfaceTypeToInstantiateIdentifiers = [
                        aid],
                      Syntax.classOrInterfaceTypeToInstantiateTypeArguments = (Just targs)}
              in  
                let body = (Utils.javaConstructorCall ci [] Nothing)
                in  
                  let pkg = (Names.javaPackageName [
                          "java",
                          "util",
                          "concurrent",
                          "atomic"])
                  in  
                    let artype = (Utils.javaRefType [
                            rt] (Just pkg) "AtomicReference")
                    in (Right (Just (Utils.variableDeclarationStatement aliasesExt artype id body))))))))) (Right Nothing))

toDeclStatement :: (Helpers.JavaEnvironment -> Helpers.Aliases -> Graph.Graph -> S.Set Core.Name -> S.Set Core.Name -> [Core.Binding] -> Core.Name -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.BlockStatement)
toDeclStatement envExt aliasesExt gExt recursiveVars thunkedVars flatBindings name cx g =  
  let binding = (Lists.head (Lists.filter (\b -> Equality.equal (Core.bindingName b) name) flatBindings))
  in  
    let value = (Core.bindingTerm binding)
    in (Eithers.bind (Maybes.cases (Core.bindingType binding) (CoderUtils.typeOfTerm cx gExt value) (\ts -> Right (Core.typeSchemeType ts))) (\typ -> Eithers.bind (encodeType aliasesExt Sets.empty typ cx g) (\jtype ->  
      let id = (Utils.variableToJavaIdentifier name)
      in  
        let annotatedValue = (Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ typ)) value)
        in (Eithers.bind (encodeTerm envExt annotatedValue cx g) (\rhs -> Logic.ifElse (Sets.member name recursiveVars) (Right (Syntax.BlockStatementStatement (Utils.javaMethodInvocationToJavaStatement (Utils.methodInvocation (Just (Left (Syntax.ExpressionName {
          Syntax.expressionNameQualifier = Nothing,
          Syntax.expressionNameIdentifier = id}))) (Syntax.Identifier Names.setMethodName) [
          rhs])))) (Logic.ifElse (Sets.member name thunkedVars) (Eithers.bind (Utils.javaTypeToJavaReferenceType jtype cx) (\rt ->  
          let lazyType = (Utils.javaRefType [
                  rt] Names.hydraUtilPackageName "Lazy")
          in  
            let lambdaBody = (Syntax.LambdaBodyExpression rhs)
            in  
              let supplierLambda = (Syntax.ExpressionLambda (Syntax.LambdaExpression {
                      Syntax.lambdaExpressionParameters = (Syntax.LambdaParametersTuple []),
                      Syntax.lambdaExpressionBody = lambdaBody}))
              in  
                let targs = (typeArgsOrDiamond [
                        Syntax.TypeArgumentReference rt])
                in  
                  let lazyExpr = (Utils.javaConstructorCall (Utils.javaConstructorName (Syntax.Identifier "hydra.util.Lazy") (Just targs)) [
                          supplierLambda] Nothing)
                  in (Right (Utils.variableDeclarationStatement aliasesExt lazyType id lazyExpr)))) (Right (Utils.variableDeclarationStatement aliasesExt jtype id rhs))))))))

bindingsToStatements :: (Helpers.JavaEnvironment -> [Core.Binding] -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) ([Syntax.BlockStatement], Helpers.JavaEnvironment))
bindingsToStatements env bindings cx g0 =  
  let aliases = (Helpers.javaEnvironmentAliases env)
  in  
    let g = (Helpers.javaEnvironmentGraph env)
    in  
      let flatBindings = (dedupBindings (Helpers.aliasesInScopeJavaVars aliases) (flattenBindings bindings))
      in  
        let gExtended = (Schemas.extendGraphForLet CoderUtils.bindingMetadata g (Core.Let {
                Core.letBindings = flatBindings,
                Core.letBody = (Core.TermVariable (Core.Name "dummy"))}))
        in  
          let bindingVars = (Sets.fromList (Lists.map (\b -> Core.bindingName b) flatBindings))
          in  
            let allDeps = (Maps.fromList (Lists.map (\b ->  
                    let key = (Core.bindingName b)
                    in  
                      let deps = (Sets.intersection bindingVars (Rewriting.freeVariablesInTerm (Core.bindingTerm b)))
                      in (key, deps)) flatBindings))
            in  
              let sorted = (Sorting.topologicalSortComponents (Lists.map (\entry ->  
                      let key = (Pairs.first entry)
                      in  
                        let deps = (Pairs.second entry)
                        in (key, (Sets.toList deps))) (Maps.toList allDeps)))
              in  
                let recursiveVars = (Sets.fromList (Lists.concat (Lists.map (\names -> Logic.ifElse (Equality.equal (Lists.length names) 1) ( 
                        let singleName = (Lists.head names)
                        in (Maybes.cases (Maps.lookup singleName allDeps) [] (\deps -> Logic.ifElse (Sets.member singleName deps) [
                          singleName] []))) names) sorted)))
                in  
                  let thunkedVars = (Sets.fromList (Lists.concat (Lists.map (\b ->  
                          let bname = (Core.bindingName b)
                          in (Logic.ifElse (Logic.and (Logic.not (Sets.member bname recursiveVars)) (Logic.and (needsThunking (Core.bindingTerm b)) (Logic.not (bindingIsFunctionType b)))) [
                            bname] [])) flatBindings)))
                  in  
                    let aliasesExtended = Helpers.Aliases {
                            Helpers.aliasesCurrentNamespace = (Helpers.aliasesCurrentNamespace aliases),
                            Helpers.aliasesPackages = (Helpers.aliasesPackages aliases),
                            Helpers.aliasesBranchVars = (Helpers.aliasesBranchVars aliases),
                            Helpers.aliasesRecursiveVars = (Sets.union (Helpers.aliasesRecursiveVars aliases) recursiveVars),
                            Helpers.aliasesInScopeTypeParams = (Helpers.aliasesInScopeTypeParams aliases),
                            Helpers.aliasesPolymorphicLocals = (Helpers.aliasesPolymorphicLocals aliases),
                            Helpers.aliasesInScopeJavaVars = (Sets.union (Helpers.aliasesInScopeJavaVars aliases) bindingVars),
                            Helpers.aliasesVarRenames = (Helpers.aliasesVarRenames aliases),
                            Helpers.aliasesLambdaVars = (Helpers.aliasesLambdaVars aliases),
                            Helpers.aliasesTypeVarSubst = (Helpers.aliasesTypeVarSubst aliases),
                            Helpers.aliasesTrustedTypeVars = (Helpers.aliasesTrustedTypeVars aliases),
                            Helpers.aliasesMethodCodomain = (Helpers.aliasesMethodCodomain aliases),
                            Helpers.aliasesThunkedVars = (Sets.union (Helpers.aliasesThunkedVars aliases) thunkedVars)}
                    in  
                      let envExtended = Helpers.JavaEnvironment {
                              Helpers.javaEnvironmentAliases = aliasesExtended,
                              Helpers.javaEnvironmentGraph = gExtended}
                      in (Logic.ifElse (Lists.null bindings) (Right ([], envExtended)) (Eithers.bind (Eithers.mapList (\names -> Eithers.bind (Eithers.mapList (\n -> toDeclInit aliasesExtended gExtended recursiveVars flatBindings n cx g) names) (\inits -> Eithers.bind (Eithers.mapList (\n -> toDeclStatement envExtended aliasesExtended gExtended recursiveVars thunkedVars flatBindings n cx g) names) (\decls -> Right (Lists.concat2 (Maybes.cat inits) decls)))) sorted) (\groups -> Right (Lists.concat groups, envExtended))))

toClassDecl :: (Bool -> Bool -> Helpers.Aliases -> [Syntax.TypeParameter] -> Core.Name -> Core.Type -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.ClassDeclaration)
toClassDecl isInner isSer aliases tparams elName t cx g =  
  let wrap = (\t_ -> declarationForRecordType isInner isSer aliases tparams elName [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "value"),
            Core.fieldTypeType = (Rewriting.deannotateType t_)}] cx g)
  in ((\x -> case x of
    Core.TypeRecord v0 -> (declarationForRecordType isInner isSer aliases tparams elName (Core.rowTypeFields v0) cx g)
    Core.TypeUnion v0 -> (declarationForUnionType isSer aliases tparams elName (Core.rowTypeFields v0) cx g)
    Core.TypeForall v0 ->  
      let v = (Core.forallTypeParameter v0)
      in  
        let body = (Core.forallTypeBody v0)
        in  
          let param = (Utils.javaTypeParameter (Formatting.capitalize (Core.unName v)))
          in (toClassDecl False isSer aliases (Lists.concat2 tparams [
            param]) elName body cx g)
    Core.TypeWrap v0 ->  
      let wtype = (Core.wrappedTypeBody v0)
      in (declarationForRecordType isInner isSer aliases tparams elName [
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "value"),
          Core.fieldTypeType = wtype}] cx g)
    _ -> (wrap t)) (Rewriting.deannotateType t))

declarationForUnionType :: (Bool -> Helpers.Aliases -> [Syntax.TypeParameter] -> Core.Name -> [Core.FieldType] -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.ClassDeclaration)
declarationForUnionType isSer aliases tparams elName fields cx g = (Eithers.bind (Eithers.mapList (\ft ->  
  let fname = (Core.fieldTypeName ft)
  in  
    let ftype = (Core.fieldTypeType ft)
    in  
      let rfields = (Logic.ifElse (Schemas.isUnitType (Rewriting.deannotateType ftype)) [] [
              Core.FieldType {
                Core.fieldTypeName = (Core.Name "value"),
                Core.fieldTypeType = (Rewriting.deannotateType ftype)}])
      in  
        let varName = (Utils.variantClassName False elName fname)
        in (Eithers.bind (declarationForRecordType_ True isSer aliases [] varName (Logic.ifElse isSer (Just elName) Nothing) rfields cx g) (\innerDecl -> Right (augmentVariantClass aliases tparams elName innerDecl)))) fields) (\variantClasses ->  
  let variantDecls = (Lists.map (\vc -> Syntax.ClassBodyDeclarationClassMember (Syntax.ClassMemberDeclarationClass vc)) variantClasses)
  in (Eithers.bind (Eithers.mapList (\pair -> addComment (Pairs.first pair) (Pairs.second pair) cx g) (Lists.zip variantDecls fields)) (\variantDecls_ ->  
    let privateConst = (Utils.makeConstructor aliases elName True [] [])
    in  
      let acceptDecl = (Utils.toAcceptMethod True tparams)
      in  
        let vtparams = (Lists.concat2 tparams [
                Utils.javaTypeParameter Names.visitorReturnParameter])
        in  
          let visitorMethods = (Lists.map (\ft ->  
                  let fname = (Core.fieldTypeName ft)
                  in  
                    let typeArgs = (Lists.map (\tp -> Utils.typeParameterToTypeArgument tp) tparams)
                    in  
                      let varRef = (Utils.javaClassTypeToJavaType (Utils.nameToJavaClassType aliases False typeArgs (Utils.variantClassName False elName fname) Nothing))
                      in  
                        let param = (Utils.javaTypeToJavaFormalParameter varRef (Core.Name "instance"))
                        in  
                          let resultR = (Utils.javaTypeToJavaResult (Syntax.TypeReference Utils.visitorTypeVariable))
                          in (Utils.interfaceMethodDeclaration [] [] Names.visitMethodName [
                            param] resultR Nothing)) fields)
          in  
            let visitorBody = (Syntax.InterfaceBody visitorMethods)
            in  
              let visitor = (Utils.javaInterfaceDeclarationToJavaClassBodyDeclaration (Syntax.NormalInterfaceDeclaration {
                      Syntax.normalInterfaceDeclarationModifiers = [
                        Syntax.InterfaceModifierPublic],
                      Syntax.normalInterfaceDeclarationIdentifier = (Syntax.TypeIdentifier (Syntax.Identifier Names.visitorName)),
                      Syntax.normalInterfaceDeclarationParameters = vtparams,
                      Syntax.normalInterfaceDeclarationExtends = [],
                      Syntax.normalInterfaceDeclarationBody = visitorBody}))
              in  
                let typeArgs = (Lists.map (\tp -> Utils.typeParameterToTypeArgument tp) tparams)
                in  
                  let visitorClassType = (Utils.javaClassType (Lists.concat2 (Lists.map (\tp -> Utils.typeParameterToReferenceType tp) tparams) [
                          Utils.visitorTypeVariable]) Nothing Names.visitorName)
                  in  
                    let mainInstanceParam = (Utils.javaTypeToJavaFormalParameter (Utils.javaClassTypeToJavaType (Utils.nameToJavaClassType aliases False typeArgs elName Nothing)) (Core.Name "instance"))
                    in  
                      let resultR = (Utils.javaTypeToJavaResult (Syntax.TypeReference Utils.visitorTypeVariable))
                      in  
                        let throwStmt = (Syntax.BlockStatementStatement (Utils.javaThrowIllegalStateException [
                                Utils.javaAdditiveExpressionToJavaExpression (Utils.addExpressions [
                                  Utils.javaStringMultiplicativeExpression "Non-exhaustive patterns when matching: ",
                                  (Syntax.MultiplicativeExpressionUnary (Utils.javaIdentifierToJavaUnaryExpression (Syntax.Identifier "instance")))])]))
                        in  
                          let defaultMod = [
                                  Syntax.InterfaceMethodModifierDefault]
                          in  
                            let otherwiseDecl = (Utils.interfaceMethodDeclaration defaultMod [] Names.otherwiseMethodName [
                                    mainInstanceParam] resultR (Just [
                                    throwStmt]))
                            in  
                              let pvVisitMethods = (Lists.map (\ft ->  
                                      let fname = (Core.fieldTypeName ft)
                                      in  
                                        let varRef = (Utils.javaClassTypeToJavaType (Utils.nameToJavaClassType aliases False typeArgs (Utils.variantClassName False elName fname) Nothing))
                                        in  
                                          let param = (Utils.javaTypeToJavaFormalParameter varRef (Core.Name "instance"))
                                          in  
                                            let mi = (Utils.methodInvocation Nothing (Syntax.Identifier Names.otherwiseMethodName) [
                                                    Utils.javaIdentifierToJavaExpression (Syntax.Identifier "instance")])
                                            in  
                                              let returnOtherwise = (Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just (Utils.javaPrimaryToJavaExpression (Utils.javaMethodInvocationToJavaPrimary mi)))))
                                              in (Utils.interfaceMethodDeclaration defaultMod [] Names.visitMethodName [
                                                param] resultR (Just [
                                                returnOtherwise]))) fields)
                              in  
                                let pvBody = (Syntax.InterfaceBody (Lists.concat2 [
                                        otherwiseDecl] pvVisitMethods))
                                in  
                                  let partialVisitor = (Utils.javaInterfaceDeclarationToJavaClassBodyDeclaration (Syntax.NormalInterfaceDeclaration {
                                          Syntax.normalInterfaceDeclarationModifiers = [
                                            Syntax.InterfaceModifierPublic],
                                          Syntax.normalInterfaceDeclarationIdentifier = (Syntax.TypeIdentifier (Syntax.Identifier Names.partialVisitorName)),
                                          Syntax.normalInterfaceDeclarationParameters = vtparams,
                                          Syntax.normalInterfaceDeclarationExtends = [
                                            Syntax.InterfaceType visitorClassType],
                                          Syntax.normalInterfaceDeclarationBody = pvBody}))
                                  in (Eithers.bind (constantDeclForTypeName aliases elName cx g) (\tn0 -> Eithers.bind (Eithers.mapList (\ft -> constantDeclForFieldType aliases ft cx g) fields) (\tn1 ->  
                                    let tn = (Lists.concat2 [
                                            tn0] tn1)
                                    in  
                                      let otherDecls = (Lists.map (\d -> noComment d) [
                                              privateConst,
                                              acceptDecl,
                                              visitor,
                                              partialVisitor])
                                      in  
                                        let bodyDecls = (Lists.concat [
                                                tn,
                                                otherDecls,
                                                variantDecls_])
                                        in  
                                          let mods = (Lists.concat2 classModsPublic [
                                                  Syntax.ClassModifierAbstract])
                                          in (Right (Utils.javaClassDeclaration aliases tparams elName mods Nothing (interfaceTypes isSer aliases tparams elName) bodyDecls)))))))))

augmentVariantClass :: (Helpers.Aliases -> [Syntax.TypeParameter] -> Core.Name -> Syntax.ClassDeclaration -> Syntax.ClassDeclaration)
augmentVariantClass aliases tparams elName cd = ((\x -> case x of
  Syntax.ClassDeclarationNormal v0 ->  
    let args = (Lists.map (\tp -> Utils.typeParameterToTypeArgument tp) tparams)
    in  
      let extendsPart = (Utils.nameToJavaClassType aliases True args elName Nothing)
      in  
        let newMods = [
                Syntax.ClassModifierPublic,
                Syntax.ClassModifierStatic,
                Syntax.ClassModifierFinal]
        in  
          let oldBody = (Syntax.normalClassDeclarationBody v0)
          in  
            let oldDecls = (Syntax.unClassBody oldBody)
            in  
              let acceptDecl = (noComment (Utils.toAcceptMethod False tparams))
              in  
                let newBody = (Syntax.ClassBody (Lists.concat2 oldDecls [
                        acceptDecl]))
                in (Syntax.ClassDeclarationNormal (Syntax.NormalClassDeclaration {
                  Syntax.normalClassDeclarationModifiers = newMods,
                  Syntax.normalClassDeclarationIdentifier = (Syntax.normalClassDeclarationIdentifier v0),
                  Syntax.normalClassDeclarationParameters = tparams,
                  Syntax.normalClassDeclarationExtends = (Just extendsPart),
                  Syntax.normalClassDeclarationImplements = (Syntax.normalClassDeclarationImplements v0),
                  Syntax.normalClassDeclarationBody = newBody}))
  _ -> cd) cd)

encodeTypeDefinition :: (Syntax.PackageDeclaration -> Helpers.Aliases -> Module.TypeDefinition -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) (Core.Name, Syntax.CompilationUnit))
encodeTypeDefinition pkg aliases tdef cx g =  
  let name = (Module.typeDefinitionName tdef)
  in  
    let typ = (Module.typeDefinitionType tdef)
    in  
      let serializable = (isSerializableJavaType typ)
      in  
        let imports = (Logic.ifElse serializable [
                Syntax.ImportDeclarationSingleType (Syntax.SingleTypeImportDeclaration (Utils.javaTypeName (Syntax.Identifier "java.io.Serializable")))] [])
        in (Eithers.bind (toClassDecl False serializable aliases [] name typ cx g) (\decl -> Eithers.bind (Annotations.getTypeDescription cx g typ) (\comment ->  
          let tdecl = Syntax.TypeDeclarationWithComments {
                  Syntax.typeDeclarationWithCommentsValue = (Syntax.TypeDeclarationClass decl),
                  Syntax.typeDeclarationWithCommentsComments = comment}
          in (Right (name, (Syntax.CompilationUnitOrdinary (Syntax.OrdinaryCompilationUnit {
            Syntax.ordinaryCompilationUnitPackage = (Just pkg),
            Syntax.ordinaryCompilationUnitImports = imports,
            Syntax.ordinaryCompilationUnitTypes = [
              tdecl]})))))))

peelDomainsAndCod :: (Int -> Core.Type -> ([Core.Type], Core.Type))
peelDomainsAndCod n t = (Logic.ifElse (Equality.lte n 0) ([], t) ((\x -> case x of
  Core.TypeFunction v0 ->  
    let rest = (peelDomainsAndCod (Math.sub n 1) (Core.functionTypeCodomain v0))
    in (Lists.cons (Core.functionTypeDomain v0) (Pairs.first rest), (Pairs.second rest))
  _ -> ([], t)) (Rewriting.deannotateType t)))

isSerializableJavaType :: (Core.Type -> Bool)
isSerializableJavaType typ = ((\x -> case x of
  Core.TypeRecord _ -> True
  Core.TypeUnion _ -> True
  Core.TypeWrap _ -> True
  Core.TypeForall v0 -> (isSerializableJavaType (Core.forallTypeBody v0))
  _ -> False) (Rewriting.deannotateType typ))

correctCastType :: (Core.Term -> [Core.Type] -> Core.Type -> t0 -> t1 -> Either t2 Core.Type)
correctCastType innerBody typeArgs fallback cx g = ((\x -> case x of
  Core.TermPair _ -> (Logic.ifElse (Equality.equal (Lists.length typeArgs) 2) (Right (Core.TypePair (Core.PairType {
    Core.pairTypeFirst = (Lists.head typeArgs),
    Core.pairTypeSecond = (Lists.head (Lists.tail typeArgs))}))) (Right fallback))
  _ -> (Right fallback)) (Rewriting.deannotateTerm innerBody))

typeAppFallbackCast :: (Helpers.JavaEnvironment -> Helpers.Aliases -> [M.Map Core.Name Core.Term] -> [Syntax.Type] -> Syntax.Type -> Core.Term -> Core.Type -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.Expression)
typeAppFallbackCast env aliases anns tyapps jatyp body typ cx g =  
  let annotatedBody = (Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ typ)) body)
  in (Eithers.bind (encodeTermInternal env anns (Lists.cons jatyp tyapps) annotatedBody cx g) (\jbody -> Eithers.bind (encodeType aliases Sets.empty typ cx g) (\jtype -> Eithers.bind (Utils.javaTypeToJavaReferenceType jtype cx) (\rt -> Right (Utils.javaCastExpressionToJavaExpression (Utils.javaCastExpression rt (Utils.javaExpressionToJavaUnaryExpression jbody)))))))

typeAppNullaryOrHoisted :: (Helpers.JavaEnvironment -> Helpers.Aliases -> [M.Map Core.Name Core.Term] -> [Syntax.Type] -> Syntax.Type -> Core.Term -> Core.Type -> Core.Name -> Helpers.JavaSymbolClass -> [Core.Type] -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.Expression)
typeAppNullaryOrHoisted env aliases anns tyapps jatyp body correctedTyp varName cls allTypeArgs cx g =  
  let qn = (Names_.qualifyName varName)
  in  
    let mns = (Module.qualifiedNameNamespace qn)
    in  
      let localName = (Module.qualifiedNameLocal qn)
      in ((\x -> case x of
        Helpers.JavaSymbolClassNullaryFunction -> (Maybes.cases mns (typeAppFallbackCast env aliases anns tyapps jatyp body correctedTyp cx g) (\ns_ ->  
          let classId = (Utils.nameToJavaName aliases (Names_.unqualifyName (Module.QualifiedName {
                  Module.qualifiedNameNamespace = (Just ns_),
                  Module.qualifiedNameLocal = (elementsClassName ns_)})))
          in  
            let methodId = (Syntax.Identifier (Utils.sanitizeJavaName localName))
            in (Eithers.bind (filterPhantomTypeArgs varName allTypeArgs cx g) (\filteredTypeArgs -> Eithers.bind (Eithers.mapList (\t -> Eithers.bind (encodeType aliases Sets.empty t cx g) (\jt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jt cx) (\rt -> Right (Syntax.TypeArgumentReference rt)))) filteredTypeArgs) (\jTypeArgs -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs classId methodId jTypeArgs [])))))))
        Helpers.JavaSymbolClassHoistedLambda v0 -> (Maybes.cases mns (typeAppFallbackCast env aliases anns tyapps jatyp body correctedTyp cx g) (\ns_ ->  
          let classId = (Utils.nameToJavaName aliases (Names_.unqualifyName (Module.QualifiedName {
                  Module.qualifiedNameNamespace = (Just ns_),
                  Module.qualifiedNameLocal = (elementsClassName ns_)})))
          in  
            let methodId = (Syntax.Identifier (Utils.sanitizeJavaName localName))
            in (Eithers.bind (filterPhantomTypeArgs varName allTypeArgs cx g) (\filteredTypeArgs -> Eithers.bind (Eithers.mapList (\t -> Eithers.bind (encodeType aliases Sets.empty t cx g) (\jt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jt cx) (\rt -> Right (Syntax.TypeArgumentReference rt)))) filteredTypeArgs) (\jTypeArgs ->  
              let paramNames = (Lists.map (\i -> Core.Name (Strings.cat2 "p" (Literals.showInt32 i))) (Math.range 0 (Math.sub v0 1)))
              in  
                let paramExprs = (Lists.map (\p -> Utils.javaIdentifierToJavaExpression (Utils.variableToJavaIdentifier p)) paramNames)
                in  
                  let call = (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs classId methodId jTypeArgs paramExprs))
                  in (Right (buildCurriedLambda paramNames call)))))))
        _ -> (typeAppFallbackCast env aliases anns tyapps jatyp body correctedTyp cx g)) cls)

flattenApps :: (Core.Term -> [Core.Term] -> ([Core.Term], Core.Term))
flattenApps t acc = ((\x -> case x of
  Core.TermApplication v0 -> (flattenApps (Core.applicationFunction v0) (Lists.cons (Core.applicationArgument v0) acc))
  _ -> (acc, t)) (Rewriting.deannotateTerm t))

collectLambdaDomains :: (Core.Term -> ([Core.Type], Core.Term))
collectLambdaDomains t = ((\x -> case x of
  Core.TermFunction v0 -> ((\x -> case x of
    Core.FunctionLambda v1 -> (Maybes.cases (Core.lambdaDomain v1) ([], t) (\dom ->  
      let rest = (collectLambdaDomains (Core.lambdaBody v1))
      in (Lists.cons dom (Pairs.first rest), (Pairs.second rest))))
    _ -> ([], t)) v0)
  _ -> ([], t)) (Rewriting.deannotateTerm t))

rebuildApps :: (Core.Term -> [Core.Term] -> Core.Type -> Core.Term)
rebuildApps f args fType = (Logic.ifElse (Lists.null args) f ((\x -> case x of
  Core.TypeFunction v0 ->  
    let arg = (Lists.head args)
    in  
      let rest = (Lists.tail args)
      in  
        let remainingType = (Core.functionTypeCodomain v0)
        in  
          let app = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = f,
                  Core.applicationArgument = arg}))
          in  
            let annotatedApp = (Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ remainingType)) app)
            in (rebuildApps annotatedApp rest remainingType)
  _ -> (Lists.foldl (\acc -> \a -> Core.TermApplication (Core.Application {
    Core.applicationFunction = acc,
    Core.applicationArgument = a})) f args)) (Rewriting.deannotateType fType)))

propagateTypesInAppChain :: (Core.Type -> Core.Type -> Core.Term -> Core.Term)
propagateTypesInAppChain fixedCod resultType t =  
  let flattened = (flattenApps t [])
  in  
    let args = (Pairs.first flattened)
    in  
      let fun = (Pairs.second flattened)
      in  
        let lambdaDomsResult = (collectLambdaDomains fun)
        in  
          let lambdaDoms = (Pairs.first lambdaDomsResult)
          in  
            let nArgs = (Lists.length args)
            in  
              let nLambdaDoms = (Lists.length lambdaDoms)
              in (Logic.ifElse (Logic.and (Equality.gt nLambdaDoms 0) (Equality.gt nArgs 0)) ( 
                let bodyRetType = (Pairs.second (peelDomainsAndCod (Math.sub nLambdaDoms nArgs) resultType))
                in  
                  let funType = (Lists.foldl (\c -> \d -> Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = d,
                          Core.functionTypeCodomain = c})) bodyRetType (Lists.reverse lambdaDoms))
                  in  
                    let annotatedFun = (Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ funType)) fun)
                    in (rebuildApps annotatedFun args funType)) ((\x -> case x of
                Core.TermApplication v0 ->  
                  let lhs = (Core.applicationFunction v0)
                  in  
                    let rhs = (Core.applicationArgument v0)
                    in  
                      let annotatedLhs = ((\x -> case x of
                              Core.TermFunction v1 -> ((\x -> case x of
                                Core.FunctionElimination v2 -> ((\x -> case x of
                                  Core.EliminationUnion v3 ->  
                                    let dom = (Schemas.nominalApplication (Core.caseStatementTypeName v3) [])
                                    in  
                                      let ft = (Core.TypeFunction (Core.FunctionType {
                                              Core.functionTypeDomain = dom,
                                              Core.functionTypeCodomain = fixedCod}))
                                      in (Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ ft)) lhs)
                                  _ -> lhs) v2)
                                _ -> lhs) v1)
                              _ -> lhs) (Rewriting.deannotateTerm lhs))
                      in (Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ resultType)) (Core.TermApplication (Core.Application {
                        Core.applicationFunction = annotatedLhs,
                        Core.applicationArgument = rhs})))
                _ -> (Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ resultType)) t)) (Rewriting.deannotateTerm t)))

encodeTermTCO :: (Helpers.JavaEnvironment -> Core.Name -> [Core.Name] -> M.Map Core.Name Core.Name -> Int -> Core.Term -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) [Syntax.BlockStatement])
encodeTermTCO env0 funcName paramNames tcoVarRenames tcoDepth term cx g =  
  let aliases0 = (Helpers.javaEnvironmentAliases env0)
  in  
    let env = Helpers.JavaEnvironment {
            Helpers.javaEnvironmentAliases = Helpers.Aliases {
              Helpers.aliasesCurrentNamespace = (Helpers.aliasesCurrentNamespace aliases0),
              Helpers.aliasesPackages = (Helpers.aliasesPackages aliases0),
              Helpers.aliasesBranchVars = (Helpers.aliasesBranchVars aliases0),
              Helpers.aliasesRecursiveVars = (Helpers.aliasesRecursiveVars aliases0),
              Helpers.aliasesInScopeTypeParams = (Helpers.aliasesInScopeTypeParams aliases0),
              Helpers.aliasesPolymorphicLocals = (Helpers.aliasesPolymorphicLocals aliases0),
              Helpers.aliasesInScopeJavaVars = (Helpers.aliasesInScopeJavaVars aliases0),
              Helpers.aliasesVarRenames = (Maps.union tcoVarRenames (Helpers.aliasesVarRenames aliases0)),
              Helpers.aliasesLambdaVars = (Helpers.aliasesLambdaVars aliases0),
              Helpers.aliasesTypeVarSubst = (Helpers.aliasesTypeVarSubst aliases0),
              Helpers.aliasesTrustedTypeVars = (Helpers.aliasesTrustedTypeVars aliases0),
              Helpers.aliasesMethodCodomain = (Helpers.aliasesMethodCodomain aliases0),
              Helpers.aliasesThunkedVars = (Helpers.aliasesThunkedVars aliases0)},
            Helpers.javaEnvironmentGraph = (Helpers.javaEnvironmentGraph env0)}
    in  
      let stripped = (Rewriting.deannotateAndDetypeTerm term)
      in  
        let gathered = (CoderUtils.gatherApplications stripped)
        in  
          let gatherArgs = (Pairs.first gathered)
          in  
            let gatherFun = (Pairs.second gathered)
            in  
              let strippedFun = (Rewriting.deannotateAndDetypeTerm gatherFun)
              in  
                let isSelfCall = ((\x -> case x of
                        Core.TermVariable v0 -> (Equality.equal v0 funcName)
                        _ -> False) strippedFun)
                in (Logic.ifElse (Logic.and isSelfCall (Equality.equal (Lists.length gatherArgs) (Lists.length paramNames))) ( 
                  let changePairs = (Lists.filter (\pair -> Logic.not ((\x -> case x of
                          Core.TermVariable v0 -> (Equality.equal v0 (Pairs.first pair))
                          _ -> False) (Rewriting.deannotateAndDetypeTerm (Pairs.second pair)))) (Lists.zip paramNames gatherArgs))
                  in  
                    let changedParams = (Lists.map Pairs.first changePairs)
                    in (Eithers.bind (Eithers.mapList (\pair -> encodeTerm env (Pairs.second pair) cx g) changePairs) (\jChangedArgs ->  
                      let assignments = (Lists.map (\pair ->  
                              let paramName = (Pairs.first pair)
                              in  
                                let jArg = (Pairs.second pair)
                                in (Syntax.BlockStatementStatement (Utils.javaAssignmentStatement (Syntax.LeftHandSideExpressionName (Utils.javaIdentifierToJavaExpressionName (Utils.variableToJavaIdentifier paramName))) jArg))) (Lists.zip changedParams jChangedArgs))
                      in  
                        let continueStmt = (Syntax.BlockStatementStatement (Syntax.StatementWithoutTrailing (Syntax.StatementWithoutTrailingSubstatementContinue (Syntax.ContinueStatement Nothing))))
                        in (Right (Lists.concat2 assignments [
                          continueStmt]))))) ((\x -> case x of
                  Core.TermLet v0 ->  
                    let letBindings = (Core.letBindings v0)
                    in  
                      let letBody = (Core.letBody v0)
                      in (Eithers.bind (bindingsToStatements env letBindings cx g) (\bindResult ->  
                        let letStmts = (Pairs.first bindResult)
                        in  
                          let envAfterLet = (Pairs.second bindResult)
                          in (Eithers.bind (encodeTermTCO envAfterLet funcName paramNames tcoVarRenames tcoDepth letBody cx g) (\tcoBodyStmts -> Right (Lists.concat2 letStmts tcoBodyStmts)))))
                  _ ->  
                    let gathered2 = (CoderUtils.gatherApplications term)
                    in  
                      let args2 = (Pairs.first gathered2)
                      in  
                        let body2 = (Pairs.second gathered2)
                        in (Logic.ifElse (Equality.equal (Lists.length args2) 1) ( 
                          let arg = (Lists.head args2)
                          in ((\x -> case x of
                            Core.TermFunction v0 -> ((\x -> case x of
                              Core.FunctionElimination v1 -> ((\x -> case x of
                                Core.EliminationUnion v2 ->  
                                  let aliases = (Helpers.javaEnvironmentAliases env)
                                  in  
                                    let tname = (Core.caseStatementTypeName v2)
                                    in  
                                      let dflt = (Core.caseStatementDefault v2)
                                      in  
                                        let cases_ = (Core.caseStatementCases v2)
                                        in (Eithers.bind (domTypeArgs aliases (Schemas.nominalApplication tname []) cx g) (\domArgs -> Eithers.bind (encodeTerm env arg cx g) (\jArgRaw ->  
                                          let depthSuffix = (Logic.ifElse (Equality.equal tcoDepth 0) "" (Literals.showInt32 tcoDepth))
                                          in  
                                            let matchVarId = (Utils.javaIdentifier (Strings.cat [
                                                    "_tco_match_",
                                                    (Formatting.decapitalize (Names_.localNameOf tname)),
                                                    depthSuffix]))
                                            in  
                                              let matchDecl = (Utils.varDeclarationStatement matchVarId jArgRaw)
                                              in  
                                                let jArg = (Utils.javaIdentifierToJavaExpression matchVarId)
                                                in (Eithers.bind (Eithers.mapList (\field ->  
                                                  let fieldName = (Core.fieldName field)
                                                  in  
                                                    let variantRefType = (Utils.nameToJavaReferenceType aliases True domArgs tname (Just (Formatting.capitalize (Core.unName fieldName))))
                                                    in ((\x -> case x of
                                                      Core.TermFunction v3 -> ((\x -> case x of
                                                        Core.FunctionLambda v4 -> (withLambda env v4 (\env2 ->  
                                                          let lambdaParam = (Core.lambdaParameter v4)
                                                          in  
                                                            let branchBody = (Core.lambdaBody v4)
                                                            in  
                                                              let env3 = (insertBranchVar lambdaParam env2)
                                                              in  
                                                                let varId = (Utils.variableToJavaIdentifier lambdaParam)
                                                                in  
                                                                  let castExpr = (Utils.javaCastExpressionToJavaExpression (Utils.javaCastExpression variantRefType (Utils.javaExpressionToJavaUnaryExpression jArg)))
                                                                  in  
                                                                    let localDecl = (Utils.varDeclarationStatement varId castExpr)
                                                                    in  
                                                                      let isBranchTailCall = (CoderUtils.isTailRecursiveInTailPosition funcName branchBody)
                                                                      in (Eithers.bind (Logic.ifElse isBranchTailCall (encodeTermTCO env3 funcName paramNames tcoVarRenames (Math.add tcoDepth 1) branchBody cx g) (Eithers.bind (analyzeJavaFunction env3 branchBody cx g) (\fs ->  
                                                                        let bindings = (Typing.functionStructureBindings fs)
                                                                        in  
                                                                          let innerBody = (Typing.functionStructureBody fs)
                                                                          in  
                                                                            let env4 = (Typing.functionStructureEnvironment fs)
                                                                            in (Eithers.bind (bindingsToStatements env4 bindings cx g) (\bindResult ->  
                                                                              let bindingStmts = (Pairs.first bindResult)
                                                                              in  
                                                                                let env5 = (Pairs.second bindResult)
                                                                                in (Eithers.bind (encodeTerm env5 innerBody cx g) (\jret ->  
                                                                                  let returnStmt = (Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just jret)))
                                                                                  in (Right (Lists.concat2 bindingStmts [
                                                                                    returnStmt]))))))))) (\bodyStmts ->  
                                                                        let relExpr = (Utils.javaInstanceOf (Utils.javaUnaryExpressionToJavaRelationalExpression (Utils.javaExpressionToJavaUnaryExpression jArg)) variantRefType)
                                                                        in  
                                                                          let condExpr = (Utils.javaRelationalExpressionToJavaExpression relExpr)
                                                                          in  
                                                                            let blockStmts = (Lists.cons localDecl bodyStmts)
                                                                            in  
                                                                              let ifBody = (Syntax.StatementWithoutTrailing (Syntax.StatementWithoutTrailingSubstatementBlock (Syntax.Block blockStmts)))
                                                                              in (Right (Syntax.BlockStatementStatement (Syntax.StatementIfThen (Syntax.IfThenStatement {
                                                                                Syntax.ifThenStatementExpression = condExpr,
                                                                                Syntax.ifThenStatementStatement = ifBody}))))))))
                                                        _ -> (Left (Context.InContext {
                                                          Context.inContextObject = (Error.ErrorOther (Error.OtherError "TCO: case branch is not a lambda")),
                                                          Context.inContextContext = cx}))) v3)
                                                      _ -> (Left (Context.InContext {
                                                        Context.inContextObject = (Error.ErrorOther (Error.OtherError "TCO: case branch is not a lambda")),
                                                        Context.inContextContext = cx}))) (Rewriting.deannotateTerm (Core.fieldTerm field)))) cases_) (\ifBlocks -> Eithers.bind (Maybes.cases dflt (Right [
                                                  Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just jArg))]) (\d -> Eithers.bind (encodeTerm env d cx g) (\dExpr -> Right [
                                                  Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just dExpr))]))) (\defaultStmt -> Right (Lists.concat [
                                                  [
                                                    matchDecl],
                                                  ifBlocks,
                                                  defaultStmt])))))))
                                _ -> (Eithers.bind (encodeTerm env term cx g) (\expr -> Right [
                                  Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just expr))]))) v1)
                              _ -> (Eithers.bind (encodeTerm env term cx g) (\expr -> Right [
                                Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just expr))]))) v0)
                            _ -> (Eithers.bind (encodeTerm env term cx g) (\expr -> Right [
                              Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just expr))]))) (Rewriting.deannotateAndDetypeTerm body2))) (Eithers.bind (encodeTerm env term cx g) (\expr -> Right [
                          Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just expr))])))) stripped))

encodeTermDefinition :: (Helpers.JavaEnvironment -> Module.TermDefinition -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) Syntax.InterfaceMemberDeclaration)
encodeTermDefinition env tdef cx g =  
  let name = (Module.termDefinitionName tdef)
  in  
    let term0 = (Module.termDefinitionTerm tdef)
    in  
      let ts = (Module.termDefinitionType tdef)
      in  
        let term = (Rewriting.unshadowVariables term0)
        in (Eithers.bind (analyzeJavaFunction env term cx g) (\fs ->  
          let schemeVars = (Lists.filter (\v -> isSimpleName v) (Core.typeSchemeVariables ts))
          in  
            let termVars = (Typing.functionStructureTypeParams fs)
            in  
              let schemeTypeVars = (collectTypeVars (Core.typeSchemeType ts))
              in  
                let usedSchemeVars = (Lists.filter (\v -> Sets.member v schemeTypeVars) schemeVars)
                in  
                  let tparams = (Logic.ifElse (Lists.null usedSchemeVars) termVars usedSchemeVars)
                  in  
                    let params = (Typing.functionStructureParams fs)
                    in  
                      let bindings = (Typing.functionStructureBindings fs)
                      in  
                        let body = (Typing.functionStructureBody fs)
                        in  
                          let doms = (Typing.functionStructureDomains fs)
                          in  
                            let env2 = (Typing.functionStructureEnvironment fs)
                            in  
                              let schemeType = (Core.typeSchemeType ts)
                              in  
                                let numParams = (Lists.length params)
                                in  
                                  let peelResult = (peelDomainsAndCod numParams schemeType)
                                  in  
                                    let schemeDoms = (Pairs.first peelResult)
                                    in  
                                      let cod = (Pairs.second peelResult)
                                      in  
                                        let schemeVarSet = (Sets.fromList tparams)
                                        in (Eithers.bind (Logic.ifElse (Lists.null tparams) (Right Maps.empty) (buildSubstFromAnnotations schemeVarSet term cx g)) (\typeVarSubst ->  
                                          let overgenSubst = (detectAccumulatorUnification schemeDoms cod tparams)
                                          in  
                                            let overgenVarSubst = (Maps.fromList (Maybes.cat (Lists.map (\entry ->  
                                                    let k = (Pairs.first entry)
                                                    in  
                                                      let v = (Pairs.second entry)
                                                      in ((\x -> case x of
                                                        Core.TypeVariable v0 -> (Just (k, v0))
                                                        _ -> Nothing) v)) (Maps.toList overgenSubst))))
                                            in  
                                              let fixedCod = (Logic.ifElse (Maps.null overgenSubst) cod (substituteTypeVarsWithTypes overgenSubst cod))
                                              in  
                                                let fixedDoms = (Logic.ifElse (Maps.null overgenSubst) schemeDoms (Lists.map (\d -> substituteTypeVarsWithTypes overgenSubst d) schemeDoms))
                                                in  
                                                  let fixedTparams = (Logic.ifElse (Maps.null overgenSubst) tparams (Lists.filter (\v -> Logic.not (Maps.member v overgenSubst)) tparams))
                                                  in  
                                                    let constraints = (Maybes.fromMaybe Maps.empty (Core.typeSchemeConstraints ts))
                                                    in  
                                                      let jparams = (Lists.map (\v -> Utils.javaTypeParameter (Formatting.capitalize (Core.unName v))) fixedTparams)
                                                      in  
                                                        let aliases2base = (Helpers.javaEnvironmentAliases env2)
                                                        in  
                                                          let trustedVars = (Sets.unions (Lists.map (\d -> collectTypeVars d) (Lists.concat2 fixedDoms [
                                                                  fixedCod])))
                                                          in  
                                                            let fixedSchemeVarSet = (Sets.fromList fixedTparams)
                                                            in  
                                                              let aliases2 = Helpers.Aliases {
                                                                      Helpers.aliasesCurrentNamespace = (Helpers.aliasesCurrentNamespace aliases2base),
                                                                      Helpers.aliasesPackages = (Helpers.aliasesPackages aliases2base),
                                                                      Helpers.aliasesBranchVars = (Helpers.aliasesBranchVars aliases2base),
                                                                      Helpers.aliasesRecursiveVars = (Helpers.aliasesRecursiveVars aliases2base),
                                                                      Helpers.aliasesInScopeTypeParams = fixedSchemeVarSet,
                                                                      Helpers.aliasesPolymorphicLocals = (Helpers.aliasesPolymorphicLocals aliases2base),
                                                                      Helpers.aliasesInScopeJavaVars = (Helpers.aliasesInScopeJavaVars aliases2base),
                                                                      Helpers.aliasesVarRenames = (Helpers.aliasesVarRenames aliases2base),
                                                                      Helpers.aliasesLambdaVars = (Sets.union (Helpers.aliasesLambdaVars aliases2base) (Sets.fromList params)),
                                                                      Helpers.aliasesTypeVarSubst = (Maps.union overgenVarSubst typeVarSubst),
                                                                      Helpers.aliasesTrustedTypeVars = (Sets.intersection trustedVars fixedSchemeVarSet),
                                                                      Helpers.aliasesMethodCodomain = (Just fixedCod),
                                                                      Helpers.aliasesThunkedVars = (Helpers.aliasesThunkedVars aliases2base)}
                                                              in  
                                                                let env2WithTypeParams = Helpers.JavaEnvironment {
                                                                        Helpers.javaEnvironmentAliases = aliases2,
                                                                        Helpers.javaEnvironmentGraph = (Helpers.javaEnvironmentGraph env2)}
                                                                in (Eithers.bind (bindingsToStatements env2WithTypeParams bindings cx g) (\bindResult ->  
                                                                  let bindingStmts = (Pairs.first bindResult)
                                                                  in  
                                                                    let env3 = (Pairs.second bindResult)
                                                                    in (Eithers.bind (Logic.ifElse (Maps.null overgenSubst) (Right body) (applyOvergenSubstToTermAnnotations overgenSubst body cx g)) (\body_ ->  
                                                                      let annotatedBody = (propagateTypesInAppChain fixedCod fixedCod body_)
                                                                      in (Eithers.bind (Eithers.mapList (\pair -> Eithers.bind (encodeType aliases2 Sets.empty (Pairs.first pair) cx g) (\jdom -> Right (Utils.javaTypeToJavaFormalParameter jdom (Pairs.second pair)))) (Lists.zip fixedDoms params)) (\jformalParams -> Eithers.bind (encodeType aliases2 Sets.empty fixedCod cx g) (\jcod ->  
                                                                        let result = (Utils.javaTypeToJavaResult jcod)
                                                                        in  
                                                                          let mods = [
                                                                                  Syntax.InterfaceMethodModifierStatic]
                                                                          in  
                                                                            let jname = (Utils.sanitizeJavaName (Formatting.decapitalize (Names_.localNameOf name)))
                                                                            in  
                                                                              let isTCO = False
                                                                              in (Eithers.bind (Logic.ifElse isTCO ( 
                                                                                let tcoSuffix = "_tco"
                                                                                in  
                                                                                  let snapshotNames = (Lists.map (\p -> Core.Name (Strings.cat2 (Core.unName p) tcoSuffix)) params)
                                                                                  in  
                                                                                    let tcoVarRenames = (Maps.fromList (Lists.zip params snapshotNames))
                                                                                    in  
                                                                                      let snapshotDecls = (Lists.map (\pair -> Utils.finalVarDeclarationStatement (Utils.variableToJavaIdentifier (Pairs.second pair)) (Utils.javaIdentifierToJavaExpression (Utils.variableToJavaIdentifier (Pairs.first pair)))) (Lists.zip params snapshotNames))
                                                                                      in  
                                                                                        let tcoBody = (Logic.ifElse (Lists.null bindings) annotatedBody (Core.TermLet (Core.Let {
                                                                                                Core.letBindings = bindings,
                                                                                                Core.letBody = annotatedBody})))
                                                                                        in (Eithers.bind (encodeTermTCO env2WithTypeParams name params tcoVarRenames 0 tcoBody cx g) (\tcoStmts ->  
                                                                                          let whileBodyStmts = (Lists.concat2 snapshotDecls tcoStmts)
                                                                                          in  
                                                                                            let whileBodyBlock = (Syntax.StatementWithoutTrailing (Syntax.StatementWithoutTrailingSubstatementBlock (Syntax.Block whileBodyStmts)))
                                                                                            in  
                                                                                              let noCond = Nothing
                                                                                              in  
                                                                                                let whileStmt = (Syntax.BlockStatementStatement (Syntax.StatementWhile (Syntax.WhileStatement {
                                                                                                        Syntax.whileStatementCond = noCond,
                                                                                                        Syntax.whileStatementBody = whileBodyBlock})))
                                                                                                in (Right [
                                                                                                  whileStmt])))) (Eithers.bind (encodeTerm env3 annotatedBody cx g) (\jbody ->  
                                                                                let returnSt = (Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just jbody)))
                                                                                in (Right (Lists.concat2 bindingStmts [
                                                                                  returnSt]))))) (\methodBody -> Right (Utils.interfaceMethodDeclaration mods jparams jname jformalParams result (Just methodBody)))))))))))))))

encodeDefinitions :: (Module.Module -> [Module.Definition] -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) (M.Map Core.Name Syntax.CompilationUnit))
encodeDefinitions mod defs cx g =  
  let aliases = (Utils.importAliasesForModule mod)
  in  
    let env = Helpers.JavaEnvironment {
            Helpers.javaEnvironmentAliases = aliases,
            Helpers.javaEnvironmentGraph = g}
    in  
      let pkg = (Utils.javaPackageDeclaration (Module.moduleNamespace mod))
      in  
        let partitioned = (Schemas.partitionDefinitions defs)
        in  
          let typeDefs = (Pairs.first partitioned)
          in  
            let termDefs = (Pairs.second partitioned)
            in  
              let nonTypedefDefs = (Lists.filter (\td ->  
                      let typ = (Module.typeDefinitionType td)
                      in (isSerializableJavaType typ)) typeDefs)
              in (Eithers.bind (Eithers.mapList (\td -> encodeTypeDefinition pkg aliases td cx g) nonTypedefDefs) (\typeUnits -> Eithers.bind (Logic.ifElse (Lists.null termDefs) (Right []) (Eithers.bind (Eithers.mapList (\td -> encodeTermDefinition env td cx g) termDefs) (\dataMembers -> Right [
                constructElementsInterface mod dataMembers]))) (\termUnits -> Right (Maps.fromList (Lists.concat2 typeUnits termUnits)))))

moduleToJava :: (Module.Module -> [Module.Definition] -> Context.Context -> Graph.Graph -> Either (Context.InContext Error.Error) (M.Map String String))
moduleToJava mod defs cx g = (Eithers.bind (encodeDefinitions mod defs cx g) (\units -> Right (Maps.fromList (Lists.map (\entry ->  
  let name = (Pairs.first entry)
  in  
    let unit = (Pairs.second entry)
    in (bindingNameToFilePath name, (Serialization.printExpr (Serialization.parenthesize (Serde.writeCompilationUnit unit))))) (Maps.toList units)))))
