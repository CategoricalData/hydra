-- Note: this is an automatically generated file. Do not edit.

-- | Java code generator: converts Hydra modules to Java source code

module Hydra.Ext.Java.Coder where

import qualified Hydra.Adapt.Utils as Utils
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Arity as Arity
import qualified Hydra.CoderUtils as CoderUtils
import qualified Hydra.Compute as Compute
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Encode.Core as Core__
import qualified Hydra.Ext.Java.Helpers as Helpers
import qualified Hydra.Ext.Java.Language as Language
import qualified Hydra.Ext.Java.Names as Names
import qualified Hydra.Ext.Java.Serde as Serde
import qualified Hydra.Ext.Java.Syntax as Syntax
import qualified Hydra.Ext.Java.Utils as Utils_
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Inference as Inference
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
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
import qualified Hydra.Monads as Monads
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
  in (Utils.nameToFilePath Util.CaseConventionCamel Util.CaseConventionPascal (Module.FileExtension "java") unq)

javaIdentifierToString :: (Syntax.Identifier -> String)
javaIdentifierToString id = (Syntax.unIdentifier id)

boundTypeVariables :: (Core.Type -> [Core.Name])
boundTypeVariables typ = ((\x -> case x of
  Core.TypeAnnotated v1 -> (boundTypeVariables (Core.annotatedTypeBody v1))
  Core.TypeForall v1 -> (Lists.cons (Core.forallTypeParameter v1) (boundTypeVariables (Core.forallTypeBody v1)))
  _ -> []) typ)

extractTypeApplicationArgs :: (Core.Type -> [Core.Type])
extractTypeApplicationArgs typ = (Lists.reverse (extractTypeApplicationArgs_go typ))

extractTypeApplicationArgs_go :: (Core.Type -> [Core.Type])
extractTypeApplicationArgs_go t = ((\x -> case x of
  Core.TypeApplication v1 -> (Lists.cons (Core.applicationTypeArgument v1) (extractTypeApplicationArgs_go (Core.applicationTypeFunction v1)))
  _ -> []) t)

javaTypeParametersForType :: (Core.Type -> [Syntax.TypeParameter])
javaTypeParametersForType typ =  
  let toParam = (\name -> Utils_.javaTypeParameter (Formatting.capitalize (Core.unName name))) 
      boundVars = (javaTypeParametersForType_bvars typ)
      freeVars = (Lists.filter (\v -> isLambdaBoundVariable v) (Sets.toList (Rewriting.freeVariablesInType typ)))
      vars = (Lists.nub (Lists.concat2 boundVars freeVars))
  in (Lists.map toParam vars)

javaTypeParametersForType_bvars :: (Core.Type -> [Core.Name])
javaTypeParametersForType_bvars t = ((\x -> case x of
  Core.TypeForall v1 -> (Lists.cons (Core.forallTypeParameter v1) (javaTypeParametersForType_bvars (Core.forallTypeBody v1)))
  _ -> []) t)

javaTypeArgumentsForType :: (Core.Type -> [Syntax.TypeArgument])
javaTypeArgumentsForType typ = (Lists.reverse (Lists.map Utils_.typeParameterToTypeArgument (javaTypeParametersForType typ)))

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
          Syntax.classTypeIdentifier = (Utils_.javaTypeIdentifier "Serializable"),
          Syntax.classTypeArguments = []}))
  in (Logic.ifElse isSer [
    javaSerializableType] [])

encodeLiteralType :: (Core.LiteralType -> Compute.Flow t0 Syntax.Type)
encodeLiteralType lt = ((\x -> case x of
  Core.LiteralTypeBinary -> (Flows.pure (Syntax.TypeReference (Syntax.ReferenceTypeArray (Syntax.ArrayType {
    Syntax.arrayTypeDims = (Syntax.Dims [
      []]),
    Syntax.arrayTypeVariant = (Syntax.ArrayType_VariantPrimitive (Syntax.PrimitiveTypeWithAnnotations {
      Syntax.primitiveTypeWithAnnotationsType = (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeByte)),
      Syntax.primitiveTypeWithAnnotationsAnnotations = []}))}))))
  Core.LiteralTypeBoolean -> (encodeLiteralType_simple "Boolean")
  Core.LiteralTypeFloat v1 -> ((\x -> case x of
    Core.FloatTypeBigfloat -> (Flows.pure (Utils_.javaRefType [] (Just (Names.javaPackageName [
      "java",
      "math"])) "BigDecimal"))
    Core.FloatTypeFloat32 -> (encodeLiteralType_simple "Float")
    Core.FloatTypeFloat64 -> (encodeLiteralType_simple "Double")) v1)
  Core.LiteralTypeInteger v1 -> ((\x -> case x of
    Core.IntegerTypeBigint -> (Flows.pure (Utils_.javaRefType [] (Just (Names.javaPackageName [
      "java",
      "math"])) "BigInteger"))
    Core.IntegerTypeInt8 -> (encodeLiteralType_simple "Byte")
    Core.IntegerTypeInt16 -> (encodeLiteralType_simple "Short")
    Core.IntegerTypeInt32 -> (encodeLiteralType_simple "Integer")
    Core.IntegerTypeInt64 -> (encodeLiteralType_simple "Long")
    Core.IntegerTypeUint8 -> (encodeLiteralType_simple "Short")
    Core.IntegerTypeUint16 -> (encodeLiteralType_simple "Character")
    Core.IntegerTypeUint32 -> (encodeLiteralType_simple "Long")
    Core.IntegerTypeUint64 -> (Flows.pure (Utils_.javaRefType [] (Just (Names.javaPackageName [
      "java",
      "math"])) "BigInteger"))) v1)
  Core.LiteralTypeString -> (encodeLiteralType_simple "String")) lt)

encodeLiteralType_simple :: (String -> Compute.Flow t0 Syntax.Type)
encodeLiteralType_simple n = (Flows.pure (Utils_.javaRefType [] Nothing n))

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
          Syntax.classTypeIdentifier = (Utils_.javaTypeIdentifier "Serializable"),
          Syntax.classTypeArguments = []})) 
      selfTypeArg = (Syntax.TypeArgumentReference (Utils_.nameToJavaReferenceType aliases False (Lists.map (\tp_ -> Utils_.typeParameterToTypeArgument tp_) tparams) elName Nothing))
      javaComparableType = (Syntax.InterfaceType (Syntax.ClassType {
              Syntax.classTypeAnnotations = [],
              Syntax.classTypeQualifier = Syntax.ClassTypeQualifierNone,
              Syntax.classTypeIdentifier = (Utils_.javaTypeIdentifier "Comparable"),
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
  Core.TypePair _ -> True
  Core.TypeEither _ -> True
  Core.TypeFunction _ -> True
  Core.TypeLiteral v1 -> ((\x -> case x of
    Core.LiteralTypeBinary -> True
    _ -> False) v1)
  Core.TypeForall v1 -> (isNonComparableType (Core.forallTypeBody v1))
  _ -> False) (Rewriting.deannotateType typ))

isBinaryType :: (Core.Type -> Bool)
isBinaryType typ = ((\x -> case x of
  Core.TypeLiteral v1 -> ((\x -> case x of
    Core.LiteralTypeBinary -> True
    _ -> False) v1)
  _ -> False) (Rewriting.deannotateType typ))

isBigNumericType :: (Core.Type -> Bool)
isBigNumericType typ = ((\x -> case x of
  Core.TypeLiteral v1 -> ((\x -> case x of
    Core.LiteralTypeFloat v2 -> ((\x -> case x of
      Core.FloatTypeBigfloat -> True
      _ -> False) v2)
    Core.LiteralTypeInteger v2 -> ((\x -> case x of
      Core.IntegerTypeBigint -> True
      _ -> False) v2)
    _ -> False) v1)
  _ -> False) (Rewriting.deannotateType typ))

innerClassRef :: (Helpers.Aliases -> Core.Name -> String -> Syntax.Identifier)
innerClassRef aliases name local =  
  let id = (Syntax.unIdentifier (Utils_.nameToJavaName aliases name))
  in (Syntax.Identifier (Strings.cat2 (Strings.cat2 id ".") local))

peelExpectedTypes :: (M.Map Core.Name Core.Type -> Int -> Core.Type -> [Core.Type])
peelExpectedTypes subst n t = (Logic.ifElse (Equality.equal n 0) [] ((\x -> case x of
  Core.TypeFunction v1 -> (Lists.cons (applySubstFull subst (Core.functionTypeDomain v1)) (peelExpectedTypes subst (Math.sub n 1) (Core.functionTypeCodomain v1)))
  _ -> []) (Rewriting.deannotateType t)))

applySubstFull :: (M.Map Core.Name Core.Type -> Core.Type -> Core.Type)
applySubstFull s t = ((\x -> case x of
  Core.TypeVariable v1 -> (Maps.findWithDefault t v1 s)
  Core.TypeFunction v1 -> (Core.TypeFunction (Core.FunctionType {
    Core.functionTypeDomain = (applySubstFull s (Core.functionTypeDomain v1)),
    Core.functionTypeCodomain = (applySubstFull s (Core.functionTypeCodomain v1))}))
  Core.TypeApplication v1 -> (Core.TypeApplication (Core.ApplicationType {
    Core.applicationTypeFunction = (applySubstFull s (Core.applicationTypeFunction v1)),
    Core.applicationTypeArgument = (applySubstFull s (Core.applicationTypeArgument v1))}))
  Core.TypeList v1 -> (Core.TypeList (applySubstFull s v1))
  Core.TypeSet v1 -> (Core.TypeSet (applySubstFull s v1))
  Core.TypeMaybe v1 -> (Core.TypeMaybe (applySubstFull s v1))
  Core.TypeMap v1 -> (Core.TypeMap (Core.MapType {
    Core.mapTypeKeys = (applySubstFull s (Core.mapTypeKeys v1)),
    Core.mapTypeValues = (applySubstFull s (Core.mapTypeValues v1))}))
  Core.TypePair v1 -> (Core.TypePair (Core.PairType {
    Core.pairTypeFirst = (applySubstFull s (Core.pairTypeFirst v1)),
    Core.pairTypeSecond = (applySubstFull s (Core.pairTypeSecond v1))}))
  Core.TypeEither v1 -> (Core.TypeEither (Core.EitherType {
    Core.eitherTypeLeft = (applySubstFull s (Core.eitherTypeLeft v1)),
    Core.eitherTypeRight = (applySubstFull s (Core.eitherTypeRight v1))}))
  Core.TypeForall v1 -> (Core.TypeForall (Core.ForallType {
    Core.forallTypeParameter = (Core.forallTypeParameter v1),
    Core.forallTypeBody = (applySubstFull (Maps.delete (Core.forallTypeParameter v1) s) (Core.forallTypeBody v1))}))
  _ -> t) (Rewriting.deannotateType t))

collectTypeVars :: (Core.Type -> S.Set Core.Name)
collectTypeVars typ = (collectTypeVars_go (Rewriting.deannotateType typ))

collectTypeVars_go :: (Core.Type -> S.Set Core.Name)
collectTypeVars_go t = ((\x -> case x of
  Core.TypeVariable v1 -> (Sets.singleton v1)
  Core.TypeFunction v1 -> (Sets.union (collectTypeVars_go (Rewriting.deannotateType (Core.functionTypeDomain v1))) (collectTypeVars_go (Rewriting.deannotateType (Core.functionTypeCodomain v1))))
  Core.TypeApplication v1 -> (Sets.union (collectTypeVars_go (Rewriting.deannotateType (Core.applicationTypeFunction v1))) (collectTypeVars_go (Rewriting.deannotateType (Core.applicationTypeArgument v1))))
  Core.TypeList v1 -> (collectTypeVars_go (Rewriting.deannotateType v1))
  Core.TypeSet v1 -> (collectTypeVars_go (Rewriting.deannotateType v1))
  Core.TypeMaybe v1 -> (collectTypeVars_go (Rewriting.deannotateType v1))
  Core.TypeMap v1 -> (Sets.union (collectTypeVars_go (Rewriting.deannotateType (Core.mapTypeKeys v1))) (collectTypeVars_go (Rewriting.deannotateType (Core.mapTypeValues v1))))
  Core.TypePair v1 -> (Sets.union (collectTypeVars_go (Rewriting.deannotateType (Core.pairTypeFirst v1))) (collectTypeVars_go (Rewriting.deannotateType (Core.pairTypeSecond v1))))
  Core.TypeEither v1 -> (Sets.union (collectTypeVars_go (Rewriting.deannotateType (Core.eitherTypeLeft v1))) (collectTypeVars_go (Rewriting.deannotateType (Core.eitherTypeRight v1))))
  Core.TypeForall v1 -> (collectTypeVars_go (Rewriting.deannotateType (Core.forallTypeBody v1)))
  _ -> Sets.empty) t)

substituteTypeVarsWithTypes :: (M.Map Core.Name Core.Type -> Core.Type -> Core.Type)
substituteTypeVarsWithTypes subst t = (substituteTypeVarsWithTypes_go subst (Rewriting.deannotateType t))

substituteTypeVarsWithTypes_go :: (M.Map Core.Name Core.Type -> Core.Type -> Core.Type)
substituteTypeVarsWithTypes_go subst t = ((\x -> case x of
  Core.TypeVariable v1 -> (Maybes.cases (Maps.lookup v1 subst) t (\rep -> rep))
  Core.TypeFunction v1 -> (Core.TypeFunction (Core.FunctionType {
    Core.functionTypeDomain = (substituteTypeVarsWithTypes_go subst (Core.functionTypeDomain v1)),
    Core.functionTypeCodomain = (substituteTypeVarsWithTypes_go subst (Core.functionTypeCodomain v1))}))
  Core.TypeApplication v1 -> (Core.TypeApplication (Core.ApplicationType {
    Core.applicationTypeFunction = (substituteTypeVarsWithTypes_go subst (Core.applicationTypeFunction v1)),
    Core.applicationTypeArgument = (substituteTypeVarsWithTypes_go subst (Core.applicationTypeArgument v1))}))
  Core.TypeList v1 -> (Core.TypeList (substituteTypeVarsWithTypes_go subst v1))
  Core.TypeSet v1 -> (Core.TypeSet (substituteTypeVarsWithTypes_go subst v1))
  Core.TypeMaybe v1 -> (Core.TypeMaybe (substituteTypeVarsWithTypes_go subst v1))
  Core.TypeMap v1 -> (Core.TypeMap (Core.MapType {
    Core.mapTypeKeys = (substituteTypeVarsWithTypes_go subst (Core.mapTypeKeys v1)),
    Core.mapTypeValues = (substituteTypeVarsWithTypes_go subst (Core.mapTypeValues v1))}))
  Core.TypePair v1 -> (Core.TypePair (Core.PairType {
    Core.pairTypeFirst = (substituteTypeVarsWithTypes_go subst (Core.pairTypeFirst v1)),
    Core.pairTypeSecond = (substituteTypeVarsWithTypes_go subst (Core.pairTypeSecond v1))}))
  Core.TypeEither v1 -> (Core.TypeEither (Core.EitherType {
    Core.eitherTypeLeft = (substituteTypeVarsWithTypes_go subst (Core.eitherTypeLeft v1)),
    Core.eitherTypeRight = (substituteTypeVarsWithTypes_go subst (Core.eitherTypeRight v1))}))
  Core.TypeForall v1 -> (Core.TypeForall (Core.ForallType {
    Core.forallTypeParameter = (Core.forallTypeParameter v1),
    Core.forallTypeBody = (substituteTypeVarsWithTypes_go subst (Core.forallTypeBody v1))}))
  _ -> t) (Rewriting.deannotateType t))

addComment :: (Syntax.ClassBodyDeclaration -> Core.FieldType -> Compute.Flow Graph.Graph Syntax.ClassBodyDeclarationWithComments)
addComment decl field = (Flows.map (\c -> Syntax.ClassBodyDeclarationWithComments {
  Syntax.classBodyDeclarationWithCommentsValue = decl,
  Syntax.classBodyDeclarationWithCommentsComments = c}) (CoderUtils.commentsFromFieldType field))

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
    Helpers.javaEnvironmentTypeContext = (Helpers.javaEnvironmentTypeContext env)}

getCodomain :: (M.Map Core.Name Core.Term -> Compute.Flow Graph.Graph Core.Type)
getCodomain ann = (Flows.map (\ft -> Core.functionTypeCodomain ft) (getFunctionType ann))

getFunctionType :: (M.Map Core.Name Core.Term -> Compute.Flow Graph.Graph Core.FunctionType)
getFunctionType ann = (Flows.bind (Annotations.getType ann) (\mt -> Maybes.cases mt (Flows.fail "type annotation is required for function and elimination terms in Java") (\t -> (\x -> case x of
  Core.TypeFunction v1 -> (Flows.pure v1)
  _ -> (Flows.fail (Strings.cat2 "expected function type, got: " (Core___.type_ t)))) t)))

wrapLazyArguments :: (Core.Name -> [Syntax.Expression] -> ([Syntax.Expression], (Maybe String)))
wrapLazyArguments name args = (Logic.ifElse (Logic.and (Equality.equal name (Core.Name "hydra.lib.logic.ifElse")) (Equality.equal (Lists.length args) 3)) ([
  Lists.at 0 args,
  (wrapInSupplierLambda (Lists.at 1 args)),
  (wrapInSupplierLambda (Lists.at 2 args))], (Just "lazy")) (args, Nothing))

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
  in (Logic.ifElse isPrim (Syntax.Identifier (Strings.cat2 (Strings.cat2 (elementJavaIdentifier_qualify aliases ns_ (Formatting.capitalize local)) ".") Names.applyMethodName)) (Maybes.cases ns_ (Syntax.Identifier (Utils_.sanitizeJavaName local)) (\n -> Syntax.Identifier (Strings.cat2 (Strings.cat2 (elementJavaIdentifier_qualify aliases (Just n) (elementsClassName n)) sep) (Utils_.sanitizeJavaName local)))))

elementJavaIdentifier_qualify :: (Helpers.Aliases -> Maybe Module.Namespace -> String -> String)
elementJavaIdentifier_qualify aliases mns s = (Syntax.unIdentifier (Utils_.nameToJavaName aliases (Names_.unqualifyName (Module.QualifiedName {
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
  let pkg = (Utils_.javaPackageDeclaration (Module.moduleNamespace mod)) 
      mods = [
              Syntax.InterfaceModifierPublic]
      className = (elementsClassName (Module.moduleNamespace mod))
      elName = (Names_.unqualifyName (Module.QualifiedName {
              Module.qualifiedNameNamespace = (Just (Module.moduleNamespace mod)),
              Module.qualifiedNameLocal = className}))
      body = (Syntax.InterfaceBody members)
      itf = (Syntax.TypeDeclarationInterface (Syntax.InterfaceDeclarationNormalInterface (Syntax.NormalInterfaceDeclaration {
              Syntax.normalInterfaceDeclarationModifiers = mods,
              Syntax.normalInterfaceDeclarationIdentifier = (Utils_.javaTypeIdentifier className),
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
  Syntax.InterfaceMemberDeclarationConstant v1 -> (Lists.bind (Syntax.constantDeclarationVariables v1) (splitConstantInitializer_splitVar (Syntax.constantDeclarationModifiers v1) (Syntax.constantDeclarationType v1)))
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
    Syntax.VariableInitializerExpression v1 ->  
      let varName = (javaIdentifierToString (Syntax.variableDeclaratorIdIdentifier vid)) 
          helperName = (Strings.cat2 "_init_" varName)
          callExpr = (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocation Nothing (Syntax.Identifier helperName) []))
          field = (Syntax.InterfaceMemberDeclarationConstant (Syntax.ConstantDeclaration {
                  Syntax.constantDeclarationModifiers = mods,
                  Syntax.constantDeclarationType = utype,
                  Syntax.constantDeclarationVariables = [
                    Syntax.VariableDeclarator {
                      Syntax.variableDeclaratorId = vid,
                      Syntax.variableDeclaratorInitializer = (Just (Syntax.VariableInitializerExpression callExpr))}]}))
          returnSt = (Syntax.BlockStatementStatement (Utils_.javaReturnStatement (Just v1)))
          resultType = (Syntax.ResultType utype)
          helper = (Utils_.interfaceMethodDeclaration [
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
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionLambda v2 -> (Math.add 1 (classifyDataTerm_countLambdaParams (Core.lambdaBody v2)))
    _ -> 0) v1)
  Core.TermLet v1 -> (classifyDataTerm_countLambdaParams (Core.letBody v1))
  _ -> 0) (Rewriting.deannotateTerm t))

classifyDataTerm_stripTypeLambdas :: (Core.Term -> Core.Term)
classifyDataTerm_stripTypeLambdas t = ((\x -> case x of
  Core.TermTypeLambda v1 -> (classifyDataTerm_stripTypeLambdas (Core.typeLambdaBody v1))
  _ -> t) (Rewriting.deannotateTerm t))

classifyDataReference :: (Core.Name -> Compute.Flow Graph.Graph Helpers.JavaSymbolClass)
classifyDataReference name = (Flows.bind (Lexical.dereferenceElement name) (\mel -> Maybes.cases mel (Flows.pure Helpers.JavaSymbolClassLocalVariable) (\el -> Maybes.cases (Core.bindingType el) (Flows.fail (Strings.cat2 "no type scheme for element " (Core.unName (Core.bindingName el)))) (\ts -> Flows.pure (classifyDataTerm ts (Core.bindingTerm el))))))

encodeType :: (Helpers.Aliases -> S.Set Core.Name -> Core.Type -> Compute.Flow Graph.Graph Syntax.Type)
encodeType aliases boundVars t =  
  let inScopeTypeParams = (Helpers.aliasesInScopeTypeParams aliases)
  in  
    let typeVarSubst = (Helpers.aliasesTypeVarSubst aliases)
    in ((\x -> case x of
      Core.TypeApplication v1 -> (Flows.bind (encodeType aliases boundVars (Core.applicationTypeFunction v1)) (\jlhs -> Flows.bind (Flows.bind (encodeType aliases boundVars (Core.applicationTypeArgument v1)) (\jt_ -> Utils_.javaTypeToJavaReferenceType jt_)) (\jrhs -> Utils_.addJavaTypeParameter jrhs jlhs)))
      Core.TypeFunction v1 -> (Flows.bind (Flows.bind (encodeType aliases boundVars (Core.functionTypeDomain v1)) (\jt_ -> Utils_.javaTypeToJavaReferenceType jt_)) (\jdom -> Flows.bind (Flows.bind (encodeType aliases boundVars (Core.functionTypeCodomain v1)) (\jt_ -> Utils_.javaTypeToJavaReferenceType jt_)) (\jcod -> Flows.pure (Utils_.javaRefType [
        jdom,
        jcod] Names.javaUtilFunctionPackageName "Function"))))
      Core.TypeForall v1 -> (Flows.bind (encodeType aliases (Sets.insert (Core.forallTypeParameter v1) boundVars) (Core.forallTypeBody v1)) (\jbody -> Utils_.addJavaTypeParameter (Utils_.javaTypeVariable (Core.unName (Core.forallTypeParameter v1))) jbody))
      Core.TypeList v1 -> (Flows.bind (encodeType aliases boundVars v1) (\jet -> Flows.bind (Flows.bind (Flows.pure jet) (\jt_ -> Utils_.javaTypeToJavaReferenceType jt_)) (\rt -> Flows.pure (Utils_.javaRefType [
        rt] Names.javaUtilPackageName "List"))))
      Core.TypeLiteral v1 -> (encodeLiteralType v1)
      Core.TypeEither v1 -> (Flows.bind (Flows.bind (encodeType aliases boundVars (Core.eitherTypeLeft v1)) (\jt_ -> Utils_.javaTypeToJavaReferenceType jt_)) (\jlt -> Flows.bind (Flows.bind (encodeType aliases boundVars (Core.eitherTypeRight v1)) (\jt_ -> Utils_.javaTypeToJavaReferenceType jt_)) (\jrt -> Flows.pure (Utils_.javaRefType [
        jlt,
        jrt] Names.hydraUtilPackageName "Either"))))
      Core.TypeMap v1 -> (Flows.bind (Flows.bind (encodeType aliases boundVars (Core.mapTypeKeys v1)) (\jt_ -> Utils_.javaTypeToJavaReferenceType jt_)) (\jkt -> Flows.bind (Flows.bind (encodeType aliases boundVars (Core.mapTypeValues v1)) (\jt_ -> Utils_.javaTypeToJavaReferenceType jt_)) (\jvt -> Flows.pure (Utils_.javaRefType [
        jkt,
        jvt] Names.javaUtilPackageName "Map"))))
      Core.TypePair v1 -> (Flows.bind (Flows.bind (encodeType aliases boundVars (Core.pairTypeFirst v1)) (\jt_ -> Utils_.javaTypeToJavaReferenceType jt_)) (\jfirst -> Flows.bind (Flows.bind (encodeType aliases boundVars (Core.pairTypeSecond v1)) (\jt_ -> Utils_.javaTypeToJavaReferenceType jt_)) (\jsecond -> Flows.pure (Utils_.javaRefType [
        jfirst,
        jsecond] Names.hydraUtilPackageName "Tuple.Tuple2"))))
      Core.TypeUnit -> (Flows.pure (Utils_.javaRefType [] Names.javaLangPackageName "Void"))
      Core.TypeRecord v1 -> (Logic.ifElse (Logic.and (Equality.equal (Core.rowTypeTypeName v1) (Core.Name "hydra.core.Unit")) (Lists.null (Core.rowTypeFields v1))) (Flows.pure (Utils_.javaRefType [] Names.javaLangPackageName "Void")) (Flows.pure (Syntax.TypeReference (Utils_.nameToJavaReferenceType aliases True (javaTypeArgumentsForType t) (Core.rowTypeTypeName v1) Nothing))))
      Core.TypeMaybe v1 -> (Flows.bind (Flows.bind (encodeType aliases boundVars v1) (\jt_ -> Utils_.javaTypeToJavaReferenceType jt_)) (\jot -> Flows.pure (Utils_.javaRefType [
        jot] Names.hydraUtilPackageName "Maybe")))
      Core.TypeSet v1 -> (Flows.bind (Flows.bind (encodeType aliases boundVars v1) (\jt_ -> Utils_.javaTypeToJavaReferenceType jt_)) (\jst -> Flows.pure (Utils_.javaRefType [
        jst] Names.javaUtilPackageName "Set")))
      Core.TypeUnion v1 -> (Flows.pure (Syntax.TypeReference (Utils_.nameToJavaReferenceType aliases True (javaTypeArgumentsForType t) (Core.rowTypeTypeName v1) Nothing)))
      Core.TypeVariable v1 ->  
        let name = (Maybes.fromMaybe v1 (Maps.lookup v1 typeVarSubst))
        in (Flows.bind (encodeType_resolveIfTypedef aliases boundVars inScopeTypeParams name) (\resolved -> Maybes.cases resolved (Flows.pure (Logic.ifElse (Logic.or (Sets.member name boundVars) (Sets.member name inScopeTypeParams)) (Syntax.TypeReference (Utils_.javaTypeVariable (Core.unName name))) (Logic.ifElse (isLambdaBoundVariable name) (Syntax.TypeReference (Utils_.javaTypeVariable (Core.unName name))) (Logic.ifElse (isUnresolvedInferenceVar name) (Syntax.TypeReference (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass (Utils_.javaClassType [] Names.javaLangPackageName "Object")))) (Syntax.TypeReference (Utils_.nameToJavaReferenceType aliases True [] name Nothing)))))) (\resolvedType -> encodeType aliases boundVars resolvedType)))
      Core.TypeWrap v1 -> (Flows.pure (Syntax.TypeReference (Utils_.nameToJavaReferenceType aliases True [] (Core.wrappedTypeTypeName v1) Nothing)))
      _ -> (Flows.fail (Strings.cat2 "can't encode unsupported type in Java: " (Core___.type_ t)))) (Rewriting.deannotateType t))

encodeType_resolveIfTypedef :: (t0 -> S.Set Core.Name -> S.Set Core.Name -> Core.Name -> Compute.Flow Graph.Graph (Maybe Core.Type))
encodeType_resolveIfTypedef aliases boundVars inScopeTypeParams name = (Logic.ifElse (Logic.or (Sets.member name boundVars) (Sets.member name inScopeTypeParams)) (Flows.pure Nothing) (Logic.ifElse (isLambdaBoundVariable name) (Flows.pure Nothing) (Flows.bind Monads.getState (\g -> Flows.bind (Schemas.graphToInferenceContext g) (\ix ->  
  let schemaTypes = (Typing.inferenceContextSchemaTypes ix)
  in (Maybes.cases (Maps.lookup name schemaTypes) (Flows.pure Nothing) (\ts -> Logic.ifElse (Logic.not (Lists.null (Core.typeSchemeVariables ts))) (Flows.pure Nothing) ((\x -> case x of
    Core.TypeRecord _ -> (Flows.pure Nothing)
    Core.TypeUnion _ -> (Flows.pure Nothing)
    Core.TypeWrap _ -> (Flows.pure Nothing)
    _ -> (Flows.pure (Just (Core.typeSchemeType ts)))) (Rewriting.deannotateType (Core.typeSchemeType ts))))))))))

javaTypeArgumentsForNamedType :: (Core.Name -> Compute.Flow Graph.Graph [Syntax.TypeArgument])
javaTypeArgumentsForNamedType tname = (Flows.map (\typ -> Lists.map (\tp_ -> Utils_.typeParameterToTypeArgument tp_) (javaTypeParametersForType typ)) (Schemas.requireType tname))

encodeLiteral :: (Core.Literal -> Syntax.Expression)
encodeLiteral lit = ((\x -> case x of
  Core.LiteralBinary v1 ->  
    let byteValues = (Literals.binaryToBytes v1)
    in (Utils_.javaArrayCreation Utils_.javaBytePrimitiveType (Just (Utils_.javaArrayInitializer (Lists.map (\w -> Utils_.javaLiteralToJavaExpression (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.int32ToBigint w)))) byteValues))))
  Core.LiteralBoolean v1 -> (encodeLiteral_litExp (Utils_.javaBoolean v1))
  Core.LiteralFloat v1 -> (encodeLiteral_encodeFloat v1)
  Core.LiteralInteger v1 -> (encodeLiteral_encodeInteger v1)
  Core.LiteralString v1 -> (encodeLiteral_litExp (Utils_.javaString v1))) lit)

encodeLiteral_litExp :: (Syntax.Literal -> Syntax.Expression)
encodeLiteral_litExp l = (Utils_.javaLiteralToJavaExpression l)

encodeLiteral_primCast :: (Syntax.PrimitiveType -> Syntax.Expression -> Syntax.Expression)
encodeLiteral_primCast pt expr = (Utils_.javaCastExpressionToJavaExpression (Utils_.javaCastPrimitive pt (Utils_.javaExpressionToJavaUnaryExpression expr)))

encodeLiteral_encodeFloat :: (Core.FloatValue -> Syntax.Expression)
encodeLiteral_encodeFloat f = ((\x -> case x of
  Core.FloatValueBigfloat v1 -> (Utils_.javaConstructorCall (Utils_.javaConstructorName (Syntax.Identifier "java.math.BigDecimal") Nothing) [
    encodeLiteral (Core.LiteralString (Literals.showBigfloat v1))] Nothing)
  Core.FloatValueFloat32 v1 -> (encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeFloatingPoint Syntax.FloatingPointTypeFloat)) (encodeLiteral_litExp (Syntax.LiteralFloatingPoint (Syntax.FloatingPointLiteral (Literals.float32ToBigfloat v1)))))
  Core.FloatValueFloat64 v1 -> (encodeLiteral_litExp (Syntax.LiteralFloatingPoint (Syntax.FloatingPointLiteral (Literals.float64ToBigfloat v1))))) f)

encodeLiteral_encodeInteger :: (Core.IntegerValue -> Syntax.Expression)
encodeLiteral_encodeInteger i = ((\x -> case x of
  Core.IntegerValueBigint v1 -> (Utils_.javaConstructorCall (Utils_.javaConstructorName (Syntax.Identifier "java.math.BigInteger") Nothing) [
    encodeLiteral (Core.LiteralString (Literals.showBigint v1))] Nothing)
  Core.IntegerValueInt8 v1 -> (encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeByte)) (encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.int8ToBigint v1)))))
  Core.IntegerValueInt16 v1 -> (encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeShort)) (encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.int16ToBigint v1)))))
  Core.IntegerValueInt32 v1 -> (encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.int32ToBigint v1))))
  Core.IntegerValueInt64 v1 -> (encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeLong)) (encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.int64ToBigint v1)))))
  Core.IntegerValueUint8 v1 -> (encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeShort)) (encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.uint8ToBigint v1)))))
  Core.IntegerValueUint16 v1 -> (encodeLiteral_litExp (Syntax.LiteralCharacter v1))
  Core.IntegerValueUint32 v1 -> (encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeLong)) (encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.uint32ToBigint v1)))))
  Core.IntegerValueUint64 v1 -> (Utils_.javaConstructorCall (Utils_.javaConstructorName (Syntax.Identifier "java.math.BigInteger") Nothing) [
    encodeLiteral (Core.LiteralString (Literals.showBigint (Literals.uint64ToBigint v1)))] Nothing)) i)

fieldTypeToFormalParam :: (Helpers.Aliases -> Core.FieldType -> Compute.Flow Graph.Graph Syntax.FormalParameter)
fieldTypeToFormalParam aliases ft = (Flows.bind (encodeType aliases Sets.empty (Core.fieldTypeType ft)) (\jt -> Flows.pure (Utils_.javaTypeToJavaFormalParameter jt (Core.fieldTypeName ft))))

applyCastIfSafe :: (Helpers.Aliases -> Core.Type -> Syntax.Expression -> Compute.Flow Graph.Graph Syntax.Expression)
applyCastIfSafe aliases castType expr =  
  let trusted = (Helpers.aliasesTrustedTypeVars aliases)
  in  
    let inScope = (Helpers.aliasesInScopeTypeParams aliases)
    in  
      let castVars = (collectTypeVars castType)
      in  
        let javaTypeVars = (Sets.fromList (Lists.filter (\v -> Logic.or (Sets.member v inScope) (isLambdaBoundVariable v)) (Sets.toList castVars)))
        in  
          let isSafe = (Logic.or (Sets.null trusted) (Logic.or (Sets.null javaTypeVars) (Sets.null (Sets.difference javaTypeVars trusted))))
          in (Logic.ifElse isSafe (Flows.bind (encodeType aliases Sets.empty castType) (\jtype -> Flows.bind (Utils_.javaTypeToJavaReferenceType jtype) (\rt -> Flows.pure (Utils_.javaCastExpressionToJavaExpression (Utils_.javaCastExpression rt (Utils_.javaExpressionToJavaUnaryExpression expr)))))) (Flows.pure expr))

encodeVariable :: (Helpers.JavaEnvironment -> Core.Name -> Compute.Flow Graph.Graph Syntax.Expression)
encodeVariable env name =  
  let aliases = (Helpers.javaEnvironmentAliases env)
  in  
    let jid = (Utils_.javaIdentifier (Core.unName name))
    in (Logic.ifElse (Sets.member name (Helpers.aliasesBranchVars aliases)) (Flows.pure (Utils_.javaFieldAccessToJavaExpression (Syntax.FieldAccess {
      Syntax.fieldAccessQualifier = (Syntax.FieldAccess_QualifierPrimary (Utils_.javaExpressionToJavaPrimary (Utils_.javaIdentifierToJavaExpression jid))),
      Syntax.fieldAccessIdentifier = (Utils_.javaIdentifier Names.valueFieldName)}))) (Logic.ifElse (Logic.and (Equality.equal name (Core.Name (Strings.cat [
      Names.instanceName,
      "_",
      Names.valueFieldName]))) (isRecursiveVariable aliases name)) ( 
      let instanceExpr = (Utils_.javaIdentifierToJavaExpression (Utils_.javaIdentifier Names.instanceName))
      in (Flows.pure (Utils_.javaFieldAccessToJavaExpression (Syntax.FieldAccess {
        Syntax.fieldAccessQualifier = (Syntax.FieldAccess_QualifierPrimary (Utils_.javaExpressionToJavaPrimary instanceExpr)),
        Syntax.fieldAccessIdentifier = (Utils_.javaIdentifier Names.valueFieldName)})))) (Logic.ifElse (Logic.and (isRecursiveVariable aliases name) (Logic.not (isLambdaBoundIn name (Helpers.aliasesLambdaVars aliases)))) (Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocation (Just (Left (Syntax.ExpressionName {
      Syntax.expressionNameQualifier = Nothing,
      Syntax.expressionNameIdentifier = jid}))) (Syntax.Identifier Names.getMethodName) []))) (Logic.ifElse (Logic.and (Sets.member name (Helpers.aliasesThunkedVars aliases)) (Logic.not (isLambdaBoundIn name (Helpers.aliasesLambdaVars aliases)))) (Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocation (Just (Left (Syntax.ExpressionName {
      Syntax.expressionNameQualifier = Nothing,
      Syntax.expressionNameIdentifier = jid}))) (Syntax.Identifier Names.getMethodName) []))) (Logic.ifElse (isLambdaBoundIn name (Helpers.aliasesLambdaVars aliases)) ( 
      let actualName = (findMatchingLambdaVar name (Helpers.aliasesLambdaVars aliases))
      in (Flows.pure (Utils_.javaIdentifierToJavaExpression (Utils_.variableToJavaIdentifier actualName)))) (Flows.bind (classifyDataReference name) (\cls -> (\x -> case x of
      Helpers.JavaSymbolClassHoistedLambda v1 -> (encodeVariable_hoistedLambdaCase aliases name v1)
      Helpers.JavaSymbolClassLocalVariable -> (Flows.pure (Utils_.javaIdentifierToJavaExpression (elementJavaIdentifier False False aliases name)))
      Helpers.JavaSymbolClassConstant -> (Flows.pure (Utils_.javaIdentifierToJavaExpression (elementJavaIdentifier False False aliases name)))
      Helpers.JavaSymbolClassNullaryFunction -> (Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocation Nothing (elementJavaIdentifier False False aliases name) [])))
      Helpers.JavaSymbolClassUnaryFunction -> (Flows.pure (Utils_.javaIdentifierToJavaExpression (elementJavaIdentifier False True aliases name)))) cls)))))))

encodeVariable_buildCurried :: ([Core.Name] -> Syntax.Expression -> Syntax.Expression)
encodeVariable_buildCurried params inner = (Logic.ifElse (Lists.null params) inner (Utils_.javaLambda (Lists.head params) (encodeVariable_buildCurried (Lists.tail params) inner)))

encodeVariable_hoistedLambdaCase :: (Helpers.Aliases -> Core.Name -> Int -> Compute.Flow Graph.Graph Syntax.Expression)
encodeVariable_hoistedLambdaCase aliases name arity =  
  let paramNames = (Lists.map (\i -> Core.Name (Strings.cat2 "p" (Literals.showInt32 i))) (Math.range 0 (Math.sub arity 1)))
  in  
    let paramExprs = (Lists.map (\pn -> Utils_.javaIdentifierToJavaExpression (Utils_.variableToJavaIdentifier pn)) paramNames)
    in  
      let call = (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocation Nothing (elementJavaIdentifier False False aliases name) paramExprs))
      in  
        let lam = (encodeVariable_buildCurried paramNames call)
        in (Flows.bind (Lexical.dereferenceElement name) (\mel -> Maybes.cases mel (Flows.pure lam) (\el -> Maybes.cases (Core.bindingType el) (Flows.pure lam) (\ts ->  
          let typ = (Core.typeSchemeType ts)
          in (Flows.bind (encodeType aliases Sets.empty typ) (\jtype -> Flows.bind (Utils_.javaTypeToJavaReferenceType jtype) (\rt -> Flows.pure (Utils_.javaCastExpressionToJavaExpression (Utils_.javaCastExpression rt (Utils_.javaExpressionToJavaUnaryExpression lam))))))))))

encodeNullaryConstant :: (Helpers.JavaEnvironment -> Core.Type -> Core.Function -> Compute.Flow Graph.Graph Syntax.Expression)
encodeNullaryConstant env typ fun =  
  let aliases = (Helpers.javaEnvironmentAliases env)
  in ((\x -> case x of
    Core.FunctionPrimitive v1 -> (Flows.bind (encodeNullaryConstant_typeArgsFromReturnType aliases typ) (\targs -> Logic.ifElse (Lists.null targs) ( 
      let header = (Syntax.MethodInvocation_HeaderSimple (Syntax.MethodName (elementJavaIdentifier True False aliases v1)))
      in (Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
        Syntax.methodInvocationHeader = header,
        Syntax.methodInvocationArguments = []})))) ( 
      let fullName = (Syntax.unIdentifier (elementJavaIdentifier True False aliases v1))
      in  
        let parts = (Strings.splitOn "." fullName)
        in  
          let className = (Syntax.Identifier (Strings.intercalate "." (Lists.init parts)))
          in  
            let methodName = (Syntax.Identifier (Lists.last parts))
            in (Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocationStaticWithTypeArgs className methodName targs []))))))
    _ -> (Monads.unexpected "nullary function" (Core___.function fun))) fun)

encodeNullaryConstant_typeArgsFromReturnType :: (Helpers.Aliases -> Core.Type -> Compute.Flow Graph.Graph [Syntax.TypeArgument])
encodeNullaryConstant_typeArgsFromReturnType aliases t = ((\x -> case x of
  Core.TypeSet v1 -> (Flows.bind (encodeType aliases Sets.empty v1) (\jst -> Flows.bind (Utils_.javaTypeToJavaReferenceType jst) (\rt -> Flows.pure [
    Syntax.TypeArgumentReference rt])))
  Core.TypeList v1 -> (Flows.bind (encodeType aliases Sets.empty v1) (\jlt -> Flows.bind (Utils_.javaTypeToJavaReferenceType jlt) (\rt -> Flows.pure [
    Syntax.TypeArgumentReference rt])))
  Core.TypeMaybe v1 -> (Flows.bind (encodeType aliases Sets.empty v1) (\jmt -> Flows.bind (Utils_.javaTypeToJavaReferenceType jmt) (\rt -> Flows.pure [
    Syntax.TypeArgumentReference rt])))
  Core.TypeMap v1 -> (Flows.bind (encodeType aliases Sets.empty (Core.mapTypeKeys v1)) (\jkt -> Flows.bind (Utils_.javaTypeToJavaReferenceType jkt) (\rk -> Flows.bind (encodeType aliases Sets.empty (Core.mapTypeValues v1)) (\jvt -> Flows.bind (Utils_.javaTypeToJavaReferenceType jvt) (\rv -> Flows.pure [
    Syntax.TypeArgumentReference rk,
    (Syntax.TypeArgumentReference rv)])))))
  _ -> (Flows.pure [])) (Rewriting.deannotateType t))

buildTypeVarSubst :: (S.Set Core.Name -> Core.Type -> Core.Type -> M.Map Core.Name Core.Name)
buildTypeVarSubst schemeVarSet freshTyp canonTyp = (buildTypeVarSubst_go schemeVarSet (Rewriting.deannotateType freshTyp) (Rewriting.deannotateType canonTyp))

buildTypeVarSubst_go :: (S.Set Core.Name -> Core.Type -> Core.Type -> M.Map Core.Name Core.Name)
buildTypeVarSubst_go svs ft ct =  
  let goSub = (\a -> \b -> buildTypeVarSubst_go svs (Rewriting.deannotateType a) (Rewriting.deannotateType b))
  in ((\x -> case x of
    Core.TypeVariable v1 -> ((\x -> case x of
      Core.TypeVariable v2 -> (Logic.ifElse (Logic.and (Logic.not (Equality.equal v1 v2)) (Sets.member v2 svs)) (Maps.singleton v1 v2) Maps.empty)
      _ -> Maps.empty) ct)
    Core.TypeFunction v1 -> ((\x -> case x of
      Core.TypeFunction v2 -> (Maps.union (goSub (Core.functionTypeDomain v1) (Core.functionTypeDomain v2)) (goSub (Core.functionTypeCodomain v1) (Core.functionTypeCodomain v2)))
      _ -> Maps.empty) ct)
    Core.TypeApplication v1 -> ((\x -> case x of
      Core.TypeApplication v2 -> (Maps.union (goSub (Core.applicationTypeFunction v1) (Core.applicationTypeFunction v2)) (goSub (Core.applicationTypeArgument v1) (Core.applicationTypeArgument v2)))
      _ -> Maps.empty) ct)
    Core.TypeList v1 -> ((\x -> case x of
      Core.TypeList v2 -> (goSub v1 v2)
      _ -> Maps.empty) ct)
    Core.TypeSet v1 -> ((\x -> case x of
      Core.TypeSet v2 -> (goSub v1 v2)
      _ -> Maps.empty) ct)
    Core.TypeMaybe v1 -> ((\x -> case x of
      Core.TypeMaybe v2 -> (goSub v1 v2)
      _ -> Maps.empty) ct)
    Core.TypeMap v1 -> ((\x -> case x of
      Core.TypeMap v2 -> (Maps.union (goSub (Core.mapTypeKeys v1) (Core.mapTypeKeys v2)) (goSub (Core.mapTypeValues v1) (Core.mapTypeValues v2)))
      _ -> Maps.empty) ct)
    Core.TypePair v1 -> ((\x -> case x of
      Core.TypePair v2 -> (Maps.union (goSub (Core.pairTypeFirst v1) (Core.pairTypeFirst v2)) (goSub (Core.pairTypeSecond v1) (Core.pairTypeSecond v2)))
      _ -> Maps.empty) ct)
    Core.TypeEither v1 -> ((\x -> case x of
      Core.TypeEither v2 -> (Maps.union (goSub (Core.eitherTypeLeft v1) (Core.eitherTypeLeft v2)) (goSub (Core.eitherTypeRight v1) (Core.eitherTypeRight v2)))
      _ -> Maps.empty) ct)
    Core.TypeForall v1 -> ((\x -> case x of
      Core.TypeForall v2 -> (goSub (Core.forallTypeBody v1) (Core.forallTypeBody v2))
      _ -> (buildTypeVarSubst_go svs (Rewriting.deannotateType (Core.forallTypeBody v1)) ct)) ct)
    _ -> ((\x -> case x of
      Core.TypeForall v1 -> (buildTypeVarSubst_go svs ft (Rewriting.deannotateType (Core.forallTypeBody v1)))
      _ -> Maps.empty) ct)) ft)

buildTypeSubst :: (S.Set Core.Name -> Core.Type -> Core.Type -> M.Map Core.Name Core.Type)
buildTypeSubst schemeVarSet schemeType actualType = (buildTypeSubst_go schemeVarSet (Rewriting.deannotateType schemeType) (Rewriting.deannotateType actualType))

buildTypeSubst_go :: (S.Set Core.Name -> Core.Type -> Core.Type -> M.Map Core.Name Core.Type)
buildTypeSubst_go svs st at =  
  let goSub = (\a -> \b -> buildTypeSubst_go svs (Rewriting.deannotateType a) (Rewriting.deannotateType b))
  in ((\x -> case x of
    Core.TypeVariable v1 -> (Logic.ifElse (Sets.member v1 svs) (Maps.singleton v1 at) Maps.empty)
    Core.TypeFunction v1 -> ((\x -> case x of
      Core.TypeFunction v2 -> (Maps.union (goSub (Core.functionTypeDomain v1) (Core.functionTypeDomain v2)) (goSub (Core.functionTypeCodomain v1) (Core.functionTypeCodomain v2)))
      _ -> Maps.empty) at)
    Core.TypeApplication v1 -> ((\x -> case x of
      Core.TypeApplication v2 -> (Maps.union (goSub (Core.applicationTypeFunction v1) (Core.applicationTypeFunction v2)) (goSub (Core.applicationTypeArgument v1) (Core.applicationTypeArgument v2)))
      _ -> Maps.empty) at)
    Core.TypeList v1 -> ((\x -> case x of
      Core.TypeList v2 -> (goSub v1 v2)
      _ -> Maps.empty) at)
    Core.TypeSet v1 -> ((\x -> case x of
      Core.TypeSet v2 -> (goSub v1 v2)
      _ -> Maps.empty) at)
    Core.TypeMaybe v1 -> ((\x -> case x of
      Core.TypeMaybe v2 -> (goSub v1 v2)
      _ -> Maps.empty) at)
    Core.TypeMap v1 -> ((\x -> case x of
      Core.TypeMap v2 -> (Maps.union (goSub (Core.mapTypeKeys v1) (Core.mapTypeKeys v2)) (goSub (Core.mapTypeValues v1) (Core.mapTypeValues v2)))
      _ -> Maps.empty) at)
    Core.TypePair v1 -> ((\x -> case x of
      Core.TypePair v2 -> (Maps.union (goSub (Core.pairTypeFirst v1) (Core.pairTypeFirst v2)) (goSub (Core.pairTypeSecond v1) (Core.pairTypeSecond v2)))
      _ -> Maps.empty) at)
    Core.TypeEither v1 -> ((\x -> case x of
      Core.TypeEither v2 -> (Maps.union (goSub (Core.eitherTypeLeft v1) (Core.eitherTypeLeft v2)) (goSub (Core.eitherTypeRight v1) (Core.eitherTypeRight v2)))
      _ -> Maps.empty) at)
    Core.TypeForall v1 -> ((\x -> case x of
      Core.TypeForall v2 -> (goSub (Core.forallTypeBody v1) (Core.forallTypeBody v2))
      _ -> (goSub (Core.forallTypeBody v1) at)) at)
    _ -> Maps.empty) st)

javaEnvGetTC :: (Helpers.JavaEnvironment -> Typing.TypeContext)
javaEnvGetTC env = (Helpers.javaEnvironmentTypeContext env)

javaEnvSetTC :: (Typing.TypeContext -> Helpers.JavaEnvironment -> Helpers.JavaEnvironment)
javaEnvSetTC tc env = Helpers.JavaEnvironment {
  Helpers.javaEnvironmentAliases = (Helpers.javaEnvironmentAliases env),
  Helpers.javaEnvironmentTypeContext = tc}

analyzeJavaFunction :: (Helpers.JavaEnvironment -> Core.Term -> Compute.Flow t0 (Typing.FunctionStructure Helpers.JavaEnvironment))
analyzeJavaFunction = (CoderUtils.analyzeFunctionTerm javaEnvGetTC javaEnvSetTC)

analyzeJavaFunctionNoInfer :: (Helpers.JavaEnvironment -> Core.Term -> Compute.Flow t0 (Typing.FunctionStructure Helpers.JavaEnvironment))
analyzeJavaFunctionNoInfer = (CoderUtils.analyzeFunctionTermNoInfer javaEnvGetTC javaEnvSetTC)

withLambda :: (Helpers.JavaEnvironment -> Core.Lambda -> (Helpers.JavaEnvironment -> t0) -> t0)
withLambda env lam k = (Schemas.withLambdaContext javaEnvGetTC javaEnvSetTC env lam (\env1 ->  
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
              Helpers.javaEnvironmentTypeContext = (Helpers.javaEnvironmentTypeContext env1)}
      in (k env2)))

withTypeLambda :: (Helpers.JavaEnvironment -> Core.TypeLambda -> (Helpers.JavaEnvironment -> t0) -> t0)
withTypeLambda = (Schemas.withTypeLambdaContext javaEnvGetTC javaEnvSetTC)

propagateType :: (Core.Type -> Core.Term -> Core.Term)
propagateType typ term =  
  let setTypeAnn = (\t -> Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ typ)) t)
  in ((\x -> case x of
    Core.TermFunction v1 -> ((\x -> case x of
      Core.FunctionLambda _ ->  
        let annotated = (setTypeAnn term)
        in ((\x -> case x of
          Core.TypeFunction v3 -> (propagateType_propagateIntoLambda (Core.functionTypeCodomain v3) annotated)
          _ -> annotated) (Rewriting.deannotateType typ))
      _ -> (setTypeAnn term)) v1)
    Core.TermLet v1 -> (setTypeAnn (propagateType_rebuildLet term (Core.letBindings v1) (propagateType typ (Core.letBody v1))))
    _ -> (setTypeAnn term)) (Rewriting.deannotateTerm term))

propagateType_propagateIntoLambda :: (Core.Type -> Core.Term -> Core.Term)
propagateType_propagateIntoLambda cod t = ((\x -> case x of
  Core.TermAnnotated v1 -> (Core.TermAnnotated (Core.AnnotatedTerm {
    Core.annotatedTermBody = (propagateType_propagateIntoLambda cod (Core.annotatedTermBody v1)),
    Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)}))
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionLambda v2 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.lambdaParameter v2),
      Core.lambdaDomain = (Core.lambdaDomain v2),
      Core.lambdaBody = (propagateType cod (Core.lambdaBody v2))})))
    _ -> t) v1)
  _ -> t) t)

propagateType_rebuildLet :: (Core.Term -> [Core.Binding] -> Core.Term -> Core.Term)
propagateType_rebuildLet t bindings newBody = ((\x -> case x of
  Core.TermAnnotated v1 -> (Core.TermAnnotated (Core.AnnotatedTerm {
    Core.annotatedTermBody = (propagateType_rebuildLet (Core.annotatedTermBody v1) bindings newBody),
    Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)}))
  Core.TermLet _ -> (Core.TermLet (Core.Let {
    Core.letBindings = bindings,
    Core.letBody = newBody}))
  _ -> t) t)

flattenBindings :: ([Core.Binding] -> [Core.Binding])
flattenBindings bindings = (Lists.bind bindings (\b -> (\x -> case x of
  Core.TermLet v1 -> (Lists.concat2 (flattenBindings (Core.letBindings v1)) [
    Core.Binding {
      Core.bindingName = (Core.bindingName b),
      Core.bindingTerm = (Core.letBody v1),
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
  Core.TypeForall v1 -> ((\x -> case x of
    Core.TypeFunction _ -> True
    _ -> False) (Rewriting.deannotateType (Core.forallTypeBody v1)))
  _ -> False) (Rewriting.deannotateType (Core.typeSchemeType ts))) (Core.bindingType b))

decodeTypeFromTerm :: (Core.Term -> Maybe Core.Type)
decodeTypeFromTerm term = ((\x -> case x of
  Core.TermUnion v1 -> (Logic.ifElse (Equality.equal (Core.injectionTypeName v1) (Core.Name "hydra.core.Type")) ( 
    let fname = (Core.unName (Core.fieldName (Core.injectionField v1)))
    in  
      let fterm = (Core.fieldTerm (Core.injectionField v1))
      in (Logic.ifElse (Equality.equal fname "variable") ((\x -> case x of
        Core.TermWrap v2 -> ((\x -> case x of
          Core.TermLiteral v3 -> ((\x -> case x of
            Core.LiteralString v4 -> (Just (Core.TypeVariable (Core.Name v4)))
            _ -> Nothing) v3)
          _ -> Nothing) (Core.wrappedTermBody v2))
        _ -> Nothing) fterm) (Logic.ifElse (Equality.equal fname "annotated") ((\x -> case x of
        Core.TermRecord v2 -> (Maybes.bind (Lists.safeHead (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.Name "body")) (Core.recordFields v2))) (\bodyField -> decodeTypeFromTerm (Core.fieldTerm bodyField)))
        _ -> Nothing) fterm) (Logic.ifElse (Equality.equal fname "application") ((\x -> case x of
        Core.TermRecord v2 -> (Maybes.bind (Lists.safeHead (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.Name "function")) (Core.recordFields v2))) (\funcField -> Maybes.bind (decodeTypeFromTerm (Core.fieldTerm funcField)) (\func -> Maybes.bind (Lists.safeHead (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.Name "argument")) (Core.recordFields v2))) (\argField -> Maybes.map (\arg -> Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = func,
          Core.applicationTypeArgument = arg})) (decodeTypeFromTerm (Core.fieldTerm argField))))))
        _ -> Nothing) fterm) (Logic.ifElse (Equality.equal fname "function") ((\x -> case x of
        Core.TermRecord v2 -> (Maybes.bind (Lists.safeHead (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.Name "domain")) (Core.recordFields v2))) (\domField -> Maybes.bind (decodeTypeFromTerm (Core.fieldTerm domField)) (\dom -> Maybes.bind (Lists.safeHead (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.Name "codomain")) (Core.recordFields v2))) (\codField -> Maybes.map (\cod -> Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = dom,
          Core.functionTypeCodomain = cod})) (decodeTypeFromTerm (Core.fieldTerm codField))))))
        _ -> Nothing) fterm) (Logic.ifElse (Equality.equal fname "literal") ((\x -> case x of
        Core.TermUnion v2 -> (Logic.ifElse (Equality.equal (Core.unName (Core.fieldName (Core.injectionField v2))) "string") (Just (Core.TypeLiteral Core.LiteralTypeString)) Nothing)
        _ -> Nothing) fterm) Nothing)))))) Nothing)
  _ -> Nothing) (Rewriting.deannotateTerm term))

tryInferFunctionType :: (Core.Function -> Maybe Core.Type)
tryInferFunctionType fun = ((\x -> case x of
  Core.FunctionLambda v1 -> (Maybes.bind (Core.lambdaDomain v1) (\dom ->  
    let mCod = ((\x -> case x of
            Core.TermAnnotated v2 -> (Maybes.bind (Maps.lookup Constants.key_type (Core.annotatedTermAnnotation v2)) (\typeTerm -> decodeTypeFromTerm typeTerm))
            _ -> Nothing) (Core.lambdaBody v1))
    in (Maybes.map (\cod -> Core.TypeFunction (Core.FunctionType {
      Core.functionTypeDomain = dom,
      Core.functionTypeCodomain = cod})) mCod)))
  _ -> Nothing) fun)

collectTypeApps :: (Core.Term -> [Core.Type] -> (Core.Term, [Core.Type]))
collectTypeApps t acc = ((\x -> case x of
  Core.TermTypeApplication v1 -> (collectTypeApps (Core.typeApplicationTermBody v1) (Lists.cons (Core.typeApplicationTermType v1) acc))
  _ -> (Rewriting.deannotateTerm t, acc)) (Rewriting.deannotateTerm t))

collectTypeApps0 :: (Core.Term -> [Core.Type] -> (Core.Term, [Core.Type]))
collectTypeApps0 t acc = ((\x -> case x of
  Core.TermTypeApplication v1 -> (collectTypeApps0 (Core.typeApplicationTermBody v1) (Lists.cons (Core.typeApplicationTermType v1) acc))
  _ -> (t, acc)) (Rewriting.deannotateTerm t))

countFunctionParams :: (Core.Type -> Int)
countFunctionParams t = ((\x -> case x of
  Core.TypeFunction v1 -> (Math.add 1 (countFunctionParams (Core.functionTypeCodomain v1)))
  _ -> 0) (Rewriting.deannotateType t))

peelDomainTypes :: (Int -> Core.Type -> ([Core.Type], Core.Type))
peelDomainTypes n t = (Logic.ifElse (Equality.lte n 0) ([], t) ((\x -> case x of
  Core.TypeFunction v1 ->  
    let rest = (peelDomainTypes (Math.sub n 1) (Core.functionTypeCodomain v1))
    in (Lists.cons (Core.functionTypeDomain v1) (Pairs.first rest), (Pairs.second rest))
  _ -> ([], t)) (Rewriting.deannotateType t)))

unwrapReturnType :: (Core.Type -> Core.Type)
unwrapReturnType t = ((\x -> case x of
  Core.TypeFunction v1 -> (unwrapReturnType (Core.functionTypeCodomain v1))
  Core.TypeApplication v1 -> (unwrapReturnType (Core.applicationTypeArgument v1))
  _ -> t) (Rewriting.deannotateType t))

findPairFirst :: (Core.Type -> Maybe Core.Name)
findPairFirst t = ((\x -> case x of
  Core.TypePair v1 -> ((\x -> case x of
    Core.TypeVariable v2 -> (Just v2)
    _ -> Nothing) (Rewriting.deannotateType (Core.pairTypeFirst v1)))
  _ -> Nothing) (Rewriting.deannotateType t))

extractInOutPair :: (Core.Type -> [(Core.Name, Core.Name)])
extractInOutPair t = ((\x -> case x of
  Core.TypeFunction v1 -> ((\x -> case x of
    Core.TypeVariable v2 ->  
      let retType = (unwrapReturnType (Core.functionTypeCodomain v1))
      in ((\x -> case x of
        Core.TypePair v3 -> ((\x -> case x of
          Core.TypeVariable v4 -> [
            (v2, v4)]
          _ -> []) (Rewriting.deannotateType (Core.pairTypeFirst v3)))
        _ -> []) (Rewriting.deannotateType retType))
    _ -> []) (Rewriting.deannotateType (Core.functionTypeDomain v1)))
  _ -> []) (Rewriting.deannotateType t))

extractDirectReturn :: (S.Set Core.Name -> Core.Type -> [(Core.Name, Core.Name)])
extractDirectReturn tparamSet t = (extractDirectReturn_go tparamSet t)

extractDirectReturn_go :: (S.Set Core.Name -> Core.Type -> [(Core.Name, Core.Name)])
extractDirectReturn_go tparamSet t = ((\x -> case x of
  Core.TypeFunction v1 ->  
    let dom = (Rewriting.deannotateType (Core.functionTypeDomain v1))
    in  
      let cod = (Core.functionTypeCodomain v1)
      in ((\x -> case x of
        Core.TypeVariable v2 -> (Logic.ifElse (Sets.member v2 tparamSet) ((\x -> case x of
          Core.TypeFunction v3 ->  
            let midArg = (Rewriting.deannotateType (Core.functionTypeDomain v3))
            in  
              let retPart = (Rewriting.deannotateType (Core.functionTypeCodomain v3))
              in ((\x -> case x of
                Core.TypeVariable v4 -> (Logic.ifElse (Sets.member v4 tparamSet) [] ((\x -> case x of
                  Core.TypeVariable v5 -> (Logic.ifElse (Sets.member v5 tparamSet) [
                    (v2, v5)] [])
                  _ -> []) retPart))
                _ -> ((\x -> case x of
                  Core.TypeVariable v4 -> (Logic.ifElse (Sets.member v4 tparamSet) [
                    (v2, v4)] [])
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
                        Core.TypeVariable v1 -> (Just v1)
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
  Core.TypeVariable v1 -> ((\x -> case x of
    Core.TypeVariable v2 -> (Equality.equal v1 v2)
    _ -> True) b)
  Core.TypeWrap v1 -> ((\x -> case x of
    Core.TypeWrap v2 -> (Equality.equal (Core.wrappedTypeTypeName v1) (Core.wrappedTypeTypeName v2))
    _ -> True) b)
  _ -> True) a)

isSimpleName :: (Core.Name -> Bool)
isSimpleName name = (Equality.equal (Lists.length (Strings.splitOn "." (Core.unName name))) 1)

filterPhantomTypeArgs :: (Core.Name -> [Core.Type] -> Compute.Flow Graph.Graph [Core.Type])
filterPhantomTypeArgs calleeName allTypeArgs = (Flows.bind (Lexical.dereferenceElement calleeName) (\mel -> Maybes.cases mel (Flows.pure allTypeArgs) (\el -> Maybes.cases (Core.bindingType el) (Flows.pure allTypeArgs) (\ts ->  
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
                  in (Logic.ifElse (Logic.not (Equality.equal (Lists.length schemeVars) (Lists.length allTypeArgs))) (Flows.pure allTypeArgs) (Flows.pure (filterPhantomTypeArgs_filterAndApply allTypeArgs keepFlags overgenSubst)))))))

filterPhantomTypeArgs_filterAndApply :: ([Core.Type] -> [Bool] -> M.Map Core.Name Core.Type -> [Core.Type])
filterPhantomTypeArgs_filterAndApply allTypeArgs keepFlags overgenSubst =  
  let filtered = (Lists.map (\p -> Pairs.first p) (Lists.filter (\p -> Pairs.second p) (Lists.zip allTypeArgs keepFlags)))
  in (Logic.ifElse (Logic.not (Maps.null overgenSubst)) (Lists.map (\t -> substituteTypeVarsWithTypes overgenSubst t) filtered) filtered)

filterByFlags :: ([t0] -> [Bool] -> [t0])
filterByFlags xs flags = (Lists.map (\p -> Pairs.first p) (Lists.filter (\p -> Pairs.second p) (Lists.zip xs flags)))

applySubstSimple :: (M.Map Core.Name Core.Type -> Core.Type -> Core.Type)
applySubstSimple subst t = ((\x -> case x of
  Core.TypeVariable v1 -> (Maps.findWithDefault t v1 subst)
  _ -> t) (Rewriting.deannotateType t))

buildArgSubst :: (S.Set Core.Name -> [Core.Type] -> [t0] -> M.Map Core.Name t0)
buildArgSubst schemeVarSet schemeDoms argTypes = (Maps.fromList (Lists.bind (Lists.zip schemeDoms argTypes) (\p ->  
  let sdom = (Pairs.first p)
  in  
    let argType = (Pairs.second p)
    in ((\x -> case x of
      Core.TypeVariable v1 -> (Logic.ifElse (Sets.member v1 schemeVarSet) [
        (v1, argType)] [])
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

correctTypeAppsWithArgs :: ([Core.Name] -> [Core.Type] -> Core.Type -> [Core.Term] -> Compute.Flow Graph.Graph [Core.Type])
correctTypeAppsWithArgs schemeVars fallbackTypeApps schemeType args =  
  let schemeVarSet = (Sets.fromList schemeVars)
  in  
    let irSubst = (Maps.fromList (Lists.zip schemeVars fallbackTypeApps))
    in  
      let peeled = (peelDomainTypes (Lists.length args) schemeType)
      in  
        let schemeDoms = (Pairs.first peeled)
        in (Flows.bind (Flows.mapList (\arg -> Annotations.getType (Annotations.termAnnotationInternal arg)) args) (\mArgTypes -> Logic.ifElse (Logic.not (Lists.null (Lists.filter (\m -> Maybes.isNothing m) mArgTypes))) (Flows.pure fallbackTypeApps) ( 
          let argTypes = (Lists.bind mArgTypes (\m -> Maybes.cases m [] (\x -> Lists.pure x)))
          in  
            let irDoms = (Lists.map (\d -> applySubstSimple irSubst d) schemeDoms)
            in  
              let domsMatch = (Lists.null (Lists.filter (\p -> Logic.not (typesMatch (Rewriting.deannotateType (Pairs.first p)) (Rewriting.deannotateType (Pairs.second p)))) (Lists.zip irDoms argTypes)))
              in (Logic.ifElse domsMatch (Flows.pure fallbackTypeApps) (Flows.pure (resolveTypeApps schemeVars fallbackTypeApps (buildArgSubst schemeVarSet schemeDoms argTypes)))))))

correctTypeApps :: (t0 -> Core.Name -> [Core.Term] -> [Core.Type] -> Compute.Flow Graph.Graph [Core.Type])
correctTypeApps tc name args fallbackTypeApps = (Flows.bind (Lexical.dereferenceElement name) (\mel -> Maybes.cases mel (Flows.pure fallbackTypeApps) (\el -> Maybes.cases (Core.bindingType el) (Flows.pure fallbackTypeApps) (\ts ->  
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
                            in (Logic.ifElse (Logic.or (Lists.null schemeVars) (Logic.not (Equality.equal (Lists.length schemeVars) (Lists.length filteredFallback)))) (Flows.pure filteredFallback) (correctTypeAppsWithArgs schemeVars filteredFallback schemeType args))))))

buildSubstFromAnnotations_go :: (S.Set Core.Name -> Graph.Graph -> Core.Term -> M.Map Core.Name Core.Name)
buildSubstFromAnnotations_go schemeVarSet g term = ((\x -> case x of
  Core.TermAnnotated v1 ->  
    let body = (Core.annotatedTermBody v1)
    in  
      let anns = (Core.annotatedTermAnnotation v1)
      in  
        let bodySubst = (buildSubstFromAnnotations_go schemeVarSet g body)
        in  
          let annSubst = (Maybes.cases (Maps.lookup Constants.key_type anns) Maps.empty (\typeTerm -> Eithers.either (\_ -> Maps.empty) (\annType -> (\x -> case x of
                  Core.TermFunction v2 -> ((\x -> case x of
                    Core.FunctionLambda v3 -> (Maybes.cases (Core.lambdaDomain v3) Maps.empty (\dom -> (\x -> case x of
                      Core.TypeFunction v4 -> (buildTypeVarSubst schemeVarSet (Core.functionTypeDomain v4) dom)
                      _ -> Maps.empty) (Rewriting.deannotateType annType)))
                    _ -> Maps.empty) v2)
                  _ -> Maps.empty) (Rewriting.deannotateTerm body)) (Core_.type_ g typeTerm)))
          in (Maps.union annSubst bodySubst)
  Core.TermApplication v1 -> (Maps.union (buildSubstFromAnnotations_go schemeVarSet g (Core.applicationFunction v1)) (buildSubstFromAnnotations_go schemeVarSet g (Core.applicationArgument v1)))
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionLambda v2 -> (buildSubstFromAnnotations_go schemeVarSet g (Core.lambdaBody v2))
    Core.FunctionElimination v2 -> ((\x -> case x of
      Core.EliminationUnion v3 ->  
        let defSubst = (Maybes.cases (Core.caseStatementDefault v3) Maps.empty (\d -> buildSubstFromAnnotations_go schemeVarSet g d))
        in  
          let caseSubsts = (Lists.foldl (\acc -> \fld -> Maps.union acc (buildSubstFromAnnotations_go schemeVarSet g (Core.fieldTerm fld))) Maps.empty (Core.caseStatementCases v3))
          in (Maps.union defSubst caseSubsts)
      _ -> Maps.empty) v2)
    _ -> Maps.empty) v1)
  Core.TermLet v1 ->  
    let bindingSubst = (Lists.foldl (\acc -> \b -> Maps.union acc (buildSubstFromAnnotations_go schemeVarSet g (Core.bindingTerm b))) Maps.empty (Core.letBindings v1))
    in (Maps.union bindingSubst (buildSubstFromAnnotations_go schemeVarSet g (Core.letBody v1)))
  Core.TermList v1 -> (Lists.foldl (\acc -> \t -> Maps.union acc (buildSubstFromAnnotations_go schemeVarSet g t)) Maps.empty v1)
  Core.TermMaybe v1 -> (Maybes.cases v1 Maps.empty (\t -> buildSubstFromAnnotations_go schemeVarSet g t))
  Core.TermPair v1 -> (Maps.union (buildSubstFromAnnotations_go schemeVarSet g (Pairs.first v1)) (buildSubstFromAnnotations_go schemeVarSet g (Pairs.second v1)))
  Core.TermRecord v1 -> (Lists.foldl (\acc -> \fld -> Maps.union acc (buildSubstFromAnnotations_go schemeVarSet g (Core.fieldTerm fld))) Maps.empty (Core.recordFields v1))
  Core.TermSet v1 -> (Lists.foldl (\acc -> \t -> Maps.union acc (buildSubstFromAnnotations_go schemeVarSet g t)) Maps.empty (Sets.toList v1))
  Core.TermTypeApplication v1 -> (buildSubstFromAnnotations_go schemeVarSet g (Core.typeApplicationTermBody v1))
  Core.TermTypeLambda v1 -> (buildSubstFromAnnotations_go schemeVarSet g (Core.typeLambdaBody v1))
  Core.TermEither v1 -> (Eithers.either (\t -> buildSubstFromAnnotations_go schemeVarSet g t) (\t -> buildSubstFromAnnotations_go schemeVarSet g t) v1)
  _ -> Maps.empty) term)

buildSubstFromAnnotations :: (S.Set Core.Name -> Core.Term -> Compute.Flow Graph.Graph (M.Map Core.Name Core.Name))
buildSubstFromAnnotations schemeVarSet term = (Flows.bind Monads.getState (\g -> Flows.pure (buildSubstFromAnnotations_go schemeVarSet g term)))

applyOvergenSubstToTermAnnotations_go :: (M.Map Core.Name Core.Type -> Graph.Graph -> Core.Term -> Core.Term)
applyOvergenSubstToTermAnnotations_go subst cx term = ((\x -> case x of
  Core.TermAnnotated v1 ->  
    let inner = (Core.annotatedTermBody v1)
    in  
      let ann = (Core.annotatedTermAnnotation v1)
      in  
        let ann_ = (Maybes.cases (Maps.lookup Constants.key_type ann) ann (\typeTerm -> Eithers.either (\_ -> ann) (\t ->  
                let t_ = (substituteTypeVarsWithTypes subst t)
                in (Maps.insert Constants.key_type (Core__.type_ t_) ann)) (Core_.type_ cx typeTerm)))
        in (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (applyOvergenSubstToTermAnnotations_go subst cx inner),
          Core.annotatedTermAnnotation = ann_}))
  Core.TermApplication v1 -> (Core.TermApplication (Core.Application {
    Core.applicationFunction = (applyOvergenSubstToTermAnnotations_go subst cx (Core.applicationFunction v1)),
    Core.applicationArgument = (applyOvergenSubstToTermAnnotations_go subst cx (Core.applicationArgument v1))}))
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionLambda v2 -> (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
      Core.lambdaParameter = (Core.lambdaParameter v2),
      Core.lambdaDomain = (Maybes.map (\d -> substituteTypeVarsWithTypes subst d) (Core.lambdaDomain v2)),
      Core.lambdaBody = (applyOvergenSubstToTermAnnotations_go subst cx (Core.lambdaBody v2))})))
    Core.FunctionElimination v2 -> ((\x -> case x of
      Core.EliminationUnion v3 -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
        Core.caseStatementTypeName = (Core.caseStatementTypeName v3),
        Core.caseStatementDefault = (Maybes.map (\d -> applyOvergenSubstToTermAnnotations_go subst cx d) (Core.caseStatementDefault v3)),
        Core.caseStatementCases = (Lists.map (\fld -> Core.Field {
          Core.fieldName = (Core.fieldName fld),
          Core.fieldTerm = (applyOvergenSubstToTermAnnotations_go subst cx (Core.fieldTerm fld))}) (Core.caseStatementCases v3))}))))
      _ -> term) v2)
    _ -> term) v1)
  Core.TermLet v1 -> (Core.TermLet (Core.Let {
    Core.letBindings = (Lists.map (\b -> Core.Binding {
      Core.bindingName = (Core.bindingName b),
      Core.bindingTerm = (applyOvergenSubstToTermAnnotations_go subst cx (Core.bindingTerm b)),
      Core.bindingType = (Core.bindingType b)}) (Core.letBindings v1)),
    Core.letBody = (applyOvergenSubstToTermAnnotations_go subst cx (Core.letBody v1))}))
  Core.TermTypeApplication v1 -> (Core.TermTypeApplication (Core.TypeApplicationTerm {
    Core.typeApplicationTermBody = (applyOvergenSubstToTermAnnotations_go subst cx (Core.typeApplicationTermBody v1)),
    Core.typeApplicationTermType = (substituteTypeVarsWithTypes subst (Core.typeApplicationTermType v1))}))
  Core.TermTypeLambda v1 -> (Core.TermTypeLambda (Core.TypeLambda {
    Core.typeLambdaParameter = (Core.typeLambdaParameter v1),
    Core.typeLambdaBody = (applyOvergenSubstToTermAnnotations_go subst cx (Core.typeLambdaBody v1))}))
  _ -> term) term)

applyOvergenSubstToTermAnnotations :: (M.Map Core.Name Core.Type -> Core.Term -> Compute.Flow Graph.Graph Core.Term)
applyOvergenSubstToTermAnnotations subst term0 = (Flows.bind Monads.getState (\cx -> Flows.pure (applyOvergenSubstToTermAnnotations_go subst cx term0)))

javaComparableRefType :: Syntax.ReferenceType
javaComparableRefType = (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass (Syntax.ClassType {
  Syntax.classTypeAnnotations = [],
  Syntax.classTypeQualifier = Syntax.ClassTypeQualifierNone,
  Syntax.classTypeIdentifier = (Utils_.javaTypeIdentifier "Comparable"),
  Syntax.classTypeArguments = []})))

comparableCompareExpr :: (String -> String -> Syntax.Expression)
comparableCompareExpr otherVar fname =  
  let arg = (Utils_.javaExpressionNameToJavaExpression (Utils_.fieldExpression (Utils_.javaIdentifier otherVar) (Utils_.javaIdentifier fname))) 
      castVar = (Syntax.MethodInvocation_VariantPrimary (Utils_.javaExpressionToJavaPrimary (Utils_.javaCastExpressionToJavaExpression (Utils_.javaCastExpression javaComparableRefType (Utils_.javaIdentifierToJavaUnaryExpression (Syntax.Identifier (Utils_.sanitizeJavaName fname)))))))
      header = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
              Syntax.methodInvocation_ComplexVariant = castVar,
              Syntax.methodInvocation_ComplexTypeArguments = [],
              Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier Names.compareToMethodName)}))
  in (Utils_.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
    Syntax.methodInvocationHeader = header,
    Syntax.methodInvocationArguments = [
      arg]}))

arraysCompareExpr :: (String -> String -> Syntax.Expression)
arraysCompareExpr otherVar fname =  
  let header = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
          Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantType (Utils_.javaTypeName (Syntax.Identifier "java.util.Arrays"))),
          Syntax.methodInvocation_ComplexTypeArguments = [],
          Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier "compare")})) 
      arg1 = (Utils_.javaExpressionNameToJavaExpression (Syntax.ExpressionName {
              Syntax.expressionNameQualifier = Nothing,
              Syntax.expressionNameIdentifier = (Syntax.Identifier (Utils_.sanitizeJavaName fname))}))
      arg2 = (Utils_.javaExpressionNameToJavaExpression (Utils_.fieldExpression (Utils_.javaIdentifier otherVar) (Utils_.javaIdentifier fname)))
  in (Utils_.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
    Syntax.methodInvocationHeader = header,
    Syntax.methodInvocationArguments = [
      arg1,
      arg2]}))

hashCodeCompareExpr :: (String -> String -> Syntax.Expression)
hashCodeCompareExpr otherVar fname =  
  let header = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
          Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantType (Utils_.javaTypeName (Syntax.Identifier "Integer"))),
          Syntax.methodInvocation_ComplexTypeArguments = [],
          Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier "compare")})) 
      thisHashCode = (Utils_.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
              Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
                Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantExpression (Syntax.ExpressionName {
                  Syntax.expressionNameQualifier = Nothing,
                  Syntax.expressionNameIdentifier = (Syntax.Identifier (Utils_.sanitizeJavaName fname))})),
                Syntax.methodInvocation_ComplexTypeArguments = [],
                Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier Names.hashCodeMethodName)})),
              Syntax.methodInvocationArguments = []}))
      otherHashCode = (Utils_.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
              Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
                Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantExpression (Utils_.fieldExpression (Utils_.javaIdentifier otherVar) (Utils_.javaIdentifier fname))),
                Syntax.methodInvocation_ComplexTypeArguments = [],
                Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier Names.hashCodeMethodName)})),
              Syntax.methodInvocationArguments = []}))
  in (Utils_.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
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
cmpNotZeroExpr = (Utils_.javaEqualityExpressionToJavaExpression (Syntax.EqualityExpressionNotEqual (Syntax.EqualityExpression_Binary {
  Syntax.equalityExpression_BinaryLhs = lhs,
  Syntax.equalityExpression_BinaryRhs = rhs}))) 
  where 
    lhs = (Utils_.javaRelationalExpressionToJavaEqualityExpression (Utils_.javaPostfixExpressionToJavaRelationalExpression (Syntax.PostfixExpressionName (Syntax.ExpressionName {
      Syntax.expressionNameQualifier = Nothing,
      Syntax.expressionNameIdentifier = (Utils_.javaIdentifier "cmp")}))))
    rhs = (Utils_.javaPostfixExpressionToJavaRelationalExpression (Syntax.PostfixExpressionPrimary (Utils_.javaLiteralToJavaPrimary (Utils_.javaInt 0))))

cmpDeclStatement :: (t0 -> Syntax.BlockStatement)
cmpDeclStatement aliases = (Utils_.variableDeclarationStatement aliases Utils_.javaIntType (Utils_.javaIdentifier "cmp") (Utils_.javaIntExpression 0))

compareAndReturnStmts :: (String -> Core.FieldType -> [Syntax.BlockStatement])
compareAndReturnStmts otherVar f = [
  Syntax.BlockStatementStatement (Utils_.javaAssignmentStatement (Syntax.LeftHandSideExpressionName (Syntax.ExpressionName {
    Syntax.expressionNameQualifier = Nothing,
    Syntax.expressionNameIdentifier = (Utils_.javaIdentifier "cmp")})) (compareFieldExpr otherVar f)),
  (Syntax.BlockStatementStatement (Syntax.StatementIfThen (Syntax.IfThenStatement {
    Syntax.ifThenStatementExpression = cmpNotZeroExpr,
    Syntax.ifThenStatementStatement = (Utils_.javaReturnStatement (Just (Utils_.javaExpressionNameToJavaExpression (Syntax.ExpressionName {
      Syntax.expressionNameQualifier = Nothing,
      Syntax.expressionNameIdentifier = (Utils_.javaIdentifier "cmp")}))))})))]

compareToBody :: (t0 -> String -> [Core.FieldType] -> [Syntax.BlockStatement])
compareToBody aliases otherVar fields = (Logic.ifElse (Lists.null fields) [
  Syntax.BlockStatementStatement (Utils_.javaReturnStatement (Just (Utils_.javaIntExpression 0)))] (Logic.ifElse (Equality.equal (Lists.length fields) 1) [
  Syntax.BlockStatementStatement (Utils_.javaReturnStatement (Just (compareFieldExpr otherVar (Lists.head fields))))] (Lists.concat2 [
  cmpDeclStatement aliases] (Lists.concat2 (Lists.concat (Lists.map (\f -> compareAndReturnStmts otherVar f) (Lists.init fields))) [
  Syntax.BlockStatementStatement (Utils_.javaReturnStatement (Just (compareFieldExpr otherVar (Lists.last fields))))]))))

tagCompareExpr :: Syntax.Expression
tagCompareExpr = (Utils_.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
  Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
    Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantPrimary (Utils_.javaMethodInvocationToJavaPrimary thisGetName)),
    Syntax.methodInvocation_ComplexTypeArguments = [],
    Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier Names.compareToMethodName)})),
  Syntax.methodInvocationArguments = [
    Utils_.javaMethodInvocationToJavaExpression otherGetName]})) 
  where 
    thisGetClass = Syntax.MethodInvocation {
      Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
        Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantPrimary (Utils_.javaExpressionToJavaPrimary Utils_.javaThis)),
        Syntax.methodInvocation_ComplexTypeArguments = [],
        Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier "getClass")})),
      Syntax.methodInvocationArguments = []}
    thisGetName = Syntax.MethodInvocation {
      Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
        Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantPrimary (Utils_.javaMethodInvocationToJavaPrimary thisGetClass)),
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
        Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantPrimary (Utils_.javaMethodInvocationToJavaPrimary otherGetClass)),
        Syntax.methodInvocation_ComplexTypeArguments = [],
        Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier "getName")})),
      Syntax.methodInvocationArguments = []}

tagCmpNotZeroExpr :: Syntax.Expression
tagCmpNotZeroExpr = (Utils_.javaEqualityExpressionToJavaExpression (Syntax.EqualityExpressionNotEqual (Syntax.EqualityExpression_Binary {
  Syntax.equalityExpression_BinaryLhs = lhs,
  Syntax.equalityExpression_BinaryRhs = rhs}))) 
  where 
    lhs = (Utils_.javaRelationalExpressionToJavaEqualityExpression (Utils_.javaPostfixExpressionToJavaRelationalExpression (Syntax.PostfixExpressionName (Syntax.ExpressionName {
      Syntax.expressionNameQualifier = Nothing,
      Syntax.expressionNameIdentifier = (Utils_.javaIdentifier "tagCmp")}))))
    rhs = (Utils_.javaPostfixExpressionToJavaRelationalExpression (Syntax.PostfixExpressionPrimary (Utils_.javaLiteralToJavaPrimary (Utils_.javaInt 0))))

recordCompareToMethod :: (Helpers.Aliases -> t0 -> Core.Name -> [Core.FieldType] -> Syntax.ClassBodyDeclaration)
recordCompareToMethod aliases tparams elName fields =  
  let anns = [
          Utils_.overrideAnnotation,
          Utils_.suppressWarningsUncheckedAnnotation] 
      mods = [
              Syntax.MethodModifierPublic]
      param = (Utils_.javaTypeToJavaFormalParameter (Utils_.javaTypeFromTypeName aliases elName) (Core.Name Names.otherInstanceName))
      result = (Utils_.javaTypeToJavaResult Utils_.javaIntType)
  in (Utils_.methodDeclaration mods [] anns Names.compareToMethodName [
    param] result (Just (compareToBody aliases Names.otherInstanceName fields)))

variantCompareToMethod :: (Helpers.Aliases -> t0 -> Core.Name -> Core.Name -> [Core.FieldType] -> Syntax.ClassBodyDeclaration)
variantCompareToMethod aliases tparams parentName variantName fields =  
  let anns = [
          Utils_.overrideAnnotation,
          Utils_.suppressWarningsUncheckedAnnotation] 
      mods = [
              Syntax.MethodModifierPublic]
      param = (Utils_.javaTypeToJavaFormalParameter (Utils_.javaTypeFromTypeName aliases parentName) (Core.Name Names.otherInstanceName))
      result = (Utils_.javaTypeToJavaResult Utils_.javaIntType)
      varTmpName = "o"
      tagDeclStmt = (Utils_.variableDeclarationStatement aliases Utils_.javaIntType (Utils_.javaIdentifier "tagCmp") tagCompareExpr)
      tagReturnStmt = (Syntax.BlockStatementStatement (Syntax.StatementIfThen (Syntax.IfThenStatement {
              Syntax.ifThenStatementExpression = tagCmpNotZeroExpr,
              Syntax.ifThenStatementStatement = (Utils_.javaReturnStatement (Just (Utils_.javaExpressionNameToJavaExpression (Syntax.ExpressionName {
                Syntax.expressionNameQualifier = Nothing,
                Syntax.expressionNameIdentifier = (Utils_.javaIdentifier "tagCmp")}))))})))
      variantJavaType = (Utils_.javaTypeFromTypeName aliases variantName)
      castOtherExpr = (Utils_.javaCastExpressionToJavaExpression (Utils_.javaCastExpression (Utils_.nameToJavaReferenceType aliases False [] variantName Nothing) (Utils_.javaIdentifierToJavaUnaryExpression (Syntax.Identifier Names.otherInstanceName))))
      castDeclStmt = (Utils_.variableDeclarationStatement aliases variantJavaType (Utils_.javaIdentifier varTmpName) castOtherExpr)
      emptyReturn = [
              Syntax.BlockStatementStatement (Utils_.javaReturnStatement (Just (Utils_.javaIntExpression 0)))]
      valueCompareStmt = (Logic.ifElse (Lists.null fields) emptyReturn (Lists.concat2 [
              castDeclStmt] (compareToBody aliases varTmpName fields)))
      body = (Lists.concat2 [
              tagDeclStmt,
              tagReturnStmt] valueCompareStmt)
  in (Utils_.methodDeclaration mods [] anns Names.compareToMethodName [
    param] result (Just body))

recordMemberVar :: (Helpers.Aliases -> Core.FieldType -> Compute.Flow Graph.Graph Syntax.ClassBodyDeclaration)
recordMemberVar aliases ft =  
  let mods = [
          Syntax.FieldModifierPublic,
          Syntax.FieldModifierFinal] 
      fname = (Core.fieldTypeName ft)
      ftype = (Core.fieldTypeType ft)
  in (Flows.bind (encodeType aliases Sets.empty ftype) (\jt -> Flows.pure (Utils_.javaMemberField mods jt (Utils_.fieldNameToJavaVariableDeclarator fname))))

recordWithMethod :: (Helpers.Aliases -> Core.Name -> [Core.FieldType] -> Core.FieldType -> Compute.Flow Graph.Graph Syntax.ClassBodyDeclaration)
recordWithMethod aliases elName fields field =  
  let mods = [
          Syntax.MethodModifierPublic] 
      anns = []
      methodName = (Strings.cat2 "with" (Formatting.nonAlnumToUnderscores (Formatting.capitalize (Core.unName (Core.fieldTypeName field)))))
      result = (Utils_.referenceTypeToResult (Utils_.nameToJavaReferenceType aliases False [] elName Nothing))
      consId = (Syntax.Identifier (Utils_.sanitizeJavaName (Names_.localNameOf elName)))
      fieldArgs = (Lists.map (\f -> Utils_.fieldNameToJavaExpression (Core.fieldTypeName f)) fields)
      returnStmt = (Syntax.BlockStatementStatement (Utils_.javaReturnStatement (Just (Utils_.javaConstructorCall (Utils_.javaConstructorName consId Nothing) fieldArgs Nothing))))
  in (Flows.bind (fieldTypeToFormalParam aliases field) (\param -> Flows.pure (Utils_.methodDeclaration mods [] anns methodName [
    param] result (Just [
    returnStmt]))))

recordConstructor :: (Helpers.Aliases -> Core.Name -> [Core.FieldType] -> Compute.Flow Graph.Graph Syntax.ClassBodyDeclaration)
recordConstructor aliases elName fields =  
  let assignStmts = (Lists.map (\f -> Syntax.BlockStatementStatement (Utils_.toAssignStmt (Core.fieldTypeName f))) fields)
  in (Flows.bind (Flows.mapList (\f -> fieldTypeToFormalParam aliases f) fields) (\params -> Flows.pure (Utils_.makeConstructor aliases elName False params assignStmts)))

eqClause :: (String -> Core.FieldType -> Syntax.InclusiveOrExpression)
eqClause tmpName ft =  
  let fname = (Core.unName (Core.fieldTypeName ft)) 
      ftype = (Core.fieldTypeType ft)
  in (Logic.ifElse (isBinaryType ftype) (arraysEqualsClause tmpName fname) (Logic.ifElse (isBigNumericType ftype) (compareToZeroClause tmpName fname) (equalsClause tmpName fname)))

equalsClause :: (String -> String -> Syntax.InclusiveOrExpression)
equalsClause tmpName fname =  
  let thisArg = (Utils_.javaExpressionNameToJavaExpression (Utils_.fieldExpression (Syntax.Identifier "this") (Utils_.javaIdentifier fname))) 
      otherArg = (Utils_.javaExpressionNameToJavaExpression (Utils_.fieldExpression (Utils_.javaIdentifier tmpName) (Utils_.javaIdentifier fname)))
      header = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
              Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantType (Utils_.javaTypeName (Syntax.Identifier "java.util.Objects"))),
              Syntax.methodInvocation_ComplexTypeArguments = [],
              Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier Names.equalsMethodName)}))
  in (Utils_.javaPostfixExpressionToJavaInclusiveOrExpression (Utils_.javaMethodInvocationToJavaPostfixExpression (Syntax.MethodInvocation {
    Syntax.methodInvocationHeader = header,
    Syntax.methodInvocationArguments = [
      thisArg,
      otherArg]})))

arraysEqualsClause :: (String -> String -> Syntax.InclusiveOrExpression)
arraysEqualsClause tmpName fname =  
  let thisArg = (Utils_.javaExpressionNameToJavaExpression (Utils_.fieldExpression (Syntax.Identifier "this") (Utils_.javaIdentifier fname))) 
      otherArg = (Utils_.javaExpressionNameToJavaExpression (Utils_.fieldExpression (Utils_.javaIdentifier tmpName) (Utils_.javaIdentifier fname)))
      header = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
              Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantType (Utils_.javaTypeName (Syntax.Identifier "java.util.Arrays"))),
              Syntax.methodInvocation_ComplexTypeArguments = [],
              Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier Names.equalsMethodName)}))
  in (Utils_.javaPostfixExpressionToJavaInclusiveOrExpression (Utils_.javaMethodInvocationToJavaPostfixExpression (Syntax.MethodInvocation {
    Syntax.methodInvocationHeader = header,
    Syntax.methodInvocationArguments = [
      thisArg,
      otherArg]})))

compareToZeroClause :: (String -> String -> Syntax.InclusiveOrExpression)
compareToZeroClause tmpName fname =  
  let compareToArg = (Utils_.javaExpressionNameToJavaExpression (Utils_.fieldExpression (Utils_.javaIdentifier tmpName) (Utils_.javaIdentifier fname))) 
      compareToVar = (Syntax.MethodInvocation_VariantExpression (Utils_.fieldExpression (Syntax.Identifier "this") (Utils_.javaIdentifier fname)))
      compareToHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
              Syntax.methodInvocation_ComplexVariant = compareToVar,
              Syntax.methodInvocation_ComplexTypeArguments = [],
              Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier Names.compareToMethodName)}))
      lhs = (Utils_.javaRelationalExpressionToJavaEqualityExpression (Utils_.javaPostfixExpressionToJavaRelationalExpression (Utils_.javaMethodInvocationToJavaPostfixExpression (Syntax.MethodInvocation {
              Syntax.methodInvocationHeader = compareToHeader,
              Syntax.methodInvocationArguments = [
                compareToArg]}))))
      rhs = (Utils_.javaPostfixExpressionToJavaRelationalExpression (Syntax.PostfixExpressionPrimary (Utils_.javaLiteralToJavaPrimary (Utils_.javaInt 0))))
  in (Utils_.javaEqualityExpressionToJavaInclusiveOrExpression (Syntax.EqualityExpressionEqual (Syntax.EqualityExpression_Binary {
    Syntax.equalityExpression_BinaryLhs = lhs,
    Syntax.equalityExpression_BinaryRhs = rhs})))

recordEqualsMethod :: (Helpers.Aliases -> Core.Name -> [Core.FieldType] -> Syntax.ClassBodyDeclaration)
recordEqualsMethod aliases elName fields =  
  let anns = [
          Utils_.overrideAnnotation] 
      mods = [
              Syntax.MethodModifierPublic]
      param = (Utils_.javaTypeToJavaFormalParameter (Utils_.javaRefType [] Nothing "Object") (Core.Name Names.otherInstanceName))
      result = (Utils_.javaTypeToJavaResult Utils_.javaBooleanType)
      tmpName = "o"
      instanceOfStmt = (Syntax.BlockStatementStatement (Syntax.StatementIfThen (Syntax.IfThenStatement {
              Syntax.ifThenStatementExpression = (Utils_.javaUnaryExpressionToJavaExpression (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusNot (Utils_.javaRelationalExpressionToJavaUnaryExpression (Utils_.javaInstanceOf (Utils_.javaIdentifierToJavaRelationalExpression (Utils_.javaIdentifier Names.otherInstanceName)) (Utils_.nameToJavaReferenceType aliases False [] elName Nothing)))))),
              Syntax.ifThenStatementStatement = (Utils_.javaReturnStatement (Just (Utils_.javaBooleanExpression False)))})))
      castStmt = (Utils_.variableDeclarationStatement aliases (Utils_.javaTypeFromTypeName aliases elName) (Utils_.javaIdentifier tmpName) (Utils_.javaCastExpressionToJavaExpression (Utils_.javaCastExpression (Utils_.nameToJavaReferenceType aliases False [] elName Nothing) (Utils_.javaIdentifierToJavaUnaryExpression (Syntax.Identifier (Utils_.sanitizeJavaName Names.otherInstanceName))))))
      returnAllFieldsEqual = (Syntax.BlockStatementStatement (Utils_.javaReturnStatement (Just (Logic.ifElse (Lists.null fields) (Utils_.javaBooleanExpression True) (Utils_.javaConditionalAndExpressionToJavaExpression (Syntax.ConditionalAndExpression (Lists.map (\f -> eqClause tmpName f) fields)))))))
  in (Utils_.methodDeclaration mods [] anns Names.equalsMethodName [
    param] result (Just [
    instanceOfStmt,
    castStmt,
    returnAllFieldsEqual]))

hashCodeMultPair :: (Integer -> Core.Name -> Syntax.MultiplicativeExpression)
hashCodeMultPair i fname =  
  let fnameStr = (Core.unName fname) 
      lhs = (Syntax.MultiplicativeExpressionUnary (Utils_.javaPrimaryToJavaUnaryExpression (Utils_.javaLiteralToJavaPrimary (Utils_.javaInt i))))
      rhs = (Utils_.javaPostfixExpressionToJavaUnaryExpression (Utils_.javaMethodInvocationToJavaPostfixExpression (Syntax.MethodInvocation {
              Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
                Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantType (Utils_.javaTypeName (Syntax.Identifier "java.util.Objects"))),
                Syntax.methodInvocation_ComplexTypeArguments = [],
                Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier Names.hashCodeMethodName)})),
              Syntax.methodInvocationArguments = [
                Utils_.javaExpressionNameToJavaExpression (Syntax.ExpressionName {
                  Syntax.expressionNameQualifier = Nothing,
                  Syntax.expressionNameIdentifier = (Syntax.Identifier (Utils_.sanitizeJavaName fnameStr))})]})))
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
          Utils_.overrideAnnotation] 
      mods = [
              Syntax.MethodModifierPublic]
      result = (Utils_.javaTypeToJavaResult Utils_.javaIntType)
      returnSum = (Syntax.BlockStatementStatement (Logic.ifElse (Lists.null fields) (Utils_.javaReturnStatement (Just (Utils_.javaIntExpression 0))) (Utils_.javaReturnStatement (Just (Utils_.javaAdditiveExpressionToJavaExpression (Utils_.addExpressions (Lists.zipWith hashCodeMultPair first20Primes (Lists.map (\f -> Core.fieldTypeName f) fields))))))))
  in (Utils_.methodDeclaration mods [] anns Names.hashCodeMethodName [] result (Just [
    returnSum]))

constantDecl :: (String -> Helpers.Aliases -> Core.Name -> Compute.Flow Graph.Graph Syntax.ClassBodyDeclarationWithComments)
constantDecl javaName aliases name =  
  let mods = [
          Syntax.FieldModifierPublic,
          Syntax.FieldModifierStatic,
          Syntax.FieldModifierFinal] 
      nameName = (Utils_.nameToJavaName aliases (Core.Name "hydra.core.Name"))
  in (Flows.bind Monads.getState (\g -> Flows.bind (Schemas.graphToTypeContext g) (\tc ->  
    let env = Helpers.JavaEnvironment {
            Helpers.javaEnvironmentAliases = aliases,
            Helpers.javaEnvironmentTypeContext = tc}
    in (Flows.bind (encodeType aliases Sets.empty (Core.TypeVariable (Core.Name "hydra.core.Name"))) (\jt -> Flows.bind (encodeTerm env (Core.TermLiteral (Core.LiteralString (Core.unName name)))) (\arg ->  
      let init = (Syntax.VariableInitializerExpression (Utils_.javaConstructorCall (Utils_.javaConstructorName nameName Nothing) [
              arg] Nothing))
      in  
        let var = (Utils_.javaVariableDeclarator (Syntax.Identifier javaName) (Just init))
        in (Flows.pure (noComment (Utils_.javaMemberField mods jt var)))))))))

constantDeclForFieldType :: (Helpers.Aliases -> Core.FieldType -> Compute.Flow Graph.Graph Syntax.ClassBodyDeclarationWithComments)
constantDeclForFieldType aliases ftyp =  
  let name = (Core.fieldTypeName ftyp) 
      javaName = (Strings.cat2 "FIELD_NAME_" (Formatting.nonAlnumToUnderscores (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionUpperSnake (Core.unName name))))
  in (constantDecl javaName aliases name)

constantDeclForTypeName :: (Helpers.Aliases -> Core.Name -> Compute.Flow Graph.Graph Syntax.ClassBodyDeclarationWithComments)
constantDeclForTypeName aliases name = (constantDecl "TYPE_NAME" aliases name)

declarationForRecordType :: (Bool -> Bool -> Helpers.Aliases -> [Syntax.TypeParameter] -> Core.Name -> [Core.FieldType] -> Compute.Flow Graph.Graph Syntax.ClassDeclaration)
declarationForRecordType isInner isSer aliases tparams elName fields = (declarationForRecordType_ isInner isSer aliases tparams elName Nothing fields)

declarationForRecordType_ :: (Bool -> Bool -> Helpers.Aliases -> [Syntax.TypeParameter] -> Core.Name -> Maybe Core.Name -> [Core.FieldType] -> Compute.Flow Graph.Graph Syntax.ClassDeclaration)
declarationForRecordType_ isInner isSer aliases tparams elName parentName fields = (Flows.bind (Flows.mapList (\f -> recordMemberVar aliases f) fields) (\memberVars -> Flows.bind (Flows.mapList (\p -> addComment (Pairs.first p) (Pairs.second p)) (Lists.zip memberVars fields)) (\memberVars_ -> Flows.bind (Logic.ifElse (Equality.gt (Lists.length fields) 1) (Flows.mapList (\f -> recordWithMethod aliases elName fields f) fields) (Flows.pure [])) (\withMethods -> Flows.bind (recordConstructor aliases elName fields) (\cons -> Flows.bind (Logic.ifElse isInner (Flows.pure []) (Flows.bind (constantDeclForTypeName aliases elName) (\d -> Flows.bind (Flows.mapList (\f -> constantDeclForFieldType aliases f) fields) (\dfields -> Flows.pure (Lists.cons d dfields))))) (\tn ->  
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
      in (Flows.pure (Utils_.javaClassDeclaration aliases tparams elName classModsPublic Nothing ifaces bodyDecls))))))))

takeTypeArgs :: (String -> Int -> [Syntax.Type] -> Compute.Flow t0 [Syntax.TypeArgument])
takeTypeArgs label n tyapps = (Logic.ifElse (Equality.lt (Lists.length tyapps) n) (Monads.unexpected (Strings.cat [
  "needed type arguments for ",
  label,
  ", found too few"]) "takeTypeArgs") (Flows.mapList (\jt -> Flows.bind (Utils_.javaTypeToJavaReferenceType jt) (\rt -> Flows.pure (Syntax.TypeArgumentReference rt))) (Lists.take n tyapps)))

isFieldUnitType :: (Core.Name -> Core.Name -> Compute.Flow Graph.Graph Bool)
isFieldUnitType typeName fieldName = (Flows.bind Monads.getState (\g -> Flows.bind (Schemas.graphToInferenceContext g) (\ix ->  
  let schemaTypes = (Typing.inferenceContextSchemaTypes ix)
  in (Maybes.cases (Maps.lookup typeName schemaTypes) (Flows.pure False) (\ts -> (\x -> case x of
    Core.TypeUnion v1 -> (Flows.pure (Maybes.cases (Lists.find (\ft -> Equality.equal (Core.fieldTypeName ft) fieldName) (Core.rowTypeFields v1)) False (\ft -> Schemas.isUnitType (Rewriting.deannotateType (Core.fieldTypeType ft)))))
    _ -> (Flows.pure False)) (Rewriting.deannotateType (Core.typeSchemeType ts)))))))

encodeTerm :: (Helpers.JavaEnvironment -> Core.Term -> Compute.Flow Graph.Graph Syntax.Expression)
encodeTerm env term = (encodeTermInternal env [] [] term)

encodeTermInternal :: (Helpers.JavaEnvironment -> [M.Map Core.Name Core.Term] -> [Syntax.Type] -> Core.Term -> Compute.Flow Graph.Graph Syntax.Expression)
encodeTermInternal env anns tyapps term =  
  let aliases = (Helpers.javaEnvironmentAliases env) 
      tc = (Helpers.javaEnvironmentTypeContext env)
      encode = (\t -> encodeTerm env t)
  in ((\x -> case x of
    Core.TermAnnotated v1 -> (encodeTermInternal env (Lists.cons (Core.annotatedTermAnnotation v1) anns) tyapps (Core.annotatedTermBody v1))
    Core.TermApplication v1 -> (Monads.withTrace "encode application" (encodeApplication env v1))
    Core.TermEither v1 -> (Flows.bind (takeTypeArgs "either" 2 tyapps) (\targs -> Eithers.either (\term1 -> Flows.bind (encode term1) (\expr -> Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocationStaticWithTypeArgs (Syntax.Identifier "hydra.util.Either") (Syntax.Identifier "left") targs [
      expr])))) (\term1 -> Flows.bind (encode term1) (\expr -> Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocationStaticWithTypeArgs (Syntax.Identifier "hydra.util.Either") (Syntax.Identifier "right") targs [
      expr])))) v1))
    Core.TermFunction v1 -> (Monads.withTrace (Strings.cat2 "encode function (" (Strings.cat2 (Core___.function v1) ")")) ( 
      let combinedAnns = (Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns)
      in (Flows.bind (Annotations.getType combinedAnns) (\mt -> Flows.bind (Maybes.cases mt (Maybes.cases (tryInferFunctionType v1) (CoderUtils.tryTypeOf "4" tc term) (\inferredType -> Flows.pure inferredType)) (\t -> Flows.pure t)) (\typ -> (\x -> case x of
        Core.TypeFunction v2 -> (encodeFunction env (Core.functionTypeDomain v2) (Core.functionTypeCodomain v2) v1)
        _ -> (encodeNullaryConstant env typ v1)) (Rewriting.deannotateType typ))))))
    Core.TermLet v1 -> (Monads.withTrace "encode let as block" ( 
      let bindings = (Core.letBindings v1)
      in  
        let body = (Core.letBody v1)
        in (Logic.ifElse (Lists.null bindings) (encode body) (Flows.bind (bindingsToStatements env bindings) (\bindResult ->  
          let bindingStmts = (Pairs.first bindResult)
          in  
            let env2 = (Pairs.second bindResult)
            in (Flows.bind (encodeTerm env2 body) (\jbody ->  
              let returnSt = (Syntax.BlockStatementStatement (Utils_.javaReturnStatement (Just jbody)))
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
                      let tc2 = (Helpers.javaEnvironmentTypeContext env2)
                      in  
                        let aliases2 = (Helpers.javaEnvironmentAliases env2)
                        in (Flows.bind (Annotations.getType combinedAnns) (\mt -> Flows.bind (Maybes.cases mt (CoderUtils.tryTypeOf "let-body" tc2 body) (\t -> Flows.pure t)) (\letType -> Flows.bind (encodeType aliases2 Sets.empty letType) (\jLetType -> Flows.bind (Utils_.javaTypeToJavaReferenceType jLetType) (\rt ->  
                          let supplierRt = (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass (Utils_.javaClassType [
                                  rt] Names.javaUtilFunctionPackageName "Supplier")))
                          in  
                            let castExpr = (Utils_.javaCastExpressionToJavaExpression (Utils_.javaCastExpression supplierRt (Utils_.javaExpressionToJavaUnaryExpression nullaryLambda)))
                            in (Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocation (Just (Right (Utils_.javaExpressionToJavaPrimary castExpr))) (Syntax.Identifier "get") [])))))))))))))))
    Core.TermList v1 -> (Flows.bind (Flows.mapList encode v1) (\jels -> Flows.bind (Logic.ifElse (Lists.null jels) (takeTypeArgs "list" 1 tyapps) (Flows.pure [])) (\targs -> Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocationStaticWithTypeArgs (Syntax.Identifier "java.util.List") (Syntax.Identifier "of") targs jels)))))
    Core.TermLiteral v1 -> (Flows.pure (encodeLiteral v1))
    Core.TermMap v1 -> (Flows.bind (Flows.mapList encode (Maps.keys v1)) (\jkeys -> Flows.bind (Flows.mapList encode (Maps.elems v1)) (\jvals ->  
      let pairExprs = (Lists.map (\kv -> Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocationStatic (Syntax.Identifier "java.util.Map") (Syntax.Identifier "entry") [
              Pairs.first kv,
              (Pairs.second kv)])) (Lists.zip jkeys jvals))
      in (Flows.bind (Logic.ifElse (Maps.null v1) (takeTypeArgs "map" 2 tyapps) (Flows.pure [])) (\targs -> Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocationStaticWithTypeArgs (Syntax.Identifier "java.util.Map") (Syntax.Identifier "ofEntries") targs pairExprs)))))))
    Core.TermMaybe v1 -> (Maybes.cases v1 (Flows.bind (takeTypeArgs "maybe" 1 tyapps) (\targs -> Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocationStaticWithTypeArgs (Syntax.Identifier "hydra.util.Maybe") (Syntax.Identifier "nothing") targs [])))) (\term1 -> Flows.bind (encode term1) (\expr -> Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocationStatic (Syntax.Identifier "hydra.util.Maybe") (Syntax.Identifier "just") [
      expr])))))
    Core.TermPair v1 -> (Flows.bind (encode (Pairs.first v1)) (\jterm1 -> Flows.bind (encode (Pairs.second v1)) (\jterm2 -> Flows.bind (Logic.ifElse (Lists.null tyapps) (Flows.pure Nothing) (Flows.bind (Flows.mapList (\jt -> Utils_.javaTypeToJavaReferenceType jt) tyapps) (\rts -> Flows.pure (Just (Syntax.TypeArgumentsOrDiamondArguments (Lists.map (\rt -> Syntax.TypeArgumentReference rt) rts)))))) (\mtargs -> Flows.pure (Utils_.javaConstructorCall (Utils_.javaConstructorName (Syntax.Identifier "hydra.util.Tuple.Tuple2") mtargs) [
      jterm1,
      jterm2] Nothing)))))
    Core.TermRecord v1 ->  
      let recName = (Core.recordTypeName v1)
      in (Flows.bind (Flows.mapList (\fld -> encode (Core.fieldTerm fld)) (Core.recordFields v1)) (\fieldExprs ->  
        let consId = (Utils_.nameToJavaName aliases recName)
        in (Flows.bind (Logic.ifElse (Lists.null tyapps) (Flows.pure Nothing) (Flows.bind (Flows.mapList (\jt -> Utils_.javaTypeToJavaReferenceType jt) tyapps) (\rts -> Flows.pure (Just (Syntax.TypeArgumentsOrDiamondArguments (Lists.map (\rt -> Syntax.TypeArgumentReference rt) rts)))))) (\mtargs -> Flows.pure (Utils_.javaConstructorCall (Utils_.javaConstructorName consId mtargs) fieldExprs Nothing)))))
    Core.TermSet v1 ->  
      let slist = (Sets.toList v1)
      in (Flows.bind (Flows.mapList encode slist) (\jels -> Logic.ifElse (Sets.null v1) (Flows.bind (takeTypeArgs "set" 1 tyapps) (\targs -> Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocationStaticWithTypeArgs (Syntax.Identifier "java.util.Set") (Syntax.Identifier "of") targs [])))) ( 
        let prim = (Utils_.javaMethodInvocationToJavaPrimary (Utils_.methodInvocationStatic (Syntax.Identifier "java.util.stream.Stream") (Syntax.Identifier "of") jels))
        in  
          let coll = (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocationStatic (Syntax.Identifier "java.util.stream.Collectors") (Syntax.Identifier "toSet") []))
          in (Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocation (Just (Right prim)) (Syntax.Identifier "collect") [
            coll]))))))
    Core.TermTypeLambda v1 -> (withTypeLambda env v1 (\env2 ->  
      let combinedAnns = (Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns)
      in (Flows.bind (Annotations.getType combinedAnns) (\mtyp ->  
        let annotatedBody = (Maybes.cases mtyp (Core.typeLambdaBody v1) (\t -> (\x -> case x of
                Core.TypeForall v2 -> (Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ (Core.forallTypeBody v2))) (Core.typeLambdaBody v1))
                _ -> (Core.typeLambdaBody v1)) t))
        in (encodeTerm env2 annotatedBody)))))
    Core.TermUnion v1 ->  
      let injTypeName = (Core.injectionTypeName v1)
      in  
        let injField = (Core.injectionField v1)
        in  
          let injFieldName = (Core.fieldName injField)
          in  
            let injFieldTerm = (Core.fieldTerm injField)
            in  
              let typeId = (Syntax.unIdentifier (Utils_.nameToJavaName aliases injTypeName))
              in  
                let consId = (Syntax.Identifier (Strings.cat [
                        typeId,
                        ".",
                        (Utils_.sanitizeJavaName (Formatting.capitalize (Core.unName injFieldName)))]))
                in (Flows.bind (isFieldUnitType injTypeName injFieldName) (\fieldIsUnit -> Flows.bind (Logic.ifElse (Logic.or (Schemas.isUnitTerm (Rewriting.deannotateTerm injFieldTerm)) fieldIsUnit) (Flows.pure []) (Flows.bind (encode injFieldTerm) (\ex -> Flows.pure [
                  ex]))) (\args -> Flows.pure (Utils_.javaConstructorCall (Utils_.javaConstructorName consId Nothing) args Nothing))))
    Core.TermVariable v1 -> (encodeVariable env v1)
    Core.TermUnit -> (Flows.pure (Utils_.javaLiteralToJavaExpression Syntax.LiteralNull))
    Core.TermWrap v1 -> (Flows.bind (encode (Core.wrappedTermBody v1)) (\jarg -> Flows.pure (Utils_.javaConstructorCall (Utils_.javaConstructorName (Utils_.nameToJavaName aliases (Core.wrappedTermTypeName v1)) Nothing) [
      jarg] Nothing)))
    Core.TermTypeApplication v1 ->  
      let atyp = (Core.typeApplicationTermType v1)
      in  
        let body = (Core.typeApplicationTermBody v1)
        in (Flows.bind (encodeType aliases Sets.empty atyp) (\jatyp ->  
          let combinedAnns = (Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns)
          in (Flows.bind (Annotations.getType combinedAnns) (\mtyp -> Flows.bind (Maybes.cases mtyp (CoderUtils.tryTypeOf "5" tc term) (\t -> Flows.pure t)) (\typ ->  
            let collected0 = (collectTypeApps0 body [
                    atyp])
            in  
              let innermostBody0 = (Pairs.first collected0)
              in  
                let allTypeArgs0 = (Pairs.second collected0)
                in (Flows.bind (correctCastType innermostBody0 allTypeArgs0 typ) (\correctedTyp ->  
                  let collected = (collectTypeApps body [
                          atyp])
                  in  
                    let innermostBody = (Pairs.first collected)
                    in  
                      let allTypeArgs = (Pairs.second collected)
                      in ((\x -> case x of
                        Core.TermVariable v2 -> (Flows.bind (classifyDataReference v2) (\cls -> typeAppNullaryOrHoisted env aliases anns tyapps jatyp body correctedTyp v2 cls allTypeArgs))
                        _ -> (typeAppFallbackCast env aliases anns tyapps jatyp body correctedTyp)) innermostBody))))))))
    _ -> (Flows.pure (encodeLiteral (Core.LiteralString "Unimplemented term variant")))) term)

annotateLambdaArgs :: (Core.Name -> [Core.Type] -> [Core.Term] -> Compute.Flow Graph.Graph [Core.Term])
annotateLambdaArgs cname tApps argTerms = (Logic.ifElse (Lists.null tApps) (Flows.pure argTerms) (Flows.bind (Flows.bind (Lexical.dereferenceElement cname) (\mel -> Maybes.cases mel (Flows.bind Monads.getState (\g -> Flows.pure (Maybes.map (\prim -> Graph.primitiveType prim) (Maps.lookup cname (Graph.graphPrimitives g))))) (\el -> Flows.pure (Core.bindingType el)))) (\mts -> Maybes.cases mts (Flows.pure argTerms) (\ts ->  
  let schemeType = (Core.typeSchemeType ts)
  in  
    let schemeTypeVars = (collectTypeVars schemeType)
    in  
      let schemeVars = (Lists.filter (\v -> Sets.member v schemeTypeVars) (Core.typeSchemeVariables ts))
      in (Logic.ifElse (Logic.or (Lists.null schemeVars) (Logic.not (Equality.equal (Lists.length schemeVars) (Lists.length tApps)))) (Flows.pure argTerms) ( 
        let subst = (Maps.fromList (Lists.zip schemeVars tApps))
        in  
          let expectedTypes = (peelExpectedTypes subst (Lists.length argTerms) schemeType)
          in (Flows.pure (Lists.zipWith (\arg -> \mExpected -> propagateType mExpected arg) argTerms (Lists.concat2 expectedTypes (Lists.replicate (Lists.length argTerms) (Core.TypeVariable (Core.Name "unused"))))))))))))

applyJavaArg :: (Syntax.Expression -> Syntax.Expression -> Syntax.Expression)
applyJavaArg expr jarg = (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocation (Just (Right (Utils_.javaExpressionToJavaPrimary expr))) (Syntax.Identifier Names.applyMethodName) [
  jarg]))

encodeApplication :: (Helpers.JavaEnvironment -> Core.Application -> Compute.Flow Graph.Graph Syntax.Expression)
encodeApplication env app =  
  let aliases = (Helpers.javaEnvironmentAliases env)
  in  
    let tc = (Helpers.javaEnvironmentTypeContext env)
    in  
      let gathered = (CoderUtils.gatherArgsWithTypeApps (Core.TermApplication app) [] [])
      in  
        let fun = (Pairs.first gathered)
        in  
          let args = (Pairs.first (Pairs.second gathered))
          in  
            let typeApps = (Pairs.second (Pairs.second gathered))
            in (Flows.bind (Annotations.getType (Annotations.termAnnotationInternal fun)) (\mfunTyp -> Flows.bind (Maybes.cases mfunTyp (CoderUtils.tryTypeOf "1" tc fun) (\t -> Flows.pure t)) (\funTyp ->  
              let arity = (Arity.typeArity funTyp)
              in  
                let deannotatedFun = (Rewriting.deannotateTerm fun)
                in  
                  let calleeName = ((\x -> case x of
                          Core.TermFunction v1 -> ((\x -> case x of
                            Core.FunctionPrimitive v2 -> (Just v2)
                            _ -> Nothing) v1)
                          Core.TermVariable v1 -> (Just v1)
                          _ -> Nothing) deannotatedFun)
                  in (Flows.bind (Maybes.cases calleeName (Flows.pure args) (\cname -> annotateLambdaArgs cname typeApps args)) (\annotatedArgs -> (\x -> case x of
                    Core.TermFunction v1 -> ((\x -> case x of
                      Core.FunctionPrimitive v2 ->  
                        let hargs = (Lists.take arity annotatedArgs)
                        in  
                          let rargs = (Lists.drop arity annotatedArgs)
                          in (Flows.bind (functionCall env True v2 hargs []) (\initialCall -> Flows.foldl (\acc -> \h -> Flows.bind (encodeTerm env h) (\jarg -> Flows.pure (applyJavaArg acc jarg))) initialCall rargs))
                      _ -> (encodeApplication_fallback env aliases tc typeApps (Core.applicationFunction app) (Core.applicationArgument app))) v1)
                    Core.TermVariable v1 -> (Logic.ifElse (Logic.and (isRecursiveVariable aliases v1) (Logic.not (isLambdaBoundIn v1 (Helpers.aliasesLambdaVars aliases)))) (encodeApplication_fallback env aliases tc typeApps (Core.applicationFunction app) (Core.applicationArgument app)) (Flows.bind (classifyDataReference v1) (\symClass ->  
                      let methodArity = ((\x -> case x of
                              Helpers.JavaSymbolClassHoistedLambda v2 -> v2
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
                                in (Flows.bind (Logic.ifElse (Lists.null filteredTypeApps) (Flows.pure []) (correctTypeApps tc v1 hargs filteredTypeApps)) (\safeTypeApps -> Flows.bind (filterPhantomTypeArgs v1 safeTypeApps) (\finalTypeApps -> Flows.bind (functionCall env False v1 hargs finalTypeApps) (\initialCall -> Flows.foldl (\acc -> \h -> Flows.bind (encodeTerm env h) (\jarg -> Flows.pure (applyJavaArg acc jarg))) initialCall rargs)))))))
                    _ -> (encodeApplication_fallback env aliases tc typeApps (Core.applicationFunction app) (Core.applicationArgument app))) deannotatedFun)))))

encodeApplication_fallback :: (Helpers.JavaEnvironment -> Helpers.Aliases -> Typing.TypeContext -> [Core.Type] -> Core.Term -> Core.Term -> Compute.Flow Graph.Graph Syntax.Expression)
encodeApplication_fallback env aliases tc typeApps lhs rhs = (Monads.withTrace "fallback" (Flows.bind (Annotations.getType (Annotations.termAnnotationInternal lhs)) (\mt -> Flows.bind (Maybes.cases mt (CoderUtils.tryTypeOf "2" tc lhs) (\typ -> Flows.pure typ)) (\t -> (\x -> case x of
  Core.TypeFunction v1 ->  
    let dom = (Core.functionTypeDomain v1)
    in  
      let cod = (Core.functionTypeCodomain v1)
      in ((\x -> case x of
        Core.TermFunction v2 -> ((\x -> case x of
          Core.FunctionElimination v3 -> (Flows.bind (encodeTerm env rhs) (\jarg -> Flows.bind (Logic.ifElse (Logic.not (Lists.null (javaTypeArgumentsForType dom))) (Flows.pure dom) (Flows.bind (Annotations.getType (Annotations.termAnnotationInternal rhs)) (\mrt -> Maybes.cases mrt (Flows.bind (CoderUtils.tryTypeOf "dom-enrich" tc rhs) (\rt -> Flows.pure (Logic.ifElse (Logic.not (Lists.null (javaTypeArgumentsForType rt))) rt dom))) (\rt -> Flows.pure (Logic.ifElse (Logic.not (Lists.null (javaTypeArgumentsForType rt))) rt dom))))) (\enrichedDom -> encodeElimination env (Just jarg) enrichedDom cod v3)))
          _ -> (Flows.bind (encodeTerm env lhs) (\jfun -> Flows.bind (encodeTerm env rhs) (\jarg -> Flows.pure (applyJavaArg jfun jarg))))) v2)
        _ -> (Flows.bind (encodeTerm env lhs) (\jfun -> Flows.bind (encodeTerm env rhs) (\jarg -> Flows.pure (applyJavaArg jfun jarg))))) (Rewriting.deannotateTerm lhs))
  _ -> (Monads.fail (Strings.cat [
    "Unexpected type: ",
    (Core___.type_ t)]))) (Rewriting.deannotateTypeParameters (Rewriting.deannotateType t))))))

functionCall :: (Helpers.JavaEnvironment -> Bool -> Core.Name -> [Core.Term] -> [Core.Type] -> Compute.Flow Graph.Graph Syntax.Expression)
functionCall env isPrim name args typeApps =  
  let aliases = (Helpers.javaEnvironmentAliases env)
  in  
    let isLambdaBound = (isLambdaBoundIn name (Helpers.aliasesLambdaVars aliases))
    in (Logic.ifElse (Logic.and isPrim (Logic.and (Lists.null args) (Logic.not isLambdaBound))) ( 
      let classWithApply = (Syntax.unIdentifier (elementJavaIdentifier True False aliases name))
      in  
        let suffix = (Strings.cat2 "." Names.applyMethodName)
        in  
          let className = (Strings.fromList (Lists.take (Math.sub (Strings.length classWithApply) (Strings.length suffix)) (Strings.toList classWithApply)))
          in (Flows.pure (Utils_.javaIdentifierToJavaExpression (Syntax.Identifier (Strings.cat [
            className,
            "::",
            Names.applyMethodName]))))) (Flows.bind (Flows.mapList (\arg -> encodeTerm env arg) args) (\jargs0 ->  
      let wrapResult = (wrapLazyArguments name jargs0)
      in  
        let jargs = (Pairs.first wrapResult)
        in  
          let mMethodOverride = (Pairs.second wrapResult)
          in (Logic.ifElse (Logic.or (isLocalVariable name) isLambdaBound) (Flows.bind (encodeVariable env name) (\baseExpr -> Flows.pure (Lists.foldl (\acc -> \jarg -> applyJavaArg acc jarg) baseExpr jargs))) ( 
            let overrideMethodName = (\jid -> Maybes.cases mMethodOverride jid (\m ->  
                    let s = (Syntax.unIdentifier jid)
                    in (Syntax.Identifier (Strings.cat2 (Strings.fromList (Lists.take (Math.sub (Strings.length s) (Strings.length Names.applyMethodName)) (Strings.toList s))) m))))
            in (Logic.ifElse (Lists.null typeApps) ( 
              let header = (Syntax.MethodInvocation_HeaderSimple (Syntax.MethodName (overrideMethodName (elementJavaIdentifier isPrim False aliases name))))
              in (Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
                Syntax.methodInvocationHeader = header,
                Syntax.methodInvocationArguments = jargs})))) ( 
              let qn = (Names_.qualifyName name)
              in  
                let mns = (Module.qualifiedNameNamespace qn)
                in  
                  let localName = (Module.qualifiedNameLocal qn)
                  in (Maybes.cases mns ( 
                    let header = (Syntax.MethodInvocation_HeaderSimple (Syntax.MethodName (overrideMethodName (elementJavaIdentifier isPrim False aliases name))))
                    in (Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
                      Syntax.methodInvocationHeader = header,
                      Syntax.methodInvocationArguments = jargs})))) (\ns_ ->  
                    let classId = (Utils_.nameToJavaName aliases (Names_.unqualifyName (Module.QualifiedName {
                            Module.qualifiedNameNamespace = (Just ns_),
                            Module.qualifiedNameLocal = (elementsClassName ns_)})))
                    in  
                      let methodId = (Logic.ifElse isPrim (overrideMethodName (Syntax.Identifier (Strings.cat2 (Syntax.unIdentifier (Utils_.nameToJavaName aliases (Names_.unqualifyName (Module.QualifiedName {
                              Module.qualifiedNameNamespace = (Just ns_),
                              Module.qualifiedNameLocal = (Formatting.capitalize localName)})))) (Strings.cat2 "." Names.applyMethodName)))) (Syntax.Identifier (Utils_.sanitizeJavaName localName)))
                      in (Flows.bind (Flows.mapList (\t -> Flows.bind (encodeType aliases Sets.empty t) (\jt -> Flows.bind (Utils_.javaTypeToJavaReferenceType jt) (\rt -> Flows.pure (Syntax.TypeArgumentReference rt)))) typeApps) (\jTypeArgs -> Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocationStaticWithTypeArgs classId methodId jTypeArgs jargs)))))))))))))

buildCurriedLambda :: ([Core.Name] -> Syntax.Expression -> Syntax.Expression)
buildCurriedLambda params inner = (Lists.foldl (\acc -> \p -> Utils_.javaLambda p acc) inner (Lists.reverse params))

encodeFunction :: (Helpers.JavaEnvironment -> Core.Type -> Core.Type -> Core.Function -> Compute.Flow Graph.Graph Syntax.Expression)
encodeFunction env dom cod fun =  
  let aliases = (Helpers.javaEnvironmentAliases env)
  in ((\x -> case x of
    Core.FunctionElimination v1 -> (Monads.withTrace (Strings.cat [
      "elimination (",
      (Core___.elimination v1),
      ")"]) (encodeElimination env Nothing dom cod v1))
    Core.FunctionLambda v1 -> (Monads.withTrace (Strings.cat2 "lambda " (Core.unName (Core.lambdaParameter v1))) (withLambda env v1 (\env2 ->  
      let lambdaVar = (Core.lambdaParameter v1)
      in  
        let body = (Core.lambdaBody v1)
        in ((\x -> case x of
          Core.TermFunction v2 -> ((\x -> case x of
            Core.FunctionLambda v3 -> ((\x -> case x of
              Core.TypeFunction v4 ->  
                let dom2 = (Core.functionTypeDomain v4)
                in  
                  let cod2 = (Core.functionTypeCodomain v4)
                  in (Flows.bind (encodeFunction env2 dom2 cod2 (Core.FunctionLambda v3)) (\innerJavaLambda ->  
                    let lam1 = (Utils_.javaLambda lambdaVar innerJavaLambda)
                    in (applyCastIfSafe aliases (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = dom,
                      Core.functionTypeCodomain = cod})) lam1)))
              _ -> (Monads.fail (Strings.cat2 "expected function type for lambda body, but got: " (Core___.type_ cod)))) (Rewriting.deannotateType cod))
            _ -> (Flows.bind (Monads.withTrace "analyze function body" (analyzeJavaFunction env2 body)) (\fs ->  
              let bindings = (Typing.functionStructureBindings fs)
              in  
                let innerBody = (Typing.functionStructureBody fs)
                in  
                  let env3 = (Typing.functionStructureEnvironment fs)
                  in (Flows.bind (bindingsToStatements env3 bindings) (\bindResult ->  
                    let bindingStmts = (Pairs.first bindResult)
                    in  
                      let env4 = (Pairs.second bindResult)
                      in (Flows.bind (encodeTerm env4 innerBody) (\jbody ->  
                        let lam1 = (Logic.ifElse (Lists.null bindings) (Utils_.javaLambda lambdaVar jbody) ( 
                                let returnSt = (Syntax.BlockStatementStatement (Utils_.javaReturnStatement (Just jbody)))
                                in (Utils_.javaLambdaFromBlock lambdaVar (Syntax.Block (Lists.concat2 bindingStmts [
                                  returnSt])))))
                        in (applyCastIfSafe aliases (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = dom,
                          Core.functionTypeCodomain = cod})) lam1)))))))) v2)
          _ -> (Flows.bind (Monads.withTrace "analyze function body" (analyzeJavaFunction env2 body)) (\fs ->  
            let bindings = (Typing.functionStructureBindings fs)
            in  
              let innerBody = (Typing.functionStructureBody fs)
              in  
                let env3 = (Typing.functionStructureEnvironment fs)
                in (Flows.bind (bindingsToStatements env3 bindings) (\bindResult ->  
                  let bindingStmts = (Pairs.first bindResult)
                  in  
                    let env4 = (Pairs.second bindResult)
                    in (Flows.bind (encodeTerm env4 innerBody) (\jbody ->  
                      let lam1 = (Logic.ifElse (Lists.null bindings) (Utils_.javaLambda lambdaVar jbody) ( 
                              let returnSt = (Syntax.BlockStatementStatement (Utils_.javaReturnStatement (Just jbody)))
                              in (Utils_.javaLambdaFromBlock lambdaVar (Syntax.Block (Lists.concat2 bindingStmts [
                                returnSt])))))
                      in (applyCastIfSafe aliases (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = dom,
                        Core.functionTypeCodomain = cod})) lam1)))))))) (Rewriting.deannotateTerm body)))))
    Core.FunctionPrimitive v1 ->  
      let classWithApply = (Syntax.unIdentifier (elementJavaIdentifier True False aliases v1))
      in  
        let suffix = (Strings.cat2 "." Names.applyMethodName)
        in  
          let className = (Strings.fromList (Lists.take (Math.sub (Strings.length classWithApply) (Strings.length suffix)) (Strings.toList classWithApply)))
          in  
            let arity = (Arity.typeArity (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = dom,
                    Core.functionTypeCodomain = cod})))
            in (Logic.ifElse (Equality.lte arity 1) (Flows.pure (Utils_.javaIdentifierToJavaExpression (Syntax.Identifier (Strings.cat [
              className,
              "::",
              Names.applyMethodName])))) ( 
              let paramNames = (Lists.map (\i -> Core.Name (Strings.cat2 "p" (Literals.showInt32 i))) (Math.range 0 (Math.sub arity 1)))
              in  
                let paramExprs = (Lists.map (\p -> Utils_.javaIdentifierToJavaExpression (Utils_.variableToJavaIdentifier p)) paramNames)
                in  
                  let classId = (Syntax.Identifier className)
                  in  
                    let call = (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocationStatic classId (Syntax.Identifier Names.applyMethodName) paramExprs))
                    in  
                      let curried = (buildCurriedLambda paramNames call)
                      in (Flows.bind (encodeType aliases Sets.empty (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = dom,
                        Core.functionTypeCodomain = cod}))) (\jtype -> Flows.bind (Utils_.javaTypeToJavaReferenceType jtype) (\rt -> Flows.pure (Utils_.javaCastExpressionToJavaExpression (Utils_.javaCastExpression rt (Utils_.javaExpressionToJavaUnaryExpression curried))))))))
    _ -> (Flows.pure (encodeLiteral (Core.LiteralString (Strings.cat2 "Unimplemented function variant: " (Core___.function fun)))))) fun)

extractArgType :: (t0 -> Core.Type -> Core.Type)
extractArgType _lhs typ = ((\x -> case x of
  Core.TypeApplication v1 -> ((\x -> case x of
    Core.TypeApplication _ -> (Core.applicationTypeArgument v1)
    _ -> typ) (Core.applicationTypeFunction v1))
  _ -> typ) typ)

annotateBodyWithCod :: (Core.Type -> Core.Term -> Core.Term)
annotateBodyWithCod typ term =  
  let setAnn = (\t -> Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ typ)) t)
  in ((\x -> case x of
    Core.TermTypeApplication _ -> (setAnn term)
    Core.TermApplication v1 ->  
      let lhs = (Core.applicationFunction v1)
      in  
        let rhs = (Core.applicationArgument v1)
        in  
          let annotatedRhs = ((\x -> case x of
                  Core.TermTypeApplication _ -> (annotateBodyWithCod (extractArgType lhs typ) rhs)
                  _ -> rhs) (Rewriting.deannotateTerm rhs))
          in (setAnn (Core.TermApplication (Core.Application {
            Core.applicationFunction = lhs,
            Core.applicationArgument = annotatedRhs})))
    _ -> (setAnn term)) (Rewriting.deannotateTerm term))

domTypeArgs :: (Helpers.Aliases -> Core.Type -> Compute.Flow Graph.Graph [Syntax.TypeArgument])
domTypeArgs aliases d =  
  let args = (extractTypeApplicationArgs (Rewriting.deannotateType d))
  in (Logic.ifElse (Logic.not (Lists.null args)) (Flows.mapList (\t -> Flows.bind (encodeType aliases Sets.empty t) (\jt -> Flows.bind (Utils_.javaTypeToJavaReferenceType jt) (\rt -> Flows.pure (Syntax.TypeArgumentReference rt)))) args) (Flows.pure (javaTypeArgumentsForType d)))

otherwiseBranch :: (Helpers.JavaEnvironment -> Helpers.Aliases -> Core.Type -> Core.Type -> Core.Name -> Syntax.Type -> [Syntax.TypeArgument] -> Core.Term -> Compute.Flow Graph.Graph Syntax.ClassBodyDeclarationWithComments)
otherwiseBranch env aliases dom cod tname jcod targs d =  
  let jdom = (Syntax.TypeReference (Utils_.nameToJavaReferenceType aliases True targs tname Nothing))
  in  
    let mods = [
            Syntax.MethodModifierPublic]
    in  
      let anns = [
              Utils_.overrideAnnotation]
      in  
        let param = (Utils_.javaTypeToJavaFormalParameter jdom (Core.Name "instance"))
        in  
          let result = (Syntax.ResultType (Syntax.UnannType jcod))
          in (Flows.bind (analyzeJavaFunction env d) (\fs ->  
            let bindings = (Typing.functionStructureBindings fs)
            in  
              let rawBody = (Typing.functionStructureBody fs)
              in  
                let innerBody = (annotateBodyWithCod cod rawBody)
                in  
                  let env2 = (Typing.functionStructureEnvironment fs)
                  in (Flows.bind (bindingsToStatements env2 bindings) (\bindResult ->  
                    let bindingStmts = (Pairs.first bindResult)
                    in  
                      let env3 = (Pairs.second bindResult)
                      in (Flows.bind (encodeTerm env3 innerBody) (\jret ->  
                        let returnStmt = (Syntax.BlockStatementStatement (Utils_.javaReturnStatement (Just jret)))
                        in  
                          let allStmts = (Lists.concat2 bindingStmts [
                                  returnStmt])
                          in (Flows.pure (noComment (Utils_.methodDeclaration mods [] anns Names.otherwiseMethodName [
                            param] result (Just allStmts))))))))))

visitBranch :: (Helpers.JavaEnvironment -> Helpers.Aliases -> Core.Type -> Core.Name -> Syntax.Type -> [Syntax.TypeArgument] -> Core.Field -> Compute.Flow Graph.Graph Syntax.ClassBodyDeclarationWithComments)
visitBranch env aliases dom tname jcod targs field =  
  let jdom = (Syntax.TypeReference (Utils_.nameToJavaReferenceType aliases True targs tname (Just (Formatting.capitalize (Core.unName (Core.fieldName field))))))
  in  
    let mods = [
            Syntax.MethodModifierPublic]
    in  
      let anns = [
              Utils_.overrideAnnotation]
      in  
        let result = (Syntax.ResultType (Syntax.UnannType jcod))
        in ((\x -> case x of
          Core.TermFunction v1 -> ((\x -> case x of
            Core.FunctionLambda v2 -> (withLambda env v2 (\env2 ->  
              let lambdaParam = (Core.lambdaParameter v2)
              in  
                let body = (Core.lambdaBody v2)
                in  
                  let env3 = (insertBranchVar lambdaParam env2)
                  in (Flows.bind (analyzeJavaFunction env3 body) (\fs ->  
                    let bindings = (Typing.functionStructureBindings fs)
                    in  
                      let innerBody = (Typing.functionStructureBody fs)
                      in  
                        let env4 = (Typing.functionStructureEnvironment fs)
                        in (Flows.bind (bindingsToStatements env4 bindings) (\bindResult ->  
                          let bindingStmts = (Pairs.first bindResult)
                          in  
                            let env5 = (Pairs.second bindResult)
                            in (Flows.bind (encodeTerm env5 innerBody) (\jret ->  
                              let param = (Utils_.javaTypeToJavaFormalParameter jdom lambdaParam)
                              in  
                                let returnStmt = (Syntax.BlockStatementStatement (Utils_.javaReturnStatement (Just jret)))
                                in  
                                  let allStmts = (Lists.concat2 bindingStmts [
                                          returnStmt])
                                  in (Flows.pure (noComment (Utils_.methodDeclaration mods [] anns Names.visitMethodName [
                                    param] result (Just allStmts))))))))))))
            _ -> (Monads.fail (Strings.cat2 "visitBranch: field term is not a lambda: " (Core___.term (Core.fieldTerm field))))) v1)
          _ -> (Monads.fail (Strings.cat2 "visitBranch: field term is not a lambda: " (Core___.term (Core.fieldTerm field))))) (Rewriting.deannotateTerm (Core.fieldTerm field)))

encodeElimination :: (Helpers.JavaEnvironment -> Maybe Syntax.Expression -> Core.Type -> Core.Type -> Core.Elimination -> Compute.Flow Graph.Graph Syntax.Expression)
encodeElimination env marg dom cod elm =  
  let aliases = (Helpers.javaEnvironmentAliases env)
  in ((\x -> case x of
    Core.EliminationRecord v1 ->  
      let fname = (Core.projectionField v1)
      in (Flows.bind (encodeType aliases Sets.empty dom) (\jdom0 -> Flows.bind (Utils_.javaTypeToJavaReferenceType jdom0) (\jdomr -> Maybes.cases marg ( 
        let projVar = (Core.Name "projected")
        in  
          let jbody = (Utils_.javaExpressionNameToJavaExpression (Utils_.fieldExpression (Utils_.variableToJavaIdentifier projVar) (Utils_.javaIdentifier (Core.unName fname))))
          in (Flows.pure (Utils_.javaLambda projVar jbody))) (\jarg ->  
        let qual = (Syntax.FieldAccess_QualifierPrimary (Utils_.javaExpressionToJavaPrimary jarg))
        in (Flows.pure (Utils_.javaFieldAccessToJavaExpression (Syntax.FieldAccess {
          Syntax.fieldAccessQualifier = qual,
          Syntax.fieldAccessIdentifier = (Utils_.javaIdentifier (Core.unName fname))})))))))
    Core.EliminationUnion v1 ->  
      let tname = (Core.caseStatementTypeName v1)
      in  
        let def_ = (Core.caseStatementDefault v1)
        in  
          let fields = (Core.caseStatementCases v1)
          in (Maybes.cases marg ( 
            let uVar = (Core.Name "u")
            in  
              let typedLambda = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = uVar,
                      Core.lambdaDomain = (Just dom),
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination elm)),
                        Core.applicationArgument = (Core.TermVariable uVar)}))})))
              in (encodeTerm env typedLambda)) (\jarg ->  
            let prim = (Utils_.javaExpressionToJavaPrimary jarg)
            in  
              let consId = (innerClassRef aliases tname Names.partialVisitorName)
              in (Flows.bind (encodeType aliases Sets.empty cod) (\jcod -> Flows.bind (Utils_.javaTypeToJavaReferenceType jcod) (\rt -> Flows.bind (domTypeArgs aliases dom) (\domArgs ->  
                let targs = (typeArgsOrDiamond (Lists.concat2 domArgs [
                        Syntax.TypeArgumentReference rt]))
                in (Flows.bind (Maybes.cases def_ (Flows.pure []) (\d -> Flows.bind (otherwiseBranch env aliases dom cod tname jcod domArgs d) (\b -> Flows.pure [
                  b]))) (\otherwiseBranches -> Flows.bind (Flows.mapList (\f -> visitBranch env aliases dom tname jcod domArgs f) fields) (\visitBranches ->  
                  let body = (Syntax.ClassBody (Lists.concat2 otherwiseBranches visitBranches))
                  in  
                    let visitor = (Utils_.javaConstructorCall (Utils_.javaConstructorName consId (Just targs)) [] (Just body))
                    in (Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocation (Just (Right prim)) (Syntax.Identifier Names.acceptMethodName) [
                      visitor]))))))))))))
    Core.EliminationWrap _ ->  
      let withArg = (\ja -> Utils_.javaFieldAccessToJavaExpression (Syntax.FieldAccess {
              Syntax.fieldAccessQualifier = (Syntax.FieldAccess_QualifierPrimary (Utils_.javaExpressionToJavaPrimary ja)),
              Syntax.fieldAccessIdentifier = (Utils_.javaIdentifier Names.valueFieldName)}))
      in (Flows.pure (Maybes.cases marg ( 
        let wVar = (Core.Name "wrapped")
        in  
          let wArg = (Utils_.javaIdentifierToJavaExpression (Utils_.variableToJavaIdentifier wVar))
          in (Utils_.javaLambda wVar (withArg wArg))) (\jarg -> withArg jarg)))
    _ -> (Monads.unexpected "elimination case" "encodeElimination")) elm)

toDeclInit :: (Helpers.Aliases -> Typing.TypeContext -> S.Set Core.Name -> [Core.Binding] -> Core.Name -> Compute.Flow Graph.Graph (Maybe Syntax.BlockStatement))
toDeclInit aliasesExt tcExt recursiveVars flatBindings name = (Logic.ifElse (Sets.member name recursiveVars) ( 
  let binding = (Lists.head (Lists.filter (\b -> Equality.equal (Core.bindingName b) name) flatBindings))
  in  
    let value = (Core.bindingTerm binding)
    in (Flows.bind (Maybes.cases (Core.bindingType binding) (CoderUtils.tryTypeOf "6" tcExt value) (\ts -> Flows.pure (Core.typeSchemeType ts))) (\typ -> Flows.bind (encodeType aliasesExt Sets.empty typ) (\jtype ->  
      let id = (Utils_.variableToJavaIdentifier name)
      in  
        let arid = (Syntax.Identifier "java.util.concurrent.atomic.AtomicReference")
        in  
          let aid = Syntax.AnnotatedIdentifier {
                  Syntax.annotatedIdentifierAnnotations = [],
                  Syntax.annotatedIdentifierIdentifier = arid}
          in (Flows.bind (Utils_.javaTypeToJavaReferenceType jtype) (\rt ->  
            let targs = (typeArgsOrDiamond [
                    Syntax.TypeArgumentReference rt])
            in  
              let ci = Syntax.ClassOrInterfaceTypeToInstantiate {
                      Syntax.classOrInterfaceTypeToInstantiateIdentifiers = [
                        aid],
                      Syntax.classOrInterfaceTypeToInstantiateTypeArguments = (Just targs)}
              in  
                let body = (Utils_.javaConstructorCall ci [] Nothing)
                in  
                  let pkg = (Names.javaPackageName [
                          "java",
                          "util",
                          "concurrent",
                          "atomic"])
                  in  
                    let artype = (Utils_.javaRefType [
                            rt] (Just pkg) "AtomicReference")
                    in (Flows.pure (Just (Utils_.variableDeclarationStatement aliasesExt artype id body))))))))) (Flows.pure Nothing))

toDeclStatement :: (Helpers.JavaEnvironment -> Helpers.Aliases -> Typing.TypeContext -> S.Set Core.Name -> S.Set Core.Name -> [Core.Binding] -> Core.Name -> Compute.Flow Graph.Graph Syntax.BlockStatement)
toDeclStatement envExt aliasesExt tcExt recursiveVars thunkedVars flatBindings name =  
  let binding = (Lists.head (Lists.filter (\b -> Equality.equal (Core.bindingName b) name) flatBindings))
  in  
    let value = (Core.bindingTerm binding)
    in (Flows.bind (Maybes.cases (Core.bindingType binding) (CoderUtils.tryTypeOf "7" tcExt value) (\ts -> Flows.pure (Core.typeSchemeType ts))) (\typ -> Flows.bind (encodeType aliasesExt Sets.empty typ) (\jtype ->  
      let id = (Utils_.variableToJavaIdentifier name)
      in  
        let annotatedValue = (Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ typ)) value)
        in (Flows.bind (encodeTerm envExt annotatedValue) (\rhs -> Logic.ifElse (Sets.member name recursiveVars) (Flows.pure (Syntax.BlockStatementStatement (Utils_.javaMethodInvocationToJavaStatement (Utils_.methodInvocation (Just (Left (Syntax.ExpressionName {
          Syntax.expressionNameQualifier = Nothing,
          Syntax.expressionNameIdentifier = id}))) (Syntax.Identifier Names.setMethodName) [
          rhs])))) (Logic.ifElse (Sets.member name thunkedVars) (Flows.bind (Utils_.javaTypeToJavaReferenceType jtype) (\rt ->  
          let lazyType = (Utils_.javaRefType [
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
                  let lazyExpr = (Utils_.javaConstructorCall (Utils_.javaConstructorName (Syntax.Identifier "hydra.util.Lazy") (Just targs)) [
                          supplierLambda] Nothing)
                  in (Flows.pure (Utils_.variableDeclarationStatement aliasesExt lazyType id lazyExpr)))) (Flows.pure (Utils_.variableDeclarationStatement aliasesExt jtype id rhs))))))))

bindingsToStatements :: (Helpers.JavaEnvironment -> [Core.Binding] -> Compute.Flow Graph.Graph ([Syntax.BlockStatement], Helpers.JavaEnvironment))
bindingsToStatements env bindings =  
  let aliases = (Helpers.javaEnvironmentAliases env)
  in  
    let tc = (Helpers.javaEnvironmentTypeContext env)
    in  
      let flatBindings = (dedupBindings (Helpers.aliasesInScopeJavaVars aliases) (flattenBindings bindings))
      in  
        let tcExtended = (Schemas.extendTypeContextForLet CoderUtils.bindingMetadata tc (Core.Let {
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
                              Helpers.javaEnvironmentTypeContext = tcExtended}
                      in (Logic.ifElse (Lists.null bindings) (Flows.pure ([], envExtended)) (Flows.bind (Flows.mapList (\names -> Flows.bind (Flows.mapList (\n -> toDeclInit aliasesExtended tcExtended recursiveVars flatBindings n) names) (\inits -> Flows.bind (Flows.mapList (\n -> toDeclStatement envExtended aliasesExtended tcExtended recursiveVars thunkedVars flatBindings n) names) (\decls -> Flows.pure (Lists.concat2 (Maybes.cat inits) decls)))) sorted) (\groups -> Flows.pure (Lists.concat groups, envExtended))))

toClassDecl :: (Bool -> Bool -> Helpers.Aliases -> [Syntax.TypeParameter] -> Core.Name -> Core.Type -> Compute.Flow Graph.Graph Syntax.ClassDeclaration)
toClassDecl isInner isSer aliases tparams elName t =  
  let wrap = (\t_ -> declarationForRecordType isInner isSer aliases tparams elName [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "value"),
            Core.fieldTypeType = (Rewriting.deannotateType t_)}])
  in ((\x -> case x of
    Core.TypeRecord v1 -> (declarationForRecordType isInner isSer aliases tparams elName (Core.rowTypeFields v1))
    Core.TypeUnion v1 -> (declarationForUnionType isSer aliases tparams elName (Core.rowTypeFields v1))
    Core.TypeForall v1 ->  
      let v = (Core.forallTypeParameter v1)
      in  
        let body = (Core.forallTypeBody v1)
        in  
          let param = (Utils_.javaTypeParameter (Formatting.capitalize (Core.unName v)))
          in (toClassDecl False isSer aliases (Lists.concat2 tparams [
            param]) elName body)
    Core.TypeWrap v1 ->  
      let wtype = (Core.wrappedTypeBody v1)
      in (declarationForRecordType isInner isSer aliases tparams elName [
        Core.FieldType {
          Core.fieldTypeName = (Core.Name "value"),
          Core.fieldTypeType = wtype}])
    _ -> (wrap t)) (Rewriting.deannotateType t))

declarationForUnionType :: (Bool -> Helpers.Aliases -> [Syntax.TypeParameter] -> Core.Name -> [Core.FieldType] -> Compute.Flow Graph.Graph Syntax.ClassDeclaration)
declarationForUnionType isSer aliases tparams elName fields = (Flows.bind (Flows.mapList (\ft ->  
  let fname = (Core.fieldTypeName ft)
  in  
    let ftype = (Core.fieldTypeType ft)
    in  
      let rfields = (Logic.ifElse (Schemas.isUnitType (Rewriting.deannotateType ftype)) [] [
              Core.FieldType {
                Core.fieldTypeName = (Core.Name "value"),
                Core.fieldTypeType = (Rewriting.deannotateType ftype)}])
      in  
        let varName = (Utils_.variantClassName False elName fname)
        in (Flows.bind (declarationForRecordType_ True isSer aliases [] varName (Logic.ifElse isSer (Just elName) Nothing) rfields) (\innerDecl -> Flows.pure (augmentVariantClass aliases tparams elName innerDecl)))) fields) (\variantClasses ->  
  let variantDecls = (Lists.map (\vc -> Syntax.ClassBodyDeclarationClassMember (Syntax.ClassMemberDeclarationClass vc)) variantClasses)
  in (Flows.bind (Flows.mapList (\pair -> addComment (Pairs.first pair) (Pairs.second pair)) (Lists.zip variantDecls fields)) (\variantDecls_ ->  
    let privateConst = (Utils_.makeConstructor aliases elName True [] [])
    in  
      let acceptDecl = (Utils_.toAcceptMethod True tparams)
      in  
        let vtparams = (Lists.concat2 tparams [
                Utils_.javaTypeParameter Names.visitorReturnParameter])
        in  
          let visitorMethods = (Lists.map (\ft ->  
                  let fname = (Core.fieldTypeName ft)
                  in  
                    let typeArgs = (Lists.map (\tp -> Utils_.typeParameterToTypeArgument tp) tparams)
                    in  
                      let varRef = (Utils_.javaClassTypeToJavaType (Utils_.nameToJavaClassType aliases False typeArgs (Utils_.variantClassName False elName fname) Nothing))
                      in  
                        let param = (Utils_.javaTypeToJavaFormalParameter varRef (Core.Name "instance"))
                        in  
                          let resultR = (Utils_.javaTypeToJavaResult (Syntax.TypeReference Utils_.visitorTypeVariable))
                          in (Utils_.interfaceMethodDeclaration [] [] Names.visitMethodName [
                            param] resultR Nothing)) fields)
          in  
            let visitorBody = (Syntax.InterfaceBody visitorMethods)
            in  
              let visitor = (Utils_.javaInterfaceDeclarationToJavaClassBodyDeclaration (Syntax.NormalInterfaceDeclaration {
                      Syntax.normalInterfaceDeclarationModifiers = [
                        Syntax.InterfaceModifierPublic],
                      Syntax.normalInterfaceDeclarationIdentifier = (Syntax.TypeIdentifier (Syntax.Identifier Names.visitorName)),
                      Syntax.normalInterfaceDeclarationParameters = vtparams,
                      Syntax.normalInterfaceDeclarationExtends = [],
                      Syntax.normalInterfaceDeclarationBody = visitorBody}))
              in  
                let typeArgs = (Lists.map (\tp -> Utils_.typeParameterToTypeArgument tp) tparams)
                in  
                  let visitorClassType = (Utils_.javaClassType (Lists.concat2 (Lists.map (\tp -> Utils_.typeParameterToReferenceType tp) tparams) [
                          Utils_.visitorTypeVariable]) Nothing Names.visitorName)
                  in  
                    let mainInstanceParam = (Utils_.javaTypeToJavaFormalParameter (Utils_.javaClassTypeToJavaType (Utils_.nameToJavaClassType aliases False typeArgs elName Nothing)) (Core.Name "instance"))
                    in  
                      let resultR = (Utils_.javaTypeToJavaResult (Syntax.TypeReference Utils_.visitorTypeVariable))
                      in  
                        let throwStmt = (Syntax.BlockStatementStatement (Utils_.javaThrowIllegalStateException [
                                Utils_.javaAdditiveExpressionToJavaExpression (Utils_.addExpressions [
                                  Utils_.javaStringMultiplicativeExpression "Non-exhaustive patterns when matching: ",
                                  (Syntax.MultiplicativeExpressionUnary (Utils_.javaIdentifierToJavaUnaryExpression (Syntax.Identifier "instance")))])]))
                        in  
                          let defaultMod = [
                                  Syntax.InterfaceMethodModifierDefault]
                          in  
                            let otherwiseDecl = (Utils_.interfaceMethodDeclaration defaultMod [] Names.otherwiseMethodName [
                                    mainInstanceParam] resultR (Just [
                                    throwStmt]))
                            in  
                              let pvVisitMethods = (Lists.map (\ft ->  
                                      let fname = (Core.fieldTypeName ft)
                                      in  
                                        let varRef = (Utils_.javaClassTypeToJavaType (Utils_.nameToJavaClassType aliases False typeArgs (Utils_.variantClassName False elName fname) Nothing))
                                        in  
                                          let param = (Utils_.javaTypeToJavaFormalParameter varRef (Core.Name "instance"))
                                          in  
                                            let mi = (Utils_.methodInvocation Nothing (Syntax.Identifier Names.otherwiseMethodName) [
                                                    Utils_.javaIdentifierToJavaExpression (Syntax.Identifier "instance")])
                                            in  
                                              let returnOtherwise = (Syntax.BlockStatementStatement (Utils_.javaReturnStatement (Just (Utils_.javaPrimaryToJavaExpression (Utils_.javaMethodInvocationToJavaPrimary mi)))))
                                              in (Utils_.interfaceMethodDeclaration defaultMod [] Names.visitMethodName [
                                                param] resultR (Just [
                                                returnOtherwise]))) fields)
                              in  
                                let pvBody = (Syntax.InterfaceBody (Lists.concat2 [
                                        otherwiseDecl] pvVisitMethods))
                                in  
                                  let partialVisitor = (Utils_.javaInterfaceDeclarationToJavaClassBodyDeclaration (Syntax.NormalInterfaceDeclaration {
                                          Syntax.normalInterfaceDeclarationModifiers = [
                                            Syntax.InterfaceModifierPublic],
                                          Syntax.normalInterfaceDeclarationIdentifier = (Syntax.TypeIdentifier (Syntax.Identifier Names.partialVisitorName)),
                                          Syntax.normalInterfaceDeclarationParameters = vtparams,
                                          Syntax.normalInterfaceDeclarationExtends = [
                                            Syntax.InterfaceType visitorClassType],
                                          Syntax.normalInterfaceDeclarationBody = pvBody}))
                                  in (Flows.bind (constantDeclForTypeName aliases elName) (\tn0 -> Flows.bind (Flows.mapList (\ft -> constantDeclForFieldType aliases ft) fields) (\tn1 ->  
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
                                          in (Flows.pure (Utils_.javaClassDeclaration aliases tparams elName mods Nothing (interfaceTypes isSer aliases tparams elName) bodyDecls)))))))))

augmentVariantClass :: (Helpers.Aliases -> [Syntax.TypeParameter] -> Core.Name -> Syntax.ClassDeclaration -> Syntax.ClassDeclaration)
augmentVariantClass aliases tparams elName cd = ((\x -> case x of
  Syntax.ClassDeclarationNormal v1 ->  
    let args = (Lists.map (\tp -> Utils_.typeParameterToTypeArgument tp) tparams)
    in  
      let extendsPart = (Utils_.nameToJavaClassType aliases True args elName Nothing)
      in  
        let newMods = [
                Syntax.ClassModifierPublic,
                Syntax.ClassModifierStatic,
                Syntax.ClassModifierFinal]
        in  
          let oldBody = (Syntax.normalClassDeclarationBody v1)
          in  
            let oldDecls = (Syntax.unClassBody oldBody)
            in  
              let acceptDecl = (noComment (Utils_.toAcceptMethod False tparams))
              in  
                let newBody = (Syntax.ClassBody (Lists.concat2 oldDecls [
                        acceptDecl]))
                in (Syntax.ClassDeclarationNormal (Syntax.NormalClassDeclaration {
                  Syntax.normalClassDeclarationModifiers = newMods,
                  Syntax.normalClassDeclarationIdentifier = (Syntax.normalClassDeclarationIdentifier v1),
                  Syntax.normalClassDeclarationParameters = tparams,
                  Syntax.normalClassDeclarationExtends = (Just extendsPart),
                  Syntax.normalClassDeclarationImplements = (Syntax.normalClassDeclarationImplements v1),
                  Syntax.normalClassDeclarationBody = newBody}))
  _ -> cd) cd)

encodeTypeDefinition :: (Syntax.PackageDeclaration -> Helpers.Aliases -> Module.TypeDefinition -> Compute.Flow Graph.Graph (Core.Name, Syntax.CompilationUnit))
encodeTypeDefinition pkg aliases tdef =  
  let name = (Module.typeDefinitionName tdef)
  in  
    let typ = (Module.typeDefinitionType tdef)
    in  
      let serializable = (isSerializableJavaType typ)
      in  
        let imports = (Logic.ifElse serializable [
                Syntax.ImportDeclarationSingleType (Syntax.SingleTypeImportDeclaration (Utils_.javaTypeName (Syntax.Identifier "java.io.Serializable")))] [])
        in (Flows.bind (toClassDecl False serializable aliases [] name typ) (\decl -> Flows.bind (Annotations.getTypeDescription typ) (\comment ->  
          let tdecl = Syntax.TypeDeclarationWithComments {
                  Syntax.typeDeclarationWithCommentsValue = (Syntax.TypeDeclarationClass decl),
                  Syntax.typeDeclarationWithCommentsComments = comment}
          in (Flows.pure (name, (Syntax.CompilationUnitOrdinary (Syntax.OrdinaryCompilationUnit {
            Syntax.ordinaryCompilationUnitPackage = (Just pkg),
            Syntax.ordinaryCompilationUnitImports = imports,
            Syntax.ordinaryCompilationUnitTypes = [
              tdecl]})))))))

peelDomainsAndCod :: (Int -> Core.Type -> ([Core.Type], Core.Type))
peelDomainsAndCod n t = (Logic.ifElse (Equality.lte n 0) ([], t) ((\x -> case x of
  Core.TypeFunction v1 ->  
    let rest = (peelDomainsAndCod (Math.sub n 1) (Core.functionTypeCodomain v1))
    in (Lists.cons (Core.functionTypeDomain v1) (Pairs.first rest), (Pairs.second rest))
  _ -> ([], t)) (Rewriting.deannotateType t)))

isSerializableJavaType :: (Core.Type -> Bool)
isSerializableJavaType typ = ((\x -> case x of
  Core.TypeRecord _ -> True
  Core.TypeUnion _ -> True
  Core.TypeWrap _ -> True
  Core.TypeForall v1 -> (isSerializableJavaType (Core.forallTypeBody v1))
  _ -> False) (Rewriting.deannotateType typ))

correctCastType :: (Core.Term -> [Core.Type] -> Core.Type -> Compute.Flow t0 Core.Type)
correctCastType innerBody typeArgs fallback = ((\x -> case x of
  Core.TermPair _ -> (Logic.ifElse (Equality.equal (Lists.length typeArgs) 2) (Flows.pure (Core.TypePair (Core.PairType {
    Core.pairTypeFirst = (Lists.head typeArgs),
    Core.pairTypeSecond = (Lists.head (Lists.tail typeArgs))}))) (Flows.pure fallback))
  _ -> (Flows.pure fallback)) (Rewriting.deannotateTerm innerBody))

typeAppFallbackCast :: (Helpers.JavaEnvironment -> Helpers.Aliases -> [M.Map Core.Name Core.Term] -> [Syntax.Type] -> Syntax.Type -> Core.Term -> Core.Type -> Compute.Flow Graph.Graph Syntax.Expression)
typeAppFallbackCast env aliases anns tyapps jatyp body typ =  
  let annotatedBody = (Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ typ)) body)
  in (Flows.bind (encodeTermInternal env anns (Lists.cons jatyp tyapps) annotatedBody) (\jbody -> Flows.bind (encodeType aliases Sets.empty typ) (\jtype -> Flows.bind (Utils_.javaTypeToJavaReferenceType jtype) (\rt -> Flows.pure (Utils_.javaCastExpressionToJavaExpression (Utils_.javaCastExpression rt (Utils_.javaExpressionToJavaUnaryExpression jbody)))))))

typeAppNullaryOrHoisted :: (Helpers.JavaEnvironment -> Helpers.Aliases -> [M.Map Core.Name Core.Term] -> [Syntax.Type] -> Syntax.Type -> Core.Term -> Core.Type -> Core.Name -> Helpers.JavaSymbolClass -> [Core.Type] -> Compute.Flow Graph.Graph Syntax.Expression)
typeAppNullaryOrHoisted env aliases anns tyapps jatyp body correctedTyp varName cls allTypeArgs =  
  let qn = (Names_.qualifyName varName)
  in  
    let mns = (Module.qualifiedNameNamespace qn)
    in  
      let localName = (Module.qualifiedNameLocal qn)
      in ((\x -> case x of
        Helpers.JavaSymbolClassNullaryFunction -> (Maybes.cases mns (typeAppFallbackCast env aliases anns tyapps jatyp body correctedTyp) (\ns_ ->  
          let classId = (Utils_.nameToJavaName aliases (Names_.unqualifyName (Module.QualifiedName {
                  Module.qualifiedNameNamespace = (Just ns_),
                  Module.qualifiedNameLocal = (elementsClassName ns_)})))
          in  
            let methodId = (Syntax.Identifier (Utils_.sanitizeJavaName localName))
            in (Flows.bind (filterPhantomTypeArgs varName allTypeArgs) (\filteredTypeArgs -> Flows.bind (Flows.mapList (\t -> Flows.bind (encodeType aliases Sets.empty t) (\jt -> Flows.bind (Utils_.javaTypeToJavaReferenceType jt) (\rt -> Flows.pure (Syntax.TypeArgumentReference rt)))) filteredTypeArgs) (\jTypeArgs -> Flows.pure (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocationStaticWithTypeArgs classId methodId jTypeArgs [])))))))
        Helpers.JavaSymbolClassHoistedLambda v1 -> (Maybes.cases mns (typeAppFallbackCast env aliases anns tyapps jatyp body correctedTyp) (\ns_ ->  
          let classId = (Utils_.nameToJavaName aliases (Names_.unqualifyName (Module.QualifiedName {
                  Module.qualifiedNameNamespace = (Just ns_),
                  Module.qualifiedNameLocal = (elementsClassName ns_)})))
          in  
            let methodId = (Syntax.Identifier (Utils_.sanitizeJavaName localName))
            in (Flows.bind (filterPhantomTypeArgs varName allTypeArgs) (\filteredTypeArgs -> Flows.bind (Flows.mapList (\t -> Flows.bind (encodeType aliases Sets.empty t) (\jt -> Flows.bind (Utils_.javaTypeToJavaReferenceType jt) (\rt -> Flows.pure (Syntax.TypeArgumentReference rt)))) filteredTypeArgs) (\jTypeArgs ->  
              let paramNames = (Lists.map (\i -> Core.Name (Strings.cat2 "p" (Literals.showInt32 i))) (Math.range 0 (Math.sub v1 1)))
              in  
                let paramExprs = (Lists.map (\p -> Utils_.javaIdentifierToJavaExpression (Utils_.variableToJavaIdentifier p)) paramNames)
                in  
                  let call = (Utils_.javaMethodInvocationToJavaExpression (Utils_.methodInvocationStaticWithTypeArgs classId methodId jTypeArgs paramExprs))
                  in (Flows.pure (buildCurriedLambda paramNames call)))))))
        _ -> (typeAppFallbackCast env aliases anns tyapps jatyp body correctedTyp)) cls)

flattenApps :: (Core.Term -> [Core.Term] -> ([Core.Term], Core.Term))
flattenApps t acc = ((\x -> case x of
  Core.TermApplication v1 -> (flattenApps (Core.applicationFunction v1) (Lists.cons (Core.applicationArgument v1) acc))
  _ -> (acc, t)) (Rewriting.deannotateTerm t))

collectLambdaDomains :: (Core.Term -> ([Core.Type], Core.Term))
collectLambdaDomains t = ((\x -> case x of
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionLambda v2 -> (Maybes.cases (Core.lambdaDomain v2) ([], t) (\dom ->  
      let rest = (collectLambdaDomains (Core.lambdaBody v2))
      in (Lists.cons dom (Pairs.first rest), (Pairs.second rest))))
    _ -> ([], t)) v1)
  _ -> ([], t)) (Rewriting.deannotateTerm t))

rebuildApps :: (Core.Term -> [Core.Term] -> Core.Type -> Core.Term)
rebuildApps f args fType = (Logic.ifElse (Lists.null args) f ((\x -> case x of
  Core.TypeFunction v1 ->  
    let arg = (Lists.head args)
    in  
      let rest = (Lists.tail args)
      in  
        let remainingType = (Core.functionTypeCodomain v1)
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
                Core.TermApplication v1 ->  
                  let lhs = (Core.applicationFunction v1)
                  in  
                    let rhs = (Core.applicationArgument v1)
                    in  
                      let annotatedLhs = ((\x -> case x of
                              Core.TermFunction v2 -> ((\x -> case x of
                                Core.FunctionElimination v3 -> ((\x -> case x of
                                  Core.EliminationUnion v4 ->  
                                    let dom = (Schemas.nominalApplication (Core.caseStatementTypeName v4) [])
                                    in  
                                      let ft = (Core.TypeFunction (Core.FunctionType {
                                              Core.functionTypeDomain = dom,
                                              Core.functionTypeCodomain = fixedCod}))
                                      in (Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ ft)) lhs)
                                  _ -> lhs) v3)
                                _ -> lhs) v2)
                              _ -> lhs) (Rewriting.deannotateTerm lhs))
                      in (Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ resultType)) (Core.TermApplication (Core.Application {
                        Core.applicationFunction = annotatedLhs,
                        Core.applicationArgument = rhs})))
                _ -> (Annotations.setTermAnnotation Constants.key_type (Just (Core__.type_ resultType)) t)) (Rewriting.deannotateTerm t)))

encodeTermDefinition :: (Helpers.JavaEnvironment -> Module.TermDefinition -> Compute.Flow Graph.Graph Syntax.InterfaceMemberDeclaration)
encodeTermDefinition env tdef =  
  let name = (Module.termDefinitionName tdef)
  in  
    let term0 = (Module.termDefinitionTerm tdef)
    in  
      let ts = (Module.termDefinitionType tdef)
      in (Monads.withTrace (Strings.cat2 "encode term definition \"" (Strings.cat2 (Core.unName name) "\"")) ( 
        let term = (Rewriting.unshadowVariables term0)
        in (Flows.bind (Monads.withTrace "analyze function term for term assignment" (analyzeJavaFunction env term)) (\fs ->  
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
                                        in (Flows.bind (Logic.ifElse (Lists.null tparams) (Flows.pure Maps.empty) (buildSubstFromAnnotations schemeVarSet term)) (\typeVarSubst ->  
                                          let overgenSubst = (detectAccumulatorUnification schemeDoms cod tparams)
                                          in  
                                            let overgenVarSubst = (Maps.fromList (Maybes.cat (Lists.map (\entry ->  
                                                    let k = (Pairs.first entry)
                                                    in  
                                                      let v = (Pairs.second entry)
                                                      in ((\x -> case x of
                                                        Core.TypeVariable v1 -> (Just (k, v1))
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
                                                      let jparams = (Lists.map (\v -> Utils_.javaTypeParameter (Formatting.capitalize (Core.unName v))) fixedTparams)
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
                                                                        Helpers.javaEnvironmentTypeContext = (Helpers.javaEnvironmentTypeContext env2)}
                                                                in (Flows.bind (bindingsToStatements env2WithTypeParams bindings) (\bindResult ->  
                                                                  let bindingStmts = (Pairs.first bindResult)
                                                                  in  
                                                                    let env3 = (Pairs.second bindResult)
                                                                    in (Flows.bind (Logic.ifElse (Maps.null overgenSubst) (Flows.pure body) (applyOvergenSubstToTermAnnotations overgenSubst body)) (\body_ ->  
                                                                      let annotatedBody = (propagateTypesInAppChain fixedCod fixedCod body_)
                                                                      in (Flows.bind (Flows.mapList (\pair -> Flows.bind (encodeType aliases2 Sets.empty (Pairs.first pair)) (\jdom -> Flows.pure (Utils_.javaTypeToJavaFormalParameter jdom (Pairs.second pair)))) (Lists.zip fixedDoms params)) (\jformalParams -> Flows.bind (encodeType aliases2 Sets.empty fixedCod) (\jcod ->  
                                                                        let result = (Utils_.javaTypeToJavaResult jcod)
                                                                        in (Flows.bind (encodeTerm env3 annotatedBody) (\jbody ->  
                                                                          let mods = [
                                                                                  Syntax.InterfaceMethodModifierStatic]
                                                                          in  
                                                                            let jname = (Utils_.sanitizeJavaName (Formatting.decapitalize (Names_.localNameOf name)))
                                                                            in  
                                                                              let returnSt = (Syntax.BlockStatementStatement (Utils_.javaReturnStatement (Just jbody)))
                                                                              in (Flows.pure (Utils_.interfaceMethodDeclaration mods jparams jname jformalParams result (Just (Lists.concat2 bindingStmts [
                                                                                returnSt])))))))))))))))))))

encodeDefinitions :: (Module.Module -> [Module.Definition] -> Compute.Flow Graph.Graph (M.Map Core.Name Syntax.CompilationUnit))
encodeDefinitions mod defs = (Flows.bind Monads.getState (\g -> Flows.bind (Inference.initialTypeContext g) (\tc ->  
  let aliases = (Utils_.importAliasesForModule mod)
  in  
    let env = Helpers.JavaEnvironment {
            Helpers.javaEnvironmentAliases = aliases,
            Helpers.javaEnvironmentTypeContext = tc}
    in  
      let pkg = (Utils_.javaPackageDeclaration (Module.moduleNamespace mod))
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
              in (Flows.bind (Flows.mapList (\td -> encodeTypeDefinition pkg aliases td) nonTypedefDefs) (\typeUnits -> Flows.bind (Logic.ifElse (Lists.null termDefs) (Flows.pure []) (Flows.bind (Flows.mapList (\td -> encodeTermDefinition env td) termDefs) (\dataMembers -> Flows.pure [
                constructElementsInterface mod dataMembers]))) (\termUnits -> Flows.pure (Maps.fromList (Lists.concat2 typeUnits termUnits))))))))

moduleToJava :: (Module.Module -> [Module.Definition] -> Compute.Flow Graph.Graph (M.Map String String))
moduleToJava mod defs = (Monads.withTrace (Strings.cat2 "encode module: " (Module.unNamespace (Module.moduleNamespace mod))) (Flows.bind (encodeDefinitions mod defs) (\units -> Flows.pure (Maps.fromList (Lists.map (\entry ->  
  let name = (Pairs.first entry)
  in  
    let unit = (Pairs.second entry)
    in (bindingNameToFilePath name, (Serialization.printExpr (Serialization.parenthesize (Serde.writeCompilationUnit unit))))) (Maps.toList units))))))
