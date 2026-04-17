-- Note: this is an automatically generated file. Do not edit.

-- | Java code generator: converts Hydra modules to Java source code

module Hydra.Java.Coder where

import qualified Hydra.Analysis as Analysis
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Arity as Arity
import qualified Hydra.Checking as Checking
import qualified Hydra.Constants as Constants
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Dependencies as Dependencies
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Environment as Environment
import qualified Hydra.Errors as Errors
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Java.Environment as JavaEnvironment
import qualified Hydra.Java.Language as Language
import qualified Hydra.Java.Names as JavaNames
import qualified Hydra.Java.Serde as Serde
import qualified Hydra.Java.Syntax as Syntax
import qualified Hydra.Java.Utils as Utils
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
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Predicates as Predicates
import qualified Hydra.Resolution as Resolution
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Strip as Strip
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Variables as Variables
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S

addComment :: Syntax.ClassBodyDeclaration -> Core.FieldType -> t0 -> Graph.Graph -> Either Errors.Error Syntax.ClassBodyDeclarationWithComments
addComment decl field cx g =
    Eithers.map (\c -> Syntax.ClassBodyDeclarationWithComments {
      Syntax.classBodyDeclarationWithCommentsValue = decl,
      Syntax.classBodyDeclarationWithCommentsComments = c}) (Annotations.commentsFromFieldType cx g field)

analyzeJavaFunction :: JavaEnvironment.JavaEnvironment -> Core.Term -> Context.Context -> t0 -> Either t1 (Typing.FunctionStructure JavaEnvironment.JavaEnvironment)
analyzeJavaFunction env term cx g = Analysis.analyzeFunctionTerm cx javaEnvGetGraph javaEnvSetGraph env term

annotateBodyWithCod :: Core.Type -> Core.Term -> Core.Term
annotateBodyWithCod typ term =

      let setAnn = \t -> Annotations.setTermAnnotation Constants.key_type (Just (EncodeCore.type_ typ)) t
      in case (Strip.deannotateTerm term) of
        Core.TermTypeApplication _ -> setAnn term
        Core.TermApplication v0 ->
          let lhs = Core.applicationFunction v0
              rhs = Core.applicationArgument v0
              annotatedRhs =
                      case (Strip.deannotateTerm rhs) of
                        Core.TermTypeApplication _ -> annotateBodyWithCod (extractArgType lhs typ) rhs
                        _ -> rhs
          in (setAnn (Core.TermApplication (Core.Application {
            Core.applicationFunction = lhs,
            Core.applicationArgument = annotatedRhs})))
        _ -> setAnn term

annotateLambdaArgs :: Core.Name -> [Core.Type] -> [Core.Term] -> t0 -> Graph.Graph -> Either t1 [Core.Term]
annotateLambdaArgs cname tApps argTerms cx g =
    Logic.ifElse (Lists.null tApps) (Right argTerms) (Eithers.bind (Eithers.bind (Right (Lexical.lookupBinding g cname)) (\mel -> Maybes.cases mel (Right (Maybes.map (\prim -> Graph.primitiveType prim) (Maps.lookup cname (Graph.graphPrimitives g)))) (\el -> Right (Core.bindingType el)))) (\mts -> Maybes.cases mts (Right argTerms) (\ts ->
      let schemeType = Core.typeSchemeType ts
          schemeTypeVars = collectTypeVars schemeType
          schemeVars = Lists.filter (\v -> Sets.member v schemeTypeVars) (Core.typeSchemeVariables ts)
      in (Logic.ifElse (Logic.or (Lists.null schemeVars) (Logic.not (Equality.equal (Lists.length schemeVars) (Lists.length tApps)))) (Right argTerms) (
        let subst = Maps.fromList (Lists.zip schemeVars tApps)
            expectedTypes = peelExpectedTypes subst (Lists.length argTerms) schemeType
        in (Right (Lists.zipWith (\arg -> \mExpected -> propagateType mExpected arg) argTerms (Lists.concat2 expectedTypes (Lists.replicate (Lists.length argTerms) (Core.TypeVariable (Core.Name "unused")))))))))))

applyCastIfSafe :: JavaEnvironment.Aliases -> Core.Type -> Syntax.Expression -> t0 -> Graph.Graph -> Either Errors.Error Syntax.Expression
applyCastIfSafe aliases castType expr cx g =

      let trusted = JavaEnvironment.aliasesTrustedTypeVars aliases
          inScope = JavaEnvironment.aliasesInScopeTypeParams aliases
          castVars = collectTypeVars castType
          javaTypeVars =
                  Sets.fromList (Lists.filter (\v -> Logic.or (Sets.member v inScope) (isLambdaBoundVariable v)) (Sets.toList castVars))
          isSafe = Logic.or (Sets.null trusted) (Logic.or (Sets.null javaTypeVars) (Sets.null (Sets.difference javaTypeVars trusted)))
      in (Logic.ifElse isSafe (Eithers.bind (encodeType aliases Sets.empty castType cx g) (\jtype -> Eithers.bind (Utils.javaTypeToJavaReferenceType jtype cx) (\rt -> Right (Utils.javaCastExpressionToJavaExpression (Utils.javaCastExpression rt (Utils.javaExpressionToJavaUnaryExpression expr)))))) (Right expr))

applyJavaArg :: Syntax.Expression -> Syntax.Expression -> Syntax.Expression
applyJavaArg expr jarg =
    Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocation (Just (Right (Utils.javaExpressionToJavaPrimary expr))) (Syntax.Identifier JavaNames.applyMethodName) [
      jarg])

applyOvergenSubstToTermAnnotations :: M.Map Core.Name Core.Type -> Core.Term -> t0 -> Graph.Graph -> Either t1 Core.Term
applyOvergenSubstToTermAnnotations subst term0 cx g = Right (applyOvergenSubstToTermAnnotations_go subst g term0)

applyOvergenSubstToTermAnnotations_go :: M.Map Core.Name Core.Type -> Graph.Graph -> Core.Term -> Core.Term
applyOvergenSubstToTermAnnotations_go subst cx term =
    case term of
      Core.TermAnnotated v0 ->
        let inner = Core.annotatedTermBody v0
            ann = Core.annotatedTermAnnotation v0
            ann_ =
                    Maybes.cases (Maps.lookup Constants.key_type ann) ann (\typeTerm -> Eithers.either (\_ -> ann) (\t ->
                      let t_ = substituteTypeVarsWithTypes subst t
                      in (Maps.insert Constants.key_type (EncodeCore.type_ t_) ann)) (DecodeCore.type_ cx typeTerm))
        in (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (applyOvergenSubstToTermAnnotations_go subst cx inner),
          Core.annotatedTermAnnotation = ann_}))
      Core.TermApplication v0 -> Core.TermApplication (Core.Application {
        Core.applicationFunction = (applyOvergenSubstToTermAnnotations_go subst cx (Core.applicationFunction v0)),
        Core.applicationArgument = (applyOvergenSubstToTermAnnotations_go subst cx (Core.applicationArgument v0))})
      Core.TermLambda v0 -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.lambdaParameter v0),
        Core.lambdaDomain = (Maybes.map (\d -> substituteTypeVarsWithTypes subst d) (Core.lambdaDomain v0)),
        Core.lambdaBody = (applyOvergenSubstToTermAnnotations_go subst cx (Core.lambdaBody v0))})
      Core.TermCases v0 -> Core.TermCases (Core.CaseStatement {
        Core.caseStatementTypeName = (Core.caseStatementTypeName v0),
        Core.caseStatementDefault = (Maybes.map (\d -> applyOvergenSubstToTermAnnotations_go subst cx d) (Core.caseStatementDefault v0)),
        Core.caseStatementCases = (Lists.map (\fld -> Core.Field {
          Core.fieldName = (Core.fieldName fld),
          Core.fieldTerm = (applyOvergenSubstToTermAnnotations_go subst cx (Core.fieldTerm fld))}) (Core.caseStatementCases v0))})
      Core.TermLet v0 -> Core.TermLet (Core.Let {
        Core.letBindings = (Lists.map (\b -> Core.Binding {
          Core.bindingName = (Core.bindingName b),
          Core.bindingTerm = (applyOvergenSubstToTermAnnotations_go subst cx (Core.bindingTerm b)),
          Core.bindingType = (Core.bindingType b)}) (Core.letBindings v0)),
        Core.letBody = (applyOvergenSubstToTermAnnotations_go subst cx (Core.letBody v0))})
      Core.TermTypeApplication v0 -> Core.TermTypeApplication (Core.TypeApplicationTerm {
        Core.typeApplicationTermBody = (applyOvergenSubstToTermAnnotations_go subst cx (Core.typeApplicationTermBody v0)),
        Core.typeApplicationTermType = (substituteTypeVarsWithTypes subst (Core.typeApplicationTermType v0))})
      Core.TermTypeLambda v0 -> Core.TermTypeLambda (Core.TypeLambda {
        Core.typeLambdaParameter = (Core.typeLambdaParameter v0),
        Core.typeLambdaBody = (applyOvergenSubstToTermAnnotations_go subst cx (Core.typeLambdaBody v0))})
      _ -> term

applySubstFull :: M.Map Core.Name Core.Type -> Core.Type -> Core.Type
applySubstFull s t =
    case (Strip.deannotateType t) of
      Core.TypeVariable v0 -> Maps.findWithDefault t v0 s
      Core.TypeFunction v0 -> Core.TypeFunction (Core.FunctionType {
        Core.functionTypeDomain = (applySubstFull s (Core.functionTypeDomain v0)),
        Core.functionTypeCodomain = (applySubstFull s (Core.functionTypeCodomain v0))})
      Core.TypeApplication v0 -> Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = (applySubstFull s (Core.applicationTypeFunction v0)),
        Core.applicationTypeArgument = (applySubstFull s (Core.applicationTypeArgument v0))})
      Core.TypeList v0 -> Core.TypeList (applySubstFull s v0)
      Core.TypeSet v0 -> Core.TypeSet (applySubstFull s v0)
      Core.TypeMaybe v0 -> Core.TypeMaybe (applySubstFull s v0)
      Core.TypeMap v0 -> Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = (applySubstFull s (Core.mapTypeKeys v0)),
        Core.mapTypeValues = (applySubstFull s (Core.mapTypeValues v0))})
      Core.TypePair v0 -> Core.TypePair (Core.PairType {
        Core.pairTypeFirst = (applySubstFull s (Core.pairTypeFirst v0)),
        Core.pairTypeSecond = (applySubstFull s (Core.pairTypeSecond v0))})
      Core.TypeEither v0 -> Core.TypeEither (Core.EitherType {
        Core.eitherTypeLeft = (applySubstFull s (Core.eitherTypeLeft v0)),
        Core.eitherTypeRight = (applySubstFull s (Core.eitherTypeRight v0))})
      Core.TypeForall v0 -> Core.TypeForall (Core.ForallType {
        Core.forallTypeParameter = (Core.forallTypeParameter v0),
        Core.forallTypeBody = (applySubstFull (Maps.delete (Core.forallTypeParameter v0) s) (Core.forallTypeBody v0))})
      _ -> t

applySubstSimple :: M.Map Core.Name Core.Type -> Core.Type -> Core.Type
applySubstSimple subst t =
    case (Strip.deannotateType t) of
      Core.TypeVariable v0 -> Maps.findWithDefault t v0 subst
      _ -> t

arraysCompareExpr :: String -> String -> Syntax.Expression
arraysCompareExpr otherVar fname =

      let header =
              Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
                Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantType (Utils.javaTypeName (Syntax.Identifier "java.util.Arrays"))),
                Syntax.methodInvocation_ComplexTypeArguments = [],
                Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier "compare")})
          arg1 =
                  Utils.javaExpressionNameToJavaExpression (Syntax.ExpressionName {
                    Syntax.expressionNameQualifier = Nothing,
                    Syntax.expressionNameIdentifier = (Syntax.Identifier (Utils.sanitizeJavaName fname))})
          arg2 =
                  Utils.javaExpressionNameToJavaExpression (Utils.fieldExpression (Utils.javaIdentifier otherVar) (Utils.javaIdentifier fname))
      in (Utils.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
        Syntax.methodInvocationHeader = header,
        Syntax.methodInvocationArguments = [
          arg1,
          arg2]}))

arraysEqualsClause :: String -> String -> Syntax.InclusiveOrExpression
arraysEqualsClause tmpName fname =

      let thisArg =
              Utils.javaExpressionNameToJavaExpression (Utils.fieldExpression (Syntax.Identifier "this") (Utils.javaIdentifier fname))
          otherArg =
                  Utils.javaExpressionNameToJavaExpression (Utils.fieldExpression (Utils.javaIdentifier tmpName) (Utils.javaIdentifier fname))
          header =
                  Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
                    Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantType (Utils.javaTypeName (Syntax.Identifier "java.util.Arrays"))),
                    Syntax.methodInvocation_ComplexTypeArguments = [],
                    Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier JavaNames.equalsMethodName)})
      in (Utils.javaPostfixExpressionToJavaInclusiveOrExpression (Utils.javaMethodInvocationToJavaPostfixExpression (Syntax.MethodInvocation {
        Syntax.methodInvocationHeader = header,
        Syntax.methodInvocationArguments = [
          thisArg,
          otherArg]})))

augmentVariantClass :: JavaEnvironment.Aliases -> [Syntax.TypeParameter] -> Core.Name -> Syntax.ClassDeclaration -> Syntax.ClassDeclaration
augmentVariantClass aliases tparams elName cd =
    case cd of
      Syntax.ClassDeclarationNormal v0 ->
        let args = Lists.map (\tp -> Utils.typeParameterToTypeArgument tp) tparams
            extendsPart = Utils.nameToJavaClassType aliases True args elName Nothing
            newMods =
                    [
                      Syntax.ClassModifierPublic,
                      Syntax.ClassModifierStatic,
                      Syntax.ClassModifierFinal]
            oldBody = Syntax.normalClassDeclarationBody v0
            oldDecls = Syntax.unClassBody oldBody
            acceptDecl = noComment (Utils.toAcceptMethod False tparams)
            newBody = Syntax.ClassBody (Lists.concat2 oldDecls [
                  acceptDecl])
        in (Syntax.ClassDeclarationNormal (Syntax.NormalClassDeclaration {
          Syntax.normalClassDeclarationModifiers = newMods,
          Syntax.normalClassDeclarationIdentifier = (Syntax.normalClassDeclarationIdentifier v0),
          Syntax.normalClassDeclarationParameters = tparams,
          Syntax.normalClassDeclarationExtends = (Just extendsPart),
          Syntax.normalClassDeclarationImplements = (Syntax.normalClassDeclarationImplements v0),
          Syntax.normalClassDeclarationBody = newBody}))
      _ -> cd

bindingIsFunctionType :: Core.Binding -> Bool
bindingIsFunctionType b =
    Maybes.maybe (case (Strip.deannotateTerm (Core.bindingTerm b)) of
      Core.TermLambda _ -> True
      Core.TermProject _ -> True
      Core.TermCases _ -> True
      Core.TermUnwrap _ -> True
      _ -> False) (\ts -> case (Strip.deannotateType (Core.typeSchemeType ts)) of
      Core.TypeFunction _ -> True
      Core.TypeForall v0 -> case (Strip.deannotateType (Core.forallTypeBody v0)) of
        Core.TypeFunction _ -> True
        _ -> False
      _ -> False) (Core.bindingType b)

bindingNameToFilePath :: Core.Name -> String
bindingNameToFilePath name =

      let qn = Names.qualifyName name
          ns_ = Packaging.qualifiedNameNamespace qn
          local = Packaging.qualifiedNameLocal qn
          sanitized = Formatting.sanitizeWithUnderscores Language.reservedWords local
          unq =
                  Names.unqualifyName (Packaging.QualifiedName {
                    Packaging.qualifiedNameNamespace = ns_,
                    Packaging.qualifiedNameLocal = sanitized})
      in (Names.nameToFilePath Util.CaseConventionCamel Util.CaseConventionPascal (Packaging.FileExtension "java") unq)

bindingsToStatements :: JavaEnvironment.JavaEnvironment -> [Core.Binding] -> Context.Context -> Graph.Graph -> Either Errors.Error ([Syntax.BlockStatement], JavaEnvironment.JavaEnvironment)
bindingsToStatements env bindings cx g0 =

      let aliases = JavaEnvironment.javaEnvironmentAliases env
          g = JavaEnvironment.javaEnvironmentGraph env
          flatBindings = dedupBindings (JavaEnvironment.aliasesInScopeJavaVars aliases) (flattenBindings bindings)
          gExtended =
                  Scoping.extendGraphForLet (\g2 -> \b -> Logic.ifElse (Predicates.isComplexBinding g2 b) (Just (Core.TermLiteral (Core.LiteralBoolean True))) Nothing) g (Core.Let {
                    Core.letBindings = flatBindings,
                    Core.letBody = (Core.TermVariable (Core.Name "dummy"))})
          bindingVars = Sets.fromList (Lists.map (\b -> Core.bindingName b) flatBindings)
          allDeps =
                  Maps.fromList (Lists.map (\b ->
                    let key = Core.bindingName b
                        deps = Sets.intersection bindingVars (Variables.freeVariablesInTerm (Core.bindingTerm b))
                    in (key, deps)) flatBindings)
          sorted =
                  Sorting.topologicalSortComponents (Lists.map (\entry ->
                    let key = Pairs.first entry
                        deps = Pairs.second entry
                    in (key, (Sets.toList deps))) (Maps.toList allDeps))
          recursiveVars =
                  Sets.fromList (Lists.concat (Lists.map (\names -> Logic.ifElse (Equality.equal (Lists.length names) 1) (Maybes.maybe [] (\singleName -> Maybes.cases (Maps.lookup singleName allDeps) [] (\deps -> Logic.ifElse (Sets.member singleName deps) [
                    singleName] [])) (Lists.maybeHead names)) names) sorted))
          thunkedVars =
                  Sets.fromList (Lists.concat (Lists.map (\b ->
                    let bname = Core.bindingName b
                    in (Logic.ifElse (Logic.and (Logic.not (Sets.member bname recursiveVars)) (Logic.and (needsThunking (Core.bindingTerm b)) (Logic.not (bindingIsFunctionType b)))) [
                      bname] [])) flatBindings))
          aliasesExtended =
                  JavaEnvironment.Aliases {
                    JavaEnvironment.aliasesCurrentNamespace = (JavaEnvironment.aliasesCurrentNamespace aliases),
                    JavaEnvironment.aliasesPackages = (JavaEnvironment.aliasesPackages aliases),
                    JavaEnvironment.aliasesBranchVars = (JavaEnvironment.aliasesBranchVars aliases),
                    JavaEnvironment.aliasesRecursiveVars = (Sets.union (JavaEnvironment.aliasesRecursiveVars aliases) recursiveVars),
                    JavaEnvironment.aliasesInScopeTypeParams = (JavaEnvironment.aliasesInScopeTypeParams aliases),
                    JavaEnvironment.aliasesPolymorphicLocals = (JavaEnvironment.aliasesPolymorphicLocals aliases),
                    JavaEnvironment.aliasesInScopeJavaVars = (Sets.union (JavaEnvironment.aliasesInScopeJavaVars aliases) bindingVars),
                    JavaEnvironment.aliasesVarRenames = (JavaEnvironment.aliasesVarRenames aliases),
                    JavaEnvironment.aliasesLambdaVars = (JavaEnvironment.aliasesLambdaVars aliases),
                    JavaEnvironment.aliasesTypeVarSubst = (JavaEnvironment.aliasesTypeVarSubst aliases),
                    JavaEnvironment.aliasesTrustedTypeVars = (JavaEnvironment.aliasesTrustedTypeVars aliases),
                    JavaEnvironment.aliasesMethodCodomain = (JavaEnvironment.aliasesMethodCodomain aliases),
                    JavaEnvironment.aliasesThunkedVars = (Sets.union (JavaEnvironment.aliasesThunkedVars aliases) thunkedVars)}
          envExtended =
                  JavaEnvironment.JavaEnvironment {
                    JavaEnvironment.javaEnvironmentAliases = aliasesExtended,
                    JavaEnvironment.javaEnvironmentGraph = gExtended}
      in (Logic.ifElse (Lists.null bindings) (Right ([], envExtended)) (Eithers.bind (Eithers.mapList (\names -> Eithers.bind (Eithers.mapList (\n -> toDeclInit aliasesExtended gExtended recursiveVars flatBindings n cx g) names) (\inits -> Eithers.bind (Eithers.mapList (\n -> toDeclStatement envExtended aliasesExtended gExtended recursiveVars thunkedVars flatBindings n cx g) names) (\decls -> Right (Lists.concat2 (Maybes.cat inits) decls)))) sorted) (\groups -> Right (Lists.concat groups, envExtended))))

boundTypeVariables :: Core.Type -> [Core.Name]
boundTypeVariables typ =
    case typ of
      Core.TypeAnnotated v0 -> boundTypeVariables (Core.annotatedTypeBody v0)
      Core.TypeForall v0 -> Lists.cons (Core.forallTypeParameter v0) (boundTypeVariables (Core.forallTypeBody v0))
      _ -> []

buildArgSubst :: S.Set Core.Name -> [Core.Type] -> [t0] -> M.Map Core.Name t0
buildArgSubst schemeVarSet schemeDoms argTypes =
    Maps.fromList (Lists.bind (Lists.zip schemeDoms argTypes) (\p ->
      let sdom = Pairs.first p
          argType = Pairs.second p
      in case (Strip.deannotateType sdom) of
        Core.TypeVariable v0 -> Logic.ifElse (Sets.member v0 schemeVarSet) [
          (v0, argType)] []
        _ -> []))

buildCurriedLambda :: [Core.Name] -> Syntax.Expression -> Syntax.Expression
buildCurriedLambda params inner = Lists.foldl (\acc -> \p -> Utils.javaLambda p acc) inner (Lists.reverse params)

buildSubstFromAnnotations :: S.Set Core.Name -> Core.Term -> t0 -> Graph.Graph -> Either t1 (M.Map Core.Name Core.Name)
buildSubstFromAnnotations schemeVarSet term cx g = Right (buildSubstFromAnnotations_go schemeVarSet g term)

buildSubstFromAnnotations_go :: S.Set Core.Name -> Graph.Graph -> Core.Term -> M.Map Core.Name Core.Name
buildSubstFromAnnotations_go schemeVarSet g term =
    case term of
      Core.TermAnnotated v0 ->
        let body = Core.annotatedTermBody v0
            anns = Core.annotatedTermAnnotation v0
            bodySubst = buildSubstFromAnnotations_go schemeVarSet g body
            annSubst =
                    Maybes.cases (Maps.lookup Constants.key_type anns) Maps.empty (\typeTerm -> Eithers.either (\_ -> Maps.empty) (\annType -> case (Strip.deannotateTerm body) of
                      Core.TermLambda v1 -> Maybes.cases (Core.lambdaDomain v1) Maps.empty (\dom -> case (Strip.deannotateType annType) of
                        Core.TypeFunction v2 -> buildTypeVarSubst schemeVarSet (Core.functionTypeDomain v2) dom
                        _ -> Maps.empty)
                      _ -> Maps.empty) (DecodeCore.type_ g typeTerm))
        in (Maps.union annSubst bodySubst)
      Core.TermApplication v0 -> Maps.union (buildSubstFromAnnotations_go schemeVarSet g (Core.applicationFunction v0)) (buildSubstFromAnnotations_go schemeVarSet g (Core.applicationArgument v0))
      Core.TermLambda v0 -> buildSubstFromAnnotations_go schemeVarSet g (Core.lambdaBody v0)
      Core.TermCases v0 ->
        let defSubst = Maybes.cases (Core.caseStatementDefault v0) Maps.empty (\d -> buildSubstFromAnnotations_go schemeVarSet g d)
            caseSubsts =
                    Lists.foldl (\acc -> \fld -> Maps.union acc (buildSubstFromAnnotations_go schemeVarSet g (Core.fieldTerm fld))) Maps.empty (Core.caseStatementCases v0)
        in (Maps.union defSubst caseSubsts)
      Core.TermLet v0 ->
        let bindingSubst =
                Lists.foldl (\acc -> \b -> Maps.union acc (buildSubstFromAnnotations_go schemeVarSet g (Core.bindingTerm b))) Maps.empty (Core.letBindings v0)
        in (Maps.union bindingSubst (buildSubstFromAnnotations_go schemeVarSet g (Core.letBody v0)))
      Core.TermList v0 -> Lists.foldl (\acc -> \t -> Maps.union acc (buildSubstFromAnnotations_go schemeVarSet g t)) Maps.empty v0
      Core.TermMaybe v0 -> Maybes.cases v0 Maps.empty (\t -> buildSubstFromAnnotations_go schemeVarSet g t)
      Core.TermPair v0 -> Maps.union (buildSubstFromAnnotations_go schemeVarSet g (Pairs.first v0)) (buildSubstFromAnnotations_go schemeVarSet g (Pairs.second v0))
      Core.TermRecord v0 -> Lists.foldl (\acc -> \fld -> Maps.union acc (buildSubstFromAnnotations_go schemeVarSet g (Core.fieldTerm fld))) Maps.empty (Core.recordFields v0)
      Core.TermSet v0 -> Lists.foldl (\acc -> \t -> Maps.union acc (buildSubstFromAnnotations_go schemeVarSet g t)) Maps.empty (Sets.toList v0)
      Core.TermTypeApplication v0 -> buildSubstFromAnnotations_go schemeVarSet g (Core.typeApplicationTermBody v0)
      Core.TermTypeLambda v0 -> buildSubstFromAnnotations_go schemeVarSet g (Core.typeLambdaBody v0)
      Core.TermEither v0 -> Eithers.either (\t -> buildSubstFromAnnotations_go schemeVarSet g t) (\t -> buildSubstFromAnnotations_go schemeVarSet g t) v0
      _ -> Maps.empty

buildTypeSubst :: S.Set Core.Name -> Core.Type -> Core.Type -> M.Map Core.Name Core.Type
buildTypeSubst schemeVarSet schemeType actualType =
    buildTypeSubst_go schemeVarSet (Strip.deannotateType schemeType) (Strip.deannotateType actualType)

buildTypeSubst_go :: S.Set Core.Name -> Core.Type -> Core.Type -> M.Map Core.Name Core.Type
buildTypeSubst_go svs st at =

      let goSub = \a -> \b -> buildTypeSubst_go svs (Strip.deannotateType a) (Strip.deannotateType b)
      in case st of
        Core.TypeVariable v0 -> Logic.ifElse (Sets.member v0 svs) (Maps.singleton v0 at) Maps.empty
        Core.TypeFunction v0 -> case at of
          Core.TypeFunction v1 -> Maps.union (goSub (Core.functionTypeDomain v0) (Core.functionTypeDomain v1)) (goSub (Core.functionTypeCodomain v0) (Core.functionTypeCodomain v1))
          _ -> Maps.empty
        Core.TypeApplication v0 -> case at of
          Core.TypeApplication v1 -> Maps.union (goSub (Core.applicationTypeFunction v0) (Core.applicationTypeFunction v1)) (goSub (Core.applicationTypeArgument v0) (Core.applicationTypeArgument v1))
          _ -> Maps.empty
        Core.TypeList v0 -> case at of
          Core.TypeList v1 -> goSub v0 v1
          _ -> Maps.empty
        Core.TypeSet v0 -> case at of
          Core.TypeSet v1 -> goSub v0 v1
          _ -> Maps.empty
        Core.TypeMaybe v0 -> case at of
          Core.TypeMaybe v1 -> goSub v0 v1
          _ -> Maps.empty
        Core.TypeMap v0 -> case at of
          Core.TypeMap v1 -> Maps.union (goSub (Core.mapTypeKeys v0) (Core.mapTypeKeys v1)) (goSub (Core.mapTypeValues v0) (Core.mapTypeValues v1))
          _ -> Maps.empty
        Core.TypePair v0 -> case at of
          Core.TypePair v1 -> Maps.union (goSub (Core.pairTypeFirst v0) (Core.pairTypeFirst v1)) (goSub (Core.pairTypeSecond v0) (Core.pairTypeSecond v1))
          _ -> Maps.empty
        Core.TypeEither v0 -> case at of
          Core.TypeEither v1 -> Maps.union (goSub (Core.eitherTypeLeft v0) (Core.eitherTypeLeft v1)) (goSub (Core.eitherTypeRight v0) (Core.eitherTypeRight v1))
          _ -> Maps.empty
        Core.TypeForall v0 -> case at of
          Core.TypeForall v1 -> goSub (Core.forallTypeBody v0) (Core.forallTypeBody v1)
          _ -> goSub (Core.forallTypeBody v0) at
        _ -> Maps.empty

buildTypeVarSubst :: S.Set Core.Name -> Core.Type -> Core.Type -> M.Map Core.Name Core.Name
buildTypeVarSubst schemeVarSet freshTyp canonTyp =
    buildTypeVarSubst_go schemeVarSet (Strip.deannotateType freshTyp) (Strip.deannotateType canonTyp)

buildTypeVarSubst_go :: S.Set Core.Name -> Core.Type -> Core.Type -> M.Map Core.Name Core.Name
buildTypeVarSubst_go svs ft ct =

      let goSub = \a -> \b -> buildTypeVarSubst_go svs (Strip.deannotateType a) (Strip.deannotateType b)
      in case ft of
        Core.TypeVariable v0 -> case ct of
          Core.TypeVariable v1 -> Logic.ifElse (Logic.and (Logic.not (Equality.equal v0 v1)) (Sets.member v1 svs)) (Maps.singleton v0 v1) Maps.empty
          _ -> Maps.empty
        Core.TypeFunction v0 -> case ct of
          Core.TypeFunction v1 -> Maps.union (goSub (Core.functionTypeDomain v0) (Core.functionTypeDomain v1)) (goSub (Core.functionTypeCodomain v0) (Core.functionTypeCodomain v1))
          _ -> Maps.empty
        Core.TypeApplication v0 -> case ct of
          Core.TypeApplication v1 -> Maps.union (goSub (Core.applicationTypeFunction v0) (Core.applicationTypeFunction v1)) (goSub (Core.applicationTypeArgument v0) (Core.applicationTypeArgument v1))
          _ -> Maps.empty
        Core.TypeList v0 -> case ct of
          Core.TypeList v1 -> goSub v0 v1
          _ -> Maps.empty
        Core.TypeSet v0 -> case ct of
          Core.TypeSet v1 -> goSub v0 v1
          _ -> Maps.empty
        Core.TypeMaybe v0 -> case ct of
          Core.TypeMaybe v1 -> goSub v0 v1
          _ -> Maps.empty
        Core.TypeMap v0 -> case ct of
          Core.TypeMap v1 -> Maps.union (goSub (Core.mapTypeKeys v0) (Core.mapTypeKeys v1)) (goSub (Core.mapTypeValues v0) (Core.mapTypeValues v1))
          _ -> Maps.empty
        Core.TypePair v0 -> case ct of
          Core.TypePair v1 -> Maps.union (goSub (Core.pairTypeFirst v0) (Core.pairTypeFirst v1)) (goSub (Core.pairTypeSecond v0) (Core.pairTypeSecond v1))
          _ -> Maps.empty
        Core.TypeEither v0 -> case ct of
          Core.TypeEither v1 -> Maps.union (goSub (Core.eitherTypeLeft v0) (Core.eitherTypeLeft v1)) (goSub (Core.eitherTypeRight v0) (Core.eitherTypeRight v1))
          _ -> Maps.empty
        Core.TypeForall v0 -> case ct of
          Core.TypeForall v1 -> goSub (Core.forallTypeBody v0) (Core.forallTypeBody v1)
          _ -> buildTypeVarSubst_go svs (Strip.deannotateType (Core.forallTypeBody v0)) ct
        _ -> case ct of
          Core.TypeForall v0 -> buildTypeVarSubst_go svs ft (Strip.deannotateType (Core.forallTypeBody v0))
          _ -> Maps.empty

classModsPublic :: [Syntax.ClassModifier]
classModsPublic = [
  Syntax.ClassModifierPublic]

classifyDataReference :: Core.Name -> t0 -> Graph.Graph -> Either Errors.Error JavaEnvironment.JavaSymbolClass
classifyDataReference name cx g =
    Eithers.bind (Right (Lexical.lookupBinding g name)) (\mel -> Maybes.cases mel (Right JavaEnvironment.JavaSymbolClassLocalVariable) (\el -> Maybes.cases (Core.bindingType el) (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "no type scheme for element " (Core.unName (Core.bindingName el)))))) (\ts -> Right (classifyDataTerm ts (Core.bindingTerm el)))))

classifyDataTerm :: Core.TypeScheme -> Core.Term -> JavaEnvironment.JavaSymbolClass
classifyDataTerm ts term =
    Logic.ifElse (Dependencies.isLambda term) (
      let n = classifyDataTerm_countLambdaParams term
      in (Logic.ifElse (Equality.gt n 1) (JavaEnvironment.JavaSymbolClassHoistedLambda n) JavaEnvironment.JavaSymbolClassUnaryFunction)) (
      let hasTypeParams = Logic.not (Lists.null (Core.typeSchemeVariables ts))
      in (Logic.ifElse hasTypeParams (
        let n2 = classifyDataTerm_countLambdaParams (classifyDataTerm_stripTypeLambdas term)
        in (Logic.ifElse (Equality.gt n2 0) (JavaEnvironment.JavaSymbolClassHoistedLambda n2) JavaEnvironment.JavaSymbolClassNullaryFunction)) JavaEnvironment.JavaSymbolClassNullaryFunction))

classifyDataTerm_countLambdaParams :: Core.Term -> Int
classifyDataTerm_countLambdaParams t =
    case (Strip.deannotateTerm t) of
      Core.TermLambda v0 -> Math.add 1 (classifyDataTerm_countLambdaParams (Core.lambdaBody v0))
      Core.TermLet v0 -> classifyDataTerm_countLambdaParams (Core.letBody v0)
      _ -> 0

classifyDataTerm_stripTypeLambdas :: Core.Term -> Core.Term
classifyDataTerm_stripTypeLambdas t =
    case (Strip.deannotateTerm t) of
      Core.TermTypeLambda v0 -> classifyDataTerm_stripTypeLambdas (Core.typeLambdaBody v0)
      _ -> t

cmpDeclStatement :: t0 -> Syntax.BlockStatement
cmpDeclStatement aliases =
    Utils.variableDeclarationStatement aliases Utils.javaIntType (Utils.javaIdentifier "cmp") (Utils.javaIntExpression 0)

cmpNotZeroExpr :: Syntax.Expression
cmpNotZeroExpr =
    Utils.javaEqualityExpressionToJavaExpression (Syntax.EqualityExpressionNotEqual (Syntax.EqualityExpression_Binary {
      Syntax.equalityExpression_BinaryLhs = lhs,
      Syntax.equalityExpression_BinaryRhs = rhs}))
  where
    lhs =
        Utils.javaRelationalExpressionToJavaEqualityExpression (Utils.javaPostfixExpressionToJavaRelationalExpression (Syntax.PostfixExpressionName (Syntax.ExpressionName {
          Syntax.expressionNameQualifier = Nothing,
          Syntax.expressionNameIdentifier = (Utils.javaIdentifier "cmp")})))
    rhs =
        Utils.javaPostfixExpressionToJavaRelationalExpression (Syntax.PostfixExpressionPrimary (Utils.javaLiteralToJavaPrimary (Utils.javaInt 0)))

collectForallParams :: Core.Type -> [Core.Name]
collectForallParams t =
    case (Strip.deannotateType t) of
      Core.TypeForall v0 -> Lists.cons (Core.forallTypeParameter v0) (collectForallParams (Core.forallTypeBody v0))
      _ -> []

collectLambdaDomains :: Core.Term -> ([Core.Type], Core.Term)
collectLambdaDomains t =
    case (Strip.deannotateTerm t) of
      Core.TermLambda v0 -> Maybes.cases (Core.lambdaDomain v0) ([], t) (\dom ->
        let rest = collectLambdaDomains (Core.lambdaBody v0)
        in (Lists.cons dom (Pairs.first rest), (Pairs.second rest)))
      _ -> ([], t)

collectTypeApps :: Core.Term -> [Core.Type] -> (Core.Term, [Core.Type])
collectTypeApps t acc =
    case (Strip.deannotateTerm t) of
      Core.TermTypeApplication v0 -> collectTypeApps (Core.typeApplicationTermBody v0) (Lists.cons (Core.typeApplicationTermType v0) acc)
      _ -> (Strip.deannotateTerm t, acc)

collectTypeApps0 :: Core.Term -> [Core.Type] -> (Core.Term, [Core.Type])
collectTypeApps0 t acc =
    case (Strip.deannotateTerm t) of
      Core.TermTypeApplication v0 -> collectTypeApps0 (Core.typeApplicationTermBody v0) (Lists.cons (Core.typeApplicationTermType v0) acc)
      _ -> (t, acc)

collectTypeVars :: Core.Type -> S.Set Core.Name
collectTypeVars typ = collectTypeVars_go (Strip.deannotateType typ)

collectTypeVars_go :: Core.Type -> S.Set Core.Name
collectTypeVars_go t =
    case t of
      Core.TypeVariable v0 -> Sets.singleton v0
      Core.TypeFunction v0 -> Sets.union (collectTypeVars_go (Strip.deannotateType (Core.functionTypeDomain v0))) (collectTypeVars_go (Strip.deannotateType (Core.functionTypeCodomain v0)))
      Core.TypeApplication v0 -> Sets.union (collectTypeVars_go (Strip.deannotateType (Core.applicationTypeFunction v0))) (collectTypeVars_go (Strip.deannotateType (Core.applicationTypeArgument v0)))
      Core.TypeList v0 -> collectTypeVars_go (Strip.deannotateType v0)
      Core.TypeSet v0 -> collectTypeVars_go (Strip.deannotateType v0)
      Core.TypeMaybe v0 -> collectTypeVars_go (Strip.deannotateType v0)
      Core.TypeMap v0 -> Sets.union (collectTypeVars_go (Strip.deannotateType (Core.mapTypeKeys v0))) (collectTypeVars_go (Strip.deannotateType (Core.mapTypeValues v0)))
      Core.TypePair v0 -> Sets.union (collectTypeVars_go (Strip.deannotateType (Core.pairTypeFirst v0))) (collectTypeVars_go (Strip.deannotateType (Core.pairTypeSecond v0)))
      Core.TypeEither v0 -> Sets.union (collectTypeVars_go (Strip.deannotateType (Core.eitherTypeLeft v0))) (collectTypeVars_go (Strip.deannotateType (Core.eitherTypeRight v0)))
      Core.TypeForall v0 -> collectTypeVars_go (Strip.deannotateType (Core.forallTypeBody v0))
      _ -> Sets.empty

comparableCompareExpr :: String -> String -> Syntax.Expression
comparableCompareExpr otherVar fname =

      let thisField = Utils.javaIdentifierToJavaExpression (Syntax.Identifier (Utils.sanitizeJavaName fname))
          otherField =
                  Utils.javaExpressionNameToJavaExpression (Utils.fieldExpression (Utils.javaIdentifier otherVar) (Utils.javaIdentifier fname))
      in (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStatic (Syntax.Identifier "hydra.util.Comparing") (Syntax.Identifier "compare") [
        thisField,
        otherField]))

compareAndReturnStmts :: String -> Core.FieldType -> [Syntax.BlockStatement]
compareAndReturnStmts otherVar f =
    [
      Syntax.BlockStatementStatement (Utils.javaAssignmentStatement (Syntax.LeftHandSideExpressionName (Syntax.ExpressionName {
        Syntax.expressionNameQualifier = Nothing,
        Syntax.expressionNameIdentifier = (Utils.javaIdentifier "cmp")})) (compareFieldExpr otherVar f)),
      (Syntax.BlockStatementStatement (Syntax.StatementIfThen (Syntax.IfThenStatement {
        Syntax.ifThenStatementExpression = cmpNotZeroExpr,
        Syntax.ifThenStatementStatement = (Utils.javaReturnStatement (Just (Utils.javaExpressionNameToJavaExpression (Syntax.ExpressionName {
          Syntax.expressionNameQualifier = Nothing,
          Syntax.expressionNameIdentifier = (Utils.javaIdentifier "cmp")}))))})))]

compareFieldExpr :: String -> Core.FieldType -> Syntax.Expression
compareFieldExpr otherVar ft =

      let fname = Core.unName (Core.fieldTypeName ft)
          ftype = Core.fieldTypeType ft
      in (Logic.ifElse (isBinaryType ftype) (arraysCompareExpr otherVar fname) (Logic.ifElse (isNonComparableType ftype) (hashCodeCompareExpr otherVar fname) (comparableCompareExpr otherVar fname)))

compareToBody :: t0 -> String -> [Core.FieldType] -> [Syntax.BlockStatement]
compareToBody aliases otherVar fields =

      let zeroStmts = [
            Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just (Utils.javaIntExpression 0)))]
      in (Maybes.fromMaybe zeroStmts (Maybes.map (\p ->
        let firstField = Pairs.first p
            restFields = Pairs.second p
        in (Logic.ifElse (Lists.null restFields) [
          Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just (compareFieldExpr otherVar firstField)))] (Lists.concat2 [
          cmpDeclStatement aliases] (Lists.concat2 (Lists.concat (Lists.map (\f -> compareAndReturnStmts otherVar f) (Lists.cons firstField (Maybes.fromMaybe [] (Lists.maybeInit restFields))))) [
          Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just (compareFieldExpr otherVar (Maybes.fromMaybe firstField (Lists.maybeLast restFields)))))])))) (Lists.uncons fields)))

compareToZeroClause :: String -> String -> Syntax.InclusiveOrExpression
compareToZeroClause tmpName fname =

      let compareToArg =
              Utils.javaExpressionNameToJavaExpression (Utils.fieldExpression (Utils.javaIdentifier tmpName) (Utils.javaIdentifier fname))
          compareToVar =
                  Syntax.MethodInvocation_VariantExpression (Utils.fieldExpression (Syntax.Identifier "this") (Utils.javaIdentifier fname))
          compareToHeader =
                  Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
                    Syntax.methodInvocation_ComplexVariant = compareToVar,
                    Syntax.methodInvocation_ComplexTypeArguments = [],
                    Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier JavaNames.compareToMethodName)})
          lhs =
                  Utils.javaRelationalExpressionToJavaEqualityExpression (Utils.javaPostfixExpressionToJavaRelationalExpression (Utils.javaMethodInvocationToJavaPostfixExpression (Syntax.MethodInvocation {
                    Syntax.methodInvocationHeader = compareToHeader,
                    Syntax.methodInvocationArguments = [
                      compareToArg]})))
          rhs =
                  Utils.javaPostfixExpressionToJavaRelationalExpression (Syntax.PostfixExpressionPrimary (Utils.javaLiteralToJavaPrimary (Utils.javaInt 0)))
      in (Utils.javaEqualityExpressionToJavaInclusiveOrExpression (Syntax.EqualityExpressionEqual (Syntax.EqualityExpression_Binary {
        Syntax.equalityExpression_BinaryLhs = lhs,
        Syntax.equalityExpression_BinaryRhs = rhs})))

constantDecl :: String -> JavaEnvironment.Aliases -> Core.Name -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.ClassBodyDeclarationWithComments
constantDecl javaName aliases name cx g =

      let mods =
              [
                Syntax.FieldModifierPublic,
                Syntax.FieldModifierStatic,
                Syntax.FieldModifierFinal]
          nameName = Utils.nameToJavaName aliases (Core.Name "hydra.core.Name")
          env =
                  JavaEnvironment.JavaEnvironment {
                    JavaEnvironment.javaEnvironmentAliases = aliases,
                    JavaEnvironment.javaEnvironmentGraph = g}
      in (Eithers.bind (encodeType aliases Sets.empty (Core.TypeVariable (Core.Name "hydra.core.Name")) cx g) (\jt -> Eithers.bind (encodeTerm env (Core.TermLiteral (Core.LiteralString (Core.unName name))) cx g) (\arg ->
        let init =
                Syntax.VariableInitializerExpression (Utils.javaConstructorCall (Utils.javaConstructorName nameName Nothing) [
                  arg] Nothing)
            var = Utils.javaVariableDeclarator (Syntax.Identifier javaName) (Just init)
        in (Right (noComment (Utils.javaMemberField mods jt var))))))

constantDeclForFieldType :: JavaEnvironment.Aliases -> Core.FieldType -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.ClassBodyDeclarationWithComments
constantDeclForFieldType aliases ftyp cx g =

      let name = Core.fieldTypeName ftyp
          javaName =
                  Formatting.nonAlnumToUnderscores (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionUpperSnake (Core.unName name))
      in (constantDecl javaName aliases name cx g)

constantDeclForTypeName :: JavaEnvironment.Aliases -> Core.Name -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.ClassBodyDeclarationWithComments
constantDeclForTypeName aliases name cx g = constantDecl "TYPE_" aliases name cx g

constructElementsInterface :: Packaging.Module -> [Syntax.InterfaceMemberDeclaration] -> (Core.Name, Syntax.CompilationUnit)
constructElementsInterface mod members =

      let ns = Packaging.moduleNamespace mod
          parentNs = namespaceParent ns
          pkg = Maybes.cases parentNs (Utils.javaPackageDeclaration ns) (\pns -> Utils.javaPackageDeclaration pns)
          mods = [
                Syntax.InterfaceModifierPublic]
          className = elementsClassName ns
          elName = elementsQualifiedName ns
          body = Syntax.InterfaceBody members
          itf =
                  Syntax.TypeDeclarationInterface (Syntax.InterfaceDeclarationNormalInterface (Syntax.NormalInterfaceDeclaration {
                    Syntax.normalInterfaceDeclarationModifiers = mods,
                    Syntax.normalInterfaceDeclarationIdentifier = (Utils.javaTypeIdentifier className),
                    Syntax.normalInterfaceDeclarationParameters = [],
                    Syntax.normalInterfaceDeclarationExtends = [],
                    Syntax.normalInterfaceDeclarationBody = body}))
          decl =
                  Syntax.TypeDeclarationWithComments {
                    Syntax.typeDeclarationWithCommentsValue = itf,
                    Syntax.typeDeclarationWithCommentsComments = (Packaging.moduleDescription mod)}
      in (elName, (Syntax.CompilationUnitOrdinary (Syntax.OrdinaryCompilationUnit {
        Syntax.ordinaryCompilationUnitPackage = (Just pkg),
        Syntax.ordinaryCompilationUnitImports = [],
        Syntax.ordinaryCompilationUnitTypes = [
          decl]})))

correctCastType :: Core.Term -> [Core.Type] -> Core.Type -> t0 -> t1 -> Either t2 Core.Type
correctCastType innerBody typeArgs fallback cx g =
    case (Strip.deannotateTerm innerBody) of
      Core.TermPair _ -> Logic.ifElse (Equality.equal (Lists.length typeArgs) 2) (Right (Core.TypePair (Core.PairType {
        Core.pairTypeFirst = (Maybes.fromMaybe fallback (Lists.maybeAt 0 typeArgs)),
        Core.pairTypeSecond = (Maybes.fromMaybe fallback (Lists.maybeAt 1 typeArgs))}))) (Right fallback)
      _ -> Right fallback

correctTypeApps :: t0 -> Core.Name -> [Core.Term] -> [Core.Type] -> t1 -> Graph.Graph -> Either Errors.Error [Core.Type]
correctTypeApps gr name args fallbackTypeApps cx g =
    Eithers.bind (Right (Lexical.lookupBinding g name)) (\mel -> Maybes.cases mel (Right fallbackTypeApps) (\el -> Maybes.cases (Core.bindingType el) (Right fallbackTypeApps) (\ts ->
      let schemeType = Core.typeSchemeType ts
          allSchemeVars = Lists.filter (\v -> isSimpleName v) (Core.typeSchemeVariables ts)
          schemeTypeVars = collectTypeVars schemeType
          usedFlags = Lists.map (\v -> Sets.member v schemeTypeVars) allSchemeVars
          usedSchemeVars = filterByFlags allSchemeVars usedFlags
          nParams = countFunctionParams schemeType
          peeled = peelDomainTypes nParams schemeType
          calleeDoms = Pairs.first peeled
          calleeCod = Pairs.second peeled
          overgenSubst = detectAccumulatorUnification calleeDoms calleeCod usedSchemeVars
          keepFlags =
                  Lists.map (\v -> Logic.and (Sets.member v schemeTypeVars) (Logic.not (Maps.member v overgenSubst))) allSchemeVars
          schemeVars = filterByFlags allSchemeVars keepFlags
          filteredFallback0 =
                  Logic.ifElse (Equality.equal (Lists.length allSchemeVars) (Lists.length fallbackTypeApps)) (filterByFlags fallbackTypeApps keepFlags) fallbackTypeApps
          filteredFallback =
                  Logic.ifElse (Maps.null overgenSubst) filteredFallback0 (Lists.map (\t -> substituteTypeVarsWithTypes overgenSubst t) filteredFallback0)
      in (Logic.ifElse (Logic.or (Lists.null schemeVars) (Logic.not (Equality.equal (Lists.length schemeVars) (Lists.length filteredFallback)))) (Right filteredFallback) (correctTypeAppsWithArgs schemeVars filteredFallback schemeType args cx g)))))

correctTypeAppsWithArgs :: [Core.Name] -> [Core.Type] -> Core.Type -> [Core.Term] -> t0 -> Graph.Graph -> Either Errors.Error [Core.Type]
correctTypeAppsWithArgs schemeVars fallbackTypeApps schemeType args cx g =

      let schemeVarSet = Sets.fromList schemeVars
          irSubst = Maps.fromList (Lists.zip schemeVars fallbackTypeApps)
          peeled = peelDomainTypes (Lists.length args) schemeType
          schemeDoms = Pairs.first peeled
      in (Eithers.bind (Eithers.mapList (\arg -> Eithers.bimap (\_de -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _de))) (\_a -> _a) (Annotations.getType g (Annotations.termAnnotationInternal arg))) args) (\mArgTypes -> Logic.ifElse (Logic.not (Lists.null (Lists.filter (\m -> Maybes.isNothing m) mArgTypes))) (Right fallbackTypeApps) (
        let argTypes = Lists.bind mArgTypes (\m -> Maybes.cases m [] (\x -> Lists.pure x))
            irDoms = Lists.map (\d -> applySubstSimple irSubst d) schemeDoms
            domsMatch =
                    Lists.null (Lists.filter (\p -> Logic.not (typesMatch (Strip.deannotateType (Pairs.first p)) (Strip.deannotateType (Pairs.second p)))) (Lists.zip irDoms argTypes))
        in (Logic.ifElse domsMatch (Right fallbackTypeApps) (Right (resolveTypeApps schemeVars fallbackTypeApps (buildArgSubst schemeVarSet schemeDoms argTypes)))))))

countFunctionParams :: Core.Type -> Int
countFunctionParams t =
    case (Strip.deannotateType t) of
      Core.TypeFunction v0 -> Math.add 1 (countFunctionParams (Core.functionTypeCodomain v0))
      _ -> 0

declarationForRecordType :: Bool -> Bool -> JavaEnvironment.Aliases -> [Syntax.TypeParameter] -> Core.Name -> [Core.FieldType] -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.ClassDeclaration
declarationForRecordType isInner isSer aliases tparams elName fields cx g =
    declarationForRecordType_ isInner isSer aliases tparams elName Nothing fields cx g

declarationForRecordType_ :: Bool -> Bool -> JavaEnvironment.Aliases -> [Syntax.TypeParameter] -> Core.Name -> Maybe Core.Name -> [Core.FieldType] -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.ClassDeclaration
declarationForRecordType_ isInner isSer aliases tparams elName parentName fields cx g =
    Eithers.bind (Eithers.mapList (\f -> recordMemberVar aliases f cx g) fields) (\memberVars -> Eithers.bind (Eithers.mapList (\p -> addComment (Pairs.first p) (Pairs.second p) cx g) (Lists.zip memberVars fields)) (\memberVars_ -> Eithers.bind (Logic.ifElse (Equality.gt (Lists.length fields) 1) (Eithers.mapList (\f -> recordWithMethod aliases elName fields f cx g) fields) (Right [])) (\withMethods -> Eithers.bind (recordConstructor aliases elName fields cx g) (\cons -> Eithers.bind (Logic.ifElse isInner (Right []) (Eithers.bind (constantDeclForTypeName aliases elName cx g) (\d -> Eithers.bind (Eithers.mapList (\f -> constantDeclForFieldType aliases f cx g) fields) (\dfields -> Right (Lists.cons d dfields))))) (\tn ->
      let comparableMethods =
              Maybes.cases parentName (Logic.ifElse (Logic.and (Logic.not isInner) isSer) [
                recordCompareToMethod aliases tparams elName fields] []) (\pn -> Logic.ifElse isSer [
                variantCompareToMethod aliases tparams pn elName fields] [])
          bodyDecls =
                  Lists.concat2 tn (Lists.concat2 memberVars_ (Lists.map (\x -> noComment x) (Lists.concat2 [
                    cons,
                    (recordEqualsMethod aliases elName fields),
                    (recordHashCodeMethod fields)] (Lists.concat2 comparableMethods withMethods))))
          ifaces = Logic.ifElse isInner (serializableTypes isSer) (interfaceTypes isSer aliases tparams elName)
      in (Right (Utils.javaClassDeclaration aliases tparams elName classModsPublic Nothing ifaces bodyDecls)))))))

declarationForUnionType :: Bool -> JavaEnvironment.Aliases -> [Syntax.TypeParameter] -> Core.Name -> [Core.FieldType] -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.ClassDeclaration
declarationForUnionType isSer aliases tparams elName fields cx g =
    Eithers.bind (Eithers.mapList (\ft ->
      let fname = Core.fieldTypeName ft
          ftype = Core.fieldTypeType ft
          rfields =
                  Logic.ifElse (Predicates.isUnitType (Strip.deannotateType ftype)) [] [
                    Core.FieldType {
                      Core.fieldTypeName = (Core.Name "value"),
                      Core.fieldTypeType = (Strip.deannotateType ftype)}]
          varName = Utils.variantClassName False elName fname
      in (Eithers.bind (declarationForRecordType_ True isSer aliases [] varName (Logic.ifElse isSer (Just elName) Nothing) rfields cx g) (\innerDecl -> Right (augmentVariantClass aliases tparams elName innerDecl)))) fields) (\variantClasses ->
      let variantDecls =
              Lists.map (\vc -> Syntax.ClassBodyDeclarationClassMember (Syntax.ClassMemberDeclarationClass vc)) variantClasses
      in (Eithers.bind (Eithers.mapList (\pair -> addComment (Pairs.first pair) (Pairs.second pair) cx g) (Lists.zip variantDecls fields)) (\variantDecls_ ->
        let privateConst = Utils.makeConstructor aliases elName True [] []
            acceptDecl = Utils.toAcceptMethod True tparams
            vtparams = Lists.concat2 tparams [
                  Utils.javaTypeParameter JavaNames.visitorReturnParameter]
            visitorMethods =
                    Lists.map (\ft ->
                      let fname = Core.fieldTypeName ft
                          typeArgs = Lists.map (\tp -> Utils.typeParameterToTypeArgument tp) tparams
                          varRef =
                                  Utils.javaClassTypeToJavaType (Utils.nameToJavaClassType aliases False typeArgs (Utils.variantClassName False elName fname) Nothing)
                          param = Utils.javaTypeToJavaFormalParameter varRef (Core.Name "instance")
                          resultR = Utils.javaTypeToJavaResult (Syntax.TypeReference Utils.visitorTypeVariable)
                      in (Utils.interfaceMethodDeclaration [] [] JavaNames.visitMethodName [
                        param] resultR Nothing)) fields
            visitorBody = Syntax.InterfaceBody visitorMethods
            visitor =
                    Utils.javaInterfaceDeclarationToJavaClassBodyDeclaration (Syntax.NormalInterfaceDeclaration {
                      Syntax.normalInterfaceDeclarationModifiers = [
                        Syntax.InterfaceModifierPublic],
                      Syntax.normalInterfaceDeclarationIdentifier = (Syntax.TypeIdentifier (Syntax.Identifier JavaNames.visitorName)),
                      Syntax.normalInterfaceDeclarationParameters = vtparams,
                      Syntax.normalInterfaceDeclarationExtends = [],
                      Syntax.normalInterfaceDeclarationBody = visitorBody})
            typeArgs = Lists.map (\tp -> Utils.typeParameterToTypeArgument tp) tparams
            visitorClassType =
                    Utils.javaClassType (Lists.concat2 (Lists.map (\tp -> Utils.typeParameterToReferenceType tp) tparams) [
                      Utils.visitorTypeVariable]) Nothing JavaNames.visitorName
            mainInstanceParam =
                    Utils.javaTypeToJavaFormalParameter (Utils.javaClassTypeToJavaType (Utils.nameToJavaClassType aliases False typeArgs elName Nothing)) (Core.Name "instance")
            resultR = Utils.javaTypeToJavaResult (Syntax.TypeReference Utils.visitorTypeVariable)
            throwStmt =
                    Syntax.BlockStatementStatement (Utils.javaThrowIllegalStateException [
                      Utils.javaAdditiveExpressionToJavaExpression (Utils.addExpressions [
                        Utils.javaStringMultiplicativeExpression "Non-exhaustive patterns when matching: ",
                        (Syntax.MultiplicativeExpressionUnary (Utils.javaIdentifierToJavaUnaryExpression (Syntax.Identifier "instance")))])])
            defaultMod = [
                  Syntax.InterfaceMethodModifierDefault]
            otherwiseDecl =
                    Utils.interfaceMethodDeclaration defaultMod [] JavaNames.otherwiseMethodName [
                      mainInstanceParam] resultR (Just [
                      throwStmt])
            pvVisitMethods =
                    Lists.map (\ft ->
                      let fname = Core.fieldTypeName ft
                          varRef =
                                  Utils.javaClassTypeToJavaType (Utils.nameToJavaClassType aliases False typeArgs (Utils.variantClassName False elName fname) Nothing)
                          param = Utils.javaTypeToJavaFormalParameter varRef (Core.Name "instance")
                          mi =
                                  Utils.methodInvocation Nothing (Syntax.Identifier JavaNames.otherwiseMethodName) [
                                    Utils.javaIdentifierToJavaExpression (Syntax.Identifier "instance")]
                          returnOtherwise =
                                  Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just (Utils.javaPrimaryToJavaExpression (Utils.javaMethodInvocationToJavaPrimary mi))))
                      in (Utils.interfaceMethodDeclaration defaultMod [] JavaNames.visitMethodName [
                        param] resultR (Just [
                        returnOtherwise]))) fields
            pvBody = Syntax.InterfaceBody (Lists.concat2 [
                  otherwiseDecl] pvVisitMethods)
            partialVisitor =
                    Utils.javaInterfaceDeclarationToJavaClassBodyDeclaration (Syntax.NormalInterfaceDeclaration {
                      Syntax.normalInterfaceDeclarationModifiers = [
                        Syntax.InterfaceModifierPublic],
                      Syntax.normalInterfaceDeclarationIdentifier = (Syntax.TypeIdentifier (Syntax.Identifier JavaNames.partialVisitorName)),
                      Syntax.normalInterfaceDeclarationParameters = vtparams,
                      Syntax.normalInterfaceDeclarationExtends = [
                        Syntax.InterfaceType visitorClassType],
                      Syntax.normalInterfaceDeclarationBody = pvBody})
        in (Eithers.bind (constantDeclForTypeName aliases elName cx g) (\tn0 -> Eithers.bind (Eithers.mapList (\ft -> constantDeclForFieldType aliases ft cx g) fields) (\tn1 ->
          let tn = Lists.concat2 [
                tn0] tn1
              otherDecls =
                      Lists.map (\d -> noComment d) [
                        privateConst,
                        acceptDecl,
                        visitor,
                        partialVisitor]
              bodyDecls =
                      Lists.concat [
                        tn,
                        otherDecls,
                        variantDecls_]
              mods = Lists.concat2 classModsPublic [
                    Syntax.ClassModifierAbstract]
          in (Right (Utils.javaClassDeclaration aliases tparams elName mods Nothing (interfaceTypes isSer aliases tparams elName) bodyDecls))))))))

decodeTypeFromTerm :: Core.Term -> Maybe Core.Type
decodeTypeFromTerm term =
    case (Strip.deannotateTerm term) of
      Core.TermInject v0 -> Logic.ifElse (Equality.equal (Core.injectionTypeName v0) (Core.Name "hydra.core.Type")) (
        let fname = Core.unName (Core.fieldName (Core.injectionField v0))
            fterm = Core.fieldTerm (Core.injectionField v0)
        in (Logic.ifElse (Equality.equal fname "variable") (case fterm of
          Core.TermWrap v1 -> case (Core.wrappedTermBody v1) of
            Core.TermLiteral v2 -> case v2 of
              Core.LiteralString v3 -> Just (Core.TypeVariable (Core.Name v3))
              _ -> Nothing
            _ -> Nothing
          _ -> Nothing) (Logic.ifElse (Equality.equal fname "annotated") (case fterm of
          Core.TermRecord v1 -> Maybes.bind (Lists.maybeHead (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.Name "body")) (Core.recordFields v1))) (\bodyField -> decodeTypeFromTerm (Core.fieldTerm bodyField))
          _ -> Nothing) (Logic.ifElse (Equality.equal fname "application") (case fterm of
          Core.TermRecord v1 -> Maybes.bind (Lists.maybeHead (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.Name "function")) (Core.recordFields v1))) (\funcField -> Maybes.bind (decodeTypeFromTerm (Core.fieldTerm funcField)) (\func -> Maybes.bind (Lists.maybeHead (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.Name "argument")) (Core.recordFields v1))) (\argField -> Maybes.map (\arg -> Core.TypeApplication (Core.ApplicationType {
            Core.applicationTypeFunction = func,
            Core.applicationTypeArgument = arg})) (decodeTypeFromTerm (Core.fieldTerm argField)))))
          _ -> Nothing) (Logic.ifElse (Equality.equal fname "function") (case fterm of
          Core.TermRecord v1 -> Maybes.bind (Lists.maybeHead (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.Name "domain")) (Core.recordFields v1))) (\domField -> Maybes.bind (decodeTypeFromTerm (Core.fieldTerm domField)) (\dom -> Maybes.bind (Lists.maybeHead (Lists.filter (\f -> Equality.equal (Core.fieldName f) (Core.Name "codomain")) (Core.recordFields v1))) (\codField -> Maybes.map (\cod -> Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = dom,
            Core.functionTypeCodomain = cod})) (decodeTypeFromTerm (Core.fieldTerm codField)))))
          _ -> Nothing) (Logic.ifElse (Equality.equal fname "literal") (case fterm of
          Core.TermInject v1 -> Logic.ifElse (Equality.equal (Core.unName (Core.fieldName (Core.injectionField v1))) "string") (Just (Core.TypeLiteral Core.LiteralTypeString)) Nothing
          _ -> Nothing) Nothing)))))) Nothing
      _ -> Nothing

dedupBindings :: S.Set Core.Name -> [Core.Binding] -> [Core.Binding]
dedupBindings inScope bs =
    Maybes.fromMaybe [] (Maybes.map (\p ->
      let b = Pairs.first p
          rest = Pairs.second p
          name = Core.bindingName b
      in (Logic.ifElse (Sets.member name inScope) (
        let newName = freshJavaName name inScope
            subst = Maps.singleton name newName
            rest2 =
                    Lists.map (\b2 -> Core.Binding {
                      Core.bindingName = (Core.bindingName b2),
                      Core.bindingTerm = (Variables.substituteVariables subst (Core.bindingTerm b2)),
                      Core.bindingType = (Core.bindingType b2)}) rest
        in (Lists.cons (Core.Binding {
          Core.bindingName = newName,
          Core.bindingTerm = (Core.bindingTerm b),
          Core.bindingType = (Core.bindingType b)}) (dedupBindings (Sets.insert newName inScope) rest2))) (Lists.cons b (dedupBindings (Sets.insert name inScope) rest)))) (Lists.uncons bs))

detectAccumulatorUnification :: [Core.Type] -> Core.Type -> [Core.Name] -> M.Map Core.Name Core.Type
detectAccumulatorUnification doms cod tparams =

      let tparamSet = Sets.fromList tparams
          allPairs = Lists.bind doms (\d -> extractInOutPair d)
          groupedByInput = groupPairsByFirst allPairs
          selfRefSubst = selfRefSubstitution groupedByInput
          directPairs = Lists.bind doms (\d -> extractDirectReturn tparamSet d)
          groupedDirect = groupPairsByFirst directPairs
          directInputVars = Sets.fromList (Lists.map (\p -> Pairs.first p) directPairs)
          codVar =
                  case (Strip.deannotateType cod) of
                    Core.TypeVariable v0 -> Just v0
                    _ -> Nothing
          directRefSubst = directRefSubstitution directInputVars codVar groupedDirect
          codSubst =
                  Maybes.maybe Maps.empty (\cv -> Logic.ifElse (Maps.member cv selfRefSubst) Maps.empty (Maybes.maybe Maps.empty (\refVar -> Logic.ifElse (Equality.equal cv refVar) Maps.empty (Maps.singleton cv refVar)) (findSelfRefVar groupedByInput))) (findPairFirst cod)
          domVars = Sets.fromList (Lists.bind doms (\d -> Sets.toList (collectTypeVars d)))
          danglingSubst =
                  Maybes.maybe Maps.empty (\cv -> Logic.ifElse (Sets.member cv domVars) Maps.empty (Maybes.maybe Maps.empty (\refVar -> Maps.singleton cv (Core.TypeVariable refVar)) (findSelfRefVar groupedByInput))) (findPairFirst cod)
      in (Maps.union (Maps.union (Maps.union (nameMapToTypeMap selfRefSubst) (nameMapToTypeMap codSubst)) danglingSubst) (nameMapToTypeMap directRefSubst))

directRefSubstitution :: (Eq t0, Ord t0) => (S.Set t0 -> Maybe t0 -> M.Map t0 [t0] -> M.Map t0 t0)
directRefSubstitution directInputVars codVar grouped =
    Lists.foldl (\subst -> \entry -> directRefSubstitution_processGroup directInputVars codVar subst (Pairs.first entry) (Pairs.second entry)) Maps.empty (Maps.toList grouped)

directRefSubstitution_processGroup :: (Eq t0, Ord t0) => (S.Set t0 -> Maybe t0 -> M.Map t0 t0 -> t0 -> [t0] -> M.Map t0 t0)
directRefSubstitution_processGroup directInputVars codVar subst inVar outVars =

      let selfRefCount = Lists.length (Lists.filter (\v -> Equality.equal v inVar) outVars)
          nonSelfVars = Lists.filter (\v -> Logic.not (Equality.equal v inVar)) outVars
          safeNonSelfVars =
                  Lists.filter (\v -> Logic.and (Logic.not (Sets.member v directInputVars)) (Logic.not (Equality.equal (Just v) codVar))) nonSelfVars
      in (Logic.ifElse (Logic.and (Equality.gte selfRefCount 2) (Logic.not (Lists.null safeNonSelfVars))) (Lists.foldl (\s -> \v -> Maps.insert v inVar s) subst safeNonSelfVars) subst)

domTypeArgs :: JavaEnvironment.Aliases -> Core.Type -> t0 -> Graph.Graph -> Either Errors.Error [Syntax.TypeArgument]
domTypeArgs aliases d cx g =

      let args = extractTypeApplicationArgs (Strip.deannotateType d)
      in (Logic.ifElse (Logic.not (Lists.null args)) (Eithers.mapList (\t -> Eithers.bind (encodeType aliases Sets.empty t cx g) (\jt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jt cx) (\rt -> Right (Syntax.TypeArgumentReference rt)))) args) (Right (javaTypeArgumentsForType d)))

elementJavaIdentifier :: Bool -> Bool -> JavaEnvironment.Aliases -> Core.Name -> Syntax.Identifier
elementJavaIdentifier isPrim isMethod aliases name =

      let qn = Names.qualifyName name
          ns_ = Packaging.qualifiedNameNamespace qn
          local = Packaging.qualifiedNameLocal qn
          sep = Logic.ifElse isMethod "::" "."
      in (Logic.ifElse isPrim (Syntax.Identifier (Strings.cat2 (Strings.cat2 (elementJavaIdentifier_qualify aliases ns_ (Formatting.capitalize local)) ".") JavaNames.applyMethodName)) (Maybes.cases ns_ (Syntax.Identifier (Utils.sanitizeJavaName local)) (\n -> Syntax.Identifier (Strings.cat2 (Strings.cat2 (elementJavaIdentifier_qualify aliases (namespaceParent n) (elementsClassName n)) sep) (Utils.sanitizeJavaName local)))))

elementJavaIdentifier_qualify :: JavaEnvironment.Aliases -> Maybe Packaging.Namespace -> String -> String
elementJavaIdentifier_qualify aliases mns s =
    Syntax.unIdentifier (Utils.nameToJavaName aliases (Names.unqualifyName (Packaging.QualifiedName {
      Packaging.qualifiedNameNamespace = mns,
      Packaging.qualifiedNameLocal = s})))

elementsClassName :: Packaging.Namespace -> String
elementsClassName ns =

      let nsStr = Packaging.unNamespace ns
          parts = Strings.splitOn "." nsStr
      in (Formatting.sanitizeWithUnderscores Language.reservedWords (Formatting.capitalize (Maybes.fromMaybe nsStr (Lists.maybeLast parts))))

elementsQualifiedName :: Packaging.Namespace -> Core.Name
elementsQualifiedName ns =
    Names.unqualifyName (Packaging.QualifiedName {
      Packaging.qualifiedNameNamespace = (namespaceParent ns),
      Packaging.qualifiedNameLocal = (elementsClassName ns)})

encodeApplication :: JavaEnvironment.JavaEnvironment -> Core.Application -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.Expression
encodeApplication env app cx g0 =

      let aliases = JavaEnvironment.javaEnvironmentAliases env
          g = JavaEnvironment.javaEnvironmentGraph env
          gathered = Analysis.gatherArgsWithTypeApps (Core.TermApplication app) [] []
          fun = Pairs.first gathered
          args = Pairs.first (Pairs.second gathered)
          typeApps = Pairs.second (Pairs.second gathered)
      in (Eithers.bind (Eithers.bimap (\_de -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _de))) (\_a -> _a) (Annotations.getType g (Annotations.termAnnotationInternal fun))) (\mfunTyp -> Eithers.bind (Maybes.cases mfunTyp (Checking.typeOfTerm cx g fun) (\t -> Right t)) (\funTyp ->
        let arity = Arity.typeArity funTyp
            deannotatedFun = Strip.deannotateTerm fun
            calleeName =
                    case deannotatedFun of
                      Core.TermVariable v0 -> Just v0
                      _ -> Nothing
        in (Eithers.bind (Maybes.cases calleeName (Right args) (\cname -> annotateLambdaArgs cname typeApps args cx g)) (\annotatedArgs -> case deannotatedFun of
          Core.TermVariable v0 -> Logic.ifElse (Maybes.isJust (Maps.lookup v0 (Graph.graphPrimitives g))) (
            let hargs = Lists.take arity annotatedArgs
                rargs = Lists.drop arity annotatedArgs
            in (Eithers.bind (functionCall env True v0 hargs [] cx g) (\initialCall -> Eithers.foldl (\acc -> \h -> Eithers.bind (encodeTerm env h cx g) (\jarg -> Right (applyJavaArg acc jarg))) initialCall rargs))) (Logic.ifElse (Logic.and (isRecursiveVariable aliases v0) (Logic.not (isLambdaBoundIn v0 (JavaEnvironment.aliasesLambdaVars aliases)))) (encodeApplication_fallback env aliases g typeApps (Core.applicationFunction app) (Core.applicationArgument app) cx g) (Eithers.bind (classifyDataReference v0 cx g) (\symClass ->
            let methodArity =
                    case symClass of
                      JavaEnvironment.JavaSymbolClassHoistedLambda v1 -> v1
                      _ -> arity
                hargs = Lists.take methodArity annotatedArgs
                rargs = Lists.drop methodArity annotatedArgs
                trusted = JavaEnvironment.aliasesTrustedTypeVars aliases
                inScope = JavaEnvironment.aliasesInScopeTypeParams aliases
                filteredTypeApps =
                        Logic.ifElse (Logic.or (Sets.null trusted) (Sets.null inScope)) [] (
                          let allVars = Sets.unions (Lists.map (\t -> collectTypeVars t) typeApps)
                          in (Logic.ifElse (Logic.not (Sets.null (Sets.difference allVars inScope))) [] (Logic.ifElse (Sets.null (Sets.difference allVars trusted)) typeApps [])))
            in (Eithers.bind (Logic.ifElse (Lists.null filteredTypeApps) (Right []) (correctTypeApps g v0 hargs filteredTypeApps cx g)) (\safeTypeApps -> Eithers.bind (filterPhantomTypeArgs v0 safeTypeApps cx g) (\finalTypeApps -> Eithers.bind (functionCall env False v0 hargs finalTypeApps cx g) (\initialCall -> Eithers.foldl (\acc -> \h -> Eithers.bind (encodeTerm env h cx g) (\jarg -> Right (applyJavaArg acc jarg))) initialCall rargs)))))))
          _ -> encodeApplication_fallback env aliases g typeApps (Core.applicationFunction app) (Core.applicationArgument app) cx g)))))

encodeApplication_fallback :: JavaEnvironment.JavaEnvironment -> JavaEnvironment.Aliases -> Graph.Graph -> [Core.Type] -> Core.Term -> Core.Term -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.Expression
encodeApplication_fallback env aliases gr typeApps lhs rhs cx g =
    Eithers.bind (Eithers.bimap (\_de -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _de))) (\_a -> _a) (Annotations.getType g (Annotations.termAnnotationInternal lhs))) (\mt -> Eithers.bind (Maybes.cases mt (Checking.typeOfTerm cx g lhs) (\typ -> Right typ)) (\t -> case (Strip.deannotateTypeParameters (Strip.deannotateType t)) of
      Core.TypeFunction v0 ->
        let dom = Core.functionTypeDomain v0
            cod = Core.functionTypeCodomain v0
            defaultExpr =
                    Eithers.bind (encodeTerm env lhs cx g) (\jfun -> Eithers.bind (encodeTerm env rhs cx g) (\jarg -> Right (applyJavaArg jfun jarg)))
            elimBranch =
                    Eithers.bind (encodeTerm env rhs cx g) (\jarg -> Eithers.bind (Logic.ifElse (Logic.not (Lists.null (javaTypeArgumentsForType dom))) (Right dom) (Eithers.bind (Eithers.bimap (\_de -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _de))) (\_a -> _a) (Annotations.getType g (Annotations.termAnnotationInternal rhs))) (\mrt -> Maybes.cases mrt (Eithers.bind (Checking.typeOfTerm cx g rhs) (\rt -> Right (Logic.ifElse (Logic.not (Lists.null (javaTypeArgumentsForType rt))) rt dom))) (\rt -> Right (Logic.ifElse (Logic.not (Lists.null (javaTypeArgumentsForType rt))) rt dom))))) (\enrichedDom -> encodeElimination env (Just jarg) enrichedDom cod (Strip.deannotateTerm lhs) cx g))
        in case (Strip.deannotateTerm lhs) of
          Core.TermProject _ -> elimBranch
          Core.TermCases _ -> elimBranch
          Core.TermUnwrap _ -> elimBranch
          _ -> defaultExpr
      _ -> Eithers.bind (encodeTerm env lhs cx g) (\jfun -> Eithers.bind (encodeTerm env rhs cx g) (\jarg -> Right (applyJavaArg jfun jarg)))))

encodeDefinitions :: Packaging.Module -> [Packaging.Definition] -> Context.Context -> Graph.Graph -> Either Errors.Error (M.Map Core.Name Syntax.CompilationUnit)
encodeDefinitions mod defs cx g =

      let aliases = Utils.importAliasesForModule mod
          env =
                  JavaEnvironment.JavaEnvironment {
                    JavaEnvironment.javaEnvironmentAliases = aliases,
                    JavaEnvironment.javaEnvironmentGraph = g}
          pkg = Utils.javaPackageDeclaration (Packaging.moduleNamespace mod)
          partitioned = Environment.partitionDefinitions defs
          typeDefs = Pairs.first partitioned
          termDefs = Pairs.second partitioned
          nonTypedefDefs =
                  Lists.filter (\td ->
                    let typ = Core.typeSchemeType (Packaging.typeDefinitionType td)
                    in (isSerializableJavaType typ)) typeDefs
      in (Eithers.bind (Eithers.mapList (\td -> encodeTypeDefinition pkg aliases td cx g) nonTypedefDefs) (\typeUnits -> Eithers.bind (Logic.ifElse (Lists.null termDefs) (Right []) (Eithers.bind (Eithers.mapList (\td -> encodeTermDefinition env td cx g) termDefs) (\dataMembers -> Right [
        constructElementsInterface mod dataMembers]))) (\termUnits -> Right (Maps.fromList (Lists.concat2 typeUnits termUnits)))))

encodeElimination :: JavaEnvironment.JavaEnvironment -> Maybe Syntax.Expression -> Core.Type -> Core.Type -> Core.Term -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.Expression
encodeElimination env marg dom cod elimTerm cx g =

      let aliases = JavaEnvironment.javaEnvironmentAliases env
      in case (Strip.deannotateTerm elimTerm) of
        Core.TermProject v0 ->
          let fname = Core.projectionField v0
          in (Eithers.bind (encodeType aliases Sets.empty dom cx g) (\jdom0 -> Eithers.bind (Utils.javaTypeToJavaReferenceType jdom0 cx) (\jdomr -> Maybes.cases marg (
            let projVar = Core.Name "projected"
                jbody =
                        Utils.javaExpressionNameToJavaExpression (Utils.fieldExpression (Utils.variableToJavaIdentifier projVar) (Utils.javaIdentifier (Core.unName fname)))
            in (Right (Utils.javaLambda projVar jbody))) (\jarg ->
            let qual = Syntax.FieldAccess_QualifierPrimary (Utils.javaExpressionToJavaPrimary jarg)
            in (Right (Utils.javaFieldAccessToJavaExpression (Syntax.FieldAccess {
              Syntax.fieldAccessQualifier = qual,
              Syntax.fieldAccessIdentifier = (Utils.javaIdentifier (Core.unName fname))})))))))
        Core.TermCases v0 ->
          let tname = Core.caseStatementTypeName v0
              def_ = Core.caseStatementDefault v0
              fields = Core.caseStatementCases v0
          in (Maybes.cases marg (
            let uVar = Core.Name "u"
                typedLambda =
                        Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = uVar,
                          Core.lambdaDomain = (Just dom),
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = elimTerm,
                            Core.applicationArgument = (Core.TermVariable uVar)}))})
            in (encodeTerm env typedLambda cx g)) (\jarg ->
            let prim = Utils.javaExpressionToJavaPrimary jarg
                consId = innerClassRef aliases tname JavaNames.partialVisitorName
                effectiveCod = cod
            in (Eithers.bind (encodeType aliases Sets.empty effectiveCod cx g) (\jcod -> Eithers.bind (Utils.javaTypeToJavaReferenceType jcod cx) (\rt -> Eithers.bind (domTypeArgs aliases dom cx g) (\domArgs ->
              let targs = typeArgsOrDiamond (Lists.concat2 domArgs [
                    Syntax.TypeArgumentReference rt])
              in (Eithers.bind (Maybes.cases def_ (Right []) (\d -> Eithers.bind (otherwiseBranch env aliases dom cod tname jcod domArgs d cx g) (\b -> Right [
                b]))) (\otherwiseBranches -> Eithers.bind (Eithers.mapList (\f -> visitBranch env aliases dom tname jcod domArgs f cx g) fields) (\visitBranches ->
                let body = Syntax.ClassBody (Lists.concat2 otherwiseBranches visitBranches)
                    visitor = Utils.javaConstructorCall (Utils.javaConstructorName consId (Just targs)) [] (Just body)
                in (Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocation (Just (Right prim)) (Syntax.Identifier JavaNames.acceptMethodName) [
                  visitor]))))))))))))
        Core.TermUnwrap _ ->
          let withArg =
                  \ja -> Utils.javaFieldAccessToJavaExpression (Syntax.FieldAccess {
                    Syntax.fieldAccessQualifier = (Syntax.FieldAccess_QualifierPrimary (Utils.javaExpressionToJavaPrimary ja)),
                    Syntax.fieldAccessIdentifier = (Utils.javaIdentifier JavaNames.valueFieldName)})
          in (Right (Maybes.cases marg (
            let wVar = Core.Name "wrapped"
                wArg = Utils.javaIdentifierToJavaExpression (Utils.variableToJavaIdentifier wVar)
            in (Utils.javaLambda wVar (withArg wArg))) (\jarg -> withArg jarg)))
        _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "unexpected " (Strings.cat2 "elimination case" (Strings.cat2 " in " "encodeElimination")))))

encodeFunction :: JavaEnvironment.JavaEnvironment -> Core.Type -> Core.Type -> Core.Term -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.Expression
encodeFunction env dom cod funTerm cx g =

      let aliases = JavaEnvironment.javaEnvironmentAliases env
          encodeLambdaFallback =
                  \env2 -> \lam ->
                    let lambdaVar = Core.lambdaParameter lam
                        body = Core.lambdaBody lam
                    in (Eithers.bind (analyzeJavaFunction env2 body cx g) (\fs ->
                      let bindings = Typing.functionStructureBindings fs
                          innerBody = Typing.functionStructureBody fs
                          env3 = Typing.functionStructureEnvironment fs
                      in (Eithers.bind (bindingsToStatements env3 bindings cx g) (\bindResult ->
                        let bindingStmts = Pairs.first bindResult
                            env4 = Pairs.second bindResult
                        in (Eithers.bind (encodeTerm env4 innerBody cx g) (\jbody ->
                          let lam1 =
                                  Logic.ifElse (Lists.null bindings) (Utils.javaLambda lambdaVar jbody) (
                                    let returnSt = Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just jbody))
                                    in (Utils.javaLambdaFromBlock lambdaVar (Syntax.Block (Lists.concat2 bindingStmts [
                                      returnSt]))))
                          in (applyCastIfSafe aliases (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = dom,
                            Core.functionTypeCodomain = cod})) lam1 cx g)))))))
      in case (Strip.deannotateTerm funTerm) of
        Core.TermProject _ -> encodeElimination env Nothing dom cod (Strip.deannotateTerm funTerm) cx g
        Core.TermCases _ -> encodeElimination env Nothing dom cod (Strip.deannotateTerm funTerm) cx g
        Core.TermUnwrap _ -> encodeElimination env Nothing dom cod (Strip.deannotateTerm funTerm) cx g
        Core.TermLambda v0 -> withLambda env v0 (\env2 ->
          let lambdaVar = Core.lambdaParameter v0
              body = Core.lambdaBody v0
          in case (Strip.deannotateTerm body) of
            Core.TermLambda v1 -> case (Strip.deannotateType cod) of
              Core.TypeFunction v2 ->
                let dom2 = Core.functionTypeDomain v2
                    cod2 = Core.functionTypeCodomain v2
                in (Eithers.bind (encodeFunction env2 dom2 cod2 (Core.TermLambda v1) cx g) (\innerJavaLambda ->
                  let lam1 = Utils.javaLambda lambdaVar innerJavaLambda
                  in (applyCastIfSafe aliases (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = dom,
                    Core.functionTypeCodomain = cod})) lam1 cx g)))
              _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "expected function type for lambda body, but got: " (ShowCore.type_ cod))))
            _ -> encodeLambdaFallback env2 v0)
        _ -> Right (encodeLiteral (Core.LiteralString (Strings.cat2 "Unimplemented function variant: " (ShowCore.term funTerm))))

encodeFunctionFormTerm :: JavaEnvironment.JavaEnvironment -> [M.Map Core.Name Core.Term] -> Core.Term -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.Expression
encodeFunctionFormTerm env anns term cx g =

      let combinedAnns = Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns
      in (Eithers.bind (Eithers.bimap (\_de -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _de))) (\_a -> _a) (Annotations.getType g combinedAnns)) (\mt -> Eithers.bind (Maybes.cases mt (Maybes.cases (tryInferFunctionType term) (Checking.typeOfTerm cx g term) (\inferredType -> Right inferredType)) (\t -> Right t)) (\typ -> case (Strip.deannotateType typ) of
        Core.TypeFunction v0 -> encodeFunction env (Core.functionTypeDomain v0) (Core.functionTypeCodomain v0) term cx g
        _ -> encodeNullaryConstant env typ term cx g)))

encodeFunctionPrimitiveByName :: JavaEnvironment.JavaEnvironment -> Core.Type -> Core.Type -> Core.Name -> t0 -> Graph.Graph -> Either Errors.Error Syntax.Expression
encodeFunctionPrimitiveByName env dom cod name cx g =

      let aliases = JavaEnvironment.javaEnvironmentAliases env
          classWithApply = Syntax.unIdentifier (elementJavaIdentifier True False aliases name)
          suffix = Strings.cat2 "." JavaNames.applyMethodName
          className =
                  Strings.fromList (Lists.take (Math.sub (Strings.length classWithApply) (Strings.length suffix)) (Strings.toList classWithApply))
          arity =
                  Arity.typeArity (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = dom,
                    Core.functionTypeCodomain = cod}))
      in (Logic.ifElse (Equality.lte arity 1) (Right (Utils.javaIdentifierToJavaExpression (Syntax.Identifier (Strings.cat [
        className,
        "::",
        JavaNames.applyMethodName])))) (
        let paramNames = Lists.map (\i -> Core.Name (Strings.cat2 "p" (Literals.showInt32 i))) (Math.range 0 (Math.sub arity 1))
            paramExprs = Lists.map (\p -> Utils.javaIdentifierToJavaExpression (Utils.variableToJavaIdentifier p)) paramNames
            classId = Syntax.Identifier className
            call =
                    Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStatic classId (Syntax.Identifier JavaNames.applyMethodName) paramExprs)
            curried = buildCurriedLambda paramNames call
        in (Eithers.bind (encodeType aliases Sets.empty (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = dom,
          Core.functionTypeCodomain = cod})) cx g) (\jtype -> Eithers.bind (Utils.javaTypeToJavaReferenceType jtype cx) (\rt -> Right (Utils.javaCastExpressionToJavaExpression (Utils.javaCastExpression rt (Utils.javaExpressionToJavaUnaryExpression curried))))))))

encodeLiteral :: Core.Literal -> Syntax.Expression
encodeLiteral lit =
    case lit of
      Core.LiteralBinary v0 ->
        let byteValues = Literals.binaryToBytes v0
        in (Utils.javaArrayCreation Utils.javaBytePrimitiveType (Just (Utils.javaArrayInitializer (Lists.map (\w -> Utils.javaLiteralToJavaExpression (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.int32ToBigint w)))) byteValues))))
      Core.LiteralBoolean v0 -> encodeLiteral_litExp (Utils.javaBoolean v0)
      Core.LiteralDecimal v0 -> Utils.javaConstructorCall (Utils.javaConstructorName (Syntax.Identifier "java.math.BigDecimal") Nothing) [
        encodeLiteral (Core.LiteralString (Literals.showDecimal v0))] Nothing
      Core.LiteralFloat v0 -> encodeLiteral_encodeFloat v0
      Core.LiteralInteger v0 -> encodeLiteral_encodeInteger v0
      Core.LiteralString v0 -> encodeLiteral_litExp (Utils.javaString v0)

encodeLiteralType :: Core.LiteralType -> t0 -> t1 -> Either t2 Syntax.Type
encodeLiteralType lt cx g =
    case lt of
      Core.LiteralTypeBinary -> Right (Syntax.TypeReference (Syntax.ReferenceTypeArray (Syntax.ArrayType {
        Syntax.arrayTypeDims = (Syntax.Dims [
          []]),
        Syntax.arrayTypeVariant = (Syntax.ArrayType_VariantPrimitive (Syntax.PrimitiveTypeWithAnnotations {
          Syntax.primitiveTypeWithAnnotationsType = (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeByte)),
          Syntax.primitiveTypeWithAnnotationsAnnotations = []}))})))
      Core.LiteralTypeBoolean -> encodeLiteralType_simple "Boolean" cx g
      Core.LiteralTypeDecimal -> Right (Utils.javaRefType [] (Just (JavaNames.javaPackageName [
        "java",
        "math"])) "BigDecimal")
      Core.LiteralTypeFloat v0 -> case v0 of
        Core.FloatTypeBigfloat -> Right (Utils.javaRefType [] (Just (JavaNames.javaPackageName [
          "java",
          "math"])) "BigDecimal")
        Core.FloatTypeFloat32 -> encodeLiteralType_simple "Float" cx g
        Core.FloatTypeFloat64 -> encodeLiteralType_simple "Double" cx g
      Core.LiteralTypeInteger v0 -> case v0 of
        Core.IntegerTypeBigint -> Right (Utils.javaRefType [] (Just (JavaNames.javaPackageName [
          "java",
          "math"])) "BigInteger")
        Core.IntegerTypeInt8 -> encodeLiteralType_simple "Byte" cx g
        Core.IntegerTypeInt16 -> encodeLiteralType_simple "Short" cx g
        Core.IntegerTypeInt32 -> encodeLiteralType_simple "Integer" cx g
        Core.IntegerTypeInt64 -> encodeLiteralType_simple "Long" cx g
        Core.IntegerTypeUint8 -> encodeLiteralType_simple "Short" cx g
        Core.IntegerTypeUint16 -> encodeLiteralType_simple "Character" cx g
        Core.IntegerTypeUint32 -> encodeLiteralType_simple "Long" cx g
        Core.IntegerTypeUint64 -> Right (Utils.javaRefType [] (Just (JavaNames.javaPackageName [
          "java",
          "math"])) "BigInteger")
      Core.LiteralTypeString -> encodeLiteralType_simple "String" cx g

encodeLiteralType_simple :: String -> t0 -> t1 -> Either t2 Syntax.Type
encodeLiteralType_simple n cx g = Right (Utils.javaRefType [] Nothing n)

encodeLiteral_encodeFloat :: Core.FloatValue -> Syntax.Expression
encodeLiteral_encodeFloat f =
    case f of
      Core.FloatValueBigfloat v0 -> Utils.javaConstructorCall (Utils.javaConstructorName (Syntax.Identifier "java.math.BigDecimal") Nothing) [
        encodeLiteral (Core.LiteralString (Literals.showBigfloat v0))] Nothing
      Core.FloatValueFloat32 v0 -> encodeLiteral_encodeFloat32 v0
      Core.FloatValueFloat64 v0 -> encodeLiteral_encodeFloat64 v0

encodeLiteral_encodeFloat32 :: Float -> Syntax.Expression
encodeLiteral_encodeFloat32 v =

      let s = Literals.showFloat32 v
      in (Logic.ifElse (Equality.equal s "NaN") (encodeLiteral_javaSpecialFloatExpr "Float" "NaN") (Logic.ifElse (Equality.equal s "Infinity") (encodeLiteral_javaSpecialFloatExpr "Float" "POSITIVE_INFINITY") (Logic.ifElse (Equality.equal s "-Infinity") (encodeLiteral_javaSpecialFloatExpr "Float" "NEGATIVE_INFINITY") (encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeFloatingPoint Syntax.FloatingPointTypeFloat)) (encodeLiteral_litExp (Syntax.LiteralFloatingPoint (Syntax.FloatingPointLiteral (Literals.float32ToBigfloat v))))))))

encodeLiteral_encodeFloat64 :: Double -> Syntax.Expression
encodeLiteral_encodeFloat64 v =

      let s = Literals.showFloat64 v
      in (Logic.ifElse (Equality.equal s "NaN") (encodeLiteral_javaSpecialFloatExpr "Double" "NaN") (Logic.ifElse (Equality.equal s "Infinity") (encodeLiteral_javaSpecialFloatExpr "Double" "POSITIVE_INFINITY") (Logic.ifElse (Equality.equal s "-Infinity") (encodeLiteral_javaSpecialFloatExpr "Double" "NEGATIVE_INFINITY") (Logic.ifElse (Equality.equal s "-0.0") (encodeLiteral_javaParseDouble "-0.0") (encodeLiteral_litExp (Syntax.LiteralFloatingPoint (Syntax.FloatingPointLiteral (Literals.float64ToBigfloat v))))))))

encodeLiteral_encodeInteger :: Core.IntegerValue -> Syntax.Expression
encodeLiteral_encodeInteger i =
    case i of
      Core.IntegerValueBigint v0 -> Utils.javaConstructorCall (Utils.javaConstructorName (Syntax.Identifier "java.math.BigInteger") Nothing) [
        encodeLiteral (Core.LiteralString (Literals.showBigint v0))] Nothing
      Core.IntegerValueInt8 v0 -> encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeByte)) (encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.int8ToBigint v0))))
      Core.IntegerValueInt16 v0 -> encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeShort)) (encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.int16ToBigint v0))))
      Core.IntegerValueInt32 v0 -> encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.int32ToBigint v0)))
      Core.IntegerValueInt64 v0 -> encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeLong)) (encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.int64ToBigint v0))))
      Core.IntegerValueUint8 v0 -> encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeShort)) (encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.uint8ToBigint v0))))
      Core.IntegerValueUint16 v0 -> encodeLiteral_litExp (Syntax.LiteralCharacter v0)
      Core.IntegerValueUint32 v0 -> encodeLiteral_primCast (Syntax.PrimitiveTypeNumeric (Syntax.NumericTypeIntegral Syntax.IntegralTypeLong)) (encodeLiteral_litExp (Syntax.LiteralInteger (Syntax.IntegerLiteral (Literals.uint32ToBigint v0))))
      Core.IntegerValueUint64 v0 -> Utils.javaConstructorCall (Utils.javaConstructorName (Syntax.Identifier "java.math.BigInteger") Nothing) [
        encodeLiteral (Core.LiteralString (Literals.showBigint (Literals.uint64ToBigint v0)))] Nothing

encodeLiteral_javaParseDouble :: String -> Syntax.Expression
encodeLiteral_javaParseDouble value =
    Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStatic (Syntax.Identifier "Double") (Syntax.Identifier "parseDouble") [
      encodeLiteral (Core.LiteralString value)])

encodeLiteral_javaSpecialFloatExpr :: String -> String -> Syntax.Expression
encodeLiteral_javaSpecialFloatExpr className fieldName =
    Utils.javaExpressionNameToJavaExpression (Syntax.ExpressionName {
      Syntax.expressionNameQualifier = (Just (Syntax.AmbiguousName [
        Syntax.Identifier className])),
      Syntax.expressionNameIdentifier = (Syntax.Identifier fieldName)})

encodeLiteral_litExp :: Syntax.Literal -> Syntax.Expression
encodeLiteral_litExp l = Utils.javaLiteralToJavaExpression l

encodeLiteral_primCast :: Syntax.PrimitiveType -> Syntax.Expression -> Syntax.Expression
encodeLiteral_primCast pt expr =
    Utils.javaCastExpressionToJavaExpression (Utils.javaCastPrimitive pt (Utils.javaExpressionToJavaUnaryExpression expr))

encodeNullaryConstant :: t0 -> t1 -> Core.Term -> t2 -> t3 -> Either Errors.Error t4
encodeNullaryConstant env typ funTerm cx g =
    Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "unexpected " (Strings.cat2 "nullary function" (Strings.cat2 " in " (ShowCore.term funTerm))))))

encodeNullaryConstant_typeArgsFromReturnType :: JavaEnvironment.Aliases -> Core.Type -> t0 -> Graph.Graph -> Either Errors.Error [Syntax.TypeArgument]
encodeNullaryConstant_typeArgsFromReturnType aliases t cx g =
    case (Strip.deannotateType t) of
      Core.TypeSet v0 -> Eithers.bind (encodeType aliases Sets.empty v0 cx g) (\jst -> Eithers.bind (Utils.javaTypeToJavaReferenceType jst cx) (\rt -> Right [
        Syntax.TypeArgumentReference rt]))
      Core.TypeList v0 -> Eithers.bind (encodeType aliases Sets.empty v0 cx g) (\jlt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jlt cx) (\rt -> Right [
        Syntax.TypeArgumentReference rt]))
      Core.TypeMaybe v0 -> Eithers.bind (encodeType aliases Sets.empty v0 cx g) (\jmt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jmt cx) (\rt -> Right [
        Syntax.TypeArgumentReference rt]))
      Core.TypeMap v0 -> Eithers.bind (encodeType aliases Sets.empty (Core.mapTypeKeys v0) cx g) (\jkt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jkt cx) (\rk -> Eithers.bind (encodeType aliases Sets.empty (Core.mapTypeValues v0) cx g) (\jvt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jvt cx) (\rv -> Right [
        Syntax.TypeArgumentReference rk,
        (Syntax.TypeArgumentReference rv)]))))
      _ -> Right []

encodeNullaryPrimitiveByName :: JavaEnvironment.JavaEnvironment -> Core.Type -> Core.Name -> t0 -> Graph.Graph -> Either Errors.Error Syntax.Expression
encodeNullaryPrimitiveByName env typ name cx g =

      let aliases = JavaEnvironment.javaEnvironmentAliases env
      in (Eithers.bind (encodeNullaryConstant_typeArgsFromReturnType aliases typ cx g) (\targs -> Logic.ifElse (Lists.null targs) (
        let header = Syntax.MethodInvocation_HeaderSimple (Syntax.MethodName (elementJavaIdentifier True False aliases name))
        in (Right (Utils.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
          Syntax.methodInvocationHeader = header,
          Syntax.methodInvocationArguments = []})))) (
        let fullName = Syntax.unIdentifier (elementJavaIdentifier True False aliases name)
            parts = Strings.splitOn "." fullName
            className = Syntax.Identifier (Strings.intercalate "." (Maybes.fromMaybe [] (Lists.maybeInit parts)))
            methodName = Syntax.Identifier (Maybes.fromMaybe fullName (Lists.maybeLast parts))
        in (Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs className methodName targs []))))))

encodeTerm :: JavaEnvironment.JavaEnvironment -> Core.Term -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.Expression
encodeTerm env term cx g = encodeTermInternal env [] [] term cx g

encodeTermDefinition :: JavaEnvironment.JavaEnvironment -> Packaging.TermDefinition -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.InterfaceMemberDeclaration
encodeTermDefinition env tdef cx g =

      let name = Packaging.termDefinitionName tdef
          term0 = Packaging.termDefinitionTerm tdef
          ts =
                  Maybes.maybe (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Unit")),
                    Core.typeSchemeConstraints = Nothing}) (\x -> x) (Packaging.termDefinitionType tdef)
          term = Variables.unshadowVariables term0
      in (Eithers.bind (analyzeJavaFunction env term cx g) (\fs ->
        let schemeVars = Lists.filter (\v -> isSimpleName v) (Core.typeSchemeVariables ts)
            termVars = Typing.functionStructureTypeParams fs
            schemeTypeVars = collectTypeVars (Core.typeSchemeType ts)
            usedSchemeVars = Lists.filter (\v -> Sets.member v schemeTypeVars) schemeVars
            tparams = Logic.ifElse (Lists.null usedSchemeVars) termVars usedSchemeVars
            params = Typing.functionStructureParams fs
            bindings = Typing.functionStructureBindings fs
            body = Typing.functionStructureBody fs
            doms = Typing.functionStructureDomains fs
            env2 = Typing.functionStructureEnvironment fs
            schemeType = Core.typeSchemeType ts
            numParams = Lists.length params
            peelResult = peelDomainsAndCod numParams schemeType
            schemeDoms = Pairs.first peelResult
            cod = Pairs.second peelResult
            schemeVarSet = Sets.fromList tparams
        in (Eithers.bind (Logic.ifElse (Lists.null tparams) (Right Maps.empty) (buildSubstFromAnnotations schemeVarSet term cx g)) (\typeVarSubst ->
          let overgenSubst = detectAccumulatorUnification schemeDoms cod tparams
              overgenVarSubst =
                      Maps.fromList (Maybes.cat (Lists.map (\entry ->
                        let k = Pairs.first entry
                            v = Pairs.second entry
                        in case v of
                          Core.TypeVariable v0 -> Just (k, v0)
                          _ -> Nothing) (Maps.toList overgenSubst)))
              fixedCod = Logic.ifElse (Maps.null overgenSubst) cod (substituteTypeVarsWithTypes overgenSubst cod)
              fixedDoms =
                      Logic.ifElse (Maps.null overgenSubst) schemeDoms (Lists.map (\d -> substituteTypeVarsWithTypes overgenSubst d) schemeDoms)
              fixedTparams =
                      Logic.ifElse (Maps.null overgenSubst) tparams (Lists.filter (\v -> Logic.not (Maps.member v overgenSubst)) tparams)
              constraints = Maybes.fromMaybe Maps.empty (Core.typeSchemeConstraints ts)
              jparams = Lists.map (\v -> Utils.javaTypeParameter (Formatting.capitalize (Core.unName v))) fixedTparams
              aliases2base = JavaEnvironment.javaEnvironmentAliases env2
              trustedVars = Sets.unions (Lists.map (\d -> collectTypeVars d) (Lists.concat2 fixedDoms [
                    fixedCod]))
              fixedSchemeVarSet = Sets.fromList fixedTparams
              aliases2 =
                      JavaEnvironment.Aliases {
                        JavaEnvironment.aliasesCurrentNamespace = (JavaEnvironment.aliasesCurrentNamespace aliases2base),
                        JavaEnvironment.aliasesPackages = (JavaEnvironment.aliasesPackages aliases2base),
                        JavaEnvironment.aliasesBranchVars = (JavaEnvironment.aliasesBranchVars aliases2base),
                        JavaEnvironment.aliasesRecursiveVars = (JavaEnvironment.aliasesRecursiveVars aliases2base),
                        JavaEnvironment.aliasesInScopeTypeParams = fixedSchemeVarSet,
                        JavaEnvironment.aliasesPolymorphicLocals = (JavaEnvironment.aliasesPolymorphicLocals aliases2base),
                        JavaEnvironment.aliasesInScopeJavaVars = (JavaEnvironment.aliasesInScopeJavaVars aliases2base),
                        JavaEnvironment.aliasesVarRenames = (JavaEnvironment.aliasesVarRenames aliases2base),
                        JavaEnvironment.aliasesLambdaVars = (Sets.union (JavaEnvironment.aliasesLambdaVars aliases2base) (Sets.fromList params)),
                        JavaEnvironment.aliasesTypeVarSubst = (Maps.union overgenVarSubst typeVarSubst),
                        JavaEnvironment.aliasesTrustedTypeVars = (Sets.intersection trustedVars fixedSchemeVarSet),
                        JavaEnvironment.aliasesMethodCodomain = (Just fixedCod),
                        JavaEnvironment.aliasesThunkedVars = (JavaEnvironment.aliasesThunkedVars aliases2base)}
              env2WithTypeParams =
                      JavaEnvironment.JavaEnvironment {
                        JavaEnvironment.javaEnvironmentAliases = aliases2,
                        JavaEnvironment.javaEnvironmentGraph = (JavaEnvironment.javaEnvironmentGraph env2)}
          in (Eithers.bind (bindingsToStatements env2WithTypeParams bindings cx g) (\bindResult ->
            let bindingStmts = Pairs.first bindResult
                env3 = Pairs.second bindResult
            in (Eithers.bind (Logic.ifElse (Maps.null overgenSubst) (Right body) (applyOvergenSubstToTermAnnotations overgenSubst body cx g)) (\body_ ->
              let annotatedBody = propagateTypesInAppChain fixedCod fixedCod body_
              in (Eithers.bind (Eithers.mapList (\pair -> Eithers.bind (encodeType aliases2 Sets.empty (Pairs.first pair) cx g) (\jdom -> Right (Utils.javaTypeToJavaFormalParameter jdom (Pairs.second pair)))) (Lists.zip fixedDoms params)) (\jformalParams -> Eithers.bind (encodeType aliases2 Sets.empty fixedCod cx g) (\jcod ->
                let result = Utils.javaTypeToJavaResult jcod
                    mods = [
                          Syntax.InterfaceMethodModifierStatic]
                    jname = Utils.sanitizeJavaName (Formatting.decapitalize (Names.localNameOf name))
                    isTCO = False
                in (Eithers.bind (Logic.ifElse isTCO (
                  let tcoSuffix = "_tco"
                      snapshotNames = Lists.map (\p -> Core.Name (Strings.cat2 (Core.unName p) tcoSuffix)) params
                      tcoVarRenames = Maps.fromList (Lists.zip params snapshotNames)
                      snapshotDecls =
                              Lists.map (\pair -> Utils.finalVarDeclarationStatement (Utils.variableToJavaIdentifier (Pairs.second pair)) (Utils.javaIdentifierToJavaExpression (Utils.variableToJavaIdentifier (Pairs.first pair)))) (Lists.zip params snapshotNames)
                      tcoBody =
                              Logic.ifElse (Lists.null bindings) annotatedBody (Core.TermLet (Core.Let {
                                Core.letBindings = bindings,
                                Core.letBody = annotatedBody}))
                  in (Eithers.bind (encodeTermTCO env2WithTypeParams name params tcoVarRenames 0 tcoBody cx g) (\tcoStmts ->
                    let whileBodyStmts = Lists.concat2 snapshotDecls tcoStmts
                        whileBodyBlock =
                                Syntax.StatementWithoutTrailing (Syntax.StatementWithoutTrailingSubstatementBlock (Syntax.Block whileBodyStmts))
                        noCond = Nothing
                        whileStmt =
                                Syntax.BlockStatementStatement (Syntax.StatementWhile (Syntax.WhileStatement {
                                  Syntax.whileStatementCond = noCond,
                                  Syntax.whileStatementBody = whileBodyBlock}))
                    in (Right [
                      whileStmt])))) (Eithers.bind (encodeTerm env3 annotatedBody cx g) (\jbody ->
                  let returnSt = Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just jbody))
                  in (Right (Lists.concat2 bindingStmts [
                    returnSt]))))) (\methodBody -> Right (Utils.interfaceMethodDeclaration mods jparams jname jformalParams result (Just methodBody)))))))))))))))

encodeTermInternal :: JavaEnvironment.JavaEnvironment -> [M.Map Core.Name Core.Term] -> [Syntax.Type] -> Core.Term -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.Expression
encodeTermInternal env anns tyapps term cx g0 =

      let aliases = JavaEnvironment.javaEnvironmentAliases env
          g = JavaEnvironment.javaEnvironmentGraph env
          encode = \t -> encodeTerm env t cx g
      in case term of
        Core.TermAnnotated v0 -> encodeTermInternal env (Lists.cons (Core.annotatedTermAnnotation v0) anns) tyapps (Core.annotatedTermBody v0) cx g
        Core.TermApplication v0 -> encodeApplication env v0 cx g
        Core.TermEither v0 -> Eithers.bind (Logic.ifElse (Lists.null tyapps) (Right Nothing) (Eithers.bind (takeTypeArgs "either" 2 tyapps cx g) (\ta -> Right (Just ta)))) (\mtargs ->
          let combinedAnns = Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns
          in (Eithers.bind (Eithers.bimap (\_de -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _de))) (\_a -> _a) (Annotations.getType g combinedAnns)) (\mEitherType ->
            let branchTypes =
                    Maybes.bind mEitherType (\etyp -> case (Strip.deannotateType etyp) of
                      Core.TypeEither v1 -> Just (Core.eitherTypeLeft v1, (Core.eitherTypeRight v1))
                      _ -> Nothing)
                encodeWithType =
                        \branchType -> \t1 ->
                          let annotated = Annotations.setTermAnnotation Constants.key_type (Just (EncodeCore.type_ branchType)) t1
                          in (encodeTermInternal env anns [] annotated cx g)
                eitherCall =
                        \methodName -> \expr -> Maybes.cases mtargs (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStatic (Syntax.Identifier "hydra.util.Either") (Syntax.Identifier methodName) [
                          expr])) (\targs -> Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs (Syntax.Identifier "hydra.util.Either") (Syntax.Identifier methodName) targs [
                          expr]))
            in (Eithers.either (\term1 -> Eithers.bind (Maybes.cases branchTypes (encode term1) (\bt -> encodeWithType (Pairs.first bt) term1)) (\expr -> Right (eitherCall "left" expr))) (\term1 -> Eithers.bind (Maybes.cases branchTypes (encode term1) (\bt -> encodeWithType (Pairs.second bt) term1)) (\expr -> Right (eitherCall "right" expr))) v0))))
        Core.TermLambda _ -> encodeFunctionFormTerm env anns term cx g
        Core.TermProject _ -> encodeFunctionFormTerm env anns term cx g
        Core.TermCases _ -> encodeFunctionFormTerm env anns term cx g
        Core.TermUnwrap _ -> encodeFunctionFormTerm env anns term cx g
        Core.TermLet v0 ->
          let bindings = Core.letBindings v0
              body = Core.letBody v0
          in (Logic.ifElse (Lists.null bindings) (encodeTermInternal env anns [] body cx g) (Eithers.bind (bindingsToStatements env bindings cx g) (\bindResult ->
            let bindingStmts = Pairs.first bindResult
                env2 = Pairs.second bindResult
            in (Eithers.bind (encodeTermInternal env2 anns [] body cx g) (\jbody ->
              let returnSt = Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just jbody))
                  block = Syntax.Block (Lists.concat2 bindingStmts [
                        returnSt])
                  nullaryLambda =
                          Syntax.ExpressionLambda (Syntax.LambdaExpression {
                            Syntax.lambdaExpressionParameters = (Syntax.LambdaParametersTuple []),
                            Syntax.lambdaExpressionBody = (Syntax.LambdaBodyBlock block)})
                  combinedAnns = Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns
                  g2 = JavaEnvironment.javaEnvironmentGraph env2
                  aliases2 = JavaEnvironment.javaEnvironmentAliases env2
              in (Eithers.bind (Eithers.bimap (\_de -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _de))) (\_a -> _a) (Annotations.getType g combinedAnns)) (\mt -> Eithers.bind (Maybes.cases mt (Checking.typeOfTerm cx g2 body) (\t -> Right t)) (\letType -> Eithers.bind (encodeType aliases2 Sets.empty letType cx g) (\jLetType -> Eithers.bind (Utils.javaTypeToJavaReferenceType jLetType cx) (\rt ->
                let supplierRt =
                        Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass (Utils.javaClassType [
                          rt] JavaNames.javaUtilFunctionPackageName "Supplier"))
                    castExpr =
                            Utils.javaCastExpressionToJavaExpression (Utils.javaCastExpression supplierRt (Utils.javaExpressionToJavaUnaryExpression nullaryLambda))
                in (Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocation (Just (Right (Utils.javaExpressionToJavaPrimary castExpr))) (Syntax.Identifier "get") [])))))))))))))
        Core.TermList v0 -> Logic.ifElse (Lists.null v0) (Logic.ifElse (Lists.null tyapps) (Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStatic (Syntax.Identifier "java.util.Collections") (Syntax.Identifier "emptyList") []))) (Eithers.bind (takeTypeArgs "list" 1 tyapps cx g) (\targs -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs (Syntax.Identifier "java.util.Collections") (Syntax.Identifier "emptyList") targs []))))) (Eithers.bind (Eithers.mapList encode v0) (\jels -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStatic (Syntax.Identifier "java.util.Arrays") (Syntax.Identifier "asList") jels))))
        Core.TermLiteral v0 -> Right (encodeLiteral v0)
        Core.TermMap v0 -> Logic.ifElse (Maps.null v0) (Logic.ifElse (Lists.null tyapps) (Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStatic (Syntax.Identifier "java.util.Collections") (Syntax.Identifier "emptyMap") []))) (Eithers.bind (takeTypeArgs "map" 2 tyapps cx g) (\targs -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs (Syntax.Identifier "java.util.Collections") (Syntax.Identifier "emptyMap") targs []))))) (Eithers.bind (Eithers.mapList encode (Maps.keys v0)) (\jkeys -> Eithers.bind (Eithers.mapList encode (Maps.elems v0)) (\jvals ->
          let pairExprs =
                  Lists.map (\kv -> Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStatic (Syntax.Identifier "java.util.Map") (Syntax.Identifier "entry") [
                    Pairs.first kv,
                    (Pairs.second kv)])) (Lists.zip jkeys jvals)
              innerMap =
                      Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStatic (Syntax.Identifier "java.util.Map") (Syntax.Identifier "ofEntries") pairExprs)
          in (Right (Utils.javaConstructorCall (Utils.javaConstructorName (Syntax.Identifier "java.util.TreeMap") Nothing) [
            innerMap] Nothing)))))
        Core.TermMaybe v0 -> Maybes.cases v0 (Logic.ifElse (Lists.null tyapps) (Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStatic (Syntax.Identifier "hydra.util.Maybe") (Syntax.Identifier "nothing") []))) (Eithers.bind (takeTypeArgs "maybe" 1 tyapps cx g) (\targs -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs (Syntax.Identifier "hydra.util.Maybe") (Syntax.Identifier "nothing") targs []))))) (\term1 -> Eithers.bind (encode term1) (\expr -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStatic (Syntax.Identifier "hydra.util.Maybe") (Syntax.Identifier "just") [
          expr]))))
        Core.TermPair v0 -> Eithers.bind (encode (Pairs.first v0)) (\jterm1 -> Eithers.bind (encode (Pairs.second v0)) (\jterm2 -> Eithers.bind (Logic.ifElse (Lists.null tyapps) (Right Nothing) (Eithers.bind (Eithers.mapList (\jt -> Utils.javaTypeToJavaReferenceType jt cx) tyapps) (\rts -> Right (Just (Syntax.TypeArgumentsOrDiamondArguments (Lists.map (\rt -> Syntax.TypeArgumentReference rt) rts)))))) (\mtargs -> Right (Utils.javaConstructorCall (Utils.javaConstructorName (Syntax.Identifier "hydra.util.Pair") mtargs) [
          jterm1,
          jterm2] Nothing))))
        Core.TermRecord v0 ->
          let recName = Core.recordTypeName v0
              mRecordType = Eithers.either (\_ -> Nothing) (\t -> Just t) (Resolution.requireType cx g recName)
              strippedRecTyp = Maybes.map (\recTyp -> stripForalls (Strip.deannotateType recTyp)) mRecordType
              mFieldTypeMap =
                      Maybes.bind strippedRecTyp (\bodyTyp -> case bodyTyp of
                        Core.TypeRecord v1 -> Just (Maps.fromList (Lists.map (\ft -> (Core.fieldTypeName ft, (Core.fieldTypeType ft))) v1))
                        _ -> Nothing)
              combinedAnnsRec = Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns
          in (Eithers.bind (Eithers.bimap (\_de -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _de))) (\_a -> _a) (Annotations.getType g combinedAnnsRec)) (\mAnnotType ->
            let mTypeSubst =
                    Maybes.bind mAnnotType (\annTyp -> Maybes.bind mRecordType (\recTyp ->
                      let args = extractTypeApplicationArgs (Strip.deannotateType annTyp)
                          params = collectForallParams (Strip.deannotateType recTyp)
                      in (Logic.ifElse (Logic.or (Lists.null args) (Logic.not (Equality.equal (Lists.length args) (Lists.length params)))) Nothing (Just (Maps.fromList (Lists.zip params args))))))
                encodeField =
                        \fld -> Maybes.cases mFieldTypeMap (encode (Core.fieldTerm fld)) (\ftmap ->
                          let mftyp = Maps.lookup (Core.fieldName fld) ftmap
                          in (Maybes.cases mftyp (encode (Core.fieldTerm fld)) (\ftyp ->
                            let resolvedType = Maybes.cases mTypeSubst ftyp (\subst -> applySubstFull subst ftyp)
                                annotatedFieldTerm =
                                        Annotations.setTermAnnotation Constants.key_type (Just (EncodeCore.type_ resolvedType)) (Core.fieldTerm fld)
                            in (encodeTermInternal env anns [] annotatedFieldTerm cx g))))
            in (Eithers.bind (Eithers.mapList encodeField (Core.recordFields v0)) (\fieldExprs ->
              let consId = Utils.nameToJavaName aliases recName
              in (Eithers.bind (Logic.ifElse (Logic.not (Lists.null tyapps)) (Eithers.bind (Eithers.mapList (\jt -> Utils.javaTypeToJavaReferenceType jt cx) tyapps) (\rts -> Right (Just (Syntax.TypeArgumentsOrDiamondArguments (Lists.map (\rt -> Syntax.TypeArgumentReference rt) rts))))) (
                let combinedAnns = Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns
                in (Eithers.bind (Eithers.bimap (\_de -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _de))) (\_a -> _a) (Annotations.getType g combinedAnns)) (\mtyp -> Maybes.cases mtyp (Right Nothing) (\annTyp ->
                  let typeArgs = extractTypeApplicationArgs (Strip.deannotateType annTyp)
                  in (Logic.ifElse (Lists.null typeArgs) (Right Nothing) (Eithers.bind (Eithers.mapList (\t -> Eithers.bind (encodeType aliases Sets.empty t cx g) (\jt -> Utils.javaTypeToJavaReferenceType jt cx)) typeArgs) (\jTypeArgs -> Right (Just (Syntax.TypeArgumentsOrDiamondArguments (Lists.map (\rt -> Syntax.TypeArgumentReference rt) jTypeArgs))))))))))) (\mtargs -> Right (Utils.javaConstructorCall (Utils.javaConstructorName consId mtargs) fieldExprs Nothing)))))))
        Core.TermSet v0 -> Logic.ifElse (Sets.null v0) (Logic.ifElse (Lists.null tyapps) (Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStatic (Syntax.Identifier "java.util.Collections") (Syntax.Identifier "emptySet") []))) (Eithers.bind (takeTypeArgs "set" 1 tyapps cx g) (\targs -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs (Syntax.Identifier "java.util.Collections") (Syntax.Identifier "emptySet") targs []))))) (
          let slist = Sets.toList v0
          in (Eithers.bind (Eithers.mapList encode slist) (\jels ->
            let innerSet =
                    Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStatic (Syntax.Identifier "java.util.Set") (Syntax.Identifier "of") jels)
            in (Right (Utils.javaConstructorCall (Utils.javaConstructorName (Syntax.Identifier "java.util.TreeSet") Nothing) [
              innerSet] Nothing)))))
        Core.TermTypeLambda v0 -> withTypeLambda env v0 (\env2 ->
          let combinedAnns = Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns
          in (Eithers.bind (Eithers.bimap (\_de -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _de))) (\_a -> _a) (Annotations.getType g combinedAnns)) (\mtyp ->
            let annotatedBody =
                    Maybes.cases mtyp (Core.typeLambdaBody v0) (\t -> case t of
                      Core.TypeForall v1 -> Annotations.setTermAnnotation Constants.key_type (Just (EncodeCore.type_ (Core.forallTypeBody v1))) (Core.typeLambdaBody v0)
                      _ -> Core.typeLambdaBody v0)
            in (encodeTerm env2 annotatedBody cx g))))
        Core.TermInject v0 ->
          let injTypeName = Core.injectionTypeName v0
              injField = Core.injectionField v0
              injFieldName = Core.fieldName injField
              injFieldTerm = Core.fieldTerm injField
              typeId = Syntax.unIdentifier (Utils.nameToJavaName aliases injTypeName)
              consId =
                      Syntax.Identifier (Strings.cat [
                        typeId,
                        ".",
                        (Utils.sanitizeJavaName (Formatting.capitalize (Core.unName injFieldName)))])
          in (Eithers.bind (isFieldUnitType injTypeName injFieldName cx g) (\fieldIsUnit -> Eithers.bind (Logic.ifElse (Logic.or (Predicates.isUnitTerm (Strip.deannotateTerm injFieldTerm)) fieldIsUnit) (Right []) (Eithers.bind (encode injFieldTerm) (\ex -> Right [
            ex]))) (\args -> Right (Utils.javaConstructorCall (Utils.javaConstructorName consId Nothing) args Nothing))))
        Core.TermVariable v0 -> Maybes.cases (Maps.lookup v0 (Graph.graphPrimitives g)) (encodeVariable env v0 cx g) (\_prim ->
          let combinedAnns = Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns
          in (Eithers.bind (Eithers.bimap (\_de -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _de))) (\_a -> _a) (Annotations.getType g combinedAnns)) (\mt -> Eithers.bind (Maybes.cases mt (Checking.typeOfTerm cx g term) (\t -> Right t)) (\typ -> case (Strip.deannotateType typ) of
            Core.TypeFunction v1 -> encodeFunctionPrimitiveByName env (Core.functionTypeDomain v1) (Core.functionTypeCodomain v1) v0 cx g
            _ -> encodeNullaryPrimitiveByName env typ v0 cx g))))
        Core.TermUnit -> Right (Utils.javaLiteralToJavaExpression Syntax.LiteralNull)
        Core.TermWrap v0 -> Eithers.bind (encode (Core.wrappedTermBody v0)) (\jarg -> Right (Utils.javaConstructorCall (Utils.javaConstructorName (Utils.nameToJavaName aliases (Core.wrappedTermTypeName v0)) Nothing) [
          jarg] Nothing))
        Core.TermTypeApplication v0 ->
          let atyp = Core.typeApplicationTermType v0
              body = Core.typeApplicationTermBody v0
          in (Eithers.bind (encodeType aliases Sets.empty atyp cx g) (\jatyp ->
            let combinedAnns = Lists.foldl (\acc -> \m -> Maps.union acc m) Maps.empty anns
            in (Eithers.bind (Eithers.bimap (\_de -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _de))) (\_a -> _a) (Annotations.getType g combinedAnns)) (\mtyp -> Eithers.bind (Maybes.cases mtyp (Checking.typeOfTerm cx g term) (\t -> Right t)) (\typ ->
              let collected0 = collectTypeApps0 body [
                    atyp]
                  innermostBody0 = Pairs.first collected0
                  allTypeArgs0 = Pairs.second collected0
              in (Eithers.bind (correctCastType innermostBody0 allTypeArgs0 typ cx g) (\correctedTyp ->
                let collected = collectTypeApps body [
                      atyp]
                    innermostBody = Pairs.first collected
                    allTypeArgs = Pairs.second collected
                in case innermostBody of
                  Core.TermVariable v1 -> Eithers.bind (classifyDataReference v1 cx g) (\cls -> typeAppNullaryOrHoisted env aliases anns tyapps jatyp body correctedTyp v1 cls allTypeArgs cx g)
                  Core.TermEither v1 -> Logic.ifElse (Equality.equal (Lists.length allTypeArgs) 2) (
                    let eitherBranchTypes =
                            (Maybes.fromMaybe correctedTyp (Lists.maybeAt 0 allTypeArgs), (Maybes.fromMaybe correctedTyp (Lists.maybeAt 1 allTypeArgs)))
                    in (Eithers.bind (Eithers.mapList (\t -> Eithers.bind (encodeType aliases Sets.empty t cx g) (\jt -> Utils.javaTypeToJavaReferenceType jt cx)) allTypeArgs) (\jTypeArgs ->
                      let eitherTargs = Lists.map (\rt -> Syntax.TypeArgumentReference rt) jTypeArgs
                          encodeEitherBranch =
                                  \branchType -> \t1 ->
                                    let annotated = Annotations.setTermAnnotation Constants.key_type (Just (EncodeCore.type_ branchType)) t1
                                    in (encodeTermInternal env anns [] annotated cx g)
                      in (Eithers.either (\term1 -> Eithers.bind (encodeEitherBranch (Pairs.first eitherBranchTypes) term1) (\expr -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs (Syntax.Identifier "hydra.util.Either") (Syntax.Identifier "left") eitherTargs [
                        expr])))) (\term1 -> Eithers.bind (encodeEitherBranch (Pairs.second eitherBranchTypes) term1) (\expr -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs (Syntax.Identifier "hydra.util.Either") (Syntax.Identifier "right") eitherTargs [
                        expr])))) v1)))) (typeAppFallbackCast env aliases anns tyapps jatyp body correctedTyp cx g)
                  _ -> typeAppFallbackCast env aliases anns tyapps jatyp body correctedTyp cx g)))))))
        _ -> Right (encodeLiteral (Core.LiteralString "Unimplemented term variant"))

encodeTermTCO :: JavaEnvironment.JavaEnvironment -> Core.Name -> [Core.Name] -> M.Map Core.Name Core.Name -> Int -> Core.Term -> Context.Context -> Graph.Graph -> Either Errors.Error [Syntax.BlockStatement]
encodeTermTCO env0 funcName paramNames tcoVarRenames tcoDepth term cx g =

      let aliases0 = JavaEnvironment.javaEnvironmentAliases env0
          env =
                  JavaEnvironment.JavaEnvironment {
                    JavaEnvironment.javaEnvironmentAliases = JavaEnvironment.Aliases {
                      JavaEnvironment.aliasesCurrentNamespace = (JavaEnvironment.aliasesCurrentNamespace aliases0),
                      JavaEnvironment.aliasesPackages = (JavaEnvironment.aliasesPackages aliases0),
                      JavaEnvironment.aliasesBranchVars = (JavaEnvironment.aliasesBranchVars aliases0),
                      JavaEnvironment.aliasesRecursiveVars = (JavaEnvironment.aliasesRecursiveVars aliases0),
                      JavaEnvironment.aliasesInScopeTypeParams = (JavaEnvironment.aliasesInScopeTypeParams aliases0),
                      JavaEnvironment.aliasesPolymorphicLocals = (JavaEnvironment.aliasesPolymorphicLocals aliases0),
                      JavaEnvironment.aliasesInScopeJavaVars = (JavaEnvironment.aliasesInScopeJavaVars aliases0),
                      JavaEnvironment.aliasesVarRenames = (Maps.union tcoVarRenames (JavaEnvironment.aliasesVarRenames aliases0)),
                      JavaEnvironment.aliasesLambdaVars = (JavaEnvironment.aliasesLambdaVars aliases0),
                      JavaEnvironment.aliasesTypeVarSubst = (JavaEnvironment.aliasesTypeVarSubst aliases0),
                      JavaEnvironment.aliasesTrustedTypeVars = (JavaEnvironment.aliasesTrustedTypeVars aliases0),
                      JavaEnvironment.aliasesMethodCodomain = (JavaEnvironment.aliasesMethodCodomain aliases0),
                      JavaEnvironment.aliasesThunkedVars = (JavaEnvironment.aliasesThunkedVars aliases0)},
                    JavaEnvironment.javaEnvironmentGraph = (JavaEnvironment.javaEnvironmentGraph env0)}
          stripped = Strip.deannotateAndDetypeTerm term
          gathered = Analysis.gatherApplications stripped
          gatherArgs = Pairs.first gathered
          gatherFun = Pairs.second gathered
          strippedFun = Strip.deannotateAndDetypeTerm gatherFun
          isSelfCall =
                  case strippedFun of
                    Core.TermVariable v0 -> Equality.equal v0 funcName
                    _ -> False
      in (Logic.ifElse (Logic.and isSelfCall (Equality.equal (Lists.length gatherArgs) (Lists.length paramNames))) (
        let changePairs =
                Lists.filter (\pair -> Logic.not (case (Strip.deannotateAndDetypeTerm (Pairs.second pair)) of
                  Core.TermVariable v0 -> Equality.equal v0 (Pairs.first pair)
                  _ -> False)) (Lists.zip paramNames gatherArgs)
            changedParams = Lists.map Pairs.first changePairs
        in (Eithers.bind (Eithers.mapList (\pair -> encodeTerm env (Pairs.second pair) cx g) changePairs) (\jChangedArgs ->
          let assignments =
                  Lists.map (\pair ->
                    let paramName = Pairs.first pair
                        jArg = Pairs.second pair
                    in (Syntax.BlockStatementStatement (Utils.javaAssignmentStatement (Syntax.LeftHandSideExpressionName (Utils.javaIdentifierToJavaExpressionName (Utils.variableToJavaIdentifier paramName))) jArg))) (Lists.zip changedParams jChangedArgs)
              continueStmt =
                      Syntax.BlockStatementStatement (Syntax.StatementWithoutTrailing (Syntax.StatementWithoutTrailingSubstatementContinue (Syntax.ContinueStatement Nothing)))
          in (Right (Lists.concat2 assignments [
            continueStmt]))))) (case stripped of
        Core.TermLet v0 ->
          let letBindings = Core.letBindings v0
              letBody = Core.letBody v0
          in (Eithers.bind (bindingsToStatements env letBindings cx g) (\bindResult ->
            let letStmts = Pairs.first bindResult
                envAfterLet = Pairs.second bindResult
            in (Eithers.bind (encodeTermTCO envAfterLet funcName paramNames tcoVarRenames tcoDepth letBody cx g) (\tcoBodyStmts -> Right (Lists.concat2 letStmts tcoBodyStmts)))))
        _ ->
          let gathered2 = Analysis.gatherApplications term
              args2 = Pairs.first gathered2
              body2 = Pairs.second gathered2
          in (Logic.ifElse (Equality.equal (Lists.length args2) 1) (
            let arg = Maybes.fromMaybe Core.TermUnit (Lists.maybeHead args2)
            in case (Strip.deannotateAndDetypeTerm body2) of
              Core.TermCases v0 ->
                let aliases = JavaEnvironment.javaEnvironmentAliases env
                    tname = Core.caseStatementTypeName v0
                    dflt = Core.caseStatementDefault v0
                    cases_ = Core.caseStatementCases v0
                in (Eithers.bind (domTypeArgs aliases (Resolution.nominalApplication tname []) cx g) (\domArgs -> Eithers.bind (encodeTerm env arg cx g) (\jArgRaw ->
                  let depthSuffix = Logic.ifElse (Equality.equal tcoDepth 0) "" (Literals.showInt32 tcoDepth)
                      matchVarId =
                              Utils.javaIdentifier (Strings.cat [
                                "_tco_match_",
                                (Formatting.decapitalize (Names.localNameOf tname)),
                                depthSuffix])
                      matchDecl = Utils.varDeclarationStatement matchVarId jArgRaw
                      jArg = Utils.javaIdentifierToJavaExpression matchVarId
                  in (Eithers.bind (Eithers.mapList (\field ->
                    let fieldName = Core.fieldName field
                        variantRefType =
                                Utils.nameToJavaReferenceType aliases True domArgs tname (Just (Formatting.capitalize (Core.unName fieldName)))
                    in case (Strip.deannotateTerm (Core.fieldTerm field)) of
                      Core.TermLambda v1 -> withLambda env v1 (\env2 ->
                        let lambdaParam = Core.lambdaParameter v1
                            branchBody = Core.lambdaBody v1
                            env3 = insertBranchVar lambdaParam env2
                            varId = Utils.variableToJavaIdentifier lambdaParam
                            castExpr =
                                    Utils.javaCastExpressionToJavaExpression (Utils.javaCastExpression variantRefType (Utils.javaExpressionToJavaUnaryExpression jArg))
                            localDecl = Utils.varDeclarationStatement varId castExpr
                            isBranchTailCall = Analysis.isTailRecursiveInTailPosition funcName branchBody
                        in (Eithers.bind (Logic.ifElse isBranchTailCall (encodeTermTCO env3 funcName paramNames tcoVarRenames (Math.add tcoDepth 1) branchBody cx g) (Eithers.bind (analyzeJavaFunction env3 branchBody cx g) (\fs ->
                          let bindings = Typing.functionStructureBindings fs
                              innerBody = Typing.functionStructureBody fs
                              env4 = Typing.functionStructureEnvironment fs
                          in (Eithers.bind (bindingsToStatements env4 bindings cx g) (\bindResult ->
                            let bindingStmts = Pairs.first bindResult
                                env5 = Pairs.second bindResult
                            in (Eithers.bind (encodeTerm env5 innerBody cx g) (\jret ->
                              let returnStmt = Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just jret))
                              in (Right (Lists.concat2 bindingStmts [
                                returnStmt]))))))))) (\bodyStmts ->
                          let relExpr =
                                  Utils.javaInstanceOf (Utils.javaUnaryExpressionToJavaRelationalExpression (Utils.javaExpressionToJavaUnaryExpression jArg)) variantRefType
                              condExpr = Utils.javaRelationalExpressionToJavaExpression relExpr
                              blockStmts = Lists.cons localDecl bodyStmts
                              ifBody = Syntax.StatementWithoutTrailing (Syntax.StatementWithoutTrailingSubstatementBlock (Syntax.Block blockStmts))
                          in (Right (Syntax.BlockStatementStatement (Syntax.StatementIfThen (Syntax.IfThenStatement {
                            Syntax.ifThenStatementExpression = condExpr,
                            Syntax.ifThenStatementStatement = ifBody})))))))
                      _ -> Left (Errors.ErrorOther (Errors.OtherError "TCO: case branch is not a lambda"))) cases_) (\ifBlocks -> Eithers.bind (Maybes.cases dflt (Right [
                    Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just jArg))]) (\d -> Eithers.bind (encodeTerm env d cx g) (\dExpr -> Right [
                    Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just dExpr))]))) (\defaultStmt -> Right (Lists.concat [
                    [
                      matchDecl],
                    ifBlocks,
                    defaultStmt])))))))
              _ -> Eithers.bind (encodeTerm env term cx g) (\expr -> Right [
                Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just expr))])) (Eithers.bind (encodeTerm env term cx g) (\expr -> Right [
            Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just expr))])))))

encodeType :: JavaEnvironment.Aliases -> S.Set Core.Name -> Core.Type -> t0 -> Graph.Graph -> Either Errors.Error Syntax.Type
encodeType aliases boundVars t cx g =

      let inScopeTypeParams = JavaEnvironment.aliasesInScopeTypeParams aliases
          typeVarSubst = JavaEnvironment.aliasesTypeVarSubst aliases
      in case (Strip.deannotateType t) of
        Core.TypeApplication v0 -> Eithers.bind (encodeType aliases boundVars (Core.applicationTypeFunction v0) cx g) (\jlhs -> Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.applicationTypeArgument v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jrhs -> Utils.addJavaTypeParameter jrhs jlhs cx))
        Core.TypeFunction v0 -> Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.functionTypeDomain v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jdom -> Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.functionTypeCodomain v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jcod -> Right (Utils.javaRefType [
          jdom,
          jcod] JavaNames.javaUtilFunctionPackageName "Function")))
        Core.TypeForall v0 -> Eithers.bind (encodeType aliases (Sets.insert (Core.forallTypeParameter v0) boundVars) (Core.forallTypeBody v0) cx g) (\jbody -> Utils.addJavaTypeParameter (Utils.javaTypeVariable (Core.unName (Core.forallTypeParameter v0))) jbody cx)
        Core.TypeList v0 -> Eithers.bind (encodeType aliases boundVars v0 cx g) (\jet -> Eithers.bind (Eithers.bind (Right jet) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\rt -> Right (Utils.javaRefType [
          rt] JavaNames.javaUtilPackageName "List")))
        Core.TypeLiteral v0 -> encodeLiteralType v0 cx g
        Core.TypeEither v0 -> Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.eitherTypeLeft v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jlt -> Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.eitherTypeRight v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jrt -> Right (Utils.javaRefType [
          jlt,
          jrt] JavaNames.hydraUtilPackageName "Either")))
        Core.TypeMap v0 -> Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.mapTypeKeys v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jkt -> Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.mapTypeValues v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jvt -> Right (Utils.javaRefType [
          jkt,
          jvt] JavaNames.javaUtilPackageName "Map")))
        Core.TypePair v0 -> Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.pairTypeFirst v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jfirst -> Eithers.bind (Eithers.bind (encodeType aliases boundVars (Core.pairTypeSecond v0) cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jsecond -> Right (Utils.javaRefType [
          jfirst,
          jsecond] JavaNames.hydraUtilPackageName "Pair")))
        Core.TypeUnit -> Right (Utils.javaRefType [] JavaNames.javaLangPackageName "Void")
        Core.TypeRecord v0 -> Logic.ifElse (Lists.null v0) (Right (Utils.javaRefType [] JavaNames.javaLangPackageName "Void")) (Left (Errors.ErrorOther (Errors.OtherError "unexpected anonymous record type")))
        Core.TypeMaybe v0 -> Eithers.bind (Eithers.bind (encodeType aliases boundVars v0 cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jot -> Right (Utils.javaRefType [
          jot] JavaNames.hydraUtilPackageName "Maybe"))
        Core.TypeSet v0 -> Eithers.bind (Eithers.bind (encodeType aliases boundVars v0 cx g) (\jt_ -> Utils.javaTypeToJavaReferenceType jt_ cx)) (\jst -> Right (Utils.javaRefType [
          jst] JavaNames.javaUtilPackageName "Set"))
        Core.TypeUnion _ -> Left (Errors.ErrorOther (Errors.OtherError "unexpected anonymous union type"))
        Core.TypeVariable v0 ->
          let name = Maybes.fromMaybe v0 (Maps.lookup v0 typeVarSubst)
          in (Eithers.bind (encodeType_resolveIfTypedef aliases boundVars inScopeTypeParams name cx g) (\resolved -> Maybes.cases resolved (Right (Logic.ifElse (Logic.or (Sets.member name boundVars) (Sets.member name inScopeTypeParams)) (Syntax.TypeReference (Utils.javaTypeVariable (Core.unName name))) (Logic.ifElse (isLambdaBoundVariable name) (Syntax.TypeReference (Utils.javaTypeVariable (Core.unName name))) (Logic.ifElse (isUnresolvedInferenceVar name) (Syntax.TypeReference (Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass (Utils.javaClassType [] JavaNames.javaLangPackageName "Object")))) (Syntax.TypeReference (Utils.nameToJavaReferenceType aliases True [] name Nothing)))))) (\resolvedType -> encodeType aliases boundVars resolvedType cx g)))
        Core.TypeWrap _ -> Left (Errors.ErrorOther (Errors.OtherError "unexpected anonymous wrap type"))
        _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "can't encode unsupported type in Java: " (ShowCore.type_ t))))

encodeTypeDefinition :: Syntax.PackageDeclaration -> JavaEnvironment.Aliases -> Packaging.TypeDefinition -> Context.Context -> Graph.Graph -> Either Errors.Error (Core.Name, Syntax.CompilationUnit)
encodeTypeDefinition pkg aliases tdef cx g =

      let name = Packaging.typeDefinitionName tdef
          typ = Core.typeSchemeType (Packaging.typeDefinitionType tdef)
          serializable = isSerializableJavaType typ
          imports =
                  Logic.ifElse serializable [
                    Syntax.ImportDeclarationSingleType (Syntax.SingleTypeImportDeclaration (Utils.javaTypeName (Syntax.Identifier "java.io.Serializable")))] []
      in (Eithers.bind (toClassDecl False serializable aliases [] name typ cx g) (\decl -> Eithers.bind (Annotations.getTypeDescription cx g typ) (\comment ->
        let tdecl =
                Syntax.TypeDeclarationWithComments {
                  Syntax.typeDeclarationWithCommentsValue = (Syntax.TypeDeclarationClass decl),
                  Syntax.typeDeclarationWithCommentsComments = comment}
        in (Right (name, (Syntax.CompilationUnitOrdinary (Syntax.OrdinaryCompilationUnit {
          Syntax.ordinaryCompilationUnitPackage = (Just pkg),
          Syntax.ordinaryCompilationUnitImports = imports,
          Syntax.ordinaryCompilationUnitTypes = [
            tdecl]})))))))

encodeType_resolveIfTypedef :: t0 -> S.Set Core.Name -> S.Set Core.Name -> Core.Name -> t1 -> Graph.Graph -> Either t2 (Maybe Core.Type)
encodeType_resolveIfTypedef aliases boundVars inScopeTypeParams name cx g =
    Logic.ifElse (Logic.or (Sets.member name boundVars) (Sets.member name inScopeTypeParams)) (Right Nothing) (Logic.ifElse (isLambdaBoundVariable name) (Right Nothing) (
      let schemaTypes = Graph.graphSchemaTypes g
      in (Maybes.cases (Maps.lookup name schemaTypes) (Right Nothing) (\ts -> Logic.ifElse (Logic.not (Lists.null (Core.typeSchemeVariables ts))) (Right Nothing) (case (Strip.deannotateType (Core.typeSchemeType ts)) of
        Core.TypeRecord _ -> Right Nothing
        Core.TypeUnion _ -> Right Nothing
        Core.TypeWrap _ -> Right Nothing
        _ -> Right (Just (Core.typeSchemeType ts)))))))

encodeVariable :: JavaEnvironment.JavaEnvironment -> Core.Name -> t0 -> Graph.Graph -> Either Errors.Error Syntax.Expression
encodeVariable env name cx g =

      let aliases = JavaEnvironment.javaEnvironmentAliases env
          resolvedName = Utils.lookupJavaVarName aliases name
          jid = Utils.javaIdentifier (Core.unName resolvedName)
      in (Logic.ifElse (Sets.member name (JavaEnvironment.aliasesBranchVars aliases)) (Right (Utils.javaFieldAccessToJavaExpression (Syntax.FieldAccess {
        Syntax.fieldAccessQualifier = (Syntax.FieldAccess_QualifierPrimary (Utils.javaExpressionToJavaPrimary (Utils.javaIdentifierToJavaExpression jid))),
        Syntax.fieldAccessIdentifier = (Utils.javaIdentifier JavaNames.valueFieldName)}))) (Logic.ifElse (Logic.and (Equality.equal name (Core.Name (Strings.cat [
        JavaNames.instanceName,
        "_",
        JavaNames.valueFieldName]))) (isRecursiveVariable aliases name)) (
        let instanceExpr = Utils.javaIdentifierToJavaExpression (Utils.javaIdentifier JavaNames.instanceName)
        in (Right (Utils.javaFieldAccessToJavaExpression (Syntax.FieldAccess {
          Syntax.fieldAccessQualifier = (Syntax.FieldAccess_QualifierPrimary (Utils.javaExpressionToJavaPrimary instanceExpr)),
          Syntax.fieldAccessIdentifier = (Utils.javaIdentifier JavaNames.valueFieldName)})))) (Logic.ifElse (Logic.and (isRecursiveVariable aliases name) (Logic.not (isLambdaBoundIn name (JavaEnvironment.aliasesLambdaVars aliases)))) (Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocation (Just (Left (Syntax.ExpressionName {
        Syntax.expressionNameQualifier = Nothing,
        Syntax.expressionNameIdentifier = jid}))) (Syntax.Identifier JavaNames.getMethodName) []))) (Logic.ifElse (Logic.and (Sets.member name (JavaEnvironment.aliasesThunkedVars aliases)) (Logic.not (isLambdaBoundIn name (JavaEnvironment.aliasesLambdaVars aliases)))) (Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocation (Just (Left (Syntax.ExpressionName {
        Syntax.expressionNameQualifier = Nothing,
        Syntax.expressionNameIdentifier = jid}))) (Syntax.Identifier JavaNames.getMethodName) []))) (Logic.ifElse (isLambdaBoundIn name (JavaEnvironment.aliasesLambdaVars aliases)) (
        let actualName = findMatchingLambdaVar name (JavaEnvironment.aliasesLambdaVars aliases)
            resolvedActual = Utils.lookupJavaVarName aliases actualName
        in (Right (Utils.javaIdentifierToJavaExpression (Utils.variableToJavaIdentifier resolvedActual)))) (Logic.ifElse (Sets.member name (JavaEnvironment.aliasesInScopeJavaVars aliases)) (Right (Utils.javaIdentifierToJavaExpression (elementJavaIdentifier False False aliases resolvedName))) (Eithers.bind (classifyDataReference name cx g) (\cls -> case cls of
        JavaEnvironment.JavaSymbolClassHoistedLambda v0 -> encodeVariable_hoistedLambdaCase aliases name v0 cx g
        JavaEnvironment.JavaSymbolClassLocalVariable -> Right (Utils.javaIdentifierToJavaExpression (elementJavaIdentifier False False aliases resolvedName))
        JavaEnvironment.JavaSymbolClassConstant -> Right (Utils.javaIdentifierToJavaExpression (elementJavaIdentifier False False aliases name))
        JavaEnvironment.JavaSymbolClassNullaryFunction -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocation Nothing (elementJavaIdentifier False False aliases name) []))
        JavaEnvironment.JavaSymbolClassUnaryFunction -> Right (Utils.javaIdentifierToJavaExpression (elementJavaIdentifier False True aliases name))))))))))

encodeVariable_buildCurried :: [Core.Name] -> Syntax.Expression -> Syntax.Expression
encodeVariable_buildCurried params inner =
    Maybes.fromMaybe inner (Maybes.map (\p -> Utils.javaLambda (Pairs.first p) (encodeVariable_buildCurried (Pairs.second p) inner)) (Lists.uncons params))

encodeVariable_hoistedLambdaCase :: JavaEnvironment.Aliases -> Core.Name -> Int -> t0 -> Graph.Graph -> Either Errors.Error Syntax.Expression
encodeVariable_hoistedLambdaCase aliases name arity cx g =

      let paramNames = Lists.map (\i -> Core.Name (Strings.cat2 "p" (Literals.showInt32 i))) (Math.range 0 (Math.sub arity 1))
          paramExprs = Lists.map (\pn -> Utils.javaIdentifierToJavaExpression (Utils.variableToJavaIdentifier pn)) paramNames
          call =
                  Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocation Nothing (elementJavaIdentifier False False aliases name) paramExprs)
          lam = encodeVariable_buildCurried paramNames call
      in (Eithers.bind (Right (Lexical.lookupBinding g name)) (\mel -> Maybes.cases mel (Right lam) (\el -> Maybes.cases (Core.bindingType el) (Right lam) (\ts ->
        let typ = Core.typeSchemeType ts
        in (Eithers.bind (encodeType aliases Sets.empty typ cx g) (\jtype -> Eithers.bind (Utils.javaTypeToJavaReferenceType jtype cx) (\rt -> Right (Utils.javaCastExpressionToJavaExpression (Utils.javaCastExpression rt (Utils.javaExpressionToJavaUnaryExpression lam))))))))))

eqClause :: String -> Core.FieldType -> Syntax.InclusiveOrExpression
eqClause tmpName ft =

      let fname = Core.unName (Core.fieldTypeName ft)
          ftype = Core.fieldTypeType ft
      in (Logic.ifElse (isBinaryType ftype) (arraysEqualsClause tmpName fname) (Logic.ifElse (isBigNumericType ftype) (compareToZeroClause tmpName fname) (equalsClause tmpName fname)))

equalsClause :: String -> String -> Syntax.InclusiveOrExpression
equalsClause tmpName fname =

      let thisArg =
              Utils.javaExpressionNameToJavaExpression (Utils.fieldExpression (Syntax.Identifier "this") (Utils.javaIdentifier fname))
          otherArg =
                  Utils.javaExpressionNameToJavaExpression (Utils.fieldExpression (Utils.javaIdentifier tmpName) (Utils.javaIdentifier fname))
          header =
                  Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
                    Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantType (Utils.javaTypeName (Syntax.Identifier "java.util.Objects"))),
                    Syntax.methodInvocation_ComplexTypeArguments = [],
                    Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier JavaNames.equalsMethodName)})
      in (Utils.javaPostfixExpressionToJavaInclusiveOrExpression (Utils.javaMethodInvocationToJavaPostfixExpression (Syntax.MethodInvocation {
        Syntax.methodInvocationHeader = header,
        Syntax.methodInvocationArguments = [
          thisArg,
          otherArg]})))

extractArgType :: t0 -> Core.Type -> Core.Type
extractArgType _lhs typ =
    case typ of
      Core.TypeApplication v0 -> case (Core.applicationTypeFunction v0) of
        Core.TypeApplication _ -> Core.applicationTypeArgument v0
        _ -> typ
      _ -> typ

extractDirectReturn :: S.Set Core.Name -> Core.Type -> [(Core.Name, Core.Name)]
extractDirectReturn tparamSet t = extractDirectReturn_go tparamSet t

extractDirectReturn_go :: S.Set Core.Name -> Core.Type -> [(Core.Name, Core.Name)]
extractDirectReturn_go tparamSet t =
    case (Strip.deannotateType t) of
      Core.TypeFunction v0 ->
        let dom = Strip.deannotateType (Core.functionTypeDomain v0)
            cod = Core.functionTypeCodomain v0
        in case dom of
          Core.TypeVariable v1 -> Logic.ifElse (Sets.member v1 tparamSet) (case (Strip.deannotateType cod) of
            Core.TypeFunction v2 ->
              let midArg = Strip.deannotateType (Core.functionTypeDomain v2)
                  retPart = Strip.deannotateType (Core.functionTypeCodomain v2)
              in case midArg of
                Core.TypeVariable v3 -> Logic.ifElse (Sets.member v3 tparamSet) [] (case retPart of
                  Core.TypeVariable v4 -> Logic.ifElse (Sets.member v4 tparamSet) [
                    (v1, v4)] []
                  _ -> [])
                _ -> case retPart of
                  Core.TypeVariable v3 -> Logic.ifElse (Sets.member v3 tparamSet) [
                    (v1, v3)] []
                  _ -> []
            _ -> []) (extractDirectReturn_go tparamSet cod)
          _ -> extractDirectReturn_go tparamSet cod
      _ -> []

extractInOutPair :: Core.Type -> [(Core.Name, Core.Name)]
extractInOutPair t =
    case (Strip.deannotateType t) of
      Core.TypeFunction v0 -> case (Strip.deannotateType (Core.functionTypeDomain v0)) of
        Core.TypeVariable v1 ->
          let retType = unwrapReturnType (Core.functionTypeCodomain v0)
          in case (Strip.deannotateType retType) of
            Core.TypePair v2 -> case (Strip.deannotateType (Core.pairTypeFirst v2)) of
              Core.TypeVariable v3 -> [
                (v1, v3)]
              _ -> []
            _ -> []
        _ -> []
      _ -> []

extractTypeApplicationArgs :: Core.Type -> [Core.Type]
extractTypeApplicationArgs typ = Lists.reverse (extractTypeApplicationArgs_go typ)

extractTypeApplicationArgs_go :: Core.Type -> [Core.Type]
extractTypeApplicationArgs_go t =
    case t of
      Core.TypeApplication v0 -> Lists.cons (Core.applicationTypeArgument v0) (extractTypeApplicationArgs_go (Core.applicationTypeFunction v0))
      _ -> []

fieldTypeToFormalParam :: JavaEnvironment.Aliases -> Core.FieldType -> t0 -> Graph.Graph -> Either Errors.Error Syntax.FormalParameter
fieldTypeToFormalParam aliases ft cx g =
    Eithers.bind (encodeType aliases Sets.empty (Core.fieldTypeType ft) cx g) (\jt -> Right (Utils.javaTypeToJavaFormalParameter jt (Core.fieldTypeName ft)))

filterByFlags :: [t0] -> [Bool] -> [t0]
filterByFlags xs flags = Lists.map (\p -> Pairs.first p) (Lists.filter (\p -> Pairs.second p) (Lists.zip xs flags))

filterPhantomTypeArgs :: Core.Name -> [Core.Type] -> t0 -> Graph.Graph -> Either t1 [Core.Type]
filterPhantomTypeArgs calleeName allTypeArgs cx g =
    Eithers.bind (Right (Lexical.lookupBinding g calleeName)) (\mel -> Maybes.cases mel (Right allTypeArgs) (\el -> Maybes.cases (Core.bindingType el) (Right allTypeArgs) (\ts ->
      let schemeVars = Lists.filter (\v -> isSimpleName v) (Core.typeSchemeVariables ts)
          schemeTypeVars = collectTypeVars (Core.typeSchemeType ts)
          schemeType = Core.typeSchemeType ts
          nParams = countFunctionParams schemeType
          peeled = peelDomainTypes nParams schemeType
          calleeDoms = Pairs.first peeled
          calleeCod = Pairs.second peeled
          overgenSubst = detectAccumulatorUnification calleeDoms calleeCod schemeVars
          keepFlags = Lists.map (\v -> Logic.and (Sets.member v schemeTypeVars) (Logic.not (Maps.member v overgenSubst))) schemeVars
      in (Logic.ifElse (Logic.not (Equality.equal (Lists.length schemeVars) (Lists.length allTypeArgs))) (Right allTypeArgs) (Right (filterPhantomTypeArgs_filterAndApply allTypeArgs keepFlags overgenSubst))))))

filterPhantomTypeArgs_filterAndApply :: [Core.Type] -> [Bool] -> M.Map Core.Name Core.Type -> [Core.Type]
filterPhantomTypeArgs_filterAndApply allTypeArgs keepFlags overgenSubst =

      let filtered = Lists.map (\p -> Pairs.first p) (Lists.filter (\p -> Pairs.second p) (Lists.zip allTypeArgs keepFlags))
      in (Logic.ifElse (Logic.not (Maps.null overgenSubst)) (Lists.map (\t -> substituteTypeVarsWithTypes overgenSubst t) filtered) filtered)

findMatchingLambdaVar :: Core.Name -> S.Set Core.Name -> Core.Name
findMatchingLambdaVar name lambdaVars =
    Logic.ifElse (Sets.member name lambdaVars) name (Logic.ifElse (isLambdaBoundIn_isQualified name) (Maybes.fromMaybe name (Lists.find (\lv -> Logic.and (isLambdaBoundIn_isQualified lv) (Equality.equal (Names.localNameOf lv) (Names.localNameOf name))) (Sets.toList lambdaVars))) (Logic.ifElse (Sets.member (Core.Name (Names.localNameOf name)) lambdaVars) (Core.Name (Names.localNameOf name)) name))

findPairFirst :: Core.Type -> Maybe Core.Name
findPairFirst t =
    case (Strip.deannotateType t) of
      Core.TypePair v0 -> case (Strip.deannotateType (Core.pairTypeFirst v0)) of
        Core.TypeVariable v1 -> Just v1
        _ -> Nothing
      _ -> Nothing

findSelfRefVar :: (Eq t0, Ord t0) => (M.Map t0 [t0] -> Maybe t0)
findSelfRefVar grouped =

      let selfRefs = Lists.filter (\entry -> Lists.elem (Pairs.first entry) (Pairs.second entry)) (Maps.toList grouped)
      in (Maybes.map (\entry -> Pairs.first entry) (Lists.maybeHead selfRefs))

first20Primes :: [Integer]
first20Primes =
    [
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

flattenApps :: Core.Term -> [Core.Term] -> ([Core.Term], Core.Term)
flattenApps t acc =
    case (Strip.deannotateTerm t) of
      Core.TermApplication v0 -> flattenApps (Core.applicationFunction v0) (Lists.cons (Core.applicationArgument v0) acc)
      _ -> (acc, t)

flattenBindings :: [Core.Binding] -> [Core.Binding]
flattenBindings bindings =
    Lists.bind bindings (\b -> case (Strip.deannotateTerm (Core.bindingTerm b)) of
      Core.TermLet v0 -> Lists.concat2 (flattenBindings (Core.letBindings v0)) [
        Core.Binding {
          Core.bindingName = (Core.bindingName b),
          Core.bindingTerm = (Core.letBody v0),
          Core.bindingType = (Core.bindingType b)}]
      _ -> [
        b])

freshJavaName :: Core.Name -> S.Set Core.Name -> Core.Name
freshJavaName base avoid = freshJavaName_go base avoid 2

freshJavaName_go :: Core.Name -> S.Set Core.Name -> Int -> Core.Name
freshJavaName_go base avoid i =

      let candidate = Core.Name (Strings.cat2 (Core.unName base) (Literals.showInt32 i))
      in (Logic.ifElse (Sets.member candidate avoid) (freshJavaName_go base avoid (Math.add i 1)) candidate)

functionCall :: JavaEnvironment.JavaEnvironment -> Bool -> Core.Name -> [Core.Term] -> [Core.Type] -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.Expression
functionCall env isPrim name args typeApps cx g =

      let aliases = JavaEnvironment.javaEnvironmentAliases env
          isLambdaBound = isLambdaBoundIn name (JavaEnvironment.aliasesLambdaVars aliases)
      in (Eithers.bind (Eithers.mapList (\arg -> encodeTerm env arg cx g) args) (\jargs0 ->
        let wrapResult = wrapLazyArguments name jargs0
            jargs = Pairs.first wrapResult
            mMethodOverride = Pairs.second wrapResult
        in (Logic.ifElse (Logic.or (isLocalVariable name) isLambdaBound) (Eithers.bind (encodeVariable env name cx g) (\baseExpr -> Right (Lists.foldl (\acc -> \jarg -> applyJavaArg acc jarg) baseExpr jargs))) (
          let overrideMethodName =
                  \jid -> Maybes.cases mMethodOverride jid (\m ->
                    let s = Syntax.unIdentifier jid
                    in (Syntax.Identifier (Strings.cat2 (Strings.fromList (Lists.take (Math.sub (Strings.length s) (Strings.length JavaNames.applyMethodName)) (Strings.toList s))) m)))
          in (Logic.ifElse (Lists.null typeApps) (
            let header =
                    Syntax.MethodInvocation_HeaderSimple (Syntax.MethodName (overrideMethodName (elementJavaIdentifier isPrim False aliases name)))
            in (Right (Utils.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
              Syntax.methodInvocationHeader = header,
              Syntax.methodInvocationArguments = jargs})))) (
            let qn = Names.qualifyName name
                mns = Packaging.qualifiedNameNamespace qn
                localName = Packaging.qualifiedNameLocal qn
            in (Maybes.cases mns (
              let header =
                      Syntax.MethodInvocation_HeaderSimple (Syntax.MethodName (overrideMethodName (elementJavaIdentifier isPrim False aliases name)))
              in (Right (Utils.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
                Syntax.methodInvocationHeader = header,
                Syntax.methodInvocationArguments = jargs})))) (\ns_ ->
              let classId = Utils.nameToJavaName aliases (elementsQualifiedName ns_)
                  methodId =
                          Logic.ifElse isPrim (overrideMethodName (Syntax.Identifier (Strings.cat2 (Syntax.unIdentifier (Utils.nameToJavaName aliases (Names.unqualifyName (Packaging.QualifiedName {
                            Packaging.qualifiedNameNamespace = (Just ns_),
                            Packaging.qualifiedNameLocal = (Formatting.capitalize localName)})))) (Strings.cat2 "." JavaNames.applyMethodName)))) (Syntax.Identifier (Utils.sanitizeJavaName localName))
              in (Eithers.bind (Eithers.mapList (\t -> Eithers.bind (encodeType aliases Sets.empty t cx g) (\jt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jt cx) (\rt -> Right (Syntax.TypeArgumentReference rt)))) typeApps) (\jTypeArgs -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs classId methodId jTypeArgs jargs))))))))))))

getCodomain :: M.Map Core.Name Core.Term -> t0 -> Graph.Graph -> Either Errors.Error Core.Type
getCodomain ann cx g = Eithers.map (\ft -> Core.functionTypeCodomain ft) (getFunctionType ann cx g)

getFunctionType :: M.Map Core.Name Core.Term -> t0 -> Graph.Graph -> Either Errors.Error Core.FunctionType
getFunctionType ann cx g =
    Eithers.bind (Eithers.bimap (\_de -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _de))) (\_a -> _a) (Annotations.getType g ann)) (\mt -> Maybes.cases mt (Left (Errors.ErrorOther (Errors.OtherError "type annotation is required for function and elimination terms in Java"))) (\t -> case t of
      Core.TypeFunction v0 -> Right v0
      _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "expected function type, got: " (ShowCore.type_ t))))))

groupPairsByFirst :: Ord t0 => ([(t0, t1)] -> M.Map t0 [t1])
groupPairsByFirst pairs =
    Lists.foldl (\m -> \p ->
      let k = Pairs.first p
          v = Pairs.second p
      in (Maps.alter (\mv -> Maybes.maybe (Just [
        v]) (\vs -> Just (Lists.concat2 vs [
        v])) mv) k m)) Maps.empty pairs

hashCodeCompareExpr :: String -> String -> Syntax.Expression
hashCodeCompareExpr otherVar fname =

      let header =
              Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
                Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantType (Utils.javaTypeName (Syntax.Identifier "Integer"))),
                Syntax.methodInvocation_ComplexTypeArguments = [],
                Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier "compare")})
          thisHashCode =
                  Utils.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
                    Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
                      Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantExpression (Syntax.ExpressionName {
                        Syntax.expressionNameQualifier = Nothing,
                        Syntax.expressionNameIdentifier = (Syntax.Identifier (Utils.sanitizeJavaName fname))})),
                      Syntax.methodInvocation_ComplexTypeArguments = [],
                      Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier JavaNames.hashCodeMethodName)})),
                    Syntax.methodInvocationArguments = []})
          otherHashCode =
                  Utils.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
                    Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
                      Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantExpression (Utils.fieldExpression (Utils.javaIdentifier otherVar) (Utils.javaIdentifier fname))),
                      Syntax.methodInvocation_ComplexTypeArguments = [],
                      Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier JavaNames.hashCodeMethodName)})),
                    Syntax.methodInvocationArguments = []})
      in (Utils.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
        Syntax.methodInvocationHeader = header,
        Syntax.methodInvocationArguments = [
          thisHashCode,
          otherHashCode]}))

hashCodeMultPair :: Integer -> Core.Name -> Syntax.MultiplicativeExpression
hashCodeMultPair i fname =

      let fnameStr = Core.unName fname
          lhs =
                  Syntax.MultiplicativeExpressionUnary (Utils.javaPrimaryToJavaUnaryExpression (Utils.javaLiteralToJavaPrimary (Utils.javaInt i)))
          rhs =
                  Utils.javaPostfixExpressionToJavaUnaryExpression (Utils.javaMethodInvocationToJavaPostfixExpression (Syntax.MethodInvocation {
                    Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
                      Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantType (Utils.javaTypeName (Syntax.Identifier "java.util.Objects"))),
                      Syntax.methodInvocation_ComplexTypeArguments = [],
                      Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier JavaNames.hashCodeMethodName)})),
                    Syntax.methodInvocationArguments = [
                      Utils.javaExpressionNameToJavaExpression (Syntax.ExpressionName {
                        Syntax.expressionNameQualifier = Nothing,
                        Syntax.expressionNameIdentifier = (Syntax.Identifier (Utils.sanitizeJavaName fnameStr))})]}))
      in (Syntax.MultiplicativeExpressionTimes (Syntax.MultiplicativeExpression_Binary {
        Syntax.multiplicativeExpression_BinaryLhs = lhs,
        Syntax.multiplicativeExpression_BinaryRhs = rhs}))

innerClassRef :: JavaEnvironment.Aliases -> Core.Name -> String -> Syntax.Identifier
innerClassRef aliases name local =

      let id = Syntax.unIdentifier (Utils.nameToJavaName aliases name)
      in (Syntax.Identifier (Strings.cat2 (Strings.cat2 id ".") local))

insertBranchVar :: Core.Name -> JavaEnvironment.JavaEnvironment -> JavaEnvironment.JavaEnvironment
insertBranchVar name env =

      let aliases = JavaEnvironment.javaEnvironmentAliases env
      in JavaEnvironment.JavaEnvironment {
        JavaEnvironment.javaEnvironmentAliases = JavaEnvironment.Aliases {
          JavaEnvironment.aliasesCurrentNamespace = (JavaEnvironment.aliasesCurrentNamespace aliases),
          JavaEnvironment.aliasesPackages = (JavaEnvironment.aliasesPackages aliases),
          JavaEnvironment.aliasesBranchVars = (Sets.insert name (JavaEnvironment.aliasesBranchVars aliases)),
          JavaEnvironment.aliasesRecursiveVars = (JavaEnvironment.aliasesRecursiveVars aliases),
          JavaEnvironment.aliasesInScopeTypeParams = (JavaEnvironment.aliasesInScopeTypeParams aliases),
          JavaEnvironment.aliasesPolymorphicLocals = (JavaEnvironment.aliasesPolymorphicLocals aliases),
          JavaEnvironment.aliasesInScopeJavaVars = (JavaEnvironment.aliasesInScopeJavaVars aliases),
          JavaEnvironment.aliasesVarRenames = (JavaEnvironment.aliasesVarRenames aliases),
          JavaEnvironment.aliasesLambdaVars = (JavaEnvironment.aliasesLambdaVars aliases),
          JavaEnvironment.aliasesTypeVarSubst = (JavaEnvironment.aliasesTypeVarSubst aliases),
          JavaEnvironment.aliasesTrustedTypeVars = (JavaEnvironment.aliasesTrustedTypeVars aliases),
          JavaEnvironment.aliasesMethodCodomain = Nothing,
          JavaEnvironment.aliasesThunkedVars = (JavaEnvironment.aliasesThunkedVars aliases)},
        JavaEnvironment.javaEnvironmentGraph = (JavaEnvironment.javaEnvironmentGraph env)}

interfaceTypes :: Bool -> JavaEnvironment.Aliases -> [Syntax.TypeParameter] -> Core.Name -> [Syntax.InterfaceType]
interfaceTypes isSer aliases tparams elName =

      let javaSerializableType =
              Syntax.InterfaceType (Syntax.ClassType {
                Syntax.classTypeAnnotations = [],
                Syntax.classTypeQualifier = Syntax.ClassTypeQualifierNone,
                Syntax.classTypeIdentifier = (Utils.javaTypeIdentifier "Serializable"),
                Syntax.classTypeArguments = []})
          selfTypeArg =
                  Syntax.TypeArgumentReference (Utils.nameToJavaReferenceType aliases False (Lists.map (\tp_ -> Utils.typeParameterToTypeArgument tp_) tparams) elName Nothing)
          javaComparableType =
                  Syntax.InterfaceType (Syntax.ClassType {
                    Syntax.classTypeAnnotations = [],
                    Syntax.classTypeQualifier = Syntax.ClassTypeQualifierNone,
                    Syntax.classTypeIdentifier = (Utils.javaTypeIdentifier "Comparable"),
                    Syntax.classTypeArguments = [
                      selfTypeArg]})
      in (Logic.ifElse isSer [
        javaSerializableType,
        javaComparableType] [])

isBigNumericType :: Core.Type -> Bool
isBigNumericType typ =
    case (Strip.deannotateType typ) of
      Core.TypeLiteral v0 -> case v0 of
        Core.LiteralTypeDecimal -> True
        Core.LiteralTypeFloat v1 -> case v1 of
          Core.FloatTypeBigfloat -> True
          _ -> False
        Core.LiteralTypeInteger v1 -> case v1 of
          Core.IntegerTypeBigint -> True
          _ -> False
        _ -> False
      _ -> False

isBinaryType :: Core.Type -> Bool
isBinaryType typ =
    case (Strip.deannotateType typ) of
      Core.TypeLiteral v0 -> case v0 of
        Core.LiteralTypeBinary -> True
        _ -> False
      _ -> False

isFieldUnitType :: Core.Name -> Core.Name -> t0 -> Graph.Graph -> Either t1 Bool
isFieldUnitType typeName fieldName cx g =

      let schemaTypes = Graph.graphSchemaTypes g
      in (Maybes.cases (Maps.lookup typeName schemaTypes) (Right False) (\ts -> case (Strip.deannotateType (Core.typeSchemeType ts)) of
        Core.TypeUnion v0 -> Right (Maybes.cases (Lists.find (\ft -> Equality.equal (Core.fieldTypeName ft) fieldName) v0) False (\ft -> Predicates.isUnitType (Strip.deannotateType (Core.fieldTypeType ft))))
        _ -> Right False))

isLambdaBoundIn :: Core.Name -> S.Set Core.Name -> Bool
isLambdaBoundIn name lambdaVars =
    Logic.or (Sets.member name lambdaVars) (Logic.or (Logic.and (isLambdaBoundIn_isQualified name) (Maybes.isJust (Lists.find (\lv -> Logic.and (isLambdaBoundIn_isQualified lv) (Equality.equal (Names.localNameOf lv) (Names.localNameOf name))) (Sets.toList lambdaVars)))) (Logic.and (Logic.not (isLambdaBoundIn_isQualified name)) (Sets.member (Core.Name (Names.localNameOf name)) lambdaVars)))

isLambdaBoundIn_isQualified :: Core.Name -> Bool
isLambdaBoundIn_isQualified n = Maybes.isJust (Packaging.qualifiedNameNamespace (Names.qualifyName n))

isLambdaBoundVariable :: Core.Name -> Bool
isLambdaBoundVariable name =

      let v = Core.unName name
      in (Equality.lte (Strings.length v) 4)

isLocalVariable :: Core.Name -> Bool
isLocalVariable name = Maybes.isNothing (Packaging.qualifiedNameNamespace (Names.qualifyName name))

isNonComparableType :: Core.Type -> Bool
isNonComparableType typ =
    case (Strip.deannotateType typ) of
      Core.TypeEither _ -> True
      Core.TypeFunction _ -> True
      Core.TypeUnit -> True
      Core.TypeLiteral v0 -> case v0 of
        Core.LiteralTypeBinary -> True
        _ -> False
      Core.TypeForall v0 -> isNonComparableType (Core.forallTypeBody v0)
      _ -> False

isRecursiveVariable :: JavaEnvironment.Aliases -> Core.Name -> Bool
isRecursiveVariable aliases name = Sets.member name (JavaEnvironment.aliasesRecursiveVars aliases)

isSerializableJavaType :: Core.Type -> Bool
isSerializableJavaType typ = Predicates.isNominalType typ

isSimpleName :: Core.Name -> Bool
isSimpleName name = Equality.equal (Lists.length (Strings.splitOn "." (Core.unName name))) 1

isUnresolvedInferenceVar :: Core.Name -> Bool
isUnresolvedInferenceVar name =

      let chars = Strings.toList (Core.unName name)
      in (Maybes.fromMaybe False (Maybes.map (\p ->
        let firstCh = Pairs.first p
            rest = Pairs.second p
        in (Logic.ifElse (Logic.not (Equality.equal firstCh 116)) False (Logic.and (Logic.not (Lists.null rest)) (Lists.null (Lists.filter (\c -> Logic.not (isUnresolvedInferenceVar_isDigit c)) rest))))) (Lists.uncons chars)))

isUnresolvedInferenceVar_isDigit :: Int -> Bool
isUnresolvedInferenceVar_isDigit c = Logic.and (Equality.gte c 48) (Equality.lte c 57)

java11Features :: JavaEnvironment.JavaFeatures
java11Features = JavaEnvironment.JavaFeatures {
  JavaEnvironment.javaFeaturesSupportsDiamondOperator = True}

java8Features :: JavaEnvironment.JavaFeatures
java8Features = JavaEnvironment.JavaFeatures {
  JavaEnvironment.javaFeaturesSupportsDiamondOperator = False}

javaComparableRefType :: Syntax.ReferenceType
javaComparableRefType =
    Syntax.ReferenceTypeClassOrInterface (Syntax.ClassOrInterfaceTypeClass (Syntax.ClassType {
      Syntax.classTypeAnnotations = [],
      Syntax.classTypeQualifier = Syntax.ClassTypeQualifierNone,
      Syntax.classTypeIdentifier = (Utils.javaTypeIdentifier "Comparable"),
      Syntax.classTypeArguments = []}))

javaEnvGetGraph :: JavaEnvironment.JavaEnvironment -> Graph.Graph
javaEnvGetGraph env = JavaEnvironment.javaEnvironmentGraph env

javaEnvSetGraph :: Graph.Graph -> JavaEnvironment.JavaEnvironment -> JavaEnvironment.JavaEnvironment
javaEnvSetGraph g env =
    JavaEnvironment.JavaEnvironment {
      JavaEnvironment.javaEnvironmentAliases = (JavaEnvironment.javaEnvironmentAliases env),
      JavaEnvironment.javaEnvironmentGraph = g}

javaFeatures :: JavaEnvironment.JavaFeatures
javaFeatures = java11Features

javaIdentifierToString :: Syntax.Identifier -> String
javaIdentifierToString id = Syntax.unIdentifier id

javaTypeArgumentsForNamedType :: Core.Name -> t0 -> Graph.Graph -> Either Errors.Error [Syntax.TypeArgument]
javaTypeArgumentsForNamedType tname cx g =
    Eithers.bind (Resolution.requireType cx g tname) (\typ -> Right (Lists.map (\tp_ -> Utils.typeParameterToTypeArgument tp_) (javaTypeParametersForType typ)))

javaTypeArgumentsForType :: Core.Type -> [Syntax.TypeArgument]
javaTypeArgumentsForType typ = Lists.reverse (Lists.map Utils.typeParameterToTypeArgument (javaTypeParametersForType typ))

javaTypeParametersForType :: Core.Type -> [Syntax.TypeParameter]
javaTypeParametersForType typ =

      let toParam = \name -> Utils.javaTypeParameter (Formatting.capitalize (Core.unName name))
          boundVars = javaTypeParametersForType_bvars typ
          freeVars = Lists.filter (\v -> isLambdaBoundVariable v) (Sets.toList (Variables.freeVariablesInType typ))
          vars = Lists.nub (Lists.concat2 boundVars freeVars)
      in (Lists.map toParam vars)

javaTypeParametersForType_bvars :: Core.Type -> [Core.Name]
javaTypeParametersForType_bvars t =
    case t of
      Core.TypeForall v0 -> Lists.cons (Core.forallTypeParameter v0) (javaTypeParametersForType_bvars (Core.forallTypeBody v0))
      _ -> []

moduleToJava :: Packaging.Module -> [Packaging.Definition] -> Context.Context -> Graph.Graph -> Either Errors.Error (M.Map String String)
moduleToJava mod defs cx g =
    Eithers.bind (encodeDefinitions mod defs cx g) (\units -> Right (Maps.fromList (Lists.map (\entry ->
      let name = Pairs.first entry
          unit = Pairs.second entry
      in (bindingNameToFilePath name, (Serialization.printExpr (Serialization.parenthesize (Serde.writeCompilationUnit unit))))) (Maps.toList units))))

nameMapToTypeMap :: Ord t0 => (M.Map t0 Core.Name -> M.Map t0 Core.Type)
nameMapToTypeMap m = Maps.map (\v -> Core.TypeVariable v) m

namespaceParent :: Packaging.Namespace -> Maybe Packaging.Namespace
namespaceParent ns =

      let parts = Strings.splitOn "." (Packaging.unNamespace ns)
          initParts = Maybes.fromMaybe [] (Lists.maybeInit parts)
      in (Logic.ifElse (Lists.null initParts) Nothing (Just (Packaging.Namespace (Strings.intercalate "." initParts))))

needsThunking :: Core.Term -> Bool
needsThunking t =
    case (Strip.deannotateTerm t) of
      Core.TermLet _ -> True
      Core.TermTypeApplication _ -> True
      Core.TermTypeLambda _ -> True
      _ -> Lists.foldl (\b -> \st -> Logic.or b (needsThunking st)) False (Rewriting.subterms t)

noComment :: Syntax.ClassBodyDeclaration -> Syntax.ClassBodyDeclarationWithComments
noComment decl =
    Syntax.ClassBodyDeclarationWithComments {
      Syntax.classBodyDeclarationWithCommentsValue = decl,
      Syntax.classBodyDeclarationWithCommentsComments = Nothing}

otherwiseBranch :: JavaEnvironment.JavaEnvironment -> JavaEnvironment.Aliases -> Core.Type -> Core.Type -> Core.Name -> Syntax.Type -> [Syntax.TypeArgument] -> Core.Term -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.ClassBodyDeclarationWithComments
otherwiseBranch env aliases dom cod tname jcod targs d cx g =

      let jdom = Syntax.TypeReference (Utils.nameToJavaReferenceType aliases True targs tname Nothing)
          mods = [
                Syntax.MethodModifierPublic]
          anns = [
                Utils.overrideAnnotation]
          param = Utils.javaTypeToJavaFormalParameter jdom (Core.Name "instance")
          result = Syntax.ResultType (Syntax.UnannType jcod)
      in (Eithers.bind (analyzeJavaFunction env d cx g) (\fs ->
        let bindings = Typing.functionStructureBindings fs
            rawBody = Typing.functionStructureBody fs
            innerBody = annotateBodyWithCod cod rawBody
            env2 = Typing.functionStructureEnvironment fs
        in (Eithers.bind (bindingsToStatements env2 bindings cx g) (\bindResult ->
          let bindingStmts = Pairs.first bindResult
              env3 = Pairs.second bindResult
          in (Eithers.bind (encodeTerm env3 innerBody cx g) (\jret ->
            let returnStmt = Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just jret))
                allStmts = Lists.concat2 bindingStmts [
                      returnStmt]
            in (Right (noComment (Utils.methodDeclaration mods [] anns JavaNames.otherwiseMethodName [
              param] result (Just allStmts))))))))))

peelDomainTypes :: Int -> Core.Type -> ([Core.Type], Core.Type)
peelDomainTypes n t =
    Logic.ifElse (Equality.lte n 0) ([], t) (case (Strip.deannotateType t) of
      Core.TypeFunction v0 ->
        let rest = peelDomainTypes (Math.sub n 1) (Core.functionTypeCodomain v0)
        in (Lists.cons (Core.functionTypeDomain v0) (Pairs.first rest), (Pairs.second rest))
      _ -> ([], t))

peelDomainsAndCod :: Int -> Core.Type -> ([Core.Type], Core.Type)
peelDomainsAndCod n t =
    Logic.ifElse (Equality.lte n 0) ([], t) (case (Strip.deannotateType t) of
      Core.TypeFunction v0 ->
        let rest = peelDomainsAndCod (Math.sub n 1) (Core.functionTypeCodomain v0)
        in (Lists.cons (Core.functionTypeDomain v0) (Pairs.first rest), (Pairs.second rest))
      _ -> ([], t))

peelExpectedTypes :: M.Map Core.Name Core.Type -> Int -> Core.Type -> [Core.Type]
peelExpectedTypes subst n t =
    Logic.ifElse (Equality.equal n 0) [] (case (Strip.deannotateType t) of
      Core.TypeFunction v0 -> Lists.cons (applySubstFull subst (Core.functionTypeDomain v0)) (peelExpectedTypes subst (Math.sub n 1) (Core.functionTypeCodomain v0))
      _ -> [])

propagateType :: Core.Type -> Core.Term -> Core.Term
propagateType typ term =

      let setTypeAnn = \t -> Annotations.setTermAnnotation Constants.key_type (Just (EncodeCore.type_ typ)) t
      in case (Strip.deannotateTerm term) of
        Core.TermLambda _ ->
          let annotated = setTypeAnn term
          in case (Strip.deannotateType typ) of
            Core.TypeFunction v1 -> propagateType_propagateIntoLambda (Core.functionTypeCodomain v1) annotated
            _ -> annotated
        Core.TermLet v0 ->
          let propagatedBindings =
                  Lists.map (\b -> Maybes.maybe b (\ts -> Core.Binding {
                    Core.bindingName = (Core.bindingName b),
                    Core.bindingTerm = (propagateType (Core.typeSchemeType ts) (Core.bindingTerm b)),
                    Core.bindingType = (Core.bindingType b)}) (Core.bindingType b)) (Core.letBindings v0)
          in (setTypeAnn (propagateType_rebuildLet term propagatedBindings (propagateType typ (Core.letBody v0))))
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
              annotatedFun =
                      case (Strip.deannotateTerm fun) of
                        Core.TermCases v1 ->
                          let dom = Resolution.nominalApplication (Core.caseStatementTypeName v1) []
                              ft =
                                      Core.TypeFunction (Core.FunctionType {
                                        Core.functionTypeDomain = dom,
                                        Core.functionTypeCodomain = typ})
                          in (Annotations.setTermAnnotation Constants.key_type (Just (EncodeCore.type_ ft)) fun)
                        _ -> fun
          in (setTypeAnn (Core.TermApplication (Core.Application {
            Core.applicationFunction = annotatedFun,
            Core.applicationArgument = arg})))
        _ -> setTypeAnn term

propagateType_propagateIntoLambda :: Core.Type -> Core.Term -> Core.Term
propagateType_propagateIntoLambda cod t =
    case t of
      Core.TermAnnotated v0 -> Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (propagateType_propagateIntoLambda cod (Core.annotatedTermBody v0)),
        Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})
      Core.TermLambda v0 -> Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.lambdaParameter v0),
        Core.lambdaDomain = (Core.lambdaDomain v0),
        Core.lambdaBody = (propagateType cod (Core.lambdaBody v0))})
      _ -> t

propagateType_rebuildLet :: Core.Term -> [Core.Binding] -> Core.Term -> Core.Term
propagateType_rebuildLet t bindings newBody =
    case t of
      Core.TermAnnotated v0 -> Core.TermAnnotated (Core.AnnotatedTerm {
        Core.annotatedTermBody = (propagateType_rebuildLet (Core.annotatedTermBody v0) bindings newBody),
        Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})
      Core.TermLet _ -> Core.TermLet (Core.Let {
        Core.letBindings = bindings,
        Core.letBody = newBody})
      _ -> t

propagateTypesInAppChain :: Core.Type -> Core.Type -> Core.Term -> Core.Term
propagateTypesInAppChain fixedCod resultType t =

      let flattened = flattenApps t []
          args = Pairs.first flattened
          fun = Pairs.second flattened
          lambdaDomsResult = collectLambdaDomains fun
          lambdaDoms = Pairs.first lambdaDomsResult
          nArgs = Lists.length args
          nLambdaDoms = Lists.length lambdaDoms
      in (Logic.ifElse (Logic.and (Equality.gt nLambdaDoms 0) (Equality.gt nArgs 0)) (
        let bodyRetType = Pairs.second (peelDomainsAndCod (Math.sub nLambdaDoms nArgs) resultType)
            funType =
                    Lists.foldl (\c -> \d -> Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = d,
                      Core.functionTypeCodomain = c})) bodyRetType (Lists.reverse lambdaDoms)
            annotatedFun = Annotations.setTermAnnotation Constants.key_type (Just (EncodeCore.type_ funType)) fun
        in (rebuildApps annotatedFun args funType)) (case (Strip.deannotateTerm t) of
        Core.TermApplication v0 ->
          let lhs = Core.applicationFunction v0
              rhs = Core.applicationArgument v0
              annotatedLhs =
                      case (Strip.deannotateTerm lhs) of
                        Core.TermCases v1 ->
                          let dom = Resolution.nominalApplication (Core.caseStatementTypeName v1) []
                              ft =
                                      Core.TypeFunction (Core.FunctionType {
                                        Core.functionTypeDomain = dom,
                                        Core.functionTypeCodomain = fixedCod})
                          in (Annotations.setTermAnnotation Constants.key_type (Just (EncodeCore.type_ ft)) lhs)
                        _ -> lhs
          in (Annotations.setTermAnnotation Constants.key_type (Just (EncodeCore.type_ resultType)) (Core.TermApplication (Core.Application {
            Core.applicationFunction = annotatedLhs,
            Core.applicationArgument = rhs})))
        _ -> Annotations.setTermAnnotation Constants.key_type (Just (EncodeCore.type_ resultType)) t))

rebuildApps :: Core.Term -> [Core.Term] -> Core.Type -> Core.Term
rebuildApps f args fType =
    Logic.ifElse (Lists.null args) f (case (Strip.deannotateType fType) of
      Core.TypeFunction v0 -> Maybes.fromMaybe f (Maybes.map (\p ->
        let arg = Pairs.first p
            rest = Pairs.second p
            remainingType = Core.functionTypeCodomain v0
            app =
                    Core.TermApplication (Core.Application {
                      Core.applicationFunction = f,
                      Core.applicationArgument = arg})
            annotatedApp = Annotations.setTermAnnotation Constants.key_type (Just (EncodeCore.type_ remainingType)) app
        in (rebuildApps annotatedApp rest remainingType)) (Lists.uncons args))
      _ -> Lists.foldl (\acc -> \a -> Core.TermApplication (Core.Application {
        Core.applicationFunction = acc,
        Core.applicationArgument = a})) f args)

recordCompareToMethod :: JavaEnvironment.Aliases -> t0 -> Core.Name -> [Core.FieldType] -> Syntax.ClassBodyDeclaration
recordCompareToMethod aliases tparams elName fields =

      let anns =
              [
                Utils.overrideAnnotation,
                Utils.suppressWarningsUncheckedAnnotation]
          mods = [
                Syntax.MethodModifierPublic]
          param =
                  Utils.javaTypeToJavaFormalParameter (Utils.javaTypeFromTypeName aliases elName) (Core.Name JavaNames.otherInstanceName)
          result = Utils.javaTypeToJavaResult Utils.javaIntType
      in (Utils.methodDeclaration mods [] anns JavaNames.compareToMethodName [
        param] result (Just (compareToBody aliases JavaNames.otherInstanceName fields)))

recordConstructor :: JavaEnvironment.Aliases -> Core.Name -> [Core.FieldType] -> t0 -> Graph.Graph -> Either Errors.Error Syntax.ClassBodyDeclaration
recordConstructor aliases elName fields cx g =

      let assignStmts = Lists.map (\f -> Syntax.BlockStatementStatement (Utils.toAssignStmt (Core.fieldTypeName f))) fields
      in (Eithers.bind (Eithers.mapList (\f -> fieldTypeToFormalParam aliases f cx g) fields) (\params -> Right (Utils.makeConstructor aliases elName False params assignStmts)))

recordEqualsMethod :: JavaEnvironment.Aliases -> Core.Name -> [Core.FieldType] -> Syntax.ClassBodyDeclaration
recordEqualsMethod aliases elName fields =

      let anns = [
            Utils.overrideAnnotation]
          mods = [
                Syntax.MethodModifierPublic]
          param = Utils.javaTypeToJavaFormalParameter (Utils.javaRefType [] Nothing "Object") (Core.Name JavaNames.otherInstanceName)
          result = Utils.javaTypeToJavaResult Utils.javaBooleanType
          tmpName = "o"
          instanceOfStmt =
                  Syntax.BlockStatementStatement (Syntax.StatementIfThen (Syntax.IfThenStatement {
                    Syntax.ifThenStatementExpression = (Utils.javaUnaryExpressionToJavaExpression (Syntax.UnaryExpressionOther (Syntax.UnaryExpressionNotPlusMinusNot (Utils.javaRelationalExpressionToJavaUnaryExpression (Utils.javaInstanceOf (Utils.javaIdentifierToJavaRelationalExpression (Utils.javaIdentifier JavaNames.otherInstanceName)) (Utils.nameToJavaReferenceType aliases False [] elName Nothing)))))),
                    Syntax.ifThenStatementStatement = (Utils.javaReturnStatement (Just (Utils.javaBooleanExpression False)))}))
          castStmt =
                  Utils.variableDeclarationStatement aliases (Utils.javaTypeFromTypeName aliases elName) (Utils.javaIdentifier tmpName) (Utils.javaCastExpressionToJavaExpression (Utils.javaCastExpression (Utils.nameToJavaReferenceType aliases False [] elName Nothing) (Utils.javaIdentifierToJavaUnaryExpression (Syntax.Identifier (Utils.sanitizeJavaName JavaNames.otherInstanceName)))))
          returnAllFieldsEqual =
                  Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just (Logic.ifElse (Lists.null fields) (Utils.javaBooleanExpression True) (Utils.javaConditionalAndExpressionToJavaExpression (Syntax.ConditionalAndExpression (Lists.map (\f -> eqClause tmpName f) fields))))))
      in (Utils.methodDeclaration mods [] anns JavaNames.equalsMethodName [
        param] result (Just [
        instanceOfStmt,
        castStmt,
        returnAllFieldsEqual]))

recordHashCodeMethod :: [Core.FieldType] -> Syntax.ClassBodyDeclaration
recordHashCodeMethod fields =

      let anns = [
            Utils.overrideAnnotation]
          mods = [
                Syntax.MethodModifierPublic]
          result = Utils.javaTypeToJavaResult Utils.javaIntType
          returnSum =
                  Syntax.BlockStatementStatement (Logic.ifElse (Lists.null fields) (Utils.javaReturnStatement (Just (Utils.javaIntExpression 0))) (Utils.javaReturnStatement (Just (Utils.javaAdditiveExpressionToJavaExpression (Utils.addExpressions (Lists.zipWith hashCodeMultPair first20Primes (Lists.map (\f -> Core.fieldTypeName f) fields)))))))
      in (Utils.methodDeclaration mods [] anns JavaNames.hashCodeMethodName [] result (Just [
        returnSum]))

recordMemberVar :: JavaEnvironment.Aliases -> Core.FieldType -> t0 -> Graph.Graph -> Either Errors.Error Syntax.ClassBodyDeclaration
recordMemberVar aliases ft cx g =

      let mods =
              [
                Syntax.FieldModifierPublic,
                Syntax.FieldModifierFinal]
          fname = Core.fieldTypeName ft
          ftype = Core.fieldTypeType ft
      in (Eithers.bind (encodeType aliases Sets.empty ftype cx g) (\jt -> Right (Utils.javaMemberField mods jt (Utils.fieldNameToJavaVariableDeclarator fname))))

recordWithMethod :: JavaEnvironment.Aliases -> Core.Name -> [Core.FieldType] -> Core.FieldType -> t0 -> Graph.Graph -> Either Errors.Error Syntax.ClassBodyDeclaration
recordWithMethod aliases elName fields field cx g =

      let mods = [
            Syntax.MethodModifierPublic]
          anns = []
          methodName =
                  Strings.cat2 "with" (Formatting.nonAlnumToUnderscores (Formatting.capitalize (Core.unName (Core.fieldTypeName field))))
          result = Utils.referenceTypeToResult (Utils.nameToJavaReferenceType aliases False [] elName Nothing)
          consId = Syntax.Identifier (Utils.sanitizeJavaName (Names.localNameOf elName))
          fieldArgs = Lists.map (\f -> Utils.fieldNameToJavaExpression (Core.fieldTypeName f)) fields
          returnStmt =
                  Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just (Utils.javaConstructorCall (Utils.javaConstructorName consId Nothing) fieldArgs Nothing)))
      in (Eithers.bind (fieldTypeToFormalParam aliases field cx g) (\param -> Right (Utils.methodDeclaration mods [] anns methodName [
        param] result (Just [
        returnStmt]))))

resolveTypeApps :: [Core.Name] -> [Core.Type] -> M.Map Core.Name Core.Type -> [Core.Type]
resolveTypeApps schemeVars fallbackTypeApps argSubst =

      let resolvedVars = Sets.fromList (Maps.keys argSubst)
          unresolvedVars = Lists.filter (\v -> Logic.not (Sets.member v resolvedVars)) schemeVars
          usedTypes = Sets.fromList (Maps.elems argSubst)
          unusedIrTypes = Lists.filter (\t -> Logic.not (Sets.member t usedTypes)) fallbackTypeApps
          remainingSubst = Maps.fromList (Lists.zip unresolvedVars unusedIrTypes)
          fullSubst = Maps.union argSubst remainingSubst
      in (Lists.map (\v -> Maps.findWithDefault (Core.TypeVariable v) v fullSubst) schemeVars)

selfRefSubstitution :: (Eq t0, Ord t0) => (M.Map t0 [t0] -> M.Map t0 t0)
selfRefSubstitution grouped =
    Lists.foldl (\subst -> \entry -> selfRefSubstitution_processGroup subst (Pairs.first entry) (Pairs.second entry)) Maps.empty (Maps.toList grouped)

selfRefSubstitution_processGroup :: (Eq t0, Ord t0) => (M.Map t0 t0 -> t0 -> [t0] -> M.Map t0 t0)
selfRefSubstitution_processGroup subst inVar outVars =
    Logic.ifElse (Lists.elem inVar outVars) (Lists.foldl (\s -> \v -> Logic.ifElse (Equality.equal v inVar) s (Maps.insert v inVar s)) subst outVars) subst

serializableTypes :: Bool -> [Syntax.InterfaceType]
serializableTypes isSer =

      let javaSerializableType =
              Syntax.InterfaceType (Syntax.ClassType {
                Syntax.classTypeAnnotations = [],
                Syntax.classTypeQualifier = Syntax.ClassTypeQualifierNone,
                Syntax.classTypeIdentifier = (Utils.javaTypeIdentifier "Serializable"),
                Syntax.classTypeArguments = []})
      in (Logic.ifElse isSer [
        javaSerializableType] [])

splitConstantInitializer :: Syntax.InterfaceMemberDeclaration -> [Syntax.InterfaceMemberDeclaration]
splitConstantInitializer member =
    case member of
      Syntax.InterfaceMemberDeclarationConstant v0 -> Lists.bind (Syntax.constantDeclarationVariables v0) (splitConstantInitializer_splitVar (Syntax.constantDeclarationModifiers v0) (Syntax.constantDeclarationType v0))
      _ -> [
        member]

splitConstantInitializer_splitVar :: [Syntax.ConstantModifier] -> Syntax.UnannType -> Syntax.VariableDeclarator -> [Syntax.InterfaceMemberDeclaration]
splitConstantInitializer_splitVar mods utype vd =

      let vid = Syntax.variableDeclaratorId vd
          mInit = Syntax.variableDeclaratorInitializer vd
      in (Maybes.cases mInit [
        Syntax.InterfaceMemberDeclarationConstant (Syntax.ConstantDeclaration {
          Syntax.constantDeclarationModifiers = mods,
          Syntax.constantDeclarationType = utype,
          Syntax.constantDeclarationVariables = [
            vd]})] (\init_ -> case init_ of
        Syntax.VariableInitializerExpression v0 ->
          let varName = javaIdentifierToString (Syntax.variableDeclaratorIdIdentifier vid)
              helperName = Strings.cat2 "_init_" varName
              callExpr = Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocation Nothing (Syntax.Identifier helperName) [])
              field =
                      Syntax.InterfaceMemberDeclarationConstant (Syntax.ConstantDeclaration {
                        Syntax.constantDeclarationModifiers = mods,
                        Syntax.constantDeclarationType = utype,
                        Syntax.constantDeclarationVariables = [
                          Syntax.VariableDeclarator {
                            Syntax.variableDeclaratorId = vid,
                            Syntax.variableDeclaratorInitializer = (Just (Syntax.VariableInitializerExpression callExpr))}]})
              returnSt = Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just v0))
              resultType = Syntax.ResultType utype
              helper =
                      Utils.interfaceMethodDeclaration [
                        Syntax.InterfaceMethodModifierStatic,
                        Syntax.InterfaceMethodModifierPrivate] [] helperName [] resultType (Just [
                        returnSt])
          in [
            field,
            helper]
        _ -> [
          Syntax.InterfaceMemberDeclarationConstant (Syntax.ConstantDeclaration {
            Syntax.constantDeclarationModifiers = mods,
            Syntax.constantDeclarationType = utype,
            Syntax.constantDeclarationVariables = [
              vd]})]))

stripForalls :: Core.Type -> Core.Type
stripForalls t =
    case (Strip.deannotateType t) of
      Core.TypeForall v0 -> stripForalls (Core.forallTypeBody v0)
      _ -> t

substituteTypeVarsWithTypes :: M.Map Core.Name Core.Type -> Core.Type -> Core.Type
substituteTypeVarsWithTypes subst t = substituteTypeVarsWithTypes_go subst (Strip.deannotateType t)

substituteTypeVarsWithTypes_go :: M.Map Core.Name Core.Type -> Core.Type -> Core.Type
substituteTypeVarsWithTypes_go subst t =
    case (Strip.deannotateType t) of
      Core.TypeVariable v0 -> Maybes.cases (Maps.lookup v0 subst) t (\rep -> rep)
      Core.TypeFunction v0 -> Core.TypeFunction (Core.FunctionType {
        Core.functionTypeDomain = (substituteTypeVarsWithTypes_go subst (Core.functionTypeDomain v0)),
        Core.functionTypeCodomain = (substituteTypeVarsWithTypes_go subst (Core.functionTypeCodomain v0))})
      Core.TypeApplication v0 -> Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = (substituteTypeVarsWithTypes_go subst (Core.applicationTypeFunction v0)),
        Core.applicationTypeArgument = (substituteTypeVarsWithTypes_go subst (Core.applicationTypeArgument v0))})
      Core.TypeList v0 -> Core.TypeList (substituteTypeVarsWithTypes_go subst v0)
      Core.TypeSet v0 -> Core.TypeSet (substituteTypeVarsWithTypes_go subst v0)
      Core.TypeMaybe v0 -> Core.TypeMaybe (substituteTypeVarsWithTypes_go subst v0)
      Core.TypeMap v0 -> Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = (substituteTypeVarsWithTypes_go subst (Core.mapTypeKeys v0)),
        Core.mapTypeValues = (substituteTypeVarsWithTypes_go subst (Core.mapTypeValues v0))})
      Core.TypePair v0 -> Core.TypePair (Core.PairType {
        Core.pairTypeFirst = (substituteTypeVarsWithTypes_go subst (Core.pairTypeFirst v0)),
        Core.pairTypeSecond = (substituteTypeVarsWithTypes_go subst (Core.pairTypeSecond v0))})
      Core.TypeEither v0 -> Core.TypeEither (Core.EitherType {
        Core.eitherTypeLeft = (substituteTypeVarsWithTypes_go subst (Core.eitherTypeLeft v0)),
        Core.eitherTypeRight = (substituteTypeVarsWithTypes_go subst (Core.eitherTypeRight v0))})
      Core.TypeForall v0 -> Core.TypeForall (Core.ForallType {
        Core.forallTypeParameter = (Core.forallTypeParameter v0),
        Core.forallTypeBody = (substituteTypeVarsWithTypes_go subst (Core.forallTypeBody v0))})
      _ -> t

tagCmpNotZeroExpr :: Syntax.Expression
tagCmpNotZeroExpr =
    Utils.javaEqualityExpressionToJavaExpression (Syntax.EqualityExpressionNotEqual (Syntax.EqualityExpression_Binary {
      Syntax.equalityExpression_BinaryLhs = lhs,
      Syntax.equalityExpression_BinaryRhs = rhs}))
  where
    lhs =
        Utils.javaRelationalExpressionToJavaEqualityExpression (Utils.javaPostfixExpressionToJavaRelationalExpression (Syntax.PostfixExpressionName (Syntax.ExpressionName {
          Syntax.expressionNameQualifier = Nothing,
          Syntax.expressionNameIdentifier = (Utils.javaIdentifier "tagCmp")})))
    rhs =
        Utils.javaPostfixExpressionToJavaRelationalExpression (Syntax.PostfixExpressionPrimary (Utils.javaLiteralToJavaPrimary (Utils.javaInt 0)))

tagCompareExpr :: Syntax.Expression
tagCompareExpr =
    Utils.javaMethodInvocationToJavaExpression (Syntax.MethodInvocation {
      Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
        Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantPrimary (Utils.javaMethodInvocationToJavaPrimary thisGetName)),
        Syntax.methodInvocation_ComplexTypeArguments = [],
        Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier JavaNames.compareToMethodName)})),
      Syntax.methodInvocationArguments = [
        Utils.javaMethodInvocationToJavaExpression otherGetName]})
  where
    thisGetClass =
        Syntax.MethodInvocation {
          Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
            Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantPrimary (Utils.javaExpressionToJavaPrimary Utils.javaThis)),
            Syntax.methodInvocation_ComplexTypeArguments = [],
            Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier "getClass")})),
          Syntax.methodInvocationArguments = []}
    thisGetName =
        Syntax.MethodInvocation {
          Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
            Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantPrimary (Utils.javaMethodInvocationToJavaPrimary thisGetClass)),
            Syntax.methodInvocation_ComplexTypeArguments = [],
            Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier "getName")})),
          Syntax.methodInvocationArguments = []}
    otherGetClass =
        Syntax.MethodInvocation {
          Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
            Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantExpression (Syntax.ExpressionName {
              Syntax.expressionNameQualifier = Nothing,
              Syntax.expressionNameIdentifier = (Syntax.Identifier JavaNames.otherInstanceName)})),
            Syntax.methodInvocation_ComplexTypeArguments = [],
            Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier "getClass")})),
          Syntax.methodInvocationArguments = []}
    otherGetName =
        Syntax.MethodInvocation {
          Syntax.methodInvocationHeader = (Syntax.MethodInvocation_HeaderComplex (Syntax.MethodInvocation_Complex {
            Syntax.methodInvocation_ComplexVariant = (Syntax.MethodInvocation_VariantPrimary (Utils.javaMethodInvocationToJavaPrimary otherGetClass)),
            Syntax.methodInvocation_ComplexTypeArguments = [],
            Syntax.methodInvocation_ComplexIdentifier = (Syntax.Identifier "getName")})),
          Syntax.methodInvocationArguments = []}

takeTypeArgs :: String -> Int -> [Syntax.Type] -> t0 -> t1 -> Either Errors.Error [Syntax.TypeArgument]
takeTypeArgs label n tyapps cx g =
    Logic.ifElse (Equality.lt (Lists.length tyapps) n) (Left (Errors.ErrorOther (Errors.OtherError (Strings.cat [
      "needed type arguments for ",
      label,
      ", found too few"])))) (Eithers.mapList (\jt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jt cx) (\rt -> Right (Syntax.TypeArgumentReference rt))) (Lists.take n tyapps))

toClassDecl :: Bool -> Bool -> JavaEnvironment.Aliases -> [Syntax.TypeParameter] -> Core.Name -> Core.Type -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.ClassDeclaration
toClassDecl isInner isSer aliases tparams elName t cx g =

      let wrap =
              \t_ -> declarationForRecordType isInner isSer aliases tparams elName [
                Core.FieldType {
                  Core.fieldTypeName = (Core.Name "value"),
                  Core.fieldTypeType = (Strip.deannotateType t_)}] cx g
      in case (Strip.deannotateType t) of
        Core.TypeRecord v0 -> declarationForRecordType isInner isSer aliases tparams elName v0 cx g
        Core.TypeUnion v0 -> declarationForUnionType isSer aliases tparams elName v0 cx g
        Core.TypeForall v0 ->
          let v = Core.forallTypeParameter v0
              body = Core.forallTypeBody v0
              param = Utils.javaTypeParameter (Formatting.capitalize (Core.unName v))
          in (toClassDecl False isSer aliases (Lists.concat2 tparams [
            param]) elName body cx g)
        Core.TypeWrap v0 -> declarationForRecordType isInner isSer aliases tparams elName [
          Core.FieldType {
            Core.fieldTypeName = (Core.Name "value"),
            Core.fieldTypeType = v0}] cx g
        _ -> wrap t

toDeclInit :: JavaEnvironment.Aliases -> Graph.Graph -> S.Set Core.Name -> [Core.Binding] -> Core.Name -> Context.Context -> Graph.Graph -> Either Errors.Error (Maybe Syntax.BlockStatement)
toDeclInit aliasesExt gExt recursiveVars flatBindings name cx g =
    Logic.ifElse (Sets.member name recursiveVars) (
      let binding =
              Maybes.fromMaybe (Core.Binding {
                Core.bindingName = name,
                Core.bindingTerm = Core.TermUnit,
                Core.bindingType = Nothing}) (Lists.maybeHead (Lists.filter (\b -> Equality.equal (Core.bindingName b) name) flatBindings))
          value = Core.bindingTerm binding
      in (Eithers.bind (Maybes.cases (Core.bindingType binding) (Checking.typeOfTerm cx gExt value) (\ts -> Right (Core.typeSchemeType ts))) (\typ -> Eithers.bind (encodeType aliasesExt Sets.empty typ cx g) (\jtype ->
        let id = Utils.variableToJavaIdentifier name
            arid = Syntax.Identifier "java.util.concurrent.atomic.AtomicReference"
            aid =
                    Syntax.AnnotatedIdentifier {
                      Syntax.annotatedIdentifierAnnotations = [],
                      Syntax.annotatedIdentifierIdentifier = arid}
        in (Eithers.bind (Utils.javaTypeToJavaReferenceType jtype cx) (\rt ->
          let targs = typeArgsOrDiamond [
                Syntax.TypeArgumentReference rt]
              ci =
                      Syntax.ClassOrInterfaceTypeToInstantiate {
                        Syntax.classOrInterfaceTypeToInstantiateIdentifiers = [
                          aid],
                        Syntax.classOrInterfaceTypeToInstantiateTypeArguments = (Just targs)}
              body = Utils.javaConstructorCall ci [] Nothing
              pkg =
                      JavaNames.javaPackageName [
                        "java",
                        "util",
                        "concurrent",
                        "atomic"]
              artype = Utils.javaRefType [
                    rt] (Just pkg) "AtomicReference"
          in (Right (Just (Utils.variableDeclarationStatement aliasesExt artype id body))))))))) (Right Nothing)

toDeclStatement :: JavaEnvironment.JavaEnvironment -> JavaEnvironment.Aliases -> Graph.Graph -> S.Set Core.Name -> S.Set Core.Name -> [Core.Binding] -> Core.Name -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.BlockStatement
toDeclStatement envExt aliasesExt gExt recursiveVars thunkedVars flatBindings name cx g =

      let binding =
              Maybes.fromMaybe (Core.Binding {
                Core.bindingName = name,
                Core.bindingTerm = Core.TermUnit,
                Core.bindingType = Nothing}) (Lists.maybeHead (Lists.filter (\b -> Equality.equal (Core.bindingName b) name) flatBindings))
          value = Core.bindingTerm binding
      in (Eithers.bind (Maybes.cases (Core.bindingType binding) (Checking.typeOfTerm cx gExt value) (\ts -> Right (Core.typeSchemeType ts))) (\typ -> Eithers.bind (encodeType aliasesExt Sets.empty typ cx g) (\jtype ->
        let id = Utils.variableToJavaIdentifier name
            annotatedValue = Annotations.setTermAnnotation Constants.key_type (Just (EncodeCore.type_ typ)) value
        in (Eithers.bind (encodeTerm envExt annotatedValue cx g) (\rhs -> Logic.ifElse (Sets.member name recursiveVars) (Right (Syntax.BlockStatementStatement (Utils.javaMethodInvocationToJavaStatement (Utils.methodInvocation (Just (Left (Syntax.ExpressionName {
          Syntax.expressionNameQualifier = Nothing,
          Syntax.expressionNameIdentifier = id}))) (Syntax.Identifier JavaNames.setMethodName) [
          rhs])))) (Logic.ifElse (Sets.member name thunkedVars) (Eithers.bind (Utils.javaTypeToJavaReferenceType jtype cx) (\rt ->
          let lazyType = Utils.javaRefType [
                rt] JavaNames.hydraUtilPackageName "Lazy"
              lambdaBody = Syntax.LambdaBodyExpression rhs
              supplierLambda =
                      Syntax.ExpressionLambda (Syntax.LambdaExpression {
                        Syntax.lambdaExpressionParameters = (Syntax.LambdaParametersTuple []),
                        Syntax.lambdaExpressionBody = lambdaBody})
              targs = typeArgsOrDiamond [
                    Syntax.TypeArgumentReference rt]
              lazyExpr =
                      Utils.javaConstructorCall (Utils.javaConstructorName (Syntax.Identifier "hydra.util.Lazy") (Just targs)) [
                        supplierLambda] Nothing
          in (Right (Utils.variableDeclarationStatement aliasesExt lazyType id lazyExpr)))) (Right (Utils.variableDeclarationStatement aliasesExt jtype id rhs))))))))

tryInferFunctionType :: Core.Term -> Maybe Core.Type
tryInferFunctionType funTerm =
    case (Strip.deannotateTerm funTerm) of
      Core.TermLambda v0 -> Maybes.bind (Core.lambdaDomain v0) (\dom ->
        let mCod =
                case (Core.lambdaBody v0) of
                  Core.TermAnnotated v1 -> Maybes.bind (Maps.lookup Constants.key_type (Core.annotatedTermAnnotation v1)) (\typeTerm -> decodeTypeFromTerm typeTerm)
                  Core.TermLambda _ -> tryInferFunctionType (Core.lambdaBody v0)
                  _ -> Nothing
        in (Maybes.map (\cod -> Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = dom,
          Core.functionTypeCodomain = cod})) mCod))
      _ -> Nothing

typeAppFallbackCast :: JavaEnvironment.JavaEnvironment -> JavaEnvironment.Aliases -> [M.Map Core.Name Core.Term] -> [Syntax.Type] -> Syntax.Type -> Core.Term -> Core.Type -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.Expression
typeAppFallbackCast env aliases anns tyapps jatyp body typ cx g =

      let annotatedBody = Annotations.setTermAnnotation Constants.key_type (Just (EncodeCore.type_ typ)) body
      in (Eithers.bind (encodeTermInternal env anns (Lists.cons jatyp tyapps) annotatedBody cx g) (\jbody -> Eithers.bind (encodeType aliases Sets.empty typ cx g) (\jtype -> Eithers.bind (Utils.javaTypeToJavaReferenceType jtype cx) (\rt -> Right (Utils.javaCastExpressionToJavaExpression (Utils.javaCastExpression rt (Utils.javaExpressionToJavaUnaryExpression jbody)))))))

typeAppNullaryOrHoisted :: JavaEnvironment.JavaEnvironment -> JavaEnvironment.Aliases -> [M.Map Core.Name Core.Term] -> [Syntax.Type] -> Syntax.Type -> Core.Term -> Core.Type -> Core.Name -> JavaEnvironment.JavaSymbolClass -> [Core.Type] -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.Expression
typeAppNullaryOrHoisted env aliases anns tyapps jatyp body correctedTyp varName cls allTypeArgs cx g =

      let qn = Names.qualifyName varName
          mns = Packaging.qualifiedNameNamespace qn
          localName = Packaging.qualifiedNameLocal qn
      in case cls of
        JavaEnvironment.JavaSymbolClassNullaryFunction -> Maybes.cases mns (typeAppFallbackCast env aliases anns tyapps jatyp body correctedTyp cx g) (\ns_ ->
          let classId = Utils.nameToJavaName aliases (elementsQualifiedName ns_)
              methodId = Syntax.Identifier (Utils.sanitizeJavaName localName)
          in (Eithers.bind (filterPhantomTypeArgs varName allTypeArgs cx g) (\filteredTypeArgs -> Eithers.bind (Eithers.mapList (\t -> Eithers.bind (encodeType aliases Sets.empty t cx g) (\jt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jt cx) (\rt -> Right (Syntax.TypeArgumentReference rt)))) filteredTypeArgs) (\jTypeArgs -> Right (Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs classId methodId jTypeArgs []))))))
        JavaEnvironment.JavaSymbolClassHoistedLambda v0 -> Maybes.cases mns (typeAppFallbackCast env aliases anns tyapps jatyp body correctedTyp cx g) (\ns_ ->
          let classId = Utils.nameToJavaName aliases (elementsQualifiedName ns_)
              methodId = Syntax.Identifier (Utils.sanitizeJavaName localName)
          in (Eithers.bind (filterPhantomTypeArgs varName allTypeArgs cx g) (\filteredTypeArgs -> Eithers.bind (Eithers.mapList (\t -> Eithers.bind (encodeType aliases Sets.empty t cx g) (\jt -> Eithers.bind (Utils.javaTypeToJavaReferenceType jt cx) (\rt -> Right (Syntax.TypeArgumentReference rt)))) filteredTypeArgs) (\jTypeArgs ->
            let paramNames = Lists.map (\i -> Core.Name (Strings.cat2 "p" (Literals.showInt32 i))) (Math.range 0 (Math.sub v0 1))
                paramExprs = Lists.map (\p -> Utils.javaIdentifierToJavaExpression (Utils.variableToJavaIdentifier p)) paramNames
                call =
                        Utils.javaMethodInvocationToJavaExpression (Utils.methodInvocationStaticWithTypeArgs classId methodId jTypeArgs paramExprs)
            in (Right (buildCurriedLambda paramNames call))))))
        _ -> typeAppFallbackCast env aliases anns tyapps jatyp body correctedTyp cx g

typeArgsOrDiamond :: [Syntax.TypeArgument] -> Syntax.TypeArgumentsOrDiamond
typeArgsOrDiamond args =
    Logic.ifElse (JavaEnvironment.javaFeaturesSupportsDiamondOperator javaFeatures) Syntax.TypeArgumentsOrDiamondDiamond (Syntax.TypeArgumentsOrDiamondArguments args)

typesMatch :: Core.Type -> Core.Type -> Bool
typesMatch a b =
    case a of
      Core.TypeVariable v0 -> case b of
        Core.TypeVariable v1 -> Equality.equal v0 v1
        _ -> True
      Core.TypeWrap v0 -> case b of
        Core.TypeWrap v1 -> Equality.equal v0 v1
        _ -> True
      _ -> True

unwrapReturnType :: Core.Type -> Core.Type
unwrapReturnType t =
    case (Strip.deannotateType t) of
      Core.TypeFunction v0 -> unwrapReturnType (Core.functionTypeCodomain v0)
      Core.TypeApplication v0 -> unwrapReturnType (Core.applicationTypeArgument v0)
      _ -> t

variantCompareToMethod :: JavaEnvironment.Aliases -> t0 -> Core.Name -> Core.Name -> [Core.FieldType] -> Syntax.ClassBodyDeclaration
variantCompareToMethod aliases tparams parentName variantName fields =

      let anns =
              [
                Utils.overrideAnnotation,
                Utils.suppressWarningsUncheckedAnnotation]
          mods = [
                Syntax.MethodModifierPublic]
          param =
                  Utils.javaTypeToJavaFormalParameter (Utils.javaTypeFromTypeName aliases parentName) (Core.Name JavaNames.otherInstanceName)
          result = Utils.javaTypeToJavaResult Utils.javaIntType
          varTmpName = "o"
          tagDeclStmt = Utils.variableDeclarationStatement aliases Utils.javaIntType (Utils.javaIdentifier "tagCmp") tagCompareExpr
          tagReturnStmt =
                  Syntax.BlockStatementStatement (Syntax.StatementIfThen (Syntax.IfThenStatement {
                    Syntax.ifThenStatementExpression = tagCmpNotZeroExpr,
                    Syntax.ifThenStatementStatement = (Utils.javaReturnStatement (Just (Utils.javaExpressionNameToJavaExpression (Syntax.ExpressionName {
                      Syntax.expressionNameQualifier = Nothing,
                      Syntax.expressionNameIdentifier = (Utils.javaIdentifier "tagCmp")}))))}))
          variantJavaType = Utils.javaTypeFromTypeName aliases variantName
          castOtherExpr =
                  Utils.javaCastExpressionToJavaExpression (Utils.javaCastExpression (Utils.nameToJavaReferenceType aliases False [] variantName Nothing) (Utils.javaIdentifierToJavaUnaryExpression (Syntax.Identifier JavaNames.otherInstanceName)))
          castDeclStmt = Utils.variableDeclarationStatement aliases variantJavaType (Utils.javaIdentifier varTmpName) castOtherExpr
          emptyReturn = [
                Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just (Utils.javaIntExpression 0)))]
          valueCompareStmt =
                  Logic.ifElse (Lists.null fields) emptyReturn (Lists.concat2 [
                    castDeclStmt] (compareToBody aliases varTmpName fields))
          body =
                  Lists.concat2 [
                    tagDeclStmt,
                    tagReturnStmt] valueCompareStmt
      in (Utils.methodDeclaration mods [] anns JavaNames.compareToMethodName [
        param] result (Just body))

visitBranch :: JavaEnvironment.JavaEnvironment -> JavaEnvironment.Aliases -> Core.Type -> Core.Name -> Syntax.Type -> [Syntax.TypeArgument] -> Core.Field -> Context.Context -> Graph.Graph -> Either Errors.Error Syntax.ClassBodyDeclarationWithComments
visitBranch env aliases dom tname jcod targs field cx g =

      let jdom =
              Syntax.TypeReference (Utils.nameToJavaReferenceType aliases True targs tname (Just (Formatting.capitalize (Core.unName (Core.fieldName field)))))
          mods = [
                Syntax.MethodModifierPublic]
          anns = [
                Utils.overrideAnnotation]
          result = Syntax.ResultType (Syntax.UnannType jcod)
      in case (Strip.deannotateTerm (Core.fieldTerm field)) of
        Core.TermLambda v0 -> withLambda env v0 (\env2 ->
          let lambdaParam = Core.lambdaParameter v0
              body = Core.lambdaBody v0
              env3 = insertBranchVar lambdaParam env2
          in (Eithers.bind (analyzeJavaFunction env3 body cx g) (\fs ->
            let bindings = Typing.functionStructureBindings fs
                innerBody = Typing.functionStructureBody fs
                env4 = Typing.functionStructureEnvironment fs
            in (Eithers.bind (bindingsToStatements env4 bindings cx g) (\bindResult ->
              let bindingStmts = Pairs.first bindResult
                  env5 = Pairs.second bindResult
              in (Eithers.bind (encodeTerm env5 innerBody cx g) (\jret ->
                let param = Utils.javaTypeToJavaFormalParameter jdom lambdaParam
                    returnStmt = Syntax.BlockStatementStatement (Utils.javaReturnStatement (Just jret))
                    allStmts = Lists.concat2 bindingStmts [
                          returnStmt]
                in (Right (noComment (Utils.methodDeclaration mods [] anns JavaNames.visitMethodName [
                  param] result (Just allStmts)))))))))))
        _ -> Left (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "visitBranch: field term is not a lambda: " (ShowCore.term (Core.fieldTerm field)))))

withLambda :: JavaEnvironment.JavaEnvironment -> Core.Lambda -> (JavaEnvironment.JavaEnvironment -> t0) -> t0
withLambda env lam k =
    Environment.withLambdaContext javaEnvGetGraph javaEnvSetGraph env lam (\env1 ->
      let aliases = JavaEnvironment.javaEnvironmentAliases env1
          aliases2 =
                  JavaEnvironment.Aliases {
                    JavaEnvironment.aliasesCurrentNamespace = (JavaEnvironment.aliasesCurrentNamespace aliases),
                    JavaEnvironment.aliasesPackages = (JavaEnvironment.aliasesPackages aliases),
                    JavaEnvironment.aliasesBranchVars = (JavaEnvironment.aliasesBranchVars aliases),
                    JavaEnvironment.aliasesRecursiveVars = (JavaEnvironment.aliasesRecursiveVars aliases),
                    JavaEnvironment.aliasesInScopeTypeParams = (JavaEnvironment.aliasesInScopeTypeParams aliases),
                    JavaEnvironment.aliasesPolymorphicLocals = (JavaEnvironment.aliasesPolymorphicLocals aliases),
                    JavaEnvironment.aliasesInScopeJavaVars = (JavaEnvironment.aliasesInScopeJavaVars aliases),
                    JavaEnvironment.aliasesVarRenames = (JavaEnvironment.aliasesVarRenames aliases),
                    JavaEnvironment.aliasesLambdaVars = (Sets.insert (Core.lambdaParameter lam) (JavaEnvironment.aliasesLambdaVars aliases)),
                    JavaEnvironment.aliasesTypeVarSubst = (JavaEnvironment.aliasesTypeVarSubst aliases),
                    JavaEnvironment.aliasesTrustedTypeVars = (JavaEnvironment.aliasesTrustedTypeVars aliases),
                    JavaEnvironment.aliasesMethodCodomain = (JavaEnvironment.aliasesMethodCodomain aliases),
                    JavaEnvironment.aliasesThunkedVars = (JavaEnvironment.aliasesThunkedVars aliases)}
          env2 =
                  JavaEnvironment.JavaEnvironment {
                    JavaEnvironment.javaEnvironmentAliases = aliases2,
                    JavaEnvironment.javaEnvironmentGraph = (JavaEnvironment.javaEnvironmentGraph env1)}
      in (k env2))

withTypeLambda :: JavaEnvironment.JavaEnvironment -> Core.TypeLambda -> (JavaEnvironment.JavaEnvironment -> t0) -> t0
withTypeLambda = Environment.withTypeLambdaContext javaEnvGetGraph javaEnvSetGraph

wrapInSupplierLambda :: Syntax.Expression -> Syntax.Expression
wrapInSupplierLambda expr =
    Syntax.ExpressionLambda (Syntax.LambdaExpression {
      Syntax.lambdaExpressionParameters = (Syntax.LambdaParametersTuple []),
      Syntax.lambdaExpressionBody = (Syntax.LambdaBodyExpression expr)})

wrapLazyArguments :: Core.Name -> [Syntax.Expression] -> ([Syntax.Expression], (Maybe String))
wrapLazyArguments name args =

      let dummyExpr = Utils.javaIntExpression 0
          argAt = \i -> Maybes.fromMaybe dummyExpr (Lists.maybeAt i args)
      in (Logic.ifElse (Logic.and (Equality.equal name (Core.Name "hydra.lib.logic.ifElse")) (Equality.equal (Lists.length args) 3)) ([
        argAt 0,
        (wrapInSupplierLambda (argAt 1)),
        (wrapInSupplierLambda (argAt 2))], (Just "lazy")) (Logic.ifElse (Logic.and (Equality.equal name (Core.Name "hydra.lib.maybes.maybe")) (Equality.equal (Lists.length args) 3)) ([
        wrapInSupplierLambda (argAt 0),
        (argAt 1),
        (argAt 2)], (Just "applyLazy")) (Logic.ifElse (Logic.and (Equality.equal name (Core.Name "hydra.lib.maybes.cases")) (Equality.equal (Lists.length args) 3)) ([
        argAt 0,
        (wrapInSupplierLambda (argAt 1)),
        (argAt 2)], (Just "applyLazy")) (Logic.ifElse (Logic.and (Equality.equal name (Core.Name "hydra.lib.maps.findWithDefault")) (Equality.equal (Lists.length args) 3)) ([
        wrapInSupplierLambda (argAt 0),
        (argAt 1),
        (argAt 2)], (Just "applyLazy")) (Logic.ifElse (Logic.and (Logic.or (Equality.equal name (Core.Name "hydra.lib.maybes.fromMaybe")) (Logic.or (Equality.equal name (Core.Name "hydra.lib.eithers.fromLeft")) (Equality.equal name (Core.Name "hydra.lib.eithers.fromRight")))) (Equality.equal (Lists.length args) 2)) ([
        wrapInSupplierLambda (argAt 0),
        (argAt 1)], (Just "applyLazy")) (args, Nothing))))))
