-- Note: this is an automatically generated file. Do not edit.
-- | TypeScript code generator: emits TypeScript type declarations from Hydra modules

module Hydra.TypeScript.Coder where
import qualified Hydra.Analysis as Analysis
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Environment as Environment
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
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
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Strip as Strip
import qualified Hydra.TypeScript.Language as Language
import qualified Hydra.TypeScript.Serde as Serde
import qualified Hydra.TypeScript.Syntax as Syntax
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Variables as Variables
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
collectForallParams :: Core.Type -> [Core.Name]
collectForallParams t =

      let dt = Strip.deannotateType t
      in case dt of
        Core.TypeForall v0 -> Lists.cons (Core.forallTypeParameter v0) (collectForallParams (Core.forallTypeBody v0))
        _ -> []
collectImports :: Packaging.Namespace -> Core.Type -> S.Set Core.Name
collectImports currentNs t =

      let vars = Variables.freeVariablesInType t
      in (filterNonLocalNames currentNs vars)
collectInnerTypeImports :: Packaging.Namespace -> Core.Term -> S.Set Core.Name
collectInnerTypeImports currentNs term =

      let subs = Rewriting.subterms term
          ownVars =
                  case (Strip.deannotateTerm term) of
                    Core.TermLambda v0 -> Maybes.cases (Core.lambdaDomain v0) Sets.empty (\d -> Variables.freeVariablesInType d)
                    Core.TermTypeApplication v0 -> Variables.freeVariablesInType (Core.typeApplicationTermType v0)
                    Core.TermTypeLambda _ -> Sets.empty
                    Core.TermLet v0 -> Lists.foldl (\acc -> \b -> Maybes.cases (Core.bindingTypeScheme b) acc (\ts -> Sets.union acc (Variables.freeVariablesInType (Core.typeSchemeBody ts)))) Sets.empty (Core.letBindings v0)
                    _ -> Sets.empty
          childVars = Lists.foldl (\acc -> \s -> Sets.union acc (collectInnerTypeImports currentNs s)) Sets.empty subs
      in (filterNonLocalNames currentNs (Sets.union ownVars childVars))
collectTermImports :: Packaging.Namespace -> Core.Term -> S.Set Core.Name
collectTermImports currentNs t =

      let vars = Variables.freeVariablesInTerm t
      in (filterNonLocalNames currentNs vars)
encodeBindingAsStatement :: Context.Context -> Graph.Graph -> Packaging.Namespace -> Core.Binding -> Syntax.Statement
encodeBindingAsStatement cx g currentNs b =

      let bname = Core.bindingName b
          lname = Formatting.sanitizeWithUnderscores Language.typeScriptReservedWords (Names.localNameOf bname)
          bterm = Core.bindingTerm b
          dterm = Strip.deannotateTerm bterm
      in case dterm of
        Core.TermLambda _ ->
          let innerFunDecl =
                  functionDeclarationFromTerm cx g currentNs lname bterm (Maybes.bind (Core.bindingTypeScheme b) (\ts -> Just (Core.typeSchemeBody ts)))
          in (Syntax.StatementFunctionDeclaration innerFunDecl)
        _ ->
          let expr = encodeTerm cx g currentNs bterm
              declarator =
                      Syntax.VariableDeclarator {
                        Syntax.variableDeclaratorId = (Syntax.PatternIdentifier (tsIdent lname)),
                        Syntax.variableDeclaratorInit = (Just expr)}
              varDecl =
                      Syntax.VariableDeclaration {
                        Syntax.variableDeclarationKind = Syntax.VariableKindConst,
                        Syntax.variableDeclarationDeclarations = [
                          declarator]}
          in (Syntax.StatementVariableDeclaration varDecl)
encodeLazyCall :: Context.Context -> Graph.Graph -> Packaging.Namespace -> Core.Term -> [Core.Term] -> [Bool] -> Syntax.Expression
encodeLazyCall cx g currentNs headTerm args lazyFlags =

      let headExpr = encodeTerm cx g currentNs headTerm
          paired = Lists.zip args lazyFlags
          renderArg =
                  \p ->
                    let argTerm = Pairs.first p
                        isLazy = Pairs.second p
                        expr = encodeTerm cx g currentNs argTerm
                    in (Logic.ifElse isLazy (tsArrow [] expr) expr)
          argExprs = Lists.map renderArg paired
      in (tsCall headExpr argExprs)
encodeLiteral :: Core.Literal -> Syntax.Expression
encodeLiteral lit =

      let litExpr = \lit -> Syntax.ExpressionLiteral lit
          numLit = \i -> litExpr (Syntax.LiteralNumber (Syntax.NumericLiteralInteger i))
          floatLit = \f -> litExpr (Syntax.LiteralNumber (Syntax.NumericLiteralFloat f))
          strLit =
                  \s -> litExpr (Syntax.LiteralString (Syntax.StringLiteral {
                    Syntax.stringLiteralValue = s,
                    Syntax.stringLiteralSingleQuote = False}))
          boolLit = \b -> litExpr (Syntax.LiteralBoolean b)
          bigIntCall = \txt -> tsCall (tsExprIdent "BigInt") [
                strLit txt]
      in case lit of
        Core.LiteralBinary v0 -> strLit (Literals.binaryToString v0)
        Core.LiteralBoolean v0 -> boolLit v0
        Core.LiteralDecimal v0 -> numLit (Literals.bigintToInt64 (Literals.decimalToBigint v0))
        Core.LiteralString v0 -> strLit v0
        Core.LiteralInteger v0 -> case v0 of
          Core.IntegerValueBigint v1 -> bigIntCall (Literals.showBigint v1)
          Core.IntegerValueInt8 v1 -> numLit (Literals.bigintToInt64 (Literals.int8ToBigint v1))
          Core.IntegerValueInt16 v1 -> numLit (Literals.bigintToInt64 (Literals.int16ToBigint v1))
          Core.IntegerValueInt32 v1 -> numLit (Literals.bigintToInt64 (Literals.int32ToBigint v1))
          Core.IntegerValueInt64 v1 -> bigIntCall (Literals.showInt64 v1)
          Core.IntegerValueUint8 v1 -> numLit (Literals.bigintToInt64 (Literals.uint8ToBigint v1))
          Core.IntegerValueUint16 v1 -> numLit (Literals.bigintToInt64 (Literals.uint16ToBigint v1))
          Core.IntegerValueUint32 v1 -> numLit (Literals.bigintToInt64 (Literals.uint32ToBigint v1))
          Core.IntegerValueUint64 v1 -> bigIntCall (Literals.showUint64 v1)
          _ -> numLit (Literals.bigintToInt64 (Literals.int32ToBigint 0))
        Core.LiteralFloat v0 -> case v0 of
          Core.FloatValueFloat32 v1 -> floatLit (Literals.float32ToFloat64 v1)
          Core.FloatValueFloat64 v1 -> floatLit v1
          _ -> floatLit 0.0
        _ -> litExpr Syntax.LiteralNull
encodeLiteralType :: Core.LiteralType -> Syntax.TypeExpression
encodeLiteralType lt =
    case lt of
      Core.LiteralTypeBinary -> tsParamApp1 "ReadonlyArray" (tsNamedType "number")
      Core.LiteralTypeBoolean -> tsNamedType "boolean"
      Core.LiteralTypeDecimal -> tsNamedType "number"
      Core.LiteralTypeFloat v0 -> case v0 of
        Core.FloatTypeFloat32 -> tsNamedType "number"
        Core.FloatTypeFloat64 -> tsNamedType "number"
      Core.LiteralTypeInteger v0 -> case v0 of
        Core.IntegerTypeBigint -> tsNamedType "bigint"
        Core.IntegerTypeInt8 -> tsNamedType "number"
        Core.IntegerTypeInt16 -> tsNamedType "number"
        Core.IntegerTypeInt32 -> tsNamedType "number"
        Core.IntegerTypeInt64 -> tsNamedType "bigint"
        Core.IntegerTypeUint8 -> tsNamedType "number"
        Core.IntegerTypeUint16 -> tsNamedType "number"
        Core.IntegerTypeUint32 -> tsNamedType "number"
        Core.IntegerTypeUint64 -> tsNamedType "bigint"
      Core.LiteralTypeString -> tsNamedType "string"
encodeParam :: t0 -> t1 -> Core.Name -> Core.Type -> Syntax.Pattern
encodeParam cx g pname dom =

      let nstr = sanitizeParamName pname
      in case (Strip.deannotateType dom) of
        Core.TypeVariable _ -> tsTypedIdent nstr Syntax.TypeExpressionAny
        _ -> tsTypedIdent nstr (encodeTypeOrAny cx g dom)
encodeTerm :: Context.Context -> Graph.Graph -> Packaging.Namespace -> Core.Term -> Syntax.Expression
encodeTerm cx g currentNs term =
    case term of
      Core.TermAnnotated v0 -> encodeTerm cx g currentNs (Core.annotatedTermBody v0)
      Core.TermLiteral v0 -> encodeLiteral v0
      Core.TermVariable v0 ->
        let local = Formatting.sanitizeWithUnderscores Language.typeScriptReservedWords (Names.localNameOf v0)
        in (Maybes.cases (Names.namespaceOf v0) (tsExprIdent local) (\ns -> Logic.ifElse (Equality.equal (Packaging.unNamespace currentNs) (Packaging.unNamespace ns)) (tsExprIdent local) (
          let nsSegs = Lists.drop 1 (Strings.splitOn "." (Packaging.unNamespace ns))
              alias = Strings.cat2 "$mod_" (Strings.intercalate "_" nsSegs)
          in (tsMember (tsExprIdent alias) local))))
      Core.TermLambda v0 ->
        let lamTerm = Core.TermLambda v0
            fsLE = Analysis.analyzeFunctionTerm cx (\e -> e) (\newG -> \_old -> newG) g lamTerm
            fsL =
                    Eithers.either (\_err -> Typing.FunctionStructure {
                      Typing.functionStructureTypeParams = [],
                      Typing.functionStructureParams = [],
                      Typing.functionStructureBindings = [],
                      Typing.functionStructureBody = lamTerm,
                      Typing.functionStructureDomains = [],
                      Typing.functionStructureCodomain = Nothing,
                      Typing.functionStructureEnvironment = g}) (\ok -> ok) fsLE
            fsLParams = Typing.functionStructureParams fsL
            fsLDoms = Typing.functionStructureDomains fsL
            fsLBindings = Typing.functionStructureBindings fsL
            fsLBody = Typing.functionStructureBody fsL
            fsLEnv = Typing.functionStructureEnvironment fsL
            innerBody =
                    Logic.ifElse (Lists.null fsLBindings) fsLBody (Core.TermLet (Core.Let {
                      Core.letBindings = fsLBindings,
                      Core.letBody = fsLBody}))
            paramPatterns =
                    Lists.map (\pn -> tsTypedIdent (Formatting.sanitizeWithUnderscores Language.typeScriptReservedWords (Names.localNameOf pn)) Syntax.TypeExpressionAny) fsLParams
            bExpr = encodeTerm cx fsLEnv currentNs innerBody
        in (tsArrowTyped paramPatterns bExpr)
      Core.TermApplication v0 ->
        let asTerm = Core.TermApplication v0
            flat = flattenApplication asTerm
            headTerm = Pairs.first flat
            args = Pairs.second flat
            mName = termHeadVariable headTerm
            argc = Lists.length args
            lazyMaybe =
                    Maybes.cases mName Nothing (\n ->
                      let qn = Core.unName n
                      in (Logic.ifElse (Logic.and (Equality.equal qn "hydra.lib.logic.ifElse") (Equality.equal argc 3)) (Just (encodeLazyCall cx g currentNs headTerm args [
                        False,
                        True,
                        True])) (Logic.ifElse (Logic.and (Equality.equal qn "hydra.lib.maybes.cases") (Equality.equal argc 3)) (Just (encodeLazyCall cx g currentNs headTerm args [
                        False,
                        True,
                        False])) (Logic.ifElse (Logic.and (Equality.equal qn "hydra.lib.maybes.maybe") (Equality.equal argc 3)) (Just (encodeLazyCall cx g currentNs headTerm args [
                        True,
                        False,
                        False])) (Logic.ifElse (Logic.and (Equality.equal qn "hydra.lib.maybes.fromMaybe") (Equality.equal argc 2)) (Just (encodeLazyCall cx g currentNs headTerm args [
                        True,
                        False])) (Logic.ifElse (Logic.and (Logic.or (Equality.equal qn "hydra.lib.eithers.fromLeft") (Equality.equal qn "hydra.lib.eithers.fromRight")) (Equality.equal argc 2)) (Just (encodeLazyCall cx g currentNs headTerm args [
                        True,
                        False])) (Logic.ifElse (Logic.and (Equality.equal qn "hydra.lib.maps.findWithDefault") (Equality.equal argc 3)) (Just (encodeLazyCall cx g currentNs headTerm args [
                        True,
                        False,
                        False])) Nothing)))))))
        in (Maybes.cases lazyMaybe (
          let dHead = Strip.deannotateAndDetypeTerm headTerm
              encArgs = Lists.map (encodeTerm cx g currentNs) args
          in case dHead of
            Core.TermProject v1 -> Logic.ifElse (Lists.null encArgs) (
              let headExpr = encodeTerm cx g currentNs headTerm
              in headExpr) (
              let fname = Formatting.sanitizeWithUnderscores Language.typeScriptReservedWords (Core.unName (Core.projectionField v1))
                  firstA = Maybes.fromMaybe (tsExprIdent "undefined") (Lists.maybeHead encArgs)
                  restA = Lists.drop 1 encArgs
                  fieldExpr = tsMember firstA fname
              in (Logic.ifElse (Lists.null restA) fieldExpr (tsCall fieldExpr restA)))
            Core.TermUnwrap _ -> Logic.ifElse (Lists.null encArgs) (
              let headExpr = encodeTerm cx g currentNs headTerm
              in headExpr) (
              let firstA = Maybes.fromMaybe (tsExprIdent "undefined") (Lists.maybeHead encArgs)
                  restA = Lists.drop 1 encArgs
                  valueExpr = tsMember firstA "value"
              in (Logic.ifElse (Lists.null restA) valueExpr (tsCall valueExpr restA)))
            _ ->
              let headExpr = encodeTerm cx g currentNs headTerm
              in (tsCall headExpr encArgs)) (\e -> e))
      Core.TermUnit -> tsUndefined
      Core.TermList v0 -> tsArray (Lists.map (encodeTerm cx g currentNs) v0)
      Core.TermSet v0 -> tsNew (tsExprIdent "Set") [
        tsArray (Lists.map (encodeTerm cx g currentNs) (Sets.toList v0))]
      Core.TermMap v0 -> tsNew (tsExprIdent "Map") [
        tsArray (Lists.map (\entry -> tsArray [
          encodeTerm cx g currentNs (Pairs.first entry),
          (encodeTerm cx g currentNs (Pairs.second entry))]) (Maps.toList v0))]
      Core.TermPair v0 -> tsAsAny (tsArray [
        encodeTerm cx g currentNs (Pairs.first v0),
        (encodeTerm cx g currentNs (Pairs.second v0))])
      Core.TermMaybe v0 -> Maybes.cases v0 (tsAsAny (tsObject [
        ("tag", (tsExprStr "nothing"))])) (\v -> tsAsAny (tsObject [
        ("tag", (tsExprStr "just")),
        ("value", (encodeTerm cx g currentNs v))]))
      Core.TermRecord v0 ->
        let fields = Core.recordFields v0
        in (tsObject (Lists.map (\f -> (
          Formatting.sanitizeWithUnderscores Language.typeScriptReservedWords (Core.unName (Core.fieldName f)),
          (encodeTerm cx g currentNs (Core.fieldTerm f)))) fields))
      Core.TermInject v0 ->
        let fname = Core.unName (Core.fieldName (Core.injectionField v0))
            fterm = Core.fieldTerm (Core.injectionField v0)
            isUnit =
                    case (Strip.deannotateTerm fterm) of
                      Core.TermUnit -> True
                      _ -> False
        in (Logic.ifElse isUnit (tsAsAny (tsObject [
          ("tag", (tsExprStr fname))])) (tsAsAny (tsObject [
          ("tag", (tsExprStr fname)),
          ("value", (encodeTerm cx g currentNs fterm))])))
      Core.TermWrap v0 -> tsObject [
        ("value", (encodeTerm cx g currentNs (Core.wrappedTermBody v0)))]
      Core.TermLet v0 ->
        let bindings = Core.letBindings v0
            body = Core.letBody v0
            encodedBody = encodeTerm cx g currentNs body
            bindingStmts = Lists.map (\b -> encodeBindingAsStatement cx g currentNs b) bindings
            returnStmt = Syntax.StatementReturn (Just encodedBody)
            stmts = Lists.concat2 bindingStmts [
                  returnStmt]
            iifeArrow =
                    Syntax.ExpressionArrow (Syntax.ArrowFunctionExpression {
                      Syntax.arrowFunctionExpressionParams = [],
                      Syntax.arrowFunctionExpressionBody = (Syntax.ArrowFunctionBodyBlock stmts),
                      Syntax.arrowFunctionExpressionAsync = False})
        in (tsCall iifeArrow [])
      Core.TermTypeApplication v0 -> encodeTerm cx g currentNs (Core.typeApplicationTermBody v0)
      Core.TermTypeLambda v0 -> encodeTerm cx g currentNs (Core.typeLambdaBody v0)
      Core.TermProject v0 ->
        let fname = Formatting.sanitizeWithUnderscores Language.typeScriptReservedWords (Core.unName (Core.projectionField v0))
        in (tsArrowTyped [
          tsTypedIdent "x" Syntax.TypeExpressionAny] (tsMember (tsExprIdent "x") fname))
      Core.TermUnwrap _ -> tsArrowTyped [
        tsTypedIdent "x" Syntax.TypeExpressionAny] (tsMember (tsExprIdent "x") "value")
      Core.TermCases v0 ->
        let armFields = Core.caseStatementCases v0
            defaultMaybe = Core.caseStatementDefault v0
            tail =
                    Maybes.cases defaultMaybe (tsCall (tsArrow [] (tsCall (tsExprIdent "(() => { throw new Error('unmatched case'); })") [])) []) (\dt -> encodeTerm cx g currentNs dt)
            uVar = "u"
            uExpr = tsAsAny (tsExprIdent uVar)
            uTag = tsMember uExpr "tag"
            uValue = tsMember uExpr "value"
            reversedArms = Lists.reverse armFields
            body =
                    Lists.foldl (\acc -> \f ->
                      let fname = Core.unName (Core.fieldName f)
                          armExpr = encodeTerm cx g currentNs (Core.fieldTerm f)
                      in (tsCond (Syntax.ExpressionBinary (Syntax.BinaryExpression {
                        Syntax.binaryExpressionOperator = Syntax.BinaryOperatorStrictEqual,
                        Syntax.binaryExpressionLeft = uTag,
                        Syntax.binaryExpressionRight = (tsExprStr fname)})) (tsCall armExpr [
                        uValue]) acc)) tail reversedArms
        in (tsArrow [
          uVar] body)
      Core.TermEither v0 -> Eithers.either (\l -> tsAsAny (tsObject [
        ("tag", (tsExprStr "left")),
        ("value", (encodeTerm cx g currentNs l))])) (\r -> tsAsAny (tsObject [
        ("tag", (tsExprStr "right")),
        ("value", (encodeTerm cx g currentNs r))])) v0
      _ -> tsExprIdent "null"
encodeTermDefinition :: Context.Context -> Graph.Graph -> Packaging.Namespace -> Packaging.TermDefinition -> Syntax.ModuleItem
encodeTermDefinition cx g currentNs td =

      let name = Packaging.termDefinitionName td
          lname = Formatting.sanitizeWithUnderscores Language.typeScriptReservedWords (Names.localNameOf name)
          rawTerm = Packaging.termDefinitionTerm td
          asExport = \stmt -> Syntax.ModuleItemExport (Syntax.ExportDeclarationDeclaration stmt)
          mScheme = Maybes.bind (Packaging.termDefinitionTypeScheme td) (\ts -> Just (Core.typeSchemeBody ts))
          dterm = Strip.deannotateTerm rawTerm
          funDecl = functionDeclarationFromTerm cx g currentNs lname rawTerm mScheme
          asFunDecl = asExport (Syntax.StatementFunctionDeclaration funDecl)
      in case dterm of
        Core.TermLambda _ -> asFunDecl
        Core.TermTypeLambda _ -> asFunDecl
        _ ->
          let expr = encodeTerm cx g currentNs rawTerm
              declarator =
                      Syntax.VariableDeclarator {
                        Syntax.variableDeclaratorId = (Syntax.PatternIdentifier (tsIdent lname)),
                        Syntax.variableDeclaratorInit = (Just expr)}
              varDecl =
                      Syntax.VariableDeclaration {
                        Syntax.variableDeclarationKind = Syntax.VariableKindConst,
                        Syntax.variableDeclarationDeclarations = [
                          declarator]}
          in (asExport (Syntax.StatementVariableDeclaration varDecl))
encodeType :: t0 -> t1 -> Core.Type -> Either t2 Syntax.TypeExpression
encodeType cx g t =

      let typ = Strip.deannotateType t
      in case typ of
        Core.TypeAnnotated v0 -> encodeType cx g (Core.annotatedTypeBody v0)
        Core.TypeApplication v0 ->
          let fnTyp = Core.applicationTypeFunction v0
              argTyp = Core.applicationTypeArgument v0
          in (Eithers.bind (encodeType cx g fnTyp) (\encFn -> Eithers.bind (encodeType cx g argTyp) (\encArg -> case encFn of
            Syntax.TypeExpressionIdentifier _ -> Right (Syntax.TypeExpressionParameterized (Syntax.ParameterizedTypeExpression {
              Syntax.parameterizedTypeExpressionBase = encFn,
              Syntax.parameterizedTypeExpressionArguments = [
                encArg]}))
            Syntax.TypeExpressionParameterized v1 -> Right (Syntax.TypeExpressionParameterized (Syntax.ParameterizedTypeExpression {
              Syntax.parameterizedTypeExpressionBase = (Syntax.parameterizedTypeExpressionBase v1),
              Syntax.parameterizedTypeExpressionArguments = (Lists.concat2 (Syntax.parameterizedTypeExpressionArguments v1) [
                encArg])}))
            _ -> Right encFn)))
        Core.TypeForall v0 -> encodeType cx g (Core.forallTypeBody v0)
        Core.TypeUnit -> Right Syntax.TypeExpressionVoid
        Core.TypeVoid -> Right Syntax.TypeExpressionNever
        Core.TypeLiteral v0 -> Right (encodeLiteralType v0)
        Core.TypeList v0 -> Eithers.map (\enc -> tsParamApp1 "ReadonlyArray" enc) (encodeType cx g v0)
        Core.TypeSet v0 -> Eithers.map tsReadonlySet (encodeType cx g v0)
        Core.TypeMap v0 -> Eithers.bind (encodeType cx g (Core.mapTypeKeys v0)) (\kt -> Eithers.bind (encodeType cx g (Core.mapTypeValues v0)) (\vt -> Right (tsReadonlyMap kt vt)))
        Core.TypeMaybe v0 -> Eithers.map (\enc -> Syntax.TypeExpressionUnion [
          Syntax.TypeExpressionObject [
            tsPropSig "tag" False (Syntax.TypeExpressionLiteral (Syntax.LiteralString (Syntax.StringLiteral {
              Syntax.stringLiteralValue = "just",
              Syntax.stringLiteralSingleQuote = False}))),
            (tsPropSig "value" False enc)],
          (Syntax.TypeExpressionObject [
            tsPropSig "tag" False (Syntax.TypeExpressionLiteral (Syntax.LiteralString (Syntax.StringLiteral {
              Syntax.stringLiteralValue = "nothing",
              Syntax.stringLiteralSingleQuote = False})))])]) (encodeType cx g v0)
        Core.TypeEither v0 -> Eithers.bind (encodeType cx g (Core.eitherTypeLeft v0)) (\lt -> Eithers.bind (encodeType cx g (Core.eitherTypeRight v0)) (\rt ->
          let leftArm =
                  Syntax.TypeExpressionObject [
                    tsPropSig "tag" False (Syntax.TypeExpressionLiteral (Syntax.LiteralString (Syntax.StringLiteral {
                      Syntax.stringLiteralValue = "left",
                      Syntax.stringLiteralSingleQuote = False}))),
                    (tsPropSig "value" False lt)]
              rightArm =
                      Syntax.TypeExpressionObject [
                        tsPropSig "tag" False (Syntax.TypeExpressionLiteral (Syntax.LiteralString (Syntax.StringLiteral {
                          Syntax.stringLiteralValue = "right",
                          Syntax.stringLiteralSingleQuote = False}))),
                        (tsPropSig "value" False rt)]
          in (Right (Syntax.TypeExpressionUnion [
            leftArm,
            rightArm]))))
        Core.TypePair v0 -> Eithers.bind (encodeType cx g (Core.pairTypeFirst v0)) (\ft -> Eithers.bind (encodeType cx g (Core.pairTypeSecond v0)) (\st -> Right (tsTuple [
          ft,
          st])))
        Core.TypeFunction v0 -> Eithers.bind (encodeType cx g (Core.functionTypeDomain v0)) (\dom -> Eithers.bind (encodeType cx g (Core.functionTypeCodomain v0)) (\cod -> Right (Syntax.TypeExpressionFunction (Syntax.FunctionTypeExpression {
          Syntax.functionTypeExpressionTypeParameters = [],
          Syntax.functionTypeExpressionParameters = [
            dom],
          Syntax.functionTypeExpressionReturnType = cod}))))
        Core.TypeVariable v0 -> Right (tsNamedType (Formatting.capitalize (Names.localNameOf v0)))
        Core.TypeWrap v0 -> encodeType cx g v0
        Core.TypeRecord v0 -> Eithers.bind (Eithers.mapList (\ft ->
          let fname = Core.unName (Core.fieldTypeName ft)
              ftyp = Core.fieldTypeType ft
          in (Eithers.bind (encodeType cx g ftyp) (\sftyp -> Right (tsPropSig fname False sftyp)))) v0) (\members -> Right (Syntax.TypeExpressionObject members))
        Core.TypeUnion v0 -> Eithers.bind (Eithers.mapList (\ft ->
          let fname = Core.unName (Core.fieldTypeName ft)
              ftyp = Core.fieldTypeType ft
          in (Eithers.bind (encodeType cx g ftyp) (\sftyp -> Right (Syntax.TypeExpressionObject [
            tsPropSig "tag" False (Syntax.TypeExpressionLiteral (Syntax.LiteralString (Syntax.StringLiteral {
              Syntax.stringLiteralValue = fname,
              Syntax.stringLiteralSingleQuote = False}))),
            (tsPropSig "value" False sftyp)])))) v0) (\arms -> Right (Syntax.TypeExpressionUnion arms))
encodeTypeDefinition :: t0 -> t1 -> Packaging.TypeDefinition -> Either t2 Syntax.ModuleItem
encodeTypeDefinition cx g tdef =

      let name = Packaging.typeDefinitionName tdef
          typScheme = Packaging.typeDefinitionTypeScheme tdef
          rawTyp = Core.typeSchemeBody typScheme
          lname = Formatting.capitalize (Names.localNameOf name)
          forallParams = collectForallParams rawTyp
          typ = stripForalls rawTyp
          typeParams = Lists.map (\v -> tsParam (Formatting.capitalize (Core.unName v))) forallParams
          dtyp = Strip.deannotateType typ
      in case dtyp of
        Core.TypeRecord v0 -> Eithers.bind (Eithers.mapList (\ft ->
          let fname = Core.unName (Core.fieldTypeName ft)
              ftyp = Core.fieldTypeType ft
          in (Eithers.bind (encodeType cx g ftyp) (\sftyp -> Right (tsPropSig fname False sftyp)))) v0) (\members -> Right (Syntax.ModuleItemInterface (Syntax.InterfaceDeclaration {
          Syntax.interfaceDeclarationName = (tsIdent lname),
          Syntax.interfaceDeclarationTypeParameters = typeParams,
          Syntax.interfaceDeclarationExtends = [],
          Syntax.interfaceDeclarationMembers = members})))
        Core.TypeUnion v0 -> Eithers.bind (Eithers.mapList (\ft ->
          let fname = Core.unName (Core.fieldTypeName ft)
              ftyp = Core.fieldTypeType ft
              dtyp2 = Strip.deannotateType ftyp
          in case dtyp2 of
            Core.TypeUnit -> Right (Syntax.TypeExpressionObject [
              tsPropSig "tag" False (Syntax.TypeExpressionLiteral (Syntax.LiteralString (Syntax.StringLiteral {
                Syntax.stringLiteralValue = fname,
                Syntax.stringLiteralSingleQuote = False})))])
            _ -> Eithers.bind (encodeType cx g ftyp) (\sftyp -> Right (Syntax.TypeExpressionObject [
              tsPropSig "tag" False (Syntax.TypeExpressionLiteral (Syntax.LiteralString (Syntax.StringLiteral {
                Syntax.stringLiteralValue = fname,
                Syntax.stringLiteralSingleQuote = False}))),
              (tsPropSig "value" False sftyp)]))) v0) (\arms -> Right (Syntax.ModuleItemTypeAlias (Syntax.TypeAliasDeclaration {
          Syntax.typeAliasDeclarationName = (tsIdent lname),
          Syntax.typeAliasDeclarationTypeParameters = typeParams,
          Syntax.typeAliasDeclarationType = (Syntax.TypeExpressionUnion arms)})))
        Core.TypeWrap v0 -> Eithers.bind (encodeType cx g v0) (\sftyp -> Right (Syntax.ModuleItemInterface (Syntax.InterfaceDeclaration {
          Syntax.interfaceDeclarationName = (tsIdent lname),
          Syntax.interfaceDeclarationTypeParameters = typeParams,
          Syntax.interfaceDeclarationExtends = [],
          Syntax.interfaceDeclarationMembers = [
            tsPropSig "value" False sftyp]})))
        _ -> Eithers.bind (encodeType cx g typ) (\styp -> Right (Syntax.ModuleItemTypeAlias (Syntax.TypeAliasDeclaration {
          Syntax.typeAliasDeclarationName = (tsIdent lname),
          Syntax.typeAliasDeclarationTypeParameters = typeParams,
          Syntax.typeAliasDeclarationType = styp})))
encodeTypeOrAny :: t0 -> t1 -> Core.Type -> Syntax.TypeExpression
encodeTypeOrAny cx g typ = Eithers.either (\_e -> Syntax.TypeExpressionAny) (\te -> te) (encodeType cx g typ)
filterNonLocalNames :: Packaging.Namespace -> S.Set Core.Name -> S.Set Core.Name
filterNonLocalNames currentNs names =
    Sets.fromList (Maybes.cat (Lists.map (\n -> Maybes.cases (Names.namespaceOf n) Nothing (\nameNs -> Logic.ifElse (Equality.equal (Packaging.unNamespace currentNs) (Packaging.unNamespace nameNs)) Nothing (Just n))) (Sets.toList names)))
flattenApplication :: Core.Term -> (Core.Term, [Core.Term])
flattenApplication t =

      let dt = Strip.deannotateTerm t
      in case dt of
        Core.TermApplication v0 ->
          let inner = flattenApplication (Core.applicationFunction v0)
              head_ = Pairs.first inner
              prevArgs = Pairs.second inner
          in (head_, (Lists.concat2 prevArgs (Lists.singleton (Core.applicationArgument v0))))
        _ -> (t, [])
functionDeclarationFromTerm :: Context.Context -> Graph.Graph -> Packaging.Namespace -> String -> Core.Term -> Maybe Core.Type -> Syntax.FunctionDeclaration
functionDeclarationFromTerm cx g currentNs lname term _mScheme =

      let fsE = Analysis.analyzeFunctionTerm cx (\e -> e) (\newG -> \_old -> newG) g term
          fs =
                  Eithers.either (\_err -> Typing.FunctionStructure {
                    Typing.functionStructureTypeParams = [],
                    Typing.functionStructureParams = [],
                    Typing.functionStructureBindings = [],
                    Typing.functionStructureBody = term,
                    Typing.functionStructureDomains = [],
                    Typing.functionStructureCodomain = Nothing,
                    Typing.functionStructureEnvironment = g}) (\ok -> ok) fsE
          fsParams = Typing.functionStructureParams fs
          fsDoms = Typing.functionStructureDomains fs
          fsBindings = Typing.functionStructureBindings fs
          fsBody = Typing.functionStructureBody fs
          fsEnv = Typing.functionStructureEnvironment fs
          domPad = Core.TypeVariable (Core.Name "_")
          fsDomsPadded = Lists.concat2 fsDoms (Lists.replicate (Math.sub (Lists.length fsParams) (Lists.length fsDoms)) domPad)
          paramPatterns =
                  Lists.map (\pair -> encodeParam cx fsEnv (Pairs.first pair) (Pairs.second pair)) (Lists.zip fsParams fsDomsPadded)
          bindingStmts = Lists.map (\b -> encodeBindingAsStatement cx fsEnv currentNs b) fsBindings
          bodyExpr = encodeTerm cx fsEnv currentNs fsBody
          returnStmt = Syntax.StatementReturn (Just bodyExpr)
          block = Lists.concat2 bindingStmts [
                returnStmt]
      in Syntax.FunctionDeclaration {
        Syntax.functionDeclarationId = (tsIdent lname),
        Syntax.functionDeclarationParams = paramPatterns,
        Syntax.functionDeclarationBody = block,
        Syntax.functionDeclarationAsync = False,
        Syntax.functionDeclarationGenerator = False}
importsToText :: String -> Packaging.Namespace -> S.Set Core.Name -> String
importsToText kind currentNs names =

      let pairs =
              Maybes.cat (Lists.map (\n -> Maybes.cases (Names.namespaceOf n) Nothing (\ns -> Logic.ifElse (Equality.equal (Packaging.unNamespace currentNs) (Packaging.unNamespace ns)) Nothing (Just (ns, n)))) (Sets.toList names))
          transformLocal =
                  \s -> Logic.ifElse (Equality.equal kind "type") (Formatting.capitalize s) (Formatting.sanitizeWithUnderscores Language.typeScriptReservedWords s)
          importKeyword = Logic.ifElse (Equality.equal kind "type") "import type" "import"
          grouped =
                  Lists.foldl (\acc -> \p ->
                    let ns = Pairs.first p
                        n = Pairs.second p
                        local = transformLocal (Names.localNameOf n)
                        existing = Maybes.fromMaybe [] (Maps.lookup ns acc)
                    in (Maps.insert ns (Lists.cons local existing) acc)) Maps.empty pairs
          currentSegs = Lists.drop 1 (Strings.splitOn "." (Packaging.unNamespace currentNs))
          currentDepth = Lists.length currentSegs
          currentIsTest =
                  Logic.and (Logic.not (Lists.null currentSegs)) (Equality.equal (Maybes.fromMaybe "" (Lists.maybeHead currentSegs)) "test")
          baseUpPrefix = Logic.ifElse (Equality.equal currentDepth 1) "./" (Strings.cat (Lists.replicate (Math.sub currentDepth 1) "../"))
          lines =
                  Lists.map (\entry ->
                    let ns = Pairs.first entry
                        locals = Pairs.second entry
                        targetSegs = Lists.drop 1 (Strings.splitOn "." (Packaging.unNamespace ns))
                        targetIsTest =
                                Logic.and (Logic.not (Lists.null targetSegs)) (Equality.equal (Maybes.fromMaybe "" (Lists.maybeHead targetSegs)) "test")
                        targetPath = Strings.intercalate "/" targetSegs
                        upPrefix =
                                Logic.ifElse (Logic.and currentIsTest (Logic.not targetIsTest)) (Strings.cat2 baseUpPrefix "../../../main/typescript/hydra/") baseUpPrefix
                        moduleAlias = Strings.cat2 "$mod_" (Strings.intercalate "_" targetSegs)
                    in (Logic.ifElse (Equality.equal kind "type") (Strings.cat [
                      importKeyword,
                      " { ",
                      (Strings.intercalate ", " locals),
                      " } from \"",
                      upPrefix,
                      targetPath,
                      ".js\";\n"]) (Strings.cat [
                      "import * as ",
                      moduleAlias,
                      " from \"",
                      upPrefix,
                      targetPath,
                      ".js\";\n"]))) (Maps.toList grouped)
      in (Strings.cat lines)
moduleToTypeScript :: Packaging.Module -> [Packaging.Definition] -> Context.Context -> Graph.Graph -> Either t0 (M.Map String String)
moduleToTypeScript mod defs cx g =

      let currentNs = Packaging.moduleNamespace mod
          partitioned = Environment.partitionDefinitions defs
          typeDefs = Pairs.first partitioned
          rawTermDefs = Pairs.second partitioned
          termDefs = sortTermDefsTopologically currentNs rawTermDefs
          typeImportsFromTypes =
                  Lists.foldl (\acc -> \td -> Sets.union acc (collectImports currentNs (Core.typeSchemeBody (Packaging.typeDefinitionTypeScheme td)))) Sets.empty typeDefs
          typeImportsFromTerms =
                  Lists.foldl (\acc -> \td -> Maybes.cases (Packaging.termDefinitionTypeScheme td) acc (\ts -> Sets.union acc (collectImports currentNs (Core.typeSchemeBody ts)))) Sets.empty termDefs
          typeImportsFromInner =
                  Lists.foldl (\acc -> \td -> Sets.union acc (collectInnerTypeImports currentNs (Packaging.termDefinitionTerm td))) Sets.empty termDefs
          typeImports = Sets.union (Sets.union typeImportsFromTypes typeImportsFromTerms) typeImportsFromInner
          termImports =
                  Lists.foldl (\acc -> \td -> Sets.union acc (collectTermImports currentNs (Packaging.termDefinitionTerm td))) Sets.empty termDefs
          typeImportsBlock = importsToText "type" currentNs typeImports
          termImportsBlock = importsToText "value" currentNs termImports
          importsBlock = Strings.cat2 typeImportsBlock termImportsBlock
      in (Eithers.bind (Eithers.mapList (encodeTypeDefinition cx g) typeDefs) (\typeItems ->
        let termItems = Lists.map (encodeTermDefinition cx g currentNs) termDefs
            allItems = Lists.concat2 typeItems termItems
            header = "// Note: this is an automatically generated file. Do not edit.\n\n"
            body = Strings.intercalate "\n\n" (Lists.map printModuleItem allItems)
            filePath = Names.namespaceToFilePath Util.CaseConventionCamel (Packaging.FileExtension "ts") (Packaging.moduleNamespace mod)
        in (Right (Maps.singleton filePath (Strings.cat [
          header,
          importsBlock,
          (Logic.ifElse (Equality.equal importsBlock "") "" "\n"),
          body,
          (Logic.ifElse (Equality.equal body "") "" "\n")])))))
printInterfaceDeclaration :: Syntax.InterfaceDeclaration -> String
printInterfaceDeclaration decl =

      let name = Syntax.unIdentifier (Syntax.interfaceDeclarationName decl)
          params = printTypeParameterList (Syntax.interfaceDeclarationTypeParameters decl)
          exts = Syntax.interfaceDeclarationExtends decl
          extClause =
                  Logic.ifElse (Lists.null exts) "" (Strings.cat2 " extends " (Strings.intercalate ", " (Lists.map printTypeExpression exts)))
          members = Syntax.interfaceDeclarationMembers decl
          body =
                  Logic.ifElse (Lists.null members) "" (Strings.cat [
                    "\n  ",
                    (Strings.intercalate ";\n  " (Lists.map printPropertySignature members)),
                    ";\n"])
      in (Strings.cat [
        "export interface ",
        name,
        params,
        extClause,
        " {",
        body,
        "}\n"])
printLiteral :: Syntax.Literal -> String
printLiteral lit =
    case lit of
      Syntax.LiteralString v0 -> tsEscapeString (Syntax.stringLiteralValue v0)
      Syntax.LiteralBoolean v0 -> Logic.ifElse v0 "true" "false"
      Syntax.LiteralNull -> "null"
      Syntax.LiteralUndefined -> "undefined"
      _ -> "null"
printModuleItem :: Syntax.ModuleItem -> String
printModuleItem mi =
    case mi of
      Syntax.ModuleItemInterface v0 -> printInterfaceDeclaration v0
      Syntax.ModuleItemTypeAlias v0 -> printTypeAliasDeclaration v0
      Syntax.ModuleItemStatement _ -> Serialization.printExpr (Serde.moduleItemToExpr mi)
      Syntax.ModuleItemImport _ -> Serialization.printExpr (Serde.moduleItemToExpr mi)
      Syntax.ModuleItemExport _ -> Serialization.printExpr (Serde.moduleItemToExpr mi)
      _ -> ""
printPropertySignature :: Syntax.PropertySignature -> String
printPropertySignature ps =
    Strings.cat [
      Logic.ifElse (Syntax.propertySignatureReadonly ps) "readonly " "",
      (Syntax.unIdentifier (Syntax.propertySignatureName ps)),
      (Logic.ifElse (Syntax.propertySignatureOptional ps) "?" ""),
      ": ",
      (printTypeExpression (Syntax.propertySignatureType ps))]
printTypeAliasDeclaration :: Syntax.TypeAliasDeclaration -> String
printTypeAliasDeclaration decl =

      let name = Syntax.unIdentifier (Syntax.typeAliasDeclarationName decl)
          params = printTypeParameterList (Syntax.typeAliasDeclarationTypeParameters decl)
          rhs = printTypeExpression (Syntax.typeAliasDeclarationType decl)
      in (Strings.cat [
        "export type ",
        name,
        params,
        " = ",
        rhs,
        ";\n"])
printTypeExpression :: Syntax.TypeExpression -> String
printTypeExpression t =
    case t of
      Syntax.TypeExpressionIdentifier v0 -> Syntax.unIdentifier v0
      Syntax.TypeExpressionLiteral v0 -> printLiteral v0
      Syntax.TypeExpressionArray v0 -> Strings.cat [
        "ReadonlyArray<",
        (printTypeExpression (Syntax.unArrayTypeExpression v0)),
        ">"]
      Syntax.TypeExpressionTuple v0 -> Strings.cat [
        "readonly [",
        (Strings.intercalate ", " (Lists.map printTypeExpression v0)),
        "]"]
      Syntax.TypeExpressionUnion v0 -> Strings.intercalate " | " (Lists.map printTypeExpression v0)
      Syntax.TypeExpressionIntersection v0 -> Strings.intercalate " & " (Lists.map printTypeExpression v0)
      Syntax.TypeExpressionParameterized v0 -> Strings.cat [
        printTypeExpression (Syntax.parameterizedTypeExpressionBase v0),
        "<",
        (Strings.intercalate ", " (Lists.map printTypeExpression (Syntax.parameterizedTypeExpressionArguments v0))),
        ">"]
      Syntax.TypeExpressionOptional v0 -> Strings.cat [
        printTypeExpression v0,
        " | undefined"]
      Syntax.TypeExpressionReadonly v0 -> Strings.cat2 "readonly " (printTypeExpression v0)
      Syntax.TypeExpressionObject v0 -> Strings.cat [
        "{ ",
        (Strings.intercalate "; " (Lists.map printPropertySignature v0)),
        " }"]
      Syntax.TypeExpressionFunction _ -> "((...args: any[]) => any)"
      Syntax.TypeExpressionAny -> "any"
      Syntax.TypeExpressionUnknown -> "unknown"
      Syntax.TypeExpressionVoid -> "void"
      Syntax.TypeExpressionNever -> "never"
      _ -> "unknown"
printTypeParameter :: Syntax.TypeParameter -> String
printTypeParameter tp =

      let name = Syntax.unIdentifier (Syntax.typeParameterName tp)
          constraint = Syntax.typeParameterConstraint tp
      in (Maybes.cases constraint name (\c -> Strings.cat [
        name,
        " extends ",
        (printTypeExpression c)]))
printTypeParameterList :: [Syntax.TypeParameter] -> String
printTypeParameterList tps =
    Logic.ifElse (Lists.null tps) "" (Strings.cat [
      "<",
      (Strings.intercalate ", " (Lists.map printTypeParameter tps)),
      ">"])
sanitizeParamName :: Core.Name -> String
sanitizeParamName n = Formatting.sanitizeWithUnderscores Language.typeScriptReservedWords (Names.localNameOf n)
sortTermDefsTopologically :: t0 -> [Packaging.TermDefinition] -> [Packaging.TermDefinition]
sortTermDefsTopologically currentNs tdefs =

      let byName = Maps.fromList (Lists.map (\td -> (Packaging.termDefinitionName td, td)) tdefs)
          adjacency =
                  Lists.map (\td ->
                    let tname = Packaging.termDefinitionName td
                        tterm = Packaging.termDefinitionTerm td
                        freeVars = Variables.freeVariablesInTerm tterm
                        deps = Lists.filter (\n -> Maps.member n byName) (Sets.toList freeVars)
                    in (tname, deps)) tdefs
          sccs = Sorting.topologicalSortComponents adjacency
      in (Maybes.cat (Lists.map (\n -> Maps.lookup n byName) (Lists.concat sccs)))
stripForalls :: Core.Type -> Core.Type
stripForalls t =

      let dt = Strip.deannotateType t
      in case dt of
        Core.TypeForall v0 -> stripForalls (Core.forallTypeBody v0)
        _ -> dt
termHeadVariable :: Core.Term -> Maybe Core.Name
termHeadVariable t =

      let dt = Strip.deannotateTerm t
      in case dt of
        Core.TermVariable v0 -> Just v0
        Core.TermTypeApplication v0 -> termHeadVariable (Core.typeApplicationTermBody v0)
        _ -> Nothing
tsArray :: [Syntax.Expression] -> Syntax.Expression
tsArray elems = Syntax.ExpressionArray (Lists.map (\e -> Syntax.ArrayElementExpression e) elems)
tsArrow :: [String] -> Syntax.Expression -> Syntax.Expression
tsArrow params body =
    Syntax.ExpressionArrow (Syntax.ArrowFunctionExpression {
      Syntax.arrowFunctionExpressionParams = (Lists.map (\p -> Syntax.PatternIdentifier (tsIdent p)) params),
      Syntax.arrowFunctionExpressionBody = (Syntax.ArrowFunctionBodyExpression body),
      Syntax.arrowFunctionExpressionAsync = False})
tsArrowTyped :: [Syntax.Pattern] -> Syntax.Expression -> Syntax.Expression
tsArrowTyped patterns body =
    Syntax.ExpressionArrow (Syntax.ArrowFunctionExpression {
      Syntax.arrowFunctionExpressionParams = patterns,
      Syntax.arrowFunctionExpressionBody = (Syntax.ArrowFunctionBodyExpression body),
      Syntax.arrowFunctionExpressionAsync = False})
tsAsAny :: Syntax.Expression -> Syntax.Expression
tsAsAny e =
    Syntax.ExpressionAsExpression (Syntax.AsExpression {
      Syntax.asExpressionExpression = e,
      Syntax.asExpressionType = Syntax.TypeExpressionAny})
tsCall :: Syntax.Expression -> [Syntax.Expression] -> Syntax.Expression
tsCall callee args =
    Syntax.ExpressionCall (Syntax.CallExpression {
      Syntax.callExpressionCallee = callee,
      Syntax.callExpressionArguments = args,
      Syntax.callExpressionOptional = False})
tsCond :: Syntax.Expression -> Syntax.Expression -> Syntax.Expression -> Syntax.Expression
tsCond test cons alt =
    Syntax.ExpressionConditional (Syntax.ConditionalExpression {
      Syntax.conditionalExpressionTest = test,
      Syntax.conditionalExpressionConsequent = cons,
      Syntax.conditionalExpressionAlternate = alt})
tsEscapeString :: String -> String
tsEscapeString s =

      let escapeChar =
              \c -> Logic.ifElse (Equality.equal c 34) "\\\"" (Logic.ifElse (Equality.equal c 92) "\\\\" (Logic.ifElse (Equality.equal c 10) "\\n" (Logic.ifElse (Equality.equal c 13) "\\r" (Logic.ifElse (Equality.equal c 9) "\\t" (Logic.ifElse (Equality.equal c 8) "\\b" (Logic.ifElse (Equality.equal c 12) "\\f" (Strings.fromList (Lists.pure c))))))))
      in (Strings.cat [
        "\"",
        (Strings.cat (Lists.map escapeChar (Strings.toList s))),
        "\""])
tsExprIdent :: String -> Syntax.Expression
tsExprIdent s = Syntax.ExpressionIdentifier (tsIdent s)
tsExprStr :: String -> Syntax.Expression
tsExprStr s =
    Syntax.ExpressionLiteral (Syntax.LiteralString (Syntax.StringLiteral {
      Syntax.stringLiteralValue = s,
      Syntax.stringLiteralSingleQuote = False}))
tsIdent :: String -> Syntax.Identifier
tsIdent s = Syntax.Identifier s
tsMember :: Syntax.Expression -> String -> Syntax.Expression
tsMember obj prop =
    Syntax.ExpressionMember (Syntax.MemberExpression {
      Syntax.memberExpressionObject = obj,
      Syntax.memberExpressionProperty = (tsExprIdent prop),
      Syntax.memberExpressionComputed = False,
      Syntax.memberExpressionOptional = False})
tsNamedType :: String -> Syntax.TypeExpression
tsNamedType n = Syntax.TypeExpressionIdentifier (tsIdent n)
tsNew :: Syntax.Expression -> [Syntax.Expression] -> Syntax.Expression
tsNew callee args =
    Syntax.ExpressionNew (Syntax.CallExpression {
      Syntax.callExpressionCallee = callee,
      Syntax.callExpressionArguments = args,
      Syntax.callExpressionOptional = False})
tsObject :: [(String, Syntax.Expression)] -> Syntax.Expression
tsObject props =
    Syntax.ExpressionObject (Lists.map (\kv ->
      let k = Pairs.first kv
          v = Pairs.second kv
      in Syntax.Property {
        Syntax.propertyKey = (tsExprIdent k),
        Syntax.propertyValue = v,
        Syntax.propertyKind = Syntax.PropertyKindInit,
        Syntax.propertyComputed = False,
        Syntax.propertyShorthand = False}) props)
tsParam :: String -> Syntax.TypeParameter
tsParam n =
    Syntax.TypeParameter {
      Syntax.typeParameterName = (tsIdent n),
      Syntax.typeParameterConstraint = Nothing,
      Syntax.typeParameterDefault = Nothing}
tsParamApp1 :: String -> Syntax.TypeExpression -> Syntax.TypeExpression
tsParamApp1 n arg =
    Syntax.TypeExpressionParameterized (Syntax.ParameterizedTypeExpression {
      Syntax.parameterizedTypeExpressionBase = (tsNamedType n),
      Syntax.parameterizedTypeExpressionArguments = [
        arg]})
tsParamApp2 :: String -> Syntax.TypeExpression -> Syntax.TypeExpression -> Syntax.TypeExpression
tsParamApp2 n a b =
    Syntax.TypeExpressionParameterized (Syntax.ParameterizedTypeExpression {
      Syntax.parameterizedTypeExpressionBase = (tsNamedType n),
      Syntax.parameterizedTypeExpressionArguments = [
        a,
        b]})
tsParen :: Syntax.Expression -> Syntax.Expression
tsParen e = Syntax.ExpressionParenthesized e
tsPropSig :: String -> Bool -> Syntax.TypeExpression -> Syntax.PropertySignature
tsPropSig name optional typ =

      let safe = Formatting.sanitizeWithUnderscores Language.typeScriptReservedWords name
      in Syntax.PropertySignature {
        Syntax.propertySignatureName = (tsIdent safe),
        Syntax.propertySignatureType = typ,
        Syntax.propertySignatureOptional = optional,
        Syntax.propertySignatureReadonly = True}
tsReadonlyMap :: Syntax.TypeExpression -> Syntax.TypeExpression -> Syntax.TypeExpression
tsReadonlyMap k v = tsParamApp2 "ReadonlyMap" k v
tsReadonlySet :: Syntax.TypeExpression -> Syntax.TypeExpression
tsReadonlySet t = tsParamApp1 "ReadonlySet" t
tsTuple :: [Syntax.TypeExpression] -> Syntax.TypeExpression
tsTuple ts = Syntax.TypeExpressionTuple ts
tsTypedIdent :: String -> Syntax.TypeExpression -> Syntax.Pattern
tsTypedIdent name typ =
    Syntax.PatternTyped (Syntax.TypedPattern {
      Syntax.typedPatternPattern = (Syntax.PatternIdentifier (tsIdent name)),
      Syntax.typedPatternType = typ})
tsUndefined :: Syntax.Expression
tsUndefined = tsExprIdent "undefined"
