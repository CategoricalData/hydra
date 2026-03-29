-- Note: this is an automatically generated file. Do not edit.

-- | Lisp code generator: converts Hydra type and term modules to Lisp AST

module Hydra.Ext.Lisp.Coder where

import qualified Hydra.CoderUtils as CoderUtils
import qualified Hydra.Core as Core
import qualified Hydra.Ext.Lisp.Language as Language
import qualified Hydra.Ext.Lisp.Syntax as Syntax
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Sorting as Sorting
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

dialectCadr :: Syntax.Dialect -> String
dialectCadr d =
    case d of
      Syntax.DialectClojure -> "second"
      _ -> "cadr"

dialectCar :: Syntax.Dialect -> String
dialectCar d =
    case d of
      Syntax.DialectClojure -> "first"
      _ -> "car"

dialectConstructorPrefix :: Syntax.Dialect -> String
dialectConstructorPrefix d =
    case d of
      Syntax.DialectClojure -> "->"
      _ -> "make-"

dialectEqual :: Syntax.Dialect -> String
dialectEqual d =
    case d of
      Syntax.DialectClojure -> "="
      Syntax.DialectCommonLisp -> "equal"
      Syntax.DialectEmacsLisp -> "equal"
      _ -> "equal?"

encodeApplication :: Syntax.Dialect -> t0 -> Graph.Graph -> S.Set Core.Name -> Core.Term -> Core.Term -> Either t1 Syntax.Expression
encodeApplication dialect cx g thunkedVars rawFun rawArg =

      let dFun = Rewriting.deannotateTerm rawFun
          normal =
                  Eithers.bind (encodeTerm dialect cx g thunkedVars rawFun) (\fun -> Eithers.bind (encodeTerm dialect cx g thunkedVars rawArg) (\arg -> Right (lispApp fun [
                    arg])))
          enc = \t -> encodeTerm dialect cx g thunkedVars t
      in case dFun of
        Core.TermApplication v0 ->
          let midFun = Core.applicationFunction v0
              midArg = Core.applicationArgument v0
              dMidFun = Rewriting.deannotateTerm midFun
              isLazy2 =
                      Logic.or (isPrimitiveRef "hydra.lib.eithers.fromLeft" dMidFun) (Logic.or (isPrimitiveRef "hydra.lib.eithers.fromRight" dMidFun) (isPrimitiveRef "hydra.lib.maybes.fromMaybe" dMidFun))
          in (Logic.ifElse isLazy2 (Eithers.bind (enc midFun) (\ePrim -> Eithers.bind (enc midArg) (\eDef -> Eithers.bind (enc rawArg) (\eArg -> Right (lispApp (lispApp ePrim [
            wrapInThunk eDef]) [
            eArg]))))) (case dMidFun of
            Core.TermApplication v1 ->
              let innerFun = Core.applicationFunction v1
                  innerArg = Core.applicationArgument v1
                  dInnerFun = Rewriting.deannotateTerm innerFun
              in (Logic.ifElse (isPrimitiveRef "hydra.lib.logic.ifElse" dInnerFun) (Eithers.bind (enc innerArg) (\eC -> Eithers.bind (enc midArg) (\eT -> Eithers.bind (enc rawArg) (\eE -> Right (Syntax.ExpressionIf (Syntax.IfExpression {
                Syntax.ifExpressionCondition = eC,
                Syntax.ifExpressionThen = eT,
                Syntax.ifExpressionElse = (Just eE)})))))) (Logic.ifElse (isPrimitiveRef "hydra.lib.maybes.maybe" dInnerFun) (Eithers.bind (enc innerFun) (\eP -> Eithers.bind (enc innerArg) (\eDef -> Eithers.bind (enc midArg) (\eF -> Eithers.bind (enc rawArg) (\eM -> Right (lispApp (lispApp (lispApp eP [
                wrapInThunk eDef]) [
                eF]) [
                eM])))))) (Logic.ifElse (isPrimitiveRef "hydra.lib.maybes.cases" dInnerFun) (Eithers.bind (enc innerFun) (\eP -> Eithers.bind (enc innerArg) (\eM -> Eithers.bind (enc midArg) (\eN -> Eithers.bind (enc rawArg) (\eJ -> Right (lispApp (lispApp (lispApp eP [
                eM]) [
                wrapInThunk eN]) [
                eJ])))))) normal)))
            _ -> normal))
        _ -> normal

encodeElimination :: Syntax.Dialect -> t0 -> Graph.Graph -> S.Set Core.Name -> Core.Elimination -> Maybe Core.Term -> Either t1 Syntax.Expression
encodeElimination dialect cx g thunkedVars elim marg =
    case elim of
      Core.EliminationRecord v0 ->
        let fname = Formatting.convertCaseCamelToLowerSnake (Core.unName (Core.projectionField v0))
            tname = qualifiedSnakeName (Core.projectionTypeName v0)
        in (Maybes.cases marg (Right (lispLambdaExpr [
          "v"] (Syntax.ExpressionFieldAccess (Syntax.FieldAccess {
          Syntax.fieldAccessRecordType = (Syntax.Symbol tname),
          Syntax.fieldAccessField = (Syntax.Symbol fname),
          Syntax.fieldAccessTarget = (lispVar "v")})))) (\arg -> Eithers.bind (encodeTerm dialect cx g thunkedVars arg) (\sarg -> Right (Syntax.ExpressionFieldAccess (Syntax.FieldAccess {
          Syntax.fieldAccessRecordType = (Syntax.Symbol tname),
          Syntax.fieldAccessField = (Syntax.Symbol fname),
          Syntax.fieldAccessTarget = sarg})))))
      Core.EliminationUnion v0 ->
        let tname = Names.localNameOf (Core.caseStatementTypeName v0)
            caseFields = Core.caseStatementCases v0
            defCase = Core.caseStatementDefault v0
        in (Eithers.bind (Eithers.mapList (\cf ->
          let cfname = Formatting.convertCaseCamelToLowerSnake (Core.unName (Core.fieldName cf))
              cfterm = Core.fieldTerm cf
              condExpr =
                      lispApp (lispVar (dialectEqual dialect)) [
                        lispApp (lispVar (dialectCar dialect)) [
                          lispVar "match_target"],
                        (lispKeyword cfname)]
          in (Eithers.bind (encodeTerm dialect cx g thunkedVars (Core.TermApplication (Core.Application {
            Core.applicationFunction = cfterm,
            Core.applicationArgument = (Core.TermVariable (Core.Name "match_value"))}))) (\bodyExpr -> Right (Syntax.CondClause {
            Syntax.condClauseCondition = condExpr,
            Syntax.condClauseBody = bodyExpr})))) caseFields) (\clauses -> Eithers.bind (Maybes.cases defCase (Right Nothing) (\dt -> Eithers.bind (encodeTerm dialect cx g thunkedVars dt) (\defBody -> Right (Just defBody)))) (\defExpr ->
          let condExpr =
                  Syntax.ExpressionCond (Syntax.CondExpression {
                    Syntax.condExpressionClauses = clauses,
                    Syntax.condExpressionDefault = defExpr})
              innerExpr =
                      lispApp (lispLambdaExpr [
                        "match_value"] condExpr) [
                        lispApp (lispVar (dialectCadr dialect)) [
                          lispVar "match_target"]]
          in (Maybes.cases marg (Right (lispLambdaExpr [
            "match_target"] innerExpr)) (\arg -> Eithers.bind (encodeTerm dialect cx g thunkedVars arg) (\sarg -> Right (lispApp (lispLambdaExpr [
            "match_target"] innerExpr) [
            sarg])))))))
      Core.EliminationWrap _ -> Maybes.cases marg (Right (lispLambdaExpr [
        "v"] (lispVar "v"))) (\arg -> encodeTerm dialect cx g thunkedVars arg)

encodeFieldDef :: Core.FieldType -> Syntax.FieldDefinition
encodeFieldDef ft =

      let fname = Core.unName (Core.fieldTypeName ft)
      in Syntax.FieldDefinition {
        Syntax.fieldDefinitionName = (Syntax.Symbol (Formatting.convertCaseCamelToLowerSnake fname)),
        Syntax.fieldDefinitionDefaultValue = Nothing}

encodeFunction :: Syntax.Dialect -> t0 -> Graph.Graph -> S.Set Core.Name -> Core.Function -> Either t1 Syntax.Expression
encodeFunction dialect cx g thunkedVars fun =
    case fun of
      Core.FunctionLambda v0 ->
        let param =
                Formatting.convertCaseCamelOrUnderscoreToLowerSnake (Formatting.sanitizeWithUnderscores Language.lispReservedWords (Core.unName (Core.lambdaParameter v0)))
        in (Eithers.bind (encodeTerm dialect cx g thunkedVars (Core.lambdaBody v0)) (\body -> Right (lispLambdaExpr [
          param] body)))
      Core.FunctionPrimitive v0 -> Right (lispVar (Formatting.convertCaseCamelOrUnderscoreToLowerSnake (Formatting.sanitizeWithUnderscores Language.lispReservedWords (Core.unName v0))))
      Core.FunctionElimination v0 -> encodeElimination dialect cx g thunkedVars v0 Nothing

encodeLetAsLambdaApp :: Syntax.Dialect -> t0 -> Graph.Graph -> S.Set Core.Name -> [Core.Binding] -> Core.Term -> Either t1 Syntax.Expression
encodeLetAsLambdaApp dialect cx g thunkedVars bindings body =
    Eithers.bind (encodeTerm dialect cx g thunkedVars body) (\bodyExpr -> Eithers.foldl (\acc -> \b ->
      let bname =
              Formatting.convertCaseCamelOrUnderscoreToLowerSnake (Formatting.sanitizeWithUnderscores Language.lispReservedWords (Core.unName (Core.bindingName b)))
      in (Eithers.bind (encodeTerm dialect cx g thunkedVars (Core.bindingTerm b)) (\bval -> Right (lispApp (lispLambdaExpr [
        bname] acc) [
        bval])))) bodyExpr (Lists.reverse bindings))

encodeLetAsNative :: Syntax.Dialect -> t0 -> Graph.Graph -> S.Set Core.Name -> [Core.Binding] -> Core.Term -> Either t1 Syntax.Expression
encodeLetAsNative dialect cx g thunkedVars bindings body =

      let isClojureTop =
              case dialect of
                Syntax.DialectClojure -> True
                _ -> False
          gExtended =
                  Rewriting.extendGraphForLet lispBindingMetadata g (Core.Let {
                    Core.letBindings = bindings,
                    Core.letBody = body})
          newThunkedNames =
                  Sets.fromList (Maybes.cat (Lists.map (\b -> Logic.ifElse (lispShouldThunkBinding g b) (Just (Core.bindingName b)) Nothing) bindings))
          thunkedVarsExtended = Sets.union thunkedVars newThunkedNames
      in (Eithers.bind (encodeTerm dialect cx gExtended thunkedVarsExtended body) (\bodyExpr ->
        let sortedBindings =
                Logic.ifElse True (
                  let allNames = Sets.fromList (Lists.map (\b -> Core.bindingName b) bindings)
                      adjList =
                              Lists.map (\b -> (Core.bindingName b, (Sets.toList (Sets.intersection allNames (Rewriting.freeVariablesInTerm (Core.bindingTerm b)))))) bindings
                      sortResult = Sorting.topologicalSort adjList
                      nameToBinding = Maps.fromList (Lists.map (\b -> (Core.bindingName b, b)) bindings)
                  in (Eithers.either (\_ -> bindings) (\sorted -> Lists.map (\name -> Maybes.fromMaybe (Lists.head bindings) (Maps.lookup name nameToBinding)) sorted) sortResult)) bindings
        in (Eithers.bind (Eithers.mapList (\b ->
          let bname =
                  Formatting.convertCaseCamelOrUnderscoreToLowerSnake (Formatting.sanitizeWithUnderscores Language.lispReservedWords (Core.unName (Core.bindingName b)))
              isSelfRef = Sets.member (Core.bindingName b) (Rewriting.freeVariablesInTerm (Core.bindingTerm b))
              isLambda =
                      case (Rewriting.deannotateTerm (Core.bindingTerm b)) of
                        Core.TermFunction v0 -> case v0 of
                          Core.FunctionLambda _ -> True
                          _ -> False
                        _ -> False
              needsThunk = lispShouldThunkBinding g b
          in (Eithers.bind (encodeTerm dialect cx gExtended thunkedVarsExtended (Core.bindingTerm b)) (\bval ->
            let isClojure =
                    case dialect of
                      Syntax.DialectClojure -> True
                      _ -> False
                wrappedVal =
                        Logic.ifElse isClojure (Logic.ifElse isSelfRef (Logic.ifElse isLambda (case bval of
                          Syntax.ExpressionLambda v0 -> Syntax.ExpressionLambda (Syntax.Lambda {
                            Syntax.lambdaName = (Just (Syntax.Symbol bname)),
                            Syntax.lambdaParams = (Syntax.lambdaParams v0),
                            Syntax.lambdaRestParam = (Syntax.lambdaRestParam v0),
                            Syntax.lambdaBody = (Syntax.lambdaBody v0)})
                          _ -> bval) (lispNamedLambdaExpr bname [
                          "_arg"] (lispApp bval [
                          lispVar "_arg"]))) bval) (Logic.ifElse (Logic.and isSelfRef (Logic.not isLambda)) (lispLambdaExpr [
                          "_arg"] (lispApp bval [
                          lispVar "_arg"])) bval)
                thunkedVal = Logic.ifElse (Logic.and needsThunk (Logic.not isSelfRef)) (wrapInDelay wrappedVal) wrappedVal
            in (Right (bname, thunkedVal))))) sortedBindings) (\encodedBindings ->
          let allBindingNames = Sets.fromList (Lists.map (\b -> Core.bindingName b) bindings)
              hasCrossRefs =
                      Lists.foldl (\acc -> \b -> Logic.or acc (Logic.not (Sets.null (Sets.intersection allBindingNames (Rewriting.freeVariablesInTerm (Core.bindingTerm b)))))) False bindings
              hasSelfRef =
                      Lists.foldl (\acc -> \b -> Logic.or acc (Sets.member (Core.bindingName b) (Rewriting.freeVariablesInTerm (Core.bindingTerm b)))) False bindings
              isRecursive = hasSelfRef
              letKind =
                      Logic.ifElse isRecursive Syntax.LetKindRecursive (Logic.ifElse (Lists.null (Lists.tail bindings)) Syntax.LetKindParallel Syntax.LetKindSequential)
              lispBindings =
                      Lists.map (\eb -> Syntax.LetBindingSimple (Syntax.SimpleBinding {
                        Syntax.simpleBindingName = (Syntax.Symbol (Pairs.first eb)),
                        Syntax.simpleBindingValue = (Pairs.second eb)})) encodedBindings
          in (Right (Syntax.ExpressionLet (Syntax.LetExpression {
            Syntax.letExpressionKind = letKind,
            Syntax.letExpressionBindings = lispBindings,
            Syntax.letExpressionBody = [
              bodyExpr]})))))))

encodeLiteral :: Core.Literal -> Syntax.Expression
encodeLiteral lit =
    case lit of
      Core.LiteralBoolean v0 -> Syntax.ExpressionLiteral (Syntax.LiteralBoolean v0)
      Core.LiteralString v0 -> Syntax.ExpressionLiteral (Syntax.LiteralString v0)
      Core.LiteralFloat v0 -> case v0 of
        Core.FloatValueFloat32 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralFloat (Syntax.FloatLiteral {
          Syntax.floatLiteralValue = (Literals.float32ToBigfloat v1),
          Syntax.floatLiteralPrecision = Nothing}))
        Core.FloatValueFloat64 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralFloat (Syntax.FloatLiteral {
          Syntax.floatLiteralValue = (Literals.float64ToBigfloat v1),
          Syntax.floatLiteralPrecision = Nothing}))
        Core.FloatValueBigfloat v1 -> Syntax.ExpressionLiteral (Syntax.LiteralFloat (Syntax.FloatLiteral {
          Syntax.floatLiteralValue = v1,
          Syntax.floatLiteralPrecision = Nothing}))
      Core.LiteralInteger v0 -> case v0 of
        Core.IntegerValueInt8 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = (Literals.int8ToBigint v1),
          Syntax.integerLiteralBigint = False}))
        Core.IntegerValueInt16 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = (Literals.int16ToBigint v1),
          Syntax.integerLiteralBigint = False}))
        Core.IntegerValueInt32 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = (Literals.int32ToBigint v1),
          Syntax.integerLiteralBigint = False}))
        Core.IntegerValueInt64 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = (Literals.int64ToBigint v1),
          Syntax.integerLiteralBigint = False}))
        Core.IntegerValueUint8 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = (Literals.uint8ToBigint v1),
          Syntax.integerLiteralBigint = False}))
        Core.IntegerValueUint16 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = (Literals.uint16ToBigint v1),
          Syntax.integerLiteralBigint = False}))
        Core.IntegerValueUint32 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = (Literals.uint32ToBigint v1),
          Syntax.integerLiteralBigint = False}))
        Core.IntegerValueUint64 v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = (Literals.uint64ToBigint v1),
          Syntax.integerLiteralBigint = False}))
        Core.IntegerValueBigint v1 -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
          Syntax.integerLiteralValue = v1,
          Syntax.integerLiteralBigint = True}))
      Core.LiteralBinary v0 ->
        let byteValues = Literals.binaryToBytes v0
        in (Syntax.ExpressionVector (Syntax.VectorLiteral {
          Syntax.vectorLiteralElements = (Lists.map (\bv -> Syntax.ExpressionLiteral (Syntax.LiteralInteger (Syntax.IntegerLiteral {
            Syntax.integerLiteralValue = (Literals.int32ToBigint bv),
            Syntax.integerLiteralBigint = False}))) byteValues)}))

encodeTerm :: Syntax.Dialect -> t0 -> Graph.Graph -> S.Set Core.Name -> Core.Term -> Either t1 Syntax.Expression
encodeTerm dialect cx g thunkedVars term =
    case term of
      Core.TermAnnotated v0 -> encodeTerm dialect cx g thunkedVars (Core.annotatedTermBody v0)
      Core.TermApplication v0 ->
        let rawFun = Core.applicationFunction v0
            rawArg = Core.applicationArgument v0
        in (encodeApplication dialect cx g thunkedVars rawFun rawArg)
      Core.TermEither v0 -> Eithers.either (\l -> Eithers.bind (encodeTerm dialect cx g thunkedVars l) (\sl -> Right (lispApp (lispVar "list") [
        lispKeyword "left",
        sl]))) (\r -> Eithers.bind (encodeTerm dialect cx g thunkedVars r) (\sr -> Right (lispApp (lispVar "list") [
        lispKeyword "right",
        sr]))) v0
      Core.TermFunction v0 -> encodeFunction dialect cx g thunkedVars v0
      Core.TermLet v0 ->
        let bindings = Core.letBindings v0
            body = Core.letBody v0
        in (encodeLetAsNative dialect cx g thunkedVars bindings body)
      Core.TermList v0 -> Eithers.bind (Eithers.mapList (encodeTerm dialect cx g thunkedVars) v0) (\sels -> Right (lispListExpr sels))
      Core.TermLiteral v0 -> Right (encodeLiteral v0)
      Core.TermMap v0 -> Eithers.bind (Eithers.mapList (\entry -> Eithers.bind (encodeTerm dialect cx g thunkedVars (Pairs.first entry)) (\k -> Eithers.bind (encodeTerm dialect cx g thunkedVars (Pairs.second entry)) (\v -> Right (Syntax.MapEntry {
        Syntax.mapEntryKey = k,
        Syntax.mapEntryValue = v})))) (Maps.toList v0)) (\pairs -> Right (Syntax.ExpressionMap (Syntax.MapLiteral {
        Syntax.mapLiteralEntries = pairs})))
      Core.TermMaybe v0 -> Maybes.cases v0 (Right (lispApp (lispVar "list") [
        lispKeyword "nothing"])) (\val -> Eithers.bind (encodeTerm dialect cx g thunkedVars val) (\sval -> Right (lispApp (lispVar "list") [
        lispKeyword "just",
        sval])))
      Core.TermPair v0 -> Eithers.bind (encodeTerm dialect cx g thunkedVars (Pairs.first v0)) (\f -> Eithers.bind (encodeTerm dialect cx g thunkedVars (Pairs.second v0)) (\s -> Right (lispListExpr [
        f,
        s])))
      Core.TermRecord v0 ->
        let rname = Core.recordTypeName v0
            fields = Core.recordFields v0
        in (Eithers.bind (Eithers.mapList (\f -> encodeTerm dialect cx g thunkedVars (Core.fieldTerm f)) fields) (\sfields ->
          let constructorName = Strings.cat2 (dialectConstructorPrefix dialect) (qualifiedSnakeName rname)
          in (Right (lispApp (lispVar constructorName) sfields))))
      Core.TermSet v0 -> Eithers.bind (Eithers.mapList (encodeTerm dialect cx g thunkedVars) (Sets.toList v0)) (\sels -> Right (Syntax.ExpressionSet (Syntax.SetLiteral {
        Syntax.setLiteralElements = sels})))
      Core.TermUnion v0 ->
        let tname = Names.localNameOf (Core.injectionTypeName v0)
            field = Core.injectionField v0
            fname = Core.unName (Core.fieldName field)
            fterm = Core.fieldTerm field
            dterm = Rewriting.deannotateTerm fterm
            isUnit =
                    case dterm of
                      Core.TermUnit -> True
                      Core.TermRecord v1 -> Lists.null (Core.recordFields v1)
                      _ -> False
        in (Logic.ifElse isUnit (Right (lispApp (lispVar "list") [
          lispKeyword (Formatting.convertCaseCamelToLowerSnake fname),
          lispNilExpr])) (Eithers.bind (encodeTerm dialect cx g thunkedVars fterm) (\sval -> Right (lispApp (lispVar "list") [
          lispKeyword (Formatting.convertCaseCamelToLowerSnake fname),
          sval]))))
      Core.TermUnit -> Right lispNilExpr
      Core.TermVariable v0 ->
        let sname =
                Formatting.convertCaseCamelOrUnderscoreToLowerSnake (Formatting.sanitizeWithUnderscores Language.lispReservedWords (Core.unName v0))
            varExpr = lispVar sname
            isThunked = Sets.member v0 thunkedVars
        in (Logic.ifElse isThunked (Right (lispApp (lispVar "force") [
          varExpr])) (Right varExpr))
      Core.TermTypeApplication v0 -> encodeTerm dialect cx g thunkedVars (Core.typeApplicationTermBody v0)
      Core.TermTypeLambda v0 -> encodeTerm dialect cx g thunkedVars (Core.typeLambdaBody v0)
      Core.TermWrap v0 -> encodeTerm dialect cx g thunkedVars (Core.wrappedTermBody v0)

encodeTermDefinition :: Syntax.Dialect -> t0 -> Graph.Graph -> Module.TermDefinition -> Either t1 Syntax.TopLevelFormWithComments
encodeTermDefinition dialect cx g tdef =

      let name = Module.termDefinitionName tdef
          term = Module.termDefinitionTerm tdef
          lname = qualifiedSnakeName name
          dterm = Rewriting.deannotateTerm term
      in case dterm of
        Core.TermFunction v0 -> case v0 of
          Core.FunctionLambda _ -> Eithers.bind (encodeTerm dialect cx g Sets.empty term) (\sterm -> Right (lispTopForm (Syntax.TopLevelFormVariable (Syntax.VariableDefinition {
            Syntax.variableDefinitionName = (Syntax.Symbol lname),
            Syntax.variableDefinitionValue = sterm,
            Syntax.variableDefinitionDoc = Nothing}))))
          _ -> Eithers.bind (encodeTerm dialect cx g Sets.empty term) (\sterm -> Right (lispTopForm (Syntax.TopLevelFormVariable (Syntax.VariableDefinition {
            Syntax.variableDefinitionName = (Syntax.Symbol lname),
            Syntax.variableDefinitionValue = sterm,
            Syntax.variableDefinitionDoc = Nothing}))))
        _ -> Eithers.bind (encodeTerm dialect cx g Sets.empty term) (\sterm -> Right (lispTopForm (Syntax.TopLevelFormVariable (Syntax.VariableDefinition {
          Syntax.variableDefinitionName = (Syntax.Symbol lname),
          Syntax.variableDefinitionValue = sterm,
          Syntax.variableDefinitionDoc = Nothing}))))

encodeType :: t0 -> t1 -> Core.Type -> Either t2 Syntax.TypeSpecifier
encodeType cx g t =

      let typ = Rewriting.deannotateType t
      in case typ of
        Core.TypeAnnotated v0 -> encodeType cx g (Core.annotatedTypeBody v0)
        Core.TypeApplication v0 -> encodeType cx g (Core.applicationTypeFunction v0)
        Core.TypeUnit -> Right Syntax.TypeSpecifierUnit
        Core.TypeLiteral v0 -> Right (case v0 of
          Core.LiteralTypeBinary -> Syntax.TypeSpecifierNamed (Syntax.Symbol "ByteArray")
          Core.LiteralTypeBoolean -> Syntax.TypeSpecifierNamed (Syntax.Symbol "Boolean")
          Core.LiteralTypeFloat _ -> Syntax.TypeSpecifierNamed (Syntax.Symbol "Float")
          Core.LiteralTypeInteger _ -> Syntax.TypeSpecifierNamed (Syntax.Symbol "Integer")
          Core.LiteralTypeString -> Syntax.TypeSpecifierNamed (Syntax.Symbol "String"))
        Core.TypeList v0 -> Eithers.map (\enc -> Syntax.TypeSpecifierList enc) (encodeType cx g v0)
        Core.TypeSet v0 -> Eithers.map (\enc -> Syntax.TypeSpecifierSet enc) (encodeType cx g v0)
        Core.TypeMap _ -> Right (Syntax.TypeSpecifierNamed (Syntax.Symbol "Map"))
        Core.TypeMaybe v0 -> Eithers.map (\enc -> Syntax.TypeSpecifierMaybe enc) (encodeType cx g v0)
        Core.TypeEither _ -> Right (Syntax.TypeSpecifierNamed (Syntax.Symbol "Either"))
        Core.TypePair _ -> Right (Syntax.TypeSpecifierNamed (Syntax.Symbol "Pair"))
        Core.TypeFunction _ -> Right (Syntax.TypeSpecifierNamed (Syntax.Symbol "Function"))
        Core.TypeRecord _ -> Right (Syntax.TypeSpecifierNamed (Syntax.Symbol "Record"))
        Core.TypeUnion _ -> Right (Syntax.TypeSpecifierNamed (Syntax.Symbol "Union"))
        Core.TypeWrap _ -> Right (Syntax.TypeSpecifierNamed (Syntax.Symbol "Wrapper"))
        Core.TypeVariable v0 -> Right (Syntax.TypeSpecifierNamed (Syntax.Symbol (Core.unName v0)))
        Core.TypeForall v0 -> encodeType cx g (Core.forallTypeBody v0)
        _ -> Right (Syntax.TypeSpecifierNamed (Syntax.Symbol "Any"))

encodeTypeBody :: String -> Core.Type -> Core.Type -> Either t0 Syntax.TopLevelFormWithComments
encodeTypeBody lname origTyp typ =
    case typ of
      Core.TypeForall v0 -> encodeTypeBody lname origTyp (Core.forallTypeBody v0)
      Core.TypeRecord v0 ->
        let fields = Lists.map encodeFieldDef v0
        in (Right (lispTopForm (Syntax.TopLevelFormRecordType (Syntax.RecordTypeDefinition {
          Syntax.recordTypeDefinitionName = (Syntax.Symbol lname),
          Syntax.recordTypeDefinitionFields = fields,
          Syntax.recordTypeDefinitionDoc = Nothing}))))
      Core.TypeUnion v0 ->
        let variantNames =
                Lists.map (\f -> Syntax.ExpressionLiteral (Syntax.LiteralKeyword (Syntax.Keyword {
                  Syntax.keywordName = (Formatting.convertCaseCamelToLowerSnake (Core.unName (Core.fieldTypeName f))),
                  Syntax.keywordNamespace = Nothing}))) v0
        in (Right (lispTopForm (Syntax.TopLevelFormVariable (Syntax.VariableDefinition {
          Syntax.variableDefinitionName = (Syntax.Symbol (Strings.cat2 lname "-variants")),
          Syntax.variableDefinitionValue = (lispListExpr variantNames),
          Syntax.variableDefinitionDoc = (Just (Syntax.Docstring (Strings.cat2 "Variants of the " lname)))}))))
      Core.TypeWrap _ -> Right (lispTopForm (Syntax.TopLevelFormRecordType (Syntax.RecordTypeDefinition {
        Syntax.recordTypeDefinitionName = (Syntax.Symbol lname),
        Syntax.recordTypeDefinitionFields = [
          Syntax.FieldDefinition {
            Syntax.fieldDefinitionName = (Syntax.Symbol "value"),
            Syntax.fieldDefinitionDefaultValue = Nothing}],
        Syntax.recordTypeDefinitionDoc = Nothing})))
      _ -> Right (Syntax.TopLevelFormWithComments {
        Syntax.topLevelFormWithCommentsDoc = Nothing,
        Syntax.topLevelFormWithCommentsComment = (Just (Syntax.Comment {
          Syntax.commentStyle = Syntax.CommentStyleLine,
          Syntax.commentText = (Strings.cat2 (Strings.cat2 lname " = ") (Core_.type_ origTyp))})),
        Syntax.topLevelFormWithCommentsForm = (Syntax.TopLevelFormExpression (Syntax.ExpressionLiteral Syntax.LiteralNil))})

encodeTypeDefinition :: t0 -> t1 -> Module.TypeDefinition -> Either t2 Syntax.TopLevelFormWithComments
encodeTypeDefinition cx g tdef =

      let name = Module.typeDefinitionName tdef
          typ = Module.typeDefinitionType tdef
          lname = qualifiedSnakeName name
          dtyp = Rewriting.deannotateType typ
      in (encodeTypeBody lname typ dtyp)

isCasesPrimitive :: Core.Name -> Bool
isCasesPrimitive name = Equality.equal name (Core.Name "hydra.lib.maybes.cases")

isLazy2ArgPrimitive :: Core.Name -> Bool
isLazy2ArgPrimitive name =
    Logic.or (Equality.equal name (Core.Name "hydra.lib.eithers.fromLeft")) (Logic.or (Equality.equal name (Core.Name "hydra.lib.eithers.fromRight")) (Equality.equal name (Core.Name "hydra.lib.maybes.fromMaybe")))

isLazy3ArgPrimitive :: Core.Name -> Bool
isLazy3ArgPrimitive name = Equality.equal name (Core.Name "hydra.lib.maybes.maybe")

isPrimitiveRef :: String -> Core.Term -> Bool
isPrimitiveRef primName term =
    case term of
      Core.TermFunction v0 -> case v0 of
        Core.FunctionPrimitive v1 -> Equality.equal (Core.unName v1) primName
        _ -> False
      Core.TermVariable v0 -> Equality.equal (Core.unName v0) primName
      Core.TermAnnotated v0 -> isPrimitiveRef primName (Core.annotatedTermBody v0)
      Core.TermTypeApplication v0 -> isPrimitiveRef primName (Core.typeApplicationTermBody v0)
      Core.TermTypeLambda v0 -> isPrimitiveRef primName (Core.typeLambdaBody v0)
      _ -> False

lispApp :: Syntax.Expression -> [Syntax.Expression] -> Syntax.Expression
lispApp fun args =
    Syntax.ExpressionApplication (Syntax.Application {
      Syntax.applicationFunction = fun,
      Syntax.applicationArguments = args})

lispBindingMetadata :: Graph.Graph -> Core.Binding -> Maybe Core.Term
lispBindingMetadata g b = Logic.ifElse (lispShouldThunkBinding g b) (CoderUtils.bindingMetadata g b) Nothing

lispKeyword :: String -> Syntax.Expression
lispKeyword name =
    Syntax.ExpressionLiteral (Syntax.LiteralKeyword (Syntax.Keyword {
      Syntax.keywordName = name,
      Syntax.keywordNamespace = Nothing}))

lispLambdaExpr :: [String] -> Syntax.Expression -> Syntax.Expression
lispLambdaExpr params body =
    Syntax.ExpressionLambda (Syntax.Lambda {
      Syntax.lambdaName = Nothing,
      Syntax.lambdaParams = (Lists.map (\p -> Syntax.Symbol p) params),
      Syntax.lambdaRestParam = Nothing,
      Syntax.lambdaBody = [
        body]})

lispListExpr :: [Syntax.Expression] -> Syntax.Expression
lispListExpr elements =
    Syntax.ExpressionList (Syntax.ListLiteral {
      Syntax.listLiteralElements = elements,
      Syntax.listLiteralQuoted = False})

lispLitExpr :: Syntax.Literal -> Syntax.Expression
lispLitExpr lit = Syntax.ExpressionLiteral lit

lispNamedLambdaExpr :: String -> [String] -> Syntax.Expression -> Syntax.Expression
lispNamedLambdaExpr name params body =
    Syntax.ExpressionLambda (Syntax.Lambda {
      Syntax.lambdaName = (Just (Syntax.Symbol name)),
      Syntax.lambdaParams = (Lists.map (\p -> Syntax.Symbol p) params),
      Syntax.lambdaRestParam = Nothing,
      Syntax.lambdaBody = [
        body]})

lispNilExpr :: Syntax.Expression
lispNilExpr = Syntax.ExpressionLiteral Syntax.LiteralNil

lispShouldThunkBinding :: t0 -> Core.Binding -> Bool
lispShouldThunkBinding g b =

      let term = Core.bindingTerm b
          isLambda =
                  case (Rewriting.deannotateTerm term) of
                    Core.TermFunction v0 -> case v0 of
                      Core.FunctionLambda _ -> True
                      _ -> False
                    _ -> False
      in False

lispSymbol :: String -> Syntax.Symbol
lispSymbol name = Syntax.Symbol name

lispTopForm :: Syntax.TopLevelForm -> Syntax.TopLevelFormWithComments
lispTopForm form =
    Syntax.TopLevelFormWithComments {
      Syntax.topLevelFormWithCommentsDoc = Nothing,
      Syntax.topLevelFormWithCommentsComment = Nothing,
      Syntax.topLevelFormWithCommentsForm = form}

lispTopFormWithComments :: Maybe String -> Syntax.TopLevelForm -> Syntax.TopLevelFormWithComments
lispTopFormWithComments mdoc form =
    Syntax.TopLevelFormWithComments {
      Syntax.topLevelFormWithCommentsDoc = (Maybes.map (\d -> Syntax.Docstring d) mdoc),
      Syntax.topLevelFormWithCommentsComment = Nothing,
      Syntax.topLevelFormWithCommentsForm = form}

lispVar :: String -> Syntax.Expression
lispVar name =
    Syntax.ExpressionVariable (Syntax.VariableReference {
      Syntax.variableReferenceName = (Syntax.Symbol name),
      Syntax.variableReferenceFunctionNamespace = False})

moduleExports :: [Syntax.TopLevelFormWithComments] -> [Syntax.ExportDeclaration]
moduleExports forms =

      let symbols =
              Lists.concat (Lists.map (\fwc ->
                let form = Syntax.topLevelFormWithCommentsForm fwc
                in case form of
                  Syntax.TopLevelFormVariable v0 -> [
                    Syntax.variableDefinitionName v0]
                  Syntax.TopLevelFormRecordType v0 ->
                    let rname = Syntax.unSymbol (Syntax.recordTypeDefinitionName v0)
                        fields = Syntax.recordTypeDefinitionFields v0
                        fieldSyms =
                                Lists.map (\f ->
                                  let fn = Syntax.unSymbol (Syntax.fieldDefinitionName f)
                                  in (Syntax.Symbol (Strings.cat [
                                    rname,
                                    "-",
                                    fn]))) fields
                    in (Lists.concat [
                      [
                        Syntax.Symbol (Strings.cat2 "make-" rname),
                        (Syntax.Symbol (Strings.cat2 rname "?"))],
                      fieldSyms])
                  _ -> []) forms)
      in (Logic.ifElse (Lists.null symbols) [] [
        Syntax.ExportDeclaration {
          Syntax.exportDeclarationSymbols = symbols}])

moduleImports :: Module.Namespace -> [Module.Definition] -> [Syntax.ImportDeclaration]
moduleImports focusNs defs =

      let depNss = Sets.toList (Sets.delete focusNs (Schemas.definitionDependencyNamespaces defs))
      in (Lists.map (\ns -> Syntax.ImportDeclaration {
        Syntax.importDeclarationModule = (Syntax.NamespaceName (Module.unNamespace ns)),
        Syntax.importDeclarationSpec = Syntax.ImportSpecAll}) depNss)

moduleToLisp :: Syntax.Dialect -> Module.Module -> [Module.Definition] -> t0 -> Graph.Graph -> Either t1 Syntax.Program
moduleToLisp dialect mod defs0 cx g =

      let defs = CoderUtils.reorderDefs defs0
          partitioned = Schemas.partitionDefinitions defs
          allTypeDefs = Pairs.first partitioned
          termDefs = Pairs.second partitioned
          typeDefs = Lists.filter (\td -> Schemas.isNominalType (Module.typeDefinitionType td)) allTypeDefs
      in (Eithers.bind (Eithers.mapList (encodeTypeDefinition cx g) typeDefs) (\typeItems -> Eithers.bind (Eithers.mapList (encodeTermDefinition dialect cx g) termDefs) (\termItems ->
        let allItems = Lists.concat2 typeItems termItems
            nsName = Module.unNamespace (Module.moduleNamespace mod)
            focusNs = Module.moduleNamespace mod
            imports = moduleImports focusNs defs
            exports = moduleExports allItems
        in (Right (Syntax.Program {
          Syntax.programDialect = dialect,
          Syntax.programModule = (Just (Syntax.ModuleDeclaration {
            Syntax.moduleDeclarationName = (Syntax.NamespaceName nsName),
            Syntax.moduleDeclarationDoc = Nothing})),
          Syntax.programImports = imports,
          Syntax.programExports = exports,
          Syntax.programForms = allItems})))))

qualifiedSnakeName :: Core.Name -> String
qualifiedSnakeName name =

      let raw = Core.unName name
          parts = Strings.splitOn "." raw
          snakeParts = Lists.map (\p -> Formatting.convertCaseCamelOrUnderscoreToLowerSnake p) parts
          joined = Strings.intercalate "_" snakeParts
      in (Formatting.sanitizeWithUnderscores Language.lispReservedWords joined)

qualifiedTypeName :: Core.Name -> String
qualifiedTypeName name = Formatting.capitalize (Names.localNameOf name)

wrapInDelay :: Syntax.Expression -> Syntax.Expression
wrapInDelay expr = lispApp (lispVar "delay") [
  expr]

wrapInThunk :: Syntax.Expression -> Syntax.Expression
wrapInThunk expr =
    Syntax.ExpressionLambda (Syntax.Lambda {
      Syntax.lambdaName = Nothing,
      Syntax.lambdaParams = [],
      Syntax.lambdaRestParam = Nothing,
      Syntax.lambdaBody = [
        expr]})
