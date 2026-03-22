-- Note: this is an automatically generated file. Do not edit.

-- | Python code generator: converts Hydra modules to Python source code

module Hydra.Ext.Python.Coder where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Arity as Arity
import qualified Hydra.Checking as Checking
import qualified Hydra.CoderUtils as CoderUtils
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Ext.Python.Helpers as Helpers
import qualified Hydra.Ext.Python.Names as Names
import qualified Hydra.Ext.Python.Serde as Serde
import qualified Hydra.Ext.Python.Syntax as Syntax
import qualified Hydra.Ext.Python.Utils as Utils
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
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Version-aware inline type parameters
useInlineTypeParamsFor :: Helpers.PythonVersion -> Bool
useInlineTypeParamsFor version = Equality.equal version Helpers.PythonVersionPython312

-- | Legacy constant for backward compatibility; use useInlineTypeParamsFor in new code
useInlineTypeParams :: Bool
useInlineTypeParams = useInlineTypeParamsFor Utils.targetPythonVersion

-- | Version-aware type alias statement generation
typeAliasStatementFor :: Helpers.PythonEnvironment -> Syntax.Name -> [Syntax.TypeParameter] -> Maybe String -> Syntax.Expression -> Syntax.Statement
typeAliasStatementFor env name tparams mcomment tyexpr =
    Logic.ifElse (useInlineTypeParamsFor (Helpers.pythonEnvironmentVersion env)) (Utils.typeAliasStatement name tparams mcomment tyexpr) (Utils.typeAliasStatement310 name tparams mcomment tyexpr)

-- | Version-aware union type statement generation
unionTypeStatementsFor :: Helpers.PythonEnvironment -> Syntax.Name -> [Syntax.TypeParameter] -> Maybe String -> Syntax.Expression -> [Syntax.Statement] -> [Syntax.Statement]
unionTypeStatementsFor env name tparams mcomment tyexpr extraStmts =
    Logic.ifElse (useInlineTypeParamsFor (Helpers.pythonEnvironmentVersion env)) (Lists.concat2 [
      Utils.typeAliasStatement name tparams mcomment tyexpr] extraStmts) (Utils.unionTypeClassStatements310 name mcomment tyexpr extraStmts)

-- | Wrap a Python expression in a nullary lambda (thunk) for lazy evaluation
wrapInNullaryLambda :: Syntax.Expression -> Syntax.Expression
wrapInNullaryLambda expr =
    Syntax.ExpressionLambda (Syntax.Lambda {
      Syntax.lambdaParams = Syntax.LambdaParameters {
        Syntax.lambdaParametersSlashNoDefault = Nothing,
        Syntax.lambdaParametersParamNoDefault = [],
        Syntax.lambdaParametersParamWithDefault = [],
        Syntax.lambdaParametersStarEtc = Nothing},
      Syntax.lambdaBody = expr})

-- | Wrap specific arguments in nullary lambdas for primitives that require lazy evaluation
wrapLazyArguments :: Core.Name -> [Syntax.Expression] -> [Syntax.Expression]
wrapLazyArguments name args =
    Logic.ifElse (Logic.and (Equality.equal name (Core.Name "hydra.lib.logic.ifElse")) (Equality.equal (Lists.length args) 3)) [
      Lists.at 0 args,
      (wrapInNullaryLambda (Lists.at 1 args)),
      (wrapInNullaryLambda (Lists.at 2 args))] (Logic.ifElse (Logic.and (Equality.equal name (Core.Name "hydra.lib.maybes.cases")) (Equality.equal (Lists.length args) 3)) [
      Lists.at 0 args,
      (wrapInNullaryLambda (Lists.at 1 args)),
      (Lists.at 2 args)] (Logic.ifElse (Logic.and (Logic.or (Equality.equal name (Core.Name "hydra.lib.maybes.maybe")) (Equality.equal name (Core.Name "hydra.lib.maybes.fromMaybe"))) (Equality.gte (Lists.length args) 1)) (Lists.cons (wrapInNullaryLambda (Lists.at 0 args)) (Lists.tail args)) args))

-- | Create integer literal expression
pyInt :: Integer -> Syntax.Expression
pyInt n = Utils.pyAtomToPyExpression (Syntax.AtomNumber (Syntax.NumberInteger n))

-- | Decorator for @lru_cache(1) to memoize zero-argument function results
lruCacheDecorator :: Syntax.NamedExpression
lruCacheDecorator =
    Syntax.NamedExpressionSimple (Utils.functionCall (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "lru_cache"))) [
      pyInt 1])

-- | Create a thunk (zero-argument lambda) wrapped with lru_cache(1) for memoization
makeThunk :: Syntax.Expression -> Syntax.Expression
makeThunk pbody =
    Utils.functionCall (Utils.pyExpressionToPyPrimary (Utils.functionCall (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "lru_cache"))) [
      pyInt 1])) [
      wrapInNullaryLambda pbody]

-- | Create a curried lambda chain from a list of parameter names and a body
makeCurriedLambda :: [Syntax.Name] -> Syntax.Expression -> Syntax.Expression
makeCurriedLambda params body =
    Lists.foldl (\acc -> \p -> Syntax.ExpressionLambda (Syntax.Lambda {
      Syntax.lambdaParams = Syntax.LambdaParameters {
        Syntax.lambdaParametersSlashNoDefault = Nothing,
        Syntax.lambdaParametersParamNoDefault = [
          Syntax.LambdaParamNoDefault p],
        Syntax.lambdaParametersParamWithDefault = [],
        Syntax.lambdaParametersStarEtc = Nothing},
      Syntax.lambdaBody = acc})) body (Lists.reverse params)

-- | Create Generic[...] argument expression for class definition
genericArg :: [Core.Name] -> Maybe Syntax.Expression
genericArg tparamList =
    Logic.ifElse (Lists.null tparamList) Nothing (Just (Utils.pyPrimaryToPyExpression (Utils.primaryWithExpressionSlices (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "Generic"))) (Lists.map (\n -> Syntax.ExpressionSimple (Syntax.Disjunction [
      Syntax.Conjunction [
        Syntax.InversionSimple (Syntax.Comparison {
          Syntax.comparisonLhs = Syntax.BitwiseOr {
            Syntax.bitwiseOrLhs = Nothing,
            Syntax.bitwiseOrRhs = Syntax.BitwiseXor {
              Syntax.bitwiseXorLhs = Nothing,
              Syntax.bitwiseXorRhs = Syntax.BitwiseAnd {
                Syntax.bitwiseAndLhs = Nothing,
                Syntax.bitwiseAndRhs = Syntax.ShiftExpression {
                  Syntax.shiftExpressionLhs = Nothing,
                  Syntax.shiftExpressionRhs = Syntax.Sum {
                    Syntax.sumLhs = Nothing,
                    Syntax.sumRhs = Syntax.Term {
                      Syntax.termLhs = Nothing,
                      Syntax.termRhs = (Syntax.FactorSimple (Syntax.Power {
                        Syntax.powerLhs = Syntax.AwaitPrimary {
                          Syntax.awaitPrimaryAwait = False,
                          Syntax.awaitPrimaryPrimary = (Syntax.PrimarySimple (Syntax.AtomName (Names.encodeTypeVariable n)))},
                        Syntax.powerRhs = Nothing}))}}}}}},
          Syntax.comparisonRhs = []})]])) tparamList))))

-- | Create args for variant (Node[type], Generic[tparams])
variantArgs :: Syntax.Expression -> [Core.Name] -> Syntax.Args
variantArgs ptype tparams =
    Utils.pyExpressionsToPyArgs (Maybes.cat [
      Just (Utils.pyPrimaryToPyExpression (Utils.primaryWithExpressionSlices (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "Node"))) [
        ptype])),
      (genericArg tparams)])

-- | Get type parameters from environment as Python TypeParameters
environmentTypeParameters :: Helpers.PythonEnvironment -> [Syntax.TypeParameter]
environmentTypeParameters env =
    Lists.map (\arg_ -> Utils.pyNameToPyTypeParameter (Names.encodeTypeVariable arg_)) (Pairs.first (Helpers.pythonEnvironmentBoundTypeVariables env))

-- | Encode a float value to a Python expression
encodeFloatValue :: Core.FloatValue -> Either t0 Syntax.Expression
encodeFloatValue fv =
    case fv of
      Core.FloatValueBigfloat v0 -> Right (Utils.functionCall (Utils.pyNameToPyPrimary (Syntax.Name "Decimal")) [
        Utils.singleQuotedString (Literals.showBigfloat v0)])
      Core.FloatValueFloat32 v0 -> Right (Utils.pyAtomToPyExpression (Syntax.AtomNumber (Syntax.NumberFloat (Literals.float32ToBigfloat v0))))
      Core.FloatValueFloat64 v0 -> Right (Utils.pyAtomToPyExpression (Syntax.AtomNumber (Syntax.NumberFloat (Literals.float64ToBigfloat v0))))

-- | Encode an integer value to a Python expression
encodeIntegerValue :: Core.IntegerValue -> Either t0 Syntax.Expression
encodeIntegerValue iv =

      let toPyInt = \n -> Right (Utils.pyAtomToPyExpression (Syntax.AtomNumber (Syntax.NumberInteger n)))
      in case iv of
        Core.IntegerValueBigint v0 -> toPyInt v0
        Core.IntegerValueInt8 v0 -> toPyInt (Literals.int8ToBigint v0)
        Core.IntegerValueInt16 v0 -> toPyInt (Literals.int16ToBigint v0)
        Core.IntegerValueInt32 v0 -> toPyInt (Literals.int32ToBigint v0)
        Core.IntegerValueInt64 v0 -> toPyInt (Literals.int64ToBigint v0)
        Core.IntegerValueUint8 v0 -> toPyInt (Literals.uint8ToBigint v0)
        Core.IntegerValueUint16 v0 -> toPyInt (Literals.uint16ToBigint v0)
        Core.IntegerValueUint32 v0 -> toPyInt (Literals.uint32ToBigint v0)
        Core.IntegerValueUint64 v0 -> toPyInt (Literals.uint64ToBigint v0)

-- | Encode a literal value to a Python expression
encodeLiteral :: Core.Literal -> Either t0 Syntax.Expression
encodeLiteral lit =
    case lit of
      Core.LiteralBinary v0 ->
        let byteValues = Literals.binaryToBytes v0
        in (Right (Utils.functionCall (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "bytes"))) [
          Utils.pyAtomToPyExpression (Syntax.AtomList (Utils.pyList (Lists.map (\byteVal -> Utils.pyAtomToPyExpression (Syntax.AtomNumber (Syntax.NumberInteger (Literals.int32ToBigint byteVal)))) byteValues)))]))
      Core.LiteralBoolean v0 -> Right (Utils.pyAtomToPyExpression (Logic.ifElse v0 Syntax.AtomTrue Syntax.AtomFalse))
      Core.LiteralFloat v0 -> encodeFloatValue v0
      Core.LiteralInteger v0 -> encodeIntegerValue v0
      Core.LiteralString v0 -> Right (Utils.stringToPyExpression Syntax.QuoteStyleDouble v0)

-- | Encode a literal type to a Python type expression
encodeLiteralType :: Core.LiteralType -> Either t0 Syntax.Expression
encodeLiteralType lt =

      let findName =
              case lt of
                Core.LiteralTypeBinary -> "bytes"
                Core.LiteralTypeBoolean -> "bool"
                Core.LiteralTypeFloat v0 -> case v0 of
                  Core.FloatTypeBigfloat -> "Decimal"
                  Core.FloatTypeFloat32 -> "float"
                  Core.FloatTypeFloat64 -> "float"
                Core.LiteralTypeInteger _ -> "int"
                Core.LiteralTypeString -> "str"
      in (Right (Syntax.ExpressionSimple (Syntax.Disjunction [
        Syntax.Conjunction [
          Syntax.InversionSimple (Syntax.Comparison {
            Syntax.comparisonLhs = Syntax.BitwiseOr {
              Syntax.bitwiseOrLhs = Nothing,
              Syntax.bitwiseOrRhs = Syntax.BitwiseXor {
                Syntax.bitwiseXorLhs = Nothing,
                Syntax.bitwiseXorRhs = Syntax.BitwiseAnd {
                  Syntax.bitwiseAndLhs = Nothing,
                  Syntax.bitwiseAndRhs = Syntax.ShiftExpression {
                    Syntax.shiftExpressionLhs = Nothing,
                    Syntax.shiftExpressionRhs = Syntax.Sum {
                      Syntax.sumLhs = Nothing,
                      Syntax.sumRhs = Syntax.Term {
                        Syntax.termLhs = Nothing,
                        Syntax.termRhs = (Syntax.FactorSimple (Syntax.Power {
                          Syntax.powerLhs = Syntax.AwaitPrimary {
                            Syntax.awaitPrimaryAwait = False,
                            Syntax.awaitPrimaryPrimary = (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name findName)))},
                          Syntax.powerRhs = Nothing}))}}}}}},
            Syntax.comparisonRhs = []})]])))

-- | Encode an application type to Python expression
encodeApplicationType :: Helpers.PythonEnvironment -> Core.ApplicationType -> Either t0 Syntax.Expression
encodeApplicationType env at =

      let gatherParams =
              \t -> \ps -> case (Rewriting.deannotateType t) of
                Core.TypeApplication v0 -> gatherParams (Core.applicationTypeFunction v0) (Lists.cons (Core.applicationTypeArgument v0) ps)
                Core.TypeAnnotated _ -> (t, ps)
                Core.TypeFunction _ -> (t, ps)
                Core.TypeForall _ -> (t, ps)
                Core.TypeList _ -> (t, ps)
                Core.TypeLiteral _ -> (t, ps)
                Core.TypeMap _ -> (t, ps)
                Core.TypeMaybe _ -> (t, ps)
                Core.TypeEither _ -> (t, ps)
                Core.TypePair _ -> (t, ps)
                Core.TypeRecord _ -> (t, ps)
                Core.TypeSet _ -> (t, ps)
                Core.TypeUnion _ -> (t, ps)
                Core.TypeUnit -> (t, ps)
                Core.TypeVariable _ -> (t, ps)
                Core.TypeVoid -> (t, ps)
                Core.TypeWrap _ -> (t, ps)
          bodyAndArgs = gatherParams (Core.TypeApplication at) []
          body = Pairs.first bodyAndArgs
          args = Pairs.second bodyAndArgs
      in (Eithers.bind (encodeType env body) (\pyBody -> Eithers.bind (Eithers.mapList (encodeType env) args) (\pyArgs -> Right (Utils.primaryAndParams (Utils.pyExpressionToPyPrimary pyBody) pyArgs))))

-- | Encode a forall type to Python expression
encodeForallType :: Helpers.PythonEnvironment -> Core.ForallType -> Either t0 Syntax.Expression
encodeForallType env lt =

      let gatherParams =
              \t -> \ps -> case (Rewriting.deannotateType t) of
                Core.TypeForall v0 -> gatherParams (Core.forallTypeBody v0) (Lists.cons (Core.forallTypeParameter v0) ps)
                Core.TypeAnnotated _ -> (t, (Lists.reverse ps))
                Core.TypeApplication _ -> (t, (Lists.reverse ps))
                Core.TypeFunction _ -> (t, (Lists.reverse ps))
                Core.TypeList _ -> (t, (Lists.reverse ps))
                Core.TypeLiteral _ -> (t, (Lists.reverse ps))
                Core.TypeMap _ -> (t, (Lists.reverse ps))
                Core.TypeMaybe _ -> (t, (Lists.reverse ps))
                Core.TypeEither _ -> (t, (Lists.reverse ps))
                Core.TypePair _ -> (t, (Lists.reverse ps))
                Core.TypeRecord _ -> (t, (Lists.reverse ps))
                Core.TypeSet _ -> (t, (Lists.reverse ps))
                Core.TypeUnion _ -> (t, (Lists.reverse ps))
                Core.TypeUnit -> (t, (Lists.reverse ps))
                Core.TypeVariable _ -> (t, (Lists.reverse ps))
                Core.TypeVoid -> (t, (Lists.reverse ps))
                Core.TypeWrap _ -> (t, (Lists.reverse ps))
          bodyAndParams = gatherParams (Core.TypeForall lt) []
          body = Pairs.first bodyAndParams
          params = Pairs.second bodyAndParams
      in (Eithers.bind (encodeType env body) (\pyBody -> Right (Utils.primaryAndParams (Utils.pyExpressionToPyPrimary pyBody) (Lists.map (\n -> Syntax.ExpressionSimple (Syntax.Disjunction [
        Syntax.Conjunction [
          Syntax.InversionSimple (Syntax.Comparison {
            Syntax.comparisonLhs = Syntax.BitwiseOr {
              Syntax.bitwiseOrLhs = Nothing,
              Syntax.bitwiseOrRhs = Syntax.BitwiseXor {
                Syntax.bitwiseXorLhs = Nothing,
                Syntax.bitwiseXorRhs = Syntax.BitwiseAnd {
                  Syntax.bitwiseAndLhs = Nothing,
                  Syntax.bitwiseAndRhs = Syntax.ShiftExpression {
                    Syntax.shiftExpressionLhs = Nothing,
                    Syntax.shiftExpressionRhs = Syntax.Sum {
                      Syntax.sumLhs = Nothing,
                      Syntax.sumRhs = Syntax.Term {
                        Syntax.termLhs = Nothing,
                        Syntax.termRhs = (Syntax.FactorSimple (Syntax.Power {
                          Syntax.powerLhs = Syntax.AwaitPrimary {
                            Syntax.awaitPrimaryAwait = False,
                            Syntax.awaitPrimaryPrimary = (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name (Core.unName n))))},
                          Syntax.powerRhs = Nothing}))}}}}}},
            Syntax.comparisonRhs = []})]])) params))))

-- | Encode a function type to Python Callable expression
encodeFunctionType :: Helpers.PythonEnvironment -> Core.FunctionType -> Either t0 Syntax.Expression
encodeFunctionType env ft =

      let gatherParams =
              \rdoms -> \ftype ->
                let innerCod = Core.functionTypeCodomain ftype
                    dom = Core.functionTypeDomain ftype
                in case (Rewriting.deannotateType innerCod) of
                  Core.TypeFunction v0 -> gatherParams (Lists.cons dom rdoms) v0
                  Core.TypeAnnotated _ -> (Lists.reverse (Lists.cons dom rdoms), innerCod)
                  Core.TypeApplication _ -> (Lists.reverse (Lists.cons dom rdoms), innerCod)
                  Core.TypeForall _ -> (Lists.reverse (Lists.cons dom rdoms), innerCod)
                  Core.TypeList _ -> (Lists.reverse (Lists.cons dom rdoms), innerCod)
                  Core.TypeLiteral _ -> (Lists.reverse (Lists.cons dom rdoms), innerCod)
                  Core.TypeMap _ -> (Lists.reverse (Lists.cons dom rdoms), innerCod)
                  Core.TypeMaybe _ -> (Lists.reverse (Lists.cons dom rdoms), innerCod)
                  Core.TypeEither _ -> (Lists.reverse (Lists.cons dom rdoms), innerCod)
                  Core.TypePair _ -> (Lists.reverse (Lists.cons dom rdoms), innerCod)
                  Core.TypeRecord _ -> (Lists.reverse (Lists.cons dom rdoms), innerCod)
                  Core.TypeSet _ -> (Lists.reverse (Lists.cons dom rdoms), innerCod)
                  Core.TypeUnion _ -> (Lists.reverse (Lists.cons dom rdoms), innerCod)
                  Core.TypeUnit -> (Lists.reverse (Lists.cons dom rdoms), innerCod)
                  Core.TypeVariable _ -> (Lists.reverse (Lists.cons dom rdoms), innerCod)
                  Core.TypeVoid -> (Lists.reverse (Lists.cons dom rdoms), innerCod)
                  Core.TypeWrap _ -> (Lists.reverse (Lists.cons dom rdoms), innerCod)
          domsAndCod = gatherParams [] ft
          doms = Pairs.first domsAndCod
          cod = Pairs.second domsAndCod
      in (Eithers.bind (Eithers.mapList (encodeType env) doms) (\pydoms -> Eithers.bind (encodeType env cod) (\pycod -> Right (Utils.pyPrimaryToPyExpression (Utils.primaryWithSlices (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "Callable"))) (Utils.pyPrimaryToPySlice (Syntax.PrimarySimple (Syntax.AtomList (Utils.pyList pydoms)))) [
        Syntax.SliceOrStarredExpressionSlice (Utils.pyExpressionToPySlice pycod)])))))

-- | Encode a Hydra type to a Python type expression
encodeType :: Helpers.PythonEnvironment -> Core.Type -> Either t0 Syntax.Expression
encodeType env typ =

      let dflt = Right (Utils.doubleQuotedString (Strings.cat2 "type = " (Core_.type_ (Rewriting.deannotateType typ))))
      in case (Rewriting.deannotateType typ) of
        Core.TypeApplication v0 -> encodeApplicationType env v0
        Core.TypeFunction v0 -> encodeFunctionType env v0
        Core.TypeForall v0 -> encodeForallType env v0
        Core.TypeList v0 -> Eithers.bind (encodeType env v0) (\pyet -> Right (Utils.nameAndParams (Syntax.Name "frozenlist") [
          pyet]))
        Core.TypeMap v0 -> Eithers.bind (encodeType env (Core.mapTypeKeys v0)) (\pykt -> Eithers.bind (encodeType env (Core.mapTypeValues v0)) (\pyvt -> Right (Utils.nameAndParams (Syntax.Name "FrozenDict") [
          pykt,
          pyvt])))
        Core.TypeLiteral v0 -> encodeLiteralType v0
        Core.TypeMaybe v0 -> Eithers.bind (encodeType env v0) (\ptype -> Right (Utils.pyPrimaryToPyExpression (Utils.primaryWithExpressionSlices (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "Maybe"))) [
          ptype])))
        Core.TypeEither v0 -> Eithers.bind (encodeType env (Core.eitherTypeLeft v0)) (\pyleft -> Eithers.bind (encodeType env (Core.eitherTypeRight v0)) (\pyright -> Right (Utils.pyPrimaryToPyExpression (Utils.primaryWithExpressionSlices (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "Either"))) [
          pyleft,
          pyright]))))
        Core.TypePair v0 -> Eithers.bind (encodeType env (Core.pairTypeFirst v0)) (\pyFirst -> Eithers.bind (encodeType env (Core.pairTypeSecond v0)) (\pySecond -> Right (Utils.nameAndParams (Syntax.Name "tuple") [
          pyFirst,
          pySecond])))
        Core.TypeRecord _ -> dflt
        Core.TypeSet v0 -> Eithers.bind (encodeType env v0) (\pyet -> Right (Utils.nameAndParams (Syntax.Name "frozenset") [
          pyet]))
        Core.TypeUnion _ -> dflt
        Core.TypeUnit -> Right (Utils.pyNameToPyExpression Utils.pyNone)
        Core.TypeVoid -> Right (Utils.pyNameToPyExpression Utils.pyNone)
        Core.TypeVariable v0 -> Right (Names.typeVariableReference env v0)
        Core.TypeWrap _ -> dflt
        Core.TypeAnnotated _ -> dflt

-- | Encode a type to a Python expression, quoting if the type has free variables
encodeTypeQuoted :: Helpers.PythonEnvironment -> Core.Type -> Either t0 Syntax.Expression
encodeTypeQuoted env typ =
    Eithers.bind (encodeType env typ) (\pytype -> Right (Logic.ifElse (Sets.null (Rewriting.freeVariablesInType typ)) pytype (Utils.doubleQuotedString (Serialization.printExpr (Serde.encodeExpression pytype)))))

-- | Generate name constants for a type as class-level attributes
encodeNameConstants :: Helpers.PythonEnvironment -> Core.Name -> [Core.FieldType] -> [Syntax.Statement]
encodeNameConstants env name fields =

      let toStmt =
              \pair -> Utils.assignmentStatement (Pairs.first pair) (Utils.functionCall (Utils.pyNameToPyPrimary (Names.encodeName True Util.CaseConventionPascal env (Core.Name "hydra.core.Name"))) [
                Utils.doubleQuotedString (Core.unName (Pairs.second pair))])
          namePair = (Names.encodeConstantForTypeName env name, name)
          fieldPairs =
                  Lists.map (\field -> (Names.encodeConstantForFieldName env name (Core.fieldTypeName field), (Core.fieldTypeName field))) fields
      in (Lists.map toStmt (Lists.cons namePair fieldPairs))

-- | Find type parameters in a type that are bound in the environment
findTypeParams :: Helpers.PythonEnvironment -> Core.Type -> [Core.Name]
findTypeParams env typ =

      let boundVars = Pairs.second (Helpers.pythonEnvironmentBoundTypeVariables env)
          isBound = \v -> Maybes.isJust (Maps.lookup v boundVars)
      in (Lists.filter isBound (Sets.toList (Rewriting.freeVariablesInType typ)))

-- | Encode a wrapped type (newtype) to a Python class definition
encodeWrappedType :: Helpers.PythonEnvironment -> Core.Name -> Core.Type -> Maybe String -> Either t0 [Syntax.Statement]
encodeWrappedType env name typ comment =

      let tparamList = Pairs.first (Helpers.pythonEnvironmentBoundTypeVariables env)
      in (Eithers.bind (encodeTypeQuoted env typ) (\ptypeQuoted ->
        let pyName = Names.encodeName False Util.CaseConventionPascal env name
            body = Utils.indentedBlock comment []
            typeConstStmt =
                    Utils.dottedAssignmentStatement pyName (Names.encodeConstantForTypeName env name) (Utils.functionCall (Utils.pyNameToPyPrimary (Names.encodeName True Util.CaseConventionPascal env (Core.Name "hydra.core.Name"))) [
                      Utils.doubleQuotedString (Core.unName name)])
        in (Right [
          Utils.pyClassDefinitionToPyStatement (Syntax.ClassDefinition {
            Syntax.classDefinitionDecorators = Nothing,
            Syntax.classDefinitionName = pyName,
            Syntax.classDefinitionTypeParams = (Lists.map (\arg_ -> Utils.pyNameToPyTypeParameter (Names.encodeTypeVariable arg_)) (findTypeParams env typ)),
            Syntax.classDefinitionArguments = (Just (variantArgs ptypeQuoted tparamList)),
            Syntax.classDefinitionBody = body}),
          typeConstStmt])))

-- | Extend a PythonEnvironment with a new bound type variable
extendEnvWithTypeVar :: Helpers.PythonEnvironment -> Core.Name -> Helpers.PythonEnvironment
extendEnvWithTypeVar env var_ =

      let oldBound = Helpers.pythonEnvironmentBoundTypeVariables env
          tparamList = Pairs.first oldBound
          tparamMap = Pairs.second oldBound
          newList = Lists.concat2 tparamList [
                var_]
          newMap = Maps.insert var_ (Names.encodeTypeVariable var_) tparamMap
      in Helpers.PythonEnvironment {
        Helpers.pythonEnvironmentNamespaces = (Helpers.pythonEnvironmentNamespaces env),
        Helpers.pythonEnvironmentBoundTypeVariables = (newList, newMap),
        Helpers.pythonEnvironmentGraph = (Helpers.pythonEnvironmentGraph env),
        Helpers.pythonEnvironmentNullaryBindings = (Helpers.pythonEnvironmentNullaryBindings env),
        Helpers.pythonEnvironmentVersion = (Helpers.pythonEnvironmentVersion env),
        Helpers.pythonEnvironmentSkipCasts = (Helpers.pythonEnvironmentSkipCasts env),
        Helpers.pythonEnvironmentInlineVariables = (Helpers.pythonEnvironmentInlineVariables env)}

-- | Extract lambdas and their bodies from a term
gatherLambdas :: Core.Term -> ([Core.Name], Core.Term)
gatherLambdas term =

      let go =
              \params -> \t -> case (Rewriting.deannotateAndDetypeTerm t) of
                Core.TermFunction v0 -> case v0 of
                  Core.FunctionLambda v1 -> go (Lists.concat2 params [
                    Core.lambdaParameter v1]) (Core.lambdaBody v1)
                  _ -> (params, t)
                _ -> (params, t)
      in (go [] term)

-- | Extend environment with lambda parameters from a term
extendEnvWithLambdaParams :: Helpers.PythonEnvironment -> Core.Term -> Helpers.PythonEnvironment
extendEnvWithLambdaParams env term =

      let go =
              \e -> \t -> case (Rewriting.deannotateAndDetypeTerm t) of
                Core.TermFunction v0 -> case v0 of
                  Core.FunctionLambda v1 ->
                    let newTc = Rewriting.extendGraphForLambda (pythonEnvironmentGetGraph e) v1
                        newEnv = pythonEnvironmentSetGraph newTc e
                    in (go newEnv (Core.lambdaBody v1))
                  _ -> e
                _ -> e
      in (go env term)

-- | Wrap a bare reference to a polymorphic function in an uncurried lambda
makeSimpleLambda :: Int -> Syntax.Expression -> Syntax.Expression
makeSimpleLambda arity lhs =

      let args = Lists.map (\i -> Syntax.Name (Strings.cat2 "x" (Literals.showInt32 i))) (Math.range 1 arity)
      in (Logic.ifElse (Equality.equal arity 0) lhs (Syntax.ExpressionLambda (Syntax.Lambda {
        Syntax.lambdaParams = Syntax.LambdaParameters {
          Syntax.lambdaParametersSlashNoDefault = Nothing,
          Syntax.lambdaParametersParamNoDefault = (Lists.map (\a -> Syntax.LambdaParamNoDefault a) args),
          Syntax.lambdaParametersParamWithDefault = [],
          Syntax.lambdaParametersStarEtc = Nothing},
        Syntax.lambdaBody = (Utils.functionCall (Utils.pyExpressionToPyPrimary lhs) (Lists.map (\a -> Syntax.ExpressionSimple (Syntax.Disjunction [
          Syntax.Conjunction [
            Syntax.InversionSimple (Syntax.Comparison {
              Syntax.comparisonLhs = Syntax.BitwiseOr {
                Syntax.bitwiseOrLhs = Nothing,
                Syntax.bitwiseOrRhs = Syntax.BitwiseXor {
                  Syntax.bitwiseXorLhs = Nothing,
                  Syntax.bitwiseXorRhs = Syntax.BitwiseAnd {
                    Syntax.bitwiseAndLhs = Nothing,
                    Syntax.bitwiseAndRhs = Syntax.ShiftExpression {
                      Syntax.shiftExpressionLhs = Nothing,
                      Syntax.shiftExpressionRhs = Syntax.Sum {
                        Syntax.sumLhs = Nothing,
                        Syntax.sumRhs = Syntax.Term {
                          Syntax.termLhs = Nothing,
                          Syntax.termRhs = (Syntax.FactorSimple (Syntax.Power {
                            Syntax.powerLhs = Syntax.AwaitPrimary {
                              Syntax.awaitPrimaryAwait = False,
                              Syntax.awaitPrimaryPrimary = (Syntax.PrimarySimple (Syntax.AtomName a))},
                            Syntax.powerRhs = Nothing}))}}}}}},
              Syntax.comparisonRhs = []})]])) args))})))

-- | Check if a term is a case statement applied to exactly one argument
isCaseStatementApplication :: Core.Term -> Maybe (Core.Name, (Maybe Core.Term, ([Core.Field], Core.Term)))
isCaseStatementApplication term =

      let gathered = CoderUtils.gatherApplications term
          args = Pairs.first gathered
          body = Pairs.second gathered
      in (Logic.ifElse (Logic.not (Equality.equal (Lists.length args) 1)) Nothing (
        let arg = Lists.head args
        in case (Rewriting.deannotateAndDetypeTerm body) of
          Core.TermFunction v0 -> case v0 of
            Core.FunctionElimination v1 -> case v1 of
              Core.EliminationUnion v2 -> Just (Core.caseStatementTypeName v2, (Core.caseStatementDefault v2, (Core.caseStatementCases v2, arg)))
              _ -> Nothing
            _ -> Nothing
          _ -> Nothing))

-- | Check if a variant field has unit type
isVariantUnitType :: [Core.FieldType] -> Core.Name -> Bool
isVariantUnitType rowType fieldName =

      let mfield = Lists.find (\ft -> Equality.equal (Core.fieldTypeName ft) fieldName) rowType
      in (Maybes.fromMaybe False (Maybes.map (\ft -> Schemas.isUnitType (Rewriting.deannotateType (Core.fieldTypeType ft))) mfield))

-- | Create a wildcard case block with a given body statement
wildcardCaseBlock :: Syntax.Statement -> Syntax.CaseBlock
wildcardCaseBlock stmt =
    Syntax.CaseBlock {
      Syntax.caseBlockPatterns = (Utils.pyClosedPatternToPyPatterns Syntax.ClosedPatternWildcard),
      Syntax.caseBlockGuard = Nothing,
      Syntax.caseBlockBody = (Utils.indentedBlock Nothing [
        [
          stmt]])}

-- | Create a value pattern for an enum variant
enumVariantPattern :: Helpers.PythonEnvironment -> Core.Name -> Core.Name -> Syntax.ClosedPattern
enumVariantPattern env typeName fieldName =
    Syntax.ClosedPatternValue (Syntax.ValuePattern (Syntax.Attribute [
      Names.encodeName True Util.CaseConventionPascal env typeName,
      (Names.encodeEnumValue env fieldName)]))

-- | Create a class pattern for a unit variant (no value captured)
classVariantPatternUnit :: Syntax.Name -> Syntax.ClosedPattern
classVariantPatternUnit pyVariantName =
    Syntax.ClosedPatternClass (Syntax.ClassPattern {
      Syntax.classPatternNameOrAttribute = (Syntax.NameOrAttribute [
        pyVariantName]),
      Syntax.classPatternPositionalPatterns = Nothing,
      Syntax.classPatternKeywordPatterns = Nothing})

-- | Create a class pattern for a variant with captured value
classVariantPatternWithCapture :: Helpers.PythonEnvironment -> Syntax.Name -> Core.Name -> Syntax.ClosedPattern
classVariantPatternWithCapture env pyVariantName varName =

      let pyVarNameAttr = Syntax.NameOrAttribute [
            pyVariantName]
          capturePattern =
                  Syntax.ClosedPatternCapture (Syntax.CapturePattern (Syntax.PatternCaptureTarget (Names.encodeName False Util.CaseConventionLowerSnake env varName)))
          keywordPattern =
                  Syntax.KeywordPattern {
                    Syntax.keywordPatternName = (Syntax.Name "value"),
                    Syntax.keywordPatternPattern = (Syntax.PatternOr (Syntax.OrPattern [
                      capturePattern]))}
      in (Syntax.ClosedPatternClass (Syntax.ClassPattern {
        Syntax.classPatternNameOrAttribute = pyVarNameAttr,
        Syntax.classPatternPositionalPatterns = Nothing,
        Syntax.classPatternKeywordPatterns = (Just (Syntax.KeywordPatterns [
          keywordPattern]))}))

-- | Check if union cases are fully covered
isCasesFull :: [t0] -> [t1] -> Bool
isCasesFull rowType cases_ =

      let numCases = Lists.length cases_
          numFields = Lists.length rowType
      in (Logic.not (Equality.lt numCases numFields))

-- | Create a ClosedPattern for a variant based on its characteristics
variantClosedPattern :: Helpers.PythonEnvironment -> Core.Name -> Core.Name -> Syntax.Name -> t0 -> Bool -> Core.Name -> Bool -> Syntax.ClosedPattern
variantClosedPattern env typeName fieldName pyVariantName rowType isEnum varName shouldCapture =
    Logic.ifElse isEnum (enumVariantPattern env typeName fieldName) (Logic.ifElse (Logic.not shouldCapture) (classVariantPatternUnit pyVariantName) (classVariantPatternWithCapture env pyVariantName varName))

-- | Rewrite case statements to avoid variable name collisions
deduplicateCaseVariables :: [Core.Field] -> [Core.Field]
deduplicateCaseVariables cases_ =

      let rewriteCase =
              \state -> \field ->
                let countByName = Pairs.first state
                    done = Pairs.second state
                    fname = Core.fieldName field
                    fterm = Core.fieldTerm field
                in case (Rewriting.deannotateAndDetypeTerm fterm) of
                  Core.TermFunction v0 -> case v0 of
                    Core.FunctionLambda v1 ->
                      let v = Core.lambdaParameter v1
                          mdom = Core.lambdaDomain v1
                          body = Core.lambdaBody v1
                      in (Maybes.maybe (Maps.insert v 1 countByName, (Lists.cons field done)) (\count ->
                        let count2 = Math.add count 1
                            v2 = Core.Name (Strings.cat2 (Core.unName v) (Literals.showInt32 count2))
                            newBody = Reduction.alphaConvert v v2 body
                            newLam =
                                    Core.Lambda {
                                      Core.lambdaParameter = v2,
                                      Core.lambdaDomain = mdom,
                                      Core.lambdaBody = newBody}
                            newTerm = Core.TermFunction (Core.FunctionLambda newLam)
                            newField =
                                    Core.Field {
                                      Core.fieldName = fname,
                                      Core.fieldTerm = newTerm}
                        in (Maps.insert v count2 countByName, (Lists.cons newField done))) (Maps.lookup v countByName))
                    _ -> (countByName, (Lists.cons field done))
                  _ -> (countByName, (Lists.cons field done))
          result = Lists.foldl rewriteCase (Maps.empty, []) cases_
      in (Lists.reverse (Pairs.second result))

-- | Substitute unit for a variable in a term (for unit variant case handling)
eliminateUnitVar :: Core.Name -> Core.Term -> Core.Term
eliminateUnitVar v term0 =

      let rewriteField =
              \rewrite -> \fld -> Core.Field {
                Core.fieldName = (Core.fieldName fld),
                Core.fieldTerm = (rewrite (Core.fieldTerm fld))}
          rewriteBinding =
                  \rewrite -> \bnd -> Core.Binding {
                    Core.bindingName = (Core.bindingName bnd),
                    Core.bindingTerm = (rewrite (Core.bindingTerm bnd)),
                    Core.bindingType = (Core.bindingType bnd)}
          rewrite =
                  \recurse -> \term -> case (Rewriting.deannotateAndDetypeTerm term) of
                    Core.TermVariable v0 -> Logic.ifElse (Equality.equal v0 v) Core.TermUnit term
                    Core.TermAnnotated v0 -> Core.TermAnnotated (Core.AnnotatedTerm {
                      Core.annotatedTermBody = (recurse (Core.annotatedTermBody v0)),
                      Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})
                    Core.TermApplication v0 -> Core.TermApplication (Core.Application {
                      Core.applicationFunction = (recurse (Core.applicationFunction v0)),
                      Core.applicationArgument = (recurse (Core.applicationArgument v0))})
                    Core.TermFunction v0 -> case v0 of
                      Core.FunctionLambda v1 -> Logic.ifElse (Equality.equal (Core.lambdaParameter v1) v) term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.lambdaParameter v1),
                        Core.lambdaDomain = (Core.lambdaDomain v1),
                        Core.lambdaBody = (recurse (Core.lambdaBody v1))})))
                      Core.FunctionElimination v1 -> case v1 of
                        Core.EliminationUnion v2 -> Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.caseStatementTypeName v2),
                          Core.caseStatementDefault = (Maybes.map recurse (Core.caseStatementDefault v2)),
                          Core.caseStatementCases = (Lists.map (rewriteField recurse) (Core.caseStatementCases v2))})))
                        _ -> term
                      _ -> term
                    Core.TermLet v0 -> Core.TermLet (Core.Let {
                      Core.letBindings = (Lists.map (rewriteBinding recurse) (Core.letBindings v0)),
                      Core.letBody = (recurse (Core.letBody v0))})
                    Core.TermList v0 -> Core.TermList (Lists.map recurse v0)
                    Core.TermMap v0 -> Core.TermMap (Maps.fromList (Lists.map (\kv -> (recurse (Pairs.first kv), (recurse (Pairs.second kv)))) (Maps.toList v0)))
                    Core.TermRecord v0 -> Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.recordTypeName v0),
                      Core.recordFields = (Lists.map (rewriteField recurse) (Core.recordFields v0))})
                    Core.TermSet v0 -> Core.TermSet (Sets.map recurse v0)
                    Core.TermUnion v0 -> Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = (Core.injectionTypeName v0),
                      Core.injectionField = (rewriteField recurse (Core.injectionField v0))})
                    Core.TermMaybe v0 -> Core.TermMaybe (Maybes.map recurse v0)
                    Core.TermPair v0 -> Core.TermPair (recurse (Pairs.first v0), (recurse (Pairs.second v0)))
                    Core.TermWrap v0 -> Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = (Core.wrappedTermTypeName v0),
                      Core.wrappedTermBody = (recurse (Core.wrappedTermBody v0))})
                    Core.TermEither v0 -> Core.TermEither (Eithers.bimap recurse recurse v0)
                    Core.TermTypeApplication v0 -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (recurse (Core.typeApplicationTermBody v0)),
                      Core.typeApplicationTermType = (Core.typeApplicationTermType v0)})
                    Core.TermTypeLambda v0 -> Core.TermTypeLambda (Core.TypeLambda {
                      Core.typeLambdaParameter = (Core.typeLambdaParameter v0),
                      Core.typeLambdaBody = (recurse (Core.typeLambdaBody v0))})
                    _ -> term
          go = \term -> rewrite go term
      in (go term0)

-- | Encode the default (wildcard) case block for a match statement
encodeDefaultCaseBlock :: (t0 -> Either t1 Syntax.Expression) -> Bool -> Maybe t0 -> Core.Name -> Either t1 [Syntax.CaseBlock]
encodeDefaultCaseBlock encodeTerm isFull mdflt tname =
    Eithers.bind (Maybes.maybe (Right (Logic.ifElse isFull (Utils.raiseAssertionError "Unreachable: all variants handled") (Utils.raiseTypeError (Strings.cat2 "Unsupported " (Names_.localNameOf tname))))) (\d -> Eithers.bind (encodeTerm d) (\pyexpr -> Right (Utils.returnSingle pyexpr))) mdflt) (\stmt ->
      let patterns = Utils.pyClosedPatternToPyPatterns Syntax.ClosedPatternWildcard
          body = Utils.indentedBlock Nothing [
                [
                  stmt]]
      in (Right [
        Syntax.CaseBlock {
          Syntax.caseBlockPatterns = patterns,
          Syntax.caseBlockGuard = Nothing,
          Syntax.caseBlockBody = body}]))

-- | Encode a single case (Field) into a CaseBlock for a match statement
encodeCaseBlock :: t0 -> Helpers.PythonEnvironment -> Core.Name -> [Core.FieldType] -> Bool -> (Helpers.PythonEnvironment -> Core.Term -> Either t1 [Syntax.Statement]) -> Core.Field -> Either t1 Syntax.CaseBlock
encodeCaseBlock cx env tname rowType isEnum encodeBody field =

      let fname = Core.fieldName field
          fterm = Core.fieldTerm field
          stripped = Rewriting.deannotateAndDetypeTerm fterm
          effectiveLambda =
                  case stripped of
                    Core.TermFunction v0 -> case v0 of
                      Core.FunctionLambda v1 -> v1
                      _ ->
                        let syntheticVar2 = Core.Name "_matchValue"
                        in Core.Lambda {
                          Core.lambdaParameter = syntheticVar2,
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = stripped,
                            Core.applicationArgument = (Core.TermVariable syntheticVar2)}))}
                    _ ->
                      let syntheticVar = Core.Name "_matchValue"
                      in Core.Lambda {
                        Core.lambdaParameter = syntheticVar,
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = stripped,
                          Core.applicationArgument = (Core.TermVariable syntheticVar)}))}
          v = Core.lambdaParameter effectiveLambda
          rawBody = Core.lambdaBody effectiveLambda
          isUnitVariant = isVariantUnitType rowType fname
          effectiveBody = Logic.ifElse isUnitVariant (eliminateUnitVar v rawBody) rawBody
          shouldCapture =
                  Logic.not (Logic.or isUnitVariant (Logic.or (Rewriting.isFreeVariableInTerm v rawBody) (Schemas.isUnitTerm rawBody)))
          env2 = pythonEnvironmentSetGraph (Rewriting.extendGraphForLambda (pythonEnvironmentGetGraph env) effectiveLambda) env
          pyVariantName = deconflictVariantName True env2 tname fname (Helpers.pythonEnvironmentGraph env2)
          pattern = variantClosedPattern env2 tname fname pyVariantName rowType isEnum v shouldCapture
      in (Eithers.bind (encodeBody env2 effectiveBody) (\stmts ->
        let pyBody = Utils.indentedBlock Nothing [
              stmts]
        in (Right (Syntax.CaseBlock {
          Syntax.caseBlockPatterns = (Utils.pyClosedPatternToPyPatterns pattern),
          Syntax.caseBlockGuard = Nothing,
          Syntax.caseBlockBody = pyBody}))))

-- | Accessor for the graph field of PyGraph
pyGraphGraph :: Helpers.PyGraph -> Graph.Graph
pyGraphGraph pyg = Helpers.pyGraphGraph pyg

-- | Accessor for the metadata field of PyGraph
pyGraphMetadata :: Helpers.PyGraph -> Helpers.PythonModuleMetadata
pyGraphMetadata pyg = Helpers.pyGraphMetadata pyg

-- | Constructor for PyGraph record
makePyGraph :: Graph.Graph -> Helpers.PythonModuleMetadata -> Helpers.PyGraph
makePyGraph g m =
    Helpers.PyGraph {
      Helpers.pyGraphGraph = g,
      Helpers.pyGraphMetadata = m}

-- | Encode a field type for record definitions (field: type annotation)
encodeFieldType :: Context.Context -> Helpers.PythonEnvironment -> Core.FieldType -> Either (Context.InContext Errors.Error) Syntax.Statement
encodeFieldType cx env fieldType =

      let fname = Core.fieldTypeName fieldType
          ftype = Core.fieldTypeType fieldType
      in (Eithers.bind (Annotations.getTypeDescription cx (pythonEnvironmentGetGraph env) ftype) (\comment ->
        let pyName = Syntax.SingleTargetName (Names.encodeFieldName env fname)
        in (Eithers.bind (encodeType env ftype) (\pyType ->
          let annotatedPyType = Utils.annotatedExpression comment pyType
          in (Right (Utils.pyAssignmentToPyStatement (Syntax.AssignmentTyped (Syntax.TypedAssignment {
            Syntax.typedAssignmentLhs = pyName,
            Syntax.typedAssignmentType = annotatedPyType,
            Syntax.typedAssignmentRhs = Nothing}))))))))

-- | Create a @dataclass(frozen=True) decorator
dataclassDecorator :: Syntax.NamedExpression
dataclassDecorator =
    Syntax.NamedExpressionSimple (Utils.pyPrimaryToPyExpression (Utils.primaryWithRhs (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "dataclass"))) (Syntax.PrimaryRhsCall (Syntax.Args {
      Syntax.argsPositional = [],
      Syntax.argsKwargOrStarred = [
        Syntax.KwargOrStarredKwarg (Syntax.Kwarg {
          Syntax.kwargName = (Syntax.Name "frozen"),
          Syntax.kwargValue = (Utils.pyAtomToPyExpression Syntax.AtomTrue)})],
      Syntax.argsKwargOrDoubleStarred = []}))))

-- | Encode a record type as a Python dataclass
encodeRecordType :: Context.Context -> Helpers.PythonEnvironment -> Core.Name -> [Core.FieldType] -> Maybe String -> Either (Context.InContext Errors.Error) Syntax.Statement
encodeRecordType cx env name rowType comment =
    Eithers.bind (Eithers.mapList (encodeFieldType cx env) rowType) (\pyFields ->
      let constStmts = encodeNameConstants env name rowType
          body =
                  Utils.indentedBlock comment [
                    pyFields,
                    constStmts]
          boundVars = Helpers.pythonEnvironmentBoundTypeVariables env
          tparamList = Pairs.first boundVars
          mGenericArg = genericArg tparamList
          args = Maybes.maybe Nothing (\a -> Just (Utils.pyExpressionsToPyArgs [
                a])) mGenericArg
          decs = Just (Syntax.Decorators [
                dataclassDecorator])
          pyName = Names.encodeName False Util.CaseConventionPascal env name
          noTypeParams = []
      in (Right (Utils.pyClassDefinitionToPyStatement (Syntax.ClassDefinition {
        Syntax.classDefinitionDecorators = decs,
        Syntax.classDefinitionName = pyName,
        Syntax.classDefinitionTypeParams = noTypeParams,
        Syntax.classDefinitionArguments = args,
        Syntax.classDefinitionBody = body}))))

-- | Encode an enum value assignment statement with optional comment
encodeEnumValueAssignment :: Context.Context -> Helpers.PythonEnvironment -> Core.FieldType -> Either (Context.InContext Errors.Error) [Syntax.Statement]
encodeEnumValueAssignment cx env fieldType =

      let fname = Core.fieldTypeName fieldType
          ftype = Core.fieldTypeType fieldType
      in (Eithers.bind (Annotations.getTypeDescription cx (pythonEnvironmentGetGraph env) ftype) (\mcomment ->
        let pyName = Names.encodeEnumValue env fname
            fnameStr = Core.unName fname
            pyValue =
                    Utils.functionCall (Utils.pyNameToPyPrimary (Names.encodeName True Util.CaseConventionPascal env (Core.Name "hydra.core.Name"))) [
                      Utils.doubleQuotedString fnameStr]
            assignStmt = Utils.assignmentStatement pyName pyValue
        in (Right (Maybes.maybe [
          assignStmt] (\c -> [
          assignStmt,
          (Utils.pyExpressionToPyStatement (Utils.tripleQuotedString c))]) mcomment))))

-- | Deconflict a variant name to avoid collisions with type names
deconflictVariantName :: Bool -> Helpers.PythonEnvironment -> Core.Name -> Core.Name -> Graph.Graph -> Syntax.Name
deconflictVariantName isQualified env unionName fname g =

      let candidateHydraName = Core.Name (Strings.cat2 (Core.unName unionName) (Formatting.capitalize (Core.unName fname)))
          elements = Lexical.graphToBindings g
          collision =
                  Maybes.isJust (Lists.find (\b -> Equality.equal (Core.unName (Core.bindingName b)) (Core.unName candidateHydraName)) elements)
      in (Logic.ifElse collision (Syntax.Name (Strings.cat2 (Syntax.unName (Names.variantName isQualified env unionName fname)) "_")) (Names.variantName isQualified env unionName fname))

-- | Encode a union field as a variant class
encodeUnionField :: Context.Context -> Helpers.PythonEnvironment -> Core.Name -> Core.FieldType -> Either (Context.InContext Errors.Error) Syntax.Statement
encodeUnionField cx env unionName fieldType =

      let fname = Core.fieldTypeName fieldType
          ftype = Core.fieldTypeType fieldType
      in (Eithers.bind (Annotations.getTypeDescription cx (pythonEnvironmentGetGraph env) ftype) (\fcomment ->
        let isUnit = Equality.equal (Rewriting.deannotateType ftype) Core.TypeUnit
            varName = deconflictVariantName False env unionName fname (Helpers.pythonEnvironmentGraph env)
            tparamNames = findTypeParams env ftype
            tparamPyNames = Lists.map Names.encodeTypeVariable tparamNames
            fieldParams = Lists.map Utils.pyNameToPyTypeParameter tparamPyNames
            body =
                    Logic.ifElse isUnit (Utils.indentedBlock fcomment [
                      Utils.unitVariantMethods varName]) (Utils.indentedBlock fcomment [])
        in (Eithers.bind (Logic.ifElse isUnit (Right Nothing) (Eithers.bind (encodeTypeQuoted env ftype) (\quotedType -> Right (Just (variantArgs quotedType []))))) (\margs -> Right (Utils.pyClassDefinitionToPyStatement (Syntax.ClassDefinition {
          Syntax.classDefinitionDecorators = Nothing,
          Syntax.classDefinitionName = varName,
          Syntax.classDefinitionTypeParams = fieldParams,
          Syntax.classDefinitionArguments = margs,
          Syntax.classDefinitionBody = body}))))))

-- | Encode a union type as an enum (for unit-only fields) or variant classes
encodeUnionType :: Context.Context -> Helpers.PythonEnvironment -> Core.Name -> [Core.FieldType] -> Maybe String -> Either (Context.InContext Errors.Error) [Syntax.Statement]
encodeUnionType cx env name rowType comment =
    Logic.ifElse (Schemas.isEnumRowType rowType) (Eithers.bind (Eithers.mapList (encodeEnumValueAssignment cx env) rowType) (\vals ->
      let body = Utils.indentedBlock comment vals
          enumName = Syntax.Name "Enum"
          args = Just (Utils.pyExpressionsToPyArgs [
                Utils.pyNameToPyExpression enumName])
          pyName = Names.encodeName False Util.CaseConventionPascal env name
          typeConstStmt =
                  Utils.dottedAssignmentStatement pyName (Names.encodeConstantForTypeName env name) (Utils.functionCall (Utils.pyNameToPyPrimary (Names.encodeName True Util.CaseConventionPascal env (Core.Name "hydra.core.Name"))) [
                    Utils.doubleQuotedString (Core.unName name)])
      in (Right [
        Utils.pyClassDefinitionToPyStatement (Syntax.ClassDefinition {
          Syntax.classDefinitionDecorators = Nothing,
          Syntax.classDefinitionName = pyName,
          Syntax.classDefinitionTypeParams = [],
          Syntax.classDefinitionArguments = args,
          Syntax.classDefinitionBody = body}),
        typeConstStmt]))) (
      let constStmts = encodeNameConstants env name rowType
      in (Eithers.bind (Eithers.mapList (encodeUnionField cx env name) rowType) (\fieldStmts ->
        let tparams = environmentTypeParameters env
            unionAlts = Lists.map (encodeUnionFieldAlt env name) rowType
            unionStmts =
                    unionTypeStatementsFor env (Names.encodeName False Util.CaseConventionPascal env name) tparams comment (Utils.orExpression unionAlts) constStmts
        in (Right (Lists.concat2 fieldStmts unionStmts)))))

-- | Encode a union field as a primary expression for | alternatives
encodeUnionFieldAlt :: Helpers.PythonEnvironment -> Core.Name -> Core.FieldType -> Syntax.Primary
encodeUnionFieldAlt env unionName fieldType =

      let fname = Core.fieldTypeName fieldType
          ftype = Core.fieldTypeType fieldType
          tparamNames = findTypeParams env ftype
          tparams = Lists.map Names.encodeTypeVariable tparamNames
          namePrim = Utils.pyNameToPyPrimary (Names.variantName False env unionName fname)
      in (Logic.ifElse (Lists.null tparams) namePrim (
        let tparamExprs = Lists.map Utils.pyNameToPyExpression tparams
        in (Utils.primaryWithExpressionSlices namePrim tparamExprs)))

-- | Encode a simple type alias definition
encodeTypeDefSingle :: Helpers.PythonEnvironment -> Core.Name -> Maybe String -> Syntax.Expression -> [Syntax.Statement]
encodeTypeDefSingle env name comment typeExpr =

      let pyName = Names.encodeName False Util.CaseConventionPascal env name
          tparams = environmentTypeParameters env
      in [
        typeAliasStatementFor env pyName tparams comment typeExpr]

-- | Encode a type definition, dispatching based on type structure
encodeTypeAssignment :: Context.Context -> Helpers.PythonEnvironment -> Core.Name -> Core.Type -> Maybe String -> Either (Context.InContext Errors.Error) [[Syntax.Statement]]
encodeTypeAssignment cx env name typ comment =
    Eithers.bind (encodeTypeAssignmentInner cx env name typ comment) (\defStmts -> Right (Lists.map (\s -> [
      s]) defStmts))

-- | Encode the inner type definition, unwrapping forall types
encodeTypeAssignmentInner :: Context.Context -> Helpers.PythonEnvironment -> Core.Name -> Core.Type -> Maybe String -> Either (Context.InContext Errors.Error) [Syntax.Statement]
encodeTypeAssignmentInner cx env name typ comment =

      let stripped = Rewriting.deannotateType typ
          dflt = Eithers.bind (encodeType env typ) (\typeExpr -> Right (encodeTypeDefSingle env name comment typeExpr))
      in case stripped of
        Core.TypeForall v0 ->
          let tvar = Core.forallTypeParameter v0
              body = Core.forallTypeBody v0
              newEnv = extendEnvWithTypeVar env tvar
          in (encodeTypeAssignmentInner cx newEnv name body comment)
        Core.TypeRecord v0 -> Eithers.map (\s -> [
          s]) (encodeRecordType cx env name v0 comment)
        Core.TypeUnion v0 -> encodeUnionType cx env name v0 comment
        Core.TypeWrap v0 -> encodeWrappedType env name v0 comment
        _ -> dflt

-- | Create an expression that calls hydra.dsl.python.unsupported(message) at runtime
unsupportedExpression :: String -> Syntax.Expression
unsupportedExpression msg =
    Utils.functionCall (Utils.pyExpressionToPyPrimary (Utils.projectFromExpression (Utils.projectFromExpression (Utils.projectFromExpression (Syntax.ExpressionSimple (Syntax.Disjunction [
      Syntax.Conjunction [
        Syntax.InversionSimple (Syntax.Comparison {
          Syntax.comparisonLhs = Syntax.BitwiseOr {
            Syntax.bitwiseOrLhs = Nothing,
            Syntax.bitwiseOrRhs = Syntax.BitwiseXor {
              Syntax.bitwiseXorLhs = Nothing,
              Syntax.bitwiseXorRhs = Syntax.BitwiseAnd {
                Syntax.bitwiseAndLhs = Nothing,
                Syntax.bitwiseAndRhs = Syntax.ShiftExpression {
                  Syntax.shiftExpressionLhs = Nothing,
                  Syntax.shiftExpressionRhs = Syntax.Sum {
                    Syntax.sumLhs = Nothing,
                    Syntax.sumRhs = Syntax.Term {
                      Syntax.termLhs = Nothing,
                      Syntax.termRhs = (Syntax.FactorSimple (Syntax.Power {
                        Syntax.powerLhs = Syntax.AwaitPrimary {
                          Syntax.awaitPrimaryAwait = False,
                          Syntax.awaitPrimaryPrimary = (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "hydra")))},
                        Syntax.powerRhs = Nothing}))}}}}}},
          Syntax.comparisonRhs = []})]])) (Syntax.Name "dsl")) (Syntax.Name "python")) (Syntax.Name "unsupported"))) [
      Utils.stringToPyExpression Syntax.QuoteStyleDouble msg]

-- | Create an uncurried lambda with multiple parameters
makeUncurriedLambda :: [Syntax.Name] -> Syntax.Expression -> Syntax.Expression
makeUncurriedLambda params body =
    Syntax.ExpressionLambda (Syntax.Lambda {
      Syntax.lambdaParams = Syntax.LambdaParameters {
        Syntax.lambdaParametersSlashNoDefault = Nothing,
        Syntax.lambdaParametersParamNoDefault = (Lists.map (\p -> Syntax.LambdaParamNoDefault p) params),
        Syntax.lambdaParametersParamWithDefault = [],
        Syntax.lambdaParametersStarEtc = Nothing},
      Syntax.lambdaBody = body})

-- | Encode a field (name-value pair) to a Python (Name, Expression) pair
encodeField :: t0 -> Helpers.PythonEnvironment -> Core.Field -> (Core.Term -> Either t1 t2) -> Either t1 (Syntax.Name, t2)
encodeField cx env field encodeTerm =

      let fname = Core.fieldName field
          fterm = Core.fieldTerm field
      in (Eithers.bind (encodeTerm fterm) (\pterm -> Right (Names.encodeFieldName env fname, pterm)))

-- | Extract CaseStatement from a case elimination term
extractCaseElimination :: Core.Term -> Maybe Core.CaseStatement
extractCaseElimination term =
    case (Rewriting.deannotateAndDetypeTerm term) of
      Core.TermFunction v0 -> case v0 of
        Core.FunctionElimination v1 -> case v1 of
          Core.EliminationUnion v2 -> Just v2
          _ -> Nothing
        _ -> Nothing
      _ -> Nothing

-- | Encode bindings as function definitions
encodeBindingsAsDefs :: t0 -> (t0 -> t1 -> Either t2 t3) -> [t1] -> Either t2 [t3]
encodeBindingsAsDefs env encodeBinding bindings = Eithers.mapList (encodeBinding env) bindings

-- | Encode a binding as a Python statement (function definition or assignment)
encodeBindingAs :: Context.Context -> Helpers.PythonEnvironment -> Core.Binding -> Either (Context.InContext Errors.Error) Syntax.Statement
encodeBindingAs cx env binding =

      let name1 = Core.bindingName binding
          term1 = Core.bindingTerm binding
          mts = Core.bindingType binding
          fname = Names.encodeName True Util.CaseConventionLowerSnake env name1
      in (Maybes.maybe (
        let gathered = gatherLambdas term1
            lambdaParams = Pairs.first gathered
            innerBody = Pairs.second gathered
            mcsa = isCaseStatementApplication innerBody
        in (Maybes.maybe (
          let mcs = extractCaseElimination term1
          in (Maybes.maybe (Eithers.map (\stmts -> Lists.head stmts) (encodeTermMultiline cx env term1)) (\cs ->
            let tname = Core.caseStatementTypeName cs
                dflt = Core.caseStatementDefault cs
                cases_ = Core.caseStatementCases cs
            in (Eithers.bind (Schemas.requireUnionType cx (pythonEnvironmentGetGraph env) tname) (\rt ->
              let isEnum = Schemas.isEnumRowType rt
                  isFull = isCasesFull rt cases_
                  innerParam =
                          Syntax.Param {
                            Syntax.paramName = (Syntax.Name "x"),
                            Syntax.paramAnnotation = Nothing}
                  param =
                          Syntax.ParamNoDefault {
                            Syntax.paramNoDefaultParam = innerParam,
                            Syntax.paramNoDefaultTypeComment = Nothing}
                  params =
                          Syntax.ParametersParamNoDefault (Syntax.ParamNoDefaultParameters {
                            Syntax.paramNoDefaultParametersParamNoDefault = [
                              param],
                            Syntax.paramNoDefaultParametersParamWithDefault = [],
                            Syntax.paramNoDefaultParametersStarEtc = Nothing})
              in (Eithers.bind (Eithers.mapList (encodeCaseBlock cx env tname rt isEnum (\e -> \t -> encodeTermMultiline cx e t)) cases_) (\pyCases -> Eithers.bind (encodeDefaultCaseBlock (\t -> encodeTermInline cx env False t) isFull dflt tname) (\pyDflt ->
                let subj = Syntax.SubjectExpressionSimple (Syntax.NamedExpressionSimple (Utils.pyNameToPyExpression (Syntax.Name "x")))
                    allCases = Lists.concat2 pyCases pyDflt
                    matchStmt =
                            Syntax.StatementCompound (Syntax.CompoundStatementMatch (Syntax.MatchStatement {
                              Syntax.matchStatementSubject = subj,
                              Syntax.matchStatementCases = allCases}))
                    body = Utils.indentedBlock Nothing [
                          [
                            matchStmt]]
                    funcDefRaw =
                            Syntax.FunctionDefRaw {
                              Syntax.functionDefRawAsync = False,
                              Syntax.functionDefRawName = fname,
                              Syntax.functionDefRawTypeParams = [],
                              Syntax.functionDefRawParams = (Just params),
                              Syntax.functionDefRawReturnType = Nothing,
                              Syntax.functionDefRawFuncTypeComment = Nothing,
                              Syntax.functionDefRawBlock = body}
                in (Right (Syntax.StatementCompound (Syntax.CompoundStatementFunction (Syntax.FunctionDefinition {
                  Syntax.functionDefinitionDecorators = Nothing,
                  Syntax.functionDefinitionRaw = funcDefRaw})))))))))) mcs)) (\csa -> Logic.ifElse (Lists.null lambdaParams) (
          let mcs = extractCaseElimination term1
          in (Maybes.maybe (Eithers.map (\stmts -> Lists.head stmts) (encodeTermMultiline cx env term1)) (\cs ->
            let tname = Core.caseStatementTypeName cs
                dflt = Core.caseStatementDefault cs
                cases_ = Core.caseStatementCases cs
            in (Eithers.bind (Schemas.requireUnionType cx (pythonEnvironmentGetGraph env) tname) (\rt ->
              let isEnum = Schemas.isEnumRowType rt
                  isFull = isCasesFull rt cases_
                  innerParam =
                          Syntax.Param {
                            Syntax.paramName = (Syntax.Name "x"),
                            Syntax.paramAnnotation = Nothing}
                  param =
                          Syntax.ParamNoDefault {
                            Syntax.paramNoDefaultParam = innerParam,
                            Syntax.paramNoDefaultTypeComment = Nothing}
                  params =
                          Syntax.ParametersParamNoDefault (Syntax.ParamNoDefaultParameters {
                            Syntax.paramNoDefaultParametersParamNoDefault = [
                              param],
                            Syntax.paramNoDefaultParametersParamWithDefault = [],
                            Syntax.paramNoDefaultParametersStarEtc = Nothing})
              in (Eithers.bind (Eithers.mapList (encodeCaseBlock cx env tname rt isEnum (\e -> \t -> encodeTermMultiline cx e t)) cases_) (\pyCases -> Eithers.bind (encodeDefaultCaseBlock (\t -> encodeTermInline cx env False t) isFull dflt tname) (\pyDflt ->
                let subj = Syntax.SubjectExpressionSimple (Syntax.NamedExpressionSimple (Utils.pyNameToPyExpression (Syntax.Name "x")))
                    allCases = Lists.concat2 pyCases pyDflt
                    matchStmt =
                            Syntax.StatementCompound (Syntax.CompoundStatementMatch (Syntax.MatchStatement {
                              Syntax.matchStatementSubject = subj,
                              Syntax.matchStatementCases = allCases}))
                    body = Utils.indentedBlock Nothing [
                          [
                            matchStmt]]
                    funcDefRaw =
                            Syntax.FunctionDefRaw {
                              Syntax.functionDefRawAsync = False,
                              Syntax.functionDefRawName = fname,
                              Syntax.functionDefRawTypeParams = [],
                              Syntax.functionDefRawParams = (Just params),
                              Syntax.functionDefRawReturnType = Nothing,
                              Syntax.functionDefRawFuncTypeComment = Nothing,
                              Syntax.functionDefRawBlock = body}
                in (Right (Syntax.StatementCompound (Syntax.CompoundStatementFunction (Syntax.FunctionDefinition {
                  Syntax.functionDefinitionDecorators = Nothing,
                  Syntax.functionDefinitionRaw = funcDefRaw})))))))))) mcs)) (
          let tname = Pairs.first csa
              rest1 = Pairs.second csa
              dflt = Pairs.first rest1
              rest2 = Pairs.second rest1
              cases_ = Pairs.first rest2
          in (Eithers.bind (Schemas.requireUnionType cx (pythonEnvironmentGetGraph env) tname) (\rt ->
            let isEnum = Schemas.isEnumRowType rt
                isFull = isCasesFull rt cases_
                capturedVarNames = Lists.init lambdaParams
                matchLambdaParam = Lists.last lambdaParams
                capturedParams =
                        Lists.map (\n -> Syntax.ParamNoDefault {
                          Syntax.paramNoDefaultParam = Syntax.Param {
                            Syntax.paramName = (Names.encodeName False Util.CaseConventionLowerSnake env n),
                            Syntax.paramAnnotation = Nothing},
                          Syntax.paramNoDefaultTypeComment = Nothing}) capturedVarNames
                matchArgName = Names.encodeName False Util.CaseConventionLowerSnake env matchLambdaParam
                matchParam =
                        Syntax.ParamNoDefault {
                          Syntax.paramNoDefaultParam = Syntax.Param {
                            Syntax.paramName = matchArgName,
                            Syntax.paramAnnotation = Nothing},
                          Syntax.paramNoDefaultTypeComment = Nothing}
                allParams = Lists.concat2 capturedParams [
                      matchParam]
                params =
                        Syntax.ParametersParamNoDefault (Syntax.ParamNoDefaultParameters {
                          Syntax.paramNoDefaultParametersParamNoDefault = allParams,
                          Syntax.paramNoDefaultParametersParamWithDefault = [],
                          Syntax.paramNoDefaultParametersStarEtc = Nothing})
                envWithParams = extendEnvWithLambdaParams env term1
            in (Eithers.bind (Eithers.mapList (encodeCaseBlock cx envWithParams tname rt isEnum (\e -> \t -> encodeTermMultiline cx e t)) cases_) (\pyCases -> Eithers.bind (encodeDefaultCaseBlock (\t -> encodeTermInline cx envWithParams False t) isFull dflt tname) (\pyDflt ->
              let subj = Syntax.SubjectExpressionSimple (Syntax.NamedExpressionSimple (Utils.pyNameToPyExpression matchArgName))
                  allCases = Lists.concat2 pyCases pyDflt
                  matchStmt =
                          Syntax.StatementCompound (Syntax.CompoundStatementMatch (Syntax.MatchStatement {
                            Syntax.matchStatementSubject = subj,
                            Syntax.matchStatementCases = allCases}))
                  body = Utils.indentedBlock Nothing [
                        [
                          matchStmt]]
                  funcDefRaw =
                          Syntax.FunctionDefRaw {
                            Syntax.functionDefRawAsync = False,
                            Syntax.functionDefRawName = fname,
                            Syntax.functionDefRawTypeParams = [],
                            Syntax.functionDefRawParams = (Just params),
                            Syntax.functionDefRawReturnType = Nothing,
                            Syntax.functionDefRawFuncTypeComment = Nothing,
                            Syntax.functionDefRawBlock = body}
              in (Right (Syntax.StatementCompound (Syntax.CompoundStatementFunction (Syntax.FunctionDefinition {
                Syntax.functionDefinitionDecorators = Nothing,
                Syntax.functionDefinitionRaw = funcDefRaw}))))))))))) mcsa)) (\ts -> Eithers.bind (Annotations.getTermDescription cx (pythonEnvironmentGetGraph env) term1) (\comment ->
        let normComment = Maybes.map CoderUtils.normalizeComment comment
        in (encodeTermAssignment cx env name1 term1 ts normComment))) mts)

-- | Encode a definition (term or type) to Python statements
encodeDefinition :: Context.Context -> Helpers.PythonEnvironment -> Module.Definition -> Either (Context.InContext Errors.Error) [[Syntax.Statement]]
encodeDefinition cx env def_ =
    case def_ of
      Module.DefinitionTerm v0 ->
        let name = Module.termDefinitionName v0
            term = Module.termDefinitionTerm v0
            typ = Module.termDefinitionType v0
        in (Eithers.bind (Annotations.getTermDescription cx (pythonEnvironmentGetGraph env) term) (\comment ->
          let normComment = Maybes.map CoderUtils.normalizeComment comment
          in (Eithers.bind (encodeTermAssignment cx env name term typ normComment) (\stmt -> Right [
            [
              stmt]]))))
      Module.DefinitionType v0 ->
        let name = Module.typeDefinitionName v0
            typ = Module.typeDefinitionType v0
        in (Eithers.bind (Annotations.getTypeDescription cx (pythonEnvironmentGetGraph env) typ) (\comment ->
          let normComment = Maybes.map CoderUtils.normalizeComment comment
          in (encodeTypeAssignment cx env name typ normComment)))

-- | Calculate term arity with proper primitive handling
termArityWithPrimitives :: Graph.Graph -> Core.Term -> Int
termArityWithPrimitives graph term =
    case (Rewriting.deannotateAndDetypeTerm term) of
      Core.TermApplication v0 -> Math.max 0 (Math.sub (termArityWithPrimitives graph (Core.applicationFunction v0)) 1)
      Core.TermFunction v0 -> functionArityWithPrimitives graph v0
      Core.TermVariable v0 -> Maybes.maybe 0 (\el -> Maybes.maybe (Arity.termArity (Core.bindingTerm el)) (\ts -> Arity.typeSchemeArity ts) (Core.bindingType el)) (Lexical.lookupElement graph v0)
      _ -> 0

-- | Calculate function arity with proper primitive handling
functionArityWithPrimitives :: Graph.Graph -> Core.Function -> Int
functionArityWithPrimitives graph f =
    case f of
      Core.FunctionElimination _ -> 1
      Core.FunctionLambda v0 -> Math.add 1 (termArityWithPrimitives graph (Core.lambdaBody v0))
      Core.FunctionPrimitive v0 -> Maybes.maybe 0 (\prim -> Arity.primitiveArity prim) (Maps.lookup v0 (Graph.graphPrimitives graph))
      _ -> 0

-- | Get the Graph from a PythonEnvironment
pythonEnvironmentGetGraph :: Helpers.PythonEnvironment -> Graph.Graph
pythonEnvironmentGetGraph env = Helpers.pythonEnvironmentGraph env

-- | Set the Graph in a PythonEnvironment
pythonEnvironmentSetGraph :: Graph.Graph -> Helpers.PythonEnvironment -> Helpers.PythonEnvironment
pythonEnvironmentSetGraph tc env =
    Helpers.PythonEnvironment {
      Helpers.pythonEnvironmentNamespaces = (Helpers.pythonEnvironmentNamespaces env),
      Helpers.pythonEnvironmentBoundTypeVariables = (Helpers.pythonEnvironmentBoundTypeVariables env),
      Helpers.pythonEnvironmentGraph = tc,
      Helpers.pythonEnvironmentNullaryBindings = (Helpers.pythonEnvironmentNullaryBindings env),
      Helpers.pythonEnvironmentVersion = (Helpers.pythonEnvironmentVersion env),
      Helpers.pythonEnvironmentSkipCasts = (Helpers.pythonEnvironmentSkipCasts env),
      Helpers.pythonEnvironmentInlineVariables = (Helpers.pythonEnvironmentInlineVariables env)}

-- | Execute a computation with lambda context (adds lambda parameter to Graph)
withLambda :: Helpers.PythonEnvironment -> Core.Lambda -> (Helpers.PythonEnvironment -> t0) -> t0
withLambda = Schemas.withLambdaContext pythonEnvironmentGetGraph pythonEnvironmentSetGraph

-- | Execute a computation with type lambda context
withTypeLambda :: Helpers.PythonEnvironment -> Core.TypeLambda -> (Helpers.PythonEnvironment -> t0) -> t0
withTypeLambda = Schemas.withTypeLambdaContext pythonEnvironmentGetGraph pythonEnvironmentSetGraph

-- | Execute a computation with let context (adds let bindings to Graph)
withLet :: Helpers.PythonEnvironment -> Core.Let -> (Helpers.PythonEnvironment -> t0) -> t0
withLet = Schemas.withLetContext pythonEnvironmentGetGraph pythonEnvironmentSetGraph pythonBindingMetadata

-- | Execute a computation with inline let context (for walrus operators)
withLetInline :: Helpers.PythonEnvironment -> Core.Let -> (Helpers.PythonEnvironment -> t0) -> t0
withLetInline env lt body =

      let bindingNames = Lists.map (\b -> Core.bindingName b) (Core.letBindings lt)
          inlineVars = Sets.fromList bindingNames
          noMetadata = \tc -> \b -> Nothing
      in (Schemas.withLetContext pythonEnvironmentGetGraph pythonEnvironmentSetGraph noMetadata env lt (\innerEnv ->
        let updatedEnv =
                Helpers.PythonEnvironment {
                  Helpers.pythonEnvironmentNamespaces = (Helpers.pythonEnvironmentNamespaces innerEnv),
                  Helpers.pythonEnvironmentBoundTypeVariables = (Helpers.pythonEnvironmentBoundTypeVariables innerEnv),
                  Helpers.pythonEnvironmentGraph = (Helpers.pythonEnvironmentGraph innerEnv),
                  Helpers.pythonEnvironmentNullaryBindings = (Helpers.pythonEnvironmentNullaryBindings innerEnv),
                  Helpers.pythonEnvironmentVersion = (Helpers.pythonEnvironmentVersion innerEnv),
                  Helpers.pythonEnvironmentSkipCasts = (Helpers.pythonEnvironmentSkipCasts innerEnv),
                  Helpers.pythonEnvironmentInlineVariables = (Sets.union inlineVars (Helpers.pythonEnvironmentInlineVariables innerEnv))}
        in (body updatedEnv)))

-- | Create initial empty metadata for a Python module
initialMetadata :: Module.Namespace -> Helpers.PythonModuleMetadata
initialMetadata ns =

      let dottedNs = Names.encodeNamespace ns
          emptyNs =
                  Module.Namespaces {
                    Module.namespacesFocus = (ns, dottedNs),
                    Module.namespacesMapping = Maps.empty}
      in Helpers.PythonModuleMetadata {
        Helpers.pythonModuleMetadataNamespaces = emptyNs,
        Helpers.pythonModuleMetadataTypeVariables = Sets.empty,
        Helpers.pythonModuleMetadataUsesAnnotated = False,
        Helpers.pythonModuleMetadataUsesCallable = False,
        Helpers.pythonModuleMetadataUsesCast = False,
        Helpers.pythonModuleMetadataUsesLruCache = False,
        Helpers.pythonModuleMetadataUsesTypeAlias = False,
        Helpers.pythonModuleMetadataUsesDataclass = False,
        Helpers.pythonModuleMetadataUsesDecimal = False,
        Helpers.pythonModuleMetadataUsesEither = False,
        Helpers.pythonModuleMetadataUsesEnum = False,
        Helpers.pythonModuleMetadataUsesFrozenDict = False,
        Helpers.pythonModuleMetadataUsesFrozenList = False,
        Helpers.pythonModuleMetadataUsesGeneric = False,
        Helpers.pythonModuleMetadataUsesJust = False,
        Helpers.pythonModuleMetadataUsesLeft = False,
        Helpers.pythonModuleMetadataUsesMaybe = False,
        Helpers.pythonModuleMetadataUsesName = False,
        Helpers.pythonModuleMetadataUsesNode = False,
        Helpers.pythonModuleMetadataUsesNothing = False,
        Helpers.pythonModuleMetadataUsesRight = False,
        Helpers.pythonModuleMetadataUsesTypeVar = False}

-- | Create an initial Python environment for code generation
initialEnvironment :: Module.Namespaces Syntax.DottedName -> Graph.Graph -> Helpers.PythonEnvironment
initialEnvironment namespaces tcontext =
    Helpers.PythonEnvironment {
      Helpers.pythonEnvironmentNamespaces = namespaces,
      Helpers.pythonEnvironmentBoundTypeVariables = ([], Maps.empty),
      Helpers.pythonEnvironmentGraph = tcontext,
      Helpers.pythonEnvironmentNullaryBindings = Sets.empty,
      Helpers.pythonEnvironmentVersion = targetPythonVersion,
      Helpers.pythonEnvironmentSkipCasts = True,
      Helpers.pythonEnvironmentInlineVariables = Sets.empty}

-- | The target Python version for code generation
targetPythonVersion :: Helpers.PythonVersion
targetPythonVersion = Utils.targetPythonVersion

-- | Like bindingMetadata, but only for bindings that will actually be thunked
pythonBindingMetadata :: Graph.Graph -> Core.Binding -> Maybe Core.Term
pythonBindingMetadata g b = Logic.ifElse (shouldThunkBinding g b) (CoderUtils.bindingMetadata g b) Nothing

-- | Determine if a binding should be thunked based on its complexity and triviality
shouldThunkBinding :: Graph.Graph -> Core.Binding -> Bool
shouldThunkBinding g b = Logic.and (CoderUtils.isComplexBinding g b) (Logic.not (CoderUtils.isTrivialTerm (Core.bindingTerm b)))

-- | Analyze a function term with Python-specific Graph management
analyzePythonFunction :: Context.Context -> Helpers.PythonEnvironment -> Core.Term -> Either t0 (Typing.FunctionStructure Helpers.PythonEnvironment)
analyzePythonFunction cx env term =
    CoderUtils.analyzeFunctionTermWith cx pythonBindingMetadata pythonEnvironmentGetGraph pythonEnvironmentSetGraph env term

-- | Execute a computation with definitions in scope
withDefinitions :: Helpers.PythonEnvironment -> [Module.Definition] -> (Helpers.PythonEnvironment -> t0) -> t0
withDefinitions env defs body =

      let bindings =
              Maybes.cat (Lists.map (\def_ -> case def_ of
                Module.DefinitionTerm v0 -> Just (Core.Binding {
                  Core.bindingName = (Module.termDefinitionName v0),
                  Core.bindingTerm = (Module.termDefinitionTerm v0),
                  Core.bindingType = (Just (Module.termDefinitionType v0))})
                Module.DefinitionType _ -> Nothing
                _ -> Nothing) defs)
          dummyLet =
                  Core.Let {
                    Core.letBindings = bindings,
                    Core.letBody = (Core.TermLiteral (Core.LiteralString "dummy"))}
      in (withLet env dummyLet body)

-- | Encode a binding as a walrus operator assignment
encodeBindingAsAssignment :: Context.Context -> Bool -> Helpers.PythonEnvironment -> Core.Binding -> Either (Context.InContext Errors.Error) Syntax.NamedExpression
encodeBindingAsAssignment cx allowThunking env binding =

      let name = Core.bindingName binding
          term = Core.bindingTerm binding
          mts = Core.bindingType binding
          pyName = Names.encodeName False Util.CaseConventionLowerSnake env name
      in (Eithers.bind (encodeTermInline cx env False term) (\pbody ->
        let tc = Helpers.pythonEnvironmentGraph env
            isComplexVar = CoderUtils.isComplexVariable tc name
            termIsComplex = CoderUtils.isComplexTerm tc term
            isTrivial = CoderUtils.isTrivialTerm term
            needsThunk =
                    Logic.ifElse isTrivial False (Maybes.maybe (Logic.and allowThunking (Logic.or isComplexVar termIsComplex)) (\ts -> Logic.and allowThunking (Logic.and (Equality.equal (Arity.typeSchemeArity ts) 0) (Logic.or isComplexVar termIsComplex))) mts)
            pterm = Logic.ifElse needsThunk (makeThunk pbody) pbody
        in (Right (Syntax.NamedExpressionAssignment (Syntax.AssignmentExpression {
          Syntax.assignmentExpressionName = pyName,
          Syntax.assignmentExpressionExpression = pterm})))))

-- | Encode a term body for TCO: tail self-calls become param reassignment + continue
encodeTermMultilineTCO :: Context.Context -> Helpers.PythonEnvironment -> Core.Name -> [Core.Name] -> Core.Term -> Either (Context.InContext Errors.Error) [Syntax.Statement]
encodeTermMultilineTCO cx env funcName paramNames term =

      let stripped = Rewriting.deannotateAndDetypeTerm term
          gathered = CoderUtils.gatherApplications stripped
          gatherArgs = Pairs.first gathered
          gatherFun = Pairs.second gathered
          strippedFun = Rewriting.deannotateAndDetypeTerm gatherFun
          isSelfCall =
                  case strippedFun of
                    Core.TermVariable v0 -> Equality.equal v0 funcName
                    _ -> False
      in (Logic.ifElse (Logic.and isSelfCall (Equality.equal (Lists.length gatherArgs) (Lists.length paramNames))) (Eithers.bind (Eithers.mapList (\a -> encodeTermInline cx env False a) gatherArgs) (\pyArgs ->
        let assignments =
                Lists.map (\pair ->
                  let paramName = Pairs.first pair
                      pyArg = Pairs.second pair
                  in (Utils.assignmentStatement (Names.encodeName False Util.CaseConventionLowerSnake env paramName) pyArg)) (Lists.zip paramNames pyArgs)
            continueStmt = Syntax.StatementSimple [
                  Syntax.SimpleStatementContinue]
        in (Right (Lists.concat2 assignments [
          continueStmt])))) (
        let gathered2 = CoderUtils.gatherApplications term
            args2 = Pairs.first gathered2
            body2 = Pairs.second gathered2
        in (Logic.ifElse (Equality.equal (Lists.length args2) 1) (
          let arg = Lists.head args2
          in case (Rewriting.deannotateAndDetypeTerm body2) of
            Core.TermFunction v0 -> case v0 of
              Core.FunctionElimination v1 -> case v1 of
                Core.EliminationUnion v2 ->
                  let tname = Core.caseStatementTypeName v2
                      dflt = Core.caseStatementDefault v2
                      cases_ = Core.caseStatementCases v2
                  in (Eithers.bind (Schemas.requireUnionType cx (pythonEnvironmentGetGraph env) tname) (\rt ->
                    let isEnum = Schemas.isEnumRowType rt
                        isFull = isCasesFull rt cases_
                    in (Eithers.bind (encodeTermInline cx env False arg) (\pyArg -> Eithers.bind (Eithers.mapList (encodeCaseBlock cx env tname rt isEnum (\e2 -> \t2 -> encodeTermMultilineTCO cx e2 funcName paramNames t2)) (deduplicateCaseVariables cases_)) (\pyCases -> Eithers.bind (encodeDefaultCaseBlock (\t2 -> encodeTermInline cx env False t2) isFull dflt tname) (\pyDflt ->
                      let subj = Syntax.SubjectExpressionSimple (Syntax.NamedExpressionSimple pyArg)
                          matchStmt =
                                  Syntax.StatementCompound (Syntax.CompoundStatementMatch (Syntax.MatchStatement {
                                    Syntax.matchStatementSubject = subj,
                                    Syntax.matchStatementCases = (Lists.concat2 pyCases pyDflt)}))
                      in (Right [
                        matchStmt])))))))
                _ -> Eithers.bind (encodeTermInline cx env False term) (\expr -> Right [
                  Utils.returnSingle expr])
              _ -> Eithers.bind (encodeTermInline cx env False term) (\expr -> Right [
                Utils.returnSingle expr])
            _ -> Eithers.bind (encodeTermInline cx env False term) (\expr -> Right [
              Utils.returnSingle expr])) (Eithers.bind (encodeTermInline cx env False term) (\expr -> Right [
          Utils.returnSingle expr])))))

-- | Encode a function definition with parameters and body
encodeFunctionDefinition :: Context.Context -> Helpers.PythonEnvironment -> Core.Name -> [Core.Name] -> [Core.Name] -> Core.Term -> [Core.Type] -> Maybe Core.Type -> Maybe String -> [Syntax.Statement] -> Either (Context.InContext Errors.Error) Syntax.Statement
encodeFunctionDefinition cx env name tparams args body doms mcod comment prefixes =
    Eithers.bind (Eithers.mapList (\pair ->
      let argName = Pairs.first pair
          typ = Pairs.second pair
      in (Eithers.bind (encodeType env typ) (\pyTyp -> Right (Syntax.ParamNoDefault {
        Syntax.paramNoDefaultParam = Syntax.Param {
          Syntax.paramName = (Names.encodeName False Util.CaseConventionLowerSnake env argName),
          Syntax.paramAnnotation = (Just (Syntax.Annotation pyTyp))},
        Syntax.paramNoDefaultTypeComment = Nothing})))) (Lists.zip args doms)) (\pyArgs ->
      let pyParams =
              Syntax.ParametersParamNoDefault (Syntax.ParamNoDefaultParameters {
                Syntax.paramNoDefaultParametersParamNoDefault = pyArgs,
                Syntax.paramNoDefaultParametersParamWithDefault = [],
                Syntax.paramNoDefaultParametersStarEtc = Nothing})
          isTCO = Logic.and (Logic.not (Lists.null args)) (CoderUtils.isSelfTailRecursive name body)
      in (Eithers.bind (Logic.ifElse isTCO (Eithers.bind (encodeTermMultilineTCO cx env name args body) (\tcoStmts ->
        let trueExpr = Syntax.NamedExpressionSimple (Utils.pyAtomToPyExpression Syntax.AtomTrue)
            whileBody = Utils.indentedBlock Nothing [
                  Lists.concat2 prefixes tcoStmts]
            whileStmt =
                    Syntax.StatementCompound (Syntax.CompoundStatementWhile (Syntax.WhileStatement {
                      Syntax.whileStatementCondition = trueExpr,
                      Syntax.whileStatementBody = whileBody,
                      Syntax.whileStatementElse = Nothing}))
        in (Right (Utils.indentedBlock comment [
          [
            whileStmt]])))) (Eithers.bind (encodeTermMultiline cx env body) (\stmts -> Right (Utils.indentedBlock comment [
        Lists.concat2 prefixes stmts])))) (\block -> Eithers.bind (Maybes.maybe (Right Nothing) (\cod -> Eithers.bind (encodeType env cod) (\pytyp -> Right (Just pytyp))) mcod) (\mreturnType ->
        let pyTparams =
                Logic.ifElse useInlineTypeParams (Lists.map (\arg_ -> Utils.pyNameToPyTypeParameter (Names.encodeTypeVariable arg_)) tparams) []
            isThunk = Lists.null args
            mDecorators = Logic.ifElse isThunk (Just (Syntax.Decorators [
                  lruCacheDecorator])) Nothing
            pyName = Names.encodeName False Util.CaseConventionLowerSnake env name
        in (Right (Syntax.StatementCompound (Syntax.CompoundStatementFunction (Syntax.FunctionDefinition {
          Syntax.functionDefinitionDecorators = mDecorators,
          Syntax.functionDefinitionRaw = Syntax.FunctionDefRaw {
            Syntax.functionDefRawAsync = False,
            Syntax.functionDefRawName = pyName,
            Syntax.functionDefRawTypeParams = pyTparams,
            Syntax.functionDefRawParams = (Just pyParams),
            Syntax.functionDefRawReturnType = mreturnType,
            Syntax.functionDefRawFuncTypeComment = Nothing,
            Syntax.functionDefRawBlock = block}}))))))))

-- | Encode a term to a list of statements with return as final statement
encodeTermMultiline :: Context.Context -> Helpers.PythonEnvironment -> Core.Term -> Either (Context.InContext Errors.Error) [Syntax.Statement]
encodeTermMultiline cx env term =

      let dfltLogic =
              Eithers.bind (analyzePythonFunction cx env term) (\fs ->
                let params = Typing.functionStructureParams fs
                    bindings = Typing.functionStructureBindings fs
                    innerBody = Typing.functionStructureBody fs
                    env2 = Typing.functionStructureEnvironment fs
                in (Logic.ifElse (Lists.null bindings) (Eithers.bind (encodeTermInline cx env False term) (\expr -> Right [
                  Utils.returnSingle expr])) (Eithers.bind (Eithers.mapList (encodeBindingAs cx env2) bindings) (\bindingStmts -> Eithers.bind (encodeTermMultiline cx env2 innerBody) (\bodyStmts -> Right (Lists.concat2 bindingStmts bodyStmts))))))
          gathered = CoderUtils.gatherApplications term
          args = Pairs.first gathered
          body = Pairs.second gathered
      in (Logic.ifElse (Equality.equal (Lists.length args) 1) (
        let arg = Lists.head args
        in case (Rewriting.deannotateAndDetypeTerm body) of
          Core.TermFunction v0 -> case v0 of
            Core.FunctionElimination v1 -> case v1 of
              Core.EliminationUnion v2 ->
                let tname = Core.caseStatementTypeName v2
                    dflt = Core.caseStatementDefault v2
                    cases_ = Core.caseStatementCases v2
                in (Eithers.bind (Schemas.requireUnionType cx (pythonEnvironmentGetGraph env) tname) (\rt ->
                  let isEnum = Schemas.isEnumRowType rt
                      isFull = isCasesFull rt cases_
                  in (Eithers.bind (encodeTermInline cx env False arg) (\pyArg -> Eithers.bind (Eithers.mapList (encodeCaseBlock cx env tname rt isEnum (\e -> \t -> encodeTermMultiline cx e t)) (deduplicateCaseVariables cases_)) (\pyCases -> Eithers.bind (encodeDefaultCaseBlock (\t -> encodeTermInline cx env False t) isFull dflt tname) (\pyDflt ->
                    let subj = Syntax.SubjectExpressionSimple (Syntax.NamedExpressionSimple pyArg)
                        matchStmt =
                                Syntax.StatementCompound (Syntax.CompoundStatementMatch (Syntax.MatchStatement {
                                  Syntax.matchStatementSubject = subj,
                                  Syntax.matchStatementCases = (Lists.concat2 pyCases pyDflt)}))
                    in (Right [
                      matchStmt])))))))
              _ -> dfltLogic
            _ -> dfltLogic
          _ -> dfltLogic) dfltLogic)

-- | Encode a function term to a Python expression
encodeFunction :: Context.Context -> Helpers.PythonEnvironment -> Core.Function -> Either (Context.InContext Errors.Error) Syntax.Expression
encodeFunction cx env f =
    case f of
      Core.FunctionLambda v0 -> Eithers.bind (analyzePythonFunction cx env (Core.TermFunction (Core.FunctionLambda v0))) (\fs ->
        let params = Typing.functionStructureParams fs
            bindings = Typing.functionStructureBindings fs
            innerBody = Typing.functionStructureBody fs
            innerEnv0 = Typing.functionStructureEnvironment fs
            bindingNames = Lists.map (\b -> Core.bindingName b) bindings
            innerEnv =
                    Helpers.PythonEnvironment {
                      Helpers.pythonEnvironmentNamespaces = (Helpers.pythonEnvironmentNamespaces innerEnv0),
                      Helpers.pythonEnvironmentBoundTypeVariables = (Helpers.pythonEnvironmentBoundTypeVariables innerEnv0),
                      Helpers.pythonEnvironmentGraph = (Helpers.pythonEnvironmentGraph innerEnv0),
                      Helpers.pythonEnvironmentNullaryBindings = (Helpers.pythonEnvironmentNullaryBindings innerEnv0),
                      Helpers.pythonEnvironmentVersion = (Helpers.pythonEnvironmentVersion innerEnv0),
                      Helpers.pythonEnvironmentSkipCasts = (Helpers.pythonEnvironmentSkipCasts innerEnv0),
                      Helpers.pythonEnvironmentInlineVariables = (Sets.union (Sets.fromList bindingNames) (Helpers.pythonEnvironmentInlineVariables innerEnv0))}
        in (Eithers.bind (encodeTermInline cx innerEnv False innerBody) (\pbody ->
          let pparams = Lists.map (Names.encodeName False Util.CaseConventionLowerSnake innerEnv) params
          in (Logic.ifElse (Lists.null bindings) (Right (makeUncurriedLambda pparams pbody)) (Eithers.bind (Eithers.mapList (encodeBindingAsAssignment cx False innerEnv) bindings) (\pbindingExprs ->
            let pbindingStarExprs = Lists.map (\ne -> Syntax.StarNamedExpressionSimple ne) pbindingExprs
                pbodyStarExpr = Utils.pyExpressionToPyStarNamedExpression pbody
                tupleElements = Lists.concat2 pbindingStarExprs [
                      pbodyStarExpr]
                tupleExpr = Utils.pyAtomToPyExpression (Syntax.AtomTuple (Syntax.Tuple tupleElements))
                indexValue =
                        Utils.pyAtomToPyExpression (Syntax.AtomNumber (Syntax.NumberInteger (Literals.int32ToBigint (Lists.length bindings))))
                indexedExpr = Utils.primaryWithExpressionSlices (Utils.pyExpressionToPyPrimary tupleExpr) [
                      indexValue]
            in (Right (makeUncurriedLambda pparams (Utils.pyPrimaryToPyExpression indexedExpr)))))))))
      Core.FunctionPrimitive v0 -> encodeVariable cx env v0 []
      Core.FunctionElimination v0 -> case v0 of
        Core.EliminationRecord v1 ->
          let fname = Core.projectionField v1
          in (Right (makeCurriedLambda [
            Syntax.Name "v1"] (Utils.projectFromExpression (Syntax.ExpressionSimple (Syntax.Disjunction [
            Syntax.Conjunction [
              Syntax.InversionSimple (Syntax.Comparison {
                Syntax.comparisonLhs = Syntax.BitwiseOr {
                  Syntax.bitwiseOrLhs = Nothing,
                  Syntax.bitwiseOrRhs = Syntax.BitwiseXor {
                    Syntax.bitwiseXorLhs = Nothing,
                    Syntax.bitwiseXorRhs = Syntax.BitwiseAnd {
                      Syntax.bitwiseAndLhs = Nothing,
                      Syntax.bitwiseAndRhs = Syntax.ShiftExpression {
                        Syntax.shiftExpressionLhs = Nothing,
                        Syntax.shiftExpressionRhs = Syntax.Sum {
                          Syntax.sumLhs = Nothing,
                          Syntax.sumRhs = Syntax.Term {
                            Syntax.termLhs = Nothing,
                            Syntax.termRhs = (Syntax.FactorSimple (Syntax.Power {
                              Syntax.powerLhs = Syntax.AwaitPrimary {
                                Syntax.awaitPrimaryAwait = False,
                                Syntax.awaitPrimaryPrimary = (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "v1")))},
                              Syntax.powerRhs = Nothing}))}}}}}},
                Syntax.comparisonRhs = []})]])) (Names.encodeFieldName env fname))))
        Core.EliminationWrap _ -> Right (makeCurriedLambda [
          Syntax.Name "v1"] (Utils.projectFromExpression (Syntax.ExpressionSimple (Syntax.Disjunction [
          Syntax.Conjunction [
            Syntax.InversionSimple (Syntax.Comparison {
              Syntax.comparisonLhs = Syntax.BitwiseOr {
                Syntax.bitwiseOrLhs = Nothing,
                Syntax.bitwiseOrRhs = Syntax.BitwiseXor {
                  Syntax.bitwiseXorLhs = Nothing,
                  Syntax.bitwiseXorRhs = Syntax.BitwiseAnd {
                    Syntax.bitwiseAndLhs = Nothing,
                    Syntax.bitwiseAndRhs = Syntax.ShiftExpression {
                      Syntax.shiftExpressionLhs = Nothing,
                      Syntax.shiftExpressionRhs = Syntax.Sum {
                        Syntax.sumLhs = Nothing,
                        Syntax.sumRhs = Syntax.Term {
                          Syntax.termLhs = Nothing,
                          Syntax.termRhs = (Syntax.FactorSimple (Syntax.Power {
                            Syntax.powerLhs = Syntax.AwaitPrimary {
                              Syntax.awaitPrimaryAwait = False,
                              Syntax.awaitPrimaryPrimary = (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "v1")))},
                            Syntax.powerRhs = Nothing}))}}}}}},
              Syntax.comparisonRhs = []})]])) (Syntax.Name "value")))
        Core.EliminationUnion _ -> Right (unsupportedExpression "case expressions as values are not yet supported")

-- | Encode a term assignment to a Python statement
encodeTermAssignment :: Context.Context -> Helpers.PythonEnvironment -> Core.Name -> Core.Term -> Core.TypeScheme -> Maybe String -> Either (Context.InContext Errors.Error) Syntax.Statement
encodeTermAssignment cx env name term ts comment =
    Eithers.bind (analyzePythonFunction cx env term) (\fs ->
      let tparams = Typing.functionStructureTypeParams fs
          params = Typing.functionStructureParams fs
          bindings = Typing.functionStructureBindings fs
          body = Typing.functionStructureBody fs
          doms = Typing.functionStructureDomains fs
          mcod = Typing.functionStructureCodomain fs
          env2 = Typing.functionStructureEnvironment fs
          tc = Helpers.pythonEnvironmentGraph env2
          binding =
                  Core.Binding {
                    Core.bindingName = name,
                    Core.bindingTerm = term,
                    Core.bindingType = (Just ts)}
          isComplex = CoderUtils.isComplexBinding tc binding
          isTrivial = CoderUtils.isTrivialTerm term
      in (Logic.ifElse (Logic.and isComplex (Logic.not isTrivial)) (Eithers.bind (Eithers.mapList (encodeBindingAs cx env2) bindings) (\bindingStmts -> encodeFunctionDefinition cx env2 name tparams params body doms mcod comment bindingStmts)) (Eithers.bind (encodeTermInline cx env2 False body) (\bodyExpr ->
        let pyName = Names.encodeName False Util.CaseConventionLowerSnake env2 name
        in (Right (Utils.annotatedStatement comment (Utils.assignmentStatement pyName bodyExpr)))))))

-- | Encode a variable reference to a Python expression
encodeVariable :: Context.Context -> Helpers.PythonEnvironment -> Core.Name -> [Syntax.Expression] -> Either (Context.InContext Errors.Error) Syntax.Expression
encodeVariable cx env name args =

      let g = pythonEnvironmentGetGraph env
          tc = Helpers.pythonEnvironmentGraph env
          tcTypes = Graph.graphBoundTypes tc
          tcLambdaVars = Graph.graphLambdaVariables tc
          tcMetadata = Graph.graphMetadata tc
          inlineVars = Helpers.pythonEnvironmentInlineVariables env
          mTypScheme = Maps.lookup name tcTypes
          mTyp = Maybes.map (\ts_ -> Core.typeSchemeType ts_) mTypScheme
          asVariable = Names.termVariableReference env name
          asFunctionCall =
                  Utils.functionCall (Utils.pyNameToPyPrimary (Names.encodeName True Util.CaseConventionLowerSnake env name)) args
      in (Logic.ifElse (Logic.not (Lists.null args)) (Maybes.maybe (Right asFunctionCall) (\prim ->
        let primArity = Arity.primitiveArity prim
        in (Logic.ifElse (Equality.equal primArity (Lists.length args)) (Right asFunctionCall) (
          let numRemaining = Math.sub primArity (Lists.length args)
              remainingParams = Lists.map (\i -> Syntax.Name (Strings.cat2 "x" (Literals.showInt32 i))) (Math.range 1 numRemaining)
              remainingExprs =
                      Lists.map (\n -> Syntax.ExpressionSimple (Syntax.Disjunction [
                        Syntax.Conjunction [
                          Syntax.InversionSimple (Syntax.Comparison {
                            Syntax.comparisonLhs = Syntax.BitwiseOr {
                              Syntax.bitwiseOrLhs = Nothing,
                              Syntax.bitwiseOrRhs = Syntax.BitwiseXor {
                                Syntax.bitwiseXorLhs = Nothing,
                                Syntax.bitwiseXorRhs = Syntax.BitwiseAnd {
                                  Syntax.bitwiseAndLhs = Nothing,
                                  Syntax.bitwiseAndRhs = Syntax.ShiftExpression {
                                    Syntax.shiftExpressionLhs = Nothing,
                                    Syntax.shiftExpressionRhs = Syntax.Sum {
                                      Syntax.sumLhs = Nothing,
                                      Syntax.sumRhs = Syntax.Term {
                                        Syntax.termLhs = Nothing,
                                        Syntax.termRhs = (Syntax.FactorSimple (Syntax.Power {
                                          Syntax.powerLhs = Syntax.AwaitPrimary {
                                            Syntax.awaitPrimaryAwait = False,
                                            Syntax.awaitPrimaryPrimary = (Syntax.PrimarySimple (Syntax.AtomName n))},
                                          Syntax.powerRhs = Nothing}))}}}}}},
                            Syntax.comparisonRhs = []})]])) remainingParams
              allArgs = Lists.concat2 args remainingExprs
              fullCall =
                      Utils.functionCall (Utils.pyNameToPyPrimary (Names.encodeName True Util.CaseConventionLowerSnake env name)) allArgs
          in (Right (makeUncurriedLambda remainingParams fullCall))))) (Lexical.lookupPrimitive g name)) (Maybes.maybe (Logic.ifElse (Sets.member name tcLambdaVars) (Right asVariable) (Logic.ifElse (Sets.member name inlineVars) (Right asVariable) (Maybes.maybe (Maybes.maybe (Maybes.maybe (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat2 "Unknown variable: " (Core.unName name)))),
        Context.inContextContext = cx})) (\_ -> Right asFunctionCall) (Maps.lookup name tcMetadata)) (\el ->
        let elTrivial1 = CoderUtils.isTrivialTerm (Core.bindingTerm el)
        in (Maybes.maybe (Right asVariable) (\ts -> Logic.ifElse (Logic.and (Logic.and (Equality.equal (Arity.typeSchemeArity ts) 0) (CoderUtils.isComplexBinding tc el)) (Logic.not elTrivial1)) (Right asFunctionCall) (
          let asFunctionRef =
                  Logic.ifElse (Logic.not (Lists.null (Core.typeSchemeVariables ts))) (makeSimpleLambda (Arity.typeArity (Core.typeSchemeType ts)) asVariable) asVariable
          in (Right asFunctionRef))) (Core.bindingType el))) (Lexical.lookupElement g name)) (\prim ->
        let primArity = Arity.primitiveArity prim
        in (Logic.ifElse (Equality.equal primArity 0) (Right asFunctionCall) (
          let ts = Graph.primitiveType prim
              asFunctionRef =
                      Logic.ifElse (Logic.not (Lists.null (Core.typeSchemeVariables ts))) (makeSimpleLambda (Arity.typeArity (Core.typeSchemeType ts)) asVariable) asVariable
          in (Right asFunctionRef)))) (Lexical.lookupPrimitive g name)))) (\typ -> Logic.ifElse (Sets.member name tcLambdaVars) (Right asVariable) (Logic.ifElse (Sets.member name inlineVars) (
        let asFunctionRef =
                Logic.ifElse (Logic.not (Sets.null (Rewriting.freeVariablesInType typ))) (makeSimpleLambda (Arity.typeArity typ) asVariable) asVariable
        in (Right asFunctionRef)) (Logic.ifElse (Logic.not (Maps.member name tcMetadata)) (Maybes.maybe (
        let asFunctionRef =
                Logic.ifElse (Logic.not (Sets.null (Rewriting.freeVariablesInType typ))) (makeSimpleLambda (Arity.typeArity typ) asVariable) asVariable
        in (Right asFunctionRef)) (\el ->
        let elTrivial = CoderUtils.isTrivialTerm (Core.bindingTerm el)
        in (Maybes.maybe (Logic.ifElse (Logic.and (Equality.equal (Arity.typeArity typ) 0) (Logic.not elTrivial)) (Right asFunctionCall) (
          let asFunctionRef =
                  Logic.ifElse (Logic.not (Sets.null (Rewriting.freeVariablesInType typ))) (makeSimpleLambda (Arity.typeArity typ) asVariable) asVariable
          in (Right asFunctionRef))) (\ts -> Logic.ifElse (Logic.and (Logic.and (Equality.equal (Arity.typeArity typ) 0) (CoderUtils.isComplexBinding tc el)) (Logic.not elTrivial)) (Right asFunctionCall) (
          let asFunctionRef =
                  Logic.ifElse (Logic.not (Sets.null (Rewriting.freeVariablesInType typ))) (makeSimpleLambda (Arity.typeArity typ) asVariable) asVariable
          in (Right asFunctionRef))) (Core.bindingType el))) (Lexical.lookupElement g name)) (Logic.ifElse (Logic.and (Equality.equal (Arity.typeArity typ) 0) (CoderUtils.isComplexVariable tc name)) (Right asFunctionCall) (
        let asFunctionRef =
                Logic.ifElse (Logic.not (Sets.null (Rewriting.freeVariablesInType typ))) (makeSimpleLambda (Arity.typeArity typ) asVariable) asVariable
        in (Right asFunctionRef)))))) mTyp))

-- | Encode a function application to a Python expression
encodeApplication :: Context.Context -> Helpers.PythonEnvironment -> Core.Application -> Either (Context.InContext Errors.Error) Syntax.Expression
encodeApplication cx env app =

      let g = pythonEnvironmentGetGraph env
          term = Core.TermApplication app
          gathered = CoderUtils.gatherArgs term []
          fun = Pairs.first gathered
          args = Pairs.second gathered
          knownArity = termArityWithPrimitives g fun
          arity = Math.max knownArity (Lists.length args)
      in (Eithers.bind (Eithers.mapList (\t -> encodeTermInline cx env False t) args) (\pargs ->
        let hargs = Lists.take arity pargs
            rargs = Lists.drop arity pargs
        in (Eithers.bind (encodeApplicationInner cx env fun hargs rargs) (\result ->
          let lhs = Pairs.first result
              remainingRargs = Pairs.second result
              pyapp = Lists.foldl (\t -> \a -> Utils.functionCall (Utils.pyExpressionToPyPrimary t) [
                    a]) lhs remainingRargs
          in (Right pyapp)))))

-- | Inner helper for encodeApplication
encodeApplicationInner :: Context.Context -> Helpers.PythonEnvironment -> Core.Term -> [Syntax.Expression] -> [Syntax.Expression] -> Either (Context.InContext Errors.Error) (Syntax.Expression, [Syntax.Expression])
encodeApplicationInner cx env fun hargs rargs =

      let firstArg = Lists.head hargs
          restArgs = Lists.tail hargs
          withRest = \e -> Logic.ifElse (Lists.null restArgs) e (Utils.functionCall (Utils.pyExpressionToPyPrimary e) restArgs)
          defaultCase =
                  Eithers.bind (encodeTermInline cx env False fun) (\pfun -> Right (Utils.functionCall (Utils.pyExpressionToPyPrimary pfun) hargs, rargs))
      in case (Rewriting.deannotateAndDetypeTerm fun) of
        Core.TermFunction v0 -> case v0 of
          Core.FunctionElimination v1 -> case v1 of
            Core.EliminationRecord v2 ->
              let fname = Core.projectionField v2
                  fieldExpr = Utils.projectFromExpression firstArg (Names.encodeFieldName env fname)
              in (Right (withRest fieldExpr, rargs))
            Core.EliminationUnion v2 -> Eithers.bind (encodeUnionEliminationInline cx env v2 firstArg) (\inlineExpr -> Right (withRest inlineExpr, rargs))
            Core.EliminationWrap _ ->
              let valueExpr = Utils.projectFromExpression firstArg (Syntax.Name "value")
                  allArgs = Lists.concat2 restArgs rargs
              in (Logic.ifElse (Lists.null allArgs) (Right (valueExpr, [])) (Right (Utils.functionCall (Utils.pyExpressionToPyPrimary valueExpr) allArgs, [])))
            _ -> defaultCase
          Core.FunctionPrimitive v1 ->
            let wrappedArgs = wrapLazyArguments v1 hargs
            in (Eithers.bind (encodeVariable cx env v1 wrappedArgs) (\expr -> Right (expr, rargs)))
          Core.FunctionLambda _ -> Eithers.bind (encodeTermInline cx env False fun) (\pfun -> Right (Utils.functionCall (Utils.pyExpressionToPyPrimary pfun) hargs, rargs))
          _ -> defaultCase
        Core.TermVariable v0 ->
          let g = pythonEnvironmentGetGraph env
              allArgs = Lists.concat2 hargs rargs
          in (Maybes.maybe (Eithers.bind (encodeVariable cx env v0 hargs) (\expr -> Right (expr, rargs))) (\el -> Maybes.maybe (Eithers.bind (encodeVariable cx env v0 hargs) (\expr -> Right (expr, rargs))) (\ts ->
            let elArity = Arity.typeSchemeArity ts
                consumeCount = Math.min elArity (Lists.length allArgs)
                consumedArgs = Lists.take consumeCount allArgs
                remainingArgs = Lists.drop consumeCount allArgs
            in (Logic.ifElse (Lists.null consumedArgs) (Eithers.bind (encodeVariable cx env v0 []) (\expr -> Right (expr, rargs))) (Right (Utils.functionCall (Utils.pyNameToPyPrimary (Names.encodeName True Util.CaseConventionLowerSnake env v0)) consumedArgs, remainingArgs)))) (Core.bindingType el)) (Lexical.lookupElement g v0))
        _ -> defaultCase

-- | Encode a union elimination as an inline conditional chain (isinstance-based ternary)
encodeUnionEliminationInline :: Context.Context -> Helpers.PythonEnvironment -> Core.CaseStatement -> Syntax.Expression -> Either (Context.InContext Errors.Error) Syntax.Expression
encodeUnionEliminationInline cx env cs pyArg =

      let tname = Core.caseStatementTypeName cs
          mdefault = Core.caseStatementDefault cs
          cases_ = Core.caseStatementCases cs
      in (Eithers.bind (Schemas.requireUnionType cx (pythonEnvironmentGetGraph env) tname) (\rt ->
        let isEnum = Schemas.isEnumRowType rt
            valueExpr = Utils.projectFromExpression pyArg (Syntax.Name "value")
            isinstancePrimary = Utils.pyNameToPyPrimary (Syntax.Name "isinstance")
        in (Eithers.bind (Maybes.maybe (Right (unsupportedExpression "no matching case in inline union elimination")) (\dflt -> encodeTermInline cx env False dflt) mdefault) (\pyDefault ->
          let encodeBranch =
                  \field ->
                    let fname = Core.fieldName field
                        fterm = Core.fieldTerm field
                        isUnitVariant = isVariantUnitType rt fname
                        pyVariantName = deconflictVariantName True env tname fname (Helpers.pythonEnvironmentGraph env)
                        isinstanceCheck =
                                Logic.ifElse isEnum (Syntax.ExpressionSimple (Syntax.Disjunction [
                                  Syntax.Conjunction [
                                    Syntax.InversionSimple (Syntax.Comparison {
                                      Syntax.comparisonLhs = (Utils.pyExpressionToBitwiseOr pyArg),
                                      Syntax.comparisonRhs = [
                                        Syntax.CompareOpBitwiseOrPair {
                                          Syntax.compareOpBitwiseOrPairOperator = Syntax.CompareOpEq,
                                          Syntax.compareOpBitwiseOrPairRhs = (Utils.pyExpressionToBitwiseOr (Utils.pyNameToPyExpression pyVariantName))}]})]])) (Utils.functionCall isinstancePrimary [
                                  pyArg,
                                  (Utils.pyNameToPyExpression pyVariantName)])
                    in (Eithers.bind (encodeTermInline cx env False fterm) (\pyBranch ->
                      let pyResult =
                              Logic.ifElse isEnum (Utils.functionCall (Utils.pyExpressionToPyPrimary pyBranch) [
                                pyArg]) (Logic.ifElse isUnitVariant (Utils.functionCall (Utils.pyExpressionToPyPrimary pyBranch) [
                                pyArg]) (Utils.functionCall (Utils.pyExpressionToPyPrimary pyBranch) [
                                valueExpr]))
                      in (Right (isinstanceCheck, pyResult))))
          in (Eithers.bind (Eithers.mapList encodeBranch cases_) (\encodedBranches ->
            let buildChain =
                    \elseExpr -> \branchPair ->
                      let checkExpr = Pairs.first branchPair
                          resultExpr = Pairs.second branchPair
                      in (Syntax.ExpressionConditional (Syntax.Conditional {
                        Syntax.conditionalBody = (Utils.pyExpressionToDisjunction resultExpr),
                        Syntax.conditionalIf = (Utils.pyExpressionToDisjunction checkExpr),
                        Syntax.conditionalElse = elseExpr}))
            in (Right (Lists.foldl buildChain pyDefault (Lists.reverse encodedBranches)))))))))

-- | Encode a term to a Python expression (inline form)
encodeTermInline :: Context.Context -> Helpers.PythonEnvironment -> Bool -> Core.Term -> Either (Context.InContext Errors.Error) Syntax.Expression
encodeTermInline cx env noCast term =

      let encode = \t -> encodeTermInline cx env False t
          stripTypeApps =
                  \t -> case t of
                    Core.TermAnnotated v0 -> stripTypeApps (Core.annotatedTermBody v0)
                    Core.TermTypeApplication v0 -> stripTypeApps (Core.typeApplicationTermBody v0)
                    _ -> t
          withCast =
                  \pyexp -> Logic.ifElse (Logic.or noCast (Helpers.pythonEnvironmentSkipCasts env)) (Right pyexp) (
                    let tc = Helpers.pythonEnvironmentGraph env
                        mtyp = Eithers.map (\_r -> Pairs.first _r) (Checking.typeOf cx tc [] term)
                    in (Eithers.either (\_ -> Right pyexp) (\typ -> Eithers.either (\_ -> Right pyexp) (\pytyp -> Right (Utils.castTo pytyp pyexp)) (encodeType env typ)) mtyp))
      in case (Rewriting.deannotateAndDetypeTerm term) of
        Core.TermApplication v0 -> encodeApplication cx env v0
        Core.TermEither v0 -> Eithers.either (\t1 -> Eithers.bind (encode t1) (\pyexp -> withCast (Utils.functionCall (Utils.pyNameToPyPrimary (Syntax.Name "Left")) [
          pyexp]))) (\t1 -> Eithers.bind (encode t1) (\pyexp -> withCast (Utils.functionCall (Utils.pyNameToPyPrimary (Syntax.Name "Right")) [
          pyexp]))) v0
        Core.TermFunction v0 -> encodeFunction cx env v0
        Core.TermLet v0 ->
          let bindings = Core.letBindings v0
              body = Core.letBody v0
          in (Logic.ifElse (Lists.null bindings) (encodeTermInline cx env False body) (withLetInline env v0 (\innerEnv -> Eithers.bind (Eithers.mapList (encodeBindingAsAssignment cx False innerEnv) bindings) (\pbindingExprs -> Eithers.bind (encodeTermInline cx innerEnv False body) (\pbody ->
            let pbindingStarExprs = Lists.map (\ne -> Syntax.StarNamedExpressionSimple ne) pbindingExprs
                pbodyStarExpr = Utils.pyExpressionToPyStarNamedExpression pbody
                tupleElements = Lists.concat2 pbindingStarExprs [
                      pbodyStarExpr]
                tupleExpr = Utils.pyAtomToPyExpression (Syntax.AtomTuple (Syntax.Tuple tupleElements))
                indexValue =
                        Utils.pyAtomToPyExpression (Syntax.AtomNumber (Syntax.NumberInteger (Literals.int32ToBigint (Lists.length bindings))))
                indexedExpr = Utils.primaryWithExpressionSlices (Utils.pyExpressionToPyPrimary tupleExpr) [
                      indexValue]
            in (Right (Utils.pyPrimaryToPyExpression indexedExpr)))))))
        Core.TermList v0 -> Eithers.bind (Eithers.mapList encode v0) (\pyExprs -> Right (Utils.pyAtomToPyExpression (Syntax.AtomTuple (Syntax.Tuple (Lists.map Utils.pyExpressionToPyStarNamedExpression pyExprs)))))
        Core.TermLiteral v0 -> encodeLiteral v0
        Core.TermMap v0 -> Eithers.bind (Eithers.mapList (\kv ->
          let k = Pairs.first kv
              v = Pairs.second kv
          in (Eithers.bind (encode k) (\pyK -> Eithers.bind (encode v) (\pyV -> Right (Syntax.DoubleStarredKvpairPair (Syntax.Kvpair {
            Syntax.kvpairKey = pyK,
            Syntax.kvpairValue = pyV})))))) (Maps.toList v0)) (\pairs -> Right (Utils.functionCall (Utils.pyNameToPyPrimary (Syntax.Name "FrozenDict")) [
          Utils.pyAtomToPyExpression (Syntax.AtomDict (Syntax.Dict pairs))]))
        Core.TermMaybe v0 -> Maybes.maybe (Right (Utils.functionCall (Utils.pyNameToPyPrimary (Syntax.Name "Nothing")) [])) (\t1 -> Eithers.bind (encode t1) (\pyexp -> withCast (Utils.functionCall (Utils.pyNameToPyPrimary (Syntax.Name "Just")) [
          pyexp]))) v0
        Core.TermPair v0 ->
          let t1 = Pairs.first v0
              t2 = Pairs.second v0
          in (Eithers.bind (encode t1) (\pyExpr1 -> Eithers.bind (encode t2) (\pyExpr2 -> Right (Utils.pyAtomToPyExpression (Syntax.AtomTuple (Syntax.Tuple [
            Utils.pyExpressionToPyStarNamedExpression pyExpr1,
            (Utils.pyExpressionToPyStarNamedExpression pyExpr2)]))))))
        Core.TermRecord v0 ->
          let tname = Core.recordTypeName v0
              fields = Core.recordFields v0
          in (Eithers.bind (Eithers.mapList (\fld -> encode (Core.fieldTerm fld)) fields) (\pargs -> Right (Utils.functionCall (Utils.pyNameToPyPrimary (Names.encodeNameQualified env tname)) pargs)))
        Core.TermSet v0 -> Eithers.bind (Eithers.mapList encode (Sets.toList v0)) (\pyEls -> Right (Utils.functionCall (Utils.pyNameToPyPrimary (Syntax.Name "frozenset")) [
          Utils.pyAtomToPyExpression (Syntax.AtomSet (Syntax.Set (Lists.map Utils.pyExpressionToPyStarNamedExpression pyEls)))]))
        Core.TermTypeApplication v0 ->
          let body = Core.typeApplicationTermBody v0
          in (Eithers.bind (encodeTermInline cx env True (stripTypeApps body)) (\pybase -> withCast pybase))
        Core.TermTypeLambda v0 ->
          let body = Core.typeLambdaBody v0
          in (withTypeLambda env v0 (\env2 -> encodeTermInline cx env2 noCast body))
        Core.TermUnion v0 ->
          let tname = Core.injectionTypeName v0
              field = Core.injectionField v0
          in (Eithers.bind (Schemas.requireUnionType cx (pythonEnvironmentGetGraph env) tname) (\rt -> Logic.ifElse (Schemas.isEnumRowType rt) (Right (Utils.projectFromExpression (Utils.pyNameToPyExpression (Names.encodeNameQualified env tname)) (Names.encodeEnumValue env (Core.fieldName field)))) (
            let fname = Core.fieldName field
                isUnitVariant =
                        Maybes.maybe False (\ft -> Schemas.isUnitType (Rewriting.deannotateType (Core.fieldTypeType ft))) (Lists.find (\ft -> Equality.equal (Core.unName (Core.fieldTypeName ft)) (Core.unName fname)) rt)
            in (Eithers.bind (Logic.ifElse (Logic.or (Schemas.isUnitTerm (Core.fieldTerm field)) isUnitVariant) (Right []) (Eithers.bind (encode (Core.fieldTerm field)) (\parg -> Right [
              parg]))) (\args ->
              let deconflictedName = deconflictVariantName True env tname fname (Helpers.pythonEnvironmentGraph env)
              in (Right (Utils.castTo (Names.typeVariableReference env tname) (Utils.functionCall (Utils.pyNameToPyPrimary deconflictedName) args))))))))
        Core.TermUnit -> Right (Utils.pyNameToPyExpression Utils.pyNone)
        Core.TermVariable v0 -> encodeVariable cx env v0 []
        Core.TermWrap v0 ->
          let tname = Core.wrappedTermTypeName v0
              inner = Core.wrappedTermBody v0
          in (Eithers.bind (encode inner) (\parg -> Right (Utils.functionCall (Utils.pyNameToPyPrimary (Names.encodeNameQualified env tname)) [
            parg])))

-- | Extend metadata based on a term (used during module encoding)
extendMetaForTerm :: Bool -> Helpers.PythonModuleMetadata -> Core.Term -> Helpers.PythonModuleMetadata
extendMetaForTerm topLevel meta0 term =

      let step =
              \meta -> \t -> case t of
                Core.TermEither v0 ->
                  let metaWithCast = setMetaUsesCast True meta
                  in (Eithers.either (\_ -> setMetaUsesLeft metaWithCast True) (\_ -> setMetaUsesRight metaWithCast True) v0)
                Core.TermFunction v0 -> case v0 of
                  Core.FunctionLambda v1 -> Maybes.maybe meta (\dom -> Logic.ifElse topLevel (extendMetaForType True False dom meta) meta) (Core.lambdaDomain v1)
                  _ -> meta
                Core.TermLet v0 ->
                  let bindings = Core.letBindings v0
                  in (Lists.foldl (
                    let forBinding =
                            \m -> \b -> Maybes.maybe m (\ts ->
                              let term1 = Core.bindingTerm b
                              in (Logic.ifElse (CoderUtils.isSimpleAssignment term1) m (extendMetaForType True True (Core.typeSchemeType ts) m))) (Core.bindingType b)
                    in forBinding) meta bindings)
                Core.TermLiteral v0 -> case v0 of
                  Core.LiteralFloat v1 -> case v1 of
                    Core.FloatValueBigfloat _ -> setMetaUsesDecimal meta True
                    _ -> meta
                  _ -> meta
                Core.TermMap _ -> setMetaUsesFrozenDict meta True
                Core.TermMaybe v0 -> Maybes.maybe (setMetaUsesNothing meta True) (\_ -> setMetaUsesJust meta True) v0
                Core.TermUnion _ -> setMetaUsesCast True meta
                _ -> meta
      in (Rewriting.foldOverTerm Coders.TraversalOrderPre step meta0 term)

-- | Extend metadata based on a type (used during module encoding)
extendMetaForType :: Bool -> Bool -> Core.Type -> Helpers.PythonModuleMetadata -> Helpers.PythonModuleMetadata
extendMetaForType topLevel isTermAnnot typ meta =

      let currentTvars = Helpers.pythonModuleMetadataTypeVariables meta
          newTvars = collectTypeVariables currentTvars typ
          metaWithTvars = setMetaTypeVariables meta newTvars
          metaWithSubtypes = Lists.foldl (\m -> \t -> extendMetaForType False isTermAnnot t m) metaWithTvars (Rewriting.subtypes typ)
      in case (Rewriting.deannotateType typ) of
        Core.TypeFunction v0 ->
          let cod = Core.functionTypeCodomain v0
              dom = Core.functionTypeDomain v0
              meta2 = extendMetaForType topLevel isTermAnnot cod metaWithSubtypes
              meta3 = extendMetaForType False isTermAnnot dom meta2
          in (Logic.ifElse (Logic.and isTermAnnot topLevel) meta3 (setMetaUsesCallable meta3 True))
        Core.TypeList _ -> setMetaUsesFrozenList metaWithSubtypes True
        Core.TypeMap _ -> setMetaUsesFrozenDict metaWithSubtypes True
        Core.TypeMaybe _ -> setMetaUsesMaybe metaWithSubtypes True
        Core.TypeEither _ -> setMetaUsesEither metaWithSubtypes True
        Core.TypeLiteral v0 -> case v0 of
          Core.LiteralTypeFloat v1 -> case v1 of
            Core.FloatTypeBigfloat -> setMetaUsesDecimal metaWithSubtypes True
            _ -> metaWithSubtypes
          _ -> metaWithSubtypes
        Core.TypeUnion v0 -> Logic.ifElse (Schemas.isEnumRowType v0) (setMetaUsesEnum metaWithSubtypes True) (Logic.ifElse (Logic.not (Lists.null v0)) (setMetaUsesNode metaWithSubtypes True) metaWithSubtypes)
        Core.TypeForall v0 ->
          let body = Core.forallTypeBody v0
              metaForWrap = digForWrap isTermAnnot metaWithSubtypes body
          in case (Rewriting.deannotateType body) of
            Core.TypeRecord _ -> setMetaUsesGeneric metaForWrap True
            _ -> metaForWrap
        Core.TypeRecord v0 ->
          let hasAnnotated = Lists.foldl (\b -> \ft -> Logic.or b (Annotations.hasTypeDescription (Core.fieldTypeType ft))) False v0
              meta1 = Logic.ifElse (Lists.null v0) metaWithSubtypes (setMetaUsesDataclass metaWithSubtypes True)
          in (Logic.ifElse hasAnnotated (setMetaUsesAnnotated meta1 True) meta1)
        Core.TypeWrap _ -> Logic.ifElse isTermAnnot metaWithSubtypes (setMetaUsesNode metaWithSubtypes True)
        _ -> metaWithSubtypes

-- | Recursively dig through forall types to find wrap types
digForWrap :: Bool -> Helpers.PythonModuleMetadata -> Core.Type -> Helpers.PythonModuleMetadata
digForWrap isTermAnnot meta typ =
    case (Rewriting.deannotateType typ) of
      Core.TypeForall v0 -> digForWrap isTermAnnot meta (Core.forallTypeBody v0)
      Core.TypeWrap _ -> Logic.ifElse isTermAnnot meta (setMetaUsesNode meta True)
      _ -> meta

setMetaNamespaces :: Module.Namespaces Syntax.DottedName -> Helpers.PythonModuleMetadata -> Helpers.PythonModuleMetadata
setMetaNamespaces ns m =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = ns,
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesLeft :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesLeft m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = b,
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesRight :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesRight m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = b,
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesDecimal :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesDecimal m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = b,
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesFrozenDict :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesFrozenDict m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = b,
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesNothing :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesNothing m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = b,
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesJust :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesJust m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = b,
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesCallable :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesCallable m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = b,
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesLruCache :: Bool -> Helpers.PythonModuleMetadata -> Helpers.PythonModuleMetadata
setMetaUsesLruCache b m =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = b,
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesCast :: Bool -> Helpers.PythonModuleMetadata -> Helpers.PythonModuleMetadata
setMetaUsesCast b m =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = b,
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesGeneric :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesGeneric m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = b,
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesFrozenList :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesFrozenList m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = b,
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesMaybe :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesMaybe m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = b,
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesEither :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesEither m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = b,
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesNode :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesNode m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = b,
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesEnum :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesEnum m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = b,
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesAnnotated :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesAnnotated m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = b,
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesDataclass :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesDataclass m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = b,
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaTypeVariables :: Helpers.PythonModuleMetadata -> S.Set Core.Name -> Helpers.PythonModuleMetadata
setMetaTypeVariables m tvars =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = tvars,
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

-- | Check if a name is a type variable (unqualified - no dots)
isTypeVariableName :: Core.Name -> Bool
isTypeVariableName name = Equality.equal 1 (Lists.length (Strings.splitOn "." (Core.unName name)))

-- | Collect type variables from a type
collectTypeVariables :: S.Set Core.Name -> Core.Type -> S.Set Core.Name
collectTypeVariables initial typ =
    case (Rewriting.deannotateType typ) of
      Core.TypeForall v0 ->
        let v = Core.forallTypeParameter v0
            body = Core.forallTypeBody v0
        in (collectTypeVariables (Sets.insert v initial) body)
      _ ->
        let freeVars = Rewriting.freeVariablesInType typ
            isTypeVar = \n -> isTypeVariableName n
            filteredList = Lists.filter isTypeVar (Sets.toList freeVars)
        in (Sets.union initial (Sets.fromList filteredList))

-- | Extend metadata for a list of types
extendMetaForTypes :: [Core.Type] -> Helpers.PythonModuleMetadata -> Helpers.PythonModuleMetadata
extendMetaForTypes types meta =

      let names = Sets.unions (Lists.map (\t -> Rewriting.typeDependencyNames False t) types)
          currentNs = Helpers.pythonModuleMetadataNamespaces meta
          updatedNs = Schemas.addNamesToNamespaces Names.encodeNamespace names currentNs
          meta1 = setMetaNamespaces updatedNs meta
      in (Lists.foldl (\m -> \t -> extendMetaForType True False t m) meta1 types)

setMetaUsesName :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesName m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = b,
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

setMetaUsesTypeVar :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesTypeVar m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = (Helpers.pythonModuleMetadataUsesTypeAlias m),
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = b}

-- | Create an initial empty metadata record with given namespaces
emptyMetadata :: Module.Namespaces Syntax.DottedName -> Helpers.PythonModuleMetadata
emptyMetadata ns =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = ns,
      Helpers.pythonModuleMetadataTypeVariables = Sets.empty,
      Helpers.pythonModuleMetadataUsesAnnotated = False,
      Helpers.pythonModuleMetadataUsesCallable = False,
      Helpers.pythonModuleMetadataUsesCast = False,
      Helpers.pythonModuleMetadataUsesLruCache = False,
      Helpers.pythonModuleMetadataUsesTypeAlias = False,
      Helpers.pythonModuleMetadataUsesDataclass = False,
      Helpers.pythonModuleMetadataUsesDecimal = False,
      Helpers.pythonModuleMetadataUsesEither = False,
      Helpers.pythonModuleMetadataUsesEnum = False,
      Helpers.pythonModuleMetadataUsesFrozenDict = False,
      Helpers.pythonModuleMetadataUsesFrozenList = False,
      Helpers.pythonModuleMetadataUsesGeneric = False,
      Helpers.pythonModuleMetadataUsesJust = False,
      Helpers.pythonModuleMetadataUsesLeft = False,
      Helpers.pythonModuleMetadataUsesMaybe = False,
      Helpers.pythonModuleMetadataUsesName = False,
      Helpers.pythonModuleMetadataUsesNode = False,
      Helpers.pythonModuleMetadataUsesNothing = False,
      Helpers.pythonModuleMetadataUsesRight = False,
      Helpers.pythonModuleMetadataUsesTypeVar = False}

-- | Gather metadata from definitions
gatherMetadata :: Module.Namespace -> [Module.Definition] -> Helpers.PythonModuleMetadata
gatherMetadata focusNs defs =

      let start = emptyMetadata (Utils.findNamespaces focusNs defs)
          addDef =
                  \meta -> \def -> case def of
                    Module.DefinitionTerm v0 ->
                      let term = Module.termDefinitionTerm v0
                          typScheme = Module.termDefinitionType v0
                          typ = Core.typeSchemeType typScheme
                          meta2 = extendMetaForType True True typ meta
                      in (extendMetaForTerm True meta2 term)
                    Module.DefinitionType v0 ->
                      let typ = Module.typeDefinitionType v0
                          meta2 = setMetaUsesName meta True
                      in (Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> extendMetaForType True False t m) meta2 typ)
          result = Lists.foldl addDef start defs
          tvars = Helpers.pythonModuleMetadataTypeVariables result
          result2 = setMetaUsesCast True (setMetaUsesLruCache True result)
      in (setMetaUsesTypeVar result2 (Logic.not (Sets.null tvars)))

setMetaUsesTypeAlias :: Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata
setMetaUsesTypeAlias m b =
    Helpers.PythonModuleMetadata {
      Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
      Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
      Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
      Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
      Helpers.pythonModuleMetadataUsesCast = (Helpers.pythonModuleMetadataUsesCast m),
      Helpers.pythonModuleMetadataUsesLruCache = (Helpers.pythonModuleMetadataUsesLruCache m),
      Helpers.pythonModuleMetadataUsesTypeAlias = b,
      Helpers.pythonModuleMetadataUsesDataclass = (Helpers.pythonModuleMetadataUsesDataclass m),
      Helpers.pythonModuleMetadataUsesDecimal = (Helpers.pythonModuleMetadataUsesDecimal m),
      Helpers.pythonModuleMetadataUsesEither = (Helpers.pythonModuleMetadataUsesEither m),
      Helpers.pythonModuleMetadataUsesEnum = (Helpers.pythonModuleMetadataUsesEnum m),
      Helpers.pythonModuleMetadataUsesFrozenDict = (Helpers.pythonModuleMetadataUsesFrozenDict m),
      Helpers.pythonModuleMetadataUsesFrozenList = (Helpers.pythonModuleMetadataUsesFrozenList m),
      Helpers.pythonModuleMetadataUsesGeneric = (Helpers.pythonModuleMetadataUsesGeneric m),
      Helpers.pythonModuleMetadataUsesJust = (Helpers.pythonModuleMetadataUsesJust m),
      Helpers.pythonModuleMetadataUsesLeft = (Helpers.pythonModuleMetadataUsesLeft m),
      Helpers.pythonModuleMetadataUsesMaybe = (Helpers.pythonModuleMetadataUsesMaybe m),
      Helpers.pythonModuleMetadataUsesName = (Helpers.pythonModuleMetadataUsesName m),
      Helpers.pythonModuleMetadataUsesNode = (Helpers.pythonModuleMetadataUsesNode m),
      Helpers.pythonModuleMetadataUsesNothing = (Helpers.pythonModuleMetadataUsesNothing m),
      Helpers.pythonModuleMetadataUsesRight = (Helpers.pythonModuleMetadataUsesRight m),
      Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)}

-- | Check whether a list of definitions contains any type definitions
isTypeModuleCheck :: [Module.Definition] -> Bool
isTypeModuleCheck defs =
    Logic.not (Lists.null (Lists.filter (\d -> case d of
      Module.DefinitionType _ -> True
      _ -> False) defs))

-- | Reorder definitions: types first, then topologically sorted terms
reorderDefs :: [Module.Definition] -> [Module.Definition]
reorderDefs defs =

      let partitioned = Schemas.partitionDefinitions defs
          typeDefsRaw = Pairs.first partitioned
          termDefsRaw = Pairs.second partitioned
          nameFirst = Lists.filter (\td -> Equality.equal (Module.typeDefinitionName td) (Core.Name "hydra.core.Name")) typeDefsRaw
          nameRest =
                  Lists.filter (\td -> Logic.not (Equality.equal (Module.typeDefinitionName td) (Core.Name "hydra.core.Name"))) typeDefsRaw
          sortedTypeDefs =
                  Lists.concat [
                    Lists.map (\td -> Module.DefinitionType td) nameFirst,
                    (Lists.map (\td -> Module.DefinitionType td) nameRest)]
          termDefs = Lists.map (\td -> Module.DefinitionTerm td) termDefsRaw
          sortedTermDefs =
                  Lists.concat (Sorting.topologicalSortNodes (\d -> case d of
                    Module.DefinitionTerm v0 -> Module.termDefinitionName v0) (\d -> case d of
                    Module.DefinitionTerm v0 -> Sets.toList (Rewriting.freeVariablesInTerm (Module.termDefinitionTerm v0))
                    _ -> []) termDefs)
      in (Lists.concat [
        sortedTypeDefs,
        sortedTermDefs])

-- | Create a TypeVar assignment statement for a type variable name
tvarStatement :: Syntax.Name -> Syntax.Statement
tvarStatement name =
    Utils.assignmentStatement name (Utils.functionCall (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "TypeVar"))) [
      Utils.doubleQuotedString (Syntax.unName name)])

-- | Conditionally include a symbol name based on a boolean flag
condImportSymbol :: t0 -> Bool -> Maybe t0
condImportSymbol name flag = Logic.ifElse flag (Just name) Nothing

-- | Generate domain import statements from namespace mappings
moduleDomainImports :: Module.Namespaces Syntax.DottedName -> [Syntax.ImportStatement]
moduleDomainImports namespaces =

      let names = Lists.sort (Maps.elems (Module.namespacesMapping namespaces))
      in (Lists.map (\ns -> Syntax.ImportStatementName (Syntax.ImportName [
        Syntax.DottedAsName {
          Syntax.dottedAsNameName = ns,
          Syntax.dottedAsNameAs = Nothing}])) names)

-- | Generate a single from-import statement
standardImportStatement :: String -> [String] -> Syntax.ImportStatement
standardImportStatement modName symbols =
    Syntax.ImportStatementFrom (Syntax.ImportFrom {
      Syntax.importFromPrefixes = [],
      Syntax.importFromDottedName = (Just (Syntax.DottedName [
        Syntax.Name modName])),
      Syntax.importFromTargets = (Syntax.ImportFromTargetsSimple (Lists.map (\s -> Syntax.ImportFromAsName {
        Syntax.importFromAsNameName = (Syntax.Name s),
        Syntax.importFromAsNameAs = Nothing}) symbols))})

-- | Generate standard import statements based on module metadata
moduleStandardImports :: Helpers.PythonModuleMetadata -> [Syntax.ImportStatement]
moduleStandardImports meta =

      let pairs =
              [
                ("__future__", [
                  condImportSymbol "annotations" Names.useFutureAnnotations]),
                ("collections.abc", [
                  condImportSymbol "Callable" (Helpers.pythonModuleMetadataUsesCallable meta)]),
                ("dataclasses", [
                  condImportSymbol "dataclass" (Helpers.pythonModuleMetadataUsesDataclass meta)]),
                ("decimal", [
                  condImportSymbol "Decimal" (Helpers.pythonModuleMetadataUsesDecimal meta)]),
                ("enum", [
                  condImportSymbol "Enum" (Helpers.pythonModuleMetadataUsesEnum meta)]),
                ("functools", [
                  condImportSymbol "lru_cache" (Helpers.pythonModuleMetadataUsesLruCache meta)]),
                ("hydra.dsl.python", [
                  condImportSymbol "Either" (Helpers.pythonModuleMetadataUsesEither meta),
                  (condImportSymbol "FrozenDict" (Helpers.pythonModuleMetadataUsesFrozenDict meta)),
                  (condImportSymbol "Just" (Helpers.pythonModuleMetadataUsesJust meta)),
                  (condImportSymbol "Left" (Helpers.pythonModuleMetadataUsesLeft meta)),
                  (condImportSymbol "Maybe" (Helpers.pythonModuleMetadataUsesMaybe meta)),
                  (condImportSymbol "Node" (Helpers.pythonModuleMetadataUsesNode meta)),
                  (condImportSymbol "Nothing" (Helpers.pythonModuleMetadataUsesNothing meta)),
                  (condImportSymbol "Right" (Helpers.pythonModuleMetadataUsesRight meta)),
                  (condImportSymbol "frozenlist" (Helpers.pythonModuleMetadataUsesFrozenList meta))]),
                ("typing", [
                  condImportSymbol "Annotated" (Helpers.pythonModuleMetadataUsesAnnotated meta),
                  (condImportSymbol "Generic" (Helpers.pythonModuleMetadataUsesGeneric meta)),
                  (condImportSymbol "TypeAlias" (Helpers.pythonModuleMetadataUsesTypeAlias meta)),
                  (condImportSymbol "TypeVar" (Helpers.pythonModuleMetadataUsesTypeVar meta)),
                  (condImportSymbol "cast" (Helpers.pythonModuleMetadataUsesCast meta))])]
          simplified =
                  Maybes.cat (Lists.map (\p ->
                    let modName = Pairs.first p
                        symbols = Maybes.cat (Pairs.second p)
                    in (Logic.ifElse (Lists.null symbols) Nothing (Just (modName, symbols)))) pairs)
      in (Lists.map (\p -> standardImportStatement (Pairs.first p) (Pairs.second p)) simplified)

-- | Generate all import statements for a Python module
moduleImports :: Module.Namespaces Syntax.DottedName -> Helpers.PythonModuleMetadata -> [Syntax.Statement]
moduleImports namespaces meta =
    Lists.map (\imp -> Utils.pySimpleStatementToPyStatement (Syntax.SimpleStatementImport imp)) (Lists.concat [
      moduleStandardImports meta,
      (moduleDomainImports namespaces)])

-- | Encode a Hydra module to a Python module AST
encodePythonModule :: Context.Context -> Graph.Graph -> Module.Module -> [Module.Definition] -> Either (Context.InContext Errors.Error) Syntax.Module
encodePythonModule cx g mod defs0 =

      let defs = reorderDefs defs0
          meta0 = gatherMetadata (Module.moduleNamespace mod) defs
          namespaces0 = Helpers.pythonModuleMetadataNamespaces meta0
          env0 = initialEnvironment namespaces0 g
          isTypeMod = isTypeModuleCheck defs0
      in (withDefinitions env0 defs (\env -> Eithers.bind (Eithers.map (\xs -> Lists.concat xs) (Eithers.mapList (\d -> encodeDefinition cx env d) defs)) (\defStmts ->
        let meta2 = Logic.ifElse (Logic.and (Logic.not isTypeMod) useInlineTypeParams) (setMetaUsesTypeVar meta0 False) meta0
            meta =
                    Logic.ifElse (Logic.and isTypeMod (Equality.equal targetPythonVersion Helpers.PythonVersionPython310)) (setMetaUsesTypeAlias meta2 True) meta2
            namespaces = Helpers.pythonModuleMetadataNamespaces meta0
            commentStmts =
                    Maybes.maybe [] (\c -> [
                      Utils.commentStatement c]) (Maybes.map CoderUtils.normalizeComment (Module.moduleDescription mod))
            importStmts = moduleImports namespaces meta
            tvars =
                    Logic.ifElse (Logic.or isTypeMod (Logic.not useInlineTypeParams)) (Helpers.pythonModuleMetadataTypeVariables meta) Sets.empty
            tvarStmts = Lists.map (\tv -> tvarStatement (Names.encodeTypeVariable tv)) (Sets.toList tvars)
            body =
                    Lists.filter (\group -> Logic.not (Lists.null group)) (Lists.concat [
                      [
                        commentStmts,
                        importStmts,
                        tvarStmts],
                      defStmts])
        in (Right (Syntax.Module body)))))

-- | Convert a Hydra module to Python source files
moduleToPython :: Module.Module -> [Module.Definition] -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) (M.Map String String)
moduleToPython mod defs cx g =
    Eithers.bind (encodePythonModule cx g mod defs) (\file ->
      let s = Serialization.printExpr (Serialization.parenthesize (Serde.encodeModule file))
          path = Names_.namespaceToFilePath Util.CaseConventionLowerSnake (Module.FileExtension "py") (Module.moduleNamespace mod)
      in (Right (Maps.singleton path s)))
