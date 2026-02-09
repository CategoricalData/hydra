-- Note: this is an automatically generated file. Do not edit.

-- | Python code generator: converts Hydra modules to Python source code

module Hydra.Ext.Python.Coder where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Arity as Arity
import qualified Hydra.Checking as Checking
import qualified Hydra.CoderUtils as CoderUtils
import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Ext.Python.Helpers as Helpers
import qualified Hydra.Ext.Python.Names as Names
import qualified Hydra.Ext.Python.Serde as Serde
import qualified Hydra.Ext.Python.Syntax as Syntax
import qualified Hydra.Ext.Python.Utils as Utils
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
useInlineTypeParamsFor :: (Helpers.PythonVersion -> Bool)
useInlineTypeParamsFor version = (Equality.equal version Helpers.PythonVersionPython312)

-- | Legacy constant for backward compatibility; use useInlineTypeParamsFor in new code
useInlineTypeParams :: Bool
useInlineTypeParams = (useInlineTypeParamsFor Utils.targetPythonVersion)

-- | Version-aware type alias statement generation
typeAliasStatementFor :: (Helpers.PythonEnvironment -> Syntax.Name -> [Syntax.TypeParameter] -> Maybe String -> Syntax.Expression -> Syntax.Statement)
typeAliasStatementFor env name tparams mcomment tyexpr = (Logic.ifElse (useInlineTypeParamsFor (Helpers.pythonEnvironmentVersion env)) (Utils.typeAliasStatement name tparams mcomment tyexpr) (Utils.typeAliasStatement310 name tparams mcomment tyexpr))

-- | Version-aware union type statement generation
unionTypeStatementsFor :: (Helpers.PythonEnvironment -> Syntax.Name -> [Syntax.TypeParameter] -> Maybe String -> Syntax.Expression -> [Syntax.Statement])
unionTypeStatementsFor env name tparams mcomment tyexpr = (Logic.ifElse (useInlineTypeParamsFor (Helpers.pythonEnvironmentVersion env)) [
  Utils.typeAliasStatement name tparams mcomment tyexpr] (Utils.unionTypeClassStatements310 name mcomment tyexpr))

-- | Wrap a Python expression in a nullary lambda (thunk) for lazy evaluation
wrapInNullaryLambda :: (Syntax.Expression -> Syntax.Expression)
wrapInNullaryLambda expr = (Syntax.ExpressionLambda (Syntax.Lambda {
  Syntax.lambdaParams = Syntax.LambdaParameters {
    Syntax.lambdaParametersSlashNoDefault = Nothing,
    Syntax.lambdaParametersParamNoDefault = [],
    Syntax.lambdaParametersParamWithDefault = [],
    Syntax.lambdaParametersStarEtc = Nothing},
  Syntax.lambdaBody = expr}))

-- | Wrap specific arguments in nullary lambdas for primitives that require lazy evaluation
wrapLazyArguments :: (Core.Name -> [Syntax.Expression] -> [Syntax.Expression])
wrapLazyArguments name args = (Logic.ifElse (Logic.and (Equality.equal name (Core.Name "hydra.lib.logic.ifElse")) (Equality.equal (Lists.length args) 3)) [
  Lists.at 0 args,
  (wrapInNullaryLambda (Lists.at 1 args)),
  (wrapInNullaryLambda (Lists.at 2 args))] args)

-- | Create integer literal expression
pyInt :: (Integer -> Syntax.Expression)
pyInt n = (Utils.pyAtomToPyExpression (Syntax.AtomNumber (Syntax.NumberInteger n)))

-- | Decorator for @lru_cache(1) to memoize zero-argument function results
lruCacheDecorator :: Syntax.NamedExpression
lruCacheDecorator = (Syntax.NamedExpressionSimple (Utils.functionCall (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "lru_cache"))) [
  pyInt 1]))

-- | Create a thunk (zero-argument lambda) wrapped with lru_cache(1) for memoization
makeThunk :: (Syntax.Expression -> Syntax.Expression)
makeThunk pbody = (Utils.functionCall (Utils.pyExpressionToPyPrimary (Utils.functionCall (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "lru_cache"))) [
  pyInt 1])) [
  wrapInNullaryLambda pbody])

-- | Create a curried lambda chain from a list of parameter names and a body
makeCurriedLambda :: ([Syntax.Name] -> Syntax.Expression -> Syntax.Expression)
makeCurriedLambda params body = (Lists.foldl (\acc -> \p -> Syntax.ExpressionLambda (Syntax.Lambda {
  Syntax.lambdaParams = Syntax.LambdaParameters {
    Syntax.lambdaParametersSlashNoDefault = Nothing,
    Syntax.lambdaParametersParamNoDefault = [
      Syntax.LambdaParamNoDefault p],
    Syntax.lambdaParametersParamWithDefault = [],
    Syntax.lambdaParametersStarEtc = Nothing},
  Syntax.lambdaBody = acc})) body (Lists.reverse params))

-- | Create Generic[...] argument expression for class definition
genericArg :: ([Core.Name] -> Maybe Syntax.Expression)
genericArg tparamList = (Logic.ifElse (Lists.null tparamList) Nothing (Just (Utils.pyPrimaryToPyExpression (Utils.primaryWithExpressionSlices (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "Generic"))) (Lists.map (\n -> Syntax.ExpressionSimple (Syntax.Disjunction [
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
      Syntax.comparisonRhs = []})]])) tparamList)))))

-- | Create args for variant (Node[type], Generic[tparams])
variantArgs :: (Syntax.Expression -> [Core.Name] -> Syntax.Args)
variantArgs ptype tparams = (Utils.pyExpressionsToPyArgs (Maybes.cat [
  Just (Utils.pyPrimaryToPyExpression (Utils.primaryWithExpressionSlices (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "Node"))) [
    ptype])),
  (genericArg tparams)]))

-- | Get type parameters from environment as Python TypeParameters
environmentTypeParameters :: (Helpers.PythonEnvironment -> [Syntax.TypeParameter])
environmentTypeParameters env = (Lists.map (\arg_ -> Utils.pyNameToPyTypeParameter (Names.encodeTypeVariable arg_)) (Pairs.first (Helpers.pythonEnvironmentBoundTypeVariables env)))

encodeFloatValue :: (Core.FloatValue -> Compute.Flow t0 Syntax.Expression)
encodeFloatValue fv = ((\x -> case x of
  Core.FloatValueBigfloat v1 -> (Flows.pure (Utils.functionCall (Utils.pyNameToPyPrimary (Syntax.Name "Decimal")) [
    Utils.singleQuotedString (Literals.showBigfloat v1)]))
  Core.FloatValueFloat32 v1 -> (Flows.pure (Utils.pyAtomToPyExpression (Syntax.AtomNumber (Syntax.NumberFloat (Literals.float32ToBigfloat v1)))))
  Core.FloatValueFloat64 v1 -> (Flows.pure (Utils.pyAtomToPyExpression (Syntax.AtomNumber (Syntax.NumberFloat v1))))) fv)

encodeIntegerValue :: (Core.IntegerValue -> Compute.Flow t0 Syntax.Expression)
encodeIntegerValue iv =  
  let toPyInt = (\n -> Flows.pure (Utils.pyAtomToPyExpression (Syntax.AtomNumber (Syntax.NumberInteger n))))
  in ((\x -> case x of
    Core.IntegerValueBigint v1 -> (toPyInt v1)
    Core.IntegerValueInt8 v1 -> (toPyInt (Literals.int8ToBigint v1))
    Core.IntegerValueInt16 v1 -> (toPyInt (Literals.int16ToBigint v1))
    Core.IntegerValueInt32 v1 -> (toPyInt (Literals.int32ToBigint v1))
    Core.IntegerValueInt64 v1 -> (toPyInt (Literals.int64ToBigint v1))
    Core.IntegerValueUint8 v1 -> (toPyInt (Literals.uint8ToBigint v1))
    Core.IntegerValueUint16 v1 -> (toPyInt (Literals.uint16ToBigint v1))
    Core.IntegerValueUint32 v1 -> (toPyInt (Literals.uint32ToBigint v1))
    Core.IntegerValueUint64 v1 -> (toPyInt (Literals.uint64ToBigint v1))) iv)

encodeLiteral :: (Core.Literal -> Compute.Flow t0 Syntax.Expression)
encodeLiteral lit = ((\x -> case x of
  Core.LiteralBinary v1 ->  
    let byteValues = (Literals.binaryToBytes v1)
    in (Flows.pure (Utils.functionCall (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "bytes"))) [
      Utils.pyAtomToPyExpression (Syntax.AtomList (Utils.pyList (Lists.map (\byteVal -> Utils.pyAtomToPyExpression (Syntax.AtomNumber (Syntax.NumberInteger (Literals.int32ToBigint byteVal)))) byteValues)))]))
  Core.LiteralBoolean v1 -> (Flows.pure (Utils.pyAtomToPyExpression (Logic.ifElse v1 Syntax.AtomTrue Syntax.AtomFalse)))
  Core.LiteralFloat v1 -> (encodeFloatValue v1)
  Core.LiteralInteger v1 -> (encodeIntegerValue v1)
  Core.LiteralString v1 -> (Flows.pure (Utils.stringToPyExpression Syntax.QuoteStyleDouble v1))) lit)

encodeLiteralType :: (Core.LiteralType -> Compute.Flow t0 Syntax.Expression)
encodeLiteralType lt =  
  let findName = ((\x -> case x of
          Core.LiteralTypeBinary -> "bytes"
          Core.LiteralTypeBoolean -> "bool"
          Core.LiteralTypeFloat v1 -> ((\x -> case x of
            Core.FloatTypeBigfloat -> "Decimal"
            Core.FloatTypeFloat32 -> "float"
            Core.FloatTypeFloat64 -> "float") v1)
          Core.LiteralTypeInteger _ -> "int"
          Core.LiteralTypeString -> "str") lt)
  in (Flows.pure (Syntax.ExpressionSimple (Syntax.Disjunction [
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

encodeApplicationType :: (Helpers.PythonEnvironment -> Core.ApplicationType -> Compute.Flow t0 Syntax.Expression)
encodeApplicationType env at =  
  let gatherParams = (\t -> \ps -> (\x -> case x of
          Core.TypeApplication v1 -> (gatherParams (Core.applicationTypeFunction v1) (Lists.cons (Core.applicationTypeArgument v1) ps))
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
          Core.TypeWrap _ -> (t, ps)) (Rewriting.deannotateType t))
  in  
    let bodyAndArgs = (gatherParams (Core.TypeApplication at) [])
    in  
      let body = (Pairs.first bodyAndArgs)
      in  
        let args = (Pairs.second bodyAndArgs)
        in (Flows.bind (encodeType env body) (\pyBody -> Flows.bind (Flows.mapList (encodeType env) args) (\pyArgs -> Flows.pure (Utils.primaryAndParams (Utils.pyExpressionToPyPrimary pyBody) pyArgs))))

encodeForallType :: (Helpers.PythonEnvironment -> Core.ForallType -> Compute.Flow t0 Syntax.Expression)
encodeForallType env lt =  
  let gatherParams = (\t -> \ps -> (\x -> case x of
          Core.TypeForall v1 -> (gatherParams (Core.forallTypeBody v1) (Lists.cons (Core.forallTypeParameter v1) ps))
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
          Core.TypeWrap _ -> (t, (Lists.reverse ps))) (Rewriting.deannotateType t))
  in  
    let bodyAndParams = (gatherParams (Core.TypeForall lt) [])
    in  
      let body = (Pairs.first bodyAndParams)
      in  
        let params = (Pairs.second bodyAndParams)
        in (Flows.bind (encodeType env body) (\pyBody -> Flows.pure (Utils.primaryAndParams (Utils.pyExpressionToPyPrimary pyBody) (Lists.map (\n -> Syntax.ExpressionSimple (Syntax.Disjunction [
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

encodeFunctionType :: (Helpers.PythonEnvironment -> Core.FunctionType -> Compute.Flow t0 Syntax.Expression)
encodeFunctionType env ft =  
  let gatherParams = (\rdoms -> \ftype ->  
          let cod = (Core.functionTypeCodomain ftype)
          in  
            let dom = (Core.functionTypeDomain ftype)
            in ((\x -> case x of
              Core.TypeFunction v1 -> (gatherParams (Lists.cons dom rdoms) v1)
              Core.TypeAnnotated _ -> (Lists.reverse (Lists.cons dom rdoms), cod)
              Core.TypeApplication _ -> (Lists.reverse (Lists.cons dom rdoms), cod)
              Core.TypeForall _ -> (Lists.reverse (Lists.cons dom rdoms), cod)
              Core.TypeList _ -> (Lists.reverse (Lists.cons dom rdoms), cod)
              Core.TypeLiteral _ -> (Lists.reverse (Lists.cons dom rdoms), cod)
              Core.TypeMap _ -> (Lists.reverse (Lists.cons dom rdoms), cod)
              Core.TypeMaybe _ -> (Lists.reverse (Lists.cons dom rdoms), cod)
              Core.TypeEither _ -> (Lists.reverse (Lists.cons dom rdoms), cod)
              Core.TypePair _ -> (Lists.reverse (Lists.cons dom rdoms), cod)
              Core.TypeRecord _ -> (Lists.reverse (Lists.cons dom rdoms), cod)
              Core.TypeSet _ -> (Lists.reverse (Lists.cons dom rdoms), cod)
              Core.TypeUnion _ -> (Lists.reverse (Lists.cons dom rdoms), cod)
              Core.TypeUnit -> (Lists.reverse (Lists.cons dom rdoms), cod)
              Core.TypeVariable _ -> (Lists.reverse (Lists.cons dom rdoms), cod)
              Core.TypeWrap _ -> (Lists.reverse (Lists.cons dom rdoms), cod)) (Rewriting.deannotateType cod)))
  in  
    let domsAndCod = (gatherParams [] ft)
    in  
      let doms = (Pairs.first domsAndCod)
      in  
        let cod = (Pairs.second domsAndCod)
        in (Flows.bind (Flows.mapList (encodeType env) doms) (\pydoms -> Flows.bind (encodeType env cod) (\pycod -> Flows.pure (Utils.pyPrimaryToPyExpression (Utils.primaryWithSlices (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "Callable"))) (Utils.pyPrimaryToPySlice (Syntax.PrimarySimple (Syntax.AtomList (Utils.pyList pydoms)))) [
          Syntax.SliceOrStarredExpressionSlice (Utils.pyExpressionToPySlice pycod)])))))

encodeType :: (Helpers.PythonEnvironment -> Core.Type -> Compute.Flow t0 Syntax.Expression)
encodeType env typ =  
  let dflt = (Flows.pure (Utils.doubleQuotedString (Strings.cat2 "type = " (Core_.type_ (Rewriting.deannotateType typ)))))
  in ((\x -> case x of
    Core.TypeApplication v1 -> (encodeApplicationType env v1)
    Core.TypeFunction v1 -> (encodeFunctionType env v1)
    Core.TypeForall v1 -> (encodeForallType env v1)
    Core.TypeList v1 -> (Flows.bind (encodeType env v1) (\pyet -> Flows.pure (Utils.nameAndParams (Syntax.Name "frozenlist") [
      pyet])))
    Core.TypeMap v1 -> (Flows.bind (encodeType env (Core.mapTypeKeys v1)) (\pykt -> Flows.bind (encodeType env (Core.mapTypeValues v1)) (\pyvt -> Flows.pure (Utils.nameAndParams (Syntax.Name "FrozenDict") [
      pykt,
      pyvt]))))
    Core.TypeLiteral v1 -> (encodeLiteralType v1)
    Core.TypeMaybe v1 -> (Flows.bind (encodeType env v1) (\ptype -> Flows.pure (Utils.pyPrimaryToPyExpression (Utils.primaryWithExpressionSlices (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "Maybe"))) [
      ptype]))))
    Core.TypeEither v1 -> (Flows.bind (encodeType env (Core.eitherTypeLeft v1)) (\pyleft -> Flows.bind (encodeType env (Core.eitherTypeRight v1)) (\pyright -> Flows.pure (Utils.pyPrimaryToPyExpression (Utils.primaryWithExpressionSlices (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "Either"))) [
      pyleft,
      pyright])))))
    Core.TypePair v1 -> (Flows.bind (encodeType env (Core.pairTypeFirst v1)) (\pyFirst -> Flows.bind (encodeType env (Core.pairTypeSecond v1)) (\pySecond -> Flows.pure (Utils.nameAndParams (Syntax.Name "tuple") [
      pyFirst,
      pySecond]))))
    Core.TypeRecord v1 -> (Flows.pure (Names.typeVariableReference env (Core.rowTypeTypeName v1)))
    Core.TypeSet v1 -> (Flows.bind (encodeType env v1) (\pyet -> Flows.pure (Utils.nameAndParams (Syntax.Name "frozenset") [
      pyet])))
    Core.TypeUnion v1 -> (Flows.pure (Names.typeVariableReference env (Core.rowTypeTypeName v1)))
    Core.TypeUnit -> (Flows.pure (Utils.pyNameToPyExpression Utils.pyNone))
    Core.TypeVariable v1 -> (Flows.pure (Names.typeVariableReference env v1))
    Core.TypeWrap v1 -> (Flows.pure (Names.typeVariableReference env (Core.wrappedTypeTypeName v1)))
    Core.TypeAnnotated _ -> dflt) (Rewriting.deannotateType typ))

encodeTypeQuoted :: (Helpers.PythonEnvironment -> Core.Type -> Compute.Flow t0 Syntax.Expression)
encodeTypeQuoted env typ = (Flows.bind (encodeType env typ) (\pytype -> Flows.pure (Logic.ifElse (Sets.null (Rewriting.freeVariablesInType typ)) pytype (Utils.doubleQuotedString (Serialization.printExpr (Serde.encodeExpression pytype))))))

-- | Generate name constants for a type
encodeNameConstants :: (Helpers.PythonEnvironment -> Core.Name -> Core.Type -> [Syntax.Statement])
encodeNameConstants env name typ =  
  let toStmt = (\pair -> Utils.assignmentStatement (Pairs.first pair) (Utils.functionCall (Utils.pyNameToPyPrimary (Names.encodeName True Util.CaseConventionPascal env (Core.Name "hydra.core.Name"))) [
          Utils.doubleQuotedString (Core.unName (Pairs.second pair))]))
  in  
    let namePair = (Names.encodeConstantForTypeName env name, name)
    in  
      let fieldPair = (\field -> (Names.encodeConstantForFieldName env name (Core.fieldTypeName field), (Core.fieldTypeName field)))
      in  
        let fieldPairs = (\t -> (\x -> case x of
                Core.TypeForall v1 -> (fieldPairs (Core.forallTypeBody v1))
                Core.TypeRecord v1 -> (Lists.map fieldPair (Core.rowTypeFields v1))
                Core.TypeUnion v1 -> (Lists.map fieldPair (Core.rowTypeFields v1))
                Core.TypeAnnotated _ -> []
                Core.TypeApplication _ -> []
                Core.TypeFunction _ -> []
                Core.TypeList _ -> []
                Core.TypeLiteral _ -> []
                Core.TypeMap _ -> []
                Core.TypeMaybe _ -> []
                Core.TypeEither _ -> []
                Core.TypePair _ -> []
                Core.TypeSet _ -> []
                Core.TypeUnit -> []
                Core.TypeVariable _ -> []
                Core.TypeWrap _ -> []) (Rewriting.deannotateType t))
        in (Lists.map toStmt (Lists.cons namePair (fieldPairs typ)))

-- | Find type parameters in a type that are bound in the environment
findTypeParams :: (Helpers.PythonEnvironment -> Core.Type -> [Core.Name])
findTypeParams env typ =  
  let boundVars = (Pairs.second (Helpers.pythonEnvironmentBoundTypeVariables env))
  in  
    let isBound = (\v -> Maybes.isJust (Maps.lookup v boundVars))
    in (Lists.filter isBound (Sets.toList (Rewriting.freeVariablesInType typ)))

encodeWrappedType :: (Helpers.PythonEnvironment -> Core.Name -> Core.Type -> Maybe String -> Compute.Flow t0 Syntax.Statement)
encodeWrappedType env name typ comment =  
  let tparamList = (Pairs.first (Helpers.pythonEnvironmentBoundTypeVariables env))
  in (Flows.bind (encodeTypeQuoted env typ) (\ptypeQuoted ->  
    let body = (Utils.indentedBlock comment [])
    in (Flows.pure (Utils.pyClassDefinitionToPyStatement (Syntax.ClassDefinition {
      Syntax.classDefinitionDecorators = Nothing,
      Syntax.classDefinitionName = (Names.encodeName False Util.CaseConventionPascal env name),
      Syntax.classDefinitionTypeParams = (Lists.map (\arg_ -> Utils.pyNameToPyTypeParameter (Names.encodeTypeVariable arg_)) (findTypeParams env typ)),
      Syntax.classDefinitionArguments = (Just (variantArgs ptypeQuoted tparamList)),
      Syntax.classDefinitionBody = body})))))

-- | Extend a PythonEnvironment with a new bound type variable
extendEnvWithTypeVar :: (Helpers.PythonEnvironment -> Core.Name -> Helpers.PythonEnvironment)
extendEnvWithTypeVar env var_ =  
  let oldBound = (Helpers.pythonEnvironmentBoundTypeVariables env)
  in  
    let tparamList = (Pairs.first oldBound)
    in  
      let tparamMap = (Pairs.second oldBound)
      in  
        let newList = (Lists.concat2 tparamList [
                var_])
        in  
          let newMap = (Maps.insert var_ (Names.encodeTypeVariable var_) tparamMap)
          in Helpers.PythonEnvironment {
            Helpers.pythonEnvironmentNamespaces = (Helpers.pythonEnvironmentNamespaces env),
            Helpers.pythonEnvironmentBoundTypeVariables = (newList, newMap),
            Helpers.pythonEnvironmentTypeContext = (Helpers.pythonEnvironmentTypeContext env),
            Helpers.pythonEnvironmentNullaryBindings = (Helpers.pythonEnvironmentNullaryBindings env),
            Helpers.pythonEnvironmentVersion = (Helpers.pythonEnvironmentVersion env),
            Helpers.pythonEnvironmentSkipCasts = (Helpers.pythonEnvironmentSkipCasts env),
            Helpers.pythonEnvironmentInlineVariables = (Helpers.pythonEnvironmentInlineVariables env)}

-- | Extract lambdas and their bodies from a term
gatherLambdas :: (Core.Term -> ([Core.Name], Core.Term))
gatherLambdas term =  
  let go = (\params -> \t -> (\x -> case x of
          Core.TermFunction v1 -> ((\x -> case x of
            Core.FunctionLambda v2 -> (go (Lists.concat2 params [
              Core.lambdaParameter v2]) (Core.lambdaBody v2))
            _ -> (params, t)) v1)
          _ -> (params, t)) (Rewriting.deannotateAndDetypeTerm t))
  in (go [] term)

-- | Extend environment with lambda parameters from a term
extendEnvWithLambdaParams :: (Helpers.PythonEnvironment -> Core.Term -> Helpers.PythonEnvironment)
extendEnvWithLambdaParams env term =  
  let go = (\e -> \t -> (\x -> case x of
          Core.TermFunction v1 -> ((\x -> case x of
            Core.FunctionLambda v2 ->  
              let newTc = (Schemas.extendTypeContextForLambda (pythonEnvironmentGetTypeContext e) v2)
              in  
                let newEnv = (pythonEnvironmentSetTypeContext newTc e)
                in (go newEnv (Core.lambdaBody v2))
            _ -> e) v1)
          _ -> e) (Rewriting.deannotateTerm t))
  in (go env term)

-- | Wrap a bare reference to a polymorphic function in an uncurried lambda
makeSimpleLambda :: (Int -> Syntax.Expression -> Syntax.Expression)
makeSimpleLambda arity lhs =  
  let args = (Lists.map (\i -> Syntax.Name (Strings.cat2 "x" (Literals.showInt32 i))) (Math.range 1 arity))
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
isCaseStatementApplication :: (Core.Term -> Maybe (Core.Name, (Maybe Core.Term, ([Core.Field], Core.Term))))
isCaseStatementApplication term =  
  let gathered = (CoderUtils.gatherApplications term)
  in  
    let args = (Pairs.first gathered)
    in  
      let body = (Pairs.second gathered)
      in (Logic.ifElse (Logic.not (Equality.equal (Lists.length args) 1)) Nothing ( 
        let arg = (Lists.head args)
        in ((\x -> case x of
          Core.TermFunction v1 -> ((\x -> case x of
            Core.FunctionElimination v2 -> ((\x -> case x of
              Core.EliminationUnion v3 -> (Just (Core.caseStatementTypeName v3, (Core.caseStatementDefault v3, (Core.caseStatementCases v3, arg))))
              _ -> Nothing) v2)
            _ -> Nothing) v1)
          _ -> Nothing) (Rewriting.deannotateAndDetypeTerm body))))

-- | Check if a variant field has unit type
isVariantUnitType :: (Core.RowType -> Core.Name -> Bool)
isVariantUnitType rowType fieldName =  
  let fields = (Core.rowTypeFields rowType)
  in  
    let mfield = (Lists.find (\ft -> Equality.equal (Core.fieldTypeName ft) fieldName) fields)
    in (Maybes.fromMaybe False (Maybes.map (\ft -> Schemas.isUnitType (Rewriting.deannotateType (Core.fieldTypeType ft))) mfield))

-- | Create a wildcard case block with a given body statement
wildcardCaseBlock :: (Syntax.Statement -> Syntax.CaseBlock)
wildcardCaseBlock stmt = Syntax.CaseBlock {
  Syntax.caseBlockPatterns = (Utils.pyClosedPatternToPyPatterns Syntax.ClosedPatternWildcard),
  Syntax.caseBlockGuard = Nothing,
  Syntax.caseBlockBody = (Utils.indentedBlock Nothing [
    [
      stmt]])}

-- | Create a value pattern for an enum variant
enumVariantPattern :: (Helpers.PythonEnvironment -> Core.Name -> Core.Name -> Syntax.ClosedPattern)
enumVariantPattern env typeName fieldName = (Syntax.ClosedPatternValue (Syntax.ValuePattern (Syntax.Attribute [
  Names.encodeName True Util.CaseConventionPascal env typeName,
  (Names.encodeEnumValue env fieldName)])))

-- | Create a class pattern for a unit variant (no value captured)
classVariantPatternUnit :: (Helpers.PythonEnvironment -> Core.Name -> Core.Name -> Syntax.ClosedPattern)
classVariantPatternUnit env typeName fieldName = (Syntax.ClosedPatternClass (Syntax.ClassPattern {
  Syntax.classPatternNameOrAttribute = (Syntax.NameOrAttribute [
    Names.variantName True env typeName fieldName]),
  Syntax.classPatternPositionalPatterns = Nothing,
  Syntax.classPatternKeywordPatterns = Nothing}))

-- | Create a class pattern for a variant with captured value
classVariantPatternWithCapture :: (Helpers.PythonEnvironment -> Core.Name -> Core.Name -> Core.Name -> Syntax.ClosedPattern)
classVariantPatternWithCapture env typeName fieldName varName =  
  let pyVarName = (Syntax.NameOrAttribute [
          Names.variantName True env typeName fieldName])
  in  
    let capturePattern = (Syntax.ClosedPatternCapture (Syntax.CapturePattern (Syntax.PatternCaptureTarget (Names.encodeName False Util.CaseConventionLowerSnake env varName))))
    in  
      let keywordPattern = Syntax.KeywordPattern {
              Syntax.keywordPatternName = (Syntax.Name "value"),
              Syntax.keywordPatternPattern = (Syntax.PatternOr (Syntax.OrPattern [
                capturePattern]))}
      in (Syntax.ClosedPatternClass (Syntax.ClassPattern {
        Syntax.classPatternNameOrAttribute = pyVarName,
        Syntax.classPatternPositionalPatterns = Nothing,
        Syntax.classPatternKeywordPatterns = (Just (Syntax.KeywordPatterns [
          keywordPattern]))}))

isCasesFull :: (Core.RowType -> [t0] -> Bool)
isCasesFull rowType cases_ =  
  let numCases = (Lists.length cases_)
  in  
    let numFields = (Lists.length (Core.rowTypeFields rowType))
    in (Logic.not (Equality.lt numCases numFields))

variantClosedPattern :: (Helpers.PythonEnvironment -> Core.Name -> Core.Name -> t0 -> Bool -> Core.Name -> Bool -> Syntax.ClosedPattern)
variantClosedPattern env typeName fieldName rowType isEnum varName shouldCapture = (Logic.ifElse isEnum (enumVariantPattern env typeName fieldName) (Logic.ifElse (Logic.not shouldCapture) (classVariantPatternUnit env typeName fieldName) (classVariantPatternWithCapture env typeName fieldName varName)))

-- | Rewrite case statements to avoid variable name collisions
deduplicateCaseVariables :: ([Core.Field] -> [Core.Field])
deduplicateCaseVariables cases_ =  
  let rewriteCase = (\state -> \field ->  
          let countByName = (Pairs.first state)
          in  
            let done = (Pairs.second state)
            in  
              let fname = (Core.fieldName field)
              in  
                let fterm = (Core.fieldTerm field)
                in ((\x -> case x of
                  Core.TermFunction v1 -> ((\x -> case x of
                    Core.FunctionLambda v2 ->  
                      let v = (Core.lambdaParameter v2)
                      in  
                        let mdom = (Core.lambdaDomain v2)
                        in  
                          let body = (Core.lambdaBody v2)
                          in (Maybes.maybe (Maps.insert v 1 countByName, (Lists.cons field done)) (\count ->  
                            let count2 = (Math.add count 1)
                            in  
                              let v2 = (Core.Name (Strings.cat2 (Core.unName v) (Literals.showInt32 count2)))
                              in  
                                let newBody = (Reduction.alphaConvert v v2 body)
                                in  
                                  let newLam = Core.Lambda {
                                          Core.lambdaParameter = v2,
                                          Core.lambdaDomain = mdom,
                                          Core.lambdaBody = newBody}
                                  in  
                                    let newTerm = (Core.TermFunction (Core.FunctionLambda newLam))
                                    in  
                                      let newField = Core.Field {
                                              Core.fieldName = fname,
                                              Core.fieldTerm = newTerm}
                                      in (Maps.insert v count2 countByName, (Lists.cons newField done))) (Maps.lookup v countByName))
                    _ -> (countByName, (Lists.cons field done))) v1)
                  _ -> (countByName, (Lists.cons field done))) (Rewriting.deannotateTerm fterm)))
  in  
    let result = (Lists.foldl rewriteCase (Maps.empty, []) cases_)
    in (Lists.reverse (Pairs.second result))

-- | Substitute unit for a variable in a term (for unit variant case handling)
eliminateUnitVar :: (Core.Name -> Core.Term -> Core.Term)
eliminateUnitVar v term0 =  
  let rewriteField = (\rewrite -> \fld -> Core.Field {
          Core.fieldName = (Core.fieldName fld),
          Core.fieldTerm = (rewrite (Core.fieldTerm fld))})
  in  
    let rewriteBinding = (\rewrite -> \bnd -> Core.Binding {
            Core.bindingName = (Core.bindingName bnd),
            Core.bindingTerm = (rewrite (Core.bindingTerm bnd)),
            Core.bindingType = (Core.bindingType bnd)})
    in  
      let rewrite = (\recurse -> \term -> (\x -> case x of
              Core.TermVariable v1 -> (Logic.ifElse (Equality.equal v1 v) Core.TermUnit term)
              Core.TermAnnotated v1 -> (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (recurse (Core.annotatedTermBody v1)),
                Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v1)}))
              Core.TermApplication v1 -> (Core.TermApplication (Core.Application {
                Core.applicationFunction = (recurse (Core.applicationFunction v1)),
                Core.applicationArgument = (recurse (Core.applicationArgument v1))}))
              Core.TermFunction v1 -> ((\x -> case x of
                Core.FunctionLambda v2 -> (Logic.ifElse (Equality.equal (Core.lambdaParameter v2) v) term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.lambdaParameter v2),
                  Core.lambdaDomain = (Core.lambdaDomain v2),
                  Core.lambdaBody = (recurse (Core.lambdaBody v2))}))))
                Core.FunctionElimination v2 -> ((\x -> case x of
                  Core.EliminationUnion v3 -> (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.caseStatementTypeName v3),
                    Core.caseStatementDefault = (Maybes.map recurse (Core.caseStatementDefault v3)),
                    Core.caseStatementCases = (Lists.map (rewriteField recurse) (Core.caseStatementCases v3))}))))
                  _ -> term) v2)
                _ -> term) v1)
              Core.TermLet v1 -> (Core.TermLet (Core.Let {
                Core.letBindings = (Lists.map (rewriteBinding recurse) (Core.letBindings v1)),
                Core.letBody = (recurse (Core.letBody v1))}))
              Core.TermList v1 -> (Core.TermList (Lists.map recurse v1))
              Core.TermMap v1 -> (Core.TermMap (Maps.fromList (Lists.map (\kv -> (recurse (Pairs.first kv), (recurse (Pairs.second kv)))) (Maps.toList v1))))
              Core.TermRecord v1 -> (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.recordTypeName v1),
                Core.recordFields = (Lists.map (rewriteField recurse) (Core.recordFields v1))}))
              Core.TermSet v1 -> (Core.TermSet (Sets.map recurse v1))
              Core.TermUnion v1 -> (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = (Core.injectionTypeName v1),
                Core.injectionField = (rewriteField recurse (Core.injectionField v1))}))
              Core.TermMaybe v1 -> (Core.TermMaybe (Maybes.map recurse v1))
              Core.TermPair v1 -> (Core.TermPair (recurse (Pairs.first v1), (recurse (Pairs.second v1))))
              Core.TermWrap v1 -> (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.wrappedTermTypeName v1),
                Core.wrappedTermBody = (recurse (Core.wrappedTermBody v1))}))
              Core.TermEither v1 -> (Core.TermEither (Eithers.bimap recurse recurse v1))
              Core.TermTypeApplication v1 -> (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (recurse (Core.typeApplicationTermBody v1)),
                Core.typeApplicationTermType = (Core.typeApplicationTermType v1)}))
              Core.TermTypeLambda v1 -> (Core.TermTypeLambda (Core.TypeLambda {
                Core.typeLambdaParameter = (Core.typeLambdaParameter v1),
                Core.typeLambdaBody = (recurse (Core.typeLambdaBody v1))}))
              _ -> term) (Rewriting.deannotateTerm term))
      in  
        let go = (\term -> rewrite go term)
        in (go term0)

encodeDefaultCaseBlock :: ((t0 -> Compute.Flow t1 Syntax.Expression) -> Bool -> Maybe t0 -> Core.Name -> Compute.Flow t1 [Syntax.CaseBlock])
encodeDefaultCaseBlock encodeTerm isFull mdflt tname = (Flows.bind (Maybes.maybe (Flows.pure (Logic.ifElse isFull (Utils.raiseAssertionError "Unreachable: all variants handled") (Utils.raiseTypeError (Strings.cat2 "Unsupported " (Names_.localNameOf tname))))) (\d -> Flows.bind (encodeTerm d) (\pyexpr -> Flows.pure (Utils.returnSingle pyexpr))) mdflt) (\stmt ->  
  let patterns = (Utils.pyClosedPatternToPyPatterns Syntax.ClosedPatternWildcard)
  in  
    let body = (Utils.indentedBlock Nothing [
            [
              stmt]])
    in (Flows.pure [
      Syntax.CaseBlock {
        Syntax.caseBlockPatterns = patterns,
        Syntax.caseBlockGuard = Nothing,
        Syntax.caseBlockBody = body}])))

encodeCaseBlock :: (Helpers.PythonEnvironment -> Core.Name -> Core.RowType -> Bool -> (Helpers.PythonEnvironment -> Core.Term -> Compute.Flow t0 [Syntax.Statement]) -> Core.Field -> Compute.Flow t0 Syntax.CaseBlock)
encodeCaseBlock env tname rowType isEnum encodeBody field =  
  let fname = (Core.fieldName field)
  in  
    let fterm = (Core.fieldTerm field)
    in ((\x -> case x of
      Core.TermFunction v1 -> ((\x -> case x of
        Core.FunctionLambda v2 ->  
          let v = (Core.lambdaParameter v2)
          in  
            let rawBody = (Core.lambdaBody v2)
            in  
              let isUnitVariant = (isVariantUnitType rowType fname)
              in  
                let effectiveBody = (Logic.ifElse isUnitVariant (eliminateUnitVar v rawBody) rawBody)
                in  
                  let shouldCapture = (Logic.not (Logic.or isUnitVariant (Logic.or (Rewriting.isFreeVariableInTerm v rawBody) (Schemas.isUnitTerm rawBody))))
                  in (Flows.bind (Flows.pure (pythonEnvironmentSetTypeContext (Schemas.extendTypeContextForLambda (pythonEnvironmentGetTypeContext env) v2) env)) (\env2 ->  
                    let pattern = (variantClosedPattern env2 tname fname rowType isEnum v shouldCapture)
                    in (Flows.bind (encodeBody env2 effectiveBody) (\stmts ->  
                      let pyBody = (Utils.indentedBlock Nothing [
                              stmts])
                      in (Flows.pure (Syntax.CaseBlock {
                        Syntax.caseBlockPatterns = (Utils.pyClosedPatternToPyPatterns pattern),
                        Syntax.caseBlockGuard = Nothing,
                        Syntax.caseBlockBody = pyBody}))))))) v1)) (Rewriting.deannotateTerm fterm))

-- | Accessor for the graph field of PyGraph
pyGraphGraph :: (Helpers.PyGraph -> Graph.Graph)
pyGraphGraph = Helpers.pyGraphGraph

-- | Accessor for the metadata field of PyGraph
pyGraphMetadata :: (Helpers.PyGraph -> Helpers.PythonModuleMetadata)
pyGraphMetadata = Helpers.pyGraphMetadata

-- | Constructor for PyGraph record
makePyGraph :: (Graph.Graph -> Helpers.PythonModuleMetadata -> Helpers.PyGraph)
makePyGraph g m = Helpers.PyGraph {
  Helpers.pyGraphGraph = g,
  Helpers.pyGraphMetadata = m}

inGraphContext :: (Compute.Flow Graph.Graph t0 -> Compute.Flow Helpers.PyGraph t0)
inGraphContext graphFlow = (CoderUtils.inCoderGraphContext pyGraphGraph pyGraphMetadata makePyGraph graphFlow)

-- | Encode a field type for record definitions (field: type annotation)
encodeFieldType :: (Helpers.PythonEnvironment -> Core.FieldType -> Compute.Flow Helpers.PyGraph Syntax.Statement)
encodeFieldType env fieldType =  
  let fname = (Core.fieldTypeName fieldType)
  in  
    let ftype = (Core.fieldTypeType fieldType)
    in (Flows.bind (inGraphContext (Annotations.getTypeDescription ftype)) (\comment ->  
      let pyName = (Syntax.SingleTargetName (Names.encodeFieldName env fname))
      in (Flows.bind (encodeType env ftype) (\pyType ->  
        let annotatedPyType = (Utils.annotatedExpression comment pyType)
        in (Flows.pure (Utils.pyAssignmentToPyStatement (Syntax.AssignmentTyped (Syntax.TypedAssignment {
          Syntax.typedAssignmentLhs = pyName,
          Syntax.typedAssignmentType = annotatedPyType,
          Syntax.typedAssignmentRhs = Nothing}))))))))

-- | Create a @dataclass(frozen=True) decorator
dataclassDecorator :: Syntax.NamedExpression
dataclassDecorator = (Syntax.NamedExpressionSimple (Utils.pyPrimaryToPyExpression (Utils.primaryWithRhs (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "dataclass"))) (Syntax.PrimaryRhsCall (Syntax.Args {
  Syntax.argsPositional = [],
  Syntax.argsKwargOrStarred = [
    Syntax.KwargOrStarredKwarg (Syntax.Kwarg {
      Syntax.kwargName = (Syntax.Name "frozen"),
      Syntax.kwargValue = (Utils.pyAtomToPyExpression Syntax.AtomTrue)})],
  Syntax.argsKwargOrDoubleStarred = []})))))

-- | Encode a record type as a Python dataclass
encodeRecordType :: (Helpers.PythonEnvironment -> Core.Name -> Core.RowType -> Maybe String -> Compute.Flow Helpers.PyGraph Syntax.Statement)
encodeRecordType env name rowType comment =  
  let tfields = (Core.rowTypeFields rowType)
  in (Flows.bind (Flows.mapList (encodeFieldType env) tfields) (\pyFields ->  
    let body = (Utils.indentedBlock comment [
            pyFields])
    in  
      let boundVars = (Helpers.pythonEnvironmentBoundTypeVariables env)
      in  
        let tparamList = (Pairs.first boundVars)
        in  
          let mGenericArg = (genericArg tparamList)
          in  
            let args = (Maybes.maybe Nothing (\a -> Just (Utils.pyExpressionsToPyArgs [
                    a])) mGenericArg)
            in  
              let decs = (Just (Syntax.Decorators [
                      dataclassDecorator]))
              in  
                let pyName = (Names.encodeName False Util.CaseConventionPascal env name)
                in  
                  let noTypeParams = []
                  in (Flows.pure (Utils.pyClassDefinitionToPyStatement (Syntax.ClassDefinition {
                    Syntax.classDefinitionDecorators = decs,
                    Syntax.classDefinitionName = pyName,
                    Syntax.classDefinitionTypeParams = noTypeParams,
                    Syntax.classDefinitionArguments = args,
                    Syntax.classDefinitionBody = body})))))

-- | Encode an enum value assignment statement with optional comment
encodeEnumValueAssignment :: (Helpers.PythonEnvironment -> Core.FieldType -> Compute.Flow Helpers.PyGraph [Syntax.Statement])
encodeEnumValueAssignment env fieldType =  
  let fname = (Core.fieldTypeName fieldType)
  in  
    let ftype = (Core.fieldTypeType fieldType)
    in (Flows.bind (inGraphContext (Annotations.getTypeDescription ftype)) (\mcomment ->  
      let pyName = (Names.encodeEnumValue env fname)
      in  
        let fnameStr = (Core.unName fname)
        in  
          let pyValue = (Utils.doubleQuotedString fnameStr)
          in  
            let assignStmt = (Utils.assignmentStatement pyName pyValue)
            in (Flows.pure (Maybes.maybe [
              assignStmt] (\c -> [
              assignStmt,
              (Utils.pyExpressionToPyStatement (Utils.tripleQuotedString c))]) mcomment))))

-- | Encode a union field as a variant class
encodeUnionField :: (Helpers.PythonEnvironment -> Core.Name -> Core.FieldType -> Compute.Flow Helpers.PyGraph Syntax.Statement)
encodeUnionField env unionName fieldType =  
  let fname = (Core.fieldTypeName fieldType)
  in  
    let ftype = (Core.fieldTypeType fieldType)
    in (Flows.bind (inGraphContext (Annotations.getTypeDescription ftype)) (\fcomment ->  
      let isUnit = (Equality.equal (Rewriting.deannotateType ftype) Core.TypeUnit)
      in  
        let varName = (Names.variantName False env unionName fname)
        in  
          let tparamNames = (findTypeParams env ftype)
          in  
            let tparamPyNames = (Lists.map Names.encodeTypeVariable tparamNames)
            in  
              let fieldParams = (Lists.map Utils.pyNameToPyTypeParameter tparamPyNames)
              in  
                let body = (Logic.ifElse isUnit (Utils.indentedBlock fcomment [
                        Utils.unitVariantMethods varName]) (Utils.indentedBlock fcomment []))
                in (Flows.bind (Logic.ifElse isUnit (Flows.pure Nothing) (Flows.bind (encodeTypeQuoted env ftype) (\quotedType -> Flows.pure (Just (variantArgs quotedType []))))) (\margs -> Flows.pure (Utils.pyClassDefinitionToPyStatement (Syntax.ClassDefinition {
                  Syntax.classDefinitionDecorators = Nothing,
                  Syntax.classDefinitionName = varName,
                  Syntax.classDefinitionTypeParams = fieldParams,
                  Syntax.classDefinitionArguments = margs,
                  Syntax.classDefinitionBody = body}))))))

-- | Encode a union type as an enum (for unit-only fields) or variant classes
encodeUnionType :: (Helpers.PythonEnvironment -> Core.Name -> Core.RowType -> Maybe String -> Compute.Flow Helpers.PyGraph [Syntax.Statement])
encodeUnionType env name rowType comment =  
  let tfields = (Core.rowTypeFields rowType)
  in (Logic.ifElse (Schemas.isEnumRowType rowType) (Flows.bind (Flows.mapList (encodeEnumValueAssignment env) tfields) (\vals ->  
    let body = (Utils.indentedBlock comment vals)
    in  
      let enumName = (Syntax.Name "Enum")
      in  
        let args = (Just (Utils.pyExpressionsToPyArgs [
                Utils.pyNameToPyExpression enumName]))
        in  
          let pyName = (Names.encodeName False Util.CaseConventionPascal env name)
          in (Flows.pure [
            Utils.pyClassDefinitionToPyStatement (Syntax.ClassDefinition {
              Syntax.classDefinitionDecorators = Nothing,
              Syntax.classDefinitionName = pyName,
              Syntax.classDefinitionTypeParams = [],
              Syntax.classDefinitionArguments = args,
              Syntax.classDefinitionBody = body})]))) (Flows.bind (Flows.mapList (encodeUnionField env name) tfields) (\fieldStmts ->  
    let tparams = (environmentTypeParameters env)
    in  
      let unionAlts = (Lists.map (encodeUnionFieldAlt env name) tfields)
      in  
        let unionStmts = (unionTypeStatementsFor env (Names.encodeName False Util.CaseConventionPascal env name) tparams comment (Utils.orExpression unionAlts))
        in (Flows.pure (Lists.concat2 fieldStmts unionStmts)))))

-- | Encode a union field as a primary expression for | alternatives
encodeUnionFieldAlt :: (Helpers.PythonEnvironment -> Core.Name -> Core.FieldType -> Syntax.Primary)
encodeUnionFieldAlt env unionName fieldType =  
  let fname = (Core.fieldTypeName fieldType)
  in  
    let ftype = (Core.fieldTypeType fieldType)
    in  
      let tparamNames = (findTypeParams env ftype)
      in  
        let tparams = (Lists.map Names.encodeTypeVariable tparamNames)
        in  
          let namePrim = (Utils.pyNameToPyPrimary (Names.variantName False env unionName fname))
          in (Logic.ifElse (Lists.null tparams) namePrim ( 
            let tparamExprs = (Lists.map Utils.pyNameToPyExpression tparams)
            in (Utils.primaryWithExpressionSlices namePrim tparamExprs)))

-- | Encode a simple type alias definition
encodeTypeDefSingle :: (Helpers.PythonEnvironment -> Core.Name -> Maybe String -> Syntax.Expression -> [Syntax.Statement])
encodeTypeDefSingle env name comment typeExpr =  
  let pyName = (Names.encodeName False Util.CaseConventionPascal env name)
  in  
    let tparams = (environmentTypeParameters env)
    in [
      typeAliasStatementFor env pyName tparams comment typeExpr]

-- | Encode a type definition, dispatching based on type structure
encodeTypeAssignment :: (Helpers.PythonEnvironment -> Core.Name -> Core.Type -> Maybe String -> Compute.Flow Helpers.PyGraph [[Syntax.Statement]])
encodeTypeAssignment env name typ comment = (Flows.bind (encodeTypeAssignmentInner env name typ comment) (\defStmts ->  
  let constStmts = (encodeNameConstants env name typ)
  in (Flows.pure (Lists.concat2 (Lists.map (\s -> [
    s]) defStmts) [
    constStmts]))))

-- | Encode the inner type definition, unwrapping forall types
encodeTypeAssignmentInner :: (Helpers.PythonEnvironment -> Core.Name -> Core.Type -> Maybe String -> Compute.Flow Helpers.PyGraph [Syntax.Statement])
encodeTypeAssignmentInner env name typ comment =  
  let stripped = (Rewriting.deannotateType typ)
  in  
    let dflt = (Flows.bind (encodeType env typ) (\typeExpr -> Flows.pure (encodeTypeDefSingle env name comment typeExpr)))
    in ((\x -> case x of
      Core.TypeForall v1 ->  
        let tvar = (Core.forallTypeParameter v1)
        in  
          let body = (Core.forallTypeBody v1)
          in  
            let newEnv = (extendEnvWithTypeVar env tvar)
            in (encodeTypeAssignmentInner newEnv name body comment)
      Core.TypeRecord v1 -> (Flows.map (\s -> [
        s]) (encodeRecordType env name v1 comment))
      Core.TypeUnion v1 -> (encodeUnionType env name v1 comment)
      Core.TypeWrap v1 ->  
        let innerType = (Core.wrappedTypeBody v1)
        in (Flows.map (\s -> [
          s]) (encodeWrappedType env name innerType comment))
      _ -> dflt) stripped)

-- | Create an expression that calls hydra.dsl.python.unsupported(message) at runtime
unsupportedExpression :: (String -> Syntax.Expression)
unsupportedExpression msg = (Utils.functionCall (Utils.pyExpressionToPyPrimary (Utils.projectFromExpression (Utils.projectFromExpression (Utils.projectFromExpression (Syntax.ExpressionSimple (Syntax.Disjunction [
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
  Utils.stringToPyExpression Syntax.QuoteStyleDouble msg])

-- | Create an uncurried lambda with multiple parameters
makeUncurriedLambda :: ([Syntax.Name] -> Syntax.Expression -> Syntax.Expression)
makeUncurriedLambda params body = (Syntax.ExpressionLambda (Syntax.Lambda {
  Syntax.lambdaParams = Syntax.LambdaParameters {
    Syntax.lambdaParametersSlashNoDefault = Nothing,
    Syntax.lambdaParametersParamNoDefault = (Lists.map (\p -> Syntax.LambdaParamNoDefault p) params),
    Syntax.lambdaParametersParamWithDefault = [],
    Syntax.lambdaParametersStarEtc = Nothing},
  Syntax.lambdaBody = body}))

encodeField :: (Helpers.PythonEnvironment -> Core.Field -> (Core.Term -> Compute.Flow t0 t1) -> Compute.Flow t0 (Syntax.Name, t1))
encodeField env field encodeTerm =  
  let fname = (Core.fieldName field)
  in  
    let fterm = (Core.fieldTerm field)
    in (Flows.bind (encodeTerm fterm) (\pterm -> Flows.pure (Names.encodeFieldName env fname, pterm)))

-- | Extract CaseStatement from a case elimination term
extractCaseElimination :: (Core.Term -> Maybe Core.CaseStatement)
extractCaseElimination term = ((\x -> case x of
  Core.TermFunction v1 -> ((\x -> case x of
    Core.FunctionElimination v2 -> ((\x -> case x of
      Core.EliminationUnion v3 -> (Just v3)
      _ -> Nothing) v2)
    _ -> Nothing) v1)
  _ -> Nothing) (Rewriting.deannotateAndDetypeTerm term))

encodeBindingsAsDefs :: (t0 -> (t0 -> t1 -> Compute.Flow t2 t3) -> [t1] -> Compute.Flow t2 [t3])
encodeBindingsAsDefs env encodeBinding bindings = (Flows.mapList (encodeBinding env) bindings)

-- | Encode a binding as a Python statement (function definition or assignment)
encodeBindingAs :: (Helpers.PythonEnvironment -> Core.Binding -> Compute.Flow Helpers.PyGraph Syntax.Statement)
encodeBindingAs env binding =  
  let name1 = (Core.bindingName binding)
  in  
    let term1 = (Core.bindingTerm binding)
    in  
      let mts = (Core.bindingType binding)
      in  
        let fname = (Names.encodeName True Util.CaseConventionLowerSnake env name1)
        in (Maybes.maybe ( 
          let gathered = (gatherLambdas term1)
          in  
            let lambdaParams = (Pairs.first gathered)
            in  
              let innerBody = (Pairs.second gathered)
              in  
                let mcsa = (isCaseStatementApplication innerBody)
                in (Maybes.maybe ( 
                  let mcs = (extractCaseElimination term1)
                  in (Maybes.maybe (Flows.map (\stmts -> Lists.head stmts) (encodeTermMultiline env term1)) (\cs ->  
                    let tname = (Core.caseStatementTypeName cs)
                    in  
                      let dflt = (Core.caseStatementDefault cs)
                      in  
                        let cases_ = (Core.caseStatementCases cs)
                        in (Flows.bind (inGraphContext (Schemas.requireUnionType tname)) (\rt ->  
                          let isEnum = (Schemas.isEnumRowType rt)
                          in  
                            let isFull = (isCasesFull rt cases_)
                            in  
                              let innerParam = Syntax.Param {
                                      Syntax.paramName = (Syntax.Name "x"),
                                      Syntax.paramAnnotation = Nothing}
                              in  
                                let param = Syntax.ParamNoDefault {
                                        Syntax.paramNoDefaultParam = innerParam,
                                        Syntax.paramNoDefaultTypeComment = Nothing}
                                in  
                                  let params = (Syntax.ParametersParamNoDefault (Syntax.ParamNoDefaultParameters {
                                          Syntax.paramNoDefaultParametersParamNoDefault = [
                                            param],
                                          Syntax.paramNoDefaultParametersParamWithDefault = [],
                                          Syntax.paramNoDefaultParametersStarEtc = Nothing}))
                                  in (Flows.bind (Flows.mapList (encodeCaseBlock env tname rt isEnum (\e -> \t -> encodeTermMultiline e t)) cases_) (\pyCases -> Flows.bind (encodeDefaultCaseBlock (\t -> encodeTermInline env False t) isFull dflt tname) (\pyDflt ->  
                                    let subj = (Syntax.SubjectExpressionSimple (Syntax.NamedExpressionSimple (Utils.pyNameToPyExpression (Syntax.Name "x"))))
                                    in  
                                      let allCases = (Lists.concat2 pyCases pyDflt)
                                      in  
                                        let matchStmt = (Syntax.StatementCompound (Syntax.CompoundStatementMatch (Syntax.MatchStatement {
                                                Syntax.matchStatementSubject = subj,
                                                Syntax.matchStatementCases = allCases})))
                                        in  
                                          let body = (Utils.indentedBlock Nothing [
                                                  [
                                                    matchStmt]])
                                          in  
                                            let funcDefRaw = Syntax.FunctionDefRaw {
                                                    Syntax.functionDefRawAsync = False,
                                                    Syntax.functionDefRawName = fname,
                                                    Syntax.functionDefRawTypeParams = [],
                                                    Syntax.functionDefRawParams = (Just params),
                                                    Syntax.functionDefRawReturnType = Nothing,
                                                    Syntax.functionDefRawFuncTypeComment = Nothing,
                                                    Syntax.functionDefRawBlock = body}
                                            in (Flows.pure (Syntax.StatementCompound (Syntax.CompoundStatementFunction (Syntax.FunctionDefinition {
                                              Syntax.functionDefinitionDecorators = Nothing,
                                              Syntax.functionDefinitionRaw = funcDefRaw})))))))))) mcs)) (\csa -> Logic.ifElse (Lists.null lambdaParams) ( 
                  let mcs = (extractCaseElimination term1)
                  in (Maybes.maybe (Flows.map (\stmts -> Lists.head stmts) (encodeTermMultiline env term1)) (\cs ->  
                    let tname = (Core.caseStatementTypeName cs)
                    in  
                      let dflt = (Core.caseStatementDefault cs)
                      in  
                        let cases_ = (Core.caseStatementCases cs)
                        in (Flows.bind (inGraphContext (Schemas.requireUnionType tname)) (\rt ->  
                          let isEnum = (Schemas.isEnumRowType rt)
                          in  
                            let isFull = (isCasesFull rt cases_)
                            in  
                              let innerParam = Syntax.Param {
                                      Syntax.paramName = (Syntax.Name "x"),
                                      Syntax.paramAnnotation = Nothing}
                              in  
                                let param = Syntax.ParamNoDefault {
                                        Syntax.paramNoDefaultParam = innerParam,
                                        Syntax.paramNoDefaultTypeComment = Nothing}
                                in  
                                  let params = (Syntax.ParametersParamNoDefault (Syntax.ParamNoDefaultParameters {
                                          Syntax.paramNoDefaultParametersParamNoDefault = [
                                            param],
                                          Syntax.paramNoDefaultParametersParamWithDefault = [],
                                          Syntax.paramNoDefaultParametersStarEtc = Nothing}))
                                  in (Flows.bind (Flows.mapList (encodeCaseBlock env tname rt isEnum (\e -> \t -> encodeTermMultiline e t)) cases_) (\pyCases -> Flows.bind (encodeDefaultCaseBlock (\t -> encodeTermInline env False t) isFull dflt tname) (\pyDflt ->  
                                    let subj = (Syntax.SubjectExpressionSimple (Syntax.NamedExpressionSimple (Utils.pyNameToPyExpression (Syntax.Name "x"))))
                                    in  
                                      let allCases = (Lists.concat2 pyCases pyDflt)
                                      in  
                                        let matchStmt = (Syntax.StatementCompound (Syntax.CompoundStatementMatch (Syntax.MatchStatement {
                                                Syntax.matchStatementSubject = subj,
                                                Syntax.matchStatementCases = allCases})))
                                        in  
                                          let body = (Utils.indentedBlock Nothing [
                                                  [
                                                    matchStmt]])
                                          in  
                                            let funcDefRaw = Syntax.FunctionDefRaw {
                                                    Syntax.functionDefRawAsync = False,
                                                    Syntax.functionDefRawName = fname,
                                                    Syntax.functionDefRawTypeParams = [],
                                                    Syntax.functionDefRawParams = (Just params),
                                                    Syntax.functionDefRawReturnType = Nothing,
                                                    Syntax.functionDefRawFuncTypeComment = Nothing,
                                                    Syntax.functionDefRawBlock = body}
                                            in (Flows.pure (Syntax.StatementCompound (Syntax.CompoundStatementFunction (Syntax.FunctionDefinition {
                                              Syntax.functionDefinitionDecorators = Nothing,
                                              Syntax.functionDefinitionRaw = funcDefRaw})))))))))) mcs)) ( 
                  let tname = (Pairs.first csa)
                  in  
                    let rest1 = (Pairs.second csa)
                    in  
                      let dflt = (Pairs.first rest1)
                      in  
                        let rest2 = (Pairs.second rest1)
                        in  
                          let cases_ = (Pairs.first rest2)
                          in (Flows.bind (inGraphContext (Schemas.requireUnionType tname)) (\rt ->  
                            let isEnum = (Schemas.isEnumRowType rt)
                            in  
                              let isFull = (isCasesFull rt cases_)
                              in  
                                let capturedParams = (Lists.map (\n -> Syntax.ParamNoDefault {
                                        Syntax.paramNoDefaultParam = Syntax.Param {
                                          Syntax.paramName = (Names.encodeName False Util.CaseConventionLowerSnake env n),
                                          Syntax.paramAnnotation = Nothing},
                                        Syntax.paramNoDefaultTypeComment = Nothing}) lambdaParams)
                                in  
                                  let matchArgName = (Syntax.Name "x")
                                  in  
                                    let matchParam = Syntax.ParamNoDefault {
                                            Syntax.paramNoDefaultParam = Syntax.Param {
                                              Syntax.paramName = matchArgName,
                                              Syntax.paramAnnotation = Nothing},
                                            Syntax.paramNoDefaultTypeComment = Nothing}
                                    in  
                                      let allParams = (Lists.concat2 capturedParams [
                                              matchParam])
                                      in  
                                        let params = (Syntax.ParametersParamNoDefault (Syntax.ParamNoDefaultParameters {
                                                Syntax.paramNoDefaultParametersParamNoDefault = allParams,
                                                Syntax.paramNoDefaultParametersParamWithDefault = [],
                                                Syntax.paramNoDefaultParametersStarEtc = Nothing}))
                                        in  
                                          let envWithParams = (extendEnvWithLambdaParams env term1)
                                          in (Flows.bind (Flows.mapList (encodeCaseBlock envWithParams tname rt isEnum (\e -> \t -> encodeTermMultiline e t)) cases_) (\pyCases -> Flows.bind (encodeDefaultCaseBlock (\t -> encodeTermInline envWithParams False t) isFull dflt tname) (\pyDflt ->  
                                            let subj = (Syntax.SubjectExpressionSimple (Syntax.NamedExpressionSimple (Utils.pyNameToPyExpression matchArgName)))
                                            in  
                                              let allCases = (Lists.concat2 pyCases pyDflt)
                                              in  
                                                let matchStmt = (Syntax.StatementCompound (Syntax.CompoundStatementMatch (Syntax.MatchStatement {
                                                        Syntax.matchStatementSubject = subj,
                                                        Syntax.matchStatementCases = allCases})))
                                                in  
                                                  let body = (Utils.indentedBlock Nothing [
                                                          [
                                                            matchStmt]])
                                                  in  
                                                    let funcDefRaw = Syntax.FunctionDefRaw {
                                                            Syntax.functionDefRawAsync = False,
                                                            Syntax.functionDefRawName = fname,
                                                            Syntax.functionDefRawTypeParams = [],
                                                            Syntax.functionDefRawParams = (Just params),
                                                            Syntax.functionDefRawReturnType = Nothing,
                                                            Syntax.functionDefRawFuncTypeComment = Nothing,
                                                            Syntax.functionDefRawBlock = body}
                                                    in (Flows.pure (Syntax.StatementCompound (Syntax.CompoundStatementFunction (Syntax.FunctionDefinition {
                                                      Syntax.functionDefinitionDecorators = Nothing,
                                                      Syntax.functionDefinitionRaw = funcDefRaw}))))))))))) mcsa)) (\ts -> Flows.bind (inGraphContext (Annotations.getTermDescription term1)) (\comment ->  
          let normComment = (Maybes.map CoderUtils.normalizeComment comment)
          in (encodeTermAssignment env name1 term1 ts normComment))) mts)

-- | Encode a definition (term or type) to Python statements
encodeDefinition :: (Helpers.PythonEnvironment -> Module.Definition -> Compute.Flow Helpers.PyGraph [[Syntax.Statement]])
encodeDefinition env def_ = ((\x -> case x of
  Module.DefinitionTerm v1 ->  
    let name = (Module.termDefinitionName v1)
    in  
      let term = (Module.termDefinitionTerm v1)
      in  
        let typ = (Module.termDefinitionType v1)
        in (Flows.bind (inGraphContext (Annotations.getTermDescription term)) (\comment ->  
          let normComment = (Maybes.map CoderUtils.normalizeComment comment)
          in (Flows.bind (encodeTermAssignment env name term typ normComment) (\stmt -> Flows.pure [
            [
              stmt]]))))
  Module.DefinitionType v1 ->  
    let name = (Module.typeDefinitionName v1)
    in  
      let typ = (Module.typeDefinitionType v1)
      in (Flows.bind (inGraphContext (Annotations.getTypeDescription typ)) (\comment ->  
        let normComment = (Maybes.map CoderUtils.normalizeComment comment)
        in (encodeTypeAssignment env name typ normComment)))) def_)

-- | Update the Python module metadata in the coder state
updateMeta :: ((Helpers.PythonModuleMetadata -> Helpers.PythonModuleMetadata) -> Compute.Flow Helpers.PyGraph ())
updateMeta = (CoderUtils.updateCoderMetadata pyGraphMetadata makePyGraph pyGraphGraph)

withBindings :: ([Core.Binding] -> Compute.Flow Helpers.PyGraph t0 -> Compute.Flow Helpers.PyGraph t0)
withBindings = (CoderUtils.withGraphBindings pyGraphGraph makePyGraph pyGraphMetadata)

withUpdatedGraph :: ((Graph.Graph -> Graph.Graph) -> Compute.Flow Helpers.PyGraph t0 -> Compute.Flow Helpers.PyGraph t0)
withUpdatedGraph = (CoderUtils.withUpdatedCoderGraph pyGraphGraph pyGraphMetadata makePyGraph)

-- | Calculate term arity with proper primitive handling
termArityWithPrimitives :: (Graph.Graph -> Core.Term -> Int)
termArityWithPrimitives graph term = ((\x -> case x of
  Core.TermApplication v1 -> (Math.max 0 (Math.sub (termArityWithPrimitives graph (Core.applicationFunction v1)) 1))
  Core.TermFunction v1 -> (functionArityWithPrimitives graph v1)
  _ -> 0) (Rewriting.deannotateAndDetypeTerm term))

-- | Calculate function arity with proper primitive handling
functionArityWithPrimitives :: (Graph.Graph -> Core.Function -> Int)
functionArityWithPrimitives graph f = ((\x -> case x of
  Core.FunctionElimination _ -> 1
  Core.FunctionLambda v1 -> (Math.add 1 (termArityWithPrimitives graph (Core.lambdaBody v1)))
  Core.FunctionPrimitive v1 -> (Maybes.maybe 0 (\prim -> Arity.primitiveArity prim) (Maps.lookup v1 (Graph.graphPrimitives graph)))
  _ -> 0) f)

-- | Get the TypeContext from a PythonEnvironment
pythonEnvironmentGetTypeContext :: (Helpers.PythonEnvironment -> Typing.TypeContext)
pythonEnvironmentGetTypeContext = Helpers.pythonEnvironmentTypeContext

-- | Set the TypeContext in a PythonEnvironment
pythonEnvironmentSetTypeContext :: (Typing.TypeContext -> Helpers.PythonEnvironment -> Helpers.PythonEnvironment)
pythonEnvironmentSetTypeContext tc env = Helpers.PythonEnvironment {
  Helpers.pythonEnvironmentNamespaces = (Helpers.pythonEnvironmentNamespaces env),
  Helpers.pythonEnvironmentBoundTypeVariables = (Helpers.pythonEnvironmentBoundTypeVariables env),
  Helpers.pythonEnvironmentTypeContext = tc,
  Helpers.pythonEnvironmentNullaryBindings = (Helpers.pythonEnvironmentNullaryBindings env),
  Helpers.pythonEnvironmentVersion = (Helpers.pythonEnvironmentVersion env),
  Helpers.pythonEnvironmentSkipCasts = (Helpers.pythonEnvironmentSkipCasts env),
  Helpers.pythonEnvironmentInlineVariables = (Helpers.pythonEnvironmentInlineVariables env)}

withLambda :: (Helpers.PythonEnvironment -> Core.Lambda -> (Helpers.PythonEnvironment -> t0) -> t0)
withLambda = (Schemas.withLambdaContext pythonEnvironmentGetTypeContext pythonEnvironmentSetTypeContext)

withTypeLambda :: (Helpers.PythonEnvironment -> Core.TypeLambda -> (Helpers.PythonEnvironment -> t0) -> t0)
withTypeLambda = (Schemas.withTypeLambdaContext pythonEnvironmentGetTypeContext pythonEnvironmentSetTypeContext)

withLet :: (Helpers.PythonEnvironment -> Core.Let -> (Helpers.PythonEnvironment -> t0) -> t0)
withLet = (Schemas.withLetContext pythonEnvironmentGetTypeContext pythonEnvironmentSetTypeContext CoderUtils.bindingMetadata)

withLetInline :: (Helpers.PythonEnvironment -> Core.Let -> (Helpers.PythonEnvironment -> t0) -> t0)
withLetInline env lt body =  
  let bindingNames = (Lists.map (\b -> Core.bindingName b) (Core.letBindings lt))
  in  
    let inlineVars = (Sets.fromList bindingNames)
    in  
      let noMetadata = (\tc -> \b -> Nothing)
      in (Schemas.withLetContext pythonEnvironmentGetTypeContext pythonEnvironmentSetTypeContext noMetadata env lt (\innerEnv ->  
        let updatedEnv = Helpers.PythonEnvironment {
                Helpers.pythonEnvironmentNamespaces = (Helpers.pythonEnvironmentNamespaces innerEnv),
                Helpers.pythonEnvironmentBoundTypeVariables = (Helpers.pythonEnvironmentBoundTypeVariables innerEnv),
                Helpers.pythonEnvironmentTypeContext = (Helpers.pythonEnvironmentTypeContext innerEnv),
                Helpers.pythonEnvironmentNullaryBindings = (Helpers.pythonEnvironmentNullaryBindings innerEnv),
                Helpers.pythonEnvironmentVersion = (Helpers.pythonEnvironmentVersion innerEnv),
                Helpers.pythonEnvironmentSkipCasts = (Helpers.pythonEnvironmentSkipCasts innerEnv),
                Helpers.pythonEnvironmentInlineVariables = (Sets.union inlineVars (Helpers.pythonEnvironmentInlineVariables innerEnv))}
        in (body updatedEnv)))

-- | Create initial empty metadata for a Python module
initialMetadata :: (Module.Namespace -> Helpers.PythonModuleMetadata)
initialMetadata ns =  
  let dottedNs = (Names.encodeNamespace ns)
  in  
    let emptyNs = Module.Namespaces {
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
initialEnvironment :: (Module.Namespaces Syntax.DottedName -> Typing.TypeContext -> Helpers.PythonEnvironment)
initialEnvironment namespaces tcontext = Helpers.PythonEnvironment {
  Helpers.pythonEnvironmentNamespaces = namespaces,
  Helpers.pythonEnvironmentBoundTypeVariables = ([], Maps.empty),
  Helpers.pythonEnvironmentTypeContext = tcontext,
  Helpers.pythonEnvironmentNullaryBindings = Sets.empty,
  Helpers.pythonEnvironmentVersion = targetPythonVersion,
  Helpers.pythonEnvironmentSkipCasts = True,
  Helpers.pythonEnvironmentInlineVariables = Sets.empty}

-- | The target Python version for code generation
targetPythonVersion :: Helpers.PythonVersion
targetPythonVersion = Utils.targetPythonVersion

analyzePythonFunction :: (Helpers.PythonEnvironment -> Core.Term -> Compute.Flow t0 (Typing.FunctionStructure Helpers.PythonEnvironment))
analyzePythonFunction = (CoderUtils.analyzeFunctionTerm pythonEnvironmentGetTypeContext pythonEnvironmentSetTypeContext)

analyzePythonFunctionInline :: (Helpers.PythonEnvironment -> Core.Term -> Compute.Flow t0 (Typing.FunctionStructure Helpers.PythonEnvironment))
analyzePythonFunctionInline = (CoderUtils.analyzeFunctionTermInline pythonEnvironmentGetTypeContext pythonEnvironmentSetTypeContext)

withDefinitions :: (Helpers.PythonEnvironment -> [Module.Definition] -> (Helpers.PythonEnvironment -> t0) -> t0)
withDefinitions env defs body =  
  let bindings = (Maybes.cat (Lists.map (\def_ -> (\x -> case x of
          Module.DefinitionTerm v1 -> (Just (Core.Binding {
            Core.bindingName = (Module.termDefinitionName v1),
            Core.bindingTerm = (Module.termDefinitionTerm v1),
            Core.bindingType = (Just (Module.termDefinitionType v1))}))
          Module.DefinitionType _ -> Nothing
          _ -> Nothing) def_) defs))
  in  
    let dummyLet = Core.Let {
            Core.letBindings = bindings,
            Core.letBody = (Core.TermLiteral (Core.LiteralString "dummy"))}
    in (withLet env dummyLet body)

-- | Encode a binding as a walrus operator assignment
encodeBindingAsAssignment :: (Bool -> Helpers.PythonEnvironment -> Core.Binding -> Compute.Flow Helpers.PyGraph Syntax.NamedExpression)
encodeBindingAsAssignment allowThunking env binding =  
  let name = (Core.bindingName binding)
  in  
    let term = (Core.bindingTerm binding)
    in  
      let mts = (Core.bindingType binding)
      in  
        let pyName = (Names.encodeName False Util.CaseConventionLowerSnake env name)
        in (Flows.bind (encodeTermInline env False term) (\pbody ->  
          let tc = (Helpers.pythonEnvironmentTypeContext env)
          in  
            let isComplexVar = (CoderUtils.isComplexVariable tc name)
            in  
              let termIsComplex = (CoderUtils.isComplexTerm tc term)
              in  
                let needsThunk = (Maybes.maybe (Logic.and allowThunking (Logic.or isComplexVar termIsComplex)) (\ts -> Logic.and allowThunking (Logic.and (Equality.equal (Arity.typeSchemeArity ts) 0) (Logic.or isComplexVar termIsComplex))) mts)
                in  
                  let pterm = (Logic.ifElse needsThunk (makeThunk pbody) pbody)
                  in (Flows.pure (Syntax.NamedExpressionAssignment (Syntax.AssignmentExpression {
                    Syntax.assignmentExpressionName = pyName,
                    Syntax.assignmentExpressionExpression = pterm})))))

-- | Encode a function definition with parameters and body
encodeFunctionDefinition :: (Helpers.PythonEnvironment -> Core.Name -> [Core.Name] -> [Core.Name] -> Core.Term -> [Core.Type] -> Maybe Core.Type -> Maybe String -> [Syntax.Statement] -> Compute.Flow Helpers.PyGraph Syntax.Statement)
encodeFunctionDefinition env name tparams args body doms mcod comment prefixes = (Flows.bind (Flows.mapList (\pair ->  
  let argName = (Pairs.first pair)
  in  
    let typ = (Pairs.second pair)
    in (Flows.bind (encodeType env typ) (\pyTyp -> Flows.pure (Syntax.ParamNoDefault {
      Syntax.paramNoDefaultParam = Syntax.Param {
        Syntax.paramName = (Names.encodeName False Util.CaseConventionLowerSnake env argName),
        Syntax.paramAnnotation = (Just (Syntax.Annotation pyTyp))},
      Syntax.paramNoDefaultTypeComment = Nothing})))) (Lists.zip args doms)) (\pyArgs ->  
  let pyParams = (Syntax.ParametersParamNoDefault (Syntax.ParamNoDefaultParameters {
          Syntax.paramNoDefaultParametersParamNoDefault = pyArgs,
          Syntax.paramNoDefaultParametersParamWithDefault = [],
          Syntax.paramNoDefaultParametersStarEtc = Nothing}))
  in (Flows.bind (encodeTermMultiline env body) (\stmts ->  
    let block = (Utils.indentedBlock comment [
            Lists.concat2 prefixes stmts])
    in (Flows.bind (Maybes.maybe (Flows.pure Nothing) (\cod -> Flows.bind (encodeType env cod) (\pytyp -> Flows.pure (Just pytyp))) mcod) (\mreturnType ->  
      let pyTparams = (Logic.ifElse useInlineTypeParams (Lists.map (\arg_ -> Utils.pyNameToPyTypeParameter (Names.encodeTypeVariable arg_)) tparams) [])
      in  
        let isThunk = (Lists.null args)
        in  
          let mDecorators = (Logic.ifElse isThunk (Just (Syntax.Decorators [
                  lruCacheDecorator])) Nothing)
          in (Flows.bind (Logic.ifElse isThunk (updateMeta (setMetaUsesLruCache True)) (Flows.pure ())) (\unit1 ->  
            let pyName = (Names.encodeName False Util.CaseConventionLowerSnake env name)
            in (Flows.pure (Syntax.StatementCompound (Syntax.CompoundStatementFunction (Syntax.FunctionDefinition {
              Syntax.functionDefinitionDecorators = mDecorators,
              Syntax.functionDefinitionRaw = Syntax.FunctionDefRaw {
                Syntax.functionDefRawAsync = False,
                Syntax.functionDefRawName = pyName,
                Syntax.functionDefRawTypeParams = pyTparams,
                Syntax.functionDefRawParams = (Just pyParams),
                Syntax.functionDefRawReturnType = mreturnType,
                Syntax.functionDefRawFuncTypeComment = Nothing,
                Syntax.functionDefRawBlock = block}}))))))))))))

-- | Encode a term to a list of statements with return as final statement
encodeTermMultiline :: (Helpers.PythonEnvironment -> Core.Term -> Compute.Flow Helpers.PyGraph [Syntax.Statement])
encodeTermMultiline env term =  
  let dfltLogic = (Flows.bind (analyzePythonFunction env term) (\fs ->  
          let params = (Typing.functionStructureParams fs)
          in  
            let bindings = (Typing.functionStructureBindings fs)
            in  
              let innerBody = (Typing.functionStructureBody fs)
              in  
                let env2 = (Typing.functionStructureEnvironment fs)
                in (Logic.ifElse (Logic.not (Lists.null params)) (Flows.fail "Functions currently unsupported in this context") (Logic.ifElse (Lists.null bindings) (Flows.bind (encodeTermInline env False term) (\expr -> Flows.pure [
                  Utils.returnSingle expr])) (withBindings bindings (Flows.bind (Flows.mapList (encodeBindingAs env2) bindings) (\bindingStmts -> Flows.bind (encodeTermMultiline env2 innerBody) (\bodyStmts -> Flows.pure (Lists.concat2 bindingStmts bodyStmts)))))))))
  in  
    let gathered = (CoderUtils.gatherApplications term)
    in  
      let args = (Pairs.first gathered)
      in  
        let body = (Pairs.second gathered)
        in (Logic.ifElse (Equality.equal (Lists.length args) 1) ( 
          let arg = (Lists.head args)
          in ((\x -> case x of
            Core.TermFunction v1 -> ((\x -> case x of
              Core.FunctionElimination v2 -> ((\x -> case x of
                Core.EliminationUnion v3 ->  
                  let tname = (Core.caseStatementTypeName v3)
                  in  
                    let dflt = (Core.caseStatementDefault v3)
                    in  
                      let cases_ = (Core.caseStatementCases v3)
                      in (Flows.bind (inGraphContext (Schemas.requireUnionType tname)) (\rt ->  
                        let isEnum = (Schemas.isEnumRowType rt)
                        in  
                          let isFull = (isCasesFull rt cases_)
                          in (Flows.bind (encodeTermInline env False arg) (\pyArg -> Flows.bind (Flows.mapList (encodeCaseBlock env tname rt isEnum (\e -> \t -> encodeTermMultiline e t)) (deduplicateCaseVariables cases_)) (\pyCases -> Flows.bind (encodeDefaultCaseBlock (\t -> encodeTermInline env False t) isFull dflt tname) (\pyDflt ->  
                            let subj = (Syntax.SubjectExpressionSimple (Syntax.NamedExpressionSimple pyArg))
                            in  
                              let matchStmt = (Syntax.StatementCompound (Syntax.CompoundStatementMatch (Syntax.MatchStatement {
                                      Syntax.matchStatementSubject = subj,
                                      Syntax.matchStatementCases = (Lists.concat2 pyCases pyDflt)})))
                              in (Flows.pure [
                                matchStmt])))))))
                _ -> dfltLogic) v2)
              _ -> dfltLogic) v1)
            _ -> dfltLogic) (Rewriting.deannotateAndDetypeTerm body))) dfltLogic)

-- | Encode a function term to a Python expression
encodeFunction :: (Helpers.PythonEnvironment -> Core.Function -> Compute.Flow Helpers.PyGraph Syntax.Expression)
encodeFunction env f = ((\x -> case x of
  Core.FunctionLambda v1 -> (Flows.bind (analyzePythonFunctionInline env (Core.TermFunction (Core.FunctionLambda v1))) (\fs ->  
    let params = (Typing.functionStructureParams fs)
    in  
      let bindings = (Typing.functionStructureBindings fs)
      in  
        let innerBody = (Typing.functionStructureBody fs)
        in  
          let innerEnv = (Typing.functionStructureEnvironment fs)
          in (Flows.bind (encodeTermInline innerEnv False innerBody) (\pbody ->  
            let pparams = (Lists.map (Names.encodeName False Util.CaseConventionLowerSnake innerEnv) params)
            in (Logic.ifElse (Lists.null bindings) (Flows.pure (makeUncurriedLambda pparams pbody)) (Flows.bind (Flows.mapList (encodeBindingAsAssignment False innerEnv) bindings) (\pbindingExprs ->  
              let pbindingStarExprs = (Lists.map (\ne -> Syntax.StarNamedExpressionSimple ne) pbindingExprs)
              in  
                let pbodyStarExpr = (Utils.pyExpressionToPyStarNamedExpression pbody)
                in  
                  let tupleElements = (Lists.concat2 pbindingStarExprs [
                          pbodyStarExpr])
                  in  
                    let tupleExpr = (Utils.pyAtomToPyExpression (Syntax.AtomTuple (Syntax.Tuple tupleElements)))
                    in  
                      let indexValue = (Utils.pyAtomToPyExpression (Syntax.AtomNumber (Syntax.NumberInteger (Literals.int32ToBigint (Lists.length bindings)))))
                      in  
                        let indexedExpr = (Utils.primaryWithExpressionSlices (Utils.pyExpressionToPyPrimary tupleExpr) [
                                indexValue])
                        in (Flows.pure (makeUncurriedLambda pparams (Utils.pyPrimaryToPyExpression indexedExpr))))))))))
  Core.FunctionPrimitive v1 -> (encodeVariable env v1 [])
  Core.FunctionElimination v1 -> ((\x -> case x of
    Core.EliminationRecord v2 ->  
      let fname = (Core.projectionField v2)
      in (Flows.pure (makeCurriedLambda [
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
    Core.EliminationWrap _ -> (Flows.pure (makeCurriedLambda [
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
          Syntax.comparisonRhs = []})]])) (Syntax.Name "value"))))
    Core.EliminationUnion _ -> (Flows.pure (unsupportedExpression "case expressions as values are not yet supported"))) v1)) f)

-- | Encode a term assignment to a Python statement
encodeTermAssignment :: (Helpers.PythonEnvironment -> Core.Name -> Core.Term -> Core.TypeScheme -> Maybe String -> Compute.Flow Helpers.PyGraph Syntax.Statement)
encodeTermAssignment env name term ts comment = (Flows.bind (analyzePythonFunction env term) (\fs ->  
  let tparams = (Typing.functionStructureTypeParams fs)
  in  
    let params = (Typing.functionStructureParams fs)
    in  
      let bindings = (Typing.functionStructureBindings fs)
      in  
        let body = (Typing.functionStructureBody fs)
        in  
          let doms = (Typing.functionStructureDomains fs)
          in  
            let mcod = (Typing.functionStructureCodomain fs)
            in  
              let env2 = (Typing.functionStructureEnvironment fs)
              in  
                let tc = (Helpers.pythonEnvironmentTypeContext env2)
                in  
                  let binding = Core.Binding {
                          Core.bindingName = name,
                          Core.bindingTerm = term,
                          Core.bindingType = (Just ts)}
                  in  
                    let isComplex = (CoderUtils.isComplexBinding tc binding)
                    in (Logic.ifElse isComplex (withBindings bindings (Flows.bind (Flows.mapList (encodeBindingAs env2) bindings) (\bindingStmts -> encodeFunctionDefinition env2 name tparams params body doms mcod comment bindingStmts))) (Flows.bind (encodeTermInline env2 False body) (\bodyExpr ->  
                      let pyName = (Names.encodeName False Util.CaseConventionLowerSnake env2 name)
                      in (Flows.pure (Utils.annotatedStatement comment (Utils.assignmentStatement pyName bodyExpr))))))))

-- | Encode a variable reference to a Python expression
encodeVariable :: (Helpers.PythonEnvironment -> Core.Name -> [Syntax.Expression] -> Compute.Flow Helpers.PyGraph Syntax.Expression)
encodeVariable env name args = (Flows.bind Monads.getState (\pyg ->  
  let g = (pyGraphGraph pyg)
  in  
    let tc = (Helpers.pythonEnvironmentTypeContext env)
    in  
      let tcTypes = (Typing.typeContextTypes tc)
      in  
        let tcLambdaVars = (Typing.typeContextLambdaVariables tc)
        in  
          let tcMetadata = (Typing.typeContextMetadata tc)
          in  
            let inlineVars = (Helpers.pythonEnvironmentInlineVariables env)
            in  
              let mTyp = (Maps.lookup name tcTypes)
              in  
                let asVariable = (Names.termVariableReference env name)
                in  
                  let asFunctionCall = (Utils.functionCall (Utils.pyNameToPyPrimary (Names.encodeName True Util.CaseConventionLowerSnake env name)) args)
                  in (Logic.ifElse (Logic.not (Lists.null args)) (Maybes.maybe (Flows.pure asFunctionCall) (\prim ->  
                    let primArity = (Arity.primitiveArity prim)
                    in (Logic.ifElse (Equality.equal primArity (Lists.length args)) (Flows.pure asFunctionCall) ( 
                      let numRemaining = (Math.sub primArity (Lists.length args))
                      in  
                        let remainingParams = (Lists.map (\i -> Syntax.Name (Strings.cat2 "x" (Literals.showInt32 i))) (Math.range 1 numRemaining))
                        in  
                          let remainingExprs = (Lists.map (\n -> Syntax.ExpressionSimple (Syntax.Disjunction [
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
                                      Syntax.comparisonRhs = []})]])) remainingParams)
                          in  
                            let allArgs = (Lists.concat2 args remainingExprs)
                            in  
                              let fullCall = (Utils.functionCall (Utils.pyNameToPyPrimary (Names.encodeName True Util.CaseConventionLowerSnake env name)) allArgs)
                              in (Flows.pure (makeUncurriedLambda remainingParams fullCall))))) (Lexical.lookupPrimitive g name)) (Maybes.maybe (Logic.ifElse (Sets.member name tcLambdaVars) (Flows.pure asVariable) (Maybes.maybe (Maybes.maybe (Maybes.maybe (Flows.fail (Strings.cat2 "Unknown variable: " (Core.unName name))) (\_ -> Flows.pure asFunctionCall) (Maps.lookup name tcMetadata)) (\el -> Maybes.maybe (Flows.pure asVariable) (\ts -> Logic.ifElse (Logic.and (Equality.equal (Arity.typeSchemeArity ts) 0) (CoderUtils.isComplexBinding tc el)) (Flows.pure asFunctionCall) ( 
                    let asFunctionRef = (Logic.ifElse (Logic.not (Lists.null (Core.typeSchemeVariables ts))) (makeSimpleLambda (Arity.typeArity (Core.typeSchemeType ts)) asVariable) asVariable)
                    in (Flows.pure asFunctionRef))) (Core.bindingType el)) (Lexical.lookupElement g name)) (\prim ->  
                    let primArity = (Arity.primitiveArity prim)
                    in (Logic.ifElse (Equality.equal primArity 0) (Flows.pure asFunctionCall) ( 
                      let ts = (Graph.primitiveType prim)
                      in  
                        let asFunctionRef = (Logic.ifElse (Logic.not (Lists.null (Core.typeSchemeVariables ts))) (makeSimpleLambda (Arity.typeArity (Core.typeSchemeType ts)) asVariable) asVariable)
                        in (Flows.pure asFunctionRef)))) (Lexical.lookupPrimitive g name))) (\typ -> Logic.ifElse (Sets.member name tcLambdaVars) (Flows.pure asVariable) (Logic.ifElse (Sets.member name inlineVars) ( 
                    let asFunctionRef = (Logic.ifElse (Logic.not (Sets.null (Rewriting.freeVariablesInType typ))) (makeSimpleLambda (Arity.typeArity typ) asVariable) asVariable)
                    in (Flows.pure asFunctionRef)) (Logic.ifElse (Logic.not (Maps.member name tcMetadata)) (Maybes.maybe ( 
                    let asFunctionRef = (Logic.ifElse (Logic.not (Sets.null (Rewriting.freeVariablesInType typ))) (makeSimpleLambda (Arity.typeArity typ) asVariable) asVariable)
                    in (Flows.pure asFunctionRef)) (\el -> Maybes.maybe (Logic.ifElse (Equality.equal (Arity.typeArity typ) 0) (Flows.pure asFunctionCall) ( 
                    let asFunctionRef = (Logic.ifElse (Logic.not (Sets.null (Rewriting.freeVariablesInType typ))) (makeSimpleLambda (Arity.typeArity typ) asVariable) asVariable)
                    in (Flows.pure asFunctionRef))) (\ts -> Logic.ifElse (Logic.and (Equality.equal (Arity.typeArity typ) 0) (CoderUtils.isComplexBinding tc el)) (Flows.pure asFunctionCall) ( 
                    let asFunctionRef = (Logic.ifElse (Logic.not (Sets.null (Rewriting.freeVariablesInType typ))) (makeSimpleLambda (Arity.typeArity typ) asVariable) asVariable)
                    in (Flows.pure asFunctionRef))) (Core.bindingType el)) (Lexical.lookupElement g name)) (Logic.ifElse (Logic.and (Equality.equal (Arity.typeArity typ) 0) (CoderUtils.isComplexVariable tc name)) (Flows.pure asFunctionCall) ( 
                    let asFunctionRef = (Logic.ifElse (Logic.not (Sets.null (Rewriting.freeVariablesInType typ))) (makeSimpleLambda (Arity.typeArity typ) asVariable) asVariable)
                    in (Flows.pure asFunctionRef)))))) mTyp))))

-- | Encode a function application to a Python expression
encodeApplication :: (Helpers.PythonEnvironment -> Core.Application -> Compute.Flow Helpers.PyGraph Syntax.Expression)
encodeApplication env app = (Flows.bind Monads.getState (\pyg ->  
  let g = (pyGraphGraph pyg)
  in  
    let tc = (Helpers.pythonEnvironmentTypeContext env)
    in  
      let skipCasts = (Helpers.pythonEnvironmentSkipCasts env)
      in  
        let term = (Core.TermApplication app)
        in  
          let gathered = (CoderUtils.gatherArgs term [])
          in  
            let fun = (Pairs.first gathered)
            in  
              let args = (Pairs.second gathered)
              in  
                let termArity = (termArityWithPrimitives g fun)
                in (Flows.bind (Flows.withDefault termArity (Flows.map Arity.typeArity (inGraphContext (Checking.typeOf tc [] fun)))) (\arity -> Flows.bind (Flows.mapList (\t -> encodeTermInline env False t) args) (\pargs ->  
                  let hargs = (Lists.take arity pargs)
                  in  
                    let rargs = (Lists.drop arity pargs)
                    in (Flows.bind (encodeApplicationInner env fun hargs rargs) (\result ->  
                      let lhs = (Pairs.first result)
                      in  
                        let remainingRargs = (Pairs.second result)
                        in  
                          let pyapp = (Lists.foldl (\t -> \a -> Utils.functionCall (Utils.pyExpressionToPyPrimary t) [
                                  a]) lhs remainingRargs)
                          in (Flows.pure pyapp))))))))

-- | Inner helper for encodeApplication
encodeApplicationInner :: (Helpers.PythonEnvironment -> Core.Term -> [Syntax.Expression] -> [Syntax.Expression] -> Compute.Flow Helpers.PyGraph (Syntax.Expression, [Syntax.Expression]))
encodeApplicationInner env fun hargs rargs =  
  let firstArg = (Lists.head hargs)
  in  
    let restArgs = (Lists.tail hargs)
    in  
      let withRest = (\e -> Logic.ifElse (Lists.null restArgs) e (Utils.functionCall (Utils.pyExpressionToPyPrimary e) restArgs))
      in  
        let defaultCase = (Flows.bind (encodeTermInline env False fun) (\pfun -> Flows.pure (Utils.functionCall (Utils.pyExpressionToPyPrimary pfun) hargs, rargs)))
        in ((\x -> case x of
          Core.TermFunction v1 -> ((\x -> case x of
            Core.FunctionElimination v2 -> ((\x -> case x of
              Core.EliminationRecord v3 ->  
                let fname = (Core.projectionField v3)
                in  
                  let fieldExpr = (Utils.projectFromExpression firstArg (Names.encodeFieldName env fname))
                  in (Flows.pure (withRest fieldExpr, rargs))
              Core.EliminationUnion _ -> (Flows.pure (unsupportedExpression "inline match expressions are not yet supported", rargs))
              Core.EliminationWrap _ ->  
                let valueExpr = (Utils.projectFromExpression firstArg (Syntax.Name "value"))
                in  
                  let allArgs = (Lists.concat2 restArgs rargs)
                  in (Logic.ifElse (Lists.null allArgs) (Flows.pure (valueExpr, [])) (Flows.pure (Utils.functionCall (Utils.pyExpressionToPyPrimary valueExpr) allArgs, [])))
              _ -> defaultCase) v2)
            Core.FunctionPrimitive v2 ->  
              let wrappedArgs = (wrapLazyArguments v2 hargs)
              in (Flows.bind (encodeVariable env v2 wrappedArgs) (\expr -> Flows.pure (expr, rargs)))
            Core.FunctionLambda _ -> (Flows.bind (encodeTermInline env False fun) (\pfun -> Flows.pure (Utils.functionCall (Utils.pyExpressionToPyPrimary pfun) hargs, rargs)))
            _ -> defaultCase) v1)
          Core.TermVariable v1 -> (Flows.bind Monads.getState (\pyg ->  
            let g = (pyGraphGraph pyg)
            in  
              let allArgs = (Lists.concat2 hargs rargs)
              in (Maybes.maybe (Flows.bind (encodeVariable env v1 hargs) (\expr -> Flows.pure (expr, rargs))) (\el -> Maybes.maybe (Flows.bind (encodeVariable env v1 hargs) (\expr -> Flows.pure (expr, rargs))) (\ts ->  
                let elArity = (Arity.typeSchemeArity ts)
                in  
                  let consumeCount = (Math.min elArity (Lists.length allArgs))
                  in  
                    let consumedArgs = (Lists.take consumeCount allArgs)
                    in  
                      let remainingArgs = (Lists.drop consumeCount allArgs)
                      in (Logic.ifElse (Lists.null consumedArgs) (Flows.bind (encodeVariable env v1 []) (\expr -> Flows.pure (expr, rargs))) (Flows.pure (Utils.functionCall (Utils.pyNameToPyPrimary (Names.encodeName True Util.CaseConventionLowerSnake env v1)) consumedArgs, remainingArgs)))) (Core.bindingType el)) (Lexical.lookupElement g v1))))
          _ -> defaultCase) (Rewriting.deannotateTerm fun))

-- | Encode a term to a Python expression (inline form)
encodeTermInline :: (Helpers.PythonEnvironment -> Bool -> Core.Term -> Compute.Flow Helpers.PyGraph Syntax.Expression)
encodeTermInline env noCast term =  
  let encode = (\t -> encodeTermInline env False t)
  in  
    let stripTypeApps = (\t -> (\x -> case x of
            Core.TermAnnotated v1 -> (stripTypeApps (Core.annotatedTermBody v1))
            Core.TermTypeApplication v1 -> (stripTypeApps (Core.typeApplicationTermBody v1))
            _ -> t) t)
    in  
      let withCast = (\pyexp -> Logic.ifElse (Logic.or noCast (Helpers.pythonEnvironmentSkipCasts env)) (Flows.pure pyexp) ( 
              let tc = (Helpers.pythonEnvironmentTypeContext env)
              in (Flows.withDefault pyexp (Flows.bind (Checking.typeOf tc [] term) (\typ -> Flows.bind (encodeType env typ) (\pytyp -> Flows.bind (updateMeta (\m -> Helpers.PythonModuleMetadata {
                Helpers.pythonModuleMetadataNamespaces = (Helpers.pythonModuleMetadataNamespaces m),
                Helpers.pythonModuleMetadataTypeVariables = (Helpers.pythonModuleMetadataTypeVariables m),
                Helpers.pythonModuleMetadataUsesAnnotated = (Helpers.pythonModuleMetadataUsesAnnotated m),
                Helpers.pythonModuleMetadataUsesCallable = (Helpers.pythonModuleMetadataUsesCallable m),
                Helpers.pythonModuleMetadataUsesCast = True,
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
                Helpers.pythonModuleMetadataUsesTypeVar = (Helpers.pythonModuleMetadataUsesTypeVar m)})) (\unit_ -> Flows.pure (Utils.castTo pytyp pyexp))))))))
      in ((\x -> case x of
        Core.TermApplication v1 -> (encodeApplication env v1)
        Core.TermEither v1 -> (Eithers.either (\t1 -> Flows.bind (encode t1) (\pyexp -> withCast (Utils.functionCall (Utils.pyNameToPyPrimary (Syntax.Name "Left")) [
          pyexp]))) (\t1 -> Flows.bind (encode t1) (\pyexp -> withCast (Utils.functionCall (Utils.pyNameToPyPrimary (Syntax.Name "Right")) [
          pyexp]))) v1)
        Core.TermFunction v1 -> (encodeFunction env v1)
        Core.TermLet v1 ->  
          let bindings = (Core.letBindings v1)
          in  
            let body = (Core.letBody v1)
            in (Logic.ifElse (Lists.null bindings) (encodeTermInline env False body) (withLetInline env v1 (\innerEnv -> Flows.bind (Flows.mapList (encodeBindingAsAssignment False innerEnv) bindings) (\pbindingExprs -> Flows.bind (encodeTermInline innerEnv False body) (\pbody ->  
              let pbindingStarExprs = (Lists.map (\ne -> Syntax.StarNamedExpressionSimple ne) pbindingExprs)
              in  
                let pbodyStarExpr = (Utils.pyExpressionToPyStarNamedExpression pbody)
                in  
                  let tupleElements = (Lists.concat2 pbindingStarExprs [
                          pbodyStarExpr])
                  in  
                    let tupleExpr = (Utils.pyAtomToPyExpression (Syntax.AtomTuple (Syntax.Tuple tupleElements)))
                    in  
                      let indexValue = (Utils.pyAtomToPyExpression (Syntax.AtomNumber (Syntax.NumberInteger (Literals.int32ToBigint (Lists.length bindings)))))
                      in  
                        let indexedExpr = (Utils.primaryWithExpressionSlices (Utils.pyExpressionToPyPrimary tupleExpr) [
                                indexValue])
                        in (Flows.pure (Utils.pyPrimaryToPyExpression indexedExpr)))))))
        Core.TermList v1 -> (Flows.bind (Flows.mapList encode v1) (\pyExprs -> Flows.pure (Utils.pyAtomToPyExpression (Syntax.AtomTuple (Syntax.Tuple (Lists.map Utils.pyExpressionToPyStarNamedExpression pyExprs))))))
        Core.TermLiteral v1 -> (encodeLiteral v1)
        Core.TermMap v1 -> (Flows.bind (Flows.mapList (\kv ->  
          let k = (Pairs.first kv)
          in  
            let v = (Pairs.second kv)
            in (Flows.bind (encode k) (\pyK -> Flows.bind (encode v) (\pyV -> Flows.pure (Syntax.DoubleStarredKvpairPair (Syntax.Kvpair {
              Syntax.kvpairKey = pyK,
              Syntax.kvpairValue = pyV})))))) (Maps.toList v1)) (\pairs -> Flows.pure (Utils.functionCall (Utils.pyNameToPyPrimary (Syntax.Name "FrozenDict")) [
          Utils.pyAtomToPyExpression (Syntax.AtomDict (Syntax.Dict pairs))])))
        Core.TermMaybe v1 -> (Maybes.maybe (Flows.pure (Utils.functionCall (Utils.pyNameToPyPrimary (Syntax.Name "Nothing")) [])) (\t1 -> Flows.bind (encode t1) (\pyexp -> withCast (Utils.functionCall (Utils.pyNameToPyPrimary (Syntax.Name "Just")) [
          pyexp]))) v1)
        Core.TermPair v1 ->  
          let t1 = (Pairs.first v1)
          in  
            let t2 = (Pairs.second v1)
            in (Flows.bind (encode t1) (\pyExpr1 -> Flows.bind (encode t2) (\pyExpr2 -> Flows.pure (Utils.pyAtomToPyExpression (Syntax.AtomTuple (Syntax.Tuple [
              Utils.pyExpressionToPyStarNamedExpression pyExpr1,
              (Utils.pyExpressionToPyStarNamedExpression pyExpr2)]))))))
        Core.TermRecord v1 ->  
          let tname = (Core.recordTypeName v1)
          in  
            let fields = (Core.recordFields v1)
            in (Flows.bind (Flows.mapList (\fld -> encode (Core.fieldTerm fld)) fields) (\pargs -> Flows.pure (Utils.functionCall (Utils.pyNameToPyPrimary (Names.encodeNameQualified env tname)) pargs)))
        Core.TermSet v1 -> (Flows.bind (Flows.mapList encode (Sets.toList v1)) (\pyEls -> Flows.pure (Utils.functionCall (Utils.pyNameToPyPrimary (Syntax.Name "frozenset")) [
          Utils.pyAtomToPyExpression (Syntax.AtomSet (Syntax.Set (Lists.map Utils.pyExpressionToPyStarNamedExpression pyEls)))])))
        Core.TermTypeApplication v1 ->  
          let body = (Core.typeApplicationTermBody v1)
          in (Flows.bind (encodeTermInline env True (stripTypeApps body)) (\pybase -> withCast pybase))
        Core.TermTypeLambda v1 ->  
          let body = (Core.typeLambdaBody v1)
          in (withTypeLambda env v1 (\env2 -> encodeTermInline env2 noCast body))
        Core.TermUnion v1 ->  
          let tname = (Core.injectionTypeName v1)
          in  
            let field = (Core.injectionField v1)
            in (Flows.bind (inGraphContext (Schemas.requireUnionType tname)) (\rt -> Logic.ifElse (Schemas.isEnumRowType rt) (Flows.pure (Utils.projectFromExpression (Utils.pyNameToPyExpression (Names.encodeNameQualified env tname)) (Names.encodeEnumValue env (Core.fieldName field)))) ( 
              let fname = (Core.fieldName field)
              in  
                let ftypes = (Core.rowTypeFields rt)
                in  
                  let isUnitVariant = (Maybes.maybe False (\ft -> Schemas.isUnitType (Rewriting.deannotateType (Core.fieldTypeType ft))) (Lists.find (\ft -> Equality.equal (Core.unName (Core.fieldTypeName ft)) (Core.unName fname)) ftypes))
                  in (Flows.bind (Logic.ifElse (Logic.or (Schemas.isUnitTerm (Core.fieldTerm field)) isUnitVariant) (Flows.pure []) (Flows.bind (encode (Core.fieldTerm field)) (\parg -> Flows.pure [
                    parg]))) (\args -> Flows.bind (updateMeta (setMetaUsesCast True)) (\unit_ -> Flows.pure (Utils.castTo (Names.typeVariableReference env tname) (Utils.functionCall (Utils.pyNameToPyPrimary (Names.variantName True env tname fname)) args))))))))
        Core.TermUnit -> (Flows.pure (Utils.pyNameToPyExpression Utils.pyNone))
        Core.TermVariable v1 -> (encodeVariable env v1 [])
        Core.TermWrap v1 ->  
          let tname = (Core.wrappedTermTypeName v1)
          in  
            let inner = (Core.wrappedTermBody v1)
            in (Flows.bind (encode inner) (\parg -> Flows.pure (Utils.functionCall (Utils.pyNameToPyPrimary (Names.encodeNameQualified env tname)) [
              parg])))) (Rewriting.deannotateTerm term))

-- | Extend metadata based on a term (used during module encoding)
extendMetaForTerm :: (Bool -> Helpers.PythonModuleMetadata -> Core.Term -> Helpers.PythonModuleMetadata)
extendMetaForTerm topLevel meta0 term =  
  let step = (\meta -> \t -> (\x -> case x of
          Core.TermEither v1 ->  
            let metaWithCast = (setMetaUsesCast True meta)
            in (Eithers.either (\_ -> setMetaUsesLeft metaWithCast True) (\_ -> setMetaUsesRight metaWithCast True) v1)
          Core.TermFunction v1 -> ((\x -> case x of
            Core.FunctionLambda v2 -> (Maybes.maybe meta (\dom -> Logic.ifElse topLevel (extendMetaForType True False dom meta) meta) (Core.lambdaDomain v2))
            _ -> meta) v1)
          Core.TermLet v1 ->  
            let bindings = (Core.letBindings v1)
            in (Lists.foldl ( 
              let forBinding = (\m -> \b -> Maybes.maybe m (\ts ->  
                      let term1 = (Core.bindingTerm b)
                      in (Logic.ifElse (CoderUtils.isSimpleAssignment term1) m (extendMetaForType True True (Core.typeSchemeType ts) m))) (Core.bindingType b))
              in forBinding) meta bindings)
          Core.TermLiteral v1 -> ((\x -> case x of
            Core.LiteralFloat v2 -> ((\x -> case x of
              Core.FloatValueBigfloat _ -> (setMetaUsesDecimal meta True)
              _ -> meta) v2)
            _ -> meta) v1)
          Core.TermMap _ -> (setMetaUsesFrozenDict meta True)
          Core.TermMaybe v1 -> (Maybes.maybe (setMetaUsesNothing meta True) (\_ -> setMetaUsesJust meta True) v1)
          Core.TermUnion _ -> (setMetaUsesCast True meta)
          _ -> meta) t)
  in (Rewriting.foldOverTerm Coders.TraversalOrderPre step meta0 term)

-- | Extend metadata based on a type (used during module encoding)
extendMetaForType :: (Bool -> Bool -> Core.Type -> Helpers.PythonModuleMetadata -> Helpers.PythonModuleMetadata)
extendMetaForType topLevel isTermAnnot typ meta =  
  let currentTvars = (Helpers.pythonModuleMetadataTypeVariables meta)
  in  
    let newTvars = (collectTypeVariables currentTvars typ)
    in  
      let metaWithTvars = (setMetaTypeVariables meta newTvars)
      in  
        let metaWithSubtypes = (Lists.foldl (\m -> \t -> extendMetaForType False isTermAnnot t m) metaWithTvars (Rewriting.subtypes typ))
        in ((\x -> case x of
          Core.TypeFunction v1 ->  
            let cod = (Core.functionTypeCodomain v1)
            in  
              let dom = (Core.functionTypeDomain v1)
              in  
                let meta2 = (extendMetaForType topLevel isTermAnnot cod metaWithSubtypes)
                in  
                  let meta3 = (extendMetaForType False isTermAnnot dom meta2)
                  in (Logic.ifElse (Logic.and isTermAnnot topLevel) meta3 (setMetaUsesCallable meta3 True))
          Core.TypeList _ -> (setMetaUsesFrozenList metaWithSubtypes True)
          Core.TypeMap _ -> (setMetaUsesFrozenDict metaWithSubtypes True)
          Core.TypeMaybe _ -> (setMetaUsesMaybe metaWithSubtypes True)
          Core.TypeEither _ -> (setMetaUsesEither metaWithSubtypes True)
          Core.TypeLiteral v1 -> ((\x -> case x of
            Core.LiteralTypeFloat v2 -> ((\x -> case x of
              Core.FloatTypeBigfloat -> (setMetaUsesDecimal metaWithSubtypes True)
              _ -> metaWithSubtypes) v2)
            _ -> metaWithSubtypes) v1)
          Core.TypeUnion v1 -> (Logic.ifElse (Schemas.isEnumRowType v1) (setMetaUsesEnum metaWithSubtypes True) (Logic.ifElse (Logic.not (Lists.null (Core.rowTypeFields v1))) (setMetaUsesNode metaWithSubtypes True) metaWithSubtypes))
          Core.TypeForall v1 ->  
            let body = (Core.forallTypeBody v1)
            in  
              let metaForWrap = (digForWrap isTermAnnot metaWithSubtypes body)
              in ((\x -> case x of
                Core.TypeRecord _ -> (setMetaUsesGeneric metaForWrap True)
                _ -> metaForWrap) (Rewriting.deannotateType body))
          Core.TypeRecord v1 ->  
            let fields = (Core.rowTypeFields v1)
            in  
              let hasAnnotated = (Lists.foldl (\b -> \ft -> Logic.or b (Annotations.hasTypeDescription (Core.fieldTypeType ft))) False fields)
              in  
                let meta1 = (Logic.ifElse (Lists.null fields) metaWithSubtypes (setMetaUsesDataclass metaWithSubtypes True))
                in (Logic.ifElse hasAnnotated (setMetaUsesAnnotated meta1 True) meta1)
          Core.TypeWrap _ -> (Logic.ifElse isTermAnnot metaWithSubtypes (setMetaUsesNode metaWithSubtypes True))
          _ -> metaWithSubtypes) (Rewriting.deannotateType typ))

-- | Recursively dig through forall types to find wrap types
digForWrap :: (Bool -> Helpers.PythonModuleMetadata -> Core.Type -> Helpers.PythonModuleMetadata)
digForWrap isTermAnnot meta typ = ((\x -> case x of
  Core.TypeForall v1 -> (digForWrap isTermAnnot meta (Core.forallTypeBody v1))
  Core.TypeWrap _ -> (Logic.ifElse isTermAnnot meta (setMetaUsesNode meta True))
  _ -> meta) (Rewriting.deannotateType typ))

setMetaNamespaces :: (Module.Namespaces Syntax.DottedName -> Helpers.PythonModuleMetadata -> Helpers.PythonModuleMetadata)
setMetaNamespaces ns m = Helpers.PythonModuleMetadata {
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

setMetaUsesLeft :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesLeft m b = Helpers.PythonModuleMetadata {
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

setMetaUsesRight :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesRight m b = Helpers.PythonModuleMetadata {
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

setMetaUsesDecimal :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesDecimal m b = Helpers.PythonModuleMetadata {
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

setMetaUsesFrozenDict :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesFrozenDict m b = Helpers.PythonModuleMetadata {
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

setMetaUsesNothing :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesNothing m b = Helpers.PythonModuleMetadata {
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

setMetaUsesJust :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesJust m b = Helpers.PythonModuleMetadata {
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

setMetaUsesCallable :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesCallable m b = Helpers.PythonModuleMetadata {
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

setMetaUsesLruCache :: (Bool -> Helpers.PythonModuleMetadata -> Helpers.PythonModuleMetadata)
setMetaUsesLruCache b m = Helpers.PythonModuleMetadata {
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

setMetaUsesCast :: (Bool -> Helpers.PythonModuleMetadata -> Helpers.PythonModuleMetadata)
setMetaUsesCast b m = Helpers.PythonModuleMetadata {
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

setMetaUsesGeneric :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesGeneric m b = Helpers.PythonModuleMetadata {
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

setMetaUsesFrozenList :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesFrozenList m b = Helpers.PythonModuleMetadata {
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

setMetaUsesMaybe :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesMaybe m b = Helpers.PythonModuleMetadata {
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

setMetaUsesEither :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesEither m b = Helpers.PythonModuleMetadata {
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

setMetaUsesNode :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesNode m b = Helpers.PythonModuleMetadata {
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

setMetaUsesEnum :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesEnum m b = Helpers.PythonModuleMetadata {
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

setMetaUsesAnnotated :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesAnnotated m b = Helpers.PythonModuleMetadata {
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

setMetaUsesDataclass :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesDataclass m b = Helpers.PythonModuleMetadata {
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

setMetaTypeVariables :: (Helpers.PythonModuleMetadata -> S.Set Core.Name -> Helpers.PythonModuleMetadata)
setMetaTypeVariables m tvars = Helpers.PythonModuleMetadata {
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
isTypeVariableName :: (Core.Name -> Bool)
isTypeVariableName name = (Equality.equal 1 (Lists.length (Strings.splitOn "." (Core.unName name))))

-- | Collect type variables from a type
collectTypeVariables :: (S.Set Core.Name -> Core.Type -> S.Set Core.Name)
collectTypeVariables initial typ = ((\x -> case x of
  Core.TypeForall v1 ->  
    let v = (Core.forallTypeParameter v1)
    in  
      let body = (Core.forallTypeBody v1)
      in (collectTypeVariables (Sets.insert v initial) body)
  _ ->  
    let freeVars = (Rewriting.freeVariablesInType typ)
    in  
      let isTypeVar = (\n -> isTypeVariableName n)
      in  
        let filteredList = (Lists.filter isTypeVar (Sets.toList freeVars))
        in (Sets.union initial (Sets.fromList filteredList))) (Rewriting.deannotateType typ))

-- | Extend metadata for a list of types
extendMetaForTypes :: ([Core.Type] -> Helpers.PythonModuleMetadata -> Helpers.PythonModuleMetadata)
extendMetaForTypes types meta =  
  let names = (Sets.unions (Lists.map (\t -> Rewriting.typeDependencyNames False t) types))
  in  
    let currentNs = (Helpers.pythonModuleMetadataNamespaces meta)
    in  
      let updatedNs = (Schemas.addNamesToNamespaces Names.encodeNamespace names currentNs)
      in  
        let meta1 = (setMetaNamespaces updatedNs meta)
        in (Lists.foldl (\m -> \t -> extendMetaForType True False t m) meta1 types)

setMetaUsesName :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesName m b = Helpers.PythonModuleMetadata {
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

setMetaUsesTypeVar :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesTypeVar m b = Helpers.PythonModuleMetadata {
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
emptyMetadata :: (Module.Namespaces Syntax.DottedName -> Helpers.PythonModuleMetadata)
emptyMetadata ns = Helpers.PythonModuleMetadata {
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
gatherMetadata :: (Module.Namespace -> [Module.Definition] -> Helpers.PythonModuleMetadata)
gatherMetadata focusNs defs =  
  let start = (emptyMetadata (Utils.findNamespaces focusNs defs))
  in  
    let addDef = (\meta -> \def -> (\x -> case x of
            Module.DefinitionTerm v1 ->  
              let term = (Module.termDefinitionTerm v1)
              in  
                let typScheme = (Module.termDefinitionType v1)
                in  
                  let typ = (Core.typeSchemeType typScheme)
                  in  
                    let meta2 = (extendMetaForType True True typ meta)
                    in (extendMetaForTerm True meta2 term)
            Module.DefinitionType v1 ->  
              let typ = (Module.typeDefinitionType v1)
              in  
                let meta2 = (setMetaUsesName meta True)
                in (Rewriting.foldOverType Coders.TraversalOrderPre (\m -> \t -> extendMetaForType True False t m) meta2 typ)) def)
    in  
      let result = (Lists.foldl addDef start defs)
      in  
        let tvars = (Helpers.pythonModuleMetadataTypeVariables result)
        in (setMetaUsesTypeVar result (Logic.not (Sets.null tvars)))

setMetaUsesTypeAlias :: (Helpers.PythonModuleMetadata -> Bool -> Helpers.PythonModuleMetadata)
setMetaUsesTypeAlias m b = Helpers.PythonModuleMetadata {
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
isTypeModuleCheck :: ([Module.Definition] -> Bool)
isTypeModuleCheck defs = (Logic.not (Lists.null (Lists.filter (\d -> (\x -> case x of
  Module.DefinitionType _ -> True
  _ -> False) d) defs)))

-- | Reorder definitions: types first, then topologically sorted terms
reorderDefs :: ([Module.Definition] -> [Module.Definition])
reorderDefs defs =  
  let partitioned = (Schemas.partitionDefinitions defs)
  in  
    let typeDefsRaw = (Pairs.first partitioned)
    in  
      let termDefsRaw = (Pairs.second partitioned)
      in  
        let nameFirst = (Lists.filter (\td -> Equality.equal (Module.typeDefinitionName td) (Core.Name "hydra.core.Name")) typeDefsRaw)
        in  
          let nameRest = (Lists.filter (\td -> Logic.not (Equality.equal (Module.typeDefinitionName td) (Core.Name "hydra.core.Name"))) typeDefsRaw)
          in  
            let sortedTypeDefs = (Lists.concat [
                    Lists.map (\td -> Module.DefinitionType td) nameFirst,
                    (Lists.map (\td -> Module.DefinitionType td) nameRest)])
            in  
              let termDefs = (Lists.map (\td -> Module.DefinitionTerm td) termDefsRaw)
              in  
                let sortedTermDefs = (Lists.concat (Sorting.topologicalSortNodes (\d -> (\x -> case x of
                        Module.DefinitionTerm v1 -> (Module.termDefinitionName v1)) d) (\d -> (\x -> case x of
                        Module.DefinitionTerm v1 -> (Sets.toList (Rewriting.freeVariablesInTerm (Module.termDefinitionTerm v1)))
                        _ -> []) d) termDefs))
                in (Lists.concat [
                  sortedTypeDefs,
                  sortedTermDefs])

-- | Create a TypeVar assignment statement for a type variable name
tvarStatement :: (Syntax.Name -> Syntax.Statement)
tvarStatement name = (Utils.assignmentStatement name (Utils.functionCall (Syntax.PrimarySimple (Syntax.AtomName (Syntax.Name "TypeVar"))) [
  Utils.doubleQuotedString (Syntax.unName name)]))

condImportSymbol :: (t0 -> Bool -> Maybe t0)
condImportSymbol name flag = (Logic.ifElse flag (Just name) Nothing)

-- | Generate domain import statements from namespace mappings
moduleDomainImports :: (Module.Namespaces Syntax.DottedName -> [Syntax.ImportStatement])
moduleDomainImports namespaces =  
  let names = (Lists.sort (Maps.elems (Module.namespacesMapping namespaces)))
  in (Lists.map (\ns -> Syntax.ImportStatementName (Syntax.ImportName [
    Syntax.DottedAsName {
      Syntax.dottedAsNameName = ns,
      Syntax.dottedAsNameAs = Nothing}])) names)

-- | Generate a single from-import statement
standardImportStatement :: (String -> [String] -> Syntax.ImportStatement)
standardImportStatement modName symbols = (Syntax.ImportStatementFrom (Syntax.ImportFrom {
  Syntax.importFromPrefixes = [],
  Syntax.importFromDottedName = (Just (Syntax.DottedName [
    Syntax.Name modName])),
  Syntax.importFromTargets = (Syntax.ImportFromTargetsSimple (Lists.map (\s -> Syntax.ImportFromAsName {
    Syntax.importFromAsNameName = (Syntax.Name s),
    Syntax.importFromAsNameAs = Nothing}) symbols))}))

-- | Generate standard import statements based on module metadata
moduleStandardImports :: (Helpers.PythonModuleMetadata -> [Syntax.ImportStatement])
moduleStandardImports meta =  
  let pairs = [
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
  in  
    let simplified = (Maybes.cat (Lists.map (\p ->  
            let modName = (Pairs.first p)
            in  
              let symbols = (Maybes.cat (Pairs.second p))
              in (Logic.ifElse (Lists.null symbols) Nothing (Just (modName, symbols)))) pairs))
    in (Lists.map (\p -> standardImportStatement (Pairs.first p) (Pairs.second p)) simplified)

-- | Generate all import statements for a Python module
moduleImports :: (Module.Namespaces Syntax.DottedName -> Helpers.PythonModuleMetadata -> [Syntax.Statement])
moduleImports namespaces meta = (Lists.map (\imp -> Utils.pySimpleStatementToPyStatement (Syntax.SimpleStatementImport imp)) (Lists.concat [
  moduleStandardImports meta,
  (moduleDomainImports namespaces)]))

-- | Encode a Hydra module to a Python module AST
encodePythonModule :: (Module.Module -> [Module.Definition] -> Compute.Flow Graph.Graph Syntax.Module)
encodePythonModule mod defs0 =  
  let defs = (reorderDefs defs0)
  in  
    let meta0 = (gatherMetadata (Module.moduleNamespace mod) defs)
    in (Flows.bind Monads.getState (\g -> Monads.withState (makePyGraph g meta0) ( 
      let namespaces0 = (Helpers.pythonModuleMetadataNamespaces meta0)
      in (Flows.bind (Inference.initialTypeContext g) (\tcontext ->  
        let env0 = (initialEnvironment namespaces0 tcontext)
        in  
          let isTypeMod = (isTypeModuleCheck defs0)
          in (withDefinitions env0 defs (\env -> Flows.bind (Flows.map (\xs -> Lists.concat xs) (Flows.mapList (\d -> encodeDefinition env d) defs)) (\defStmts -> Flows.bind Monads.getState (\pyg1 ->  
            let meta1 = (pyGraphMetadata pyg1)
            in  
              let meta2 = (Logic.ifElse (Logic.and (Logic.not isTypeMod) useInlineTypeParams) (setMetaUsesTypeVar meta1 False) meta1)
              in  
                let meta = (Logic.ifElse (Logic.and isTypeMod (Equality.equal targetPythonVersion Helpers.PythonVersionPython310)) (setMetaUsesTypeAlias meta2 True) meta2)
                in  
                  let namespaces = (Helpers.pythonModuleMetadataNamespaces meta1)
                  in  
                    let commentStmts = (Maybes.maybe [] (\c -> [
                            Utils.commentStatement c]) (Maybes.map CoderUtils.normalizeComment (Module.moduleDescription mod)))
                    in  
                      let importStmts = (moduleImports namespaces meta)
                      in  
                        let tvars = (Logic.ifElse (Logic.or isTypeMod (Logic.not useInlineTypeParams)) (Helpers.pythonModuleMetadataTypeVariables meta) Sets.empty)
                        in  
                          let tvarStmts = (Lists.map (\tv -> tvarStatement (Names.encodeTypeVariable tv)) (Sets.toList tvars))
                          in  
                            let body = (Lists.filter (\group -> Logic.not (Lists.null group)) (Lists.concat [
                                    [
                                      commentStmts,
                                      importStmts,
                                      tvarStmts],
                                    defStmts]))
                            in (Flows.pure (Syntax.Module body)))))))))))

-- | Convert a Hydra module to Python source files
moduleToPython :: (Module.Module -> [Module.Definition] -> Compute.Flow Graph.Graph (M.Map String String))
moduleToPython mod defs = (Flows.bind (encodePythonModule mod defs) (\file ->  
  let s = (Serialization.printExpr (Serialization.parenthesize (Serde.encodeModule file)))
  in  
    let path = (Names_.namespaceToFilePath Util.CaseConventionLowerSnake (Module.FileExtension "py") (Module.moduleNamespace mod))
    in (Flows.pure (Maps.singleton path s))))
