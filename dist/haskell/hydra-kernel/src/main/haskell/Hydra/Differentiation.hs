-- Note: this is an automatically generated file. Do not edit.

-- | Source-to-source automatic differentiation for Float64 terms.

module Hydra.Differentiation where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Differentiate a binary primitive application given both arguments and their derivatives
differentiateBinary :: Core.Name -> Core.Term -> Core.Term -> Core.Term -> Core.Term -> Core.Term
differentiateBinary bfname a b da db =
    Logic.ifElse (Logic.or (Equality.equal bfname (Core.Name "hydra.lib.math.add")) (Equality.equal bfname (Core.Name "hydra.lib.math.addFloat64"))) (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.addFloat64")),
        Core.applicationArgument = da})),
      Core.applicationArgument = db})) (Logic.ifElse (Logic.or (Equality.equal bfname (Core.Name "hydra.lib.math.sub")) (Equality.equal bfname (Core.Name "hydra.lib.math.subFloat64"))) (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.subFloat64")),
        Core.applicationArgument = da})),
      Core.applicationArgument = db})) (Logic.ifElse (Logic.or (Equality.equal bfname (Core.Name "hydra.lib.math.mul")) (Equality.equal bfname (Core.Name "hydra.lib.math.mulFloat64"))) (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.addFloat64")),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
            Core.applicationArgument = a})),
          Core.applicationArgument = db}))})),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
          Core.applicationArgument = b})),
        Core.applicationArgument = da}))})) (Logic.ifElse (Equality.equal bfname (Core.Name "hydra.lib.math.pow")) (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.pow")),
            Core.applicationArgument = a})),
          Core.applicationArgument = b}))})),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.addFloat64")),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
              Core.applicationArgument = db})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.log")),
              Core.applicationArgument = a}))}))})),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                Core.applicationArgument = b})),
              Core.applicationArgument = da}))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.pow")),
              Core.applicationArgument = a})),
            Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-1.0))))}))}))}))})) (Logic.ifElse (Equality.equal bfname (Core.Name "hydra.lib.math.atan2")) (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.subFloat64")),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                Core.applicationArgument = b})),
              Core.applicationArgument = da}))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
              Core.applicationArgument = a})),
            Core.applicationArgument = db}))}))})),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.pow")),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.addFloat64")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                  Core.applicationArgument = a})),
                Core.applicationArgument = a}))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                Core.applicationArgument = b})),
              Core.applicationArgument = b}))}))})),
        Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-1.0))))}))})) (Logic.ifElse (Equality.equal bfname (Core.Name "hydra.lib.math.logBase")) (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.subFloat64")),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.log")),
                  Core.applicationArgument = a}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                  Core.applicationArgument = db})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.pow")),
                    Core.applicationArgument = b})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-1.0))))}))}))}))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.log")),
                Core.applicationArgument = b}))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                Core.applicationArgument = da})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.pow")),
                  Core.applicationArgument = a})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-1.0))))}))}))}))}))})),
      Core.applicationArgument = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.pow")),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.log")),
                Core.applicationArgument = a}))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.log")),
              Core.applicationArgument = a}))}))})),
        Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-1.0))))}))})) (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))))))))

-- | Differentiate a function term (Float64 -> Float64) with respect to its parameter
differentiateFunction :: Core.Term -> Core.Term
differentiateFunction term =
    case term of
      Core.TermAnnotated v0 -> differentiateFunction (Core.annotatedTermBody v0)
      Core.TermLambda v0 ->
        let paramName = Core.lambdaParameter v0
            body = Core.lambdaBody v0
        in (Core.TermLambda (Core.Lambda {
          Core.lambdaParameter = paramName,
          Core.lambdaDomain = (Core.lambdaDomain v0),
          Core.lambdaBody = (differentiateTerm paramName body)}))
      _ -> term

-- | Differentiate a term with respect to a named variable
differentiateTerm :: Core.Name -> Core.Term -> Core.Term
differentiateTerm dx term =
    case term of
      Core.TermVariable v0 -> Logic.ifElse (Equality.equal v0 dx) (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0))) (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))
      Core.TermLiteral _ -> Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))
      Core.TermApplication v0 ->
        let func = Core.applicationFunction v0
            arg = Core.applicationArgument v0
        in case func of
          Core.TermVariable v1 -> Maybes.maybe (differentiateTerm dx (Core.TermApplication (Core.Application {
            Core.applicationFunction = func,
            Core.applicationArgument = arg}))) (\derivTerm -> Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = derivTerm,
                Core.applicationArgument = arg}))})),
            Core.applicationArgument = (differentiateTerm dx arg)})) (primitiveDerivative v1)
          Core.TermApplication v1 ->
            let innerFunc = Core.applicationFunction v1
                innerArg = Core.applicationArgument v1
            in case innerFunc of
              Core.TermVariable v2 -> differentiateBinary v2 innerArg arg (differentiateTerm dx innerArg) (differentiateTerm dx arg)
              _ -> Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                  Core.applicationArgument = (differentiateTerm dx (Core.TermApplication (Core.Application {
                    Core.applicationFunction = func,
                    Core.applicationArgument = arg})))})),
                Core.applicationArgument = (differentiateTerm dx arg)})
          _ -> Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
              Core.applicationArgument = (differentiateTerm dx (Core.TermApplication (Core.Application {
                Core.applicationFunction = func,
                Core.applicationArgument = arg})))})),
            Core.applicationArgument = (differentiateTerm dx arg)})
      Core.TermLambda v0 -> Logic.ifElse (Equality.equal (Core.lambdaParameter v0) dx) (Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.lambdaParameter v0),
        Core.lambdaDomain = (Core.lambdaDomain v0),
        Core.lambdaBody = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))})) (Core.TermLambda (Core.Lambda {
        Core.lambdaParameter = (Core.lambdaParameter v0),
        Core.lambdaDomain = (Core.lambdaDomain v0),
        Core.lambdaBody = (differentiateTerm dx (Core.lambdaBody v0))}))
      Core.TermCases _ -> Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))
      Core.TermProject _ -> Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))
      Core.TermUnwrap _ -> Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))
      Core.TermLet v0 -> Core.TermLet (Core.Let {
        Core.letBindings = (Lists.map (\b -> Core.Binding {
          Core.bindingName = (Core.bindingName b),
          Core.bindingTerm = (differentiateTerm dx (Core.bindingTerm b)),
          Core.bindingType = Nothing}) (Core.letBindings v0)),
        Core.letBody = (differentiateTerm dx (Core.letBody v0))})
      Core.TermAnnotated v0 -> differentiateTerm dx (Core.annotatedTermBody v0)
      Core.TermList v0 -> Core.TermList (Lists.map (differentiateTerm dx) v0)
      Core.TermPair v0 -> Core.TermPair (differentiateTerm dx (Pairs.first v0), (differentiateTerm dx (Pairs.second v0)))
      Core.TermRecord v0 -> Core.TermRecord (Core.Record {
        Core.recordTypeName = (Core.recordTypeName v0),
        Core.recordFields = (Lists.map (\fld -> Core.Field {
          Core.fieldName = (Core.fieldName fld),
          Core.fieldTerm = (differentiateTerm dx (Core.fieldTerm fld))}) (Core.recordFields v0))})
      Core.TermTypeApplication v0 -> differentiateTerm dx (Core.typeApplicationTermBody v0)
      Core.TermTypeLambda v0 -> differentiateTerm dx (Core.typeLambdaBody v0)
      Core.TermUnit -> Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))
      Core.TermSet _ -> Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))
      Core.TermMap _ -> Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))
      Core.TermEither _ -> Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))
      Core.TermMaybe _ -> Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))
      Core.TermInject _ -> Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))
      Core.TermWrap _ -> Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))

-- | Compute the gradient of a term as a record of partial derivatives
gradient :: Core.Name -> [Core.Name] -> Core.Term -> Core.Term
gradient typeName vars term =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = typeName,
      Core.recordFields = (Lists.map (\v -> Core.Field {
        Core.fieldName = v,
        Core.fieldTerm = (differentiateTerm v term)}) vars)})

-- | Look up the derivative of a unary Float64 primitive
primitiveDerivative :: Core.Name -> Maybe Core.Term
primitiveDerivative name =
    Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.sin")) (Just (Core.TermVariable (Core.Name "hydra.lib.math.cos"))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.cos")) (Just (Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "_x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.negateFloat64")),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sin")),
          Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))}))}))}))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.tan")) (Just (Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "_x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.pow")),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.cos")),
            Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))}))})),
        Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-2.0))))}))}))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.exp")) (Just (Core.TermVariable (Core.Name "hydra.lib.math.exp"))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.log")) (Just (Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "_x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.pow")),
          Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))})),
        Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-1.0))))}))}))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.sqrt")) (Just (Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "_x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
          Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.5)))})),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.pow")),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sqrt")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))}))})),
          Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-1.0))))}))}))}))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.asin")) (Just (Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "_x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.pow")),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sqrt")),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.subFloat64")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))}))}))}))})),
        Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-1.0))))}))}))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.acos")) (Just (Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "_x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.negateFloat64")),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.pow")),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sqrt")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.subFloat64")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))}))}))}))})),
          Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-1.0))))}))}))}))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.atan")) (Just (Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "_x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.pow")),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.addFloat64")),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))}))}))})),
        Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-1.0))))}))}))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.sinh")) (Just (Core.TermVariable (Core.Name "hydra.lib.math.cosh"))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.cosh")) (Just (Core.TermVariable (Core.Name "hydra.lib.math.sinh"))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.tanh")) (Just (Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "_x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.subFloat64")),
          Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))})),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.tanh")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))}))})),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.tanh")),
            Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))}))}))}))}))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.asinh")) (Just (Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "_x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.pow")),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sqrt")),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.addFloat64")),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))}))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))}))}))})),
        Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-1.0))))}))}))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.acosh")) (Just (Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "_x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.pow")),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.sqrt")),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.subFloat64")),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))}))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))}))}))})),
        Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-1.0))))}))}))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.atanh")) (Just (Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "_x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.pow")),
          Core.applicationArgument = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.subFloat64")),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mulFloat64")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "_x"))}))}))})),
        Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-1.0))))}))}))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.negate")) (Just (Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "_x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-1.0))))}))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.abs")) (Just (Core.TermVariable (Core.Name "hydra.lib.math.signum"))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.ceiling")) (Just (Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "_x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))}))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.floor")) (Just (Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "_x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))}))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.round")) (Just (Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "_x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))}))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.truncate")) (Just (Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "_x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))}))) (Logic.ifElse (Equality.equal name (Core.Name "hydra.lib.math.signum")) (Just (Core.TermLambda (Core.Lambda {
      Core.lambdaParameter = (Core.Name "_x"),
      Core.lambdaDomain = Nothing,
      Core.lambdaBody = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))}))) Nothing)))))))))))))))))))))
