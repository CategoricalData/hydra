module Hydra.Sources.Kernel.Terms.Differentiation where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

-- Deep (program-constructing) DSLs
import qualified Hydra.Dsl.Deep.Lib.Math     as DeepMath
import qualified Hydra.Dsl.Meta.DeepCore     as DeepCore

import qualified Hydra.Sources.Kernel.Terms.Strip as Strip
import qualified Hydra.Sources.Kernel.Terms.Variables as Variables


ns :: Namespace
ns = Namespace "hydra.differentiation"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns definitions
    [Strip.ns, Variables.ns]
    kernelTypesNamespaces $
    Just "Source-to-source automatic differentiation for Float64 terms."
  where
    definitions = [
      toDefinition differentiateBinary,
      toDefinition differentiateFunction,
      toDefinition differentiateTerm,
      toDefinition gradient,
      toDefinition primitiveDerivative]

-- Helper: construct a float64 literal term from a Haskell Double
f64 :: Double -> TTerm Term
f64 = MetaTerms.float64


-- | Differentiate a function term (Float64 -> Float64) with respect to its parameter.
--   Given a lambda \x -> body, returns a lambda \x -> d(body)/dx.
differentiateFunction :: TTermDefinition (Term -> Term)
differentiateFunction = define "differentiateFunction" $
  doc "Differentiate a function term (Float64 -> Float64) with respect to its parameter" $
  "term" ~>
  cases _Term (var "term")
    (Just $ var "term") [  -- Non-function terms pass through unchanged
    _Term_annotated>>: "at" ~>
      differentiateFunction @@ Core.annotatedTermBody (var "at"),
    _Term_lambda>>: "l" ~>
      "paramName" <~ Core.lambdaParameter (var "l") $
      "body" <~ Core.lambdaBody (var "l") $
      DeepCore.lambdaTyped
        (var "paramName")
        (Core.lambdaDomain $ var "l")
        (differentiateTerm @@ var "paramName" @@ var "body")]

-- | Differentiate a term with respect to a named variable.
--   Implements the standard rules of differential calculus as a source-to-source
--   transformation on Hydra terms.
differentiateTerm :: TTermDefinition (Name -> Term -> Term)
differentiateTerm = define "differentiateTerm" $
  doc "Differentiate a term with respect to a named variable" $
  "dx" ~> "term" ~>
  cases _Term (var "term") Nothing [
    -- Variable: d/dx(x) = 1.0, d/dx(y) = 0.0
    _Term_variable>>: "v" ~>
      Logic.ifElse (Equality.equal (var "v") (var "dx"))
        (f64 1.0)
        (f64 0.0),

    -- Literal: d/dx(c) = 0.0
    _Term_literal>>: constant $ f64 0.0,

    -- Application: handle primitive functions and general chain rule
    _Term_application>>: "app" ~>
      "func" <~ Core.applicationFunction (var "app") $
      "arg" <~ Core.applicationArgument (var "app") $
      -- Check if the function is a variable (potentially a primitive)
      cases _Term (var "func") (Just $
        -- General case: f(g) => f'(g) * g'
        DeepMath.mulFloat64
          (differentiateTerm @@ var "dx" @@ Core.termApplication (Core.application (var "func") (var "arg")))
          (differentiateTerm @@ var "dx" @@ var "arg")) [
        _Term_variable>>: "fname" ~>
          -- Check if this is a known unary primitive
          optCases (primitiveDerivative @@ var "fname")
            -- Not a known primitive: general application derivative
            (differentiateTerm @@ var "dx" @@ Core.termApplication (Core.application (var "func") (var "arg")))
            -- Known unary primitive: chain rule f(g(x))' = f'(g(x)) * g'(x)
            ("derivTerm" ~>
              DeepMath.mulFloat64
                (Core.termApplication $ Core.application (var "derivTerm") (var "arg"))
                (differentiateTerm @@ var "dx" @@ var "arg")),
        -- Partially applied binary primitive: f a b => handle add, mul, sub, pow, etc.
        _Term_application>>: "innerApp" ~>
          "innerFunc" <~ Core.applicationFunction (var "innerApp") $
          "innerArg" <~ Core.applicationArgument (var "innerApp") $
          cases _Term (var "innerFunc")
            (Just $
              -- Not a variable: fall back to general differentiation
              DeepMath.mulFloat64
                (differentiateTerm @@ var "dx" @@ Core.termApplication (Core.application (var "func") (var "arg")))
                (differentiateTerm @@ var "dx" @@ var "arg")) [
            _Term_variable>>: "bfname" ~>
              -- Binary primitive detected: innerFunc innerArg arg = bfname(innerArg, arg)
              differentiateBinary @@ var "bfname"
                @@ var "innerArg"
                @@ var "arg"
                @@ (differentiateTerm @@ var "dx" @@ var "innerArg")
                @@ (differentiateTerm @@ var "dx" @@ var "arg")]],

    -- Function (lambda): d/dx(\y -> body) = \y -> d/dx(body)
    -- (The bound variable y is different from x, so we differentiate the body)
    _Term_lambda>>: "l" ~>
      Logic.ifElse (Equality.equal (Core.lambdaParameter $ var "l") (var "dx"))
        -- Lambda binds the differentiation variable: derivative is zero function
        (DeepCore.lambdaTyped
          (Core.lambdaParameter $ var "l")
          (Core.lambdaDomain $ var "l")
          (f64 0.0))
        -- Lambda binds a different variable: differentiate the body
        (DeepCore.lambdaTyped
          (Core.lambdaParameter $ var "l")
          (Core.lambdaDomain $ var "l")
          (differentiateTerm @@ var "dx" @@ Core.lambdaBody (var "l"))),
    _Term_cases>>: constant $ f64 0.0,
    _Term_project>>: constant $ f64 0.0,
    _Term_unwrap>>: constant $ f64 0.0,

    -- Let: differentiate bindings and body
    _Term_let>>: "l" ~>
      Core.termLet $ Core.let_
        (Lists.map ("b" ~>
          Core.binding
            (Core.bindingName $ var "b")
            (differentiateTerm @@ var "dx" @@ Core.bindingTerm (var "b"))
            nothing)
          (Core.letBindings $ var "l"))
        (differentiateTerm @@ var "dx" @@ Core.letBody (var "l")),

    -- Annotated: strip annotation and differentiate
    _Term_annotated>>: "at" ~>
      differentiateTerm @@ var "dx" @@ Core.annotatedTermBody (var "at"),

    -- List: element-wise differentiation
    _Term_list>>: "elems" ~>
      Core.termList $ Lists.map (differentiateTerm @@ var "dx") (var "elems"),

    -- Pair: component-wise differentiation
    _Term_pair>>: "p" ~>
      Core.termPair $ pair
        (differentiateTerm @@ var "dx" @@ Pairs.first (var "p"))
        (differentiateTerm @@ var "dx" @@ Pairs.second (var "p")),

    -- Record: differentiate each field
    _Term_record>>: "r" ~>
      Core.termRecord $ Core.record
        (Core.recordTypeName $ var "r")
        (Lists.map ("fld" ~>
          Core.field
            (Core.fieldName $ var "fld")
            (differentiateTerm @@ var "dx" @@ Core.fieldTerm (var "fld")))
          (Core.recordFields $ var "r")),

    -- TypeApplication: pass through
    _Term_typeApplication>>: "ta" ~>
      differentiateTerm @@ var "dx" @@ Core.typeApplicationTermBody (var "ta"),

    -- TypeLambda: pass through
    _Term_typeLambda>>: "tl" ~>
      differentiateTerm @@ var "dx" @@ Core.typeLambdaBody (var "tl"),

    -- Unit, Set, Map, Either, Maybe, Union, Wrap: derivative is zero / unsupported
    _Term_unit>>: constant $ f64 0.0,
    _Term_set>>: constant $ f64 0.0,
    _Term_map>>: constant $ f64 0.0,
    _Term_either>>: constant $ f64 0.0,
    _Term_maybe>>: constant $ f64 0.0,
    _Term_union>>: constant $ f64 0.0,
    _Term_wrap>>: constant $ f64 0.0]

-- | Differentiate a binary primitive application.
--   bfname is the primitive name, a and b are the two arguments,
--   da and db are their derivatives with respect to x.
--   Returns d/dx(bfname(a, b)).
differentiateBinary :: TTermDefinition (Name -> Term -> Term -> Term -> Term -> Term)
differentiateBinary = define "differentiateBinary" $
  doc "Differentiate a binary primitive application given both arguments and their derivatives" $
  "bfname" ~> "a" ~> "b" ~> "da" ~> "db" ~>
    -- d/dx(a + b) = da + db (both Int32 and Float64 names)
    Logic.ifElse (Logic.or (Equality.equal (var "bfname") (encodedName _math_add))
                           (Equality.equal (var "bfname") (encodedName _math_addFloat64)))
        (DeepMath.addFloat64 (var "da") (var "db")) $
      -- d/dx(a - b) = da - db
      Logic.ifElse (Logic.or (Equality.equal (var "bfname") (encodedName _math_sub))
                             (Equality.equal (var "bfname") (encodedName _math_subFloat64)))
        (DeepMath.subFloat64 (var "da") (var "db")) $
      -- d/dx(a * b) = a*db + b*da  (product rule)
      Logic.ifElse (Logic.or (Equality.equal (var "bfname") (encodedName _math_mul))
                             (Equality.equal (var "bfname") (encodedName _math_mulFloat64)))
        (DeepMath.addFloat64
          (DeepMath.mulFloat64 (var "a") (var "db"))
          (DeepMath.mulFloat64 (var "b") (var "da"))) $
      -- d/dx(a ^ b) = a^b * (b*da/a + db*ln(a))  (general power rule)
      Logic.ifElse (Equality.equal (var "bfname") (encodedName _math_pow))
        (DeepMath.mulFloat64
          (DeepMath.pow (var "a") (var "b"))
          (DeepMath.addFloat64
            (DeepMath.mulFloat64 (var "db") (DeepMath.log (var "a")))
            (DeepMath.mulFloat64
              (DeepMath.mulFloat64 (var "b") (var "da"))
              (DeepMath.pow (var "a") (f64 (-1.0)))))) $
      -- d/dx(atan2(a, b)) = (b*da - a*db) / (a^2 + b^2)
      Logic.ifElse (Equality.equal (var "bfname") (encodedName _math_atan2))
        (DeepMath.mulFloat64
          (DeepMath.subFloat64
            (DeepMath.mulFloat64 (var "b") (var "da"))
            (DeepMath.mulFloat64 (var "a") (var "db")))
          (DeepMath.pow
            (DeepMath.addFloat64
              (DeepMath.mulFloat64 (var "a") (var "a"))
              (DeepMath.mulFloat64 (var "b") (var "b")))
            (f64 (-1.0)))) $
      -- d/dx(logBase(a, b)) = d/dx(ln(b)/ln(a))
      -- = (da*ln(b) is wrong)... use: logBase(a,b) = ln(b)/ln(a)
      -- d/dx = (ln(a)*db/b - ln(b)*da/a) / (ln(a))^2
      Logic.ifElse (Equality.equal (var "bfname") (encodedName _math_logBase))
        (DeepMath.mulFloat64
          (DeepMath.subFloat64
            (DeepMath.mulFloat64
              (DeepMath.log (var "a"))
              (DeepMath.mulFloat64 (var "db") (DeepMath.pow (var "b") (f64 (-1.0)))))
            (DeepMath.mulFloat64
              (DeepMath.log (var "b"))
              (DeepMath.mulFloat64 (var "da") (DeepMath.pow (var "a") (f64 (-1.0))))))
          (DeepMath.pow
            (DeepMath.mulFloat64 (DeepMath.log (var "a")) (DeepMath.log (var "a")))
            (f64 (-1.0)))) $
      -- Unknown binary primitive: return 0
      f64 0.0

-- | Compute the gradient of a term with respect to a list of named variables.
--   Returns a record term where each field is the partial derivative of the term
--   with respect to the corresponding variable.
gradient :: TTermDefinition (Name -> [Name] -> Term -> Term)
gradient = define "gradient" $
  doc "Compute the gradient of a term as a record of partial derivatives" $
  "typeName" ~> "vars" ~> "term" ~>
  Core.termRecord $ Core.record
    (var "typeName")
    (Lists.map ("v" ~>
      Core.field
        (var "v")
        (differentiateTerm @@ var "v" @@ var "term"))
      (var "vars"))

-- | Look up the derivative of a unary Float64 primitive by name.
--   Returns Just a term representing the derivative function (a lambda),
--   or Nothing if the primitive is not differentiable.
primitiveDerivative :: TTermDefinition (Name -> Maybe Term)
primitiveDerivative = define "primitiveDerivative" $
  doc "Look up the derivative of a unary Float64 primitive" $
  "name" ~>
  -- d/dx(sin(x)) = cos(x)
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_sin))
    (just $ DeepMath.ref _math_cos) $
  -- d/dx(cos(x)) = -sin(x)
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_cos))
    (just $ DeepCore.lambda "_x" (DeepMath.negateFloat64 (DeepMath.sin (DeepCore.var "_x")))) $
  -- d/dx(tan(x)) = 1/cos(x)^2
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_tan))
    (just $ DeepCore.lambda "_x" (DeepMath.pow (DeepMath.cos (DeepCore.var "_x")) (f64 (-2.0)))) $
  -- d/dx(exp(x)) = exp(x)
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_exp))
    (just $ DeepMath.ref _math_exp) $
  -- d/dx(log(x)) = 1/x
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_log))
    (just $ DeepCore.lambda "_x" (DeepMath.pow (DeepCore.var "_x") (f64 (-1.0)))) $
  -- d/dx(sqrt(x)) = 1/(2*sqrt(x))
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_sqrt))
    (just $ DeepCore.lambda "_x" (DeepMath.mulFloat64 (f64 0.5)
        (DeepMath.pow (DeepMath.sqrt (DeepCore.var "_x")) (f64 (-1.0))))) $
  -- d/dx(asin(x)) = 1/sqrt(1 - x^2)
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_asin))
    (just $ DeepCore.lambda "_x" (DeepMath.pow
        (DeepMath.sqrt
          (DeepMath.subFloat64 (f64 1.0) (DeepMath.mulFloat64
            (DeepCore.var "_x")
            (DeepCore.var "_x"))))
        (f64 (-1.0)))) $
  -- d/dx(acos(x)) = -1/sqrt(1 - x^2)
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_acos))
    (just $ DeepCore.lambda "_x" (DeepMath.negateFloat64 $ DeepMath.pow
        (DeepMath.sqrt
          (DeepMath.subFloat64 (f64 1.0) (DeepMath.mulFloat64
            (DeepCore.var "_x")
            (DeepCore.var "_x"))))
        (f64 (-1.0)))) $
  -- d/dx(atan(x)) = 1/(1 + x^2)
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_atan))
    (just $ DeepCore.lambda "_x" (DeepMath.pow
        (DeepMath.addFloat64 (f64 1.0) (DeepMath.mulFloat64
          (DeepCore.var "_x")
          (DeepCore.var "_x")))
        (f64 (-1.0)))) $
  -- d/dx(sinh(x)) = cosh(x)
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_sinh))
    (just $ DeepMath.ref _math_cosh) $
  -- d/dx(cosh(x)) = sinh(x)
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_cosh))
    (just $ DeepMath.ref _math_sinh) $
  -- d/dx(tanh(x)) = 1 - tanh(x)^2
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_tanh))
    (just $ DeepCore.lambda "_x" (DeepMath.subFloat64 (f64 1.0)
        (DeepMath.mulFloat64
          (DeepMath.tanh (DeepCore.var "_x"))
          (DeepMath.tanh (DeepCore.var "_x"))))) $
  -- d/dx(asinh(x)) = 1/sqrt(x^2 + 1)
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_asinh))
    (just $ DeepCore.lambda "_x" (DeepMath.pow
        (DeepMath.sqrt
          (DeepMath.addFloat64
            (DeepMath.mulFloat64
              (DeepCore.var "_x")
              (DeepCore.var "_x"))
            (f64 1.0)))
        (f64 (-1.0)))) $
  -- d/dx(acosh(x)) = 1/sqrt(x^2 - 1)
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_acosh))
    (just $ DeepCore.lambda "_x" (DeepMath.pow
        (DeepMath.sqrt
          (DeepMath.subFloat64
            (DeepMath.mulFloat64
              (DeepCore.var "_x")
              (DeepCore.var "_x"))
            (f64 1.0)))
        (f64 (-1.0)))) $
  -- d/dx(atanh(x)) = 1/(1 - x^2)
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_atanh))
    (just $ DeepCore.lambda "_x" (DeepMath.pow
        (DeepMath.subFloat64 (f64 1.0) (DeepMath.mulFloat64
          (DeepCore.var "_x")
          (DeepCore.var "_x")))
        (f64 (-1.0)))) $
  -- d/dx(negate(x)) = -1
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_negate))
    (just $ DeepCore.lambda "_x" (f64 (-1.0))) $
  -- d/dx(abs(x)) = signum(x)
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_abs))
    (just $ DeepMath.ref _math_signum) $
  -- d/dx(ceiling(x)) = 0  (piecewise constant)
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_ceiling))
    (just $ DeepCore.lambda "_x" (f64 0.0)) $
  -- d/dx(floor(x)) = 0
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_floor))
    (just $ DeepCore.lambda "_x" (f64 0.0)) $
  -- d/dx(round(x)) = 0
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_round))
    (just $ DeepCore.lambda "_x" (f64 0.0)) $
  -- d/dx(truncate(x)) = 0
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_truncate))
    (just $ DeepCore.lambda "_x" (f64 0.0)) $
  -- d/dx(signum(x)) = 0  (piecewise constant)
  Logic.ifElse (Equality.equal (var "name") (encodedName _math_signum))
    (just $ DeepCore.lambda "_x" (f64 0.0)) $
  -- Unknown primitive
  nothing
