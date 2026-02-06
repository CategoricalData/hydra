-- | AST operators for JavaScript, defining precedence and associativity.
--
-- Operator precedence is based on:
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_precedence

module Hydra.Ext.Sources.JavaScript.Operators where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import qualified Hydra.Sources.Kernel.Terms.All             as KernelTerms
import qualified Hydra.Sources.Kernel.Types.All             as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Serialization   as Serialization
import           Prelude hiding ((++))
import qualified Data.Int                                   as I
import qualified Data.List                                  as L
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified Data.Maybe                                 as Y

-- Additional imports
import Hydra.Ast


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.javaScript.operators"

module_ :: Module
module_ = Module ns elements
    [Serialization.ns]
    KernelTypes.kernelTypesNamespaces $
    Just "AST operators for JavaScript with precedence and associativity"
  where
    elements = [
      -- Function application
      toBinding appOp,

      -- Member access
      toBinding memberOp,
      toBinding optionalChainOp,

      -- Arithmetic operators
      toBinding exponentiateOp,
      toBinding multiplyOp,
      toBinding divideOp,
      toBinding moduloOp,
      toBinding addOp,
      toBinding subtractOp,

      -- Comparison operators
      toBinding lessThanOp,
      toBinding lessThanOrEqualOp,
      toBinding greaterThanOp,
      toBinding greaterThanOrEqualOp,
      toBinding inOp,
      toBinding instanceOfOp,
      toBinding equalOp,
      toBinding notEqualOp,
      toBinding strictEqualOp,
      toBinding strictNotEqualOp,

      -- Bitwise operators
      toBinding bitwiseAndOp,
      toBinding bitwiseXorOp,
      toBinding bitwiseOrOp,
      toBinding leftShiftOp,
      toBinding rightShiftOp,
      toBinding unsignedRightShiftOp,

      -- Logical operators
      toBinding logicalAndOp,
      toBinding logicalOrOp,
      toBinding nullishCoalescingOp,

      -- Assignment operators
      toBinding assignOp,

      -- Arrow function
      toBinding arrowOp,

      -- Ternary
      toBinding ternaryOp,

      -- Comma
      toBinding commaOp,

      -- Definition (for const x = ...)
      toBinding defineOp,

      -- Type annotation (for JSDoc/TypeScript)
      toBinding colonOp]

-- Note: JavaScript precedence levels (MDN):
-- 1 = comma, 2 = assignment, 3 = conditional, 4 = nullish coalescing,
-- 5 = logical or, 6 = logical and, 7-10 = bitwise, 11 = equality,
-- 12 = relational, 13 = shift, 14 = additive, 15 = multiplicative,
-- 16 = exponentiation, 17 = unary, 18 = update, 19 = new, 20 = member access

appOp :: TBinding Op
appOp = define "appOp" $
  doc "Function application (whitespace)" $
  Ast.op
    (Ast.symbol $ string "")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 20)
    Ast.associativityLeft

memberOp :: TBinding Op
memberOp = define "memberOp" $
  doc "Member access operator (.)" $
  Ast.op
    (Ast.symbol $ string ".")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 20)
    Ast.associativityLeft

optionalChainOp :: TBinding Op
optionalChainOp = define "optionalChainOp" $
  doc "Optional chaining operator (?.)" $
  Ast.op
    (Ast.symbol $ string "?.")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 20)
    Ast.associativityLeft

exponentiateOp :: TBinding Op
exponentiateOp = define "exponentiateOp" $
  doc "Exponentiation operator (**)" $
  Serialization.op @@ string "**" @@ int32 16 @@ Ast.associativityRight

multiplyOp :: TBinding Op
multiplyOp = define "multiplyOp" $
  doc "Multiplication operator (*)" $
  Serialization.op @@ string "*" @@ int32 15 @@ Ast.associativityLeft

divideOp :: TBinding Op
divideOp = define "divideOp" $
  doc "Division operator (/)" $
  Serialization.op @@ string "/" @@ int32 15 @@ Ast.associativityLeft

moduloOp :: TBinding Op
moduloOp = define "moduloOp" $
  doc "Modulo operator (%)" $
  Serialization.op @@ string "%" @@ int32 15 @@ Ast.associativityLeft

addOp :: TBinding Op
addOp = define "addOp" $
  doc "Addition operator (+)" $
  Serialization.op @@ string "+" @@ int32 14 @@ Ast.associativityLeft

subtractOp :: TBinding Op
subtractOp = define "subtractOp" $
  doc "Subtraction operator (-)" $
  Serialization.op @@ string "-" @@ int32 14 @@ Ast.associativityLeft

leftShiftOp :: TBinding Op
leftShiftOp = define "leftShiftOp" $
  doc "Left shift operator (<<)" $
  Serialization.op @@ string "<<" @@ int32 13 @@ Ast.associativityLeft

rightShiftOp :: TBinding Op
rightShiftOp = define "rightShiftOp" $
  doc "Right shift operator (>>)" $
  Serialization.op @@ string ">>" @@ int32 13 @@ Ast.associativityLeft

unsignedRightShiftOp :: TBinding Op
unsignedRightShiftOp = define "unsignedRightShiftOp" $
  doc "Unsigned right shift operator (>>>)" $
  Serialization.op @@ string ">>>" @@ int32 13 @@ Ast.associativityLeft

lessThanOp :: TBinding Op
lessThanOp = define "lessThanOp" $
  doc "Less than operator (<)" $
  Serialization.op @@ string "<" @@ int32 12 @@ Ast.associativityLeft

lessThanOrEqualOp :: TBinding Op
lessThanOrEqualOp = define "lessThanOrEqualOp" $
  doc "Less than or equal operator (<=)" $
  Serialization.op @@ string "<=" @@ int32 12 @@ Ast.associativityLeft

greaterThanOp :: TBinding Op
greaterThanOp = define "greaterThanOp" $
  doc "Greater than operator (>)" $
  Serialization.op @@ string ">" @@ int32 12 @@ Ast.associativityLeft

greaterThanOrEqualOp :: TBinding Op
greaterThanOrEqualOp = define "greaterThanOrEqualOp" $
  doc "Greater than or equal operator (>=)" $
  Serialization.op @@ string ">=" @@ int32 12 @@ Ast.associativityLeft

inOp :: TBinding Op
inOp = define "inOp" $
  doc "In operator (in)" $
  Serialization.op @@ string "in" @@ int32 12 @@ Ast.associativityLeft

instanceOfOp :: TBinding Op
instanceOfOp = define "instanceOfOp" $
  doc "Instance of operator (instanceof)" $
  Serialization.op @@ string "instanceof" @@ int32 12 @@ Ast.associativityLeft

equalOp :: TBinding Op
equalOp = define "equalOp" $
  doc "Equality operator (==)" $
  Serialization.op @@ string "==" @@ int32 11 @@ Ast.associativityLeft

notEqualOp :: TBinding Op
notEqualOp = define "notEqualOp" $
  doc "Inequality operator (!=)" $
  Serialization.op @@ string "!=" @@ int32 11 @@ Ast.associativityLeft

strictEqualOp :: TBinding Op
strictEqualOp = define "strictEqualOp" $
  doc "Strict equality operator (===)" $
  Serialization.op @@ string "===" @@ int32 11 @@ Ast.associativityLeft

strictNotEqualOp :: TBinding Op
strictNotEqualOp = define "strictNotEqualOp" $
  doc "Strict inequality operator (!==)" $
  Serialization.op @@ string "!==" @@ int32 11 @@ Ast.associativityLeft

bitwiseAndOp :: TBinding Op
bitwiseAndOp = define "bitwiseAndOp" $
  doc "Bitwise AND operator (&)" $
  Serialization.op @@ string "&" @@ int32 10 @@ Ast.associativityLeft

bitwiseXorOp :: TBinding Op
bitwiseXorOp = define "bitwiseXorOp" $
  doc "Bitwise XOR operator (^)" $
  Serialization.op @@ string "^" @@ int32 9 @@ Ast.associativityLeft

bitwiseOrOp :: TBinding Op
bitwiseOrOp = define "bitwiseOrOp" $
  doc "Bitwise OR operator (|)" $
  Serialization.op @@ string "|" @@ int32 8 @@ Ast.associativityLeft

logicalAndOp :: TBinding Op
logicalAndOp = define "logicalAndOp" $
  doc "Logical AND operator (&&)" $
  Serialization.op @@ string "&&" @@ int32 6 @@ Ast.associativityLeft

logicalOrOp :: TBinding Op
logicalOrOp = define "logicalOrOp" $
  doc "Logical OR operator (||)" $
  Serialization.op @@ string "||" @@ int32 5 @@ Ast.associativityLeft

nullishCoalescingOp :: TBinding Op
nullishCoalescingOp = define "nullishCoalescingOp" $
  doc "Nullish coalescing operator (??)" $
  Serialization.op @@ string "??" @@ int32 4 @@ Ast.associativityLeft

ternaryOp :: TBinding Op
ternaryOp = define "ternaryOp" $
  doc "Ternary operator (?:) - represents the ? part" $
  Serialization.op @@ string "?" @@ int32 3 @@ Ast.associativityRight

assignOp :: TBinding Op
assignOp = define "assignOp" $
  doc "Assignment operator (=)" $
  Serialization.op @@ string "=" @@ int32 2 @@ Ast.associativityRight

arrowOp :: TBinding Op
arrowOp = define "arrowOp" $
  doc "Arrow function operator (=>)" $
  Serialization.op @@ string "=>" @@ int32 2 @@ Ast.associativityRight

commaOp :: TBinding Op
commaOp = define "commaOp" $
  doc "Comma operator (,)" $
  Serialization.op @@ string "," @@ int32 1 @@ Ast.associativityLeft

defineOp :: TBinding Op
defineOp = define "defineOp" $
  doc "Definition operator (= in const x = ...)" $
  Serialization.op @@ string "=" @@ int32 0 @@ Ast.associativityNone

colonOp :: TBinding Op
colonOp = define "colonOp" $
  doc "Type annotation colon (:)" $
  Serialization.op @@ string ":" @@ int32 0 @@ Ast.associativityNone
