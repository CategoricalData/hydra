-- | AST operators for JavaScript, defining precedence and associativity.
--
-- Operator precedence is based on:
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_precedence

module Hydra.Ext.Sources.JavaScript.Operators where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants  as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import Hydra.Ast


define :: String -> TTerm a -> TTermDefinition a
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
      toDefinition appOp,

      -- Member access
      toDefinition memberOp,
      toDefinition optionalChainOp,

      -- Arithmetic operators
      toDefinition exponentiateOp,
      toDefinition multiplyOp,
      toDefinition divideOp,
      toDefinition moduloOp,
      toDefinition addOp,
      toDefinition subtractOp,

      -- Comparison operators
      toDefinition lessThanOp,
      toDefinition lessThanOrEqualOp,
      toDefinition greaterThanOp,
      toDefinition greaterThanOrEqualOp,
      toDefinition inOp,
      toDefinition instanceOfOp,
      toDefinition equalOp,
      toDefinition notEqualOp,
      toDefinition strictEqualOp,
      toDefinition strictNotEqualOp,

      -- Bitwise operators
      toDefinition bitwiseAndOp,
      toDefinition bitwiseXorOp,
      toDefinition bitwiseOrOp,
      toDefinition leftShiftOp,
      toDefinition rightShiftOp,
      toDefinition unsignedRightShiftOp,

      -- Logical operators
      toDefinition logicalAndOp,
      toDefinition logicalOrOp,
      toDefinition nullishCoalescingOp,

      -- Assignment operators
      toDefinition assignOp,

      -- Arrow function
      toDefinition arrowOp,

      -- Ternary
      toDefinition ternaryOp,

      -- Comma
      toDefinition commaOp,

      -- Definition (for const x = ...)
      toDefinition defineOp,

      -- Type annotation (for JSDoc/TypeScript)
      toDefinition colonOp]

-- Note: JavaScript precedence levels (MDN):
-- 1 = comma, 2 = assignment, 3 = conditional, 4 = nullish coalescing,
-- 5 = logical or, 6 = logical and, 7-10 = bitwise, 11 = equality,
-- 12 = relational, 13 = shift, 14 = additive, 15 = multiplicative,
-- 16 = exponentiation, 17 = unary, 18 = update, 19 = new, 20 = member access

appOp :: TTermDefinition Op
appOp = define "appOp" $
  doc "Function application (whitespace)" $
  Ast.op
    (Ast.symbol $ string "")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 20)
    Ast.associativityLeft

memberOp :: TTermDefinition Op
memberOp = define "memberOp" $
  doc "Member access operator (.)" $
  Ast.op
    (Ast.symbol $ string ".")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 20)
    Ast.associativityLeft

optionalChainOp :: TTermDefinition Op
optionalChainOp = define "optionalChainOp" $
  doc "Optional chaining operator (?.)" $
  Ast.op
    (Ast.symbol $ string "?.")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 20)
    Ast.associativityLeft

exponentiateOp :: TTermDefinition Op
exponentiateOp = define "exponentiateOp" $
  doc "Exponentiation operator (**)" $
  Serialization.op @@ string "**" @@ int32 16 @@ Ast.associativityRight

multiplyOp :: TTermDefinition Op
multiplyOp = define "multiplyOp" $
  doc "Multiplication operator (*)" $
  Serialization.op @@ string "*" @@ int32 15 @@ Ast.associativityLeft

divideOp :: TTermDefinition Op
divideOp = define "divideOp" $
  doc "Division operator (/)" $
  Serialization.op @@ string "/" @@ int32 15 @@ Ast.associativityLeft

moduloOp :: TTermDefinition Op
moduloOp = define "moduloOp" $
  doc "Modulo operator (%)" $
  Serialization.op @@ string "%" @@ int32 15 @@ Ast.associativityLeft

addOp :: TTermDefinition Op
addOp = define "addOp" $
  doc "Addition operator (+)" $
  Serialization.op @@ string "+" @@ int32 14 @@ Ast.associativityLeft

subtractOp :: TTermDefinition Op
subtractOp = define "subtractOp" $
  doc "Subtraction operator (-)" $
  Serialization.op @@ string "-" @@ int32 14 @@ Ast.associativityLeft

leftShiftOp :: TTermDefinition Op
leftShiftOp = define "leftShiftOp" $
  doc "Left shift operator (<<)" $
  Serialization.op @@ string "<<" @@ int32 13 @@ Ast.associativityLeft

rightShiftOp :: TTermDefinition Op
rightShiftOp = define "rightShiftOp" $
  doc "Right shift operator (>>)" $
  Serialization.op @@ string ">>" @@ int32 13 @@ Ast.associativityLeft

unsignedRightShiftOp :: TTermDefinition Op
unsignedRightShiftOp = define "unsignedRightShiftOp" $
  doc "Unsigned right shift operator (>>>)" $
  Serialization.op @@ string ">>>" @@ int32 13 @@ Ast.associativityLeft

lessThanOp :: TTermDefinition Op
lessThanOp = define "lessThanOp" $
  doc "Less than operator (<)" $
  Serialization.op @@ string "<" @@ int32 12 @@ Ast.associativityLeft

lessThanOrEqualOp :: TTermDefinition Op
lessThanOrEqualOp = define "lessThanOrEqualOp" $
  doc "Less than or equal operator (<=)" $
  Serialization.op @@ string "<=" @@ int32 12 @@ Ast.associativityLeft

greaterThanOp :: TTermDefinition Op
greaterThanOp = define "greaterThanOp" $
  doc "Greater than operator (>)" $
  Serialization.op @@ string ">" @@ int32 12 @@ Ast.associativityLeft

greaterThanOrEqualOp :: TTermDefinition Op
greaterThanOrEqualOp = define "greaterThanOrEqualOp" $
  doc "Greater than or equal operator (>=)" $
  Serialization.op @@ string ">=" @@ int32 12 @@ Ast.associativityLeft

inOp :: TTermDefinition Op
inOp = define "inOp" $
  doc "In operator (in)" $
  Serialization.op @@ string "in" @@ int32 12 @@ Ast.associativityLeft

instanceOfOp :: TTermDefinition Op
instanceOfOp = define "instanceOfOp" $
  doc "Instance of operator (instanceof)" $
  Serialization.op @@ string "instanceof" @@ int32 12 @@ Ast.associativityLeft

equalOp :: TTermDefinition Op
equalOp = define "equalOp" $
  doc "Equality operator (==)" $
  Serialization.op @@ string "==" @@ int32 11 @@ Ast.associativityLeft

notEqualOp :: TTermDefinition Op
notEqualOp = define "notEqualOp" $
  doc "Inequality operator (!=)" $
  Serialization.op @@ string "!=" @@ int32 11 @@ Ast.associativityLeft

strictEqualOp :: TTermDefinition Op
strictEqualOp = define "strictEqualOp" $
  doc "Strict equality operator (===)" $
  Serialization.op @@ string "===" @@ int32 11 @@ Ast.associativityLeft

strictNotEqualOp :: TTermDefinition Op
strictNotEqualOp = define "strictNotEqualOp" $
  doc "Strict inequality operator (!==)" $
  Serialization.op @@ string "!==" @@ int32 11 @@ Ast.associativityLeft

bitwiseAndOp :: TTermDefinition Op
bitwiseAndOp = define "bitwiseAndOp" $
  doc "Bitwise AND operator (&)" $
  Serialization.op @@ string "&" @@ int32 10 @@ Ast.associativityLeft

bitwiseXorOp :: TTermDefinition Op
bitwiseXorOp = define "bitwiseXorOp" $
  doc "Bitwise XOR operator (^)" $
  Serialization.op @@ string "^" @@ int32 9 @@ Ast.associativityLeft

bitwiseOrOp :: TTermDefinition Op
bitwiseOrOp = define "bitwiseOrOp" $
  doc "Bitwise OR operator (|)" $
  Serialization.op @@ string "|" @@ int32 8 @@ Ast.associativityLeft

logicalAndOp :: TTermDefinition Op
logicalAndOp = define "logicalAndOp" $
  doc "Logical AND operator (&&)" $
  Serialization.op @@ string "&&" @@ int32 6 @@ Ast.associativityLeft

logicalOrOp :: TTermDefinition Op
logicalOrOp = define "logicalOrOp" $
  doc "Logical OR operator (||)" $
  Serialization.op @@ string "||" @@ int32 5 @@ Ast.associativityLeft

nullishCoalescingOp :: TTermDefinition Op
nullishCoalescingOp = define "nullishCoalescingOp" $
  doc "Nullish coalescing operator (??)" $
  Serialization.op @@ string "??" @@ int32 4 @@ Ast.associativityLeft

ternaryOp :: TTermDefinition Op
ternaryOp = define "ternaryOp" $
  doc "Ternary operator (?:) - represents the ? part" $
  Serialization.op @@ string "?" @@ int32 3 @@ Ast.associativityRight

assignOp :: TTermDefinition Op
assignOp = define "assignOp" $
  doc "Assignment operator (=)" $
  Serialization.op @@ string "=" @@ int32 2 @@ Ast.associativityRight

arrowOp :: TTermDefinition Op
arrowOp = define "arrowOp" $
  doc "Arrow function operator (=>)" $
  Serialization.op @@ string "=>" @@ int32 2 @@ Ast.associativityRight

commaOp :: TTermDefinition Op
commaOp = define "commaOp" $
  doc "Comma operator (,)" $
  Serialization.op @@ string "," @@ int32 1 @@ Ast.associativityLeft

defineOp :: TTermDefinition Op
defineOp = define "defineOp" $
  doc "Definition operator (= in const x = ...)" $
  Serialization.op @@ string "=" @@ int32 0 @@ Ast.associativityNone

colonOp :: TTermDefinition Op
colonOp = define "colonOp" $
  doc "Type annotation colon (:)" $
  Serialization.op @@ string ":" @@ int32 0 @@ Ast.associativityNone
