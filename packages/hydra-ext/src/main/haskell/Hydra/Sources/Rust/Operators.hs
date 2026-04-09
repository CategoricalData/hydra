-- | AST operators for Rust serialization.
-- Defines operator precedence and associativity based on:
-- https://doc.rust-lang.org/reference/expressions.html#expression-precedence

module Hydra.Sources.Rust.Operators where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (orOp)
import Hydra.Ast (Op)
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Ast                        as Ast
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.rust.operators"

module_ :: Module
module_ = Module ns elements
    [Serialization.ns]
    KernelTypes.kernelTypesNamespaces $
    Just "AST operators for Rust serialization"
  where
    elements = [
      -- Assignment operators (lowest precedence)
      toBinding assignOp,
      toBinding addAssignOp,
      toBinding subAssignOp,
      toBinding mulAssignOp,
      toBinding divAssignOp,
      toBinding remAssignOp,
      toBinding bitAndAssignOp,
      toBinding bitOrAssignOp,
      toBinding bitXorAssignOp,
      toBinding shlAssignOp,
      toBinding shrAssignOp,
      -- Range operators
      toBinding rangeOp,
      toBinding rangeInclusiveOp,
      -- Logical operators
      toBinding orOp,
      toBinding andOp,
      -- Comparison operators
      toBinding eqOp,
      toBinding neOp,
      toBinding ltOp,
      toBinding leOp,
      toBinding gtOp,
      toBinding geOp,
      -- Bitwise operators
      toBinding bitOrOp,
      toBinding bitXorOp,
      toBinding bitAndOp,
      -- Shift operators
      toBinding shlOp,
      toBinding shrOp,
      -- Arithmetic operators
      toBinding addOp,
      toBinding subOp,
      toBinding mulOp,
      toBinding divOp,
      toBinding remOp,
      -- Cast and type ascription
      toBinding asOp,
      toBinding colonOp,
      -- Unary operators (highest precedence)
      toBinding negOp,
      toBinding notOp,
      toBinding derefOp,
      toBinding refOp,
      -- Other operators
      toBinding appOp,
      toBinding fieldOp,
      toBinding methodOp,
      toBinding arrowOp,
      toBinding fatArrowOp,
      toBinding doubleColonOp,
      toBinding colonColonOp]

-- =============================================================================
-- Assignment operators (precedence 1, right associative)
-- =============================================================================

assignOp :: TBinding Op
assignOp = define "assignOp" $
  doc "Assignment operator (=)" $
  Serialization.op @@ string "=" @@ int32 1 @@ Ast.associativityRight

addAssignOp :: TBinding Op
addAssignOp = define "addAssignOp" $
  doc "Add-assign operator (+=)" $
  Serialization.op @@ string "+=" @@ int32 1 @@ Ast.associativityRight

subAssignOp :: TBinding Op
subAssignOp = define "subAssignOp" $
  doc "Sub-assign operator (-=)" $
  Serialization.op @@ string "-=" @@ int32 1 @@ Ast.associativityRight

mulAssignOp :: TBinding Op
mulAssignOp = define "mulAssignOp" $
  doc "Mul-assign operator (*=)" $
  Serialization.op @@ string "*=" @@ int32 1 @@ Ast.associativityRight

divAssignOp :: TBinding Op
divAssignOp = define "divAssignOp" $
  doc "Div-assign operator (/=)" $
  Serialization.op @@ string "/=" @@ int32 1 @@ Ast.associativityRight

remAssignOp :: TBinding Op
remAssignOp = define "remAssignOp" $
  doc "Rem-assign operator (%=)" $
  Serialization.op @@ string "%=" @@ int32 1 @@ Ast.associativityRight

bitAndAssignOp :: TBinding Op
bitAndAssignOp = define "bitAndAssignOp" $
  doc "Bitwise and-assign operator (&=)" $
  Serialization.op @@ string "&=" @@ int32 1 @@ Ast.associativityRight

bitOrAssignOp :: TBinding Op
bitOrAssignOp = define "bitOrAssignOp" $
  doc "Bitwise or-assign operator (|=)" $
  Serialization.op @@ string "|=" @@ int32 1 @@ Ast.associativityRight

bitXorAssignOp :: TBinding Op
bitXorAssignOp = define "bitXorAssignOp" $
  doc "Bitwise xor-assign operator (^=)" $
  Serialization.op @@ string "^=" @@ int32 1 @@ Ast.associativityRight

shlAssignOp :: TBinding Op
shlAssignOp = define "shlAssignOp" $
  doc "Shift-left assign operator (<<=)" $
  Serialization.op @@ string "<<=" @@ int32 1 @@ Ast.associativityRight

shrAssignOp :: TBinding Op
shrAssignOp = define "shrAssignOp" $
  doc "Shift-right assign operator (>>=)" $
  Serialization.op @@ string ">>=" @@ int32 1 @@ Ast.associativityRight

-- =============================================================================
-- Range operators (precedence 2, neither associative)
-- =============================================================================

rangeOp :: TBinding Op
rangeOp = define "rangeOp" $
  doc "Range operator (..)" $
  Serialization.op @@ string ".." @@ int32 2 @@ Ast.associativityNone

rangeInclusiveOp :: TBinding Op
rangeInclusiveOp = define "rangeInclusiveOp" $
  doc "Inclusive range operator (..=)" $
  Serialization.op @@ string "..=" @@ int32 2 @@ Ast.associativityNone

-- =============================================================================
-- Logical operators
-- =============================================================================

orOp :: TBinding Op
orOp = define "orOp" $
  doc "Logical OR operator (||)" $
  Serialization.op @@ string "||" @@ int32 3 @@ Ast.associativityLeft

andOp :: TBinding Op
andOp = define "andOp" $
  doc "Logical AND operator (&&)" $
  Serialization.op @@ string "&&" @@ int32 4 @@ Ast.associativityLeft

-- =============================================================================
-- Comparison operators (precedence 5, require parentheses for chaining)
-- =============================================================================

eqOp :: TBinding Op
eqOp = define "eqOp" $
  doc "Equality operator (==)" $
  Serialization.op @@ string "==" @@ int32 5 @@ Ast.associativityNone

neOp :: TBinding Op
neOp = define "neOp" $
  doc "Not-equal operator (!=)" $
  Serialization.op @@ string "!=" @@ int32 5 @@ Ast.associativityNone

ltOp :: TBinding Op
ltOp = define "ltOp" $
  doc "Less-than operator (<)" $
  Serialization.op @@ string "<" @@ int32 5 @@ Ast.associativityNone

leOp :: TBinding Op
leOp = define "leOp" $
  doc "Less-than-or-equal operator (<=)" $
  Serialization.op @@ string "<=" @@ int32 5 @@ Ast.associativityNone

gtOp :: TBinding Op
gtOp = define "gtOp" $
  doc "Greater-than operator (>)" $
  Serialization.op @@ string ">" @@ int32 5 @@ Ast.associativityNone

geOp :: TBinding Op
geOp = define "geOp" $
  doc "Greater-than-or-equal operator (>=)" $
  Serialization.op @@ string ">=" @@ int32 5 @@ Ast.associativityNone

-- =============================================================================
-- Bitwise operators
-- =============================================================================

bitOrOp :: TBinding Op
bitOrOp = define "bitOrOp" $
  doc "Bitwise OR operator (|)" $
  Serialization.op @@ string "|" @@ int32 6 @@ Ast.associativityLeft

bitXorOp :: TBinding Op
bitXorOp = define "bitXorOp" $
  doc "Bitwise XOR operator (^)" $
  Serialization.op @@ string "^" @@ int32 7 @@ Ast.associativityLeft

bitAndOp :: TBinding Op
bitAndOp = define "bitAndOp" $
  doc "Bitwise AND operator (&)" $
  Serialization.op @@ string "&" @@ int32 8 @@ Ast.associativityLeft

-- =============================================================================
-- Shift operators (precedence 9)
-- =============================================================================

shlOp :: TBinding Op
shlOp = define "shlOp" $
  doc "Shift-left operator (<<)" $
  Serialization.op @@ string "<<" @@ int32 9 @@ Ast.associativityLeft

shrOp :: TBinding Op
shrOp = define "shrOp" $
  doc "Shift-right operator (>>)" $
  Serialization.op @@ string ">>" @@ int32 9 @@ Ast.associativityLeft

-- =============================================================================
-- Arithmetic operators
-- =============================================================================

addOp :: TBinding Op
addOp = define "addOp" $
  doc "Addition operator (+)" $
  Serialization.op @@ string "+" @@ int32 10 @@ Ast.associativityLeft

subOp :: TBinding Op
subOp = define "subOp" $
  doc "Subtraction operator (-)" $
  Serialization.op @@ string "-" @@ int32 10 @@ Ast.associativityLeft

mulOp :: TBinding Op
mulOp = define "mulOp" $
  doc "Multiplication operator (*)" $
  Serialization.op @@ string "*" @@ int32 11 @@ Ast.associativityLeft

divOp :: TBinding Op
divOp = define "divOp" $
  doc "Division operator (/)" $
  Serialization.op @@ string "/" @@ int32 11 @@ Ast.associativityLeft

remOp :: TBinding Op
remOp = define "remOp" $
  doc "Remainder operator (%)" $
  Serialization.op @@ string "%" @@ int32 11 @@ Ast.associativityLeft

-- =============================================================================
-- Type cast and ascription operators (precedence 12)
-- =============================================================================

asOp :: TBinding Op
asOp = define "asOp" $
  doc "Type cast operator (as)" $
  Serialization.op @@ string "as" @@ int32 12 @@ Ast.associativityLeft

colonOp :: TBinding Op
colonOp = define "colonOp" $
  doc "Type ascription operator (:)" $
  Serialization.op @@ string ":" @@ int32 12 @@ Ast.associativityLeft

-- =============================================================================
-- Unary operators (highest precedence 13)
-- =============================================================================

negOp :: TBinding Op
negOp = define "negOp" $
  doc "Unary negation operator (-)" $
  Ast.op
    (Ast.symbol $ string "-")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 13)
    Ast.associativityNone

notOp :: TBinding Op
notOp = define "notOp" $
  doc "Unary logical not operator (!)" $
  Ast.op
    (Ast.symbol $ string "!")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 13)
    Ast.associativityNone

derefOp :: TBinding Op
derefOp = define "derefOp" $
  doc "Dereference operator (*)" $
  Ast.op
    (Ast.symbol $ string "*")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 13)
    Ast.associativityNone

refOp :: TBinding Op
refOp = define "refOp" $
  doc "Reference operator (&)" $
  Ast.op
    (Ast.symbol $ string "&")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 13)
    Ast.associativityNone

-- =============================================================================
-- Other operators
-- =============================================================================

appOp :: TBinding Op
appOp = define "appOp" $
  doc "Function application operator (whitespace)" $
  Ast.op
    (Ast.symbol $ string "")
    (Ast.padding Ast.wsNone Ast.wsSpace)
    (Ast.precedence $ int32 0)
    Ast.associativityLeft

fieldOp :: TBinding Op
fieldOp = define "fieldOp" $
  doc "Field access operator (.)" $
  Ast.op
    (Ast.symbol $ string ".")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 14)
    Ast.associativityLeft

methodOp :: TBinding Op
methodOp = define "methodOp" $
  doc "Method call operator (.)" $
  Ast.op
    (Ast.symbol $ string ".")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 14)
    Ast.associativityLeft

arrowOp :: TBinding Op
arrowOp = define "arrowOp" $
  doc "Return type arrow (->)" $
  Serialization.op @@ string "->" @@ int32 0 @@ Ast.associativityRight

fatArrowOp :: TBinding Op
fatArrowOp = define "fatArrowOp" $
  doc "Match arm arrow (=>)" $
  Serialization.op @@ string "=>" @@ int32 0 @@ Ast.associativityNone

doubleColonOp :: TBinding Op
doubleColonOp = define "doubleColonOp" $
  doc "Path separator (::)" $
  Ast.op
    (Ast.symbol $ string "::")
    (Ast.padding Ast.wsNone Ast.wsNone)
    (Ast.precedence $ int32 15)
    Ast.associativityLeft

colonColonOp :: TBinding Op
colonColonOp = define "colonColonOp" $
  doc "Type annotation (::) for let statements" $
  Serialization.op @@ string ":" @@ int32 0 @@ Ast.associativityNone
