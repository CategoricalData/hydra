-- | AST operators for Rust serialization.
-- Defines operator precedence and associativity based on:
-- https://doc.rust-lang.org/reference/expressions.html#expression-precedence

module Hydra.Ext.Sources.Rust.Operators where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (orOp)
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
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules   as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple    as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations     as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking        as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util    as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting      as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars        as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference       as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages       as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical         as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals        as Literals
import qualified Hydra.Sources.Kernel.Terms.Monads          as Monads
import qualified Hydra.Sources.Kernel.Terms.Names           as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction       as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect         as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting       as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas         as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization   as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors  as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core       as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph      as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta       as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing     as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting         as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution    as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan          as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates       as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification     as Unification
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
ns = Namespace "hydra.ext.rust.operators"

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
