module Hydra.Sources.Haskell.Operators where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (orOp)
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Meta.Accessors                  as Accessors
import qualified Hydra.Dsl.Meta.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Compute                    as Compute
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Meta.Json                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows                  as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Meta.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Meta.Typing                     as Typing
import qualified Hydra.Dsl.Meta.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules  as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple   as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms    as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils    as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Monads         as Monads
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan         as Tarjan
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


haskellOperatorsDefinition :: String -> TTerm a -> TBinding a
haskellOperatorsDefinition = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.haskell.operators"

module_ :: Module
module_ = Module ns elements
    [Serialization.ns]
    KernelTypes.kernelTypesNamespaces $
    Just "AST operators for Haskell"
  where
    elements = [
      toBinding andOp,
      toBinding apOp,
      toBinding appOp,
      toBinding applyOp,
      toBinding arrowOp,
      toBinding assertOp,
      toBinding bindOp,
      toBinding caseOp,
      toBinding composeOp,
      toBinding concatOp,
      toBinding consOp,
      toBinding defineOp,
      toBinding diamondOp,
      toBinding divOp,
      toBinding divideOp,
      toBinding elemOp,
      toBinding equalOp,
      toBinding fmapOp,
      toBinding gtOp,
      toBinding gteOp,
      toBinding indexOp,
      toBinding lambdaOp,
      toBinding ltOp,
      toBinding lteOp,
      toBinding minusOp,
      toBinding modOp,
      toBinding multOp,
      toBinding neqOp,
      toBinding notElemOp,
      toBinding orOp,
      toBinding plusOp,
      toBinding quotOp,
      toBinding remOp,
      toBinding typeOp]

andOp :: TBinding Op
andOp = haskellOperatorsDefinition "andOp" $
  doc "Logical AND operator (&&)" $
  Serialization.op @@ string "&&" @@ int32 3 @@ Ast.associativityRight

apOp :: TBinding Op
apOp = haskellOperatorsDefinition "apOp" $
  doc "Applicative apply operator (<*>)" $
  Serialization.op @@ string "<*>" @@ int32 4 @@ Ast.associativityLeft

appOp :: TBinding Op
appOp = haskellOperatorsDefinition "appOp" $
  doc "Function application operator (whitespace)" $
  Ast.op
    (Ast.symbol $ string "")
    (Ast.padding Ast.wsNone Ast.wsSpace)
    (Ast.precedence $ int32 0)
    Ast.associativityLeft

applyOp :: TBinding Op
applyOp = haskellOperatorsDefinition "applyOp" $
  doc "Low-precedence function application ($)" $
  Serialization.op @@ string "$" @@ int32 0 @@ Ast.associativityRight

arrowOp :: TBinding Op
arrowOp = haskellOperatorsDefinition "arrowOp" $
  doc "Function type arrow (->)" $
  Serialization.op @@ string "->" @@ (Math.negate $ int32 1) @@ Ast.associativityRight

assertOp :: TBinding Op
assertOp = haskellOperatorsDefinition "assertOp" $
  doc "Type class constraint arrow (=>)" $
  Serialization.op @@ string "=>" @@ int32 0 @@ Ast.associativityNone

bindOp :: TBinding Op
bindOp = haskellOperatorsDefinition "bindOp" $
  doc "Monadic bind operator (>>=)" $
  Serialization.op @@ string ">>=" @@ int32 1 @@ Ast.associativityLeft

caseOp :: TBinding Op
caseOp = haskellOperatorsDefinition "caseOp" $
  doc "Case alternative arrow (->)" $
  Serialization.op @@ string "->" @@ int32 0 @@ Ast.associativityNone

composeOp :: TBinding Op
composeOp = haskellOperatorsDefinition "composeOp" $
  doc "Function composition (.)" $
  Serialization.op @@ string "." @@ int32 9 @@ Ast.associativityLeft

concatOp :: TBinding Op
concatOp = haskellOperatorsDefinition "concatOp" $
  doc "List concatenation (++)" $
  Serialization.op @@ string "++" @@ int32 5 @@ Ast.associativityRight

consOp :: TBinding Op
consOp = haskellOperatorsDefinition "consOp" $
  doc "List cons (:)" $
  Serialization.op @@ string ":" @@ int32 5 @@ Ast.associativityRight

defineOp :: TBinding Op
defineOp = haskellOperatorsDefinition "defineOp" $
  doc "Definition operator (=)" $
  Serialization.op @@ string "=" @@ int32 0 @@ Ast.associativityNone

diamondOp :: TBinding Op
diamondOp = haskellOperatorsDefinition "diamondOp" $
  doc "Semigroup append (<>)" $
  Serialization.op @@ string "<>" @@ int32 6 @@ Ast.associativityRight

divOp :: TBinding Op
divOp = haskellOperatorsDefinition "divOp" $
  doc "Integer division (`div`)" $
  Serialization.op @@ string "`div`" @@ int32 7 @@ Ast.associativityLeft

divideOp :: TBinding Op
divideOp = haskellOperatorsDefinition "divideOp" $
  doc "Fractional division (/)" $
  Serialization.op @@ string "/" @@ int32 7 @@ Ast.associativityLeft

elemOp :: TBinding Op
elemOp = haskellOperatorsDefinition "elemOp" $
  doc "List membership (`elem`)" $
  Serialization.op @@ string "`elem`" @@ int32 4 @@ Ast.associativityNone

equalOp :: TBinding Op
equalOp = haskellOperatorsDefinition "equalOp" $
  doc "Equality comparison (==)" $
  Serialization.op @@ string "==" @@ int32 4 @@ Ast.associativityNone

fmapOp :: TBinding Op
fmapOp = haskellOperatorsDefinition "fmapOp" $
  doc "Functor map (<$>)" $
  Serialization.op @@ string "<$>" @@ int32 4 @@ Ast.associativityLeft

gtOp :: TBinding Op
gtOp = haskellOperatorsDefinition "gtOp" $
  doc "Greater than (>)" $
  Serialization.op @@ string ">" @@ int32 4 @@ Ast.associativityNone

gteOp :: TBinding Op
gteOp = haskellOperatorsDefinition "gteOp" $
  doc "Greater than or equal (>=)" $
  Serialization.op @@ string ">=" @@ int32 4 @@ Ast.associativityNone

indexOp :: TBinding Op
indexOp = haskellOperatorsDefinition "indexOp" $
  doc "List indexing (!!)" $
  Serialization.op @@ string "!!" @@ int32 9 @@ Ast.associativityLeft

lambdaOp :: TBinding Op
lambdaOp = haskellOperatorsDefinition "lambdaOp" $
  doc "Lambda body arrow (->)" $
  Serialization.op @@ string "->" @@ (Math.negate $ int32 1) @@ Ast.associativityRight

ltOp :: TBinding Op
ltOp = haskellOperatorsDefinition "ltOp" $
  doc "Less than (<)" $
  Serialization.op @@ string "<" @@ int32 4 @@ Ast.associativityNone

lteOp :: TBinding Op
lteOp = haskellOperatorsDefinition "lteOp" $
  doc "Less than or equal (<=)" $
  Serialization.op @@ string ">=" @@ int32 4 @@ Ast.associativityNone

minusOp :: TBinding Op
minusOp = haskellOperatorsDefinition "minusOp" $
  doc "Subtraction (-). Originally: associativityLeft" $
  Serialization.op @@ string "-" @@ int32 6 @@ Ast.associativityBoth

modOp :: TBinding Op
modOp = haskellOperatorsDefinition "modOp" $
  doc "Modulo (`mod`)" $
  Serialization.op @@ string "`mod`" @@ int32 7 @@ Ast.associativityLeft

multOp :: TBinding Op
multOp = haskellOperatorsDefinition "multOp" $
  doc "Multiplication (*). Originally: associativityLeft" $
  Serialization.op @@ string "*" @@ int32 7 @@ Ast.associativityBoth

neqOp :: TBinding Op
neqOp = haskellOperatorsDefinition "neqOp" $
  doc "Not equal (/=)" $
  Serialization.op @@ string "/=" @@ int32 4 @@ Ast.associativityNone

notElemOp :: TBinding Op
notElemOp = haskellOperatorsDefinition "notElemOp" $
  doc "List non-membership (`notElem`)" $
  Serialization.op @@ string "`notElem`" @@ int32 4 @@ Ast.associativityNone

orOp :: TBinding Op
orOp = haskellOperatorsDefinition "orOp" $
  doc "Logical OR (||)" $
  Serialization.op @@ string "||" @@ int32 2 @@ Ast.associativityRight

plusOp :: TBinding Op
plusOp = haskellOperatorsDefinition "plusOp" $
  doc "Addition (+). Originally: associativityLeft" $
  Serialization.op @@ string "+" @@ int32 6 @@ Ast.associativityBoth

quotOp :: TBinding Op
quotOp = haskellOperatorsDefinition "quotOp" $
  doc "Integer quotient (`quot`)" $
  Serialization.op @@ string "`quot`" @@ int32 7 @@ Ast.associativityLeft

remOp :: TBinding Op
remOp = haskellOperatorsDefinition "remOp" $
  doc "Integer remainder (`rem`)" $
  Serialization.op @@ string "`rem`" @@ int32 7 @@ Ast.associativityLeft

typeOp :: TBinding Op
typeOp = haskellOperatorsDefinition "typeOp" $
  doc "Type annotation (::)" $
  Serialization.op @@ string "::" @@ int32 0 @@ Ast.associativityNone
