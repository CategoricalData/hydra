module Hydra.Sources.Haskell.Operators where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (orOp)
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                  as Paths
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
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Paths as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants      as ShowVariants
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


haskellOperatorsDefinition :: String -> TTerm a -> TTermDefinition a
haskellOperatorsDefinition = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.haskell.operators"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [Serialization.ns],
            moduleTypeDependencies = KernelTypes.kernelTypesNamespaces,
            moduleDescription = Just "AST operators for Haskell"}
  where
    definitions = [
      toDefinition andOp,
      toDefinition apOp,
      toDefinition appOp,
      toDefinition applyOp,
      toDefinition arrowOp,
      toDefinition assertOp,
      toDefinition bindOp,
      toDefinition caseOp,
      toDefinition composeOp,
      toDefinition concatOp,
      toDefinition consOp,
      toDefinition defineOp,
      toDefinition diamondOp,
      toDefinition divOp,
      toDefinition divideOp,
      toDefinition elemOp,
      toDefinition equalOp,
      toDefinition fmapOp,
      toDefinition gtOp,
      toDefinition gteOp,
      toDefinition indexOp,
      toDefinition lambdaOp,
      toDefinition ltOp,
      toDefinition lteOp,
      toDefinition minusOp,
      toDefinition modOp,
      toDefinition multOp,
      toDefinition neqOp,
      toDefinition notElemOp,
      toDefinition orOp,
      toDefinition plusOp,
      toDefinition quotOp,
      toDefinition remOp,
      toDefinition typeOp]

andOp :: TTermDefinition Op
andOp = haskellOperatorsDefinition "andOp" $
  doc "Logical AND operator (&&)" $
  Serialization.op @@ string "&&" @@ int32 3 @@ Ast.associativityRight

apOp :: TTermDefinition Op
apOp = haskellOperatorsDefinition "apOp" $
  doc "Applicative apply operator (<*>)" $
  Serialization.op @@ string "<*>" @@ int32 4 @@ Ast.associativityLeft

appOp :: TTermDefinition Op
appOp = haskellOperatorsDefinition "appOp" $
  doc "Function application operator (whitespace)" $
  Ast.op
    (Ast.symbol $ string "")
    (Ast.padding Ast.wsNone Ast.wsSpace)
    (Ast.precedence $ int32 0)
    Ast.associativityLeft

applyOp :: TTermDefinition Op
applyOp = haskellOperatorsDefinition "applyOp" $
  doc "Low-precedence function application ($)" $
  Serialization.op @@ string "$" @@ int32 0 @@ Ast.associativityRight

arrowOp :: TTermDefinition Op
arrowOp = haskellOperatorsDefinition "arrowOp" $
  doc "Function type arrow (->)" $
  Serialization.op @@ string "->" @@ (Math.negate $ int32 1) @@ Ast.associativityRight

assertOp :: TTermDefinition Op
assertOp = haskellOperatorsDefinition "assertOp" $
  doc "Type class constraint arrow (=>)" $
  Serialization.op @@ string "=>" @@ int32 0 @@ Ast.associativityNone

bindOp :: TTermDefinition Op
bindOp = haskellOperatorsDefinition "bindOp" $
  doc "Monadic bind operator (>>=)" $
  Serialization.op @@ string ">>=" @@ int32 1 @@ Ast.associativityLeft

caseOp :: TTermDefinition Op
caseOp = haskellOperatorsDefinition "caseOp" $
  doc "Case alternative arrow (->)" $
  Serialization.op @@ string "->" @@ int32 0 @@ Ast.associativityNone

composeOp :: TTermDefinition Op
composeOp = haskellOperatorsDefinition "composeOp" $
  doc "Function composition (.)" $
  Serialization.op @@ string "." @@ int32 9 @@ Ast.associativityLeft

concatOp :: TTermDefinition Op
concatOp = haskellOperatorsDefinition "concatOp" $
  doc "List concatenation (++)" $
  Serialization.op @@ string "++" @@ int32 5 @@ Ast.associativityRight

consOp :: TTermDefinition Op
consOp = haskellOperatorsDefinition "consOp" $
  doc "List cons (:)" $
  Serialization.op @@ string ":" @@ int32 5 @@ Ast.associativityRight

defineOp :: TTermDefinition Op
defineOp = haskellOperatorsDefinition "defineOp" $
  doc "Definition operator (=)" $
  Serialization.op @@ string "=" @@ int32 0 @@ Ast.associativityNone

diamondOp :: TTermDefinition Op
diamondOp = haskellOperatorsDefinition "diamondOp" $
  doc "Semigroup append (<>)" $
  Serialization.op @@ string "<>" @@ int32 6 @@ Ast.associativityRight

divOp :: TTermDefinition Op
divOp = haskellOperatorsDefinition "divOp" $
  doc "Integer division (`div`)" $
  Serialization.op @@ string "`div`" @@ int32 7 @@ Ast.associativityLeft

divideOp :: TTermDefinition Op
divideOp = haskellOperatorsDefinition "divideOp" $
  doc "Fractional division (/)" $
  Serialization.op @@ string "/" @@ int32 7 @@ Ast.associativityLeft

elemOp :: TTermDefinition Op
elemOp = haskellOperatorsDefinition "elemOp" $
  doc "List membership (`elem`)" $
  Serialization.op @@ string "`elem`" @@ int32 4 @@ Ast.associativityNone

equalOp :: TTermDefinition Op
equalOp = haskellOperatorsDefinition "equalOp" $
  doc "Equality comparison (==)" $
  Serialization.op @@ string "==" @@ int32 4 @@ Ast.associativityNone

fmapOp :: TTermDefinition Op
fmapOp = haskellOperatorsDefinition "fmapOp" $
  doc "Functor map (<$>)" $
  Serialization.op @@ string "<$>" @@ int32 4 @@ Ast.associativityLeft

gtOp :: TTermDefinition Op
gtOp = haskellOperatorsDefinition "gtOp" $
  doc "Greater than (>)" $
  Serialization.op @@ string ">" @@ int32 4 @@ Ast.associativityNone

gteOp :: TTermDefinition Op
gteOp = haskellOperatorsDefinition "gteOp" $
  doc "Greater than or equal (>=)" $
  Serialization.op @@ string ">=" @@ int32 4 @@ Ast.associativityNone

indexOp :: TTermDefinition Op
indexOp = haskellOperatorsDefinition "indexOp" $
  doc "List indexing (!!)" $
  Serialization.op @@ string "!!" @@ int32 9 @@ Ast.associativityLeft

lambdaOp :: TTermDefinition Op
lambdaOp = haskellOperatorsDefinition "lambdaOp" $
  doc "Lambda body arrow (->)" $
  Serialization.op @@ string "->" @@ (Math.negate $ int32 1) @@ Ast.associativityRight

ltOp :: TTermDefinition Op
ltOp = haskellOperatorsDefinition "ltOp" $
  doc "Less than (<)" $
  Serialization.op @@ string "<" @@ int32 4 @@ Ast.associativityNone

lteOp :: TTermDefinition Op
lteOp = haskellOperatorsDefinition "lteOp" $
  doc "Less than or equal (<=)" $
  Serialization.op @@ string ">=" @@ int32 4 @@ Ast.associativityNone

minusOp :: TTermDefinition Op
minusOp = haskellOperatorsDefinition "minusOp" $
  doc "Subtraction (-). Originally: associativityLeft" $
  Serialization.op @@ string "-" @@ int32 6 @@ Ast.associativityBoth

modOp :: TTermDefinition Op
modOp = haskellOperatorsDefinition "modOp" $
  doc "Modulo (`mod`)" $
  Serialization.op @@ string "`mod`" @@ int32 7 @@ Ast.associativityLeft

multOp :: TTermDefinition Op
multOp = haskellOperatorsDefinition "multOp" $
  doc "Multiplication (*). Originally: associativityLeft" $
  Serialization.op @@ string "*" @@ int32 7 @@ Ast.associativityBoth

neqOp :: TTermDefinition Op
neqOp = haskellOperatorsDefinition "neqOp" $
  doc "Not equal (/=)" $
  Serialization.op @@ string "/=" @@ int32 4 @@ Ast.associativityNone

notElemOp :: TTermDefinition Op
notElemOp = haskellOperatorsDefinition "notElemOp" $
  doc "List non-membership (`notElem`)" $
  Serialization.op @@ string "`notElem`" @@ int32 4 @@ Ast.associativityNone

orOp :: TTermDefinition Op
orOp = haskellOperatorsDefinition "orOp" $
  doc "Logical OR (||)" $
  Serialization.op @@ string "||" @@ int32 2 @@ Ast.associativityRight

plusOp :: TTermDefinition Op
plusOp = haskellOperatorsDefinition "plusOp" $
  doc "Addition (+). Originally: associativityLeft" $
  Serialization.op @@ string "+" @@ int32 6 @@ Ast.associativityBoth

quotOp :: TTermDefinition Op
quotOp = haskellOperatorsDefinition "quotOp" $
  doc "Integer quotient (`quot`)" $
  Serialization.op @@ string "`quot`" @@ int32 7 @@ Ast.associativityLeft

remOp :: TTermDefinition Op
remOp = haskellOperatorsDefinition "remOp" $
  doc "Integer remainder (`rem`)" $
  Serialization.op @@ string "`rem`" @@ int32 7 @@ Ast.associativityLeft

typeOp :: TTermDefinition Op
typeOp = haskellOperatorsDefinition "typeOp" $
  doc "Type annotation (::)" $
  Serialization.op @@ string "::" @@ int32 0 @@ Ast.associativityNone
