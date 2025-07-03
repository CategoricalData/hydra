module Hydra.Sources.Tier3.Ext.Haskell.Operators where

-- Standard Tier-3 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors              as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Compute                    as Compute
import qualified Hydra.Dsl.Core                       as Core
import qualified Hydra.Dsl.Graph                      as Graph
import qualified Hydra.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Lib.Equality               as Equality
import qualified Hydra.Dsl.Lib.Flows                  as Flows
import qualified Hydra.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Lib.Literals               as Literals
import qualified Hydra.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Lib.Math                   as Math
import qualified Hydra.Dsl.Lib.Optionals              as Optionals
import           Hydra.Dsl.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Lib.Strings                as Strings
import qualified Hydra.Dsl.Mantle                     as Mantle
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.TTerms                     as TTerms
import qualified Hydra.Dsl.TTypes                     as TTypes
import qualified Hydra.Dsl.Terms                      as Terms
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Types                      as Types
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Sources.Tier1.All              as Tier1
import qualified Hydra.Sources.Tier1.Constants        as Constants
import qualified Hydra.Sources.Tier1.Decode           as Decode
import qualified Hydra.Sources.Tier1.Encode.Core      as EncodeCore
import qualified Hydra.Sources.Tier1.Formatting       as Formatting
import qualified Hydra.Sources.Tier1.Functions        as Functions
import qualified Hydra.Sources.Tier1.Literals         as Literals
import qualified Hydra.Sources.Tier1.Messages         as Messages
import qualified Hydra.Sources.Tier1.Strip            as Strip
import qualified Hydra.Sources.Tier2.AdapterUtils     as AdapterUtils
import qualified Hydra.Sources.Tier2.Adapters         as Adapters
import qualified Hydra.Sources.Tier2.Annotations      as Annotations
import qualified Hydra.Sources.Tier2.Arity            as Arity
import qualified Hydra.Sources.Tier2.CoreLanguage     as CoreLanguage
import qualified Hydra.Sources.Tier2.Decode.Core      as DecodeCore
import qualified Hydra.Sources.Tier2.Describe.Core    as DescribeCore
import qualified Hydra.Sources.Tier2.Errors           as Errors
import qualified Hydra.Sources.Tier2.Extract.Core     as ExtractCore
import qualified Hydra.Sources.Tier2.Monads           as Monads
import qualified Hydra.Sources.Tier2.GrammarToModule  as GrammarToModule
import qualified Hydra.Sources.Tier2.Inference        as Inference
import qualified Hydra.Sources.Tier2.Lexical          as Lexical
import qualified Hydra.Sources.Tier2.LiteralAdapters  as LiteralAdapters
import qualified Hydra.Sources.Tier2.Qnames           as Qnames
import qualified Hydra.Sources.Tier2.Reduction        as Reduction
import qualified Hydra.Sources.Tier2.Rewriting        as Rewriting
import qualified Hydra.Sources.Tier2.Schemas          as Schemas
import qualified Hydra.Sources.Tier2.Serialization    as Serialization
import qualified Hydra.Sources.Tier2.Show.Accessors   as ShowAccessors
import qualified Hydra.Sources.Tier2.Show.Core        as ShowCore
import qualified Hydra.Sources.Tier2.Sorting          as Sorting
import qualified Hydra.Sources.Tier2.Substitution     as Substitution
import qualified Hydra.Sources.Tier2.Tarjan           as Tarjan
import qualified Hydra.Sources.Tier2.Templating       as Templating
import qualified Hydra.Sources.Tier2.TermAdapters     as TermAdapters
import qualified Hydra.Sources.Tier2.Unification      as Unification
import qualified Hydra.Sources.Tier2.Variants         as Variants
import qualified Data.Int                             as I
import qualified Data.List                            as L
import qualified Data.Map                             as M
import qualified Data.Set                             as S
import qualified Data.Maybe                           as Y

import Hydra.Ast


haskellOperatorsDefinition :: String -> TTerm a -> TElement a
haskellOperatorsDefinition = definitionInModule haskellOperatorsModule

haskellOperatorsModule :: Module
haskellOperatorsModule = Module ns elements
    [Serialization.hydraSerializationModule]
    [Tier1.hydraAstModule, Tier1.hydraGraphModule] $
    Just "AST operators for Haskell"
  where
    ns = Namespace "hydra.ext.haskell.operators"
    elements = [
      el andOpDef,
      el apOpDef,
      el appOpDef,
      el applyOpDef,
      el arrowOpDef,
      el assertOpDef,
      el bindOpDef,
      el caseOpDef,
      el composeOpDef,
      el concatOpDef,
      el consOpDef,
      el defineOpDef,
      el diamondOpDef,
      el divOpDef,
      el divideOpDef,
      el elemOpDef,
      el equalOpDef,
      el fmapOpDef,
      el gtOpDef,
      el gteOpDef,
      el indexOpDef,
      el lambdaOpDef,
      el ltOpDef,
      el lteOpDef,
      el minusOpDef,
      el modOpDef,
      el multOpDef,
      el neqOpDef,
      el notElemOpDef,
      el orOpDef,
      el plusOpDef,
      el quotOpDef,
      el remOpDef,
      el typeOpDef]

andOpDef :: TElement Op
andOpDef = haskellOperatorsDefinition "andOp" $
  ref Serialization.opDef @@ string "&&" @@ int32 3 @@ Ast.associativityRight

apOpDef :: TElement Op
apOpDef = haskellOperatorsDefinition "apOp" $
  ref Serialization.opDef @@ string "<*>" @@ int32 4 @@ Ast.associativityLeft

appOpDef :: TElement Op
appOpDef = haskellOperatorsDefinition "appOp" $
  doc "No source" $
  Ast.op
    (Ast.symbol $ string "")
    (Ast.padding Ast.wsNone Ast.wsSpace)
    (Ast.precedence $ int32 0)
    Ast.associativityLeft

applyOpDef :: TElement Op
applyOpDef = haskellOperatorsDefinition "applyOp" $
  ref Serialization.opDef @@ string "$" @@ int32 0 @@ Ast.associativityRight

arrowOpDef :: TElement Op
arrowOpDef = haskellOperatorsDefinition "arrowOp" $
  ref Serialization.opDef @@ string "->" @@ (Math.neg $ int32 1) @@ Ast.associativityRight

assertOpDef :: TElement Op
assertOpDef = haskellOperatorsDefinition "assertOp" $
  doc "No source" $
  ref Serialization.opDef @@ string "=>" @@ int32 0 @@ Ast.associativityNone

bindOpDef :: TElement Op
bindOpDef = haskellOperatorsDefinition "bindOp" $
  ref Serialization.opDef @@ string ">>=" @@ int32 1 @@ Ast.associativityLeft

caseOpDef :: TElement Op
caseOpDef = haskellOperatorsDefinition "caseOp" $
  doc "No source" $
  ref Serialization.opDef @@ string "->" @@ int32 0 @@ Ast.associativityNone

composeOpDef :: TElement Op
composeOpDef = haskellOperatorsDefinition "composeOp" $
  ref Serialization.opDef @@ string "." @@ int32 9 @@ Ast.associativityLeft

concatOpDef :: TElement Op
concatOpDef = haskellOperatorsDefinition "concatOp" $
  ref Serialization.opDef @@ string "++" @@ int32 5 @@ Ast.associativityRight

consOpDef :: TElement Op
consOpDef = haskellOperatorsDefinition "consOp" $
  ref Serialization.opDef @@ string ":" @@ int32 5 @@ Ast.associativityRight

defineOpDef :: TElement Op
defineOpDef = haskellOperatorsDefinition "defineOp" $
  doc "No source" $
  ref Serialization.opDef @@ string "=" @@ int32 0 @@ Ast.associativityNone

diamondOpDef :: TElement Op
diamondOpDef = haskellOperatorsDefinition "diamondOp" $
  ref Serialization.opDef @@ string "<>" @@ int32 6 @@ Ast.associativityRight

divOpDef :: TElement Op
divOpDef = haskellOperatorsDefinition "divOp" $
  ref Serialization.opDef @@ string "`div`" @@ int32 7 @@ Ast.associativityLeft

divideOpDef :: TElement Op
divideOpDef = haskellOperatorsDefinition "divideOp" $
  ref Serialization.opDef @@ string "/" @@ int32 7 @@ Ast.associativityLeft

elemOpDef :: TElement Op
elemOpDef = haskellOperatorsDefinition "elemOp" $
  ref Serialization.opDef @@ string "`elem`" @@ int32 4 @@ Ast.associativityNone

equalOpDef :: TElement Op
equalOpDef = haskellOperatorsDefinition "equalOp" $
  ref Serialization.opDef @@ string "==" @@ int32 4 @@ Ast.associativityNone

fmapOpDef :: TElement Op
fmapOpDef = haskellOperatorsDefinition "fmapOp" $
  ref Serialization.opDef @@ string "<$>" @@ int32 4 @@ Ast.associativityLeft

gtOpDef :: TElement Op
gtOpDef = haskellOperatorsDefinition "gtOp" $
  ref Serialization.opDef @@ string ">" @@ int32 4 @@ Ast.associativityNone

gteOpDef :: TElement Op
gteOpDef = haskellOperatorsDefinition "gteOp" $
  ref Serialization.opDef @@ string ">=" @@ int32 4 @@ Ast.associativityNone

indexOpDef :: TElement Op
indexOpDef = haskellOperatorsDefinition "indexOp" $
  ref Serialization.opDef @@ string "!!" @@ int32 9 @@ Ast.associativityLeft

lambdaOpDef :: TElement Op
lambdaOpDef = haskellOperatorsDefinition "lambdaOp" $
  doc "No source" $
  ref Serialization.opDef @@ string "->" @@ (Math.neg $ int32 1) @@ Ast.associativityRight

ltOpDef :: TElement Op
ltOpDef = haskellOperatorsDefinition "ltOp" $
  ref Serialization.opDef @@ string "<" @@ int32 4 @@ Ast.associativityNone

lteOpDef :: TElement Op
lteOpDef = haskellOperatorsDefinition "lteOp" $
  ref Serialization.opDef @@ string ">=" @@ int32 4 @@ Ast.associativityNone

minusOpDef :: TElement Op
minusOpDef = haskellOperatorsDefinition "minusOp" $
  doc "Originally: associativityLeft" $
  ref Serialization.opDef @@ string "-" @@ int32 6 @@ Ast.associativityBoth

modOpDef :: TElement Op
modOpDef = haskellOperatorsDefinition "modOp" $
  ref Serialization.opDef @@ string "`mod`" @@ int32 7 @@ Ast.associativityLeft

multOpDef :: TElement Op
multOpDef = haskellOperatorsDefinition "multOp" $
  doc "Originally: associativityLeft" $
  ref Serialization.opDef @@ string "*" @@ int32 7 @@ Ast.associativityBoth

neqOpDef :: TElement Op
neqOpDef = haskellOperatorsDefinition "neqOp" $
  ref Serialization.opDef @@ string "/=" @@ int32 4 @@ Ast.associativityNone

notElemOpDef :: TElement Op
notElemOpDef = haskellOperatorsDefinition "notElemOp" $
  ref Serialization.opDef @@ string "`notElem`" @@ int32 4 @@ Ast.associativityNone

orOpDef :: TElement Op
orOpDef = haskellOperatorsDefinition "orOp" $
  ref Serialization.opDef @@ string "||" @@ int32 2 @@ Ast.associativityRight

plusOpDef :: TElement Op
plusOpDef = haskellOperatorsDefinition "plusOp" $
  doc "Originally: associativityLeft" $
  ref Serialization.opDef @@ string "+" @@ int32 6 @@ Ast.associativityBoth

quotOpDef :: TElement Op
quotOpDef = haskellOperatorsDefinition "quotOp" $
  ref Serialization.opDef @@ string "`quot`" @@ int32 7 @@ Ast.associativityLeft

remOpDef :: TElement Op
remOpDef = haskellOperatorsDefinition "remOp" $
  ref Serialization.opDef @@ string "`rem`" @@ int32 7 @@ Ast.associativityLeft

typeOpDef :: TElement Op
typeOpDef = haskellOperatorsDefinition "typeOp" $
  doc "No source" $
  ref Serialization.opDef @@ string "::" @@ int32 0 @@ Ast.associativityNone
