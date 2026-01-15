
module Hydra.Sources.Json.Writer where

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
import qualified Hydra.Json.Model as J


jsonSerdeDefinition :: String -> TTerm a -> TBinding a
jsonSerdeDefinition = definitionInModule module_

module_ :: Module
module_ = Module ns elements
    [Serialization.ns]
    KernelTypes.kernelTypesNamespaces $
    Just "JSON serialization functions using the Hydra AST"
  where
    ns = Namespace "hydra.json.writer"
    elements = [
      toBinding colonOp,
      toBinding jsonString,
      toBinding keyValueToExpr,
      toBinding printJson,
      toBinding valueToExpr]

colonOp :: TBinding Op
colonOp = jsonSerdeDefinition "colonOp" $
  doc "The colon operator used to separate keys and values in JSON objects" $
  Ast.op
    (Ast.symbol $ string ":")
    (Ast.padding Ast.wsNone Ast.wsSpace)
    (Ast.precedence $ int32 0)
    Ast.associativityNone

-- | ASCII codepoints for characters that need escaping
quoteCode, backslashCode, newlineCode, returnCode, tabCode :: TTerm Int
quoteCode = int32 34      -- '"'
backslashCode = int32 92  -- '\\'
newlineCode = int32 10    -- '\n'
returnCode = int32 13     -- '\r'
tabCode = int32 9         -- '\t'

jsonString :: TBinding (String -> String)
jsonString = jsonSerdeDefinition "jsonString" $
  doc "Escape and quote a string for JSON output" $
  "s" ~>
  -- escape function takes a codepoint (Int) and returns a String
  "escape" <~ ("c" ~>
    Logic.ifElse (Equality.equal (var "c") quoteCode)
      (string "\\\"")
      (Logic.ifElse (Equality.equal (var "c") backslashCode)
        (string "\\\\")
        (Logic.ifElse (Equality.equal (var "c") newlineCode)
          (string "\\n")
          (Logic.ifElse (Equality.equal (var "c") returnCode)
            (string "\\r")
            (Logic.ifElse (Equality.equal (var "c") tabCode)
              (string "\\t")
              -- Convert single codepoint back to string
              (Strings.fromList (Lists.pure (var "c")))))))) $
  "escaped" <~ Strings.cat (Lists.map (var "escape") (Strings.toList $ var "s")) $
  string "\"" ++ var "escaped" ++ string "\""

keyValueToExpr :: TBinding ((String, J.Value) -> Expr)
keyValueToExpr = jsonSerdeDefinition "keyValueToExpr" $
  doc "Convert a key-value pair to an AST expression" $
  "pair" ~>
  "key" <~ Pairs.first (var "pair") $
  "value" <~ Pairs.second (var "pair") $
  Serialization.ifx @@ colonOp
    @@ (Serialization.cst @@ (jsonString @@ var "key"))
    @@ (valueToExpr @@ var "value")

valueToExpr :: TBinding (J.Value -> Expr)
valueToExpr = jsonSerdeDefinition "valueToExpr" $
  doc "Convert a JSON value to an AST expression for serialization" $
  "value" ~>
  cases J._Value (var "value") Nothing [
    J._Value_array>>: "arr" ~>
      Serialization.bracketListAdaptive @@ (Lists.map (valueToExpr) (var "arr")),
    J._Value_boolean>>: "b" ~>
      Serialization.cst @@ (Logic.ifElse (var "b") (string "true") (string "false")),
    J._Value_null>>: constant $
      Serialization.cst @@ string "null",
    J._Value_number>>: "n" ~>
      Serialization.cst @@ (Literals.showBigfloat $ var "n"),
    J._Value_object>>: "obj" ~>
      Serialization.bracesListAdaptive @@ (Lists.map (keyValueToExpr) (Maps.toList $ var "obj")),
    J._Value_string>>: "s" ~>
      Serialization.cst @@ (jsonString @@ var "s")]

printJson :: TBinding (J.Value -> String)
printJson = jsonSerdeDefinition "printJson" $
  doc "Serialize a JSON value to a string" $
  "value" ~> Serialization.printExpr @@ (valueToExpr @@ var "value")
