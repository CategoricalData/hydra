-- | Rust serializer: converts Rust AST to concrete syntax (source code).
-- Serializes the Rust syntax model (Hydra.Ext.Rust.Syntax) into properly formatted Rust source code.

module Hydra.Ext.Sources.Rust.Serde where

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
import qualified Hydra.Ext.Rust.Syntax as R
import qualified Hydra.Ext.Sources.Rust.Syntax as RustSyntax
import qualified Hydra.Ext.Sources.Rust.Operators as RustOperators


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.rust.serde"

module_ :: Module
module_ = Module ns elements
    [Constants.ns, Serialization.ns, RustOperators.ns]
    (RustSyntax.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Rust serializer: converts Rust AST to concrete syntax"
  where
    elements = [
      -- Top-level serialization
      toBinding crateToExpr,
      toBinding itemToExpr,
      toBinding itemWithCommentsToExpr,
      -- Use declarations
      toBinding useDeclarationToExpr,
      toBinding useTreeToExpr,
      -- Struct definitions
      toBinding structDefToExpr,
      toBinding structBodyToExpr,
      toBinding structFieldToExpr,
      -- Enum definitions
      toBinding enumDefToExpr,
      toBinding enumVariantToExpr,
      toBinding enumVariantBodyToExpr,
      -- Function definitions
      toBinding fnDefToExpr,
      toBinding fnParamToExpr,
      toBinding methodParamToExpr,
      -- Type aliases
      toBinding typeAliasToExpr,
      -- Const, static, module
      toBinding constDefToExpr,
      toBinding staticDefToExpr,
      toBinding modDefToExpr,
      -- Impl blocks
      toBinding implBlockToExpr,
      toBinding implItemToExpr,
      toBinding implMethodToExpr,
      -- Trait definitions
      toBinding traitDefToExpr,
      toBinding traitItemToExpr,
      toBinding traitMethodToExpr,
      toBinding traitTypeToExpr,
      toBinding traitConstToExpr,
      -- Generics and where clauses
      toBinding genericParamToExpr,
      toBinding genericParamsToExpr,
      toBinding typeParamBoundToExpr,
      toBinding whereClauseToExpr,
      -- Types
      toBinding typeToExpr,
      toBinding typePathToExpr,
      toBinding pathSegmentToExpr,
      toBinding genericArgumentsToExpr,
      toBinding genericArgToExpr,
      toBinding referenceTypeToExpr,
      -- Expressions
      toBinding expressionToExpr,
      toBinding exprPathToExpr,
      toBinding callExprToExpr,
      toBinding methodCallExprToExpr,
      toBinding fieldAccessExprToExpr,
      toBinding tupleIndexExprToExpr,
      toBinding closureExprToExpr,
      toBinding closureParamToStr,
      toBinding ifExprToExpr,
      toBinding matchExprToExpr,
      toBinding matchArmToExpr,
      toBinding loopExprToExpr,
      toBinding whileExprToExpr,
      toBinding forExprToExpr,
      toBinding binaryExprToExpr,
      toBinding binaryOpToExpr,
      toBinding unaryExprToExpr,
      toBinding refExprToExpr,
      toBinding structExprToExpr,
      toBinding fieldValueToExpr,
      toBinding arrayExprToExpr,
      toBinding indexExprToExpr,
      toBinding rangeExprToExpr,
      toBinding castExprToExpr,
      toBinding typeAscriptionExprToExpr,
      toBinding assignExprToExpr,
      toBinding compoundAssignExprToExpr,
      toBinding macroInvocationToExpr,
      -- Statements
      toBinding statementToExpr,
      toBinding letStatementToExpr,
      toBinding blockToExpr,
      -- Patterns
      toBinding patternToExpr,
      toBinding identifierPatternToExpr,
      toBinding refPatternToExpr,
      toBinding structPatternToExpr,
      toBinding fieldPatternToExpr,
      toBinding tupleStructPatternToExpr,
      toBinding rangePatternToExpr,
      -- Literals
      toBinding literalToExpr,
      toBinding integerLiteralToExpr,
      toBinding floatLiteralToExpr,
      -- Visibility and attributes
      toBinding visibilityToExpr,
      toBinding attributeToExpr,
      toBinding derivesToExpr,
      -- Comments
      toBinding toRustDocComment,
      toBinding toRustComment]

-- =============================================================================
-- Top-level serialization
-- =============================================================================

crateToExpr :: TBinding (R.Crate -> Expr)
crateToExpr = define "crateToExpr" $
  doc "Serialize a Rust crate to an AST expression" $
  lambda "crate" $
    Serialization.doubleNewlineSep @@ (Lists.map (itemWithCommentsToExpr)
      (project R._Crate R._Crate_items @@ var "crate"))

itemWithCommentsToExpr :: TBinding (R.ItemWithComments -> Expr)
itemWithCommentsToExpr = define "itemWithCommentsToExpr" $
  doc "Serialize an item with optional doc comments and visibility" $
  lambda "iwc" $ lets [
    "doc">: project R._ItemWithComments R._ItemWithComments_doc @@ var "iwc",
    "vis">: project R._ItemWithComments R._ItemWithComments_visibility @@ var "iwc",
    "item">: project R._ItemWithComments R._ItemWithComments_item @@ var "iwc",
    "docPart">: Maybes.maybe
      (list ([] :: [TTerm Expr]))
      (lambda "d" $ list [Serialization.cst @@ (toRustDocComment @@ var "d")])
      (var "doc"),
    "visPart">: visibilityToExpr @@ var "vis",
    "itemPart">: itemToExpr @@ var "item"] $
    Serialization.newlineSep @@ Lists.concat (list [var "docPart", list [
      Serialization.spaceSep @@ Maybes.cat (list [var "visPart", just (var "itemPart")])]])

itemToExpr :: TBinding (R.Item -> Expr)
itemToExpr = define "itemToExpr" $
  doc "Serialize a Rust item to an AST expression" $
  lambda "item" $
    cases R._Item (var "item") Nothing [
      R._Item_use>>: lambda "u" $ useDeclarationToExpr @@ var "u",
      R._Item_struct>>: lambda "s" $ structDefToExpr @@ var "s",
      R._Item_enum>>: lambda "e" $ enumDefToExpr @@ var "e",
      R._Item_fn>>: lambda "f" $ fnDefToExpr @@ var "f",
      R._Item_typeAlias>>: lambda "t" $ typeAliasToExpr @@ var "t",
      R._Item_impl>>: lambda "i" $ implBlockToExpr @@ var "i",
      R._Item_trait>>: lambda "t" $ traitDefToExpr @@ var "t",
      R._Item_mod>>: lambda "m" $ modDefToExpr @@ var "m",
      R._Item_const>>: lambda "c" $ constDefToExpr @@ var "c",
      R._Item_static>>: lambda "s" $ staticDefToExpr @@ var "s",
      R._Item_macro>>: lambda "m" $ macroInvocationToExpr @@ var "m"]

-- =============================================================================
-- Use declarations
-- =============================================================================

useDeclarationToExpr :: TBinding (R.UseDeclaration -> Expr)
useDeclarationToExpr = define "useDeclarationToExpr" $
  doc "Serialize a use declaration" $
  lambda "use" $ lets [
    "pub">: project R._UseDeclaration R._UseDeclaration_public @@ var "use",
    "tree">: project R._UseDeclaration R._UseDeclaration_tree @@ var "use",
    "pubKw">: Logic.ifElse (var "pub") (just $ Serialization.cst @@ string "pub") nothing] $
    Serialization.spaceSep @@ Maybes.cat (list [
      var "pubKw",
      just $ Serialization.cst @@ string "use",
      just $ useTreeToExpr @@ var "tree",
      just $ Serialization.cst @@ string ";"])

useTreeToExpr :: TBinding (R.UseTree -> Expr)
useTreeToExpr = define "useTreeToExpr" $
  doc "Serialize a use tree" $
  lambda "tree" $
    cases R._UseTree (var "tree") Nothing [
      R._UseTree_path>>: lambda "p" $
        Serialization.cst @@ (Strings.intercalate (string "::") (project R._UsePath R._UsePath_segments @@ var "p")),
      R._UseTree_rename>>: lambda "r" $ lets [
        "path">: project R._UseRename R._UseRename_path @@ var "r",
        "alias">: project R._UseRename R._UseRename_alias @@ var "r"] $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ (Strings.intercalate (string "::") (var "path")),
          Serialization.cst @@ string "as",
          Serialization.cst @@ var "alias"],
      R._UseTree_glob>>: lambda "segs" $
        Serialization.cst @@ (Strings.cat2 (Strings.intercalate (string "::") (var "segs")) (string "::*")),
      R._UseTree_group>>: lambda "g" $ lets [
        "prefix">: project R._UseGroup R._UseGroup_prefix @@ var "g",
        "trees">: project R._UseGroup R._UseGroup_trees @@ var "g",
        "prefixStr">: Logic.ifElse (Lists.null $ var "prefix")
          (string "")
          (Strings.cat2 (Strings.intercalate (string "::") (var "prefix")) (string "::"))] $
        Serialization.cst @@ (Strings.cat (list [
          var "prefixStr",
          string "{",
          Strings.intercalate (string ", ") (Lists.map (lambda "t" $ Serialization.printExpr @@ (useTreeToExpr @@ var "t")) (var "trees")),
          string "}"]))]

-- =============================================================================
-- Struct definitions
-- =============================================================================

structDefToExpr :: TBinding (R.StructDef -> Expr)
structDefToExpr = define "structDefToExpr" $
  doc "Serialize a struct definition" $
  lambda "s" $ lets [
    "name">: project R._StructDef R._StructDef_name @@ var "s",
    "generics">: project R._StructDef R._StructDef_generics @@ var "s",
    "whereC">: project R._StructDef R._StructDef_whereClause @@ var "s",
    "body">: project R._StructDef R._StructDef_body @@ var "s",
    "derives">: project R._StructDef R._StructDef_derives @@ var "s",
    "docC">: project R._StructDef R._StructDef_doc @@ var "s",
    "derivesAttr">: derivesToExpr @@ var "derives",
    "docPart">: Maybes.maybe (list ([] :: [TTerm Expr])) (lambda "d" $ list [Serialization.cst @@ (toRustDocComment @@ var "d")]) (var "docC"),
    "header">: Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "struct",
      just $ Serialization.cst @@ var "name",
      genericParamsToExpr @@ var "generics"]),
    "wherePart">: Maybes.maybe nothing (lambda "w" $ just $ whereClauseToExpr @@ var "w") (var "whereC")] $
    Serialization.newlineSep @@ Lists.concat (list [
      var "docPart",
      Maybes.maybe (list ([] :: [TTerm Expr])) (lambda "d" $ list [var "d"]) (var "derivesAttr"),
      list [Serialization.spaceSep @@ Maybes.cat (list [
        just $ var "header",
        var "wherePart",
        just $ structBodyToExpr @@ var "body"])]])

structBodyToExpr :: TBinding (R.StructBody -> Expr)
structBodyToExpr = define "structBodyToExpr" $
  doc "Serialize a struct body" $
  lambda "body" $
    cases R._StructBody (var "body") Nothing [
      R._StructBody_named>>: lambda "fields" $
        Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@
          (Lists.map (structFieldToExpr) (var "fields")),
      R._StructBody_tuple>>: lambda "fields" $
        Serialization.spaceSep @@ list [
          Serialization.parenList @@ false @@ (Lists.map (lambda "f" $ typeToExpr @@ (project R._TupleField R._TupleField_type @@ var "f")) (var "fields")),
          Serialization.cst @@ string ";"],
      R._StructBody_unit>>: constant $ Serialization.cst @@ string ";"]

structFieldToExpr :: TBinding (R.StructField -> Expr)
structFieldToExpr = define "structFieldToExpr" $
  doc "Serialize a struct field" $
  lambda "field" $ lets [
    "name">: project R._StructField R._StructField_name @@ var "field",
    "typ">: project R._StructField R._StructField_type @@ var "field",
    "pub">: project R._StructField R._StructField_public @@ var "field",
    "docC">: project R._StructField R._StructField_doc @@ var "field",
    "pubKw">: Logic.ifElse (var "pub") (just $ Serialization.cst @@ string "pub") nothing,
    "docPart">: Maybes.maybe (list ([] :: [TTerm Expr])) (lambda "d" $ list [Serialization.cst @@ (toRustDocComment @@ var "d")]) (var "docC")] $
    Serialization.newlineSep @@ Lists.concat (list [
      var "docPart",
      list [Serialization.spaceSep @@ Maybes.cat (list [
        var "pubKw",
        just $ Serialization.cst @@ (Strings.cat2 (var "name") (string ":")),
        just $ typeToExpr @@ var "typ"])]])

-- =============================================================================
-- Enum definitions
-- =============================================================================

enumDefToExpr :: TBinding (R.EnumDef -> Expr)
enumDefToExpr = define "enumDefToExpr" $
  doc "Serialize an enum definition" $
  lambda "e" $ lets [
    "name">: project R._EnumDef R._EnumDef_name @@ var "e",
    "generics">: project R._EnumDef R._EnumDef_generics @@ var "e",
    "whereC">: project R._EnumDef R._EnumDef_whereClause @@ var "e",
    "variants">: project R._EnumDef R._EnumDef_variants @@ var "e",
    "derives">: project R._EnumDef R._EnumDef_derives @@ var "e",
    "docC">: project R._EnumDef R._EnumDef_doc @@ var "e",
    "derivesAttr">: derivesToExpr @@ var "derives",
    "docPart">: Maybes.maybe (list ([] :: [TTerm Expr])) (lambda "d" $ list [Serialization.cst @@ (toRustDocComment @@ var "d")]) (var "docC"),
    "header">: Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "enum",
      just $ Serialization.cst @@ var "name",
      genericParamsToExpr @@ var "generics"]),
    "wherePart">: Maybes.maybe nothing (lambda "w" $ just $ whereClauseToExpr @@ var "w") (var "whereC"),
    "body">: Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@
      (Lists.map (enumVariantToExpr) (var "variants"))] $
    Serialization.newlineSep @@ Lists.concat (list [
      var "docPart",
      Maybes.maybe (list ([] :: [TTerm Expr])) (lambda "d" $ list [var "d"]) (var "derivesAttr"),
      list [Serialization.spaceSep @@ Maybes.cat (list [
        just $ var "header",
        var "wherePart",
        just $ var "body"])]])

enumVariantToExpr :: TBinding (R.EnumVariant -> Expr)
enumVariantToExpr = define "enumVariantToExpr" $
  doc "Serialize an enum variant" $
  lambda "v" $ lets [
    "name">: project R._EnumVariant R._EnumVariant_name @@ var "v",
    "body">: project R._EnumVariant R._EnumVariant_body @@ var "v",
    "docC">: project R._EnumVariant R._EnumVariant_doc @@ var "v",
    "docPart">: Maybes.maybe (list ([] :: [TTerm Expr])) (lambda "d" $ list [Serialization.cst @@ (toRustDocComment @@ var "d")]) (var "docC")] $
    Serialization.newlineSep @@ Lists.concat (list [
      var "docPart",
      list [Serialization.spaceSep @@ list [
        Serialization.cst @@ var "name",
        enumVariantBodyToExpr @@ var "body"]]])

enumVariantBodyToExpr :: TBinding (R.EnumVariantBody -> Expr)
enumVariantBodyToExpr = define "enumVariantBodyToExpr" $
  doc "Serialize an enum variant body" $
  lambda "body" $
    cases R._EnumVariantBody (var "body") Nothing [
      R._EnumVariantBody_unit>>: constant $ Serialization.cst @@ string "",
      R._EnumVariantBody_tuple>>: lambda "types" $
        Serialization.parenList @@ false @@ (Lists.map (typeToExpr) (var "types")),
      R._EnumVariantBody_struct>>: lambda "fields" $
        Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@
          (Lists.map (structFieldToExpr) (var "fields"))]

-- =============================================================================
-- Function definitions
-- =============================================================================

fnDefToExpr :: TBinding (R.FnDef -> Expr)
fnDefToExpr = define "fnDefToExpr" $
  doc "Serialize a function definition" $
  lambda "f" $ lets [
    "name">: project R._FnDef R._FnDef_name @@ var "f",
    "generics">: project R._FnDef R._FnDef_generics @@ var "f",
    "whereC">: project R._FnDef R._FnDef_whereClause @@ var "f",
    "params">: project R._FnDef R._FnDef_params @@ var "f",
    "retType">: project R._FnDef R._FnDef_returnType @@ var "f",
    "body">: project R._FnDef R._FnDef_body @@ var "f",
    "isAsync">: project R._FnDef R._FnDef_async @@ var "f",
    "isConst">: project R._FnDef R._FnDef_const @@ var "f",
    "isUnsafe">: project R._FnDef R._FnDef_unsafe @@ var "f",
    "docC">: project R._FnDef R._FnDef_doc @@ var "f",
    "docPart">: Maybes.maybe (list ([] :: [TTerm Expr])) (lambda "d" $ list [Serialization.cst @@ (toRustDocComment @@ var "d")]) (var "docC"),
    "asyncKw">: Logic.ifElse (var "isAsync") (just $ Serialization.cst @@ string "async") nothing,
    "constKw">: Logic.ifElse (var "isConst") (just $ Serialization.cst @@ string "const") nothing,
    "unsafeKw">: Logic.ifElse (var "isUnsafe") (just $ Serialization.cst @@ string "unsafe") nothing,
    "fnKw">: Serialization.cst @@ string "fn",
    "nameExpr">: Serialization.cst @@ var "name",
    "genericsExpr">: genericParamsToExpr @@ var "generics",
    "paramsExpr">: Serialization.parenList @@ false @@ (Lists.map (fnParamToExpr) (var "params")),
    "retTypeExpr">: Maybes.maybe nothing (lambda "t" $ just $ Serialization.spaceSep @@ list [
      Serialization.cst @@ string "->",
      typeToExpr @@ var "t"]) (var "retType"),
    "whereExpr">: Maybes.maybe nothing (lambda "w" $ just $ whereClauseToExpr @@ var "w") (var "whereC"),
    "header">: Serialization.spaceSep @@ Maybes.cat (list [
      var "asyncKw", var "constKw", var "unsafeKw",
      just $ var "fnKw", just $ var "nameExpr", var "genericsExpr",
      just $ var "paramsExpr", var "retTypeExpr", var "whereExpr"])] $
    Serialization.newlineSep @@ Lists.concat (list [
      var "docPart",
      list [Serialization.spaceSep @@ list [var "header", blockToExpr @@ var "body"]]])

fnParamToExpr :: TBinding (R.FnParam -> Expr)
fnParamToExpr = define "fnParamToExpr" $
  doc "Serialize a function parameter" $
  lambda "param" $ lets [
    "pat">: project R._FnParam R._FnParam_pattern @@ var "param",
    "typ">: project R._FnParam R._FnParam_type @@ var "param"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ (Strings.cat2 (Serialization.printExpr @@ (patternToExpr @@ var "pat")) (string ":")),
      typeToExpr @@ var "typ"]

methodParamToExpr :: TBinding (R.MethodParam -> Expr)
methodParamToExpr = define "methodParamToExpr" $
  doc "Serialize a method parameter" $
  lambda "param" $
    cases R._MethodParam (var "param") Nothing [
      R._MethodParam_self>>: lambda "sp" $
        cases R._SelfParam (var "sp") Nothing [
          R._SelfParam_owned>>: constant $ Serialization.cst @@ string "self",
          R._SelfParam_ref>>: constant $ Serialization.cst @@ string "&self",
          R._SelfParam_refMut>>: constant $ Serialization.cst @@ string "&mut self"],
      R._MethodParam_regular>>: lambda "fp" $ fnParamToExpr @@ var "fp"]

-- =============================================================================
-- Type aliases
-- =============================================================================

typeAliasToExpr :: TBinding (R.TypeAlias -> Expr)
typeAliasToExpr = define "typeAliasToExpr" $
  doc "Serialize a type alias" $
  lambda "ta" $ lets [
    "name">: project R._TypeAlias R._TypeAlias_name @@ var "ta",
    "generics">: project R._TypeAlias R._TypeAlias_generics @@ var "ta",
    "typ">: project R._TypeAlias R._TypeAlias_type @@ var "ta",
    "docC">: project R._TypeAlias R._TypeAlias_doc @@ var "ta",
    "docPart">: Maybes.maybe (list ([] :: [TTerm Expr])) (lambda "d" $ list [Serialization.cst @@ (toRustDocComment @@ var "d")]) (var "docC")] $
    Serialization.newlineSep @@ Lists.concat (list [
      var "docPart",
      list [Serialization.spaceSep @@ Maybes.cat (list [
        just $ Serialization.cst @@ string "type",
        just $ Serialization.cst @@ var "name",
        genericParamsToExpr @@ var "generics",
        just $ Serialization.cst @@ string "=",
        just $ typeToExpr @@ var "typ",
        just $ Serialization.cst @@ string ";"])]])

-- =============================================================================
-- Const, static, module definitions
-- =============================================================================

constDefToExpr :: TBinding (R.ConstDef -> Expr)
constDefToExpr = define "constDefToExpr" $
  doc "Serialize a const definition" $
  lambda "c" $ lets [
    "name">: project R._ConstDef R._ConstDef_name @@ var "c",
    "typ">: project R._ConstDef R._ConstDef_type @@ var "c",
    "val">: project R._ConstDef R._ConstDef_value @@ var "c"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "const",
      Serialization.cst @@ (Strings.cat2 (var "name") (string ":")),
      typeToExpr @@ var "typ",
      Serialization.cst @@ string "=",
      expressionToExpr @@ var "val",
      Serialization.cst @@ string ";"]

staticDefToExpr :: TBinding (R.StaticDef -> Expr)
staticDefToExpr = define "staticDefToExpr" $
  doc "Serialize a static definition" $
  lambda "s" $ lets [
    "name">: project R._StaticDef R._StaticDef_name @@ var "s",
    "typ">: project R._StaticDef R._StaticDef_type @@ var "s",
    "val">: project R._StaticDef R._StaticDef_value @@ var "s",
    "mut">: project R._StaticDef R._StaticDef_mutable @@ var "s",
    "mutKw">: Logic.ifElse (var "mut") (just $ Serialization.cst @@ string "mut") nothing] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "static",
      var "mutKw",
      just $ Serialization.cst @@ (Strings.cat2 (var "name") (string ":")),
      just $ typeToExpr @@ var "typ",
      just $ Serialization.cst @@ string "=",
      just $ expressionToExpr @@ var "val",
      just $ Serialization.cst @@ string ";"])

modDefToExpr :: TBinding (R.ModDef -> Expr)
modDefToExpr = define "modDefToExpr" $
  doc "Serialize a module definition" $
  lambda "m" $ lets [
    "name">: project R._ModDef R._ModDef_name @@ var "m",
    "body">: project R._ModDef R._ModDef_body @@ var "m"] $
    Maybes.maybe
      (Serialization.spaceSep @@ list [
        Serialization.cst @@ string "mod",
        Serialization.cst @@ var "name",
        Serialization.cst @@ string ";"])
      (lambda "items" $ Serialization.spaceSep @@ list [
        Serialization.cst @@ string "mod",
        Serialization.cst @@ var "name",
        Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@
          (Lists.map (itemToExpr) (var "items"))])
      (var "body")

-- =============================================================================
-- Impl blocks
-- =============================================================================

implBlockToExpr :: TBinding (R.ImplBlock -> Expr)
implBlockToExpr = define "implBlockToExpr" $
  doc "Serialize an impl block" $
  lambda "i" $ lets [
    "generics">: project R._ImplBlock R._ImplBlock_generics @@ var "i",
    "whereC">: project R._ImplBlock R._ImplBlock_whereClause @@ var "i",
    "trait">: project R._ImplBlock R._ImplBlock_trait @@ var "i",
    "selfType">: project R._ImplBlock R._ImplBlock_selfType @@ var "i",
    "items">: project R._ImplBlock R._ImplBlock_items @@ var "i",
    "genericsExpr">: genericParamsToExpr @@ var "generics",
    "traitPart">: Maybes.maybe nothing (lambda "t" $ just $ Serialization.spaceSep @@ list [
      typePathToExpr @@ var "t",
      Serialization.cst @@ string "for"]) (var "trait"),
    "wherePart">: Maybes.maybe nothing (lambda "w" $ just $ whereClauseToExpr @@ var "w") (var "whereC"),
    "header">: Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "impl",
      var "genericsExpr",
      var "traitPart",
      just $ typeToExpr @@ var "selfType",
      var "wherePart"]),
    "body">: Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@
      (Lists.map (implItemToExpr) (var "items"))] $
    Serialization.spaceSep @@ list [var "header", var "body"]

implItemToExpr :: TBinding (R.ImplItem -> Expr)
implItemToExpr = define "implItemToExpr" $
  doc "Serialize an impl item" $
  lambda "item" $
    cases R._ImplItem (var "item") Nothing [
      R._ImplItem_method>>: lambda "m" $ implMethodToExpr @@ var "m",
      R._ImplItem_type>>: lambda "t" $ typeAliasToExpr @@ var "t",
      R._ImplItem_const>>: lambda "c" $ constDefToExpr @@ var "c"]

implMethodToExpr :: TBinding (R.ImplMethod -> Expr)
implMethodToExpr = define "implMethodToExpr" $
  doc "Serialize an impl method" $
  lambda "m" $ lets [
    "name">: project R._ImplMethod R._ImplMethod_name @@ var "m",
    "generics">: project R._ImplMethod R._ImplMethod_generics @@ var "m",
    "whereC">: project R._ImplMethod R._ImplMethod_whereClause @@ var "m",
    "params">: project R._ImplMethod R._ImplMethod_params @@ var "m",
    "retType">: project R._ImplMethod R._ImplMethod_returnType @@ var "m",
    "body">: project R._ImplMethod R._ImplMethod_body @@ var "m",
    "pub">: project R._ImplMethod R._ImplMethod_public @@ var "m",
    "docC">: project R._ImplMethod R._ImplMethod_doc @@ var "m",
    "docPart">: Maybes.maybe (list ([] :: [TTerm Expr])) (lambda "d" $ list [Serialization.cst @@ (toRustDocComment @@ var "d")]) (var "docC"),
    "pubKw">: Logic.ifElse (var "pub") (just $ Serialization.cst @@ string "pub") nothing,
    "genericsExpr">: genericParamsToExpr @@ var "generics",
    "paramsExpr">: Serialization.parenList @@ false @@ (Lists.map (methodParamToExpr) (var "params")),
    "retTypeExpr">: Maybes.maybe nothing (lambda "t" $ just $ Serialization.spaceSep @@ list [
      Serialization.cst @@ string "->",
      typeToExpr @@ var "t"]) (var "retType"),
    "whereExpr">: Maybes.maybe nothing (lambda "w" $ just $ whereClauseToExpr @@ var "w") (var "whereC"),
    "header">: Serialization.spaceSep @@ Maybes.cat (list [
      var "pubKw",
      just $ Serialization.cst @@ string "fn",
      just $ Serialization.cst @@ var "name",
      var "genericsExpr",
      just $ var "paramsExpr",
      var "retTypeExpr",
      var "whereExpr"])] $
    Serialization.newlineSep @@ Lists.concat (list [
      var "docPart",
      list [Serialization.spaceSep @@ list [var "header", blockToExpr @@ var "body"]]])

-- =============================================================================
-- Trait definitions
-- =============================================================================

traitDefToExpr :: TBinding (R.TraitDef -> Expr)
traitDefToExpr = define "traitDefToExpr" $
  doc "Serialize a trait definition" $
  lambda "t" $ lets [
    "name">: project R._TraitDef R._TraitDef_name @@ var "t",
    "generics">: project R._TraitDef R._TraitDef_generics @@ var "t",
    "whereC">: project R._TraitDef R._TraitDef_whereClause @@ var "t",
    "supers">: project R._TraitDef R._TraitDef_superTraits @@ var "t",
    "items">: project R._TraitDef R._TraitDef_items @@ var "t",
    "isUnsafe">: project R._TraitDef R._TraitDef_unsafe @@ var "t",
    "docC">: project R._TraitDef R._TraitDef_doc @@ var "t",
    "docPart">: Maybes.maybe (list ([] :: [TTerm Expr])) (lambda "d" $ list [Serialization.cst @@ (toRustDocComment @@ var "d")]) (var "docC"),
    "unsafeKw">: Logic.ifElse (var "isUnsafe") (just $ Serialization.cst @@ string "unsafe") nothing,
    "genericsExpr">: genericParamsToExpr @@ var "generics",
    "superPart">: Logic.ifElse (Lists.null $ var "supers") nothing
      (just $ Serialization.spaceSep @@ list [
        Serialization.cst @@ string ":",
        Serialization.cst @@ (Strings.intercalate (string " + ") (Lists.map (lambda "b" $ Serialization.printExpr @@ (typeParamBoundToExpr @@ var "b")) (var "supers")))]),
    "wherePart">: Maybes.maybe nothing (lambda "w" $ just $ whereClauseToExpr @@ var "w") (var "whereC"),
    "header">: Serialization.spaceSep @@ Maybes.cat (list [
      var "unsafeKw",
      just $ Serialization.cst @@ string "trait",
      just $ Serialization.cst @@ var "name",
      var "genericsExpr",
      var "superPart",
      var "wherePart"]),
    "body">: Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@
      (Lists.map (traitItemToExpr) (var "items"))] $
    Serialization.newlineSep @@ Lists.concat (list [
      var "docPart",
      list [Serialization.spaceSep @@ list [var "header", var "body"]]])

traitItemToExpr :: TBinding (R.TraitItem -> Expr)
traitItemToExpr = define "traitItemToExpr" $
  doc "Serialize a trait item" $
  lambda "item" $
    cases R._TraitItem (var "item") Nothing [
      R._TraitItem_method>>: lambda "m" $ traitMethodToExpr @@ var "m",
      R._TraitItem_type>>: lambda "t" $ traitTypeToExpr @@ var "t",
      R._TraitItem_const>>: lambda "c" $ traitConstToExpr @@ var "c"]

traitMethodToExpr :: TBinding (R.TraitMethod -> Expr)
traitMethodToExpr = define "traitMethodToExpr" $
  doc "Serialize a trait method" $
  lambda "m" $ lets [
    "name">: project R._TraitMethod R._TraitMethod_name @@ var "m",
    "generics">: project R._TraitMethod R._TraitMethod_generics @@ var "m",
    "params">: project R._TraitMethod R._TraitMethod_params @@ var "m",
    "retType">: project R._TraitMethod R._TraitMethod_returnType @@ var "m",
    "defBody">: project R._TraitMethod R._TraitMethod_defaultBody @@ var "m",
    "genericsExpr">: genericParamsToExpr @@ var "generics",
    "paramsExpr">: Serialization.parenList @@ false @@ (Lists.map (methodParamToExpr) (var "params")),
    "retTypeExpr">: Maybes.maybe nothing (lambda "t" $ just $ Serialization.spaceSep @@ list [
      Serialization.cst @@ string "->",
      typeToExpr @@ var "t"]) (var "retType"),
    "header">: Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "fn",
      just $ Serialization.cst @@ var "name",
      var "genericsExpr",
      just $ var "paramsExpr",
      var "retTypeExpr"])] $
    Maybes.maybe
      (Serialization.spaceSep @@ list [var "header", Serialization.cst @@ string ";"])
      (lambda "body" $ Serialization.spaceSep @@ list [var "header", blockToExpr @@ var "body"])
      (var "defBody")

traitTypeToExpr :: TBinding (R.TraitType -> Expr)
traitTypeToExpr = define "traitTypeToExpr" $
  doc "Serialize a trait associated type" $
  lambda "t" $ lets [
    "name">: project R._TraitType R._TraitType_name @@ var "t",
    "bounds">: project R._TraitType R._TraitType_bounds @@ var "t",
    "def">: project R._TraitType R._TraitType_default @@ var "t",
    "boundsPart">: Logic.ifElse (Lists.null $ var "bounds") nothing
      (just $ Serialization.spaceSep @@ list [
        Serialization.cst @@ string ":",
        Serialization.cst @@ (Strings.intercalate (string " + ") (Lists.map (lambda "b" $ Serialization.printExpr @@ (typeParamBoundToExpr @@ var "b")) (var "bounds")))]),
    "defPart">: Maybes.maybe nothing (lambda "d" $ just $ Serialization.spaceSep @@ list [
      Serialization.cst @@ string "=",
      typeToExpr @@ var "d"]) (var "def")] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "type",
      just $ Serialization.cst @@ var "name",
      var "boundsPart",
      var "defPart",
      just $ Serialization.cst @@ string ";"])

traitConstToExpr :: TBinding (R.TraitConst -> Expr)
traitConstToExpr = define "traitConstToExpr" $
  doc "Serialize a trait associated constant" $
  lambda "c" $ lets [
    "name">: project R._TraitConst R._TraitConst_name @@ var "c",
    "typ">: project R._TraitConst R._TraitConst_type @@ var "c",
    "def">: project R._TraitConst R._TraitConst_default @@ var "c",
    "defPart">: Maybes.maybe nothing (lambda "d" $ just $ Serialization.spaceSep @@ list [
      Serialization.cst @@ string "=",
      expressionToExpr @@ var "d"]) (var "def")] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "const",
      just $ Serialization.cst @@ (Strings.cat2 (var "name") (string ":")),
      just $ typeToExpr @@ var "typ",
      var "defPart",
      just $ Serialization.cst @@ string ";"])

-- =============================================================================
-- Generics and where clauses
-- =============================================================================

genericParamToExpr :: TBinding (R.GenericParam -> Expr)
genericParamToExpr = define "genericParamToExpr" $
  doc "Serialize a generic parameter" $
  lambda "gp" $ lets [
    "name">: project R._GenericParam R._GenericParam_name @@ var "gp",
    "bounds">: project R._GenericParam R._GenericParam_bounds @@ var "gp"] $
    Logic.ifElse (Lists.null $ var "bounds")
      (Serialization.cst @@ var "name")
      (Serialization.spaceSep @@ list [
        Serialization.cst @@ (Strings.cat2 (var "name") (string ":")),
        Serialization.cst @@ (Strings.intercalate (string " + ") (Lists.map (lambda "b" $ Serialization.printExpr @@ (typeParamBoundToExpr @@ var "b")) (var "bounds")))])

genericParamsToExpr :: TBinding ([R.GenericParam] -> Maybe Expr)
genericParamsToExpr = define "genericParamsToExpr" $
  doc "Serialize a list of generic parameters" $
  lambda "gps" $
    Logic.ifElse (Lists.null $ var "gps")
      nothing
      (just $ Serialization.angleBracesList @@ Serialization.inlineStyle @@ (Lists.map (genericParamToExpr) (var "gps")))

typeParamBoundToExpr :: TBinding (R.TypeParamBound -> Expr)
typeParamBoundToExpr = define "typeParamBoundToExpr" $
  doc "Serialize a type parameter bound" $
  lambda "bound" $
    cases R._TypeParamBound (var "bound") Nothing [
      R._TypeParamBound_trait>>: lambda "tp" $ typePathToExpr @@ var "tp",
      R._TypeParamBound_lifetime>>: lambda "lt" $
        Serialization.cst @@ (Strings.cat2 (string "'") (project R._Lifetime R._Lifetime_name @@ var "lt"))]

whereClauseToExpr :: TBinding (R.WhereClause -> Expr)
whereClauseToExpr = define "whereClauseToExpr" $
  doc "Serialize a where clause" $
  lambda "wc" $ lets [
    "preds">: project R._WhereClause R._WhereClause_predicates @@ var "wc",
    "predExprs">: Lists.map (lambda "p" $ lets [
      "typ">: project R._WherePredicate R._WherePredicate_type @@ var "p",
      "bounds">: project R._WherePredicate R._WherePredicate_bounds @@ var "p"] $
      Serialization.spaceSep @@ list [
        typeToExpr @@ var "typ",
        Serialization.cst @@ string ":",
        Serialization.cst @@ (Strings.intercalate (string " + ") (Lists.map (lambda "b" $ Serialization.printExpr @@ (typeParamBoundToExpr @@ var "b")) (var "bounds")))]) (var "preds")] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "where",
      Serialization.commaSep @@ Serialization.inlineStyle @@ var "predExprs"]

-- =============================================================================
-- Types
-- =============================================================================

typeToExpr :: TBinding (R.Type -> Expr)
typeToExpr = define "typeToExpr" $
  doc "Serialize a Rust type" $
  lambda "typ" $
    cases R._Type (var "typ") Nothing [
      R._Type_path>>: lambda "tp" $ typePathToExpr @@ var "tp",
      R._Type_reference>>: lambda "rt" $ referenceTypeToExpr @@ var "rt",
      R._Type_slice>>: lambda "t" $ Serialization.bracketList @@ Serialization.inlineStyle @@ list [typeToExpr @@ var "t"],
      R._Type_array>>: lambda "at" $ lets [
        "elem">: project R._ArrayType R._ArrayType_element @@ var "at",
        "len">: project R._ArrayType R._ArrayType_length @@ var "at"] $
        Serialization.cst @@ (Strings.cat (list [
          string "[",
          Serialization.printExpr @@ (typeToExpr @@ var "elem"),
          string "; ",
          Serialization.printExpr @@ (expressionToExpr @@ var "len"),
          string "]"])),
      R._Type_tuple>>: lambda "ts" $ Serialization.parenList @@ false @@ (Lists.map (typeToExpr) (var "ts")),
      R._Type_fnPointer>>: lambda "fp" $ lets [
        "params">: project R._FnPointerType R._FnPointerType_params @@ var "fp",
        "ret">: project R._FnPointerType R._FnPointerType_returnType @@ var "fp"] $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "fn",
          Serialization.parenList @@ false @@ (Lists.map (typeToExpr) (var "params")),
          Serialization.cst @@ string "->",
          typeToExpr @@ var "ret"],
      R._Type_implTrait>>: lambda "bounds" $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "impl",
          Serialization.cst @@ (Strings.intercalate (string " + ") (Lists.map (lambda "b" $ Serialization.printExpr @@ (typeParamBoundToExpr @@ var "b")) (var "bounds")))],
      R._Type_dynTrait>>: lambda "bounds" $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "dyn",
          Serialization.cst @@ (Strings.intercalate (string " + ") (Lists.map (lambda "b" $ Serialization.printExpr @@ (typeParamBoundToExpr @@ var "b")) (var "bounds")))],
      R._Type_inferred>>: constant $ Serialization.cst @@ string "_",
      R._Type_unit>>: constant $ Serialization.cst @@ string "()",
      R._Type_never>>: constant $ Serialization.cst @@ string "!",
      R._Type_rawPointer>>: lambda "rp" $ lets [
        "mut">: project R._RawPointerType R._RawPointerType_mutable @@ var "rp",
        "t">: project R._RawPointerType R._RawPointerType_type @@ var "rp",
        "kw">: Logic.ifElse (var "mut") (string "*mut") (string "*const")] $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ var "kw",
          typeToExpr @@ var "t"],
      R._Type_macro>>: lambda "m" $ macroInvocationToExpr @@ var "m"]

typePathToExpr :: TBinding (R.TypePath -> Expr)
typePathToExpr = define "typePathToExpr" $
  doc "Serialize a type path" $
  lambda "tp" $ lets [
    "global">: project R._TypePath R._TypePath_global @@ var "tp",
    "segs">: project R._TypePath R._TypePath_segments @@ var "tp",
    "prefix">: Logic.ifElse (var "global") (string "::") (string ""),
    "segStrs">: Lists.map (lambda "s" $ Serialization.printExpr @@ (pathSegmentToExpr @@ var "s")) (var "segs")] $
    Serialization.cst @@ (Strings.cat2 (var "prefix") (Strings.intercalate (string "::") (var "segStrs")))

pathSegmentToExpr :: TBinding (R.PathSegment -> Expr)
pathSegmentToExpr = define "pathSegmentToExpr" $
  doc "Serialize a path segment" $
  lambda "seg" $ lets [
    "name">: project R._PathSegment R._PathSegment_name @@ var "seg",
    "args">: project R._PathSegment R._PathSegment_arguments @@ var "seg"] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ var "name",
      genericArgumentsToExpr @@ var "args"])

genericArgumentsToExpr :: TBinding (R.GenericArguments -> Maybe Expr)
genericArgumentsToExpr = define "genericArgumentsToExpr" $
  doc "Serialize generic arguments" $
  lambda "args" $
    cases R._GenericArguments (var "args") Nothing [
      R._GenericArguments_none>>: constant nothing,
      R._GenericArguments_angleBracketed>>: lambda "ab" $ lets [
        "args">: project R._AngleBracketedArgs R._AngleBracketedArgs_args @@ var "ab"] $
        just $ Serialization.angleBracesList @@ Serialization.inlineStyle @@ (Lists.map (genericArgToExpr) (var "args")),
      R._GenericArguments_parenthesized>>: lambda "pa" $ lets [
        "inputs">: project R._ParenthesizedArgs R._ParenthesizedArgs_inputs @@ var "pa",
        "output">: project R._ParenthesizedArgs R._ParenthesizedArgs_output @@ var "pa",
        "inputPart">: Serialization.parenList @@ false @@ (Lists.map (typeToExpr) (var "inputs")),
        "outputPart">: Maybes.maybe nothing (lambda "t" $ just $ Serialization.spaceSep @@ list [
          Serialization.cst @@ string "->",
          typeToExpr @@ var "t"]) (var "output")] $
        just $ Serialization.spaceSep @@ Maybes.cat (list [just $ var "inputPart", var "outputPart"])]

genericArgToExpr :: TBinding (R.GenericArg -> Expr)
genericArgToExpr = define "genericArgToExpr" $
  doc "Serialize a generic argument" $
  lambda "arg" $
    cases R._GenericArg (var "arg") Nothing [
      R._GenericArg_type>>: lambda "t" $ typeToExpr @@ var "t",
      R._GenericArg_lifetime>>: lambda "lt" $
        Serialization.cst @@ (Strings.cat2 (string "'") (project R._Lifetime R._Lifetime_name @@ var "lt")),
      R._GenericArg_const>>: lambda "e" $ expressionToExpr @@ var "e",
      R._GenericArg_binding>>: lambda "tb" $ lets [
        "name">: project R._TypeBinding R._TypeBinding_name @@ var "tb",
        "typ">: project R._TypeBinding R._TypeBinding_type @@ var "tb"] $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ var "name",
          Serialization.cst @@ string "=",
          typeToExpr @@ var "typ"]]

referenceTypeToExpr :: TBinding (R.ReferenceType -> Expr)
referenceTypeToExpr = define "referenceTypeToExpr" $
  doc "Serialize a reference type" $
  lambda "rt" $ lets [
    "lt">: project R._ReferenceType R._ReferenceType_lifetime @@ var "rt",
    "mut">: project R._ReferenceType R._ReferenceType_mutable @@ var "rt",
    "t">: project R._ReferenceType R._ReferenceType_type @@ var "rt",
    "ltPart">: Maybes.maybe (string "") (lambda "l" $ Strings.cat2 (string "'") (Strings.cat2 (project R._Lifetime R._Lifetime_name @@ var "l") (string " "))) (var "lt"),
    "mutPart">: Logic.ifElse (var "mut") (string "mut ") (string "")] $
    Serialization.cst @@ (Strings.cat (list [
      string "&",
      var "ltPart",
      var "mutPart",
      Serialization.printExpr @@ (typeToExpr @@ var "t")]))

-- =============================================================================
-- Expressions
-- =============================================================================

expressionToExpr :: TBinding (R.Expression -> Expr)
expressionToExpr = define "expressionToExpr" $
  doc "Serialize a Rust expression" $
  lambda "expr" $
    cases R._Expression (var "expr") Nothing [
      R._Expression_literal>>: lambda "l" $ literalToExpr @@ var "l",
      R._Expression_path>>: lambda "p" $ exprPathToExpr @@ var "p",
      R._Expression_block>>: lambda "b" $ blockToExpr @@ var "b",
      R._Expression_call>>: lambda "c" $ callExprToExpr @@ var "c",
      R._Expression_methodCall>>: lambda "m" $ methodCallExprToExpr @@ var "m",
      R._Expression_fieldAccess>>: lambda "f" $ fieldAccessExprToExpr @@ var "f",
      R._Expression_tupleIndex>>: lambda "t" $ tupleIndexExprToExpr @@ var "t",
      R._Expression_closure>>: lambda "c" $ closureExprToExpr @@ var "c",
      R._Expression_if>>: lambda "i" $ ifExprToExpr @@ var "i",
      R._Expression_match>>: lambda "m" $ matchExprToExpr @@ var "m",
      R._Expression_loop>>: lambda "l" $ loopExprToExpr @@ var "l",
      R._Expression_while>>: lambda "w" $ whileExprToExpr @@ var "w",
      R._Expression_for>>: lambda "f" $ forExprToExpr @@ var "f",
      R._Expression_binary>>: lambda "b" $ binaryExprToExpr @@ var "b",
      R._Expression_unary>>: lambda "u" $ unaryExprToExpr @@ var "u",
      R._Expression_reference>>: lambda "r" $ refExprToExpr @@ var "r",
      R._Expression_dereference>>: lambda "d" $ Serialization.prefix @@ (string "*") @@ (expressionToExpr @@ var "d"),
      R._Expression_struct>>: lambda "s" $ structExprToExpr @@ var "s",
      R._Expression_tuple>>: lambda "es" $ Serialization.parenList @@ false @@ (Lists.map (expressionToExpr) (var "es")),
      R._Expression_array>>: lambda "a" $ arrayExprToExpr @@ var "a",
      R._Expression_index>>: lambda "i" $ indexExprToExpr @@ var "i",
      R._Expression_range>>: lambda "r" $ rangeExprToExpr @@ var "r",
      R._Expression_return>>: lambda "mr" $ Maybes.maybe
        (Serialization.cst @@ string "return")
        (lambda "e" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "return", expressionToExpr @@ var "e"])
        (var "mr"),
      R._Expression_break>>: lambda "mb" $ Maybes.maybe
        (Serialization.cst @@ string "break")
        (lambda "e" $ Serialization.spaceSep @@ list [Serialization.cst @@ string "break", expressionToExpr @@ var "e"])
        (var "mb"),
      R._Expression_continue>>: constant $ Serialization.cst @@ string "continue",
      R._Expression_try>>: lambda "e" $ Serialization.cst @@ (Strings.cat2 (Serialization.printExpr @@ (expressionToExpr @@ var "e")) (string "?")),
      R._Expression_cast>>: lambda "c" $ castExprToExpr @@ var "c",
      R._Expression_typeAscription>>: lambda "t" $ typeAscriptionExprToExpr @@ var "t",
      R._Expression_await>>: lambda "e" $ Serialization.cst @@ (Strings.cat2 (Serialization.printExpr @@ (expressionToExpr @@ var "e")) (string ".await")),
      R._Expression_assign>>: lambda "a" $ assignExprToExpr @@ var "a",
      R._Expression_compoundAssign>>: lambda "c" $ compoundAssignExprToExpr @@ var "c",
      R._Expression_macro>>: lambda "m" $ macroInvocationToExpr @@ var "m",
      R._Expression_paren>>: lambda "e" $ Serialization.parenthesize @@ (expressionToExpr @@ var "e")]

exprPathToExpr :: TBinding (R.ExprPath -> Expr)
exprPathToExpr = define "exprPathToExpr" $
  doc "Serialize an expression path" $
  lambda "ep" $ lets [
    "global">: project R._ExprPath R._ExprPath_global @@ var "ep",
    "segs">: project R._ExprPath R._ExprPath_segments @@ var "ep",
    "prefix">: Logic.ifElse (var "global") (string "::") (string ""),
    "segStrs">: Lists.map (lambda "s" $ Serialization.printExpr @@ (pathSegmentToExpr @@ var "s")) (var "segs")] $
    Serialization.cst @@ (Strings.cat2 (var "prefix") (Strings.intercalate (string "::") (var "segStrs")))

callExprToExpr :: TBinding (R.CallExpr -> Expr)
callExprToExpr = define "callExprToExpr" $
  doc "Serialize a function call expression" $
  lambda "c" $ lets [
    "func">: project R._CallExpr R._CallExpr_function @@ var "c",
    "args">: project R._CallExpr R._CallExpr_args @@ var "c"] $
    Serialization.spaceSep @@ list [
      expressionToExpr @@ var "func",
      Serialization.parenList @@ false @@ (Lists.map (expressionToExpr) (var "args"))]

methodCallExprToExpr :: TBinding (R.MethodCallExpr -> Expr)
methodCallExprToExpr = define "methodCallExprToExpr" $
  doc "Serialize a method call expression" $
  lambda "m" $ lets [
    "recv">: project R._MethodCallExpr R._MethodCallExpr_receiver @@ var "m",
    "method">: project R._MethodCallExpr R._MethodCallExpr_method @@ var "m",
    "turbo">: project R._MethodCallExpr R._MethodCallExpr_turbofish @@ var "m",
    "args">: project R._MethodCallExpr R._MethodCallExpr_args @@ var "m",
    "turboPart">: Logic.ifElse (Lists.null $ var "turbo")
      (string "")
      (Strings.cat (list [
        string "::<",
        Strings.intercalate (string ", ") (Lists.map (lambda "t" $ Serialization.printExpr @@ (typeToExpr @@ var "t")) (var "turbo")),
        string ">"]))] $
    Serialization.cst @@ (Strings.cat (list [
      Serialization.printExpr @@ (expressionToExpr @@ var "recv"),
      string ".",
      var "method",
      var "turboPart",
      string "(",
      Strings.intercalate (string ", ") (Lists.map (lambda "a" $ Serialization.printExpr @@ (expressionToExpr @@ var "a")) (var "args")),
      string ")"]))

fieldAccessExprToExpr :: TBinding (R.FieldAccessExpr -> Expr)
fieldAccessExprToExpr = define "fieldAccessExprToExpr" $
  doc "Serialize a field access expression" $
  lambda "f" $ lets [
    "obj">: project R._FieldAccessExpr R._FieldAccessExpr_object @@ var "f",
    "field">: project R._FieldAccessExpr R._FieldAccessExpr_field @@ var "f"] $
    Serialization.cst @@ (Strings.cat (list [
      Serialization.printExpr @@ (expressionToExpr @@ var "obj"),
      string ".",
      var "field"]))

tupleIndexExprToExpr :: TBinding (R.TupleIndexExpr -> Expr)
tupleIndexExprToExpr = define "tupleIndexExprToExpr" $
  doc "Serialize a tuple index expression" $
  lambda "t" $ lets [
    "tuple">: project R._TupleIndexExpr R._TupleIndexExpr_tuple @@ var "t",
    "idx">: project R._TupleIndexExpr R._TupleIndexExpr_index @@ var "t"] $
    Serialization.cst @@ (Strings.cat (list [
      Serialization.printExpr @@ (expressionToExpr @@ var "tuple"),
      string ".",
      Literals.showInt32 $ var "idx"]))

closureExprToExpr :: TBinding (R.ClosureExpr -> Expr)
closureExprToExpr = define "closureExprToExpr" $
  doc "Serialize a closure expression" $
  lambda "c" $ lets [
    "move">: project R._ClosureExpr R._ClosureExpr_move @@ var "c",
    "params">: project R._ClosureExpr R._ClosureExpr_params @@ var "c",
    "retType">: project R._ClosureExpr R._ClosureExpr_returnType @@ var "c",
    "body">: project R._ClosureExpr R._ClosureExpr_body @@ var "c",
    "moveKw">: Logic.ifElse (var "move") (just $ Serialization.cst @@ string "move") nothing,
    "paramsStr">: Strings.cat (list [
      string "|",
      Strings.intercalate (string ", ") (Lists.map (closureParamToStr) (var "params")),
      string "|"]),
    "retPart">: Maybes.maybe nothing (lambda "t" $ just $ Serialization.spaceSep @@ list [
      Serialization.cst @@ string "->",
      typeToExpr @@ var "t"]) (var "retType")] $
    Serialization.spaceSep @@ Maybes.cat (list [
      var "moveKw",
      just $ Serialization.cst @@ var "paramsStr",
      var "retPart",
      just $ expressionToExpr @@ var "body"])

closureParamToStr :: TBinding (R.ClosureParam -> String)
closureParamToStr = define "closureParamToStr" $
  doc "Serialize a closure parameter to string" $
  lambda "cp" $ lets [
    "pat">: project R._ClosureParam R._ClosureParam_pattern @@ var "cp",
    "typ">: project R._ClosureParam R._ClosureParam_type @@ var "cp",
    "patStr">: Serialization.printExpr @@ (patternToExpr @@ var "pat")] $
    Maybes.maybe
      (var "patStr")
      (lambda "t" $ Strings.cat (list [var "patStr", string ": ", Serialization.printExpr @@ (typeToExpr @@ var "t")]))
      (var "typ")

ifExprToExpr :: TBinding (R.IfExpr -> Expr)
ifExprToExpr = define "ifExprToExpr" $
  doc "Serialize an if expression" $
  lambda "i" $ lets [
    "cond">: project R._IfExpr R._IfExpr_condition @@ var "i",
    "thenB">: project R._IfExpr R._IfExpr_thenBlock @@ var "i",
    "elseB">: project R._IfExpr R._IfExpr_elseBranch @@ var "i",
    "condExpr">: cases R._IfCondition (var "cond") Nothing [
      R._IfCondition_bool>>: lambda "e" $ expressionToExpr @@ var "e",
      R._IfCondition_let>>: lambda "lc" $ lets [
        "pat">: project R._LetCondition R._LetCondition_pattern @@ var "lc",
        "expr">: project R._LetCondition R._LetCondition_expr @@ var "lc"] $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "let",
          patternToExpr @@ var "pat",
          Serialization.cst @@ string "=",
          expressionToExpr @@ var "expr"]],
    "elsePart">: Maybes.maybe nothing (lambda "e" $ just $ Serialization.spaceSep @@ list [
      Serialization.cst @@ string "else",
      expressionToExpr @@ var "e"]) (var "elseB")] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "if",
      just $ var "condExpr",
      just $ blockToExpr @@ var "thenB",
      var "elsePart"])

matchExprToExpr :: TBinding (R.MatchExpr -> Expr)
matchExprToExpr = define "matchExprToExpr" $
  doc "Serialize a match expression" $
  lambda "m" $ lets [
    "scrut">: project R._MatchExpr R._MatchExpr_scrutinee @@ var "m",
    "arms">: project R._MatchExpr R._MatchExpr_arms @@ var "m"] $
    Serialization.spaceSep @@ list [
      Serialization.cst @@ string "match",
      expressionToExpr @@ var "scrut",
      Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@
        (Lists.map (matchArmToExpr) (var "arms"))]

matchArmToExpr :: TBinding (R.MatchArm -> Expr)
matchArmToExpr = define "matchArmToExpr" $
  doc "Serialize a match arm" $
  lambda "arm" $ lets [
    "pat">: project R._MatchArm R._MatchArm_pattern @@ var "arm",
    "guard">: project R._MatchArm R._MatchArm_guard @@ var "arm",
    "body">: project R._MatchArm R._MatchArm_body @@ var "arm",
    "guardPart">: Maybes.maybe nothing (lambda "g" $ just $ Serialization.spaceSep @@ list [
      Serialization.cst @@ string "if",
      expressionToExpr @@ var "g"]) (var "guard")] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ patternToExpr @@ var "pat",
      var "guardPart",
      just $ Serialization.cst @@ string "=>",
      just $ expressionToExpr @@ var "body",
      just $ Serialization.cst @@ string ","])

loopExprToExpr :: TBinding (R.LoopExpr -> Expr)
loopExprToExpr = define "loopExprToExpr" $
  doc "Serialize a loop expression" $
  lambda "l" $ lets [
    "label">: project R._LoopExpr R._LoopExpr_label @@ var "l",
    "body">: project R._LoopExpr R._LoopExpr_body @@ var "l",
    "labelPart">: Maybes.maybe nothing (lambda "lbl" $ just $ Serialization.cst @@ (Strings.cat2 (string "'") (Strings.cat2 (var "lbl") (string ":")))) (var "label")] $
    Serialization.spaceSep @@ Maybes.cat (list [
      var "labelPart",
      just $ Serialization.cst @@ string "loop",
      just $ blockToExpr @@ var "body"])

whileExprToExpr :: TBinding (R.WhileExpr -> Expr)
whileExprToExpr = define "whileExprToExpr" $
  doc "Serialize a while expression" $
  lambda "w" $ lets [
    "label">: project R._WhileExpr R._WhileExpr_label @@ var "w",
    "cond">: project R._WhileExpr R._WhileExpr_condition @@ var "w",
    "body">: project R._WhileExpr R._WhileExpr_body @@ var "w",
    "labelPart">: Maybes.maybe nothing (lambda "lbl" $ just $ Serialization.cst @@ (Strings.cat2 (string "'") (Strings.cat2 (var "lbl") (string ":")))) (var "label"),
    "condExpr">: cases R._IfCondition (var "cond") Nothing [
      R._IfCondition_bool>>: lambda "e" $ expressionToExpr @@ var "e",
      R._IfCondition_let>>: lambda "lc" $ lets [
        "pat">: project R._LetCondition R._LetCondition_pattern @@ var "lc",
        "expr">: project R._LetCondition R._LetCondition_expr @@ var "lc"] $
        Serialization.spaceSep @@ list [
          Serialization.cst @@ string "let",
          patternToExpr @@ var "pat",
          Serialization.cst @@ string "=",
          expressionToExpr @@ var "expr"]]] $
    Serialization.spaceSep @@ Maybes.cat (list [
      var "labelPart",
      just $ Serialization.cst @@ string "while",
      just $ var "condExpr",
      just $ blockToExpr @@ var "body"])

forExprToExpr :: TBinding (R.ForExpr -> Expr)
forExprToExpr = define "forExprToExpr" $
  doc "Serialize a for expression" $
  lambda "f" $ lets [
    "label">: project R._ForExpr R._ForExpr_label @@ var "f",
    "pat">: project R._ForExpr R._ForExpr_pattern @@ var "f",
    "iter">: project R._ForExpr R._ForExpr_iter @@ var "f",
    "body">: project R._ForExpr R._ForExpr_body @@ var "f",
    "labelPart">: Maybes.maybe nothing (lambda "lbl" $ just $ Serialization.cst @@ (Strings.cat2 (string "'") (Strings.cat2 (var "lbl") (string ":")))) (var "label")] $
    Serialization.spaceSep @@ Maybes.cat (list [
      var "labelPart",
      just $ Serialization.cst @@ string "for",
      just $ patternToExpr @@ var "pat",
      just $ Serialization.cst @@ string "in",
      just $ expressionToExpr @@ var "iter",
      just $ blockToExpr @@ var "body"])

binaryExprToExpr :: TBinding (R.BinaryExpr -> Expr)
binaryExprToExpr = define "binaryExprToExpr" $
  doc "Serialize a binary expression" $
  lambda "b" $ lets [
    "left">: project R._BinaryExpr R._BinaryExpr_left @@ var "b",
    "op">: project R._BinaryExpr R._BinaryExpr_op @@ var "b",
    "right">: project R._BinaryExpr R._BinaryExpr_right @@ var "b"] $
    Serialization.spaceSep @@ list [
      expressionToExpr @@ var "left",
      binaryOpToExpr @@ var "op",
      expressionToExpr @@ var "right"]

binaryOpToExpr :: TBinding (R.BinaryOp -> Expr)
binaryOpToExpr = define "binaryOpToExpr" $
  doc "Serialize a binary operator" $
  lambda "op" $
    Serialization.cst @@ (cases R._BinaryOp (var "op") Nothing [
      R._BinaryOp_add>>: constant $ string "+",
      R._BinaryOp_sub>>: constant $ string "-",
      R._BinaryOp_mul>>: constant $ string "*",
      R._BinaryOp_div>>: constant $ string "/",
      R._BinaryOp_rem>>: constant $ string "%",
      R._BinaryOp_and>>: constant $ string "&&",
      R._BinaryOp_or>>: constant $ string "||",
      R._BinaryOp_bitAnd>>: constant $ string "&",
      R._BinaryOp_bitOr>>: constant $ string "|",
      R._BinaryOp_bitXor>>: constant $ string "^",
      R._BinaryOp_shl>>: constant $ string "<<",
      R._BinaryOp_shr>>: constant $ string ">>",
      R._BinaryOp_eq>>: constant $ string "==",
      R._BinaryOp_ne>>: constant $ string "!=",
      R._BinaryOp_lt>>: constant $ string "<",
      R._BinaryOp_le>>: constant $ string "<=",
      R._BinaryOp_gt>>: constant $ string ">",
      R._BinaryOp_ge>>: constant $ string ">="])

unaryExprToExpr :: TBinding (R.UnaryExpr -> Expr)
unaryExprToExpr = define "unaryExprToExpr" $
  doc "Serialize a unary expression" $
  lambda "u" $ lets [
    "op">: project R._UnaryExpr R._UnaryExpr_op @@ var "u",
    "operand">: project R._UnaryExpr R._UnaryExpr_operand @@ var "u",
    "opStr">: cases R._UnaryOp (var "op") Nothing [
      R._UnaryOp_neg>>: constant $ string "-",
      R._UnaryOp_not>>: constant $ string "!"]] $
    Serialization.cst @@ (Strings.cat2 (var "opStr") (Serialization.printExpr @@ (expressionToExpr @@ var "operand")))

refExprToExpr :: TBinding (R.RefExpr -> Expr)
refExprToExpr = define "refExprToExpr" $
  doc "Serialize a reference expression" $
  lambda "r" $ lets [
    "mut">: project R._RefExpr R._RefExpr_mutable @@ var "r",
    "expr">: project R._RefExpr R._RefExpr_expr @@ var "r",
    "prefix">: Logic.ifElse (var "mut") (string "&mut ") (string "&")] $
    Serialization.cst @@ (Strings.cat2 (var "prefix") (Serialization.printExpr @@ (expressionToExpr @@ var "expr")))

structExprToExpr :: TBinding (R.StructExpr -> Expr)
structExprToExpr = define "structExprToExpr" $
  doc "Serialize a struct literal expression" $
  lambda "s" $ lets [
    "path">: project R._StructExpr R._StructExpr_path @@ var "s",
    "fields">: project R._StructExpr R._StructExpr_fields @@ var "s",
    "rest">: project R._StructExpr R._StructExpr_rest @@ var "s",
    "fieldExprs">: Lists.map (fieldValueToExpr) (var "fields"),
    "restExpr">: Maybes.maybe (list ([] :: [TTerm Expr])) (lambda "r" $ list [
      Serialization.spaceSep @@ list [
        Serialization.cst @@ string "..",
        expressionToExpr @@ var "r"]]) (var "rest"),
    "allFields">: Lists.concat2 (var "fieldExprs") (var "restExpr")] $
    Serialization.spaceSep @@ list [
      exprPathToExpr @@ var "path",
      Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@ var "allFields"]

fieldValueToExpr :: TBinding (R.FieldValue -> Expr)
fieldValueToExpr = define "fieldValueToExpr" $
  doc "Serialize a field-value pair" $
  lambda "fv" $ lets [
    "name">: project R._FieldValue R._FieldValue_name @@ var "fv",
    "val">: project R._FieldValue R._FieldValue_value @@ var "fv"] $
    Maybes.maybe
      (Serialization.cst @@ var "name")
      (lambda "v" $ Serialization.spaceSep @@ list [
        Serialization.cst @@ (Strings.cat2 (var "name") (string ":")),
        expressionToExpr @@ var "v"])
      (var "val")

arrayExprToExpr :: TBinding (R.ArrayExpr -> Expr)
arrayExprToExpr = define "arrayExprToExpr" $
  doc "Serialize an array expression" $
  lambda "a" $
    cases R._ArrayExpr (var "a") Nothing [
      R._ArrayExpr_elements>>: lambda "es" $ Serialization.bracketList @@ Serialization.halfBlockStyle @@ (Lists.map (expressionToExpr) (var "es")),
      R._ArrayExpr_repeat>>: lambda "r" $ lets [
        "elem">: project R._ArrayRepeat R._ArrayRepeat_element @@ var "r",
        "len">: project R._ArrayRepeat R._ArrayRepeat_length @@ var "r"] $
        Serialization.cst @@ (Strings.cat (list [
          string "[",
          Serialization.printExpr @@ (expressionToExpr @@ var "elem"),
          string "; ",
          Serialization.printExpr @@ (expressionToExpr @@ var "len"),
          string "]"]))]

indexExprToExpr :: TBinding (R.IndexExpr -> Expr)
indexExprToExpr = define "indexExprToExpr" $
  doc "Serialize an index expression" $
  lambda "i" $ lets [
    "obj">: project R._IndexExpr R._IndexExpr_object @@ var "i",
    "idx">: project R._IndexExpr R._IndexExpr_index @@ var "i"] $
    Serialization.cst @@ (Strings.cat (list [
      Serialization.printExpr @@ (expressionToExpr @@ var "obj"),
      string "[",
      Serialization.printExpr @@ (expressionToExpr @@ var "idx"),
      string "]"]))

rangeExprToExpr :: TBinding (R.RangeExpr -> Expr)
rangeExprToExpr = define "rangeExprToExpr" $
  doc "Serialize a range expression" $
  lambda "r" $ lets [
    "from">: project R._RangeExpr R._RangeExpr_from @@ var "r",
    "to">: project R._RangeExpr R._RangeExpr_to @@ var "r",
    "incl">: project R._RangeExpr R._RangeExpr_inclusive @@ var "r",
    "fromStr">: Maybes.maybe (string "") (lambda "f" $ Serialization.printExpr @@ (expressionToExpr @@ var "f")) (var "from"),
    "toStr">: Maybes.maybe (string "") (lambda "t" $ Serialization.printExpr @@ (expressionToExpr @@ var "t")) (var "to"),
    "op">: Logic.ifElse (var "incl") (string "..=") (string "..")] $
    Serialization.cst @@ (Strings.cat (list [var "fromStr", var "op", var "toStr"]))

castExprToExpr :: TBinding (R.CastExpr -> Expr)
castExprToExpr = define "castExprToExpr" $
  doc "Serialize a cast expression" $
  lambda "c" $ lets [
    "expr">: project R._CastExpr R._CastExpr_expr @@ var "c",
    "typ">: project R._CastExpr R._CastExpr_type @@ var "c"] $
    Serialization.spaceSep @@ list [
      expressionToExpr @@ var "expr",
      Serialization.cst @@ string "as",
      typeToExpr @@ var "typ"]

typeAscriptionExprToExpr :: TBinding (R.TypeAscriptionExpr -> Expr)
typeAscriptionExprToExpr = define "typeAscriptionExprToExpr" $
  doc "Serialize a type ascription expression" $
  lambda "t" $ lets [
    "expr">: project R._TypeAscriptionExpr R._TypeAscriptionExpr_expr @@ var "t",
    "typ">: project R._TypeAscriptionExpr R._TypeAscriptionExpr_type @@ var "t"] $
    Serialization.spaceSep @@ list [
      expressionToExpr @@ var "expr",
      Serialization.cst @@ string ":",
      typeToExpr @@ var "typ"]

assignExprToExpr :: TBinding (R.AssignExpr -> Expr)
assignExprToExpr = define "assignExprToExpr" $
  doc "Serialize an assignment expression" $
  lambda "a" $ lets [
    "target">: project R._AssignExpr R._AssignExpr_target @@ var "a",
    "val">: project R._AssignExpr R._AssignExpr_value @@ var "a"] $
    Serialization.spaceSep @@ list [
      expressionToExpr @@ var "target",
      Serialization.cst @@ string "=",
      expressionToExpr @@ var "val"]

compoundAssignExprToExpr :: TBinding (R.CompoundAssignExpr -> Expr)
compoundAssignExprToExpr = define "compoundAssignExprToExpr" $
  doc "Serialize a compound assignment expression" $
  lambda "c" $ lets [
    "target">: project R._CompoundAssignExpr R._CompoundAssignExpr_target @@ var "c",
    "op">: project R._CompoundAssignExpr R._CompoundAssignExpr_op @@ var "c",
    "val">: project R._CompoundAssignExpr R._CompoundAssignExpr_value @@ var "c",
    "opStr">: cases R._CompoundAssignOp (var "op") Nothing [
      R._CompoundAssignOp_addAssign>>: constant $ string "+=",
      R._CompoundAssignOp_subAssign>>: constant $ string "-=",
      R._CompoundAssignOp_mulAssign>>: constant $ string "*=",
      R._CompoundAssignOp_divAssign>>: constant $ string "/=",
      R._CompoundAssignOp_remAssign>>: constant $ string "%=",
      R._CompoundAssignOp_bitAndAssign>>: constant $ string "&=",
      R._CompoundAssignOp_bitOrAssign>>: constant $ string "|=",
      R._CompoundAssignOp_bitXorAssign>>: constant $ string "^=",
      R._CompoundAssignOp_shlAssign>>: constant $ string "<<=",
      R._CompoundAssignOp_shrAssign>>: constant $ string ">>="]] $
    Serialization.spaceSep @@ list [
      expressionToExpr @@ var "target",
      Serialization.cst @@ var "opStr",
      expressionToExpr @@ var "val"]

macroInvocationToExpr :: TBinding (R.MacroInvocation -> Expr)
macroInvocationToExpr = define "macroInvocationToExpr" $
  doc "Serialize a macro invocation" $
  lambda "m" $ lets [
    "path">: project R._MacroInvocation R._MacroInvocation_path @@ var "m",
    "delim">: project R._MacroInvocation R._MacroInvocation_delimiter @@ var "m",
    "tokens">: project R._MacroInvocation R._MacroInvocation_tokens @@ var "m",
    "pathStr">: Strings.intercalate (string "::") (var "path"),
    "open">: cases R._MacroDelimiter (var "delim") Nothing [
      R._MacroDelimiter_paren>>: constant $ string "(",
      R._MacroDelimiter_bracket>>: constant $ string "[",
      R._MacroDelimiter_brace>>: constant $ string "{"],
    "close">: cases R._MacroDelimiter (var "delim") Nothing [
      R._MacroDelimiter_paren>>: constant $ string ")",
      R._MacroDelimiter_bracket>>: constant $ string "]",
      R._MacroDelimiter_brace>>: constant $ string "}"]] $
    Serialization.cst @@ (Strings.cat (list [var "pathStr", string "!", var "open", var "tokens", var "close"]))

-- =============================================================================
-- Statements
-- =============================================================================

statementToExpr :: TBinding (R.Statement -> Expr)
statementToExpr = define "statementToExpr" $
  doc "Serialize a statement" $
  lambda "stmt" $
    cases R._Statement (var "stmt") Nothing [
      R._Statement_let>>: lambda "l" $ letStatementToExpr @@ var "l",
      R._Statement_expression>>: lambda "e" $ Serialization.spaceSep @@ list [expressionToExpr @@ var "e", Serialization.cst @@ string ";"],
      R._Statement_item>>: lambda "i" $ itemToExpr @@ var "i",
      R._Statement_empty>>: constant $ Serialization.cst @@ string ";"]

letStatementToExpr :: TBinding (R.LetStatement -> Expr)
letStatementToExpr = define "letStatementToExpr" $
  doc "Serialize a let statement" $
  lambda "l" $ lets [
    "pat">: project R._LetStatement R._LetStatement_pattern @@ var "l",
    "mut">: project R._LetStatement R._LetStatement_mutable @@ var "l",
    "typ">: project R._LetStatement R._LetStatement_type @@ var "l",
    "init">: project R._LetStatement R._LetStatement_init @@ var "l",
    "mutKw">: Logic.ifElse (var "mut") (just $ Serialization.cst @@ string "mut") nothing,
    "typPart">: Maybes.maybe nothing (lambda "t" $ just $ Serialization.spaceSep @@ list [
      Serialization.cst @@ string ":",
      typeToExpr @@ var "t"]) (var "typ"),
    "initPart">: Maybes.maybe nothing (lambda "e" $ just $ Serialization.spaceSep @@ list [
      Serialization.cst @@ string "=",
      expressionToExpr @@ var "e"]) (var "init")] $
    Serialization.spaceSep @@ Maybes.cat (list [
      just $ Serialization.cst @@ string "let",
      var "mutKw",
      just $ patternToExpr @@ var "pat",
      var "typPart",
      var "initPart",
      just $ Serialization.cst @@ string ";"])

blockToExpr :: TBinding (R.Block -> Expr)
blockToExpr = define "blockToExpr" $
  doc "Serialize a block" $
  lambda "b" $ lets [
    "stmts">: project R._Block R._Block_statements @@ var "b",
    "expr">: project R._Block R._Block_expression @@ var "b",
    "stmtExprs">: Lists.map (statementToExpr) (var "stmts"),
    "exprPart">: Maybes.maybe (list ([] :: [TTerm Expr])) (lambda "e" $ list [expressionToExpr @@ var "e"]) (var "expr"),
    "allParts">: Lists.concat2 (var "stmtExprs") (var "exprPart")] $
    Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@ var "allParts"

-- =============================================================================
-- Patterns
-- =============================================================================

patternToExpr :: TBinding (R.Pattern -> Expr)
patternToExpr = define "patternToExpr" $
  doc "Serialize a pattern" $
  lambda "pat" $
    cases R._Pattern (var "pat") Nothing [
      R._Pattern_wildcard>>: constant $ Serialization.cst @@ string "_",
      R._Pattern_identifier>>: lambda "ip" $ identifierPatternToExpr @@ var "ip",
      R._Pattern_literal>>: lambda "l" $ literalToExpr @@ var "l",
      R._Pattern_reference>>: lambda "rp" $ refPatternToExpr @@ var "rp",
      R._Pattern_struct>>: lambda "sp" $ structPatternToExpr @@ var "sp",
      R._Pattern_tupleStruct>>: lambda "tsp" $ tupleStructPatternToExpr @@ var "tsp",
      R._Pattern_tuple>>: lambda "ps" $ Serialization.parenList @@ false @@ (Lists.map (patternToExpr) (var "ps")),
      R._Pattern_slice>>: lambda "ps" $ Serialization.bracketList @@ Serialization.halfBlockStyle @@ (Lists.map (patternToExpr) (var "ps")),
      R._Pattern_or>>: lambda "ps" $ Serialization.cst @@ (Strings.intercalate (string " | ") (Lists.map (lambda "p" $ Serialization.printExpr @@ (patternToExpr @@ var "p")) (var "ps"))),
      R._Pattern_path>>: lambda "ep" $ exprPathToExpr @@ var "ep",
      R._Pattern_range>>: lambda "rp" $ rangePatternToExpr @@ var "rp",
      R._Pattern_rest>>: constant $ Serialization.cst @@ string "..",
      R._Pattern_paren>>: lambda "p" $ Serialization.parenthesize @@ (patternToExpr @@ var "p")]

identifierPatternToExpr :: TBinding (R.IdentifierPattern -> Expr)
identifierPatternToExpr = define "identifierPatternToExpr" $
  doc "Serialize an identifier pattern" $
  lambda "ip" $ lets [
    "name">: project R._IdentifierPattern R._IdentifierPattern_name @@ var "ip",
    "mut">: project R._IdentifierPattern R._IdentifierPattern_mutable @@ var "ip",
    "atPat">: project R._IdentifierPattern R._IdentifierPattern_atPattern @@ var "ip",
    "mutKw">: Logic.ifElse (var "mut") (just $ Serialization.cst @@ string "mut") nothing,
    "atPart">: Maybes.maybe nothing (lambda "p" $ just $ Serialization.spaceSep @@ list [
      Serialization.cst @@ string "@",
      patternToExpr @@ var "p"]) (var "atPat")] $
    Serialization.spaceSep @@ Maybes.cat (list [
      var "mutKw",
      just $ Serialization.cst @@ var "name",
      var "atPart"])

refPatternToExpr :: TBinding (R.RefPattern -> Expr)
refPatternToExpr = define "refPatternToExpr" $
  doc "Serialize a reference pattern" $
  lambda "rp" $ lets [
    "mut">: project R._RefPattern R._RefPattern_mutable @@ var "rp",
    "pat">: project R._RefPattern R._RefPattern_pattern @@ var "rp",
    "prefix">: Logic.ifElse (var "mut") (string "&mut ") (string "&")] $
    Serialization.cst @@ (Strings.cat2 (var "prefix") (Serialization.printExpr @@ (patternToExpr @@ var "pat")))

structPatternToExpr :: TBinding (R.StructPattern -> Expr)
structPatternToExpr = define "structPatternToExpr" $
  doc "Serialize a struct pattern" $
  lambda "sp" $ lets [
    "path">: project R._StructPattern R._StructPattern_path @@ var "sp",
    "fields">: project R._StructPattern R._StructPattern_fields @@ var "sp",
    "rest">: project R._StructPattern R._StructPattern_rest @@ var "sp",
    "fieldExprs">: Lists.map (fieldPatternToExpr) (var "fields"),
    "restExpr">: Logic.ifElse (var "rest") (list [Serialization.cst @@ string ".."]) (list ([] :: [TTerm Expr])),
    "allFields">: Lists.concat2 (var "fieldExprs") (var "restExpr")] $
    Serialization.spaceSep @@ list [
      exprPathToExpr @@ var "path",
      Serialization.curlyBracesList @@ nothing @@ Serialization.halfBlockStyle @@ var "allFields"]

fieldPatternToExpr :: TBinding (R.FieldPattern -> Expr)
fieldPatternToExpr = define "fieldPatternToExpr" $
  doc "Serialize a field pattern" $
  lambda "fp" $ lets [
    "name">: project R._FieldPattern R._FieldPattern_name @@ var "fp",
    "pat">: project R._FieldPattern R._FieldPattern_pattern @@ var "fp"] $
    Maybes.maybe
      (Serialization.cst @@ var "name")
      (lambda "p" $ Serialization.spaceSep @@ list [
        Serialization.cst @@ (Strings.cat2 (var "name") (string ":")),
        patternToExpr @@ var "p"])
      (var "pat")

tupleStructPatternToExpr :: TBinding (R.TupleStructPattern -> Expr)
tupleStructPatternToExpr = define "tupleStructPatternToExpr" $
  doc "Serialize a tuple struct pattern" $
  lambda "tsp" $ lets [
    "path">: project R._TupleStructPattern R._TupleStructPattern_path @@ var "tsp",
    "elems">: project R._TupleStructPattern R._TupleStructPattern_elements @@ var "tsp"] $
    Serialization.spaceSep @@ list [
      exprPathToExpr @@ var "path",
      Serialization.parenList @@ false @@ (Lists.map (patternToExpr) (var "elems"))]

rangePatternToExpr :: TBinding (R.RangePattern -> Expr)
rangePatternToExpr = define "rangePatternToExpr" $
  doc "Serialize a range pattern" $
  lambda "rp" $ lets [
    "from">: project R._RangePattern R._RangePattern_from @@ var "rp",
    "to">: project R._RangePattern R._RangePattern_to @@ var "rp",
    "incl">: project R._RangePattern R._RangePattern_inclusive @@ var "rp",
    "fromStr">: Maybes.maybe (string "") (lambda "p" $ Serialization.printExpr @@ (patternToExpr @@ var "p")) (var "from"),
    "toStr">: Maybes.maybe (string "") (lambda "p" $ Serialization.printExpr @@ (patternToExpr @@ var "p")) (var "to"),
    "op">: Logic.ifElse (var "incl") (string "..=") (string "..")] $
    Serialization.cst @@ (Strings.cat (list [var "fromStr", var "op", var "toStr"]))

-- =============================================================================
-- Literals
-- =============================================================================

literalToExpr :: TBinding (R.Literal -> Expr)
literalToExpr = define "literalToExpr" $
  doc "Serialize a literal" $
  lambda "lit" $
    cases R._Literal (var "lit") Nothing [
      R._Literal_integer>>: lambda "il" $ integerLiteralToExpr @@ var "il",
      R._Literal_float>>: lambda "fl" $ floatLiteralToExpr @@ var "fl",
      R._Literal_string>>: lambda "s" $ Serialization.cst @@ (Literals.showString $ var "s"),
      R._Literal_rawString>>: lambda "s" $ Serialization.cst @@ (Strings.cat (list [string "r\"", var "s", string "\""])),
      R._Literal_byteString>>: lambda "bs" $ Serialization.cst @@ string "b\"...\"", -- TODO: Proper binary encoding
      R._Literal_char>>: lambda "c" $ Serialization.cst @@ (Strings.cat (list [string "'", Literals.showUint32 $ var "c", string "'"])),
      R._Literal_byte>>: lambda "b" $ Serialization.cst @@ (Strings.cat (list [string "b'", Literals.showUint8 $ var "b", string "'"])),
      R._Literal_bool>>: lambda "b" $ Serialization.cst @@ (Logic.ifElse (var "b") (string "true") (string "false"))]

integerLiteralToExpr :: TBinding (R.IntegerLiteral -> Expr)
integerLiteralToExpr = define "integerLiteralToExpr" $
  doc "Serialize an integer literal" $
  lambda "il" $ lets [
    "val">: project R._IntegerLiteral R._IntegerLiteral_value @@ var "il",
    "suf">: project R._IntegerLiteral R._IntegerLiteral_suffix @@ var "il",
    "valStr">: Literals.showBigint $ var "val",
    "sufStr">: Maybes.maybe (string "") (lambda "s" $ var "s") (var "suf")] $
    Serialization.cst @@ (Strings.cat2 (var "valStr") (var "sufStr"))

floatLiteralToExpr :: TBinding (R.FloatLiteral -> Expr)
floatLiteralToExpr = define "floatLiteralToExpr" $
  doc "Serialize a float literal" $
  lambda "fl" $ lets [
    "val">: project R._FloatLiteral R._FloatLiteral_value @@ var "fl",
    "suf">: project R._FloatLiteral R._FloatLiteral_suffix @@ var "fl",
    "valStr">: Literals.showFloat64 $ var "val",
    "sufStr">: Maybes.maybe (string "") (lambda "s" $ var "s") (var "suf")] $
    Serialization.cst @@ (Strings.cat2 (var "valStr") (var "sufStr"))

-- =============================================================================
-- Visibility and attributes
-- =============================================================================

visibilityToExpr :: TBinding (R.Visibility -> Maybe Expr)
visibilityToExpr = define "visibilityToExpr" $
  doc "Serialize visibility to an optional expression" $
  lambda "vis" $
    cases R._Visibility (var "vis") Nothing [
      R._Visibility_public>>: constant $ just $ Serialization.cst @@ string "pub",
      R._Visibility_crate>>: constant $ just $ Serialization.cst @@ string "pub(crate)",
      R._Visibility_restricted>>: lambda "path" $
        just $ Serialization.cst @@ (Strings.cat (list [
          string "pub(in ",
          Strings.intercalate (string "::") (var "path"),
          string ")"])),
      R._Visibility_private>>: constant nothing]

attributeToExpr :: TBinding (R.Attribute -> Expr)
attributeToExpr = define "attributeToExpr" $
  doc "Serialize an attribute" $
  lambda "attr" $ lets [
    "inner">: project R._Attribute R._Attribute_inner @@ var "attr",
    "path">: project R._Attribute R._Attribute_path @@ var "attr",
    "tokens">: project R._Attribute R._Attribute_tokens @@ var "attr",
    "prefix">: Logic.ifElse (var "inner") (string "#![") (string "#["),
    "pathStr">: Strings.intercalate (string "::") (var "path"),
    "tokensPart">: Maybes.maybe (string "") (lambda "t" $ Strings.cat (list [string "(", var "t", string ")"])) (var "tokens")] $
    Serialization.cst @@ (Strings.cat (list [var "prefix", var "pathStr", var "tokensPart", string "]"]))

derivesToExpr :: TBinding ([String] -> Maybe Expr)
derivesToExpr = define "derivesToExpr" $
  doc "Serialize derive macros to an attribute expression" $
  lambda "derives" $
    Logic.ifElse (Lists.null $ var "derives")
      nothing
      (just $ Serialization.cst @@ (Strings.cat (list [
        string "#[derive(",
        Strings.intercalate (string ", ") (var "derives"),
        string ")]"])))

-- =============================================================================
-- Comments
-- =============================================================================

toRustDocComment :: TBinding (String -> String)
toRustDocComment = define "toRustDocComment" $
  doc "Convert a string to Rust doc comments" $
  lambda "c" $ Strings.intercalate (string "\n") $ Lists.map (lambda "s" $ Strings.cat2 (string "/// ") (var "s")) (Strings.lines $ var "c")

toRustComment :: TBinding (String -> String)
toRustComment = define "toRustComment" $
  doc "Convert a string to Rust line comments" $
  lambda "c" $ Strings.intercalate (string "\n") $ Lists.map (lambda "s" $ Strings.cat2 (string "// ") (var "s")) (Strings.lines $ var "c")
