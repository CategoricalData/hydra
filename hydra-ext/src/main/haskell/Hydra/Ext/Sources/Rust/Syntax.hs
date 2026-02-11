-- | A Rust syntax model, based on the Rust Reference grammar.
-- | See https://doc.rust-lang.org/reference/ (retrieved 2025-01-29)

module Hydra.Ext.Sources.Rust.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


ns :: Namespace
ns = Namespace "hydra.ext.rust.syntax"

define :: String -> Type -> Binding
define = datatype ns

rust :: String -> Type
rust = typeref ns

module_ :: Module
module_ = Module ns elements [Core.ns] [Core.ns] $
    Just ("A Rust syntax model, based on the Rust Reference grammar"
      ++ " (https://doc.rust-lang.org/reference/), retrieved 2025-01-29")
  where
    elements = crateLevel ++ items ++ useDeclarations ++ structDefinitions
      ++ enumDefinitions ++ functionDefinitions ++ typeAliases ++ constStaticMod
      ++ implBlocks ++ traitDefinitions ++ genericsAndWhere ++ types
      ++ expressions ++ statements ++ patterns ++ literals ++ attributesVisibility
      ++ helperTypes

    -- Crate-level constructs
    crateLevel = [
      crate,
      item,
      itemWithComments]

    -- Item definitions
    items = []

    -- Use declarations
    useDeclarations = [
      useDeclaration,
      useTree,
      usePath,
      useRename,
      useGroup]

    -- Struct definitions
    structDefinitions = [
      structDef,
      structBody,
      structField,
      tupleField]

    -- Enum definitions
    enumDefinitions = [
      enumDef,
      enumVariant,
      enumVariantBody]

    -- Function definitions
    functionDefinitions = [
      fnDef,
      fnParam,
      selfParam,
      methodParam]

    -- Type aliases
    typeAliases = [
      typeAlias]

    -- Const, static, and module definitions
    constStaticMod = [
      constDef,
      staticDef,
      modDef]

    -- Impl blocks
    implBlocks = [
      implBlock,
      implItem,
      implMethod]

    -- Trait definitions
    traitDefinitions = [
      traitDef,
      traitItem,
      traitMethod,
      traitType,
      traitConst]

    -- Generics and where clauses
    genericsAndWhere = [
      genericParam,
      typeParamBound,
      lifetime,
      whereClause,
      wherePredicate]

    -- Type expressions
    types = [
      type_,
      typePath,
      pathSegment,
      genericArguments,
      angleBracketedArgs,
      genericArg,
      typeBinding,
      parenthesizedArgs,
      referenceType,
      arrayType,
      fnPointerType,
      rawPointerType]

    -- Expressions
    expressions = [
      expression,
      exprPath,
      callExpr,
      methodCallExpr,
      fieldAccessExpr,
      tupleIndexExpr,
      closureExpr,
      closureParam,
      ifExpr,
      ifCondition,
      matchExpr,
      matchArm,
      loopExpr,
      whileExpr,
      forExpr,
      binaryExpr,
      binaryOp,
      unaryExpr,
      unaryOp,
      refExpr,
      structExpr,
      fieldValue,
      arrayExpr,
      indexExpr,
      rangeExpr,
      castExpr,
      typeAscriptionExpr,
      assignExpr,
      compoundAssignExpr,
      compoundAssignOp,
      macroInvocation,
      macroDelimiter]

    -- Statements
    statements = [
      statement,
      letStatement,
      block]

    -- Patterns
    patterns = [
      pattern,
      identifierPattern,
      refPattern,
      structPattern,
      fieldPattern,
      tupleStructPattern,
      rangePattern]

    -- Literals
    literals = [
      literal,
      integerLiteral,
      floatLiteral]

    -- Attributes and visibility
    attributesVisibility = [
      attribute,
      visibility]

    -- Helper types (used in expressions)
    helperTypes = [
      letCondition,
      arrayRepeat]


-- ================================================================================================
-- Crate-level / Module structure
-- ================================================================================================

crate :: Binding
crate = define "Crate" $
  doc "A Rust crate, represented as a collection of top-level items" $
  T.record [
    "items">:
      doc "The top-level items in the crate" $
      T.list itemWithComments]

item :: Binding
item = define "Item" $
  doc "A top-level item in a Rust module or crate" $
  T.union [
    "use">:
      doc "A use declaration" $
      rust "UseDeclaration",
    "struct">:
      doc "A struct definition" $
      rust "StructDef",
    "enum">:
      doc "An enum definition" $
      rust "EnumDef",
    "fn">:
      doc "A function definition" $
      rust "FnDef",
    "typeAlias">:
      doc "A type alias" $
      rust "TypeAlias",
    "impl">:
      doc "An impl block" $
      rust "ImplBlock",
    "trait">:
      doc "A trait definition" $
      rust "TraitDef",
    "mod">:
      doc "A module definition" $
      rust "ModDef",
    "const">:
      doc "A constant item" $
      rust "ConstDef",
    "static">:
      doc "A static item" $
      rust "StaticDef",
    "macro">:
      doc "A macro invocation as an item" $
      rust "MacroInvocation"]

itemWithComments :: Binding
itemWithComments = define "ItemWithComments" $
  doc "An item together with optional doc comments and visibility" $
  T.record [
    "doc">:
      doc "Optional documentation comment" $
      T.maybe T.string,
    "visibility">:
      doc "The item's visibility" $
      rust "Visibility",
    "item">:
      doc "The item itself" $
      rust "Item"]


-- ================================================================================================
-- Use declarations
-- ================================================================================================

useDeclaration :: Binding
useDeclaration = define "UseDeclaration" $
  doc "A use declaration (e.g., use std::collections::BTreeMap;)" $
  T.record [
    "public">:
      doc "Whether the use is public (pub use)" $
      T.boolean,
    "tree">:
      doc "The use tree describing what is imported" $
      rust "UseTree"]

useTree :: Binding
useTree = define "UseTree" $
  doc "A use tree, representing the structure of a use path" $
  T.union [
    "path">:
      doc "A simple path import" $
      rust "UsePath",
    "rename">:
      doc "A renamed import (e.g., BTreeMap as Map)" $
      rust "UseRename",
    "glob">:
      doc "A glob import (e.g., std::collections::*)" $
      T.list T.string,
    "group">:
      doc "A grouped import (e.g., {BTreeMap, BTreeSet})" $
      rust "UseGroup"]

usePath :: Binding
usePath = define "UsePath" $
  doc "A simple path import within a use tree" $
  T.record [
    "segments">:
      doc "The path segments" $
      T.list T.string]

useRename :: Binding
useRename = define "UseRename" $
  doc "A renamed import (e.g., BTreeMap as Map)" $
  T.record [
    "path">:
      doc "The original path segments" $
      T.list T.string,
    "alias">:
      doc "The alias name" $
      T.string]

useGroup :: Binding
useGroup = define "UseGroup" $
  doc "A grouped import (e.g., std::collections::{BTreeMap, BTreeSet})" $
  T.record [
    "prefix">:
      doc "The common prefix path segments" $
      T.list T.string,
    "trees">:
      doc "The individual subtrees within the group" $
      T.list $ rust "UseTree"]


-- ================================================================================================
-- Struct definition
-- ================================================================================================

structDef :: Binding
structDef = define "StructDef" $
  doc "A struct definition (e.g., struct Foo<T> { bar: T })" $
  T.record [
    "name">:
      doc "The struct name" $
      T.string,
    "generics">:
      doc "Generic type parameters" $
      T.list $ rust "GenericParam",
    "whereClause">:
      doc "Optional where clause" $
      T.maybe $ rust "WhereClause",
    "body">:
      doc "The struct body (named fields, tuple fields, or unit)" $
      rust "StructBody",
    "derives">:
      doc "Derive macros to apply" $
      T.list T.string,
    "public">:
      doc "Whether the struct is public" $
      T.boolean,
    "doc">:
      doc "Optional doc comment" $
      T.maybe T.string]

structBody :: Binding
structBody = define "StructBody" $
  doc "The body of a struct definition" $
  T.union [
    "named">:
      doc "A struct with named fields" $
      T.list $ rust "StructField",
    "tuple">:
      doc "A tuple struct" $
      T.list $ rust "TupleField",
    "unit">:
      doc "A unit struct" $
      T.unit]

structField :: Binding
structField = define "StructField" $
  doc "A named field within a struct definition" $
  T.record [
    "name">:
      doc "The field name" $
      T.string,
    "type">:
      doc "The field type" $
      rust "Type",
    "public">:
      doc "Whether the field is public" $
      T.boolean,
    "doc">:
      doc "Optional doc comment for the field" $
      T.maybe T.string]

tupleField :: Binding
tupleField = define "TupleField" $
  doc "A positional field within a tuple struct" $
  T.record [
    "type">:
      doc "The field type" $
      rust "Type",
    "public">:
      doc "Whether the field is public" $
      T.boolean]


-- ================================================================================================
-- Enum definition
-- ================================================================================================

enumDef :: Binding
enumDef = define "EnumDef" $
  doc "An enum definition (e.g., enum Foo<T> { Bar(T), Baz { x: i32 } })" $
  T.record [
    "name">:
      doc "The enum name" $
      T.string,
    "generics">:
      doc "Generic type parameters" $
      T.list $ rust "GenericParam",
    "whereClause">:
      doc "Optional where clause" $
      T.maybe $ rust "WhereClause",
    "variants">:
      doc "The enum variants" $
      T.list $ rust "EnumVariant",
    "derives">:
      doc "Derive macros to apply" $
      T.list T.string,
    "public">:
      doc "Whether the enum is public" $
      T.boolean,
    "doc">:
      doc "Optional doc comment" $
      T.maybe T.string]

enumVariant :: Binding
enumVariant = define "EnumVariant" $
  doc "A variant of an enum definition" $
  T.record [
    "name">:
      doc "The variant name" $
      T.string,
    "body">:
      doc "The variant body" $
      rust "EnumVariantBody",
    "doc">:
      doc "Optional doc comment for the variant" $
      T.maybe T.string]

enumVariantBody :: Binding
enumVariantBody = define "EnumVariantBody" $
  doc "The body of an enum variant" $
  T.union [
    "unit">:
      doc "A unit variant (e.g., Foo)" $
      T.unit,
    "tuple">:
      doc "A tuple variant (e.g., Foo(i32, String))" $
      T.list $ rust "Type",
    "struct">:
      doc "A struct variant (e.g., Foo { x: i32 })" $
      T.list $ rust "StructField"]


-- ================================================================================================
-- Function definition
-- ================================================================================================

fnDef :: Binding
fnDef = define "FnDef" $
  doc "A function definition (e.g., fn foo<T>(x: T) -> String { ... })" $
  T.record [
    "name">:
      doc "The function name" $
      T.string,
    "generics">:
      doc "Generic type parameters" $
      T.list $ rust "GenericParam",
    "whereClause">:
      doc "Optional where clause" $
      T.maybe $ rust "WhereClause",
    "params">:
      doc "The function parameters" $
      T.list $ rust "FnParam",
    "returnType">:
      doc "The return type (None means ())" $
      T.maybe $ rust "Type",
    "body">:
      doc "The function body" $
      rust "Block",
    "public">:
      doc "Whether the function is public" $
      T.boolean,
    "async">:
      doc "Whether the function is async" $
      T.boolean,
    "const">:
      doc "Whether the function is const" $
      T.boolean,
    "unsafe">:
      doc "Whether the function is unsafe" $
      T.boolean,
    "doc">:
      doc "Optional doc comment" $
      T.maybe T.string]

fnParam :: Binding
fnParam = define "FnParam" $
  doc "A function parameter" $
  T.record [
    "pattern">:
      doc "The parameter pattern" $
      rust "Pattern",
    "type">:
      doc "The parameter type" $
      rust "Type"]

selfParam :: Binding
selfParam = define "SelfParam" $
  doc "A self parameter in a method" $
  T.enum ["owned", "ref", "refMut"]

methodParam :: Binding
methodParam = define "MethodParam" $
  doc "A method parameter, which may be self or a regular parameter" $
  T.union [
    "self">:
      doc "A self parameter" $
      rust "SelfParam",
    "regular">:
      doc "A regular function parameter" $
      rust "FnParam"]


-- ================================================================================================
-- Type alias
-- ================================================================================================

typeAlias :: Binding
typeAlias = define "TypeAlias" $
  doc "A type alias definition (e.g., type Foo<T> = Bar<T>;)" $
  T.record [
    "name">:
      doc "The alias name" $
      T.string,
    "generics">:
      doc "Generic type parameters" $
      T.list $ rust "GenericParam",
    "type">:
      doc "The aliased type" $
      rust "Type",
    "public">:
      doc "Whether the alias is public" $
      T.boolean,
    "doc">:
      doc "Optional doc comment" $
      T.maybe T.string]


-- ================================================================================================
-- Const, static, and module definitions
-- ================================================================================================

constDef :: Binding
constDef = define "ConstDef" $
  doc "A constant item (e.g., const FOO: u32 = 42;)" $
  T.record [
    "name">:
      doc "The constant name" $
      T.string,
    "type">:
      doc "The constant type" $
      rust "Type",
    "value">:
      doc "The constant value expression" $
      rust "Expression",
    "public">:
      doc "Whether the constant is public" $
      T.boolean,
    "doc">:
      doc "Optional doc comment" $
      T.maybe T.string]

staticDef :: Binding
staticDef = define "StaticDef" $
  doc "A static item (e.g., static FOO: u32 = 42;)" $
  T.record [
    "name">:
      doc "The static name" $
      T.string,
    "type">:
      doc "The static type" $
      rust "Type",
    "value">:
      doc "The static value expression" $
      rust "Expression",
    "mutable">:
      doc "Whether the static is mutable" $
      T.boolean,
    "public">:
      doc "Whether the static is public" $
      T.boolean,
    "doc">:
      doc "Optional doc comment" $
      T.maybe T.string]

modDef :: Binding
modDef = define "ModDef" $
  doc "A module definition (either inline or external)" $
  T.record [
    "name">:
      doc "The module name" $
      T.string,
    "body">:
      doc "The module body (None for external file)" $
      T.maybe $ T.list $ rust "Item",
    "public">:
      doc "Whether the module is public" $
      T.boolean,
    "doc">:
      doc "Optional doc comment" $
      T.maybe T.string]


-- ================================================================================================
-- Impl block
-- ================================================================================================

implBlock :: Binding
implBlock = define "ImplBlock" $
  doc "An impl block (e.g., impl<T> Trait for Foo<T> { ... })" $
  T.record [
    "generics">:
      doc "Generic type parameters" $
      T.list $ rust "GenericParam",
    "whereClause">:
      doc "Optional where clause" $
      T.maybe $ rust "WhereClause",
    "trait">:
      doc "The trait being implemented, if any" $
      T.maybe $ rust "TypePath",
    "negative">:
      doc "Whether this is a negative impl" $
      T.boolean,
    "selfType">:
      doc "The type being implemented for" $
      rust "Type",
    "items">:
      doc "The items within the impl block" $
      T.list $ rust "ImplItem"]

implItem :: Binding
implItem = define "ImplItem" $
  doc "An item within an impl block" $
  T.union [
    "method">:
      doc "A method definition" $
      rust "ImplMethod",
    "type">:
      doc "An associated type definition" $
      rust "TypeAlias",
    "const">:
      doc "An associated constant" $
      rust "ConstDef"]

implMethod :: Binding
implMethod = define "ImplMethod" $
  doc "A method within an impl block" $
  T.record [
    "name">:
      doc "The method name" $
      T.string,
    "generics">:
      doc "Generic type parameters" $
      T.list $ rust "GenericParam",
    "whereClause">:
      doc "Optional where clause" $
      T.maybe $ rust "WhereClause",
    "params">:
      doc "The method parameters (including self)" $
      T.list $ rust "MethodParam",
    "returnType">:
      doc "The return type (None means ())" $
      T.maybe $ rust "Type",
    "body">:
      doc "The method body" $
      rust "Block",
    "public">:
      doc "Whether the method is public" $
      T.boolean,
    "default">:
      doc "Whether the method has a default implementation" $
      T.boolean,
    "doc">:
      doc "Optional doc comment" $
      T.maybe T.string]


-- ================================================================================================
-- Trait definition
-- ================================================================================================

traitDef :: Binding
traitDef = define "TraitDef" $
  doc "A trait definition (e.g., trait Foo<T>: Bar + Baz { ... })" $
  T.record [
    "name">:
      doc "The trait name" $
      T.string,
    "generics">:
      doc "Generic type parameters" $
      T.list $ rust "GenericParam",
    "whereClause">:
      doc "Optional where clause" $
      T.maybe $ rust "WhereClause",
    "superTraits">:
      doc "Supertraits" $
      T.list $ rust "TypeParamBound",
    "items">:
      doc "The items within the trait" $
      T.list $ rust "TraitItem",
    "public">:
      doc "Whether the trait is public" $
      T.boolean,
    "unsafe">:
      doc "Whether the trait is unsafe" $
      T.boolean,
    "doc">:
      doc "Optional doc comment" $
      T.maybe T.string]

traitItem :: Binding
traitItem = define "TraitItem" $
  doc "An item within a trait definition" $
  T.union [
    "method">:
      doc "A method signature or default method" $
      rust "TraitMethod",
    "type">:
      doc "An associated type" $
      rust "TraitType",
    "const">:
      doc "An associated constant" $
      rust "TraitConst"]

traitMethod :: Binding
traitMethod = define "TraitMethod" $
  doc "A method signature or default method within a trait" $
  T.record [
    "name">:
      doc "The method name" $
      T.string,
    "generics">:
      doc "Generic type parameters" $
      T.list $ rust "GenericParam",
    "whereClause">:
      doc "Optional where clause" $
      T.maybe $ rust "WhereClause",
    "params">:
      doc "The method parameters (including self)" $
      T.list $ rust "MethodParam",
    "returnType">:
      doc "The return type" $
      T.maybe $ rust "Type",
    "defaultBody">:
      doc "Optional default body" $
      T.maybe $ rust "Block",
    "doc">:
      doc "Optional doc comment" $
      T.maybe T.string]

traitType :: Binding
traitType = define "TraitType" $
  doc "An associated type within a trait" $
  T.record [
    "name">:
      doc "The associated type name" $
      T.string,
    "bounds">:
      doc "Type parameter bounds" $
      T.list $ rust "TypeParamBound",
    "default">:
      doc "Optional default type" $
      T.maybe $ rust "Type",
    "doc">:
      doc "Optional doc comment" $
      T.maybe T.string]

traitConst :: Binding
traitConst = define "TraitConst" $
  doc "An associated constant within a trait" $
  T.record [
    "name">:
      doc "The constant name" $
      T.string,
    "type">:
      doc "The constant type" $
      rust "Type",
    "default">:
      doc "Optional default value" $
      T.maybe $ rust "Expression",
    "doc">:
      doc "Optional doc comment" $
      T.maybe T.string]


-- ================================================================================================
-- Generics and where clauses
-- ================================================================================================

genericParam :: Binding
genericParam = define "GenericParam" $
  doc "A generic type parameter (e.g., T: Clone + Debug)" $
  T.record [
    "name">:
      doc "The parameter name" $
      T.string,
    "bounds">:
      doc "Trait bounds on the parameter" $
      T.list $ rust "TypeParamBound"]

typeParamBound :: Binding
typeParamBound = define "TypeParamBound" $
  doc "A bound on a type parameter" $
  T.union [
    "trait">:
      doc "A trait bound" $
      rust "TypePath",
    "lifetime">:
      doc "A lifetime bound" $
      rust "Lifetime"]

lifetime :: Binding
lifetime = define "Lifetime" $
  doc "A lifetime (e.g., 'a, 'static)" $
  T.record [
    "name">:
      doc "The lifetime name (without the leading quote)" $
      T.string]

whereClause :: Binding
whereClause = define "WhereClause" $
  doc "A where clause (e.g., where T: Clone, U: Debug)" $
  T.record [
    "predicates">:
      doc "The predicates in the where clause" $
      T.list $ rust "WherePredicate"]

wherePredicate :: Binding
wherePredicate = define "WherePredicate" $
  doc "A single predicate in a where clause" $
  T.record [
    "type">:
      doc "The type being constrained" $
      rust "Type",
    "bounds">:
      doc "The bounds on the type" $
      T.list $ rust "TypeParamBound"]


-- ================================================================================================
-- Types
-- ================================================================================================

type_ :: Binding
type_ = define "Type" $
  doc "A Rust type expression" $
  T.union [
    "path">:
      doc "A path type, possibly with generic arguments" $
      rust "TypePath",
    "reference">:
      doc "A reference type" $
      rust "ReferenceType",
    "slice">:
      doc "A slice type" $
      rust "Type",
    "array">:
      doc "An array type with a fixed size" $
      rust "ArrayType",
    "tuple">:
      doc "A tuple type" $
      T.list $ rust "Type",
    "fnPointer">:
      doc "A function pointer type" $
      rust "FnPointerType",
    "implTrait">:
      doc "An impl Trait type" $
      T.list $ rust "TypeParamBound",
    "dynTrait">:
      doc "A dyn Trait type" $
      T.list $ rust "TypeParamBound",
    "inferred">:
      doc "The inferred type placeholder (_)" $
      T.unit,
    "unit">:
      doc "The unit type (())" $
      T.unit,
    "never">:
      doc "The never type (!)" $
      T.unit,
    "rawPointer">:
      doc "A raw pointer type" $
      rust "RawPointerType",
    "macro">:
      doc "A macro invocation in type position" $
      rust "MacroInvocation"]

typePath :: Binding
typePath = define "TypePath" $
  doc "A path-based type, possibly with generic arguments" $
  T.record [
    "global">:
      doc "Whether the path is absolute (starts with ::)" $
      T.boolean,
    "segments">:
      doc "The segments of the path" $
      T.list $ rust "PathSegment"]

pathSegment :: Binding
pathSegment = define "PathSegment" $
  doc "A segment within a type path" $
  T.record [
    "name">:
      doc "The segment name" $
      T.string,
    "arguments">:
      doc "Generic arguments, if any" $
      rust "GenericArguments"]

genericArguments :: Binding
genericArguments = define "GenericArguments" $
  doc "Generic arguments to a path segment" $
  T.union [
    "none">:
      doc "No generic arguments" $
      T.unit,
    "angleBracketed">:
      doc "Angle-bracketed arguments" $
      rust "AngleBracketedArgs",
    "parenthesized">:
      doc "Parenthesized arguments for Fn traits" $
      rust "ParenthesizedArgs"]

angleBracketedArgs :: Binding
angleBracketedArgs = define "AngleBracketedArgs" $
  doc "Angle-bracketed generic arguments" $
  T.record [
    "args">:
      doc "The generic arguments" $
      T.list $ rust "GenericArg"]

genericArg :: Binding
genericArg = define "GenericArg" $
  doc "A single generic argument" $
  T.union [
    "type">:
      doc "A type argument" $
      rust "Type",
    "lifetime">:
      doc "A lifetime argument" $
      rust "Lifetime",
    "const">:
      doc "A const expression argument" $
      rust "Expression",
    "binding">:
      doc "An associated type binding" $
      rust "TypeBinding"]

typeBinding :: Binding
typeBinding = define "TypeBinding" $
  doc "An associated type binding within generic arguments" $
  T.record [
    "name">:
      doc "The associated type name" $
      T.string,
    "type">:
      doc "The bound type" $
      rust "Type"]

parenthesizedArgs :: Binding
parenthesizedArgs = define "ParenthesizedArgs" $
  doc "Parenthesized generic arguments for Fn traits" $
  T.record [
    "inputs">:
      doc "The input types" $
      T.list $ rust "Type",
    "output">:
      doc "The output type" $
      T.maybe $ rust "Type"]

referenceType :: Binding
referenceType = define "ReferenceType" $
  doc "A reference type (e.g., &T, &mut T, &'a T)" $
  T.record [
    "lifetime">:
      doc "Optional lifetime annotation" $
      T.maybe $ rust "Lifetime",
    "mutable">:
      doc "Whether the reference is mutable" $
      T.boolean,
    "type">:
      doc "The referenced type" $
      rust "Type"]

arrayType :: Binding
arrayType = define "ArrayType" $
  doc "An array type with a fixed size (e.g., [T; 3])" $
  T.record [
    "element">:
      doc "The element type" $
      rust "Type",
    "length">:
      doc "The array length (as a constant expression)" $
      rust "Expression"]

fnPointerType :: Binding
fnPointerType = define "FnPointerType" $
  doc "A function pointer type (e.g., fn(i32, i32) -> i32)" $
  T.record [
    "params">:
      doc "The parameter types" $
      T.list $ rust "Type",
    "returnType">:
      doc "The return type" $
      rust "Type"]

rawPointerType :: Binding
rawPointerType = define "RawPointerType" $
  doc "A raw pointer type (e.g., *const T, *mut T)" $
  T.record [
    "mutable">:
      doc "Whether the pointer is mutable (*mut vs *const)" $
      T.boolean,
    "type">:
      doc "The pointed-to type" $
      rust "Type"]


-- ================================================================================================
-- Expressions
-- ================================================================================================

expression :: Binding
expression = define "Expression" $
  doc "A Rust expression" $
  T.union [
    "literal">:
      doc "A literal value" $
      rust "Literal",
    "path">:
      doc "A path expression" $
      rust "ExprPath",
    "block">:
      doc "A block expression" $
      rust "Block",
    "call">:
      doc "A function call expression" $
      rust "CallExpr",
    "methodCall">:
      doc "A method call expression" $
      rust "MethodCallExpr",
    "fieldAccess">:
      doc "A field access expression" $
      rust "FieldAccessExpr",
    "tupleIndex">:
      doc "A tuple index expression" $
      rust "TupleIndexExpr",
    "closure">:
      doc "A closure expression" $
      rust "ClosureExpr",
    "if">:
      doc "An if expression, including if let" $
      rust "IfExpr",
    "match">:
      doc "A match expression" $
      rust "MatchExpr",
    "loop">:
      doc "A loop expression" $
      rust "LoopExpr",
    "while">:
      doc "A while expression, including while let" $
      rust "WhileExpr",
    "for">:
      doc "A for expression" $
      rust "ForExpr",
    "binary">:
      doc "A binary operation" $
      rust "BinaryExpr",
    "unary">:
      doc "A unary operation" $
      rust "UnaryExpr",
    "reference">:
      doc "A reference expression" $
      rust "RefExpr",
    "dereference">:
      doc "A dereference expression" $
      rust "Expression",
    "struct">:
      doc "A struct literal expression" $
      rust "StructExpr",
    "tuple">:
      doc "A tuple expression" $
      T.list $ rust "Expression",
    "array">:
      doc "An array expression" $
      rust "ArrayExpr",
    "index">:
      doc "An index expression" $
      rust "IndexExpr",
    "range">:
      doc "A range expression" $
      rust "RangeExpr",
    "return">:
      doc "A return expression" $
      T.maybe $ rust "Expression",
    "break">:
      doc "A break expression" $
      T.maybe $ rust "Expression",
    "continue">:
      doc "A continue expression" $
      T.unit,
    "try">:
      doc "A try expression (expr?)" $
      rust "Expression",
    "cast">:
      doc "A type cast expression" $
      rust "CastExpr",
    "typeAscription">:
      doc "A type ascription expression" $
      rust "TypeAscriptionExpr",
    "await">:
      doc "An await expression" $
      rust "Expression",
    "assign">:
      doc "An assignment expression" $
      rust "AssignExpr",
    "compoundAssign">:
      doc "A compound assignment expression" $
      rust "CompoundAssignExpr",
    "macro">:
      doc "A macro invocation expression" $
      rust "MacroInvocation",
    "paren">:
      doc "A parenthesized expression" $
      rust "Expression"]

exprPath :: Binding
exprPath = define "ExprPath" $
  doc "A path used as an expression" $
  T.record [
    "global">:
      doc "Whether the path is global" $
      T.boolean,
    "segments">:
      doc "The path segments" $
      T.list $ rust "PathSegment"]

callExpr :: Binding
callExpr = define "CallExpr" $
  doc "A function call expression" $
  T.record [
    "function">:
      doc "The function being called" $
      rust "Expression",
    "args">:
      doc "The arguments" $
      T.list $ rust "Expression"]

methodCallExpr :: Binding
methodCallExpr = define "MethodCallExpr" $
  doc "A method call expression" $
  T.record [
    "receiver">:
      doc "The receiver expression" $
      rust "Expression",
    "method">:
      doc "The method name" $
      T.string,
    "turbofish">:
      doc "Optional turbofish generic arguments" $
      T.list $ rust "Type",
    "args">:
      doc "The arguments (excluding the receiver)" $
      T.list $ rust "Expression"]

fieldAccessExpr :: Binding
fieldAccessExpr = define "FieldAccessExpr" $
  doc "A field access expression" $
  T.record [
    "object">:
      doc "The expression being accessed" $
      rust "Expression",
    "field">:
      doc "The field name" $
      T.string]

tupleIndexExpr :: Binding
tupleIndexExpr = define "TupleIndexExpr" $
  doc "A tuple index expression" $
  T.record [
    "tuple">:
      doc "The tuple expression" $
      rust "Expression",
    "index">:
      doc "The index (0-based)" $
      T.int32]

closureExpr :: Binding
closureExpr = define "ClosureExpr" $
  doc "A closure expression" $
  T.record [
    "move">:
      doc "Whether the closure captures by move" $
      T.boolean,
    "params">:
      doc "The closure parameters" $
      T.list $ rust "ClosureParam",
    "returnType">:
      doc "Optional return type annotation" $
      T.maybe $ rust "Type",
    "body">:
      doc "The closure body" $
      rust "Expression"]

closureParam :: Binding
closureParam = define "ClosureParam" $
  doc "A closure parameter" $
  T.record [
    "pattern">:
      doc "The parameter pattern" $
      rust "Pattern",
    "type">:
      doc "Optional type annotation" $
      T.maybe $ rust "Type"]

ifExpr :: Binding
ifExpr = define "IfExpr" $
  doc "An if expression, optionally with if let" $
  T.record [
    "condition">:
      doc "The condition" $
      rust "IfCondition",
    "thenBlock">:
      doc "The then block" $
      rust "Block",
    "elseBranch">:
      doc "An optional else branch" $
      T.maybe $ rust "Expression"]

ifCondition :: Binding
ifCondition = define "IfCondition" $
  doc "The condition of an if expression" $
  T.union [
    "bool">:
      doc "A boolean condition" $
      rust "Expression",
    "let">:
      doc "A let condition" $
      rust "LetCondition"]

matchExpr :: Binding
matchExpr = define "MatchExpr" $
  doc "A match expression" $
  T.record [
    "scrutinee">:
      doc "The expression being matched" $
      rust "Expression",
    "arms">:
      doc "The match arms" $
      T.list $ rust "MatchArm"]

matchArm :: Binding
matchArm = define "MatchArm" $
  doc "A single arm in a match expression" $
  T.record [
    "pattern">:
      doc "The pattern to match" $
      rust "Pattern",
    "guard">:
      doc "Optional guard expression" $
      T.maybe $ rust "Expression",
    "body">:
      doc "The body expression" $
      rust "Expression"]

loopExpr :: Binding
loopExpr = define "LoopExpr" $
  doc "A loop expression" $
  T.record [
    "label">:
      doc "Optional loop label" $
      T.maybe T.string,
    "body">:
      doc "The loop body" $
      rust "Block"]

whileExpr :: Binding
whileExpr = define "WhileExpr" $
  doc "A while expression, optionally with while let" $
  T.record [
    "label">:
      doc "Optional loop label" $
      T.maybe T.string,
    "condition">:
      doc "The condition" $
      rust "IfCondition",
    "body">:
      doc "The loop body" $
      rust "Block"]

forExpr :: Binding
forExpr = define "ForExpr" $
  doc "A for expression" $
  T.record [
    "label">:
      doc "Optional loop label" $
      T.maybe T.string,
    "pattern">:
      doc "The loop variable pattern" $
      rust "Pattern",
    "iter">:
      doc "The iterator expression" $
      rust "Expression",
    "body">:
      doc "The loop body" $
      rust "Block"]

binaryExpr :: Binding
binaryExpr = define "BinaryExpr" $
  doc "A binary operation" $
  T.record [
    "left">:
      doc "The left-hand operand" $
      rust "Expression",
    "op">:
      doc "The binary operator" $
      rust "BinaryOp",
    "right">:
      doc "The right-hand operand" $
      rust "Expression"]

binaryOp :: Binding
binaryOp = define "BinaryOp" $
  doc "A binary operator" $
  T.enum [
    "add", "sub", "mul", "div", "rem",
    "and", "or",
    "bitAnd", "bitOr", "bitXor", "shl", "shr",
    "eq", "ne", "lt", "le", "gt", "ge"]

unaryExpr :: Binding
unaryExpr = define "UnaryExpr" $
  doc "A unary operation" $
  T.record [
    "op">:
      doc "The unary operator" $
      rust "UnaryOp",
    "operand">:
      doc "The operand" $
      rust "Expression"]

unaryOp :: Binding
unaryOp = define "UnaryOp" $
  doc "A unary operator" $
  T.enum ["neg", "not"]

refExpr :: Binding
refExpr = define "RefExpr" $
  doc "A reference expression" $
  T.record [
    "mutable">:
      doc "Whether the reference is mutable" $
      T.boolean,
    "expr">:
      doc "The expression being referenced" $
      rust "Expression"]

structExpr :: Binding
structExpr = define "StructExpr" $
  doc "A struct literal expression" $
  T.record [
    "path">:
      doc "The struct path" $
      rust "ExprPath",
    "fields">:
      doc "The field assignments" $
      T.list $ rust "FieldValue",
    "rest">:
      doc "Optional base expression for struct update syntax" $
      T.maybe $ rust "Expression"]

fieldValue :: Binding
fieldValue = define "FieldValue" $
  doc "A field-value pair in a struct literal" $
  T.record [
    "name">:
      doc "The field name" $
      T.string,
    "value">:
      doc "The field value (None for shorthand syntax)" $
      T.maybe $ rust "Expression"]

arrayExpr :: Binding
arrayExpr = define "ArrayExpr" $
  doc "An array expression" $
  T.union [
    "elements">:
      doc "An array literal" $
      T.list $ rust "Expression",
    "repeat">:
      doc "An array repeat expression" $
      rust "ArrayRepeat"]

indexExpr :: Binding
indexExpr = define "IndexExpr" $
  doc "An index expression" $
  T.record [
    "object">:
      doc "The expression being indexed" $
      rust "Expression",
    "index">:
      doc "The index expression" $
      rust "Expression"]

rangeExpr :: Binding
rangeExpr = define "RangeExpr" $
  doc "A range expression" $
  T.record [
    "from">:
      doc "The lower bound (optional)" $
      T.maybe $ rust "Expression",
    "to">:
      doc "The upper bound (optional)" $
      T.maybe $ rust "Expression",
    "inclusive">:
      doc "Whether the range is inclusive" $
      T.boolean]

castExpr :: Binding
castExpr = define "CastExpr" $
  doc "A type cast expression" $
  T.record [
    "expr">:
      doc "The expression being cast" $
      rust "Expression",
    "type">:
      doc "The target type" $
      rust "Type"]

typeAscriptionExpr :: Binding
typeAscriptionExpr = define "TypeAscriptionExpr" $
  doc "A type ascription expression" $
  T.record [
    "expr">:
      doc "The expression" $
      rust "Expression",
    "type">:
      doc "The ascribed type" $
      rust "Type"]

assignExpr :: Binding
assignExpr = define "AssignExpr" $
  doc "An assignment expression" $
  T.record [
    "target">:
      doc "The left-hand side (target)" $
      rust "Expression",
    "value">:
      doc "The right-hand side (value)" $
      rust "Expression"]

compoundAssignExpr :: Binding
compoundAssignExpr = define "CompoundAssignExpr" $
  doc "A compound assignment expression" $
  T.record [
    "target">:
      doc "The left-hand side (target)" $
      rust "Expression",
    "op">:
      doc "The compound assignment operator" $
      rust "CompoundAssignOp",
    "value">:
      doc "The right-hand side (value)" $
      rust "Expression"]

compoundAssignOp :: Binding
compoundAssignOp = define "CompoundAssignOp" $
  doc "A compound assignment operator" $
  T.enum [
    "addAssign", "subAssign", "mulAssign", "divAssign", "remAssign",
    "bitAndAssign", "bitOrAssign", "bitXorAssign", "shlAssign", "shrAssign"]

macroInvocation :: Binding
macroInvocation = define "MacroInvocation" $
  doc "A macro invocation" $
  T.record [
    "path">:
      doc "The macro path" $
      T.list T.string,
    "delimiter">:
      doc "The delimiter style used" $
      rust "MacroDelimiter",
    "tokens">:
      doc "The token stream as a raw string" $
      T.string]

macroDelimiter :: Binding
macroDelimiter = define "MacroDelimiter" $
  doc "The delimiter style for a macro invocation" $
  T.enum ["paren", "bracket", "brace"]


-- ================================================================================================
-- Statements
-- ================================================================================================

statement :: Binding
statement = define "Statement" $
  doc "A statement within a block" $
  T.union [
    "let">:
      doc "A let binding" $
      rust "LetStatement",
    "expression">:
      doc "An expression statement" $
      rust "Expression",
    "item">:
      doc "An item declaration within a block" $
      rust "Item",
    "empty">:
      doc "An empty statement" $
      T.unit]

letStatement :: Binding
letStatement = define "LetStatement" $
  doc "A let statement" $
  T.record [
    "pattern">:
      doc "The binding pattern" $
      rust "Pattern",
    "mutable">:
      doc "Whether the binding is mutable" $
      T.boolean,
    "type">:
      doc "Optional type annotation" $
      T.maybe $ rust "Type",
    "init">:
      doc "Optional initializer expression" $
      T.maybe $ rust "Expression"]

block :: Binding
block = define "Block" $
  doc "A block expression" $
  T.record [
    "statements">:
      doc "The statements in the block" $
      T.list $ rust "Statement",
    "expression">:
      doc "An optional trailing expression" $
      T.maybe $ rust "Expression"]


-- ================================================================================================
-- Patterns
-- ================================================================================================

pattern :: Binding
pattern = define "Pattern" $
  doc "A Rust pattern (used in let, match, function parameters, etc.)" $
  T.union [
    "wildcard">:
      doc "A wildcard pattern (_)" $
      T.unit,
    "identifier">:
      doc "An identifier pattern" $
      rust "IdentifierPattern",
    "literal">:
      doc "A literal pattern" $
      rust "Literal",
    "reference">:
      doc "A reference pattern" $
      rust "RefPattern",
    "struct">:
      doc "A struct pattern" $
      rust "StructPattern",
    "tupleStruct">:
      doc "A tuple struct pattern" $
      rust "TupleStructPattern",
    "tuple">:
      doc "A tuple pattern" $
      T.list $ rust "Pattern",
    "slice">:
      doc "A slice pattern" $
      T.list $ rust "Pattern",
    "or">:
      doc "An or-pattern" $
      T.list $ rust "Pattern",
    "path">:
      doc "A path pattern" $
      rust "ExprPath",
    "range">:
      doc "A range pattern" $
      rust "RangePattern",
    "rest">:
      doc "A rest pattern (..)" $
      T.unit,
    "paren">:
      doc "A parenthesized pattern" $
      rust "Pattern"]

identifierPattern :: Binding
identifierPattern = define "IdentifierPattern" $
  doc "An identifier pattern" $
  T.record [
    "name">:
      doc "The identifier name" $
      T.string,
    "mutable">:
      doc "Whether the binding is mutable" $
      T.boolean,
    "atPattern">:
      doc "Optional sub-pattern (e.g., x @ Some(_))" $
      T.maybe $ rust "Pattern"]

refPattern :: Binding
refPattern = define "RefPattern" $
  doc "A reference pattern" $
  T.record [
    "mutable">:
      doc "Whether the reference is mutable" $
      T.boolean,
    "pattern">:
      doc "The inner pattern" $
      rust "Pattern"]

structPattern :: Binding
structPattern = define "StructPattern" $
  doc "A struct pattern" $
  T.record [
    "path">:
      doc "The struct path" $
      rust "ExprPath",
    "fields">:
      doc "The field patterns" $
      T.list $ rust "FieldPattern",
    "rest">:
      doc "Whether the pattern has a rest (..) at the end" $
      T.boolean]

fieldPattern :: Binding
fieldPattern = define "FieldPattern" $
  doc "A field pattern within a struct pattern" $
  T.record [
    "name">:
      doc "The field name" $
      T.string,
    "pattern">:
      doc "The field pattern (None for shorthand)" $
      T.maybe $ rust "Pattern"]

tupleStructPattern :: Binding
tupleStructPattern = define "TupleStructPattern" $
  doc "A tuple struct pattern" $
  T.record [
    "path">:
      doc "The path to the tuple struct or variant" $
      rust "ExprPath",
    "elements">:
      doc "The element patterns" $
      T.list $ rust "Pattern"]

rangePattern :: Binding
rangePattern = define "RangePattern" $
  doc "A range pattern" $
  T.record [
    "from">:
      doc "The lower bound" $
      T.maybe $ rust "Pattern",
    "to">:
      doc "The upper bound" $
      T.maybe $ rust "Pattern",
    "inclusive">:
      doc "Whether the range is inclusive" $
      T.boolean]


-- ================================================================================================
-- Literals
-- ================================================================================================

literal :: Binding
literal = define "Literal" $
  doc "A literal value" $
  T.union [
    "integer">:
      doc "An integer literal" $
      rust "IntegerLiteral",
    "float">:
      doc "A floating-point literal" $
      rust "FloatLiteral",
    "string">:
      doc "A string literal" $
      T.string,
    "rawString">:
      doc "A raw string literal" $
      T.string,
    "byteString">:
      doc "A byte string literal" $
      T.binary,
    "char">:
      doc "A character literal" $
      T.uint32,
    "byte">:
      doc "A byte literal" $
      T.uint8,
    "bool">:
      doc "A boolean literal" $
      T.boolean]

integerLiteral :: Binding
integerLiteral = define "IntegerLiteral" $
  doc "An integer literal with optional suffix" $
  T.record [
    "value">:
      doc "The integer value" $
      T.bigint,
    "suffix">:
      doc "Optional type suffix" $
      T.maybe T.string]

floatLiteral :: Binding
floatLiteral = define "FloatLiteral" $
  doc "A floating-point literal with optional suffix" $
  T.record [
    "value">:
      doc "The float value" $
      T.float64,
    "suffix">:
      doc "Optional type suffix" $
      T.maybe T.string]


-- ================================================================================================
-- Attributes and visibility
-- ================================================================================================

attribute :: Binding
attribute = define "Attribute" $
  doc "An attribute (e.g., #[derive(Clone)], #[cfg(test)])" $
  T.record [
    "inner">:
      doc "Whether the attribute is an inner attribute (#![...] vs #[...])" $
      T.boolean,
    "path">:
      doc "The attribute path" $
      T.list T.string,
    "tokens">:
      doc "The attribute arguments as a raw token string" $
      T.maybe T.string]

visibility :: Binding
visibility = define "Visibility" $
  doc "A visibility qualifier" $
  T.union [
    "public">:
      doc "Public (pub)" $
      T.unit,
    "crate">:
      doc "Crate-visible (pub(crate))" $
      T.unit,
    "restricted">:
      doc "Visible to a specific path (pub(in path))" $
      T.list T.string,
    "private">:
      doc "Private (default)" $
      T.unit]


-- ================================================================================================
-- Helper types (used in expressions)
-- ================================================================================================

-- | A let condition in an if-let or while-let
letCondition :: Binding
letCondition = define "LetCondition" $
  doc "A let condition (e.g., let Some(x) = opt)" $
  T.record [
    "pattern">:
      doc "The pattern" $
      rust "Pattern",
    "expr">:
      doc "The expression being matched" $
      rust "Expression"]

-- | An array repeat expression
arrayRepeat :: Binding
arrayRepeat = define "ArrayRepeat" $
  doc "An array repeat expression (e.g., [0; 10])" $
  T.record [
    "element">:
      doc "The element expression" $
      rust "Expression",
    "length">:
      doc "The length expression" $
      rust "Expression"]
