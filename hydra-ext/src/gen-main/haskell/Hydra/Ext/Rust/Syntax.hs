-- Note: this is an automatically generated file. Do not edit.

-- | A Rust syntax model, based on the Rust Reference grammar (https://doc.rust-lang.org/reference/), retrieved 2025-01-29

module Hydra.Ext.Rust.Syntax where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A Rust crate, represented as a collection of top-level items
data Crate = 
  Crate {
    -- | The top-level items in the crate
    crateItems :: [Item]}
  deriving (Eq, Ord, Read, Show)

_Crate = (Core.Name "hydra.ext.rust.syntax.Crate")

_Crate_items = (Core.Name "items")

-- | A top-level item in a Rust module or crate
data Item = 
  -- | A use declaration
  ItemUse UseDeclaration |
  -- | A struct definition
  ItemStruct StructDef |
  -- | An enum definition
  ItemEnum EnumDef |
  -- | A function definition
  ItemFn FnDef |
  -- | A type alias
  ItemTypeAlias TypeAlias |
  -- | An impl block
  ItemImpl ImplBlock |
  -- | A trait definition
  ItemTrait TraitDef |
  -- | A module definition
  ItemMod ModDef |
  -- | A constant item
  ItemConst ConstDef |
  -- | A static item
  ItemStatic StaticDef |
  -- | A macro invocation as an item
  ItemMacro MacroInvocation
  deriving (Eq, Ord, Read, Show)

_Item = (Core.Name "hydra.ext.rust.syntax.Item")

_Item_use = (Core.Name "use")

_Item_struct = (Core.Name "struct")

_Item_enum = (Core.Name "enum")

_Item_fn = (Core.Name "fn")

_Item_typeAlias = (Core.Name "typeAlias")

_Item_impl = (Core.Name "impl")

_Item_trait = (Core.Name "trait")

_Item_mod = (Core.Name "mod")

_Item_const = (Core.Name "const")

_Item_static = (Core.Name "static")

_Item_macro = (Core.Name "macro")

-- | An item together with optional doc comments and visibility
data ItemWithComments = 
  ItemWithComments {
    -- | Optional documentation comment
    itemWithCommentsDoc :: (Maybe String),
    -- | The item's visibility
    itemWithCommentsVisibility :: Visibility,
    -- | The item itself
    itemWithCommentsItem :: Item}
  deriving (Eq, Ord, Read, Show)

_ItemWithComments = (Core.Name "hydra.ext.rust.syntax.ItemWithComments")

_ItemWithComments_doc = (Core.Name "doc")

_ItemWithComments_visibility = (Core.Name "visibility")

_ItemWithComments_item = (Core.Name "item")

-- | A use declaration (e.g., use std::collections::BTreeMap;)
data UseDeclaration = 
  UseDeclaration {
    -- | Whether the use is public (pub use)
    useDeclarationPublic :: Bool,
    -- | The use tree describing what is imported
    useDeclarationTree :: UseTree}
  deriving (Eq, Ord, Read, Show)

_UseDeclaration = (Core.Name "hydra.ext.rust.syntax.UseDeclaration")

_UseDeclaration_public = (Core.Name "public")

_UseDeclaration_tree = (Core.Name "tree")

-- | A use tree, representing the structure of a use path
data UseTree = 
  -- | A simple path import
  UseTreePath UsePath |
  -- | A renamed import (e.g., BTreeMap as Map)
  UseTreeRename UseRename |
  -- | A glob import (e.g., std::collections::*)
  UseTreeGlob [String] |
  -- | A grouped import (e.g., {BTreeMap, BTreeSet})
  UseTreeGroup UseGroup
  deriving (Eq, Ord, Read, Show)

_UseTree = (Core.Name "hydra.ext.rust.syntax.UseTree")

_UseTree_path = (Core.Name "path")

_UseTree_rename = (Core.Name "rename")

_UseTree_glob = (Core.Name "glob")

_UseTree_group = (Core.Name "group")

-- | A simple path import within a use tree
data UsePath = 
  UsePath {
    -- | The path segments
    usePathSegments :: [String]}
  deriving (Eq, Ord, Read, Show)

_UsePath = (Core.Name "hydra.ext.rust.syntax.UsePath")

_UsePath_segments = (Core.Name "segments")

-- | A renamed import (e.g., BTreeMap as Map)
data UseRename = 
  UseRename {
    -- | The original path segments
    useRenamePath :: [String],
    -- | The alias name
    useRenameAlias :: String}
  deriving (Eq, Ord, Read, Show)

_UseRename = (Core.Name "hydra.ext.rust.syntax.UseRename")

_UseRename_path = (Core.Name "path")

_UseRename_alias = (Core.Name "alias")

-- | A grouped import (e.g., std::collections::{BTreeMap, BTreeSet})
data UseGroup = 
  UseGroup {
    -- | The common prefix path segments
    useGroupPrefix :: [String],
    -- | The individual subtrees within the group
    useGroupTrees :: [UseTree]}
  deriving (Eq, Ord, Read, Show)

_UseGroup = (Core.Name "hydra.ext.rust.syntax.UseGroup")

_UseGroup_prefix = (Core.Name "prefix")

_UseGroup_trees = (Core.Name "trees")

-- | A struct definition (e.g., struct Foo<T> { bar: T })
data StructDef = 
  StructDef {
    -- | The struct name
    structDefName :: String,
    -- | Generic type parameters
    structDefGenerics :: [GenericParam],
    -- | Optional where clause
    structDefWhereClause :: (Maybe WhereClause),
    -- | The struct body (named fields, tuple fields, or unit)
    structDefBody :: StructBody,
    -- | Derive macros to apply
    structDefDerives :: [String],
    -- | Whether the struct is public
    structDefPublic :: Bool,
    -- | Optional doc comment
    structDefDoc :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_StructDef = (Core.Name "hydra.ext.rust.syntax.StructDef")

_StructDef_name = (Core.Name "name")

_StructDef_generics = (Core.Name "generics")

_StructDef_whereClause = (Core.Name "whereClause")

_StructDef_body = (Core.Name "body")

_StructDef_derives = (Core.Name "derives")

_StructDef_public = (Core.Name "public")

_StructDef_doc = (Core.Name "doc")

-- | The body of a struct definition
data StructBody = 
  -- | A struct with named fields
  StructBodyNamed [StructField] |
  -- | A tuple struct
  StructBodyTuple [TupleField] |
  -- | A unit struct
  StructBodyUnit 
  deriving (Eq, Ord, Read, Show)

_StructBody = (Core.Name "hydra.ext.rust.syntax.StructBody")

_StructBody_named = (Core.Name "named")

_StructBody_tuple = (Core.Name "tuple")

_StructBody_unit = (Core.Name "unit")

-- | A named field within a struct definition
data StructField = 
  StructField {
    -- | The field name
    structFieldName :: String,
    -- | The field type
    structFieldType :: Type,
    -- | Whether the field is public
    structFieldPublic :: Bool,
    -- | Optional doc comment for the field
    structFieldDoc :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_StructField = (Core.Name "hydra.ext.rust.syntax.StructField")

_StructField_name = (Core.Name "name")

_StructField_type = (Core.Name "type")

_StructField_public = (Core.Name "public")

_StructField_doc = (Core.Name "doc")

-- | A positional field within a tuple struct
data TupleField = 
  TupleField {
    -- | The field type
    tupleFieldType :: Type,
    -- | Whether the field is public
    tupleFieldPublic :: Bool}
  deriving (Eq, Ord, Read, Show)

_TupleField = (Core.Name "hydra.ext.rust.syntax.TupleField")

_TupleField_type = (Core.Name "type")

_TupleField_public = (Core.Name "public")

-- | An enum definition (e.g., enum Foo<T> { Bar(T), Baz { x: i32 } })
data EnumDef = 
  EnumDef {
    -- | The enum name
    enumDefName :: String,
    -- | Generic type parameters
    enumDefGenerics :: [GenericParam],
    -- | Optional where clause
    enumDefWhereClause :: (Maybe WhereClause),
    -- | The enum variants
    enumDefVariants :: [EnumVariant],
    -- | Derive macros to apply
    enumDefDerives :: [String],
    -- | Whether the enum is public
    enumDefPublic :: Bool,
    -- | Optional doc comment
    enumDefDoc :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_EnumDef = (Core.Name "hydra.ext.rust.syntax.EnumDef")

_EnumDef_name = (Core.Name "name")

_EnumDef_generics = (Core.Name "generics")

_EnumDef_whereClause = (Core.Name "whereClause")

_EnumDef_variants = (Core.Name "variants")

_EnumDef_derives = (Core.Name "derives")

_EnumDef_public = (Core.Name "public")

_EnumDef_doc = (Core.Name "doc")

-- | A variant of an enum definition
data EnumVariant = 
  EnumVariant {
    -- | The variant name
    enumVariantName :: String,
    -- | The variant body
    enumVariantBody :: EnumVariantBody,
    -- | Optional doc comment for the variant
    enumVariantDoc :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_EnumVariant = (Core.Name "hydra.ext.rust.syntax.EnumVariant")

_EnumVariant_name = (Core.Name "name")

_EnumVariant_body = (Core.Name "body")

_EnumVariant_doc = (Core.Name "doc")

-- | The body of an enum variant
data EnumVariantBody = 
  -- | A unit variant (e.g., Foo)
  EnumVariantBodyUnit  |
  -- | A tuple variant (e.g., Foo(i32, String))
  EnumVariantBodyTuple [Type] |
  -- | A struct variant (e.g., Foo { x: i32 })
  EnumVariantBodyStruct [StructField]
  deriving (Eq, Ord, Read, Show)

_EnumVariantBody = (Core.Name "hydra.ext.rust.syntax.EnumVariantBody")

_EnumVariantBody_unit = (Core.Name "unit")

_EnumVariantBody_tuple = (Core.Name "tuple")

_EnumVariantBody_struct = (Core.Name "struct")

-- | A function definition (e.g., fn foo<T>(x: T) -> String { ... })
data FnDef = 
  FnDef {
    -- | The function name
    fnDefName :: String,
    -- | Generic type parameters
    fnDefGenerics :: [GenericParam],
    -- | Optional where clause
    fnDefWhereClause :: (Maybe WhereClause),
    -- | The function parameters
    fnDefParams :: [FnParam],
    -- | The return type (None means ())
    fnDefReturnType :: (Maybe Type),
    -- | The function body
    fnDefBody :: Block,
    -- | Whether the function is public
    fnDefPublic :: Bool,
    -- | Whether the function is async
    fnDefAsync :: Bool,
    -- | Whether the function is const
    fnDefConst :: Bool,
    -- | Whether the function is unsafe
    fnDefUnsafe :: Bool,
    -- | Optional doc comment
    fnDefDoc :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_FnDef = (Core.Name "hydra.ext.rust.syntax.FnDef")

_FnDef_name = (Core.Name "name")

_FnDef_generics = (Core.Name "generics")

_FnDef_whereClause = (Core.Name "whereClause")

_FnDef_params = (Core.Name "params")

_FnDef_returnType = (Core.Name "returnType")

_FnDef_body = (Core.Name "body")

_FnDef_public = (Core.Name "public")

_FnDef_async = (Core.Name "async")

_FnDef_const = (Core.Name "const")

_FnDef_unsafe = (Core.Name "unsafe")

_FnDef_doc = (Core.Name "doc")

-- | A function parameter
data FnParam = 
  FnParam {
    -- | The parameter pattern
    fnParamPattern :: Pattern,
    -- | The parameter type
    fnParamType :: Type}
  deriving (Eq, Ord, Read, Show)

_FnParam = (Core.Name "hydra.ext.rust.syntax.FnParam")

_FnParam_pattern = (Core.Name "pattern")

_FnParam_type = (Core.Name "type")

-- | A self parameter in a method
data SelfParam = 
  SelfParamOwned  |
  SelfParamRef  |
  SelfParamRefMut 
  deriving (Eq, Ord, Read, Show)

_SelfParam = (Core.Name "hydra.ext.rust.syntax.SelfParam")

_SelfParam_owned = (Core.Name "owned")

_SelfParam_ref = (Core.Name "ref")

_SelfParam_refMut = (Core.Name "refMut")

-- | A method parameter, which may be self or a regular parameter
data MethodParam = 
  -- | A self parameter
  MethodParamSelf SelfParam |
  -- | A regular function parameter
  MethodParamRegular FnParam
  deriving (Eq, Ord, Read, Show)

_MethodParam = (Core.Name "hydra.ext.rust.syntax.MethodParam")

_MethodParam_self = (Core.Name "self")

_MethodParam_regular = (Core.Name "regular")

-- | A type alias definition (e.g., type Foo<T> = Bar<T>;)
data TypeAlias = 
  TypeAlias {
    -- | The alias name
    typeAliasName :: String,
    -- | Generic type parameters
    typeAliasGenerics :: [GenericParam],
    -- | The aliased type
    typeAliasType :: Type,
    -- | Whether the alias is public
    typeAliasPublic :: Bool,
    -- | Optional doc comment
    typeAliasDoc :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_TypeAlias = (Core.Name "hydra.ext.rust.syntax.TypeAlias")

_TypeAlias_name = (Core.Name "name")

_TypeAlias_generics = (Core.Name "generics")

_TypeAlias_type = (Core.Name "type")

_TypeAlias_public = (Core.Name "public")

_TypeAlias_doc = (Core.Name "doc")

-- | A constant item (e.g., const FOO: u32 = 42;)
data ConstDef = 
  ConstDef {
    -- | The constant name
    constDefName :: String,
    -- | The constant type
    constDefType :: Type,
    -- | The constant value expression
    constDefValue :: Expression,
    -- | Whether the constant is public
    constDefPublic :: Bool,
    -- | Optional doc comment
    constDefDoc :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_ConstDef = (Core.Name "hydra.ext.rust.syntax.ConstDef")

_ConstDef_name = (Core.Name "name")

_ConstDef_type = (Core.Name "type")

_ConstDef_value = (Core.Name "value")

_ConstDef_public = (Core.Name "public")

_ConstDef_doc = (Core.Name "doc")

-- | A static item (e.g., static FOO: u32 = 42;)
data StaticDef = 
  StaticDef {
    -- | The static name
    staticDefName :: String,
    -- | The static type
    staticDefType :: Type,
    -- | The static value expression
    staticDefValue :: Expression,
    -- | Whether the static is mutable
    staticDefMutable :: Bool,
    -- | Whether the static is public
    staticDefPublic :: Bool,
    -- | Optional doc comment
    staticDefDoc :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_StaticDef = (Core.Name "hydra.ext.rust.syntax.StaticDef")

_StaticDef_name = (Core.Name "name")

_StaticDef_type = (Core.Name "type")

_StaticDef_value = (Core.Name "value")

_StaticDef_mutable = (Core.Name "mutable")

_StaticDef_public = (Core.Name "public")

_StaticDef_doc = (Core.Name "doc")

-- | A module definition (either inline or external)
data ModDef = 
  ModDef {
    -- | The module name
    modDefName :: String,
    -- | The module body (None for external file)
    modDefBody :: (Maybe [Item]),
    -- | Whether the module is public
    modDefPublic :: Bool,
    -- | Optional doc comment
    modDefDoc :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_ModDef = (Core.Name "hydra.ext.rust.syntax.ModDef")

_ModDef_name = (Core.Name "name")

_ModDef_body = (Core.Name "body")

_ModDef_public = (Core.Name "public")

_ModDef_doc = (Core.Name "doc")

-- | An impl block (e.g., impl<T> Trait for Foo<T> { ... })
data ImplBlock = 
  ImplBlock {
    -- | Generic type parameters
    implBlockGenerics :: [GenericParam],
    -- | Optional where clause
    implBlockWhereClause :: (Maybe WhereClause),
    -- | The trait being implemented, if any
    implBlockTrait :: (Maybe TypePath),
    -- | Whether this is a negative impl
    implBlockNegative :: Bool,
    -- | The type being implemented for
    implBlockSelfType :: Type,
    -- | The items within the impl block
    implBlockItems :: [ImplItem]}
  deriving (Eq, Ord, Read, Show)

_ImplBlock = (Core.Name "hydra.ext.rust.syntax.ImplBlock")

_ImplBlock_generics = (Core.Name "generics")

_ImplBlock_whereClause = (Core.Name "whereClause")

_ImplBlock_trait = (Core.Name "trait")

_ImplBlock_negative = (Core.Name "negative")

_ImplBlock_selfType = (Core.Name "selfType")

_ImplBlock_items = (Core.Name "items")

-- | An item within an impl block
data ImplItem = 
  -- | A method definition
  ImplItemMethod ImplMethod |
  -- | An associated type definition
  ImplItemType TypeAlias |
  -- | An associated constant
  ImplItemConst ConstDef
  deriving (Eq, Ord, Read, Show)

_ImplItem = (Core.Name "hydra.ext.rust.syntax.ImplItem")

_ImplItem_method = (Core.Name "method")

_ImplItem_type = (Core.Name "type")

_ImplItem_const = (Core.Name "const")

-- | A method within an impl block
data ImplMethod = 
  ImplMethod {
    -- | The method name
    implMethodName :: String,
    -- | Generic type parameters
    implMethodGenerics :: [GenericParam],
    -- | Optional where clause
    implMethodWhereClause :: (Maybe WhereClause),
    -- | The method parameters (including self)
    implMethodParams :: [MethodParam],
    -- | The return type (None means ())
    implMethodReturnType :: (Maybe Type),
    -- | The method body
    implMethodBody :: Block,
    -- | Whether the method is public
    implMethodPublic :: Bool,
    -- | Whether the method has a default implementation
    implMethodDefault :: Bool,
    -- | Optional doc comment
    implMethodDoc :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_ImplMethod = (Core.Name "hydra.ext.rust.syntax.ImplMethod")

_ImplMethod_name = (Core.Name "name")

_ImplMethod_generics = (Core.Name "generics")

_ImplMethod_whereClause = (Core.Name "whereClause")

_ImplMethod_params = (Core.Name "params")

_ImplMethod_returnType = (Core.Name "returnType")

_ImplMethod_body = (Core.Name "body")

_ImplMethod_public = (Core.Name "public")

_ImplMethod_default = (Core.Name "default")

_ImplMethod_doc = (Core.Name "doc")

-- | A trait definition (e.g., trait Foo<T>: Bar + Baz { ... })
data TraitDef = 
  TraitDef {
    -- | The trait name
    traitDefName :: String,
    -- | Generic type parameters
    traitDefGenerics :: [GenericParam],
    -- | Optional where clause
    traitDefWhereClause :: (Maybe WhereClause),
    -- | Supertraits
    traitDefSuperTraits :: [TypeParamBound],
    -- | The items within the trait
    traitDefItems :: [TraitItem],
    -- | Whether the trait is public
    traitDefPublic :: Bool,
    -- | Whether the trait is unsafe
    traitDefUnsafe :: Bool,
    -- | Optional doc comment
    traitDefDoc :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_TraitDef = (Core.Name "hydra.ext.rust.syntax.TraitDef")

_TraitDef_name = (Core.Name "name")

_TraitDef_generics = (Core.Name "generics")

_TraitDef_whereClause = (Core.Name "whereClause")

_TraitDef_superTraits = (Core.Name "superTraits")

_TraitDef_items = (Core.Name "items")

_TraitDef_public = (Core.Name "public")

_TraitDef_unsafe = (Core.Name "unsafe")

_TraitDef_doc = (Core.Name "doc")

-- | An item within a trait definition
data TraitItem = 
  -- | A method signature or default method
  TraitItemMethod TraitMethod |
  -- | An associated type
  TraitItemType TraitType |
  -- | An associated constant
  TraitItemConst TraitConst
  deriving (Eq, Ord, Read, Show)

_TraitItem = (Core.Name "hydra.ext.rust.syntax.TraitItem")

_TraitItem_method = (Core.Name "method")

_TraitItem_type = (Core.Name "type")

_TraitItem_const = (Core.Name "const")

-- | A method signature or default method within a trait
data TraitMethod = 
  TraitMethod {
    -- | The method name
    traitMethodName :: String,
    -- | Generic type parameters
    traitMethodGenerics :: [GenericParam],
    -- | Optional where clause
    traitMethodWhereClause :: (Maybe WhereClause),
    -- | The method parameters (including self)
    traitMethodParams :: [MethodParam],
    -- | The return type
    traitMethodReturnType :: (Maybe Type),
    -- | Optional default body
    traitMethodDefaultBody :: (Maybe Block),
    -- | Optional doc comment
    traitMethodDoc :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_TraitMethod = (Core.Name "hydra.ext.rust.syntax.TraitMethod")

_TraitMethod_name = (Core.Name "name")

_TraitMethod_generics = (Core.Name "generics")

_TraitMethod_whereClause = (Core.Name "whereClause")

_TraitMethod_params = (Core.Name "params")

_TraitMethod_returnType = (Core.Name "returnType")

_TraitMethod_defaultBody = (Core.Name "defaultBody")

_TraitMethod_doc = (Core.Name "doc")

-- | An associated type within a trait
data TraitType = 
  TraitType {
    -- | The associated type name
    traitTypeName :: String,
    -- | Type parameter bounds
    traitTypeBounds :: [TypeParamBound],
    -- | Optional default type
    traitTypeDefault :: (Maybe Type),
    -- | Optional doc comment
    traitTypeDoc :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_TraitType = (Core.Name "hydra.ext.rust.syntax.TraitType")

_TraitType_name = (Core.Name "name")

_TraitType_bounds = (Core.Name "bounds")

_TraitType_default = (Core.Name "default")

_TraitType_doc = (Core.Name "doc")

-- | An associated constant within a trait
data TraitConst = 
  TraitConst {
    -- | The constant name
    traitConstName :: String,
    -- | The constant type
    traitConstType :: Type,
    -- | Optional default value
    traitConstDefault :: (Maybe Expression),
    -- | Optional doc comment
    traitConstDoc :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_TraitConst = (Core.Name "hydra.ext.rust.syntax.TraitConst")

_TraitConst_name = (Core.Name "name")

_TraitConst_type = (Core.Name "type")

_TraitConst_default = (Core.Name "default")

_TraitConst_doc = (Core.Name "doc")

-- | A generic type parameter (e.g., T: Clone + Debug)
data GenericParam = 
  GenericParam {
    -- | The parameter name
    genericParamName :: String,
    -- | Trait bounds on the parameter
    genericParamBounds :: [TypeParamBound]}
  deriving (Eq, Ord, Read, Show)

_GenericParam = (Core.Name "hydra.ext.rust.syntax.GenericParam")

_GenericParam_name = (Core.Name "name")

_GenericParam_bounds = (Core.Name "bounds")

-- | A bound on a type parameter
data TypeParamBound = 
  -- | A trait bound
  TypeParamBoundTrait TypePath |
  -- | A lifetime bound
  TypeParamBoundLifetime Lifetime
  deriving (Eq, Ord, Read, Show)

_TypeParamBound = (Core.Name "hydra.ext.rust.syntax.TypeParamBound")

_TypeParamBound_trait = (Core.Name "trait")

_TypeParamBound_lifetime = (Core.Name "lifetime")

-- | A lifetime (e.g., 'a, 'static)
data Lifetime = 
  Lifetime {
    -- | The lifetime name (without the leading quote)
    lifetimeName :: String}
  deriving (Eq, Ord, Read, Show)

_Lifetime = (Core.Name "hydra.ext.rust.syntax.Lifetime")

_Lifetime_name = (Core.Name "name")

-- | A where clause (e.g., where T: Clone, U: Debug)
data WhereClause = 
  WhereClause {
    -- | The predicates in the where clause
    whereClausePredicates :: [WherePredicate]}
  deriving (Eq, Ord, Read, Show)

_WhereClause = (Core.Name "hydra.ext.rust.syntax.WhereClause")

_WhereClause_predicates = (Core.Name "predicates")

-- | A single predicate in a where clause
data WherePredicate = 
  WherePredicate {
    -- | The type being constrained
    wherePredicateType :: Type,
    -- | The bounds on the type
    wherePredicateBounds :: [TypeParamBound]}
  deriving (Eq, Ord, Read, Show)

_WherePredicate = (Core.Name "hydra.ext.rust.syntax.WherePredicate")

_WherePredicate_type = (Core.Name "type")

_WherePredicate_bounds = (Core.Name "bounds")

-- | A Rust type expression
data Type = 
  -- | A path type, possibly with generic arguments
  TypePath_ TypePath |
  -- | A reference type
  TypeReference ReferenceType |
  -- | A slice type
  TypeSlice Type |
  -- | An array type with a fixed size
  TypeArray ArrayType |
  -- | A tuple type
  TypeTuple [Type] |
  -- | A function pointer type
  TypeFnPointer FnPointerType |
  -- | An impl Trait type
  TypeImplTrait [TypeParamBound] |
  -- | A dyn Trait type
  TypeDynTrait [TypeParamBound] |
  -- | The inferred type placeholder (_)
  TypeInferred  |
  -- | The unit type (())
  TypeUnit  |
  -- | The never type (!)
  TypeNever  |
  -- | A raw pointer type
  TypeRawPointer RawPointerType |
  -- | A macro invocation in type position
  TypeMacro MacroInvocation
  deriving (Eq, Ord, Read, Show)

_Type = (Core.Name "hydra.ext.rust.syntax.Type")

_Type_path = (Core.Name "path")

_Type_reference = (Core.Name "reference")

_Type_slice = (Core.Name "slice")

_Type_array = (Core.Name "array")

_Type_tuple = (Core.Name "tuple")

_Type_fnPointer = (Core.Name "fnPointer")

_Type_implTrait = (Core.Name "implTrait")

_Type_dynTrait = (Core.Name "dynTrait")

_Type_inferred = (Core.Name "inferred")

_Type_unit = (Core.Name "unit")

_Type_never = (Core.Name "never")

_Type_rawPointer = (Core.Name "rawPointer")

_Type_macro = (Core.Name "macro")

-- | A path-based type, possibly with generic arguments
data TypePath = 
  TypePath {
    -- | Whether the path is absolute (starts with ::)
    typePathGlobal :: Bool,
    -- | The segments of the path
    typePathSegments :: [PathSegment]}
  deriving (Eq, Ord, Read, Show)

_TypePath = (Core.Name "hydra.ext.rust.syntax.TypePath")

_TypePath_global = (Core.Name "global")

_TypePath_segments = (Core.Name "segments")

-- | A segment within a type path
data PathSegment = 
  PathSegment {
    -- | The segment name
    pathSegmentName :: String,
    -- | Generic arguments, if any
    pathSegmentArguments :: GenericArguments}
  deriving (Eq, Ord, Read, Show)

_PathSegment = (Core.Name "hydra.ext.rust.syntax.PathSegment")

_PathSegment_name = (Core.Name "name")

_PathSegment_arguments = (Core.Name "arguments")

-- | Generic arguments to a path segment
data GenericArguments = 
  -- | No generic arguments
  GenericArgumentsNone  |
  -- | Angle-bracketed arguments
  GenericArgumentsAngleBracketed AngleBracketedArgs |
  -- | Parenthesized arguments for Fn traits
  GenericArgumentsParenthesized ParenthesizedArgs
  deriving (Eq, Ord, Read, Show)

_GenericArguments = (Core.Name "hydra.ext.rust.syntax.GenericArguments")

_GenericArguments_none = (Core.Name "none")

_GenericArguments_angleBracketed = (Core.Name "angleBracketed")

_GenericArguments_parenthesized = (Core.Name "parenthesized")

-- | Angle-bracketed generic arguments
data AngleBracketedArgs = 
  AngleBracketedArgs {
    -- | The generic arguments
    angleBracketedArgsArgs :: [GenericArg]}
  deriving (Eq, Ord, Read, Show)

_AngleBracketedArgs = (Core.Name "hydra.ext.rust.syntax.AngleBracketedArgs")

_AngleBracketedArgs_args = (Core.Name "args")

-- | A single generic argument
data GenericArg = 
  -- | A type argument
  GenericArgType Type |
  -- | A lifetime argument
  GenericArgLifetime Lifetime |
  -- | A const expression argument
  GenericArgConst Expression |
  -- | An associated type binding
  GenericArgBinding TypeBinding
  deriving (Eq, Ord, Read, Show)

_GenericArg = (Core.Name "hydra.ext.rust.syntax.GenericArg")

_GenericArg_type = (Core.Name "type")

_GenericArg_lifetime = (Core.Name "lifetime")

_GenericArg_const = (Core.Name "const")

_GenericArg_binding = (Core.Name "binding")

-- | An associated type binding within generic arguments
data TypeBinding = 
  TypeBinding {
    -- | The associated type name
    typeBindingName :: String,
    -- | The bound type
    typeBindingType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeBinding = (Core.Name "hydra.ext.rust.syntax.TypeBinding")

_TypeBinding_name = (Core.Name "name")

_TypeBinding_type = (Core.Name "type")

-- | Parenthesized generic arguments for Fn traits
data ParenthesizedArgs = 
  ParenthesizedArgs {
    -- | The input types
    parenthesizedArgsInputs :: [Type],
    -- | The output type
    parenthesizedArgsOutput :: (Maybe Type)}
  deriving (Eq, Ord, Read, Show)

_ParenthesizedArgs = (Core.Name "hydra.ext.rust.syntax.ParenthesizedArgs")

_ParenthesizedArgs_inputs = (Core.Name "inputs")

_ParenthesizedArgs_output = (Core.Name "output")

-- | A reference type (e.g., &T, &mut T, &'a T)
data ReferenceType = 
  ReferenceType {
    -- | Optional lifetime annotation
    referenceTypeLifetime :: (Maybe Lifetime),
    -- | Whether the reference is mutable
    referenceTypeMutable :: Bool,
    -- | The referenced type
    referenceTypeType :: Type}
  deriving (Eq, Ord, Read, Show)

_ReferenceType = (Core.Name "hydra.ext.rust.syntax.ReferenceType")

_ReferenceType_lifetime = (Core.Name "lifetime")

_ReferenceType_mutable = (Core.Name "mutable")

_ReferenceType_type = (Core.Name "type")

-- | An array type with a fixed size (e.g., [T; 3])
data ArrayType = 
  ArrayType {
    -- | The element type
    arrayTypeElement :: Type,
    -- | The array length (as a constant expression)
    arrayTypeLength :: Expression}
  deriving (Eq, Ord, Read, Show)

_ArrayType = (Core.Name "hydra.ext.rust.syntax.ArrayType")

_ArrayType_element = (Core.Name "element")

_ArrayType_length = (Core.Name "length")

-- | A function pointer type (e.g., fn(i32, i32) -> i32)
data FnPointerType = 
  FnPointerType {
    -- | The parameter types
    fnPointerTypeParams :: [Type],
    -- | The return type
    fnPointerTypeReturnType :: Type}
  deriving (Eq, Ord, Read, Show)

_FnPointerType = (Core.Name "hydra.ext.rust.syntax.FnPointerType")

_FnPointerType_params = (Core.Name "params")

_FnPointerType_returnType = (Core.Name "returnType")

-- | A raw pointer type (e.g., *const T, *mut T)
data RawPointerType = 
  RawPointerType {
    -- | Whether the pointer is mutable (*mut vs *const)
    rawPointerTypeMutable :: Bool,
    -- | The pointed-to type
    rawPointerTypeType :: Type}
  deriving (Eq, Ord, Read, Show)

_RawPointerType = (Core.Name "hydra.ext.rust.syntax.RawPointerType")

_RawPointerType_mutable = (Core.Name "mutable")

_RawPointerType_type = (Core.Name "type")

-- | A Rust expression
data Expression = 
  -- | A literal value
  ExpressionLiteral Literal |
  -- | A path expression
  ExpressionPath ExprPath |
  -- | A block expression
  ExpressionBlock Block |
  -- | A function call expression
  ExpressionCall CallExpr |
  -- | A method call expression
  ExpressionMethodCall MethodCallExpr |
  -- | A field access expression
  ExpressionFieldAccess FieldAccessExpr |
  -- | A tuple index expression
  ExpressionTupleIndex TupleIndexExpr |
  -- | A closure expression
  ExpressionClosure ClosureExpr |
  -- | An if expression, including if let
  ExpressionIf IfExpr |
  -- | A match expression
  ExpressionMatch MatchExpr |
  -- | A loop expression
  ExpressionLoop LoopExpr |
  -- | A while expression, including while let
  ExpressionWhile WhileExpr |
  -- | A for expression
  ExpressionFor ForExpr |
  -- | A binary operation
  ExpressionBinary BinaryExpr |
  -- | A unary operation
  ExpressionUnary UnaryExpr |
  -- | A reference expression
  ExpressionReference RefExpr |
  -- | A dereference expression
  ExpressionDereference Expression |
  -- | A struct literal expression
  ExpressionStruct StructExpr |
  -- | A tuple expression
  ExpressionTuple [Expression] |
  -- | An array expression
  ExpressionArray ArrayExpr |
  -- | An index expression
  ExpressionIndex IndexExpr |
  -- | A range expression
  ExpressionRange RangeExpr |
  -- | A return expression
  ExpressionReturn (Maybe Expression) |
  -- | A break expression
  ExpressionBreak (Maybe Expression) |
  -- | A continue expression
  ExpressionContinue  |
  -- | A try expression (expr?)
  ExpressionTry Expression |
  -- | A type cast expression
  ExpressionCast CastExpr |
  -- | A type ascription expression
  ExpressionTypeAscription TypeAscriptionExpr |
  -- | An await expression
  ExpressionAwait Expression |
  -- | An assignment expression
  ExpressionAssign AssignExpr |
  -- | A compound assignment expression
  ExpressionCompoundAssign CompoundAssignExpr |
  -- | A macro invocation expression
  ExpressionMacro MacroInvocation |
  -- | A parenthesized expression
  ExpressionParen Expression
  deriving (Eq, Ord, Read, Show)

_Expression = (Core.Name "hydra.ext.rust.syntax.Expression")

_Expression_literal = (Core.Name "literal")

_Expression_path = (Core.Name "path")

_Expression_block = (Core.Name "block")

_Expression_call = (Core.Name "call")

_Expression_methodCall = (Core.Name "methodCall")

_Expression_fieldAccess = (Core.Name "fieldAccess")

_Expression_tupleIndex = (Core.Name "tupleIndex")

_Expression_closure = (Core.Name "closure")

_Expression_if = (Core.Name "if")

_Expression_match = (Core.Name "match")

_Expression_loop = (Core.Name "loop")

_Expression_while = (Core.Name "while")

_Expression_for = (Core.Name "for")

_Expression_binary = (Core.Name "binary")

_Expression_unary = (Core.Name "unary")

_Expression_reference = (Core.Name "reference")

_Expression_dereference = (Core.Name "dereference")

_Expression_struct = (Core.Name "struct")

_Expression_tuple = (Core.Name "tuple")

_Expression_array = (Core.Name "array")

_Expression_index = (Core.Name "index")

_Expression_range = (Core.Name "range")

_Expression_return = (Core.Name "return")

_Expression_break = (Core.Name "break")

_Expression_continue = (Core.Name "continue")

_Expression_try = (Core.Name "try")

_Expression_cast = (Core.Name "cast")

_Expression_typeAscription = (Core.Name "typeAscription")

_Expression_await = (Core.Name "await")

_Expression_assign = (Core.Name "assign")

_Expression_compoundAssign = (Core.Name "compoundAssign")

_Expression_macro = (Core.Name "macro")

_Expression_paren = (Core.Name "paren")

-- | A path used as an expression
data ExprPath = 
  ExprPath {
    -- | Whether the path is global
    exprPathGlobal :: Bool,
    -- | The path segments
    exprPathSegments :: [PathSegment]}
  deriving (Eq, Ord, Read, Show)

_ExprPath = (Core.Name "hydra.ext.rust.syntax.ExprPath")

_ExprPath_global = (Core.Name "global")

_ExprPath_segments = (Core.Name "segments")

-- | A function call expression
data CallExpr = 
  CallExpr {
    -- | The function being called
    callExprFunction :: Expression,
    -- | The arguments
    callExprArgs :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_CallExpr = (Core.Name "hydra.ext.rust.syntax.CallExpr")

_CallExpr_function = (Core.Name "function")

_CallExpr_args = (Core.Name "args")

-- | A method call expression
data MethodCallExpr = 
  MethodCallExpr {
    -- | The receiver expression
    methodCallExprReceiver :: Expression,
    -- | The method name
    methodCallExprMethod :: String,
    -- | Optional turbofish generic arguments
    methodCallExprTurbofish :: [Type],
    -- | The arguments (excluding the receiver)
    methodCallExprArgs :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_MethodCallExpr = (Core.Name "hydra.ext.rust.syntax.MethodCallExpr")

_MethodCallExpr_receiver = (Core.Name "receiver")

_MethodCallExpr_method = (Core.Name "method")

_MethodCallExpr_turbofish = (Core.Name "turbofish")

_MethodCallExpr_args = (Core.Name "args")

-- | A field access expression
data FieldAccessExpr = 
  FieldAccessExpr {
    -- | The expression being accessed
    fieldAccessExprObject :: Expression,
    -- | The field name
    fieldAccessExprField :: String}
  deriving (Eq, Ord, Read, Show)

_FieldAccessExpr = (Core.Name "hydra.ext.rust.syntax.FieldAccessExpr")

_FieldAccessExpr_object = (Core.Name "object")

_FieldAccessExpr_field = (Core.Name "field")

-- | A tuple index expression
data TupleIndexExpr = 
  TupleIndexExpr {
    -- | The tuple expression
    tupleIndexExprTuple :: Expression,
    -- | The index (0-based)
    tupleIndexExprIndex :: Int}
  deriving (Eq, Ord, Read, Show)

_TupleIndexExpr = (Core.Name "hydra.ext.rust.syntax.TupleIndexExpr")

_TupleIndexExpr_tuple = (Core.Name "tuple")

_TupleIndexExpr_index = (Core.Name "index")

-- | A closure expression
data ClosureExpr = 
  ClosureExpr {
    -- | Whether the closure captures by move
    closureExprMove :: Bool,
    -- | The closure parameters
    closureExprParams :: [ClosureParam],
    -- | Optional return type annotation
    closureExprReturnType :: (Maybe Type),
    -- | The closure body
    closureExprBody :: Expression}
  deriving (Eq, Ord, Read, Show)

_ClosureExpr = (Core.Name "hydra.ext.rust.syntax.ClosureExpr")

_ClosureExpr_move = (Core.Name "move")

_ClosureExpr_params = (Core.Name "params")

_ClosureExpr_returnType = (Core.Name "returnType")

_ClosureExpr_body = (Core.Name "body")

-- | A closure parameter
data ClosureParam = 
  ClosureParam {
    -- | The parameter pattern
    closureParamPattern :: Pattern,
    -- | Optional type annotation
    closureParamType :: (Maybe Type)}
  deriving (Eq, Ord, Read, Show)

_ClosureParam = (Core.Name "hydra.ext.rust.syntax.ClosureParam")

_ClosureParam_pattern = (Core.Name "pattern")

_ClosureParam_type = (Core.Name "type")

-- | An if expression, optionally with if let
data IfExpr = 
  IfExpr {
    -- | The condition
    ifExprCondition :: IfCondition,
    -- | The then block
    ifExprThenBlock :: Block,
    -- | An optional else branch
    ifExprElseBranch :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_IfExpr = (Core.Name "hydra.ext.rust.syntax.IfExpr")

_IfExpr_condition = (Core.Name "condition")

_IfExpr_thenBlock = (Core.Name "thenBlock")

_IfExpr_elseBranch = (Core.Name "elseBranch")

-- | The condition of an if expression
data IfCondition = 
  -- | A boolean condition
  IfConditionBool Expression |
  -- | A let condition
  IfConditionLet LetCondition
  deriving (Eq, Ord, Read, Show)

_IfCondition = (Core.Name "hydra.ext.rust.syntax.IfCondition")

_IfCondition_bool = (Core.Name "bool")

_IfCondition_let = (Core.Name "let")

-- | A match expression
data MatchExpr = 
  MatchExpr {
    -- | The expression being matched
    matchExprScrutinee :: Expression,
    -- | The match arms
    matchExprArms :: [MatchArm]}
  deriving (Eq, Ord, Read, Show)

_MatchExpr = (Core.Name "hydra.ext.rust.syntax.MatchExpr")

_MatchExpr_scrutinee = (Core.Name "scrutinee")

_MatchExpr_arms = (Core.Name "arms")

-- | A single arm in a match expression
data MatchArm = 
  MatchArm {
    -- | The pattern to match
    matchArmPattern :: Pattern,
    -- | Optional guard expression
    matchArmGuard :: (Maybe Expression),
    -- | The body expression
    matchArmBody :: Expression}
  deriving (Eq, Ord, Read, Show)

_MatchArm = (Core.Name "hydra.ext.rust.syntax.MatchArm")

_MatchArm_pattern = (Core.Name "pattern")

_MatchArm_guard = (Core.Name "guard")

_MatchArm_body = (Core.Name "body")

-- | A loop expression
data LoopExpr = 
  LoopExpr {
    -- | Optional loop label
    loopExprLabel :: (Maybe String),
    -- | The loop body
    loopExprBody :: Block}
  deriving (Eq, Ord, Read, Show)

_LoopExpr = (Core.Name "hydra.ext.rust.syntax.LoopExpr")

_LoopExpr_label = (Core.Name "label")

_LoopExpr_body = (Core.Name "body")

-- | A while expression, optionally with while let
data WhileExpr = 
  WhileExpr {
    -- | Optional loop label
    whileExprLabel :: (Maybe String),
    -- | The condition
    whileExprCondition :: IfCondition,
    -- | The loop body
    whileExprBody :: Block}
  deriving (Eq, Ord, Read, Show)

_WhileExpr = (Core.Name "hydra.ext.rust.syntax.WhileExpr")

_WhileExpr_label = (Core.Name "label")

_WhileExpr_condition = (Core.Name "condition")

_WhileExpr_body = (Core.Name "body")

-- | A for expression
data ForExpr = 
  ForExpr {
    -- | Optional loop label
    forExprLabel :: (Maybe String),
    -- | The loop variable pattern
    forExprPattern :: Pattern,
    -- | The iterator expression
    forExprIter :: Expression,
    -- | The loop body
    forExprBody :: Block}
  deriving (Eq, Ord, Read, Show)

_ForExpr = (Core.Name "hydra.ext.rust.syntax.ForExpr")

_ForExpr_label = (Core.Name "label")

_ForExpr_pattern = (Core.Name "pattern")

_ForExpr_iter = (Core.Name "iter")

_ForExpr_body = (Core.Name "body")

-- | A binary operation
data BinaryExpr = 
  BinaryExpr {
    -- | The left-hand operand
    binaryExprLeft :: Expression,
    -- | The binary operator
    binaryExprOp :: BinaryOp,
    -- | The right-hand operand
    binaryExprRight :: Expression}
  deriving (Eq, Ord, Read, Show)

_BinaryExpr = (Core.Name "hydra.ext.rust.syntax.BinaryExpr")

_BinaryExpr_left = (Core.Name "left")

_BinaryExpr_op = (Core.Name "op")

_BinaryExpr_right = (Core.Name "right")

-- | A binary operator
data BinaryOp = 
  BinaryOpAdd  |
  BinaryOpSub  |
  BinaryOpMul  |
  BinaryOpDiv  |
  BinaryOpRem  |
  BinaryOpAnd  |
  BinaryOpOr  |
  BinaryOpBitAnd  |
  BinaryOpBitOr  |
  BinaryOpBitXor  |
  BinaryOpShl  |
  BinaryOpShr  |
  BinaryOpEq  |
  BinaryOpNe  |
  BinaryOpLt  |
  BinaryOpLe  |
  BinaryOpGt  |
  BinaryOpGe 
  deriving (Eq, Ord, Read, Show)

_BinaryOp = (Core.Name "hydra.ext.rust.syntax.BinaryOp")

_BinaryOp_add = (Core.Name "add")

_BinaryOp_sub = (Core.Name "sub")

_BinaryOp_mul = (Core.Name "mul")

_BinaryOp_div = (Core.Name "div")

_BinaryOp_rem = (Core.Name "rem")

_BinaryOp_and = (Core.Name "and")

_BinaryOp_or = (Core.Name "or")

_BinaryOp_bitAnd = (Core.Name "bitAnd")

_BinaryOp_bitOr = (Core.Name "bitOr")

_BinaryOp_bitXor = (Core.Name "bitXor")

_BinaryOp_shl = (Core.Name "shl")

_BinaryOp_shr = (Core.Name "shr")

_BinaryOp_eq = (Core.Name "eq")

_BinaryOp_ne = (Core.Name "ne")

_BinaryOp_lt = (Core.Name "lt")

_BinaryOp_le = (Core.Name "le")

_BinaryOp_gt = (Core.Name "gt")

_BinaryOp_ge = (Core.Name "ge")

-- | A unary operation
data UnaryExpr = 
  UnaryExpr {
    -- | The unary operator
    unaryExprOp :: UnaryOp,
    -- | The operand
    unaryExprOperand :: Expression}
  deriving (Eq, Ord, Read, Show)

_UnaryExpr = (Core.Name "hydra.ext.rust.syntax.UnaryExpr")

_UnaryExpr_op = (Core.Name "op")

_UnaryExpr_operand = (Core.Name "operand")

-- | A unary operator
data UnaryOp = 
  UnaryOpNeg  |
  UnaryOpNot 
  deriving (Eq, Ord, Read, Show)

_UnaryOp = (Core.Name "hydra.ext.rust.syntax.UnaryOp")

_UnaryOp_neg = (Core.Name "neg")

_UnaryOp_not = (Core.Name "not")

-- | A reference expression
data RefExpr = 
  RefExpr {
    -- | Whether the reference is mutable
    refExprMutable :: Bool,
    -- | The expression being referenced
    refExprExpr :: Expression}
  deriving (Eq, Ord, Read, Show)

_RefExpr = (Core.Name "hydra.ext.rust.syntax.RefExpr")

_RefExpr_mutable = (Core.Name "mutable")

_RefExpr_expr = (Core.Name "expr")

-- | A struct literal expression
data StructExpr = 
  StructExpr {
    -- | The struct path
    structExprPath :: ExprPath,
    -- | The field assignments
    structExprFields :: [FieldValue],
    -- | Optional base expression for struct update syntax
    structExprRest :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_StructExpr = (Core.Name "hydra.ext.rust.syntax.StructExpr")

_StructExpr_path = (Core.Name "path")

_StructExpr_fields = (Core.Name "fields")

_StructExpr_rest = (Core.Name "rest")

-- | A field-value pair in a struct literal
data FieldValue = 
  FieldValue {
    -- | The field name
    fieldValueName :: String,
    -- | The field value (None for shorthand syntax)
    fieldValueValue :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_FieldValue = (Core.Name "hydra.ext.rust.syntax.FieldValue")

_FieldValue_name = (Core.Name "name")

_FieldValue_value = (Core.Name "value")

-- | An array expression
data ArrayExpr = 
  -- | An array literal
  ArrayExprElements [Expression] |
  -- | An array repeat expression
  ArrayExprRepeat ArrayRepeat
  deriving (Eq, Ord, Read, Show)

_ArrayExpr = (Core.Name "hydra.ext.rust.syntax.ArrayExpr")

_ArrayExpr_elements = (Core.Name "elements")

_ArrayExpr_repeat = (Core.Name "repeat")

-- | An index expression
data IndexExpr = 
  IndexExpr {
    -- | The expression being indexed
    indexExprObject :: Expression,
    -- | The index expression
    indexExprIndex :: Expression}
  deriving (Eq, Ord, Read, Show)

_IndexExpr = (Core.Name "hydra.ext.rust.syntax.IndexExpr")

_IndexExpr_object = (Core.Name "object")

_IndexExpr_index = (Core.Name "index")

-- | A range expression
data RangeExpr = 
  RangeExpr {
    -- | The lower bound (optional)
    rangeExprFrom :: (Maybe Expression),
    -- | The upper bound (optional)
    rangeExprTo :: (Maybe Expression),
    -- | Whether the range is inclusive
    rangeExprInclusive :: Bool}
  deriving (Eq, Ord, Read, Show)

_RangeExpr = (Core.Name "hydra.ext.rust.syntax.RangeExpr")

_RangeExpr_from = (Core.Name "from")

_RangeExpr_to = (Core.Name "to")

_RangeExpr_inclusive = (Core.Name "inclusive")

-- | A type cast expression
data CastExpr = 
  CastExpr {
    -- | The expression being cast
    castExprExpr :: Expression,
    -- | The target type
    castExprType :: Type}
  deriving (Eq, Ord, Read, Show)

_CastExpr = (Core.Name "hydra.ext.rust.syntax.CastExpr")

_CastExpr_expr = (Core.Name "expr")

_CastExpr_type = (Core.Name "type")

-- | A type ascription expression
data TypeAscriptionExpr = 
  TypeAscriptionExpr {
    -- | The expression
    typeAscriptionExprExpr :: Expression,
    -- | The ascribed type
    typeAscriptionExprType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeAscriptionExpr = (Core.Name "hydra.ext.rust.syntax.TypeAscriptionExpr")

_TypeAscriptionExpr_expr = (Core.Name "expr")

_TypeAscriptionExpr_type = (Core.Name "type")

-- | An assignment expression
data AssignExpr = 
  AssignExpr {
    -- | The left-hand side (target)
    assignExprTarget :: Expression,
    -- | The right-hand side (value)
    assignExprValue :: Expression}
  deriving (Eq, Ord, Read, Show)

_AssignExpr = (Core.Name "hydra.ext.rust.syntax.AssignExpr")

_AssignExpr_target = (Core.Name "target")

_AssignExpr_value = (Core.Name "value")

-- | A compound assignment expression
data CompoundAssignExpr = 
  CompoundAssignExpr {
    -- | The left-hand side (target)
    compoundAssignExprTarget :: Expression,
    -- | The compound assignment operator
    compoundAssignExprOp :: CompoundAssignOp,
    -- | The right-hand side (value)
    compoundAssignExprValue :: Expression}
  deriving (Eq, Ord, Read, Show)

_CompoundAssignExpr = (Core.Name "hydra.ext.rust.syntax.CompoundAssignExpr")

_CompoundAssignExpr_target = (Core.Name "target")

_CompoundAssignExpr_op = (Core.Name "op")

_CompoundAssignExpr_value = (Core.Name "value")

-- | A compound assignment operator
data CompoundAssignOp = 
  CompoundAssignOpAddAssign  |
  CompoundAssignOpSubAssign  |
  CompoundAssignOpMulAssign  |
  CompoundAssignOpDivAssign  |
  CompoundAssignOpRemAssign  |
  CompoundAssignOpBitAndAssign  |
  CompoundAssignOpBitOrAssign  |
  CompoundAssignOpBitXorAssign  |
  CompoundAssignOpShlAssign  |
  CompoundAssignOpShrAssign 
  deriving (Eq, Ord, Read, Show)

_CompoundAssignOp = (Core.Name "hydra.ext.rust.syntax.CompoundAssignOp")

_CompoundAssignOp_addAssign = (Core.Name "addAssign")

_CompoundAssignOp_subAssign = (Core.Name "subAssign")

_CompoundAssignOp_mulAssign = (Core.Name "mulAssign")

_CompoundAssignOp_divAssign = (Core.Name "divAssign")

_CompoundAssignOp_remAssign = (Core.Name "remAssign")

_CompoundAssignOp_bitAndAssign = (Core.Name "bitAndAssign")

_CompoundAssignOp_bitOrAssign = (Core.Name "bitOrAssign")

_CompoundAssignOp_bitXorAssign = (Core.Name "bitXorAssign")

_CompoundAssignOp_shlAssign = (Core.Name "shlAssign")

_CompoundAssignOp_shrAssign = (Core.Name "shrAssign")

-- | A macro invocation
data MacroInvocation = 
  MacroInvocation {
    -- | The macro path
    macroInvocationPath :: [String],
    -- | The delimiter style used
    macroInvocationDelimiter :: MacroDelimiter,
    -- | The token stream as a raw string
    macroInvocationTokens :: String}
  deriving (Eq, Ord, Read, Show)

_MacroInvocation = (Core.Name "hydra.ext.rust.syntax.MacroInvocation")

_MacroInvocation_path = (Core.Name "path")

_MacroInvocation_delimiter = (Core.Name "delimiter")

_MacroInvocation_tokens = (Core.Name "tokens")

-- | The delimiter style for a macro invocation
data MacroDelimiter = 
  MacroDelimiterParen  |
  MacroDelimiterBracket  |
  MacroDelimiterBrace 
  deriving (Eq, Ord, Read, Show)

_MacroDelimiter = (Core.Name "hydra.ext.rust.syntax.MacroDelimiter")

_MacroDelimiter_paren = (Core.Name "paren")

_MacroDelimiter_bracket = (Core.Name "bracket")

_MacroDelimiter_brace = (Core.Name "brace")

-- | A statement within a block
data Statement = 
  -- | A let binding
  StatementLet LetStatement |
  -- | An expression statement
  StatementExpression Expression |
  -- | An item declaration within a block
  StatementItem Item |
  -- | An empty statement
  StatementEmpty 
  deriving (Eq, Ord, Read, Show)

_Statement = (Core.Name "hydra.ext.rust.syntax.Statement")

_Statement_let = (Core.Name "let")

_Statement_expression = (Core.Name "expression")

_Statement_item = (Core.Name "item")

_Statement_empty = (Core.Name "empty")

-- | A let statement
data LetStatement = 
  LetStatement {
    -- | The binding pattern
    letStatementPattern :: Pattern,
    -- | Whether the binding is mutable
    letStatementMutable :: Bool,
    -- | Optional type annotation
    letStatementType :: (Maybe Type),
    -- | Optional initializer expression
    letStatementInit :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_LetStatement = (Core.Name "hydra.ext.rust.syntax.LetStatement")

_LetStatement_pattern = (Core.Name "pattern")

_LetStatement_mutable = (Core.Name "mutable")

_LetStatement_type = (Core.Name "type")

_LetStatement_init = (Core.Name "init")

-- | A block expression
data Block = 
  Block {
    -- | The statements in the block
    blockStatements :: [Statement],
    -- | An optional trailing expression
    blockExpression :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_Block = (Core.Name "hydra.ext.rust.syntax.Block")

_Block_statements = (Core.Name "statements")

_Block_expression = (Core.Name "expression")

-- | A Rust pattern (used in let, match, function parameters, etc.)
data Pattern = 
  -- | A wildcard pattern (_)
  PatternWildcard  |
  -- | An identifier pattern
  PatternIdentifier IdentifierPattern |
  -- | A literal pattern
  PatternLiteral Literal |
  -- | A reference pattern
  PatternReference RefPattern |
  -- | A struct pattern
  PatternStruct StructPattern |
  -- | A tuple struct pattern
  PatternTupleStruct TupleStructPattern |
  -- | A tuple pattern
  PatternTuple [Pattern] |
  -- | A slice pattern
  PatternSlice [Pattern] |
  -- | An or-pattern
  PatternOr [Pattern] |
  -- | A path pattern
  PatternPath ExprPath |
  -- | A range pattern
  PatternRange RangePattern |
  -- | A rest pattern (..)
  PatternRest  |
  -- | A parenthesized pattern
  PatternParen Pattern
  deriving (Eq, Ord, Read, Show)

_Pattern = (Core.Name "hydra.ext.rust.syntax.Pattern")

_Pattern_wildcard = (Core.Name "wildcard")

_Pattern_identifier = (Core.Name "identifier")

_Pattern_literal = (Core.Name "literal")

_Pattern_reference = (Core.Name "reference")

_Pattern_struct = (Core.Name "struct")

_Pattern_tupleStruct = (Core.Name "tupleStruct")

_Pattern_tuple = (Core.Name "tuple")

_Pattern_slice = (Core.Name "slice")

_Pattern_or = (Core.Name "or")

_Pattern_path = (Core.Name "path")

_Pattern_range = (Core.Name "range")

_Pattern_rest = (Core.Name "rest")

_Pattern_paren = (Core.Name "paren")

-- | An identifier pattern
data IdentifierPattern = 
  IdentifierPattern {
    -- | The identifier name
    identifierPatternName :: String,
    -- | Whether the binding is mutable
    identifierPatternMutable :: Bool,
    -- | Optional sub-pattern (e.g., x @ Some(_))
    identifierPatternAtPattern :: (Maybe Pattern)}
  deriving (Eq, Ord, Read, Show)

_IdentifierPattern = (Core.Name "hydra.ext.rust.syntax.IdentifierPattern")

_IdentifierPattern_name = (Core.Name "name")

_IdentifierPattern_mutable = (Core.Name "mutable")

_IdentifierPattern_atPattern = (Core.Name "atPattern")

-- | A reference pattern
data RefPattern = 
  RefPattern {
    -- | Whether the reference is mutable
    refPatternMutable :: Bool,
    -- | The inner pattern
    refPatternPattern :: Pattern}
  deriving (Eq, Ord, Read, Show)

_RefPattern = (Core.Name "hydra.ext.rust.syntax.RefPattern")

_RefPattern_mutable = (Core.Name "mutable")

_RefPattern_pattern = (Core.Name "pattern")

-- | A struct pattern
data StructPattern = 
  StructPattern {
    -- | The struct path
    structPatternPath :: ExprPath,
    -- | The field patterns
    structPatternFields :: [FieldPattern],
    -- | Whether the pattern has a rest (..) at the end
    structPatternRest :: Bool}
  deriving (Eq, Ord, Read, Show)

_StructPattern = (Core.Name "hydra.ext.rust.syntax.StructPattern")

_StructPattern_path = (Core.Name "path")

_StructPattern_fields = (Core.Name "fields")

_StructPattern_rest = (Core.Name "rest")

-- | A field pattern within a struct pattern
data FieldPattern = 
  FieldPattern {
    -- | The field name
    fieldPatternName :: String,
    -- | The field pattern (None for shorthand)
    fieldPatternPattern :: (Maybe Pattern)}
  deriving (Eq, Ord, Read, Show)

_FieldPattern = (Core.Name "hydra.ext.rust.syntax.FieldPattern")

_FieldPattern_name = (Core.Name "name")

_FieldPattern_pattern = (Core.Name "pattern")

-- | A tuple struct pattern
data TupleStructPattern = 
  TupleStructPattern {
    -- | The path to the tuple struct or variant
    tupleStructPatternPath :: ExprPath,
    -- | The element patterns
    tupleStructPatternElements :: [Pattern]}
  deriving (Eq, Ord, Read, Show)

_TupleStructPattern = (Core.Name "hydra.ext.rust.syntax.TupleStructPattern")

_TupleStructPattern_path = (Core.Name "path")

_TupleStructPattern_elements = (Core.Name "elements")

-- | A range pattern
data RangePattern = 
  RangePattern {
    -- | The lower bound
    rangePatternFrom :: (Maybe Pattern),
    -- | The upper bound
    rangePatternTo :: (Maybe Pattern),
    -- | Whether the range is inclusive
    rangePatternInclusive :: Bool}
  deriving (Eq, Ord, Read, Show)

_RangePattern = (Core.Name "hydra.ext.rust.syntax.RangePattern")

_RangePattern_from = (Core.Name "from")

_RangePattern_to = (Core.Name "to")

_RangePattern_inclusive = (Core.Name "inclusive")

-- | A literal value
data Literal = 
  -- | An integer literal
  LiteralInteger IntegerLiteral |
  -- | A floating-point literal
  LiteralFloat FloatLiteral |
  -- | A string literal
  LiteralString String |
  -- | A raw string literal
  LiteralRawString String |
  -- | A byte string literal
  LiteralByteString B.ByteString |
  -- | A character literal
  LiteralChar I.Int64 |
  -- | A byte literal
  LiteralByte I.Int16 |
  -- | A boolean literal
  LiteralBool Bool
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra.ext.rust.syntax.Literal")

_Literal_integer = (Core.Name "integer")

_Literal_float = (Core.Name "float")

_Literal_string = (Core.Name "string")

_Literal_rawString = (Core.Name "rawString")

_Literal_byteString = (Core.Name "byteString")

_Literal_char = (Core.Name "char")

_Literal_byte = (Core.Name "byte")

_Literal_bool = (Core.Name "bool")

-- | An integer literal with optional suffix
data IntegerLiteral = 
  IntegerLiteral {
    -- | The integer value
    integerLiteralValue :: Integer,
    -- | Optional type suffix
    integerLiteralSuffix :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_IntegerLiteral = (Core.Name "hydra.ext.rust.syntax.IntegerLiteral")

_IntegerLiteral_value = (Core.Name "value")

_IntegerLiteral_suffix = (Core.Name "suffix")

-- | A floating-point literal with optional suffix
data FloatLiteral = 
  FloatLiteral {
    -- | The float value
    floatLiteralValue :: Double,
    -- | Optional type suffix
    floatLiteralSuffix :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_FloatLiteral = (Core.Name "hydra.ext.rust.syntax.FloatLiteral")

_FloatLiteral_value = (Core.Name "value")

_FloatLiteral_suffix = (Core.Name "suffix")

-- | An attribute (e.g., #[derive(Clone)], #[cfg(test)])
data Attribute = 
  Attribute {
    -- | Whether the attribute is an inner attribute (#![...] vs #[...])
    attributeInner :: Bool,
    -- | The attribute path
    attributePath :: [String],
    -- | The attribute arguments as a raw token string
    attributeTokens :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Attribute = (Core.Name "hydra.ext.rust.syntax.Attribute")

_Attribute_inner = (Core.Name "inner")

_Attribute_path = (Core.Name "path")

_Attribute_tokens = (Core.Name "tokens")

-- | A visibility qualifier
data Visibility = 
  -- | Public (pub)
  VisibilityPublic  |
  -- | Crate-visible (pub(crate))
  VisibilityCrate  |
  -- | Visible to a specific path (pub(in path))
  VisibilityRestricted [String] |
  -- | Private (default)
  VisibilityPrivate 
  deriving (Eq, Ord, Read, Show)

_Visibility = (Core.Name "hydra.ext.rust.syntax.Visibility")

_Visibility_public = (Core.Name "public")

_Visibility_crate = (Core.Name "crate")

_Visibility_restricted = (Core.Name "restricted")

_Visibility_private = (Core.Name "private")

-- | A let condition (e.g., let Some(x) = opt)
data LetCondition = 
  LetCondition {
    -- | The pattern
    letConditionPattern :: Pattern,
    -- | The expression being matched
    letConditionExpr :: Expression}
  deriving (Eq, Ord, Read, Show)

_LetCondition = (Core.Name "hydra.ext.rust.syntax.LetCondition")

_LetCondition_pattern = (Core.Name "pattern")

_LetCondition_expr = (Core.Name "expr")

-- | An array repeat expression (e.g., [0; 10])
data ArrayRepeat = 
  ArrayRepeat {
    -- | The element expression
    arrayRepeatElement :: Expression,
    -- | The length expression
    arrayRepeatLength :: Expression}
  deriving (Eq, Ord, Read, Show)

_ArrayRepeat = (Core.Name "hydra.ext.rust.syntax.ArrayRepeat")

_ArrayRepeat_element = (Core.Name "element")

_ArrayRepeat_length = (Core.Name "length")
