# Note: this is an automatically generated file. Do not edit.

r"""A Rust syntax model, based on the Rust Reference grammar (https://doc.rust-lang.org/reference/), retrieved 2025-01-29."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class Crate:
    r"""A Rust crate, represented as a collection of top-level items."""

    items: Annotated[frozenlist[ItemWithComments], "The top-level items in the crate"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.Crate")
    ITEMS = hydra.core.Name("items")

class ItemUse(Node["UseDeclaration"]):
    r"""A use declaration"""

class ItemStruct(Node["StructDef"]):
    r"""A struct definition"""

class ItemEnum(Node["EnumDef"]):
    r"""An enum definition"""

class ItemFn(Node["FnDef"]):
    r"""A function definition"""

class ItemTypeAlias(Node["TypeAlias"]):
    r"""A type alias"""

class ItemImpl(Node["ImplBlock"]):
    r"""An impl block"""

class ItemTrait(Node["TraitDef"]):
    r"""A trait definition"""

class ItemMod(Node["ModDef"]):
    r"""A module definition"""

class ItemConst(Node["ConstDef"]):
    r"""A constant item"""

class ItemStatic(Node["StaticDef"]):
    r"""A static item"""

class ItemMacro(Node["MacroInvocation"]):
    r"""A macro invocation as an item"""

class _ItemMeta(type):
    def __getitem__(cls, item):
        return object

# A top-level item in a Rust module or crate.
class Item(metaclass=_ItemMeta):
    r"""ItemUse | ItemStruct | ItemEnum | ItemFn | ItemTypeAlias | ItemImpl | ItemTrait | ItemMod | ItemConst | ItemStatic | ItemMacro"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.Item")
    USE = hydra.core.Name("use")
    STRUCT = hydra.core.Name("struct")
    ENUM = hydra.core.Name("enum")
    FN = hydra.core.Name("fn")
    TYPE_ALIAS = hydra.core.Name("typeAlias")
    IMPL = hydra.core.Name("impl")
    TRAIT = hydra.core.Name("trait")
    MOD = hydra.core.Name("mod")
    CONST = hydra.core.Name("const")
    STATIC = hydra.core.Name("static")
    MACRO = hydra.core.Name("macro")

@dataclass(frozen=True)
class ItemWithComments:
    r"""An item together with optional doc comments and visibility."""

    doc: Annotated[Maybe[str], "Optional documentation comment"]
    visibility: Annotated[Visibility, "The item's visibility"]
    item: Annotated[Item, "The item itself"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.ItemWithComments")
    DOC = hydra.core.Name("doc")
    VISIBILITY = hydra.core.Name("visibility")
    ITEM = hydra.core.Name("item")

@dataclass(frozen=True)
class UseDeclaration:
    r"""A use declaration (e.g., use std::collections::BTreeMap;)."""

    public: Annotated[bool, "Whether the use is public (pub use)"]
    tree: Annotated[UseTree, "The use tree describing what is imported"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.UseDeclaration")
    PUBLIC = hydra.core.Name("public")
    TREE = hydra.core.Name("tree")

class UseTreePath(Node["UsePath"]):
    r"""A simple path import"""

class UseTreeRename(Node["UseRename"]):
    r"""A renamed import (e.g., BTreeMap as Map)"""

class UseTreeGlob(Node[frozenlist[str]]):
    r"""A glob import (e.g., std::collections::*)"""

class UseTreeGroup(Node["UseGroup"]):
    r"""A grouped import (e.g., {BTreeMap, BTreeSet})"""

class _UseTreeMeta(type):
    def __getitem__(cls, item):
        return object

# A use tree, representing the structure of a use path.
class UseTree(metaclass=_UseTreeMeta):
    r"""UseTreePath | UseTreeRename | UseTreeGlob | UseTreeGroup"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.UseTree")
    PATH = hydra.core.Name("path")
    RENAME = hydra.core.Name("rename")
    GLOB = hydra.core.Name("glob")
    GROUP = hydra.core.Name("group")

@dataclass(frozen=True)
class UsePath:
    r"""A simple path import within a use tree."""

    segments: Annotated[frozenlist[str], "The path segments"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.UsePath")
    SEGMENTS = hydra.core.Name("segments")

@dataclass(frozen=True)
class UseRename:
    r"""A renamed import (e.g., BTreeMap as Map)."""

    path: Annotated[frozenlist[str], "The original path segments"]
    alias: Annotated[str, "The alias name"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.UseRename")
    PATH = hydra.core.Name("path")
    ALIAS = hydra.core.Name("alias")

@dataclass(frozen=True)
class UseGroup:
    r"""A grouped import (e.g., std::collections::{BTreeMap, BTreeSet})."""

    prefix: Annotated[frozenlist[str], "The common prefix path segments"]
    trees: Annotated[frozenlist[UseTree], "The individual subtrees within the group"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.UseGroup")
    PREFIX = hydra.core.Name("prefix")
    TREES = hydra.core.Name("trees")

@dataclass(frozen=True)
class StructDef:
    r"""A struct definition (e.g., struct Foo<T> { bar: T })."""

    name: Annotated[str, "The struct name"]
    generics: Annotated[frozenlist[GenericParam], "Generic type parameters"]
    where_clause: Annotated[Maybe[WhereClause], "Optional where clause"]
    body: Annotated[StructBody, "The struct body (named fields, tuple fields, or unit)"]
    derives: Annotated[frozenlist[str], "Derive macros to apply"]
    public: Annotated[bool, "Whether the struct is public"]
    doc: Annotated[Maybe[str], "Optional doc comment"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.StructDef")
    NAME = hydra.core.Name("name")
    GENERICS = hydra.core.Name("generics")
    WHERE_CLAUSE = hydra.core.Name("whereClause")
    BODY = hydra.core.Name("body")
    DERIVES = hydra.core.Name("derives")
    PUBLIC = hydra.core.Name("public")
    DOC = hydra.core.Name("doc")

class StructBodyNamed(Node["frozenlist[StructField]"]):
    r"""A struct with named fields"""

class StructBodyTuple(Node["frozenlist[TupleField]"]):
    r"""A tuple struct"""

class StructBodyUnit:
    r"""A unit struct"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, StructBodyUnit)
    def __hash__(self):
        return hash("StructBodyUnit")

class _StructBodyMeta(type):
    def __getitem__(cls, item):
        return object

# The body of a struct definition.
class StructBody(metaclass=_StructBodyMeta):
    r"""StructBodyNamed | StructBodyTuple | StructBodyUnit"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.StructBody")
    NAMED = hydra.core.Name("named")
    TUPLE = hydra.core.Name("tuple")
    UNIT = hydra.core.Name("unit")

@dataclass(frozen=True)
class StructField:
    r"""A named field within a struct definition."""

    name: Annotated[str, "The field name"]
    type: Annotated[Type, "The field type"]
    public: Annotated[bool, "Whether the field is public"]
    doc: Annotated[Maybe[str], "Optional doc comment for the field"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.StructField")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")
    PUBLIC = hydra.core.Name("public")
    DOC = hydra.core.Name("doc")

@dataclass(frozen=True)
class TupleField:
    r"""A positional field within a tuple struct."""

    type: Annotated[Type, "The field type"]
    public: Annotated[bool, "Whether the field is public"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.TupleField")
    TYPE = hydra.core.Name("type")
    PUBLIC = hydra.core.Name("public")

@dataclass(frozen=True)
class EnumDef:
    r"""An enum definition (e.g., enum Foo<T> { Bar(T), Baz { x: i32 } })."""

    name: Annotated[str, "The enum name"]
    generics: Annotated[frozenlist[GenericParam], "Generic type parameters"]
    where_clause: Annotated[Maybe[WhereClause], "Optional where clause"]
    variants: Annotated[frozenlist[EnumVariant], "The enum variants"]
    derives: Annotated[frozenlist[str], "Derive macros to apply"]
    public: Annotated[bool, "Whether the enum is public"]
    doc: Annotated[Maybe[str], "Optional doc comment"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.EnumDef")
    NAME = hydra.core.Name("name")
    GENERICS = hydra.core.Name("generics")
    WHERE_CLAUSE = hydra.core.Name("whereClause")
    VARIANTS = hydra.core.Name("variants")
    DERIVES = hydra.core.Name("derives")
    PUBLIC = hydra.core.Name("public")
    DOC = hydra.core.Name("doc")

@dataclass(frozen=True)
class EnumVariant:
    r"""A variant of an enum definition."""

    name: Annotated[str, "The variant name"]
    body: Annotated[EnumVariantBody, "The variant body"]
    doc: Annotated[Maybe[str], "Optional doc comment for the variant"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.EnumVariant")
    NAME = hydra.core.Name("name")
    BODY = hydra.core.Name("body")
    DOC = hydra.core.Name("doc")

class EnumVariantBodyUnit:
    r"""A unit variant (e.g., Foo)"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, EnumVariantBodyUnit)
    def __hash__(self):
        return hash("EnumVariantBodyUnit")

class EnumVariantBodyTuple(Node["frozenlist[Type]"]):
    r"""A tuple variant (e.g., Foo(i32, String))"""

class EnumVariantBodyStruct(Node["frozenlist[StructField]"]):
    r"""A struct variant (e.g., Foo { x: i32 })"""

class _EnumVariantBodyMeta(type):
    def __getitem__(cls, item):
        return object

# The body of an enum variant.
class EnumVariantBody(metaclass=_EnumVariantBodyMeta):
    r"""EnumVariantBodyUnit | EnumVariantBodyTuple | EnumVariantBodyStruct"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.EnumVariantBody")
    UNIT = hydra.core.Name("unit")
    TUPLE = hydra.core.Name("tuple")
    STRUCT = hydra.core.Name("struct")

@dataclass(frozen=True)
class FnDef:
    r"""A function definition (e.g., fn foo<T>(x: T) -> String { ... })."""

    name: Annotated[str, "The function name"]
    generics: Annotated[frozenlist[GenericParam], "Generic type parameters"]
    where_clause: Annotated[Maybe[WhereClause], "Optional where clause"]
    params: Annotated[frozenlist[FnParam], "The function parameters"]
    return_type: Annotated[Maybe[Type], "The return type (None means ())"]
    body: Annotated[Block, "The function body"]
    public: Annotated[bool, "Whether the function is public"]
    async_: Annotated[bool, "Whether the function is async"]
    const: Annotated[bool, "Whether the function is const"]
    unsafe: Annotated[bool, "Whether the function is unsafe"]
    doc: Annotated[Maybe[str], "Optional doc comment"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.FnDef")
    NAME = hydra.core.Name("name")
    GENERICS = hydra.core.Name("generics")
    WHERE_CLAUSE = hydra.core.Name("whereClause")
    PARAMS = hydra.core.Name("params")
    RETURN_TYPE = hydra.core.Name("returnType")
    BODY = hydra.core.Name("body")
    PUBLIC = hydra.core.Name("public")
    ASYNC = hydra.core.Name("async")
    CONST = hydra.core.Name("const")
    UNSAFE = hydra.core.Name("unsafe")
    DOC = hydra.core.Name("doc")

@dataclass(frozen=True)
class FnParam:
    r"""A function parameter."""

    pattern: Annotated[Pattern, "The parameter pattern"]
    type: Annotated[Type, "The parameter type"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.FnParam")
    PATTERN = hydra.core.Name("pattern")
    TYPE = hydra.core.Name("type")

class SelfParam(Enum):
    r"""A self parameter in a method."""

    OWNED = hydra.core.Name("owned")

    REF = hydra.core.Name("ref")

    REF_MUT = hydra.core.Name("refMut")

SelfParam.TYPE_ = hydra.core.Name("hydra.rust.syntax.SelfParam")

class MethodParamSelf(Node["SelfParam"]):
    r"""A self parameter"""

class MethodParamRegular(Node["FnParam"]):
    r"""A regular function parameter"""

class _MethodParamMeta(type):
    def __getitem__(cls, item):
        return object

# A method parameter, which may be self or a regular parameter.
class MethodParam(metaclass=_MethodParamMeta):
    r"""MethodParamSelf | MethodParamRegular"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.MethodParam")
    SELF = hydra.core.Name("self")
    REGULAR = hydra.core.Name("regular")

@dataclass(frozen=True)
class TypeAlias:
    r"""A type alias definition (e.g., type Foo<T> = Bar<T>;)."""

    name: Annotated[str, "The alias name"]
    generics: Annotated[frozenlist[GenericParam], "Generic type parameters"]
    type: Annotated[Type, "The aliased type"]
    public: Annotated[bool, "Whether the alias is public"]
    doc: Annotated[Maybe[str], "Optional doc comment"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.TypeAlias")
    NAME = hydra.core.Name("name")
    GENERICS = hydra.core.Name("generics")
    TYPE = hydra.core.Name("type")
    PUBLIC = hydra.core.Name("public")
    DOC = hydra.core.Name("doc")

@dataclass(frozen=True)
class ConstDef:
    r"""A constant item (e.g., const FOO: u32 = 42;)."""

    name: Annotated[str, "The constant name"]
    type: Annotated[Type, "The constant type"]
    value: Annotated[Expression, "The constant value expression"]
    public: Annotated[bool, "Whether the constant is public"]
    doc: Annotated[Maybe[str], "Optional doc comment"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.ConstDef")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")
    VALUE = hydra.core.Name("value")
    PUBLIC = hydra.core.Name("public")
    DOC = hydra.core.Name("doc")

@dataclass(frozen=True)
class StaticDef:
    r"""A static item (e.g., static FOO: u32 = 42;)."""

    name: Annotated[str, "The static name"]
    type: Annotated[Type, "The static type"]
    value: Annotated[Expression, "The static value expression"]
    mutable: Annotated[bool, "Whether the static is mutable"]
    public: Annotated[bool, "Whether the static is public"]
    doc: Annotated[Maybe[str], "Optional doc comment"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.StaticDef")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")
    VALUE = hydra.core.Name("value")
    MUTABLE = hydra.core.Name("mutable")
    PUBLIC = hydra.core.Name("public")
    DOC = hydra.core.Name("doc")

@dataclass(frozen=True)
class ModDef:
    r"""A module definition (either inline or external)."""

    name: Annotated[str, "The module name"]
    body: Annotated[Maybe[frozenlist[Item]], "The module body (None for external file)"]
    public: Annotated[bool, "Whether the module is public"]
    doc: Annotated[Maybe[str], "Optional doc comment"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.ModDef")
    NAME = hydra.core.Name("name")
    BODY = hydra.core.Name("body")
    PUBLIC = hydra.core.Name("public")
    DOC = hydra.core.Name("doc")

@dataclass(frozen=True)
class ImplBlock:
    r"""An impl block (e.g., impl<T> Trait for Foo<T> { ... })."""

    generics: Annotated[frozenlist[GenericParam], "Generic type parameters"]
    where_clause: Annotated[Maybe[WhereClause], "Optional where clause"]
    trait: Annotated[Maybe[TypePath], "The trait being implemented, if any"]
    negative: Annotated[bool, "Whether this is a negative impl"]
    self_type: Annotated[Type, "The type being implemented for"]
    items: Annotated[frozenlist[ImplItem], "The items within the impl block"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.ImplBlock")
    GENERICS = hydra.core.Name("generics")
    WHERE_CLAUSE = hydra.core.Name("whereClause")
    TRAIT = hydra.core.Name("trait")
    NEGATIVE = hydra.core.Name("negative")
    SELF_TYPE = hydra.core.Name("selfType")
    ITEMS = hydra.core.Name("items")

class ImplItemMethod(Node["ImplMethod"]):
    r"""A method definition"""

class ImplItemType(Node["TypeAlias"]):
    r"""An associated type definition"""

class ImplItemConst(Node["ConstDef"]):
    r"""An associated constant"""

class _ImplItemMeta(type):
    def __getitem__(cls, item):
        return object

# An item within an impl block.
class ImplItem(metaclass=_ImplItemMeta):
    r"""ImplItemMethod | ImplItemType | ImplItemConst"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.ImplItem")
    METHOD = hydra.core.Name("method")
    TYPE = hydra.core.Name("type")
    CONST = hydra.core.Name("const")

@dataclass(frozen=True)
class ImplMethod:
    r"""A method within an impl block."""

    name: Annotated[str, "The method name"]
    generics: Annotated[frozenlist[GenericParam], "Generic type parameters"]
    where_clause: Annotated[Maybe[WhereClause], "Optional where clause"]
    params: Annotated[frozenlist[MethodParam], "The method parameters (including self)"]
    return_type: Annotated[Maybe[Type], "The return type (None means ())"]
    body: Annotated[Block, "The method body"]
    public: Annotated[bool, "Whether the method is public"]
    default: Annotated[bool, "Whether the method has a default implementation"]
    doc: Annotated[Maybe[str], "Optional doc comment"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.ImplMethod")
    NAME = hydra.core.Name("name")
    GENERICS = hydra.core.Name("generics")
    WHERE_CLAUSE = hydra.core.Name("whereClause")
    PARAMS = hydra.core.Name("params")
    RETURN_TYPE = hydra.core.Name("returnType")
    BODY = hydra.core.Name("body")
    PUBLIC = hydra.core.Name("public")
    DEFAULT = hydra.core.Name("default")
    DOC = hydra.core.Name("doc")

@dataclass(frozen=True)
class TraitDef:
    r"""A trait definition (e.g., trait Foo<T>: Bar + Baz { ... })."""

    name: Annotated[str, "The trait name"]
    generics: Annotated[frozenlist[GenericParam], "Generic type parameters"]
    where_clause: Annotated[Maybe[WhereClause], "Optional where clause"]
    super_traits: Annotated[frozenlist[TypeParamBound], "Supertraits"]
    items: Annotated[frozenlist[TraitItem], "The items within the trait"]
    public: Annotated[bool, "Whether the trait is public"]
    unsafe: Annotated[bool, "Whether the trait is unsafe"]
    doc: Annotated[Maybe[str], "Optional doc comment"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.TraitDef")
    NAME = hydra.core.Name("name")
    GENERICS = hydra.core.Name("generics")
    WHERE_CLAUSE = hydra.core.Name("whereClause")
    SUPER_TRAITS = hydra.core.Name("superTraits")
    ITEMS = hydra.core.Name("items")
    PUBLIC = hydra.core.Name("public")
    UNSAFE = hydra.core.Name("unsafe")
    DOC = hydra.core.Name("doc")

class TraitItemMethod(Node["TraitMethod"]):
    r"""A method signature or default method"""

class TraitItemType(Node["TraitType"]):
    r"""An associated type"""

class TraitItemConst(Node["TraitConst"]):
    r"""An associated constant"""

class _TraitItemMeta(type):
    def __getitem__(cls, item):
        return object

# An item within a trait definition.
class TraitItem(metaclass=_TraitItemMeta):
    r"""TraitItemMethod | TraitItemType | TraitItemConst"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.TraitItem")
    METHOD = hydra.core.Name("method")
    TYPE = hydra.core.Name("type")
    CONST = hydra.core.Name("const")

@dataclass(frozen=True)
class TraitMethod:
    r"""A method signature or default method within a trait."""

    name: Annotated[str, "The method name"]
    generics: Annotated[frozenlist[GenericParam], "Generic type parameters"]
    where_clause: Annotated[Maybe[WhereClause], "Optional where clause"]
    params: Annotated[frozenlist[MethodParam], "The method parameters (including self)"]
    return_type: Annotated[Maybe[Type], "The return type"]
    default_body: Annotated[Maybe[Block], "Optional default body"]
    doc: Annotated[Maybe[str], "Optional doc comment"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.TraitMethod")
    NAME = hydra.core.Name("name")
    GENERICS = hydra.core.Name("generics")
    WHERE_CLAUSE = hydra.core.Name("whereClause")
    PARAMS = hydra.core.Name("params")
    RETURN_TYPE = hydra.core.Name("returnType")
    DEFAULT_BODY = hydra.core.Name("defaultBody")
    DOC = hydra.core.Name("doc")

@dataclass(frozen=True)
class TraitType:
    r"""An associated type within a trait."""

    name: Annotated[str, "The associated type name"]
    bounds: Annotated[frozenlist[TypeParamBound], "Type parameter bounds"]
    default: Annotated[Maybe[Type], "Optional default type"]
    doc: Annotated[Maybe[str], "Optional doc comment"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.TraitType")
    NAME = hydra.core.Name("name")
    BOUNDS = hydra.core.Name("bounds")
    DEFAULT = hydra.core.Name("default")
    DOC = hydra.core.Name("doc")

@dataclass(frozen=True)
class TraitConst:
    r"""An associated constant within a trait."""

    name: Annotated[str, "The constant name"]
    type: Annotated[Type, "The constant type"]
    default: Annotated[Maybe[Expression], "Optional default value"]
    doc: Annotated[Maybe[str], "Optional doc comment"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.TraitConst")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")
    DEFAULT = hydra.core.Name("default")
    DOC = hydra.core.Name("doc")

@dataclass(frozen=True)
class GenericParam:
    r"""A generic type parameter (e.g., T: Clone + Debug)."""

    name: Annotated[str, "The parameter name"]
    bounds: Annotated[frozenlist[TypeParamBound], "Trait bounds on the parameter"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.GenericParam")
    NAME = hydra.core.Name("name")
    BOUNDS = hydra.core.Name("bounds")

class TypeParamBoundTrait(Node["TypePath"]):
    r"""A trait bound"""

class TypeParamBoundLifetime(Node["Lifetime"]):
    r"""A lifetime bound"""

class _TypeParamBoundMeta(type):
    def __getitem__(cls, item):
        return object

# A bound on a type parameter.
class TypeParamBound(metaclass=_TypeParamBoundMeta):
    r"""TypeParamBoundTrait | TypeParamBoundLifetime"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.TypeParamBound")
    TRAIT = hydra.core.Name("trait")
    LIFETIME = hydra.core.Name("lifetime")

@dataclass(frozen=True)
class Lifetime:
    r"""A lifetime (e.g., 'a, 'static)."""

    name: Annotated[str, "The lifetime name (without the leading quote)"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.Lifetime")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class WhereClause:
    r"""A where clause (e.g., where T: Clone, U: Debug)."""

    predicates: Annotated[frozenlist[WherePredicate], "The predicates in the where clause"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.WhereClause")
    PREDICATES = hydra.core.Name("predicates")

@dataclass(frozen=True)
class WherePredicate:
    r"""A single predicate in a where clause."""

    type: Annotated[Type, "The type being constrained"]
    bounds: Annotated[frozenlist[TypeParamBound], "The bounds on the type"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.WherePredicate")
    TYPE = hydra.core.Name("type")
    BOUNDS = hydra.core.Name("bounds")

class TypePath_(Node["TypePath"]):
    r"""A path type, possibly with generic arguments"""

class TypeReference(Node["ReferenceType"]):
    r"""A reference type"""

class TypeSlice(Node["Type"]):
    r"""A slice type"""

class TypeArray(Node["ArrayType"]):
    r"""An array type with a fixed size"""

class TypeTuple(Node["frozenlist[Type]"]):
    r"""A tuple type"""

class TypeFnPointer(Node["FnPointerType"]):
    r"""A function pointer type"""

class TypeImplTrait(Node["frozenlist[TypeParamBound]"]):
    r"""An impl Trait type"""

class TypeDynTrait(Node["frozenlist[TypeParamBound]"]):
    r"""A dyn Trait type"""

class TypeInferred:
    r"""The inferred type placeholder (_)"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeInferred)
    def __hash__(self):
        return hash("TypeInferred")

class TypeUnit:
    r"""The unit type (())"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeUnit)
    def __hash__(self):
        return hash("TypeUnit")

class TypeNever:
    r"""The never type (!)"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeNever)
    def __hash__(self):
        return hash("TypeNever")

class TypeRawPointer(Node["RawPointerType"]):
    r"""A raw pointer type"""

class TypeMacro(Node["MacroInvocation"]):
    r"""A macro invocation in type position"""

class _TypeMeta(type):
    def __getitem__(cls, item):
        return object

# A Rust type expression.
class Type(metaclass=_TypeMeta):
    r"""TypePath | TypeReference | TypeSlice | TypeArray | TypeTuple | TypeFnPointer | TypeImplTrait | TypeDynTrait | TypeInferred | TypeUnit | TypeNever | TypeRawPointer | TypeMacro"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.Type")
    PATH = hydra.core.Name("path")
    REFERENCE = hydra.core.Name("reference")
    SLICE = hydra.core.Name("slice")
    ARRAY = hydra.core.Name("array")
    TUPLE = hydra.core.Name("tuple")
    FN_POINTER = hydra.core.Name("fnPointer")
    IMPL_TRAIT = hydra.core.Name("implTrait")
    DYN_TRAIT = hydra.core.Name("dynTrait")
    INFERRED = hydra.core.Name("inferred")
    UNIT = hydra.core.Name("unit")
    NEVER = hydra.core.Name("never")
    RAW_POINTER = hydra.core.Name("rawPointer")
    MACRO = hydra.core.Name("macro")

@dataclass(frozen=True)
class TypePath:
    r"""A path-based type, possibly with generic arguments."""

    global_: Annotated[bool, "Whether the path is absolute (starts with ::)"]
    segments: Annotated[frozenlist[PathSegment], "The segments of the path"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.TypePath")
    GLOBAL = hydra.core.Name("global")
    SEGMENTS = hydra.core.Name("segments")

@dataclass(frozen=True)
class PathSegment:
    r"""A segment within a type path."""

    name: Annotated[str, "The segment name"]
    arguments: Annotated[GenericArguments, "Generic arguments, if any"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.PathSegment")
    NAME = hydra.core.Name("name")
    ARGUMENTS = hydra.core.Name("arguments")

class GenericArgumentsNone:
    r"""No generic arguments"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, GenericArgumentsNone)
    def __hash__(self):
        return hash("GenericArgumentsNone")

class GenericArgumentsAngleBracketed(Node["AngleBracketedArgs"]):
    r"""Angle-bracketed arguments"""

class GenericArgumentsParenthesized(Node["ParenthesizedArgs"]):
    r"""Parenthesized arguments for Fn traits"""

class _GenericArgumentsMeta(type):
    def __getitem__(cls, item):
        return object

# Generic arguments to a path segment.
class GenericArguments(metaclass=_GenericArgumentsMeta):
    r"""GenericArgumentsNone | GenericArgumentsAngleBracketed | GenericArgumentsParenthesized"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.GenericArguments")
    NONE = hydra.core.Name("none")
    ANGLE_BRACKETED = hydra.core.Name("angleBracketed")
    PARENTHESIZED = hydra.core.Name("parenthesized")

@dataclass(frozen=True)
class AngleBracketedArgs:
    r"""Angle-bracketed generic arguments."""

    args: Annotated[frozenlist[GenericArg], "The generic arguments"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.AngleBracketedArgs")
    ARGS = hydra.core.Name("args")

class GenericArgType(Node["Type"]):
    r"""A type argument"""

class GenericArgLifetime(Node["Lifetime"]):
    r"""A lifetime argument"""

class GenericArgConst(Node["Expression"]):
    r"""A const expression argument"""

class GenericArgBinding(Node["TypeBinding"]):
    r"""An associated type binding"""

class _GenericArgMeta(type):
    def __getitem__(cls, item):
        return object

# A single generic argument.
class GenericArg(metaclass=_GenericArgMeta):
    r"""GenericArgType | GenericArgLifetime | GenericArgConst | GenericArgBinding"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.GenericArg")
    TYPE = hydra.core.Name("type")
    LIFETIME = hydra.core.Name("lifetime")
    CONST = hydra.core.Name("const")
    BINDING = hydra.core.Name("binding")

@dataclass(frozen=True)
class TypeBinding:
    r"""An associated type binding within generic arguments."""

    name: Annotated[str, "The associated type name"]
    type: Annotated[Type, "The bound type"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.TypeBinding")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class ParenthesizedArgs:
    r"""Parenthesized generic arguments for Fn traits."""

    inputs: Annotated[frozenlist[Type], "The input types"]
    output: Annotated[Maybe[Type], "The output type"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.ParenthesizedArgs")
    INPUTS = hydra.core.Name("inputs")
    OUTPUT = hydra.core.Name("output")

@dataclass(frozen=True)
class ReferenceType:
    r"""A reference type (e.g., &T, &mut T, &'a T)."""

    lifetime: Annotated[Maybe[Lifetime], "Optional lifetime annotation"]
    mutable: Annotated[bool, "Whether the reference is mutable"]
    type: Annotated[Type, "The referenced type"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.ReferenceType")
    LIFETIME = hydra.core.Name("lifetime")
    MUTABLE = hydra.core.Name("mutable")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class ArrayType:
    r"""An array type with a fixed size (e.g., [T; 3])."""

    element: Annotated[Type, "The element type"]
    length: Annotated[Expression, "The array length (as a constant expression)"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.ArrayType")
    ELEMENT = hydra.core.Name("element")
    LENGTH = hydra.core.Name("length")

@dataclass(frozen=True)
class FnPointerType:
    r"""A function pointer type (e.g., fn(i32, i32) -> i32)."""

    params: Annotated[frozenlist[Type], "The parameter types"]
    return_type: Annotated[Type, "The return type"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.FnPointerType")
    PARAMS = hydra.core.Name("params")
    RETURN_TYPE = hydra.core.Name("returnType")

@dataclass(frozen=True)
class RawPointerType:
    r"""A raw pointer type (e.g., *const T, *mut T)."""

    mutable: Annotated[bool, "Whether the pointer is mutable (*mut vs *const)"]
    type: Annotated[Type, "The pointed-to type"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.RawPointerType")
    MUTABLE = hydra.core.Name("mutable")
    TYPE = hydra.core.Name("type")

class ExpressionLiteral(Node["Literal"]):
    r"""A literal value"""

class ExpressionPath(Node["ExprPath"]):
    r"""A path expression"""

class ExpressionBlock(Node["Block"]):
    r"""A block expression"""

class ExpressionCall(Node["CallExpr"]):
    r"""A function call expression"""

class ExpressionMethodCall(Node["MethodCallExpr"]):
    r"""A method call expression"""

class ExpressionFieldAccess(Node["FieldAccessExpr"]):
    r"""A field access expression"""

class ExpressionTupleIndex(Node["TupleIndexExpr"]):
    r"""A tuple index expression"""

class ExpressionClosure(Node["ClosureExpr"]):
    r"""A closure expression"""

class ExpressionIf(Node["IfExpr"]):
    r"""An if expression, including if let"""

class ExpressionMatch(Node["MatchExpr"]):
    r"""A match expression"""

class ExpressionLoop(Node["LoopExpr"]):
    r"""A loop expression"""

class ExpressionWhile(Node["WhileExpr"]):
    r"""A while expression, including while let"""

class ExpressionFor(Node["ForExpr"]):
    r"""A for expression"""

class ExpressionBinary(Node["BinaryExpr"]):
    r"""A binary operation"""

class ExpressionUnary(Node["UnaryExpr"]):
    r"""A unary operation"""

class ExpressionReference(Node["RefExpr"]):
    r"""A reference expression"""

class ExpressionDereference(Node["Expression"]):
    r"""A dereference expression"""

class ExpressionStruct(Node["StructExpr"]):
    r"""A struct literal expression"""

class ExpressionTuple(Node["frozenlist[Expression]"]):
    r"""A tuple expression"""

class ExpressionArray(Node["ArrayExpr"]):
    r"""An array expression"""

class ExpressionIndex(Node["IndexExpr"]):
    r"""An index expression"""

class ExpressionRange(Node["RangeExpr"]):
    r"""A range expression"""

class ExpressionReturn(Node["Maybe[Expression]"]):
    r"""A return expression"""

class ExpressionBreak(Node["Maybe[Expression]"]):
    r"""A break expression"""

class ExpressionContinue:
    r"""A continue expression"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ExpressionContinue)
    def __hash__(self):
        return hash("ExpressionContinue")

class ExpressionTry(Node["Expression"]):
    r"""A try expression (expr?)"""

class ExpressionCast(Node["CastExpr"]):
    r"""A type cast expression"""

class ExpressionTypeAscription(Node["TypeAscriptionExpr"]):
    r"""A type ascription expression"""

class ExpressionAwait(Node["Expression"]):
    r"""An await expression"""

class ExpressionAssign(Node["AssignExpr"]):
    r"""An assignment expression"""

class ExpressionCompoundAssign(Node["CompoundAssignExpr"]):
    r"""A compound assignment expression"""

class ExpressionMacro(Node["MacroInvocation"]):
    r"""A macro invocation expression"""

class ExpressionParen(Node["Expression"]):
    r"""A parenthesized expression"""

class _ExpressionMeta(type):
    def __getitem__(cls, item):
        return object

# A Rust expression.
class Expression(metaclass=_ExpressionMeta):
    r"""ExpressionLiteral | ExpressionPath | ExpressionBlock | ExpressionCall | ExpressionMethodCall | ExpressionFieldAccess | ExpressionTupleIndex | ExpressionClosure | ExpressionIf | ExpressionMatch | ExpressionLoop | ExpressionWhile | ExpressionFor | ExpressionBinary | ExpressionUnary | ExpressionReference | ExpressionDereference | ExpressionStruct | ExpressionTuple | ExpressionArray | ExpressionIndex | ExpressionRange | ExpressionReturn | ExpressionBreak | ExpressionContinue | ExpressionTry | ExpressionCast | ExpressionTypeAscription | ExpressionAwait | ExpressionAssign | ExpressionCompoundAssign | ExpressionMacro | ExpressionParen"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.Expression")
    LITERAL = hydra.core.Name("literal")
    PATH = hydra.core.Name("path")
    BLOCK = hydra.core.Name("block")
    CALL = hydra.core.Name("call")
    METHOD_CALL = hydra.core.Name("methodCall")
    FIELD_ACCESS = hydra.core.Name("fieldAccess")
    TUPLE_INDEX = hydra.core.Name("tupleIndex")
    CLOSURE = hydra.core.Name("closure")
    IF = hydra.core.Name("if")
    MATCH = hydra.core.Name("match")
    LOOP = hydra.core.Name("loop")
    WHILE = hydra.core.Name("while")
    FOR = hydra.core.Name("for")
    BINARY = hydra.core.Name("binary")
    UNARY = hydra.core.Name("unary")
    REFERENCE = hydra.core.Name("reference")
    DEREFERENCE = hydra.core.Name("dereference")
    STRUCT = hydra.core.Name("struct")
    TUPLE = hydra.core.Name("tuple")
    ARRAY = hydra.core.Name("array")
    INDEX = hydra.core.Name("index")
    RANGE = hydra.core.Name("range")
    RETURN = hydra.core.Name("return")
    BREAK = hydra.core.Name("break")
    CONTINUE = hydra.core.Name("continue")
    TRY = hydra.core.Name("try")
    CAST = hydra.core.Name("cast")
    TYPE_ASCRIPTION = hydra.core.Name("typeAscription")
    AWAIT = hydra.core.Name("await")
    ASSIGN = hydra.core.Name("assign")
    COMPOUND_ASSIGN = hydra.core.Name("compoundAssign")
    MACRO = hydra.core.Name("macro")
    PAREN = hydra.core.Name("paren")

@dataclass(frozen=True)
class ExprPath:
    r"""A path used as an expression."""

    global_: Annotated[bool, "Whether the path is global"]
    segments: Annotated[frozenlist[PathSegment], "The path segments"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.ExprPath")
    GLOBAL = hydra.core.Name("global")
    SEGMENTS = hydra.core.Name("segments")

@dataclass(frozen=True)
class CallExpr:
    r"""A function call expression."""

    function: Annotated[Expression, "The function being called"]
    args: Annotated[frozenlist[Expression], "The arguments"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.CallExpr")
    FUNCTION = hydra.core.Name("function")
    ARGS = hydra.core.Name("args")

@dataclass(frozen=True)
class MethodCallExpr:
    r"""A method call expression."""

    receiver: Annotated[Expression, "The receiver expression"]
    method: Annotated[str, "The method name"]
    turbofish: Annotated[frozenlist[Type], "Optional turbofish generic arguments"]
    args: Annotated[frozenlist[Expression], "The arguments (excluding the receiver)"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.MethodCallExpr")
    RECEIVER = hydra.core.Name("receiver")
    METHOD = hydra.core.Name("method")
    TURBOFISH = hydra.core.Name("turbofish")
    ARGS = hydra.core.Name("args")

@dataclass(frozen=True)
class FieldAccessExpr:
    r"""A field access expression."""

    object: Annotated[Expression, "The expression being accessed"]
    field: Annotated[str, "The field name"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.FieldAccessExpr")
    OBJECT = hydra.core.Name("object")
    FIELD = hydra.core.Name("field")

@dataclass(frozen=True)
class TupleIndexExpr:
    r"""A tuple index expression."""

    tuple: Annotated[Expression, "The tuple expression"]
    index: Annotated[int, "The index (0-based)"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.TupleIndexExpr")
    TUPLE = hydra.core.Name("tuple")
    INDEX = hydra.core.Name("index")

@dataclass(frozen=True)
class ClosureExpr:
    r"""A closure expression."""

    move: Annotated[bool, "Whether the closure captures by move"]
    params: Annotated[frozenlist[ClosureParam], "The closure parameters"]
    return_type: Annotated[Maybe[Type], "Optional return type annotation"]
    body: Annotated[Expression, "The closure body"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.ClosureExpr")
    MOVE = hydra.core.Name("move")
    PARAMS = hydra.core.Name("params")
    RETURN_TYPE = hydra.core.Name("returnType")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class ClosureParam:
    r"""A closure parameter."""

    pattern: Annotated[Pattern, "The parameter pattern"]
    type: Annotated[Maybe[Type], "Optional type annotation"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.ClosureParam")
    PATTERN = hydra.core.Name("pattern")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class IfExpr:
    r"""An if expression, optionally with if let."""

    condition: Annotated[IfCondition, "The condition"]
    then_block: Annotated[Block, "The then block"]
    else_branch: Annotated[Maybe[Expression], "An optional else branch"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.IfExpr")
    CONDITION = hydra.core.Name("condition")
    THEN_BLOCK = hydra.core.Name("thenBlock")
    ELSE_BRANCH = hydra.core.Name("elseBranch")

class IfConditionBool(Node["Expression"]):
    r"""A boolean condition"""

class IfConditionLet(Node["LetCondition"]):
    r"""A let condition"""

class _IfConditionMeta(type):
    def __getitem__(cls, item):
        return object

# The condition of an if expression.
class IfCondition(metaclass=_IfConditionMeta):
    r"""IfConditionBool | IfConditionLet"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.IfCondition")
    BOOL = hydra.core.Name("bool")
    LET = hydra.core.Name("let")

@dataclass(frozen=True)
class MatchExpr:
    r"""A match expression."""

    scrutinee: Annotated[Expression, "The expression being matched"]
    arms: Annotated[frozenlist[MatchArm], "The match arms"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.MatchExpr")
    SCRUTINEE = hydra.core.Name("scrutinee")
    ARMS = hydra.core.Name("arms")

@dataclass(frozen=True)
class MatchArm:
    r"""A single arm in a match expression."""

    pattern: Annotated[Pattern, "The pattern to match"]
    guard: Annotated[Maybe[Expression], "Optional guard expression"]
    body: Annotated[Expression, "The body expression"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.MatchArm")
    PATTERN = hydra.core.Name("pattern")
    GUARD = hydra.core.Name("guard")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class LoopExpr:
    r"""A loop expression."""

    label: Annotated[Maybe[str], "Optional loop label"]
    body: Annotated[Block, "The loop body"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.LoopExpr")
    LABEL = hydra.core.Name("label")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class WhileExpr:
    r"""A while expression, optionally with while let."""

    label: Annotated[Maybe[str], "Optional loop label"]
    condition: Annotated[IfCondition, "The condition"]
    body: Annotated[Block, "The loop body"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.WhileExpr")
    LABEL = hydra.core.Name("label")
    CONDITION = hydra.core.Name("condition")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class ForExpr:
    r"""A for expression."""

    label: Annotated[Maybe[str], "Optional loop label"]
    pattern: Annotated[Pattern, "The loop variable pattern"]
    iter: Annotated[Expression, "The iterator expression"]
    body: Annotated[Block, "The loop body"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.ForExpr")
    LABEL = hydra.core.Name("label")
    PATTERN = hydra.core.Name("pattern")
    ITER = hydra.core.Name("iter")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class BinaryExpr:
    r"""A binary operation."""

    left: Annotated[Expression, "The left-hand operand"]
    op: Annotated[BinaryOp, "The binary operator"]
    right: Annotated[Expression, "The right-hand operand"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.BinaryExpr")
    LEFT = hydra.core.Name("left")
    OP = hydra.core.Name("op")
    RIGHT = hydra.core.Name("right")

class BinaryOp(Enum):
    r"""A binary operator."""

    ADD = hydra.core.Name("add")

    SUB = hydra.core.Name("sub")

    MUL = hydra.core.Name("mul")

    DIV = hydra.core.Name("div")

    REM = hydra.core.Name("rem")

    AND = hydra.core.Name("and")

    OR = hydra.core.Name("or")

    BIT_AND = hydra.core.Name("bitAnd")

    BIT_OR = hydra.core.Name("bitOr")

    BIT_XOR = hydra.core.Name("bitXor")

    SHL = hydra.core.Name("shl")

    SHR = hydra.core.Name("shr")

    EQ = hydra.core.Name("eq")

    NE = hydra.core.Name("ne")

    LT = hydra.core.Name("lt")

    LE = hydra.core.Name("le")

    GT = hydra.core.Name("gt")

    GE = hydra.core.Name("ge")

BinaryOp.TYPE_ = hydra.core.Name("hydra.rust.syntax.BinaryOp")

@dataclass(frozen=True)
class UnaryExpr:
    r"""A unary operation."""

    op: Annotated[UnaryOp, "The unary operator"]
    operand: Annotated[Expression, "The operand"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.UnaryExpr")
    OP = hydra.core.Name("op")
    OPERAND = hydra.core.Name("operand")

class UnaryOp(Enum):
    r"""A unary operator."""

    NEG = hydra.core.Name("neg")

    NOT = hydra.core.Name("not")

UnaryOp.TYPE_ = hydra.core.Name("hydra.rust.syntax.UnaryOp")

@dataclass(frozen=True)
class RefExpr:
    r"""A reference expression."""

    mutable: Annotated[bool, "Whether the reference is mutable"]
    expr: Annotated[Expression, "The expression being referenced"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.RefExpr")
    MUTABLE = hydra.core.Name("mutable")
    EXPR = hydra.core.Name("expr")

@dataclass(frozen=True)
class StructExpr:
    r"""A struct literal expression."""

    path: Annotated[ExprPath, "The struct path"]
    fields: Annotated[frozenlist[FieldValue], "The field assignments"]
    rest: Annotated[Maybe[Expression], "Optional base expression for struct update syntax"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.StructExpr")
    PATH = hydra.core.Name("path")
    FIELDS = hydra.core.Name("fields")
    REST = hydra.core.Name("rest")

@dataclass(frozen=True)
class FieldValue:
    r"""A field-value pair in a struct literal."""

    name: Annotated[str, "The field name"]
    value: Annotated[Maybe[Expression], "The field value (None for shorthand syntax)"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.FieldValue")
    NAME = hydra.core.Name("name")
    VALUE = hydra.core.Name("value")

class ArrayExprElements(Node["frozenlist[Expression]"]):
    r"""An array literal"""

class ArrayExprRepeat(Node["ArrayRepeat"]):
    r"""An array repeat expression"""

class _ArrayExprMeta(type):
    def __getitem__(cls, item):
        return object

# An array expression.
class ArrayExpr(metaclass=_ArrayExprMeta):
    r"""ArrayExprElements | ArrayExprRepeat"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.ArrayExpr")
    ELEMENTS = hydra.core.Name("elements")
    REPEAT = hydra.core.Name("repeat")

@dataclass(frozen=True)
class IndexExpr:
    r"""An index expression."""

    object: Annotated[Expression, "The expression being indexed"]
    index: Annotated[Expression, "The index expression"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.IndexExpr")
    OBJECT = hydra.core.Name("object")
    INDEX = hydra.core.Name("index")

@dataclass(frozen=True)
class RangeExpr:
    r"""A range expression."""

    from_: Annotated[Maybe[Expression], "The lower bound (optional)"]
    to: Annotated[Maybe[Expression], "The upper bound (optional)"]
    inclusive: Annotated[bool, "Whether the range is inclusive"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.RangeExpr")
    FROM = hydra.core.Name("from")
    TO = hydra.core.Name("to")
    INCLUSIVE = hydra.core.Name("inclusive")

@dataclass(frozen=True)
class CastExpr:
    r"""A type cast expression."""

    expr: Annotated[Expression, "The expression being cast"]
    type: Annotated[Type, "The target type"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.CastExpr")
    EXPR = hydra.core.Name("expr")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class TypeAscriptionExpr:
    r"""A type ascription expression."""

    expr: Annotated[Expression, "The expression"]
    type: Annotated[Type, "The ascribed type"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.TypeAscriptionExpr")
    EXPR = hydra.core.Name("expr")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class AssignExpr:
    r"""An assignment expression."""

    target: Annotated[Expression, "The left-hand side (target)"]
    value: Annotated[Expression, "The right-hand side (value)"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.AssignExpr")
    TARGET = hydra.core.Name("target")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class CompoundAssignExpr:
    r"""A compound assignment expression."""

    target: Annotated[Expression, "The left-hand side (target)"]
    op: Annotated[CompoundAssignOp, "The compound assignment operator"]
    value: Annotated[Expression, "The right-hand side (value)"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.CompoundAssignExpr")
    TARGET = hydra.core.Name("target")
    OP = hydra.core.Name("op")
    VALUE = hydra.core.Name("value")

class CompoundAssignOp(Enum):
    r"""A compound assignment operator."""

    ADD_ASSIGN = hydra.core.Name("addAssign")

    SUB_ASSIGN = hydra.core.Name("subAssign")

    MUL_ASSIGN = hydra.core.Name("mulAssign")

    DIV_ASSIGN = hydra.core.Name("divAssign")

    REM_ASSIGN = hydra.core.Name("remAssign")

    BIT_AND_ASSIGN = hydra.core.Name("bitAndAssign")

    BIT_OR_ASSIGN = hydra.core.Name("bitOrAssign")

    BIT_XOR_ASSIGN = hydra.core.Name("bitXorAssign")

    SHL_ASSIGN = hydra.core.Name("shlAssign")

    SHR_ASSIGN = hydra.core.Name("shrAssign")

CompoundAssignOp.TYPE_ = hydra.core.Name("hydra.rust.syntax.CompoundAssignOp")

@dataclass(frozen=True)
class MacroInvocation:
    r"""A macro invocation."""

    path: Annotated[frozenlist[str], "The macro path"]
    delimiter: Annotated[MacroDelimiter, "The delimiter style used"]
    tokens: Annotated[str, "The token stream as a raw string"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.MacroInvocation")
    PATH = hydra.core.Name("path")
    DELIMITER = hydra.core.Name("delimiter")
    TOKENS = hydra.core.Name("tokens")

class MacroDelimiter(Enum):
    r"""The delimiter style for a macro invocation."""

    PAREN = hydra.core.Name("paren")

    BRACKET = hydra.core.Name("bracket")

    BRACE = hydra.core.Name("brace")

MacroDelimiter.TYPE_ = hydra.core.Name("hydra.rust.syntax.MacroDelimiter")

class StatementLet(Node["LetStatement"]):
    r"""A let binding"""

class StatementExpression(Node["Expression"]):
    r"""An expression statement"""

class StatementItem(Node["Item"]):
    r"""An item declaration within a block"""

class StatementEmpty:
    r"""An empty statement"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, StatementEmpty)
    def __hash__(self):
        return hash("StatementEmpty")

class _StatementMeta(type):
    def __getitem__(cls, item):
        return object

# A statement within a block.
class Statement(metaclass=_StatementMeta):
    r"""StatementLet | StatementExpression | StatementItem | StatementEmpty"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.Statement")
    LET = hydra.core.Name("let")
    EXPRESSION = hydra.core.Name("expression")
    ITEM = hydra.core.Name("item")
    EMPTY = hydra.core.Name("empty")

@dataclass(frozen=True)
class LetStatement:
    r"""A let statement."""

    pattern: Annotated[Pattern, "The binding pattern"]
    mutable: Annotated[bool, "Whether the binding is mutable"]
    type: Annotated[Maybe[Type], "Optional type annotation"]
    init: Annotated[Maybe[Expression], "Optional initializer expression"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.LetStatement")
    PATTERN = hydra.core.Name("pattern")
    MUTABLE = hydra.core.Name("mutable")
    TYPE = hydra.core.Name("type")
    INIT = hydra.core.Name("init")

@dataclass(frozen=True)
class Block:
    r"""A block expression."""

    statements: Annotated[frozenlist[Statement], "The statements in the block"]
    expression: Annotated[Maybe[Expression], "An optional trailing expression"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.Block")
    STATEMENTS = hydra.core.Name("statements")
    EXPRESSION = hydra.core.Name("expression")

class PatternWildcard:
    r"""A wildcard pattern (_)"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PatternWildcard)
    def __hash__(self):
        return hash("PatternWildcard")

class PatternIdentifier(Node["IdentifierPattern"]):
    r"""An identifier pattern"""

class PatternLiteral(Node["Literal"]):
    r"""A literal pattern"""

class PatternReference(Node["RefPattern"]):
    r"""A reference pattern"""

class PatternStruct(Node["StructPattern"]):
    r"""A struct pattern"""

class PatternTupleStruct(Node["TupleStructPattern"]):
    r"""A tuple struct pattern"""

class PatternTuple(Node["frozenlist[Pattern]"]):
    r"""A tuple pattern"""

class PatternSlice(Node["frozenlist[Pattern]"]):
    r"""A slice pattern"""

class PatternOr(Node["frozenlist[Pattern]"]):
    r"""An or-pattern"""

class PatternPath(Node["ExprPath"]):
    r"""A path pattern"""

class PatternRange(Node["RangePattern"]):
    r"""A range pattern"""

class PatternRest:
    r"""A rest pattern (..)"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PatternRest)
    def __hash__(self):
        return hash("PatternRest")

class PatternParen(Node["Pattern"]):
    r"""A parenthesized pattern"""

class _PatternMeta(type):
    def __getitem__(cls, item):
        return object

# A Rust pattern (used in let, match, function parameters, etc.).
class Pattern(metaclass=_PatternMeta):
    r"""PatternWildcard | PatternIdentifier | PatternLiteral | PatternReference | PatternStruct | PatternTupleStruct | PatternTuple | PatternSlice | PatternOr | PatternPath | PatternRange | PatternRest | PatternParen"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.Pattern")
    WILDCARD = hydra.core.Name("wildcard")
    IDENTIFIER = hydra.core.Name("identifier")
    LITERAL = hydra.core.Name("literal")
    REFERENCE = hydra.core.Name("reference")
    STRUCT = hydra.core.Name("struct")
    TUPLE_STRUCT = hydra.core.Name("tupleStruct")
    TUPLE = hydra.core.Name("tuple")
    SLICE = hydra.core.Name("slice")
    OR = hydra.core.Name("or")
    PATH = hydra.core.Name("path")
    RANGE = hydra.core.Name("range")
    REST = hydra.core.Name("rest")
    PAREN = hydra.core.Name("paren")

@dataclass(frozen=True)
class IdentifierPattern:
    r"""An identifier pattern."""

    name: Annotated[str, "The identifier name"]
    mutable: Annotated[bool, "Whether the binding is mutable"]
    at_pattern: Annotated[Maybe[Pattern], "Optional sub-pattern (e.g., x @ Some(_))"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.IdentifierPattern")
    NAME = hydra.core.Name("name")
    MUTABLE = hydra.core.Name("mutable")
    AT_PATTERN = hydra.core.Name("atPattern")

@dataclass(frozen=True)
class RefPattern:
    r"""A reference pattern."""

    mutable: Annotated[bool, "Whether the reference is mutable"]
    pattern: Annotated[Pattern, "The inner pattern"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.RefPattern")
    MUTABLE = hydra.core.Name("mutable")
    PATTERN = hydra.core.Name("pattern")

@dataclass(frozen=True)
class StructPattern:
    r"""A struct pattern."""

    path: Annotated[ExprPath, "The struct path"]
    fields: Annotated[frozenlist[FieldPattern], "The field patterns"]
    rest: Annotated[bool, "Whether the pattern has a rest (..) at the end"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.StructPattern")
    PATH = hydra.core.Name("path")
    FIELDS = hydra.core.Name("fields")
    REST = hydra.core.Name("rest")

@dataclass(frozen=True)
class FieldPattern:
    r"""A field pattern within a struct pattern."""

    name: Annotated[str, "The field name"]
    pattern: Annotated[Maybe[Pattern], "The field pattern (None for shorthand)"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.FieldPattern")
    NAME = hydra.core.Name("name")
    PATTERN = hydra.core.Name("pattern")

@dataclass(frozen=True)
class TupleStructPattern:
    r"""A tuple struct pattern."""

    path: Annotated[ExprPath, "The path to the tuple struct or variant"]
    elements: Annotated[frozenlist[Pattern], "The element patterns"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.TupleStructPattern")
    PATH = hydra.core.Name("path")
    ELEMENTS = hydra.core.Name("elements")

@dataclass(frozen=True)
class RangePattern:
    r"""A range pattern."""

    from_: Annotated[Maybe[Pattern], "The lower bound"]
    to: Annotated[Maybe[Pattern], "The upper bound"]
    inclusive: Annotated[bool, "Whether the range is inclusive"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.RangePattern")
    FROM = hydra.core.Name("from")
    TO = hydra.core.Name("to")
    INCLUSIVE = hydra.core.Name("inclusive")

class LiteralInteger(Node["IntegerLiteral"]):
    r"""An integer literal"""

class LiteralFloat(Node["FloatLiteral"]):
    r"""A floating-point literal"""

class LiteralString(Node[str]):
    r"""A string literal"""

class LiteralRawString(Node[str]):
    r"""A raw string literal"""

class LiteralByteString(Node[bytes]):
    r"""A byte string literal"""

class LiteralChar(Node[int]):
    r"""A character literal"""

class LiteralByte(Node[int]):
    r"""A byte literal"""

class LiteralBool(Node[bool]):
    r"""A boolean literal"""

class _LiteralMeta(type):
    def __getitem__(cls, item):
        return object

# A literal value.
class Literal(metaclass=_LiteralMeta):
    r"""LiteralInteger | LiteralFloat | LiteralString | LiteralRawString | LiteralByteString | LiteralChar | LiteralByte | LiteralBool"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.Literal")
    INTEGER = hydra.core.Name("integer")
    FLOAT = hydra.core.Name("float")
    STRING = hydra.core.Name("string")
    RAW_STRING = hydra.core.Name("rawString")
    BYTE_STRING = hydra.core.Name("byteString")
    CHAR = hydra.core.Name("char")
    BYTE = hydra.core.Name("byte")
    BOOL = hydra.core.Name("bool")

@dataclass(frozen=True)
class IntegerLiteral:
    r"""An integer literal with optional suffix."""

    value: Annotated[int, "The integer value"]
    suffix: Annotated[Maybe[str], "Optional type suffix"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.IntegerLiteral")
    VALUE = hydra.core.Name("value")
    SUFFIX = hydra.core.Name("suffix")

@dataclass(frozen=True)
class FloatLiteral:
    r"""A floating-point literal with optional suffix."""

    value: Annotated[float, "The float value"]
    suffix: Annotated[Maybe[str], "Optional type suffix"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.FloatLiteral")
    VALUE = hydra.core.Name("value")
    SUFFIX = hydra.core.Name("suffix")

@dataclass(frozen=True)
class Attribute:
    r"""An attribute (e.g., #[derive(Clone)], #[cfg(test)])."""

    inner: Annotated[bool, "Whether the attribute is an inner attribute (#![...] vs #[...])"]
    path: Annotated[frozenlist[str], "The attribute path"]
    tokens: Annotated[Maybe[str], "The attribute arguments as a raw token string"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.Attribute")
    INNER = hydra.core.Name("inner")
    PATH = hydra.core.Name("path")
    TOKENS = hydra.core.Name("tokens")

class VisibilityPublic:
    r"""Public (pub)"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, VisibilityPublic)
    def __hash__(self):
        return hash("VisibilityPublic")

class VisibilityCrate:
    r"""Crate-visible (pub(crate))"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, VisibilityCrate)
    def __hash__(self):
        return hash("VisibilityCrate")

class VisibilityRestricted(Node[frozenlist[str]]):
    r"""Visible to a specific path (pub(in path))"""

class VisibilityPrivate:
    r"""Private (default)"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, VisibilityPrivate)
    def __hash__(self):
        return hash("VisibilityPrivate")

class _VisibilityMeta(type):
    def __getitem__(cls, item):
        return object

# A visibility qualifier.
class Visibility(metaclass=_VisibilityMeta):
    r"""VisibilityPublic | VisibilityCrate | VisibilityRestricted | VisibilityPrivate"""

    TYPE_ = hydra.core.Name("hydra.rust.syntax.Visibility")
    PUBLIC = hydra.core.Name("public")
    CRATE = hydra.core.Name("crate")
    RESTRICTED = hydra.core.Name("restricted")
    PRIVATE = hydra.core.Name("private")

@dataclass(frozen=True)
class LetCondition:
    r"""A let condition (e.g., let Some(x) = opt)."""

    pattern: Annotated[Pattern, "The pattern"]
    expr: Annotated[Expression, "The expression being matched"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.LetCondition")
    PATTERN = hydra.core.Name("pattern")
    EXPR = hydra.core.Name("expr")

@dataclass(frozen=True)
class ArrayRepeat:
    r"""An array repeat expression (e.g., [0; 10])."""

    element: Annotated[Expression, "The element expression"]
    length: Annotated[Expression, "The length expression"]

    TYPE_ = hydra.core.Name("hydra.rust.syntax.ArrayRepeat")
    ELEMENT = hydra.core.Name("element")
    LENGTH = hydra.core.Name("length")
