//! Rust syntax model for the Hydra code generator.
//!
//! This module defines an abstract syntax tree (AST) for a subset of the Rust language,
//! based on the [Rust Reference grammar](https://doc.rust-lang.org/reference/).
//! It follows the pattern of existing Hydra syntax models for other target languages
//! (e.g., `Hydra.Ext.Haskell.Ast` for Haskell, and similar models for Java and Python).
//!
//! The types here represent the constructs needed by a Hydra-to-Rust code generator:
//! module structure, type definitions (structs, enums, type aliases), function definitions,
//! impl blocks, trait definitions, expressions, statements, patterns, and types.
//!
//! This model will eventually be translated to Hydra DSL in Haskell and placed in
//! `hydra-ext` as `Hydra.Ext.Rust.Ast`. For now, it serves as a local design document
//! and is defined directly in Rust.

// ================================================================================================
// Module-level / Crate structure
// ================================================================================================

/// A Rust crate, represented as a collection of top-level items.
/// This is the root of the syntax tree produced by the code generator.
#[derive(Clone, Debug, PartialEq)]
pub struct Crate {
    /// The top-level items in the crate.
    pub items: Vec<Item>,
}

/// A top-level item in a Rust module or crate.
#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    /// A `use` declaration (e.g., `use std::collections::BTreeMap;`).
    Use(UseDeclaration),

    /// A struct definition.
    Struct(StructDef),

    /// An enum definition.
    Enum(EnumDef),

    /// A function definition.
    Fn(FnDef),

    /// A type alias (e.g., `type Foo = Bar<Baz>;`).
    TypeAlias(TypeAlias),

    /// An `impl` block.
    Impl(ImplBlock),

    /// A trait definition.
    Trait(TraitDef),

    /// A `mod` declaration or inline module.
    Mod(ModDef),

    /// A constant item (e.g., `const FOO: u32 = 42;`).
    Const(ConstDef),

    /// A static item (e.g., `static FOO: u32 = 42;`).
    Static(StaticDef),

    /// A macro invocation used as an item (e.g., `derive_more::Display!(...)`).
    Macro(MacroInvocation),
}

/// An item together with optional doc comments and visibility.
#[derive(Clone, Debug, PartialEq)]
pub struct ItemWithComments {
    /// Optional documentation comment (rendered as `///` lines).
    pub doc: Option<String>,

    /// Whether the item is public.
    pub public: bool,

    /// The item itself.
    pub item: Item,
}

// ================================================================================================
// Use declarations
// ================================================================================================

/// A `use` declaration (e.g., `use std::collections::BTreeMap;`).
#[derive(Clone, Debug, PartialEq)]
pub struct UseDeclaration {
    /// Whether the use is public (`pub use ...`).
    pub public: bool,

    /// The use tree describing what is imported.
    pub tree: UseTree,
}

/// A use tree, representing the structure of a `use` path.
#[derive(Clone, Debug, PartialEq)]
pub enum UseTree {
    /// A simple path import (e.g., `std::collections::BTreeMap`).
    Path(UsePath),

    /// A renamed import (e.g., `std::collections::BTreeMap as Map`).
    Rename(UseRename),

    /// A glob import (e.g., `std::collections::*`).
    Glob(Vec<String>),

    /// A grouped import (e.g., `std::collections::{BTreeMap, BTreeSet}`).
    Group(UseGroup),
}

/// A simple path import within a use tree.
#[derive(Clone, Debug, PartialEq)]
pub struct UsePath {
    /// The path segments (e.g., `["std", "collections", "BTreeMap"]`).
    pub segments: Vec<String>,
}

/// A renamed import (e.g., `std::collections::BTreeMap as Map`).
#[derive(Clone, Debug, PartialEq)]
pub struct UseRename {
    /// The original path segments.
    pub path: Vec<String>,

    /// The alias name.
    pub alias: String,
}

/// A grouped import (e.g., `std::collections::{BTreeMap, BTreeSet}`).
#[derive(Clone, Debug, PartialEq)]
pub struct UseGroup {
    /// The common prefix path segments.
    pub prefix: Vec<String>,

    /// The individual subtrees within the group.
    pub trees: Vec<UseTree>,
}

// ================================================================================================
// Struct definition
// ================================================================================================

/// A struct definition (e.g., `struct Foo<T> { bar: T }`).
#[derive(Clone, Debug, PartialEq)]
pub struct StructDef {
    /// The struct name.
    pub name: String,

    /// Generic type parameters.
    pub generics: Vec<GenericParam>,

    /// Optional where clause.
    pub where_clause: Option<WhereClause>,

    /// The struct body (named fields, tuple fields, or unit).
    pub body: StructBody,

    /// Derive macros to apply (e.g., `["Clone", "Debug", "PartialEq"]`).
    pub derives: Vec<String>,

    /// Whether the struct is public.
    pub public: bool,

    /// Optional doc comment.
    pub doc: Option<String>,
}

/// The body of a struct definition.
#[derive(Clone, Debug, PartialEq)]
pub enum StructBody {
    /// A struct with named fields (e.g., `struct Foo { x: i32, y: i32 }`).
    Named(Vec<StructField>),

    /// A tuple struct (e.g., `struct Foo(i32, i32);`).
    Tuple(Vec<TupleField>),

    /// A unit struct (e.g., `struct Foo;`).
    Unit,
}

/// A named field within a struct definition.
#[derive(Clone, Debug, PartialEq)]
pub struct StructField {
    /// The field name.
    pub name: String,

    /// The field type.
    pub type_: Type,

    /// Whether the field is public.
    pub public: bool,

    /// Optional doc comment for the field.
    pub doc: Option<String>,
}

/// A positional field within a tuple struct.
#[derive(Clone, Debug, PartialEq)]
pub struct TupleField {
    /// The field type.
    pub type_: Type,

    /// Whether the field is public.
    pub public: bool,
}

// ================================================================================================
// Enum definition
// ================================================================================================

/// An enum definition (e.g., `enum Foo<T> { Bar(T), Baz { x: i32 } }`).
#[derive(Clone, Debug, PartialEq)]
pub struct EnumDef {
    /// The enum name.
    pub name: String,

    /// Generic type parameters.
    pub generics: Vec<GenericParam>,

    /// Optional where clause.
    pub where_clause: Option<WhereClause>,

    /// The enum variants.
    pub variants: Vec<EnumVariant>,

    /// Derive macros to apply.
    pub derives: Vec<String>,

    /// Whether the enum is public.
    pub public: bool,

    /// Optional doc comment.
    pub doc: Option<String>,
}

/// A variant of an enum definition.
#[derive(Clone, Debug, PartialEq)]
pub struct EnumVariant {
    /// The variant name.
    pub name: String,

    /// The variant body.
    pub body: EnumVariantBody,

    /// Optional doc comment for the variant.
    pub doc: Option<String>,
}

/// The body of an enum variant.
#[derive(Clone, Debug, PartialEq)]
pub enum EnumVariantBody {
    /// A unit variant (e.g., `Foo`).
    Unit,

    /// A tuple variant (e.g., `Foo(i32, String)`).
    Tuple(Vec<Type>),

    /// A struct variant (e.g., `Foo { x: i32, y: String }`).
    Struct(Vec<StructField>),
}

// ================================================================================================
// Function definition
// ================================================================================================

/// A function definition (e.g., `fn foo<T>(x: T) -> String { ... }`).
#[derive(Clone, Debug, PartialEq)]
pub struct FnDef {
    /// The function name.
    pub name: String,

    /// Generic type parameters.
    pub generics: Vec<GenericParam>,

    /// Optional where clause.
    pub where_clause: Option<WhereClause>,

    /// The function parameters.
    pub params: Vec<FnParam>,

    /// The return type. `None` means the function returns `()`.
    pub return_type: Option<Type>,

    /// The function body.
    pub body: Block,

    /// Whether the function is public.
    pub public: bool,

    /// Whether the function is async.
    pub async_: bool,

    /// Whether the function is const.
    pub const_: bool,

    /// Whether the function is unsafe.
    pub unsafe_: bool,

    /// Optional doc comment.
    pub doc: Option<String>,
}

/// A function parameter.
#[derive(Clone, Debug, PartialEq)]
pub struct FnParam {
    /// The parameter pattern (usually a simple name, but can be destructuring).
    pub pattern: Pattern,

    /// The parameter type.
    pub type_: Type,
}

/// A self parameter in a method (e.g., `&self`, `&mut self`, `self`).
#[derive(Clone, Debug, PartialEq)]
pub enum SelfParam {
    /// An owned self (`self`).
    Owned,

    /// A borrowed self (`&self`).
    Ref,

    /// A mutably borrowed self (`&mut self`).
    RefMut,
}

/// A method parameter, which may be `self` or a regular parameter.
#[derive(Clone, Debug, PartialEq)]
pub enum MethodParam {
    /// A self parameter.
    SelfParam(SelfParam),

    /// A regular function parameter.
    Regular(FnParam),
}

// ================================================================================================
// Type alias
// ================================================================================================

/// A type alias definition (e.g., `type Foo<T> = Bar<T>;`).
#[derive(Clone, Debug, PartialEq)]
pub struct TypeAlias {
    /// The alias name.
    pub name: String,

    /// Generic type parameters.
    pub generics: Vec<GenericParam>,

    /// The aliased type.
    pub type_: Type,

    /// Whether the alias is public.
    pub public: bool,

    /// Optional doc comment.
    pub doc: Option<String>,
}

// ================================================================================================
// Const and static definitions
// ================================================================================================

/// A constant item (e.g., `const FOO: u32 = 42;`).
#[derive(Clone, Debug, PartialEq)]
pub struct ConstDef {
    /// The constant name.
    pub name: String,

    /// The constant type.
    pub type_: Type,

    /// The constant value expression.
    pub value: Expression,

    /// Whether the constant is public.
    pub public: bool,

    /// Optional doc comment.
    pub doc: Option<String>,
}

/// A static item (e.g., `static FOO: u32 = 42;`).
#[derive(Clone, Debug, PartialEq)]
pub struct StaticDef {
    /// The static name.
    pub name: String,

    /// The static type.
    pub type_: Type,

    /// The static value expression.
    pub value: Expression,

    /// Whether the static is mutable.
    pub mutable: bool,

    /// Whether the static is public.
    pub public: bool,

    /// Optional doc comment.
    pub doc: Option<String>,
}

// ================================================================================================
// Module definition
// ================================================================================================

/// A module definition (either inline or external).
#[derive(Clone, Debug, PartialEq)]
pub struct ModDef {
    /// The module name.
    pub name: String,

    /// The module body. `None` for `mod foo;` (external file), `Some` for inline modules.
    pub body: Option<Vec<Item>>,

    /// Whether the module is public.
    pub public: bool,

    /// Optional doc comment.
    pub doc: Option<String>,
}

// ================================================================================================
// Impl block
// ================================================================================================

/// An `impl` block (e.g., `impl<T> Trait for Foo<T> { ... }`).
#[derive(Clone, Debug, PartialEq)]
pub struct ImplBlock {
    /// Generic type parameters.
    pub generics: Vec<GenericParam>,

    /// Optional where clause.
    pub where_clause: Option<WhereClause>,

    /// The trait being implemented, if this is a trait impl.
    pub trait_: Option<TypePath>,

    /// Whether this is a negative impl (e.g., `impl !Send for Foo {}`).
    pub negative: bool,

    /// The type being implemented for.
    pub self_type: Type,

    /// The items within the impl block.
    pub items: Vec<ImplItem>,
}

/// An item within an `impl` block.
#[derive(Clone, Debug, PartialEq)]
pub enum ImplItem {
    /// A method definition.
    Method(ImplMethod),

    /// An associated type definition.
    Type(TypeAlias),

    /// An associated constant.
    Const(ConstDef),
}

/// A method within an `impl` block.
#[derive(Clone, Debug, PartialEq)]
pub struct ImplMethod {
    /// The method name.
    pub name: String,

    /// Generic type parameters.
    pub generics: Vec<GenericParam>,

    /// Optional where clause.
    pub where_clause: Option<WhereClause>,

    /// The method parameters (including `self`).
    pub params: Vec<MethodParam>,

    /// The return type. `None` means the method returns `()`.
    pub return_type: Option<Type>,

    /// The method body.
    pub body: Block,

    /// Whether the method is public.
    pub public: bool,

    /// Whether the method has a default implementation (for trait impls).
    pub default: bool,

    /// Optional doc comment.
    pub doc: Option<String>,
}

// ================================================================================================
// Trait definition
// ================================================================================================

/// A trait definition (e.g., `trait Foo<T>: Bar + Baz { ... }`).
#[derive(Clone, Debug, PartialEq)]
pub struct TraitDef {
    /// The trait name.
    pub name: String,

    /// Generic type parameters.
    pub generics: Vec<GenericParam>,

    /// Optional where clause.
    pub where_clause: Option<WhereClause>,

    /// Supertraits (e.g., `Bar + Baz` in `trait Foo: Bar + Baz`).
    pub super_traits: Vec<TypeParamBound>,

    /// The items within the trait.
    pub items: Vec<TraitItem>,

    /// Whether the trait is public.
    pub public: bool,

    /// Whether the trait is unsafe.
    pub unsafe_: bool,

    /// Optional doc comment.
    pub doc: Option<String>,
}

/// An item within a trait definition.
#[derive(Clone, Debug, PartialEq)]
pub enum TraitItem {
    /// A method signature or default method.
    Method(TraitMethod),

    /// An associated type.
    Type(TraitType),

    /// An associated constant.
    Const(TraitConst),
}

/// A method signature or default method within a trait.
#[derive(Clone, Debug, PartialEq)]
pub struct TraitMethod {
    /// The method name.
    pub name: String,

    /// Generic type parameters.
    pub generics: Vec<GenericParam>,

    /// Optional where clause.
    pub where_clause: Option<WhereClause>,

    /// The method parameters (including `self`).
    pub params: Vec<MethodParam>,

    /// The return type.
    pub return_type: Option<Type>,

    /// Optional default body.
    pub default_body: Option<Block>,

    /// Optional doc comment.
    pub doc: Option<String>,
}

/// An associated type within a trait.
#[derive(Clone, Debug, PartialEq)]
pub struct TraitType {
    /// The associated type name.
    pub name: String,

    /// Type parameter bounds (e.g., `type Item: Clone;`).
    pub bounds: Vec<TypeParamBound>,

    /// Optional default type.
    pub default: Option<Type>,

    /// Optional doc comment.
    pub doc: Option<String>,
}

/// An associated constant within a trait.
#[derive(Clone, Debug, PartialEq)]
pub struct TraitConst {
    /// The constant name.
    pub name: String,

    /// The constant type.
    pub type_: Type,

    /// Optional default value.
    pub default: Option<Expression>,

    /// Optional doc comment.
    pub doc: Option<String>,
}

// ================================================================================================
// Generics and where clauses
// ================================================================================================

/// A generic type parameter (e.g., `T: Clone + Debug`).
#[derive(Clone, Debug, PartialEq)]
pub struct GenericParam {
    /// The parameter name.
    pub name: String,

    /// Trait bounds on the parameter.
    pub bounds: Vec<TypeParamBound>,
}

/// A bound on a type parameter.
#[derive(Clone, Debug, PartialEq)]
pub enum TypeParamBound {
    /// A trait bound (e.g., `Clone`, `Iterator<Item = T>`).
    Trait(TypePath),

    /// A lifetime bound (e.g., `'a`).
    Lifetime(Lifetime),
}

/// A lifetime (e.g., `'a`, `'static`).
#[derive(Clone, Debug, PartialEq)]
pub struct Lifetime {
    /// The lifetime name (without the leading `'`).
    pub name: String,
}

/// A where clause (e.g., `where T: Clone, U: Debug`).
#[derive(Clone, Debug, PartialEq)]
pub struct WhereClause {
    /// The predicates in the where clause.
    pub predicates: Vec<WherePredicate>,
}

/// A single predicate in a where clause (e.g., `T: Clone + Debug`).
#[derive(Clone, Debug, PartialEq)]
pub struct WherePredicate {
    /// The type being constrained.
    pub type_: Type,

    /// The bounds on the type.
    pub bounds: Vec<TypeParamBound>,
}

// ================================================================================================
// Types
// ================================================================================================

/// A Rust type expression.
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    /// A path type, possibly with generic arguments (e.g., `std::collections::BTreeMap<K, V>`).
    Path(TypePath),

    /// A reference type (e.g., `&T`, `&mut T`, `&'a T`).
    Reference(ReferenceType),

    /// A slice type (e.g., `[T]`).
    Slice(Box<Type>),

    /// An array type with a fixed size (e.g., `[T; N]`).
    Array(ArrayType),

    /// A tuple type (e.g., `(A, B, C)`).
    Tuple(Vec<Type>),

    /// A function pointer type (e.g., `fn(A, B) -> C`).
    FnPointer(FnPointerType),

    /// An `impl Trait` type (e.g., `impl Iterator<Item = T>`).
    ImplTrait(Vec<TypeParamBound>),

    /// A `dyn Trait` type (e.g., `dyn Iterator<Item = T>`).
    DynTrait(Vec<TypeParamBound>),

    /// The inferred type placeholder (`_`).
    Inferred,

    /// The unit type (`()`).
    Unit,

    /// The never type (`!`).
    Never,

    /// A raw pointer type (e.g., `*const T`, `*mut T`).
    RawPointer(RawPointerType),

    /// A macro invocation in type position (e.g., `vec![]` is rare, but macros can appear).
    Macro(MacroInvocation),
}

/// A path-based type, possibly with generic arguments (e.g., `Vec<T>`, `std::io::Result<T>`).
#[derive(Clone, Debug, PartialEq)]
pub struct TypePath {
    /// Whether the path is absolute (starts with `::`).
    pub global: bool,

    /// The segments of the path.
    pub segments: Vec<PathSegment>,
}

/// A segment within a type path (e.g., `Vec<T>` has one segment with name "Vec" and argument `T`).
#[derive(Clone, Debug, PartialEq)]
pub struct PathSegment {
    /// The segment name.
    pub name: String,

    /// Generic arguments, if any.
    pub arguments: GenericArguments,
}

/// Generic arguments to a path segment.
#[derive(Clone, Debug, PartialEq)]
pub enum GenericArguments {
    /// No generic arguments.
    None,

    /// Angle-bracketed arguments (e.g., `<T, U>`, `<Item = T>`).
    AngleBracketed(AngleBracketedArgs),

    /// Parenthesized arguments for `Fn` traits (e.g., `Fn(A, B) -> C`).
    Parenthesized(ParenthesizedArgs),
}

/// Angle-bracketed generic arguments (e.g., `<T, U>`, `<T, Item = U>`).
#[derive(Clone, Debug, PartialEq)]
pub struct AngleBracketedArgs {
    /// The generic arguments.
    pub args: Vec<GenericArg>,
}

/// A single generic argument.
#[derive(Clone, Debug, PartialEq)]
pub enum GenericArg {
    /// A type argument.
    Type(Type),

    /// A lifetime argument.
    Lifetime(Lifetime),

    /// A const expression argument.
    Const(Expression),

    /// An associated type binding (e.g., `Item = T`).
    Binding(TypeBinding),
}

/// An associated type binding within generic arguments (e.g., `Item = T`).
#[derive(Clone, Debug, PartialEq)]
pub struct TypeBinding {
    /// The associated type name.
    pub name: String,

    /// The bound type.
    pub type_: Type,
}

/// Parenthesized generic arguments for `Fn` traits (e.g., `Fn(A, B) -> C`).
#[derive(Clone, Debug, PartialEq)]
pub struct ParenthesizedArgs {
    /// The input types.
    pub inputs: Vec<Type>,

    /// The output type.
    pub output: Option<Type>,
}

/// A reference type (e.g., `&T`, `&mut T`, `&'a T`).
#[derive(Clone, Debug, PartialEq)]
pub struct ReferenceType {
    /// Optional lifetime annotation.
    pub lifetime: Option<Lifetime>,

    /// Whether the reference is mutable.
    pub mutable: bool,

    /// The referenced type.
    pub type_: Box<Type>,
}

/// An array type with a fixed size (e.g., `[T; 3]`).
#[derive(Clone, Debug, PartialEq)]
pub struct ArrayType {
    /// The element type.
    pub element: Box<Type>,

    /// The array length (as a constant expression).
    pub length: Expression,
}

/// A function pointer type (e.g., `fn(i32, i32) -> i32`).
#[derive(Clone, Debug, PartialEq)]
pub struct FnPointerType {
    /// The parameter types.
    pub params: Vec<Type>,

    /// The return type.
    pub return_type: Box<Type>,
}

/// A raw pointer type (e.g., `*const T`, `*mut T`).
#[derive(Clone, Debug, PartialEq)]
pub struct RawPointerType {
    /// Whether the pointer is mutable (`*mut T` vs `*const T`).
    pub mutable: bool,

    /// The pointed-to type.
    pub type_: Box<Type>,
}

// ================================================================================================
// Expressions
// ================================================================================================

/// A Rust expression.
#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    /// A literal value (e.g., `42`, `"hello"`, `true`).
    Literal(Literal),

    /// A path expression (e.g., `foo`, `std::io::Error`, `Foo::Bar`).
    Path(ExprPath),

    /// A block expression (e.g., `{ let x = 1; x + 1 }`).
    Block(Block),

    /// A function call expression (e.g., `foo(1, 2)`).
    Call(CallExpr),

    /// A method call expression (e.g., `foo.bar(1, 2)`).
    MethodCall(MethodCallExpr),

    /// A field access expression (e.g., `foo.bar`).
    FieldAccess(FieldAccessExpr),

    /// A tuple index expression (e.g., `tuple.0`).
    TupleIndex(TupleIndexExpr),

    /// A closure expression (e.g., `|x| x + 1`, `move |x, y| x + y`).
    Closure(ClosureExpr),

    /// An `if` expression, including `if let`.
    If(IfExpr),

    /// A `match` expression.
    Match(MatchExpr),

    /// A `loop` expression.
    Loop(LoopExpr),

    /// A `while` expression, including `while let`.
    While(WhileExpr),

    /// A `for` expression.
    For(ForExpr),

    /// A binary operation (e.g., `a + b`, `a && b`).
    Binary(BinaryExpr),

    /// A unary operation (e.g., `-x`, `!x`).
    Unary(UnaryExpr),

    /// A reference expression (e.g., `&x`, `&mut x`).
    Reference(RefExpr),

    /// A dereference expression (e.g., `*x`).
    Dereference(Box<Expression>),

    /// A struct literal expression (e.g., `Foo { bar: 1, baz: 2 }`).
    Struct(StructExpr),

    /// A tuple expression (e.g., `(1, "hello", true)`).
    Tuple(Vec<Expression>),

    /// An array expression (e.g., `[1, 2, 3]`).
    Array(ArrayExpr),

    /// An index expression (e.g., `arr[0]`).
    Index(IndexExpr),

    /// A range expression (e.g., `0..10`, `..`, `0..=10`).
    Range(RangeExpr),

    /// A `return` expression.
    Return(Option<Box<Expression>>),

    /// A `break` expression.
    Break(Option<Box<Expression>>),

    /// A `continue` expression.
    Continue,

    /// A try expression (e.g., `expr?`).
    Try(Box<Expression>),

    /// A type cast expression (e.g., `expr as Type`).
    Cast(CastExpr),

    /// A type ascription expression (e.g., `expr: Type`).
    TypeAscription(TypeAscriptionExpr),

    /// An `await` expression (e.g., `future.await`).
    Await(Box<Expression>),

    /// An assignment expression (e.g., `x = 1`).
    Assign(AssignExpr),

    /// A compound assignment expression (e.g., `x += 1`).
    CompoundAssign(CompoundAssignExpr),

    /// A macro invocation expression (e.g., `vec![1, 2, 3]`, `println!("hello")`).
    Macro(MacroInvocation),

    /// A parenthesized expression.
    Paren(Box<Expression>),
}

/// A path used as an expression (e.g., `foo`, `Foo::Bar`, `::std::io::Error`).
#[derive(Clone, Debug, PartialEq)]
pub struct ExprPath {
    /// Whether the path is global (starts with `::`).
    pub global: bool,

    /// The path segments.
    pub segments: Vec<PathSegment>,
}

/// A function call expression (e.g., `foo(1, 2)` or `Foo::bar(1, 2)`).
#[derive(Clone, Debug, PartialEq)]
pub struct CallExpr {
    /// The function being called.
    pub function: Box<Expression>,

    /// The arguments.
    pub args: Vec<Expression>,
}

/// A method call expression (e.g., `receiver.method(arg1, arg2)`).
#[derive(Clone, Debug, PartialEq)]
pub struct MethodCallExpr {
    /// The receiver expression.
    pub receiver: Box<Expression>,

    /// The method name.
    pub method: String,

    /// Optional turbofish generic arguments (e.g., `::<T>`).
    pub turbofish: Vec<Type>,

    /// The arguments (excluding the receiver).
    pub args: Vec<Expression>,
}

/// A field access expression (e.g., `expr.field`).
#[derive(Clone, Debug, PartialEq)]
pub struct FieldAccessExpr {
    /// The expression being accessed.
    pub object: Box<Expression>,

    /// The field name.
    pub field: String,
}

/// A tuple index expression (e.g., `tuple.0`).
#[derive(Clone, Debug, PartialEq)]
pub struct TupleIndexExpr {
    /// The tuple expression.
    pub tuple: Box<Expression>,

    /// The index (0-based).
    pub index: usize,
}

/// A closure expression (e.g., `|x, y| x + y` or `move |x| x`).
#[derive(Clone, Debug, PartialEq)]
pub struct ClosureExpr {
    /// Whether the closure captures by move.
    pub move_: bool,

    /// The closure parameters.
    pub params: Vec<ClosureParam>,

    /// Optional return type annotation.
    pub return_type: Option<Type>,

    /// The closure body.
    pub body: Box<Expression>,
}

/// A closure parameter.
#[derive(Clone, Debug, PartialEq)]
pub struct ClosureParam {
    /// The parameter pattern.
    pub pattern: Pattern,

    /// Optional type annotation.
    pub type_: Option<Type>,
}

/// An `if` expression, optionally with `if let` (e.g., `if cond { ... } else { ... }`).
#[derive(Clone, Debug, PartialEq)]
pub struct IfExpr {
    /// The condition. For `if let`, use `IfCondition::Let`.
    pub condition: IfCondition,

    /// The `then` block.
    pub then_block: Block,

    /// An optional `else` branch (either another `if` or a block).
    pub else_branch: Option<Box<Expression>>,
}

/// The condition of an `if` expression.
#[derive(Clone, Debug, PartialEq)]
pub enum IfCondition {
    /// A boolean condition (e.g., `if x > 0`).
    Bool(Box<Expression>),

    /// A `let` condition (e.g., `if let Some(x) = opt`).
    Let(Pattern, Box<Expression>),
}

/// A `match` expression.
#[derive(Clone, Debug, PartialEq)]
pub struct MatchExpr {
    /// The expression being matched.
    pub scrutinee: Box<Expression>,

    /// The match arms.
    pub arms: Vec<MatchArm>,
}

/// A single arm in a `match` expression.
#[derive(Clone, Debug, PartialEq)]
pub struct MatchArm {
    /// The pattern to match.
    pub pattern: Pattern,

    /// Optional guard expression (e.g., `if x > 0`).
    pub guard: Option<Expression>,

    /// The body expression.
    pub body: Expression,
}

/// A `loop` expression (e.g., `loop { ... }`).
#[derive(Clone, Debug, PartialEq)]
pub struct LoopExpr {
    /// Optional loop label (e.g., `'outer`).
    pub label: Option<String>,

    /// The loop body.
    pub body: Block,
}

/// A `while` expression, optionally with `while let`.
#[derive(Clone, Debug, PartialEq)]
pub struct WhileExpr {
    /// Optional loop label.
    pub label: Option<String>,

    /// The condition (can be `while let`).
    pub condition: IfCondition,

    /// The loop body.
    pub body: Block,
}

/// A `for` expression (e.g., `for x in iter { ... }`).
#[derive(Clone, Debug, PartialEq)]
pub struct ForExpr {
    /// Optional loop label.
    pub label: Option<String>,

    /// The loop variable pattern.
    pub pattern: Pattern,

    /// The iterator expression.
    pub iter: Box<Expression>,

    /// The loop body.
    pub body: Block,
}

/// A binary operation (e.g., `a + b`, `a == b`).
#[derive(Clone, Debug, PartialEq)]
pub struct BinaryExpr {
    /// The left-hand operand.
    pub left: Box<Expression>,

    /// The binary operator.
    pub op: BinaryOp,

    /// The right-hand operand.
    pub right: Box<Expression>,
}

/// A binary operator.
#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    /// Addition (`+`).
    Add,
    /// Subtraction (`-`).
    Sub,
    /// Multiplication (`*`).
    Mul,
    /// Division (`/`).
    Div,
    /// Remainder (`%`).
    Rem,
    /// Logical AND (`&&`).
    And,
    /// Logical OR (`||`).
    Or,
    /// Bitwise AND (`&`).
    BitAnd,
    /// Bitwise OR (`|`).
    BitOr,
    /// Bitwise XOR (`^`).
    BitXor,
    /// Left shift (`<<`).
    Shl,
    /// Right shift (`>>`).
    Shr,
    /// Equality (`==`).
    Eq,
    /// Inequality (`!=`).
    Ne,
    /// Less than (`<`).
    Lt,
    /// Less than or equal (`<=`).
    Le,
    /// Greater than (`>`).
    Gt,
    /// Greater than or equal (`>=`).
    Ge,
}

/// A unary operation (e.g., `-x`, `!x`).
#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExpr {
    /// The unary operator.
    pub op: UnaryOp,

    /// The operand.
    pub operand: Box<Expression>,
}

/// A unary operator.
#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    /// Negation (`-`).
    Neg,
    /// Logical NOT (`!`).
    Not,
}

/// A reference expression (e.g., `&x`, `&mut x`).
#[derive(Clone, Debug, PartialEq)]
pub struct RefExpr {
    /// Whether the reference is mutable.
    pub mutable: bool,

    /// The expression being referenced.
    pub expr: Box<Expression>,
}

/// A struct literal expression (e.g., `Foo { bar: 1, baz: 2 }`).
#[derive(Clone, Debug, PartialEq)]
pub struct StructExpr {
    /// The struct path (e.g., `Foo`, `Foo::Bar`).
    pub path: ExprPath,

    /// The field assignments.
    pub fields: Vec<FieldValue>,

    /// Optional base expression for struct update syntax (e.g., `..default`).
    pub rest: Option<Box<Expression>>,
}

/// A field-value pair in a struct literal (e.g., `bar: 1`).
#[derive(Clone, Debug, PartialEq)]
pub struct FieldValue {
    /// The field name.
    pub name: String,

    /// The field value. `None` for shorthand syntax (e.g., `bar` instead of `bar: bar`).
    pub value: Option<Expression>,
}

/// An array expression.
#[derive(Clone, Debug, PartialEq)]
pub enum ArrayExpr {
    /// An array literal (e.g., `[1, 2, 3]`).
    Elements(Vec<Expression>),

    /// An array repeat expression (e.g., `[0; 10]`).
    Repeat(Box<Expression>, Box<Expression>),
}

/// An index expression (e.g., `arr[idx]`).
#[derive(Clone, Debug, PartialEq)]
pub struct IndexExpr {
    /// The expression being indexed.
    pub object: Box<Expression>,

    /// The index expression.
    pub index: Box<Expression>,
}

/// A range expression (e.g., `0..10`, `..`, `0..=10`).
#[derive(Clone, Debug, PartialEq)]
pub struct RangeExpr {
    /// The lower bound (optional).
    pub from: Option<Box<Expression>>,

    /// The upper bound (optional).
    pub to: Option<Box<Expression>>,

    /// Whether the range is inclusive (`..=` vs `..`).
    pub inclusive: bool,
}

/// A type cast expression (e.g., `expr as Type`).
#[derive(Clone, Debug, PartialEq)]
pub struct CastExpr {
    /// The expression being cast.
    pub expr: Box<Expression>,

    /// The target type.
    pub type_: Type,
}

/// A type ascription expression (e.g., `expr: Type`).
#[derive(Clone, Debug, PartialEq)]
pub struct TypeAscriptionExpr {
    /// The expression.
    pub expr: Box<Expression>,

    /// The ascribed type.
    pub type_: Type,
}

/// An assignment expression (e.g., `x = 1`).
#[derive(Clone, Debug, PartialEq)]
pub struct AssignExpr {
    /// The left-hand side (target).
    pub target: Box<Expression>,

    /// The right-hand side (value).
    pub value: Box<Expression>,
}

/// A compound assignment expression (e.g., `x += 1`).
#[derive(Clone, Debug, PartialEq)]
pub struct CompoundAssignExpr {
    /// The left-hand side (target).
    pub target: Box<Expression>,

    /// The compound assignment operator.
    pub op: CompoundAssignOp,

    /// The right-hand side (value).
    pub value: Box<Expression>,
}

/// A compound assignment operator.
#[derive(Clone, Debug, PartialEq)]
pub enum CompoundAssignOp {
    /// `+=`
    AddAssign,
    /// `-=`
    SubAssign,
    /// `*=`
    MulAssign,
    /// `/=`
    DivAssign,
    /// `%=`
    RemAssign,
    /// `&=`
    BitAndAssign,
    /// `|=`
    BitOrAssign,
    /// `^=`
    BitXorAssign,
    /// `<<=`
    ShlAssign,
    /// `>>=`
    ShrAssign,
}

/// A macro invocation (e.g., `vec![1, 2, 3]`, `println!("hello")`).
#[derive(Clone, Debug, PartialEq)]
pub struct MacroInvocation {
    /// The macro path (e.g., `["vec"]`, `["std", "println"]`).
    pub path: Vec<String>,

    /// The delimiter style used.
    pub delimiter: MacroDelimiter,

    /// The token stream as a raw string (unparsed macro arguments).
    pub tokens: String,
}

/// The delimiter style for a macro invocation.
#[derive(Clone, Debug, PartialEq)]
pub enum MacroDelimiter {
    /// Parentheses: `macro!(...)`.
    Paren,
    /// Square brackets: `macro![...]`.
    Bracket,
    /// Curly braces: `macro!{...}`.
    Brace,
}

// ================================================================================================
// Statements
// ================================================================================================

/// A statement within a block.
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    /// A `let` binding (e.g., `let x: i32 = 42;`).
    Let(LetStatement),

    /// An expression statement (expression followed by `;`).
    Expression(Expression),

    /// An item declaration within a block.
    Item(Item),

    /// An empty statement (just `;`).
    Empty,
}

/// A `let` statement (e.g., `let mut x: i32 = 42;`).
#[derive(Clone, Debug, PartialEq)]
pub struct LetStatement {
    /// The binding pattern.
    pub pattern: Pattern,

    /// Whether the binding is mutable.
    pub mutable: bool,

    /// Optional type annotation.
    pub type_: Option<Type>,

    /// Optional initializer expression.
    pub init: Option<Expression>,
}

// ================================================================================================
// Block
// ================================================================================================

/// A block expression (e.g., `{ stmt1; stmt2; expr }`).
///
/// In Rust, a block is a sequence of statements optionally followed by a trailing
/// expression (without semicolon) that becomes the block's value.
#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    /// The statements in the block.
    pub statements: Vec<Statement>,

    /// An optional trailing expression that is the value of the block.
    pub expression: Option<Box<Expression>>,
}

// ================================================================================================
// Patterns
// ================================================================================================

/// A Rust pattern (used in `let`, `match`, function parameters, etc.).
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    /// A wildcard pattern (`_`).
    Wildcard,

    /// An identifier pattern, optionally mutable (e.g., `x`, `mut x`).
    Identifier(IdentifierPattern),

    /// A literal pattern (e.g., `42`, `"hello"`).
    Literal(Literal),

    /// A reference pattern (e.g., `&x`, `&mut x`).
    Reference(RefPattern),

    /// A struct pattern (e.g., `Foo { bar, baz: 42, .. }`).
    Struct(StructPattern),

    /// A tuple struct pattern (e.g., `Some(x)`, `Foo(1, 2)`).
    TupleStruct(TupleStructPattern),

    /// A tuple pattern (e.g., `(a, b, c)`).
    Tuple(Vec<Pattern>),

    /// A slice pattern (e.g., `[first, .., last]`).
    Slice(Vec<Pattern>),

    /// An or-pattern (e.g., `A | B | C`).
    Or(Vec<Pattern>),

    /// A path pattern (e.g., `None`, `Foo::Bar`).
    Path(ExprPath),

    /// A range pattern (e.g., `0..=9`).
    Range(RangePattern),

    /// A rest pattern (`..`) used inside structs and tuples.
    Rest,

    /// A grouped/parenthesized pattern.
    Paren(Box<Pattern>),
}

/// An identifier pattern, optionally with mutability and a sub-pattern.
#[derive(Clone, Debug, PartialEq)]
pub struct IdentifierPattern {
    /// The identifier name.
    pub name: String,

    /// Whether the binding is mutable.
    pub mutable: bool,

    /// Optional sub-pattern (e.g., `x @ Some(_)`).
    pub at_pattern: Option<Box<Pattern>>,
}

/// A reference pattern (e.g., `&x`, `&mut x`).
#[derive(Clone, Debug, PartialEq)]
pub struct RefPattern {
    /// Whether the reference is mutable.
    pub mutable: bool,

    /// The inner pattern.
    pub pattern: Box<Pattern>,
}

/// A struct pattern (e.g., `Foo { bar, baz: 42, .. }`).
#[derive(Clone, Debug, PartialEq)]
pub struct StructPattern {
    /// The struct path.
    pub path: ExprPath,

    /// The field patterns.
    pub fields: Vec<FieldPattern>,

    /// Whether the pattern has a rest (`..`) at the end.
    pub rest: bool,
}

/// A field pattern within a struct pattern (e.g., `bar: 42` or `bar` for shorthand).
#[derive(Clone, Debug, PartialEq)]
pub struct FieldPattern {
    /// The field name.
    pub name: String,

    /// The field pattern. `None` for shorthand (field name used as binding).
    pub pattern: Option<Pattern>,
}

/// A tuple struct pattern (e.g., `Some(x)`, `Foo(1, 2)`).
#[derive(Clone, Debug, PartialEq)]
pub struct TupleStructPattern {
    /// The path to the tuple struct or variant.
    pub path: ExprPath,

    /// The element patterns.
    pub elements: Vec<Pattern>,
}

/// A range pattern (e.g., `0..=9`, `'a'..='z'`).
#[derive(Clone, Debug, PartialEq)]
pub struct RangePattern {
    /// The lower bound.
    pub from: Option<Box<Pattern>>,

    /// The upper bound.
    pub to: Option<Box<Pattern>>,

    /// Whether the range is inclusive (`..=` vs `..`).
    pub inclusive: bool,
}

// ================================================================================================
// Literals
// ================================================================================================

/// A literal value.
#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    /// An integer literal (e.g., `42`, `0xFF`, `1_000_000`).
    Integer(IntegerLiteral),

    /// A floating-point literal (e.g., `3.14`, `1e10`).
    Float(FloatLiteral),

    /// A string literal (e.g., `"hello"`).
    String(String),

    /// A raw string literal (e.g., `r#"hello"#`).
    RawString(String),

    /// A byte string literal (e.g., `b"hello"`).
    ByteString(Vec<u8>),

    /// A character literal (e.g., `'a'`).
    Char(char),

    /// A byte literal (e.g., `b'a'`).
    Byte(u8),

    /// A boolean literal (`true` or `false`).
    Bool(bool),
}

/// An integer literal with optional suffix.
#[derive(Clone, Debug, PartialEq)]
pub struct IntegerLiteral {
    /// The integer value.
    pub value: i128,

    /// Optional type suffix (e.g., `u32`, `i64`, `usize`).
    pub suffix: Option<String>,
}

/// A floating-point literal with optional suffix.
#[derive(Clone, Debug, PartialEq)]
pub struct FloatLiteral {
    /// The float value.
    pub value: f64,

    /// Optional type suffix (e.g., `f32`, `f64`).
    pub suffix: Option<String>,
}

// ================================================================================================
// Attributes
// ================================================================================================

/// An attribute (e.g., `#[derive(Clone)]`, `#[cfg(test)]`).
#[derive(Clone, Debug, PartialEq)]
pub struct Attribute {
    /// Whether the attribute is an inner attribute (`#![...]` vs `#[...]`).
    pub inner: bool,

    /// The attribute path (e.g., `["derive"]`, `["cfg"]`).
    pub path: Vec<String>,

    /// The attribute arguments as a raw token string.
    pub tokens: Option<String>,
}

// ================================================================================================
// Visibility
// ================================================================================================

/// A visibility qualifier.
#[derive(Clone, Debug, PartialEq)]
pub enum Visibility {
    /// Public (`pub`).
    Public,

    /// Crate-visible (`pub(crate)`).
    Crate,

    /// Visible to a specific path (`pub(in path)`).
    Restricted(Vec<String>),

    /// Private (default, no visibility keyword).
    Private,
}
