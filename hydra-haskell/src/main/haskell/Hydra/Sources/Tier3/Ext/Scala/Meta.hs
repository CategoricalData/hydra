module Hydra.Sources.Tier3.Ext.Scala.Meta where

import Hydra.Sources.Tier2.All
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types


scalaMetaModule :: Module
scalaMetaModule = Module ns elements [hydraCoreModule] [hydraCoreModule] $
    Just "A Scala syntax model based on Scalameta (https://scalameta.org)"
  where
    ns = Namespace "hydra.ext.scala.meta"
    def = datatype ns
    meta = typeref ns

    elements = [

      def "PredefString" $ --  See scala/Predef.scala
        wrap string,

      def "ScalaSymbol" $ --  See scala/Symbol.scala
        record [
          "name">: string],

--  scala/meta/Trees.scala source below this line. Hydra type definitions inline

-- package scala.meta
--
-- import org.scalameta.invariants._
-- import scala.meta.classifiers._
-- import scala.meta.inputs._
-- import scala.meta.tokens._
-- import scala.meta.prettyprinters._
-- import scala.meta.internal.trees._
-- import scala.meta.internal.trees.Metadata.binaryCompatField
-- @root trait Tree extends InternalTree {
      def "Tree" $ --  Note: ignoring fields of Tree and InternalTree for now
        union [
          "ref">: meta "Ref",
          "stat">: meta "Stat",
          "type">: meta "Type",
          "bounds">: meta "TypeBounds",
          "pat">: meta "Pat",
          "member">: meta "Member",
          "ctor">: meta "Ctor",
          "template">: meta "Template",
          "mod">: meta "Mod",
          "enumerator">: meta "Enumerator",
          "importer">: meta "Importer",
          "importee">: meta "Importee",
          "caseTree">: meta "CaseTree",
          "source">: meta "Source",
          "quasi">: meta "Quasi"],
--   def parent: Option[Tree]
--   def children: List[Tree]
--
--   def pos: Position
--   def tokens(implicit dialect: Dialect): Tokens
--
--   final override def canEqual(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
--   final override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
--   final override def hashCode: Int = System.identityHashCode(this)
--   final override def toString = scala.meta.internal.prettyprinters.TreeToString(this)
-- }
--
-- object Tree extends InternalTreeXtensions {
--   implicit def classifiable[T <: Tree]: Classifiable[T] = null
--   implicit def showStructure[T <: Tree]: Structure[T] =
--     scala.meta.internal.prettyprinters.TreeStructure.apply[T]
--   implicit def showSyntax[T <: Tree](implicit dialect: Dialect): Syntax[T] =
--     scala.meta.internal.prettyprinters.TreeSyntax.apply[T](dialect)
-- }
--
-- @branch trait Ref extends Tree
      def "Ref" $
        union [
          "name">: meta "Name",
          "init">: meta "Init"],
-- @branch trait Stat extends Tree
      def "Stat" $
        union [
          "term">: meta "Data",
          "decl">: meta "Decl",
          "defn">: meta "Defn",
          "importExport">: meta "ImportExportStat"],
--
-- @branch trait Name extends Ref { def value: String }
      def "Name" $
        union [
          "value">: string,
          "anonymous">: unit,
          "indeterminate">: meta "PredefString"],
-- object Name {
--   def apply(value: String): Name = if (value == "") Name.Anonymous() else Name.Indeterminate(value)
--   def unapply(name: Name): Option[String] = Some(name.value)
--   @ast class Anonymous() extends Name {
--     def value = ""
--     checkParent(ParentChecks.NameAnonymous)
--   }
--   @ast class Indeterminate(value: Predef.String @nonEmpty) extends Name
-- }
--
-- @branch trait Lit extends Data with Pat with Type {
      def "Lit" $
        union [
--   def value: Any
-- }
-- object Lit {
--   def unapply(arg: Lit): Option[Any] = Some(arg.value)
--   @ast class Null() extends Lit { def value: Any = null }
          "null">: unit,
--   @ast class Int(value: scala.Int) extends Lit
          "int">: int32,
--   // NOTE: Lit.Double/Float are strings to work the same across JS/JVM. Example:
--   // 1.4f.toString == "1.399999976158142" // in JS
--   // 1.4f.toString == "1.4"               // in JVM
--   // See https://www.scala-js.org/doc/semantics.html-- tostring-of-float-double-and-unit
--   @ast class Double(format: scala.Predef.String) extends Lit { val value = format.toDouble }
          "double">: float64,
--   object Double { def apply(double: scala.Double): Double = Lit.Double(double.toString) }
--   @ast class Float(format: scala.Predef.String) extends Lit { val value = format.toFloat }
          "float">: float32,
--   object Float { def apply(float: scala.Float): Float = Lit.Float(float.toString) }
--   @ast class Byte(value: scala.Byte) extends Lit
          "byte">: int8,
--   @ast class Short(value: scala.Short) extends Lit
          "short">: int16,
--   @ast class Char(value: scala.Char) extends Lit
          "char">: uint16,
--   @ast class Long(value: scala.Long) extends Lit
          "long">: int64,
--   @ast class Boolean(value: scala.Boolean) extends Lit
          "boolean">: boolean,
--   @ast class Unit() extends Lit { def value: Any = () }
          "unit">: unit,
--   @ast class String(value: scala.Predef.String) extends Lit
          "string">: string,
--   @ast class Symbol(value: scala.Symbol) extends Lit
          "symbol">: meta "ScalaSymbol"],
-- }
--
-- @branch trait Data extends Stat
      def "Data" $
        union [
          "lit">: meta "Lit",
          "ref">: meta "Data_Ref",
          "interpolate">: meta "Data_Interpolate",
          "xml">: meta "Data_Xml",
          "apply">: meta "Data_Apply",
          "applyUsing">: meta "Data_ApplyUsing",
          "applyType">: meta "Data_ApplyType",
          "assign">: meta "Data_Assign",
          "return">: meta "Data_Return",
          "throw">: meta "Data_Throw",
          "ascribe">: meta "Data_Ascribe",
          "annotate">: meta "Data_Annotate",
          "tuple">: meta "Data_Tuple",
          "block">: meta "Data_Block",
          "endMarker">: meta "Data_EndMarker",
          "if">: meta "Data_If",
          "quotedMacroExpr">: meta "Data_QuotedMacroExpr",
          "quotedMacroType">: meta "Data_QuotedMacroType",
          "splicedMacroExpr">: meta "Data_SplicedMacroExpr",
          "match">: meta "Data_Match",
          "try">: meta "Data_Try",
          "tryWithHandler">: meta "Data_TryWithHandler",
          "functionData">: meta "Data_FunctionData",
          "polyFunction">: meta "Data_PolyFunction",
          "partialFunction">: meta "Data_PartialFunction",
          "while">: meta "Data_While",
          "do">: meta "Data_Do",
          "for">: meta "Data_For",
          "forYield">: meta "Data_ForYield",
          "new">: meta "Data_New",
          "newAnonymous">: meta "Data_NewAnonymous",
          "placeholder">: meta "Data_Placeholder",
          "eta">: meta "Data_Eta",
          "repeated">: meta "Data_Repeated",
          "param">: meta "Data_Param"],
-- object Data {
--   @branch trait Ref extends Data with scala.meta.Ref
      def "Data_Ref" $
        union [
          "this">: meta "Data_This",
          "super">: meta "Data_Super",
          "name">: meta "Data_Name",
          "anonymous">: meta "Data_Anonymous",
          "select">: meta "Data_Select",
          "applyUnary">: meta "Data_ApplyUnary"],
--   @ast class This(qual: scala.meta.Name) extends Data_Ref
      def "Data_This" $
        wrap unit,
--   @ast class Super(thisp: scala.meta.Name, superp: scala.meta.Name) extends Data_Ref
      def "Data_Super" $
        record [
          "thisp">: meta "Name",
          "superp">: meta "Name"],
--   @ast class Name(value: Predef.String @nonEmpty) extends scala.meta.Name with Data_Ref with Pat
      def "Data_Name" $
        record [
          "value">: meta "PredefString"],
--   @ast class Anonymous() extends scala.meta.Name with Data_Ref {
      def "Data_Anonymous" $
        wrap unit,
--     def value = ""
--     checkParent(ParentChecks.AnonymousImport)
--   }
--   @ast class Select(qual: Data, name: Data_Name) extends Data_Ref with Pat
      def "Data_Select" $
        record [
          "qual">: meta "Data",
          "name">: meta "Data_Name"],
--   @ast class Interpolate(prefix: Name, parts: List[Lit] @nonEmpty, args: List[Data]) extends Data {
      def "Data_Interpolate" $
        record [
          "prefix">: meta "Data_Name",
          "parts">: list $ meta "Lit",
          "args">: list $ meta "Data"],
--     checkFields(parts.length == args.length + 1)
--   }
--   @ast class Xml(parts: List[Lit] @nonEmpty, args: List[Data]) extends Data {
      def "Data_Xml" $
        record [
          "parts">: list $ meta "Lit",
          "args">: list $ meta "Data"],
--     checkFields(parts.length == args.length + 1)
--   }
--   @ast class Apply(fun: Data, args: List[Data]) extends Data
      def "Data_Apply" $
        record [
          "fun">: meta "Data",
          "args">: list $ meta "Data"],
--   @ast class ApplyUsing(fun: Data, args: List[Data]) extends Data
      def "Data_ApplyUsing" $
        record [
          "fun">: meta "Data",
          "targs">: list $ meta "Data"],
--   @ast class ApplyType(fun: Data, targs: List[Type] @nonEmpty) extends Data
      def "Data_ApplyType" $
        record [
          "lhs">: meta "Data",
          "op">: meta "Data_Name",
          "targs">: list $ meta "Type",
          "args">: list $ meta "Data"],
--   @ast class ApplyInfix(lhs: Data, op: Name, targs: List[Type], args: List[Data]) extends Data
      def "Data_ApplyInfix" $
        record [
          "lhs">: meta "Data",
          "op">: meta "Data_Name",
          "targs">: list $ meta "Type",
          "args">: list $ meta "Data"],
--   @ast class ApplyUnary(op: Name, arg: Data) extends Data_Ref {
      def "Data_ApplyUnary" $
        record [
          "op">: meta "Data_Name",
          "arg">: meta "Data"],
--     checkFields(op.isUnaryOp)
--   }
--   @ast class Assign(lhs: Data, rhs: Data) extends Data {
      def "Data_Assign" $
        record [
          "lhs">: meta "Data",
          "rhs">: meta "Data"],
--     checkFields(lhs.is[Data_Quasi] || lhs.is[Data_Ref] || lhs.is[Data_Apply])
--     checkParent(ParentChecks.DataAssign)
--   }
--   @ast class Return(expr: Data) extends Data
      def "Data_Return" $
        record [
          "expr">: meta "Data"],
--   @ast class Throw(expr: Data) extends Data
      def "Data_Throw" $
        record [
          "expr">: meta "Data"],
--   @ast class Ascribe(expr: Data, tpe: Type) extends Data
      def "Data_Ascribe" $
        record [
          "expr">: meta "Data",
          "tpe">: meta "Type"],
--   @ast class Annotate(expr: Data, annots: List[Mod_Annot] @nonEmpty) extends Data
      def "Data_Annotate" $
        record [
          "expr">: meta "Data",
          "annots">: list $ meta "Mod_Annot"],
--   @ast class Tuple(args: List[Data] @nonEmpty) extends Data {
      def "Data_Tuple" $
        record [
          "args">: list $ meta "Data"],
--     // tuple must have more than one element
--     // however, this element may be Quasi with "hidden" list of elements inside
--     checkFields(args.length > 1 || (args.length == 1 && args.head.is[Data_Quasi]))
--   }
--   @ast class Block(stats: List[Stat]) extends Data {
      def "Data_Block" $
        record [
          "stats">: list $ meta "Stat"],
--     // extension group block can have declarations without body too
--     checkFields(stats.forall(st => st.isBlockStat || st.is[Decl]))
--   }
--   @ast class EndMarker(name: Data_Name) extends Data
      def "Data_EndMarker" $
        record [
          "name">: meta "Data_Name"],
--   @ast class If(cond: Data, thenp: Data, elsep: Data) extends Data {
      def "Data_If" $
        record [
          "cond">: meta "Data",
          "thenp">: meta "Data",
          "elsep">: meta "Data"],
--     @binaryCompatField(since = "4.4.0")
--     private var _mods: List[Mod] = Nil
--   }
--   @ast class QuotedMacroExpr(body: Data) extends Data
      def "Data_QuotedMacroExpr" $
        record [
          "body">: meta "Data"],
--   @ast class QuotedMacroType(tpe: Type) extends Data
      def "Data_QuotedMacroType" $
        record [
          "tpe">: meta "Type"],
--   @ast class SplicedMacroExpr(body: Data) extends Data
      def "Data_SplicedMacroExpr" $
        record [
          "body">: meta "Data"],
--   @ast class Match(expr: Data, cases: List[Case] @nonEmpty) extends Data {
      def "Data_Match" $
        record [
          "expr">: meta "Data",
          "cases">: list $ meta "Case"],
--     @binaryCompatField(since = "4.4.5")
--     private var _mods: List[Mod] = Nil
--   }
--   @ast class Try(expr: Data, catchp: List[Case], finallyp: Option[Data]) extends Data
      def "Data_Try" $
        record [
          "expr">: meta "Data",
          "catchp">: list $ meta "Case",
          "finallyp">: optional $ meta "Data"],
--   @ast class TryWithHandler(expr: Data, catchp: Data, finallyp: Option[Data]) extends Data
      def "Data_TryWithHandler" $
        record [
          "expr">: meta "Data",
          "catchp">: meta "Data",
          "finallyp">: optional $ meta "Data"],
--
--   @branch trait FunctionData extends Data {
      def "Data_FunctionData" $
        union [
          "contextFunction">: meta "Data_ContextFunction",
          "function">: meta "Data_Function"],
--     def params: List[Data_Param]
--     def body: Data
--   }
--   @ast class ContextFunction(params: List[Data_Param], body: Data) extends FunctionData {
      def "Data_ContextFunction" $
        record [
          "params">: list $ meta "Data_Param",
          "body">: meta "Data"],
--     checkFields(
--       params.forall(param =>
--         param.is[Data_Param.Quasi] ||
--           (param.name.is[scala.meta.Name.Anonymous] ==> param.default.isEmpty)
--       )
--     )
--   }
--   @ast class Function(params: List[Data_Param], body: Data) extends FunctionData {
      def "Data_Function" $
        record [
          "params">: list $ meta "Data_Param",
          "body">: meta "Data"],
--     checkFields(
--       params.forall(param =>
--         param.is[Data_Param.Quasi] ||
--           (param.name.is[scala.meta.Name.Anonymous] ==> param.default.isEmpty)
--       )
--     )
--     checkFields(
--       params.exists(_.is[Data_Param.Quasi]) ||
--         params.exists(_.mods.exists(_.is[Mod_Implicit])) ==> (params.length == 1)
--     )
--   }
--   @ast class PolyFunction(tparams: List[Type_Param], body: Data) extends Data
      def "Data_PolyFunction" $
        record [
          "tparams">: list $ meta "Type_Param",
          "body">: meta "Data"],
--   @ast class PartialFunction(cases: List[Case] @nonEmpty) extends Data
      def "Data_PartialFunction" $
        record [
          "cases">: list $ meta "Case"],
--   @ast class While(expr: Data, body: Data) extends Data
      def "Data_While" $
        record [
          "expr">: meta "Data",
          "body">: meta "Data"],
--   @ast class Do(body: Data, expr: Data) extends Data
      def "Data_Do" $
        record [
          "body">: meta "Data",
          "expr">: meta "Data"],
--   @ast class For(enums: List[Enumerator] @nonEmpty, body: Data) extends Data {
      def "Data_For" $
        record [
          "enums">: list $ meta "Enumerator"],
--     checkFields(
--       enums.head.is[Enumerator_Generator] || enums.head.is[Enumerator_CaseGenerator] || enums.head
--         .is[Enumerator_Quasi]
--     )
--   }
--   @ast class ForYield(enums: List[Enumerator] @nonEmpty, body: Data) extends Data
      def "Data_ForYield" $
        record [
          "enums">: list $ meta "Enumerator"],
--   @ast class New(init: Init) extends Data
      def "Data_New" $
        record [
          "init">: meta "Init"],
--   @ast class NewAnonymous(templ: Template) extends Data
      def "Data_NewAnonymous" $
        record [
          "templ">: meta "Template"],
--   @ast class Placeholder() extends Data
      def "Data_Placeholder"
        unit,
--   @ast class Eta(expr: Data) extends Data
      def "Data_Eta" $
        record [
          "expr">: meta "Data"],
--   @ast class Repeated(expr: Data) extends Data {
      def "Data_Repeated" $
        record [
          "expr">: meta "Data"],
--     checkParent(ParentChecks.DataRepeated)
--   }
--   @ast class Param(mods: List[Mod], name: meta.Name, decltpe: Option[Type], default: Option[Data])
--       extends Member
      def "Data_Param" $
        record [
          "mods">: list $ meta "Mod",
          "name">: meta "Name",
          "decltpe">: optional $ meta "Type",
          "default">: optional $ meta "Data"],
--   def fresh(): Data_Name = fresh("fresh")
--   def fresh(prefix: String): Data_Name = Data_Name(prefix + Fresh.nextId())
-- }
--
-- @branch trait Type extends Tree
      def "Type" $
        union [
          "ref">: meta "Type_Ref",
          "anonymousName">: meta "Type_AnonymousName",
          "apply">: meta "Type_Apply",
          "applyInfix">: meta "Type_ApplyInfix",
          "functionType">: meta "Type_FunctionType",
          "polyFunction">: meta "Type_PolyFunction",
          "implicitFunction">: meta "Type_ImplicitFunction",
          "tuple">: meta "Type_Tuple",
          "with">: meta "Type_With",
          "and">: meta "Type_And",
          "or">: meta "Type_Or",
          "refine">: meta "Type_Refine",
          "existential">: meta "Type_Existential",
          "annotate">: meta "Type_Annotate",
          "lambda">: meta "Type_Lambda",
          "macro">: meta "Type_Macro",
          "method">: meta "Type_Method",
          "placeholder">: meta "Type_Placeholder",
          "byName">: meta "Type_ByName",
          "repeated">: meta "Type_Repeated",
          "var">: meta "Type_Var",
          "typedParam">: meta "Type_TypedParam",
          "match">: meta "Type_Match"],
-- object Type {
--   @branch trait Ref extends Type with scala.meta.Ref
      def "Type_Ref" $
        union [
          "name">: meta "Type_Name",
          "select">: meta "Type_Select",
          "project">: meta "Type_Project",
          "singleton">: meta "Type_Singleton"],
--   @ast class Name(value: String @nonEmpty) extends scala.meta.Name with Type_Ref
      def "Type_Name" $
        record [
          "value">: string],
--   @ast class AnonymousName() extends Type
      def "Type_AnonymousName" $
        wrap unit,
--   @ast class Select(qual: Data_Ref, name: Type_Name) extends Type_Ref {
      def "Type_Select" $
        record [
          "qual">: meta "Data_Ref",
          "name">: meta "Type_Name"],
--     checkFields(qual.isPath || qual.is[Data_Super] || qual.is[Data_Ref.Quasi])
--   }
--   @ast class Project(qual: Type, name: Type_Name) extends Type_Ref
      def "Type_Project" $
        record [
          "qual">: meta "Type",
          "name">: meta "Type_Name"],
--   @ast class Singleton(ref: Data_Ref) extends Type_Ref {
      def "Type_Singleton" $
        record [
          "ref">: meta "Data_Ref"],
--     checkFields(ref.isPath || ref.is[Data_Super])
--   }
--   @ast class Apply(tpe: Type, args: List[Type] @nonEmpty) extends Type
      def "Type_Apply" $
        record [
          "tpe">: meta "Type",
          "args">: list $ meta "Type"],
--   @ast class ApplyInfix(lhs: Type, op: Name, rhs: Type) extends Type
      def "Type_ApplyInfix" $
        record [
          "lhs">: meta "Type",
          "op">: meta "Type_Name",
          "rhs">: meta "Type"],
--   @branch trait FunctionType extends Type {
      def "Type_FunctionType" $
        union [
          "function">: meta "Type_Function",
          "contextFunction">: meta "Type_ContextFunction"],
--     def params: List[Type]
--     def res: Type
--   }
--   @ast class Function(params: List[Type], res: Type) extends FunctionType
      def "Type_Function" $
        record [
          "params">: list $ meta "Type",
          "res">: meta "Type"],
--   @ast class PolyFunction(tparams: List[Type_Param], tpe: Type) extends Type
      def "Type_PolyFunction" $
        record [
          "tparams">: list $ meta "Type_Param",
          "tpe">: meta "Type"],
--   @ast class ContextFunction(params: List[Type], res: Type) extends FunctionType
      def "Type_ContextFunction" $
        record [
          "params">: list $ meta "Type",
          "res">: meta "Type"],
--   @ast @deprecated("Implicit functions are not supported in any dialect")
--   class ImplicitFunction(
      def "Type_ImplicitFunction" $
        record [
--       params: List[Type],
          "params">: list $ meta "Type",
--       res: Type
          "res">: meta "Type"],
--   ) extends Type
--   @ast class Tuple(args: List[Type] @nonEmpty) extends Type {
      def "Type_Tuple" $
        record [
          "args">: list $ meta "Type"],
--     checkFields(args.length > 1 || (args.length == 1 && args.head.is[Type_Quasi]))
--   }
--   @ast class With(lhs: Type, rhs: Type) extends Type
      def "Type_With" $
        record [
          "lhs">: meta "Type",
          "rhs">: meta "Type"],
--   @ast class And(lhs: Type, rhs: Type) extends Type
      def "Type_And" $
        record [
          "lhs">: meta "Type",
          "rhs">: meta "Type"],
--   @ast class Or(lhs: Type, rhs: Type) extends Type
      def "Type_Or" $
        record [
          "lhs">: meta "Type",
          "rhs">: meta "Type"],
--   @ast class Refine(tpe: Option[Type], stats: List[Stat]) extends Type {
      def "Type_Refine" $
        record [
          "tpe">: optional $ meta "Type",
          "stats">: list $ meta "Stat"],
--     checkFields(stats.forall(_.isRefineStat))
--   }
--   @ast class Existential(tpe: Type, stats: List[Stat] @nonEmpty) extends Type {
      def "Type_Existential" $
        record [
          "tpe">: meta "Type",
          "stats">: list $ meta "Stat"],
--     checkFields(stats.forall(_.isExistentialStat))
--   }
--   @ast class Annotate(tpe: Type, annots: List[Mod_Annot] @nonEmpty) extends Type
      def "Type_Annotate" $
        record [
          "tpe">: meta "Type",
          "annots">: list $ meta "Mod_Annot"],
--   @ast class Lambda(tparams: List[Type_Param], tpe: Type) extends Type {
      def "Type_Lambda" $
        record [
          "tparams">: list $ meta "Type_Param",
          "tpe">: meta "Type"],
--     checkParent(ParentChecks.LambdaType)
--   }
--   @ast class Macro(body: Data) extends Type
      def "Type_Macro" $
        record [
          "body">: meta "Data"],
--   @deprecated("Method type syntax is no longer supported in any dialect", "4.4.3")
--   @ast class Method(paramss: List[List[Data_Param]], tpe: Type) extends Type {
      def "Type_Method" $
        record [
          "paramss">: list $ list $ meta "Data_Param",
          "tpe">: meta "Type"],
--     checkParent(ParentChecks.TypeMethod)
--   }
--   @ast class Placeholder(bounds: Bounds) extends Type
      def "Type_Placeholder" $
        record [
          "bounds">: meta "TypeBounds"],
--   @ast class Bounds(lo: Option[Type], hi: Option[Type]) extends Tree
      def "TypeBounds" $
        record [
          "lo">: optional $ meta "Type",
          "hi">: optional $ meta "Type"],
--   @ast class ByName(tpe: Type) extends Type {
      def "Type_ByName" $
        record [
          "tpe">: meta "Type"],
--     checkParent(ParentChecks.TypeByName)
--   }
--   @ast class Repeated(tpe: Type) extends Type {
      def "Type_Repeated" $
        record [
          "tpe">: meta "Type"],
--     checkParent(ParentChecks.TypeRepeated)
--   }
--   @ast class Var(name: Name) extends Type with Member_Type {
      def "Type_Var" $
        record [
          "name">: meta "Type_Name"],
--     checkFields(name.value(0).isLower)
--     checkParent(ParentChecks.TypeVar)
--   }
--
--   @ast class TypedParam(name: Name, typ: Type) extends Type with Member_Type
      def "Type_TypedParam" $
        record [
          "name">: meta "Name",
          "typ">: meta "Type"],
--   @ast class Param(
      def "Type_Param" $
        record [
--       mods: List[Mod],
          "mods">: list $ meta "Mod",
--       name: meta.Name,
          "name">: meta "Name",
--       tparams: List[Type_Param],
          "tparams">: list $ meta "Type_Param",
--       tbounds: TypeBounds,
          "tbounds">: list $ meta "TypeBounds",
--       vbounds: List[Type],
          "vbounds">: list $ meta "Type",
--       cbounds: List[Type]
          "cbounds">: list $ meta "Type"],
--   ) extends Member
--
--   @ast class Match(tpe: Type, cases: List[TypeCase] @nonEmpty) extends Type
      def "Type_Match" $
        record [
          "tpe">: meta "Type",
          "cases">: list $ meta "TypeCase"],
--   def fresh(): Type_Name = fresh("fresh")
--   def fresh(prefix: String): Type_Name = Type_Name(prefix + Fresh.nextId())
-- }
--
-- @branch trait Pat extends Tree
      def "Pat" $
        union [
          "var">: meta "Pat_Var",
          "wildcard">: unit,
          "seqWildcard">: unit,
          "bind">: meta "Pat_Bind",
          "alternative">: meta "Pat_Alternative",
          "tuple">: meta "Pat_Tuple",
          "repeated">: meta "Pat_Repeated",
          "extract">: meta "Pat_Extract",
          "extractInfix">: meta "Pat_ExtractInfix",
          "interpolate">: meta "Pat_Interpolate",
          "xml">: meta "Pat_Xml",
          "typed">: meta "Pat_Typed",
          "macro">: meta "Pat_Macro",
          "given">: meta "Pat_Given"],
-- object Pat {
--   @ast class Var(name: scala.meta.Data_Name) extends Pat with Member_Data { @
      def "Pat_Var" $
        record [
          "name">: meta "Data_Name"],
--     // NOTE: can't do this check here because of things like `val X = 2`
--     // checkFields(name.value(0).isLower)
--     checkParent(ParentChecks.PatVar)
--   }
--   @ast class Wildcard() extends Pat
--   @ast class SeqWildcard() extends Pat {
--     checkParent(ParentChecks.PatSeqWildcard)
--   }
--   @ast class Bind(lhs: Pat, rhs: Pat) extends Pat {
      def "Pat_Bind" $
        record [
          "lhs">: meta "Pat",
          "rhs">: meta "Pat"],
--     checkFields(lhs.is[Pat_Var] || lhs.is[Pat_Quasi])
--   }
--   @ast class Alternative(lhs: Pat, rhs: Pat) extends Pat
      def "Pat_Alternative" $
        record [
          "lhs">: meta "Pat",
          "rhs">: meta "Pat"],
--   @ast class Tuple(args: List[Pat] @nonEmpty) extends Pat {
      def "Pat_Tuple" $
        record [
          "args">: list $ meta "Pat"],
--     checkFields(args.length > 1 || (args.length == 1 && args.head.is[Pat_Quasi]))
--   }
--   @ast class Repeated(name: scala.meta.Data_Name) extends Pat
      def "Pat_Repeated" $
        record [
          "name">: meta "Data_Name"],
--   @ast class Extract(fun: Data, args: List[Pat]) extends Pat {
      def "Pat_Extract" $
        record [
          "fun">: meta "Data",
          "args">: list $ meta "Pat"],
--     checkFields(fun.isExtractor)
--   }
--   @ast class ExtractInfix(lhs: Pat, op: Data_Name, rhs: List[Pat]) extends Pat
      def "Pat_ExtractInfix" $
        record [
          "lhs">: meta "Pat",
          "op">: meta "Data_Name",
          "rhs">: list $ meta "Pat"],
--   @ast class Interpolate(prefix: Data_Name, parts: List[Lit] @nonEmpty, args: List[Pat])
      def "Pat_Interpolate" $
        record [
          "prefix">: meta "Data_Name",
          "parts">: list $ meta "Lit"],
--       extends Pat {
--     checkFields(parts.length == args.length + 1)
--   }
--   @ast class Xml(parts: List[Lit] @nonEmpty, args: List[Pat]) extends Pat {
      def "Pat_Xml" $
        record [
          "parts">: list $ meta "Lit",
          "args">: list $ meta "Pat"],
--     checkFields(parts.length == args.length + 1)
--   }
--   @ast class Typed(lhs: Pat, rhs: Type) extends Pat {
      def "Pat_Typed" $
        record [
          "lhs">: meta "Pat",
          "rhs">: meta "Type"],
--     checkFields(!rhs.is[Type_Var] && !rhs.is[Type_Placeholder])
--   }
--   @ast class Macro(body: Data) extends Pat {
      def "Pat_Macro" $
        record [
          "body">: meta "Data"],
--     checkFields(body.is[Data_QuotedMacroExpr] || body.is[Data_QuotedMacroType])
--   }
--   @ast class Given(tpe: Type) extends Pat
      def "Pat_Given" $
        record [
          "tpe">: meta "Type"],
--   def fresh(): Pat_Var = Pat_Var(Data_fresh())
--   def fresh(prefix: String): Pat_Var = Pat_Var(Data_fresh(prefix))
-- }
--
-- @branch trait Member extends Tree {
      def "Member" $
        union [
          "term">: meta "Member_Data",
          "type">: meta "Member_Type",
          "termParam">: meta "Data_Param",
          "typeParam">: meta "Type_Param",
          "self">: meta "Self"],
--   def name: Name
-- }
-- object Member {
--   @branch trait Data extends Member {
      def "Member_Data" $
        union [
          "pkg">: meta "Pkg",
          "object">: meta "Pkg_Object"],
--     def name: scala.meta.Data_Name
--   }
--   @branch trait Type extends Member {
      def "Member_Type" $
        record [
--     def name: scala.meta.Type_Name
          "name">: meta "Type_Name"],
--   }
-- }
--
-- @branch trait Decl extends Stat
      def "Decl" $
        union [
          "val">: meta "Decl_Val",
          "var">: meta "Decl_Var",
          "def">: meta "Decl_Def",
          "type">: meta "Decl_Type",
          "given">: meta "Decl_Given"],
-- object Decl {
--   @ast class Val(mods: List[Mod], pats: List[Pat] @nonEmpty, decltpe: scala.meta.Type) extends Decl
      def "Decl_Val" $
        record [
          "mods">: list $ meta "Mod",
          "pats">: list $ meta "Pat",
          "decltpe">: meta "Type"],
--   @ast class Var(mods: List[Mod], pats: List[Pat] @nonEmpty, decltpe: scala.meta.Type) extends Decl
      def "Decl_Var" $
        record [
          "mods">: list $ meta "Mod",
          "pats">: list $ meta "Pat",
          "decltpe">: meta "Type"],
--   @ast class Def(
      def "Decl_Def" $
        record [
--       mods: List[Mod],
          "mods">: list $ meta "Mod",
--       name: Data_Name,
          "name">: meta "Data_Name",
--       tparams: List[scala.meta.Type_Param],
          "tparams">: list $ meta "Type_Param",
--       paramss: List[List[Data_Param]],
          "paramss">: list $ list $ meta "Data_Param",
--       decltpe: scala.meta.Type
          "decltpe">: meta "Type"],
--   ) extends Decl with Member_Data @
      --   @ast class Type(
      def "Decl_Type" $
        record [
--       mods: List[Mod],
          "mods">: list $ meta "Mod",
--       name: scala.meta.Type_Name,
          "name">: meta "Type_Name",
--       tparams: List[scala.meta.Type_Param],
          "tparams">: list $ meta "Type_Param",
--       bounds: scala.meta.TypeBounds
          "bounds">: meta "TypeBounds"],
--   ) extends Decl with Member_Type
--   @ast class Given(
      def "Decl_Given" $
        record [
--       mods: List[Mod],
          "mods">: list $ meta "Mod",
--       name: Data_Name,
          "name">: meta "Data_Name",
--       tparams: List[scala.meta.Type_Param],
          "tparams">: list $ meta "Type_Param",
--       sparams: List[List[Data_Param]],
          "sparams">: list $ list $ meta "Data_Param",
--       decltpe: scala.meta.Type
          "decltpe">: meta "Type"],
--   ) extends Decl with Member_Data @
-- }
--
-- @branch trait Defn extends Stat
      def "Defn" $
        union [
          "val">: meta "Defn_Val",
          "var">: meta "Defn_Var",
          "given">: meta "Defn_Given",
          "enum">: meta "Defn_Enum",
          "enumCase">: meta "Defn_EnumCase",
          "repeatedEnumCase">: meta "Defn_RepeatedEnumCase",
          "givenAlias">: meta "Defn_GivenAlias",
          "extensionGroup">: meta "Defn_ExtensionGroup",
          "def">: meta "Defn_Def",
          "macro">: meta "Defn_Macro",
          "type">: meta "Defn_Type",
          "class">: meta "Defn_Class",
          "trait">: meta "Defn_Trait",
          "object">: meta "Defn_Object"],
-- object Defn {
--   @ast class Val(
      def "Defn_Val" $
        record [
--       mods: List[Mod],
          "mods">: list $ meta "Mod",
--       pats: List[Pat] @nonEmpty,
          "pats">: list $ meta "Pat",
--       decltpe: Option[scala.meta.Type],
          "decltpe">: optional $ meta "Type",
--       rhs: Data
          "rhs">: meta "Data"],
--   ) extends Defn {
--     checkFields(pats.forall(!_.is[Data_Name]))
--   }
--   @ast class Var(
      def "Defn_Var" $
        record [
--       mods: List[Mod],
          "mods">: list $ meta "Mod",
--       pats: List[Pat] @nonEmpty,
          "pats">: list $ meta "Pat",
--       decltpe: Option[scala.meta.Type],
          "decltpe">: meta "Type",
--       rhs: Option[Data]
          "rhs">: optional $ meta "Data"],
--   ) extends Defn {
--     checkFields(pats.forall(!_.is[Data_Name]))
--     checkFields(decltpe.nonEmpty || rhs.nonEmpty)
--     checkFields(rhs.isEmpty ==> pats.forall(_.is[Pat_Var]))
--   }
--   @ast class Given(
      def "Defn_Given" $
        record [
--       mods: List[Mod],
          "mods">: list $ meta "Mod",
--       name: scala.meta.Name,
          "name">: meta "Name",
--       tparams: List[scala.meta.Type_Param],
          "tparams">: list $ list $ meta "Type_Param",
--       sparams: List[List[Data_Param]],
          "sparams">: list $ list $ meta "Data_Param",
--       templ: Template
          "templ">: meta "Template"],
--   ) extends Defn
--   @ast class Enum(
      def "Defn_Enum" $
        record [
--       mods: List[Mod],
          "mods">: list $ meta "Mod",
--       name: scala.meta.Type_Name,
          "name">: meta "Type_Name",
--       tparams: List[scala.meta.Type_Param],
          "tparams">: list $ meta "Type_Param",
--       ctor: Ctor_Primary,
          "ctor">: meta "Ctor_Primary",
--       templ: Template
          "template">: meta "Template"],
--   ) extends Defn with Member_Type
--   @ast class EnumCase(
      def "Defn_EnumCase" $
        record [
--       mods: List[Mod],
          "mods">: list $ meta "Mod",
--       name: Data_Name,
          "name">: meta "Data_Name",
--       tparams: List[scala.meta.Type_Param],
          "tparams">: list $ meta "Type_Param",
--       ctor: Ctor_Primary,
          "ctor">: meta "Ctor_Primary",
--       inits: List[Init]
          "inits">: list $ meta "Init"],
--   ) extends Defn with Member_Data { @
--     checkParent(ParentChecks.EnumCase)
--   }
--   @ast class RepeatedEnumCase(
      def "Defn_RepeatedEnumCase" $
        record [
--       mods: List[Mod],
          "mods">: list $ meta "Mod",
--       cases: List[Data_Name]
          "cases">: list $ meta "Data_Name"],
--   ) extends Defn {
--     checkParent(ParentChecks.EnumCase)
--   }
--   @ast class GivenAlias(
      def "Defn_GivenAlias" $
        record [
--       mods: List[Mod],
          "mods">: list $ meta "Mod",
--       name: scala.meta.Name,
          "name">: meta "Name",
--       tparams: List[scala.meta.Type_Param],
          "tparams">: list $ list $ meta "Type_Param",
--       sparams: List[List[Data_Param]],
          "sparams">: list $ list $ meta "Data_Param",
--       decltpe: scala.meta.Type,
          "decltpe">: meta "Type",
--       body: Data
          "body">: meta "Data"],
--   ) extends Defn
--   @ast class ExtensionGroup(
      def "Defn_ExtensionGroup" $
        record [
--       tparams: List[scala.meta.Type_Param],
          "tparams">: list $ meta "Type_Param",
--       paramss: List[List[Data_Param]],
          "parmss">: list $ list $ meta "Data_Param",
--       body: Stat
          "body">: meta "Stat"],
--   ) extends Defn
--   @ast class Def(
      def "Defn_Def" $
        record [
--       mods: List[Mod],
          "mods">: list $ meta "Mod",
--       name: Data_Name,
          "name">: meta "Data_Name",
--       tparams: List[scala.meta.Type_Param],
          "tparams">: list $ meta "Type_Param",
--       paramss: List[List[Data_Param]],
          "paramss">: list $ list $ meta "Data_Param",
--       decltpe: Option[scala.meta.Type],
          "decltpe">: optional $ meta "Type",
--       body: Data
          "body">: meta "Data"],
--   ) extends Defn with Member_Data { @
--     checkFields(paramss.forall(onlyLastParamCanBeRepeated))
--   }
--   @ast class Macro(
      def "Defn_Macro" $
        record [
--       mods: List[Mod],
          "mods">: list $ meta "Mod",
--       name: Data_Name,
          "name">: meta "Data_Name",
--       tparams: List[scala.meta.Type_Param],
          "tparams">: list $ meta "Type_Param",
--       paramss: List[List[Data_Param]],
          "paramss">: list $ list $ meta "Data_Param",
--       decltpe: Option[scala.meta.Type],
          "decltpe">: optional $ meta "Type",
--       body: Data
          "body">: meta "Data"],
--   ) extends Defn with Member_Data @
--   @ast class Type(
      def "Defn_Type" $
        record [
--       mods: List[Mod],
          "mods">: list $ meta "Mod",
--       name: scala.meta.Type_Name,
          "name">: meta "Type_Name",
--       tparams: List[scala.meta.Type_Param],
          "tparams">: list $ meta "Type_Param",
--       body: scala.meta.Type
          "body">: meta "Type"],
--   ) extends Defn with Member_Type {
--     @binaryCompatField("4.4.0")
--     private var _bounds: scala.meta.TypeBounds = scala.meta.TypeBounds(None, None)
--   }
--   @ast class Class(
      def "Defn_Class" $
        record [
--       mods: List[Mod],
          "mods">: list $ meta "Mod",
--       name: scala.meta.Type_Name,
          "name">: meta "Type_Name",
--       tparams: List[scala.meta.Type_Param],
          "tparams">: list $ meta "Type_Param",
--       ctor: Ctor_Primary,
          "ctor">: meta "Ctor_Primary",
--       templ: Template
          "template">: meta "Template"],
--   ) extends Defn with Member_Type
--   @ast class Trait(
      def "Defn_Trait" $
        record [
--       mods: List[Mod],
          "mods">: list $ meta "Mod",
--       name: scala.meta.Type_Name,
          "name">: meta "Type_Name",
--       tparams: List[scala.meta.Type_Param],
          "tparams">: list $ meta "Type_Param",
--       ctor: Ctor_Primary,
          "ctor">: meta "Ctor_Primary",
--       templ: Template
          "template">: meta "Template"],
--   ) extends Defn with Member_Type {
--     checkFields(templ.is[Template.Quasi] || templ.stats.forall(!_.is[Ctor]))
--   }
--   @ast class Object(mods: List[Mod], name: Data_Name, templ: Template)
      def "Defn_Object" $
        record [
          "name">: meta "Data_Name"], --  from Member_Data
--       extends Defn with Member_Data { @
--     checkFields(templ.is[Template.Quasi] || templ.stats.forall(!_.is[Ctor]))
--   }
-- }
--
-- @ast class Pkg(ref: Data_Ref, stats: List[Stat]) extends Member_Data with Stat { @
      def "Pkg" $
        record [
          "name">: meta "Data_Name", --  from Member_Data
          "ref">: meta "Data_Ref",
          "stats">: list $ meta "Stat"],
--   checkFields(ref.isQualId)
--   def name: Data_Name = ref match {
--     case name: Data_Name => name
--     case Data_Select(_, name: Data_Name) => name
--   }
-- }
-- object Pkg {
--   @ast class Object(mods: List[Mod], name: Data_Name, templ: Template)
--       extends Member_Data with Stat { @
      def "Pkg_Object" $
        record [
          "mods">: list $ meta "Mod",
          "name">: meta "Data_Name",
          "template">: meta "Template"],
--     checkFields(templ.is[Template.Quasi] || templ.stats.forall(!_.is[Ctor]))
--   }
-- }
--
-- // NOTE: The names of Ctor_Primary and Ctor_Secondary here is always Name.Anonymous.
-- // While seemingly useless, this name is crucial to one of the key principles behind the semantic API:
-- // "every definition and every reference should carry a name".
-- @branch trait Ctor extends Tree with Member
      def "Ctor" $
        union [
          "primary">: meta "Ctor_Primary",
          "secondary">: meta "Ctor_Secondary"],
-- object Ctor {
--   @ast class Primary(mods: List[Mod], name: Name, paramss: List[List[Data_Param]]) extends Ctor
      def "Ctor_Primary" $
        record [
          "mods">: list $ meta "Mod",
          "name">: meta "Name",
          "paramss">: list $ list $ meta "Data_Param"],
--   @ast class Secondary(
      def "Ctor_Secondary" $
        record [
--       mods: List[Mod],
          "mods">: list $ meta "Mod",
--       name: Name,
          "name">: meta "Name",
--       paramss: List[List[Data_Param]] @nonEmpty,
          "paramss">: list $ list $ meta "Data_Param",
--       init: Init,
          "init">: meta "Init",
--       stats: List[Stat]
          "stats">: list $ meta "Stat"],
--   ) extends Ctor with Stat {
--     checkFields(stats.forall(_.isBlockStat))
--   }
-- }
--
-- // NOTE: The name here is always Name.Anonymous.
-- // See comments to Ctor_Primary and Ctor_Secondary for justification.
-- @ast class Init(tpe: Type, name: Name, argss: List[List[Data]]) extends Ref {
      def "Init" $
        record [
          "tpe">: meta "Type",
          "name">: meta "Name",
          "argss">: list $ list $ meta "Data"],
--   checkFields(tpe.isConstructable)
--   checkParent(ParentChecks.Init)
-- }
--
-- @ast class Self(name: Name, decltpe: Option[Type]) extends Member
      def "Self" $
        wrap unit,
--
-- @ast class Template(
      def "Template" $
        record [
--     early: List[Stat],
          "early">: list $ meta "Stat",
--     inits: List[Init],
          "inits">: list $ meta "Init",
--     self: Self,
          "self">: meta "Self",
--     stats: List[Stat]
          "stats">: list $ meta "Stat"],
-- ) extends Tree {
--   @binaryCompatField("4.4.0")
--   private var _derives: List[Type] = Nil
--   checkFields(early.forall(_.isEarlyStat && inits.nonEmpty))
--   checkFields(stats.forall(_.isTemplateStat))
-- }
--
-- @branch trait Mod extends Tree
      def "Mod" $
        union [
          "annot">: meta "Mod_Annot",
          "private">: meta "Mod_Private",
          "protected">: meta "Mod_Protected",
          "implicit">: unit,
          "final">: unit,
          "sealed">: unit,
          "open">: unit,
          "super">: unit,
          "override">: unit,
          "case">: unit,
          "abstract">: unit,
          "covariant">: unit,
          "contravariant">: unit,
          "lazy">: unit,
          "valParam">: unit,
          "varParam">: unit,
          "infix">: unit,
          "inline">: unit,
          "using">: unit,
          "opaque">: unit,
          "transparent">: unit],
-- object Mod {
--   @ast class Annot(init: Init) extends Mod {
      def "Mod_Annot" $
        record [
          "init">: meta "Init"],
--     @deprecated("Use init instead", "1.9.0")
--     def body = init
--   }
--   @ast class Private(within: Ref) extends Mod {
      def "Mod_Private" $
        record [
          "within">: meta "Ref"],
--     checkFields(within.isWithin)
--   }
--   @ast class Protected(within: Ref) extends Mod {
      def "Mod_Protected" $
        record [
          "within">: meta "Ref"],
--     checkFields(within.isWithin)
--   }
--   @ast class Implicit() extends Mod
--   @ast class Final() extends Mod
--   @ast class Sealed() extends Mod
--   @ast class Open() extends Mod
--   @deprecated("Super traits introduced in dotty, but later removed.")
--   @ast class Super() extends Mod
--   @ast class Override() extends Mod
--   @ast class Case() extends Mod
--   @ast class Abstract() extends Mod
--   @ast class Covariant() extends Mod
--   @ast class Contravariant() extends Mod
--   @ast class Lazy() extends Mod
--   @ast class ValParam() extends Mod
--   @ast class VarParam() extends Mod
--   @ast class Infix() extends Mod
--   @ast class Inline() extends Mod
--   @ast class Using() extends Mod
--   @ast class Opaque() extends Mod
--   @ast class Transparent() extends Mod
-- }
--
-- @branch trait Enumerator extends Tree
      def "Enumerator" $
        union [
          "generator">: meta "Enumerator_Generator",
          "caseGenerator">: meta "Enumerator_CaseGenerator",
          "val">: meta "Enumerator_Val",
          "guard">: meta "Enumerator_Guard"],
-- object Enumerator {
--   @ast class Generator(pat: Pat, rhs: Data) extends Enumerator
      def "Enumerator_Generator" $
        record [
          "pat">: meta "Pat",
          "rhs">: meta "Data"],
--   @ast class CaseGenerator(pat: Pat, rhs: Data) extends Enumerator
      def "Enumerator_CaseGenerator" $
        record [
          "pat">: meta "Pat",
          "rhs">: meta "Data"],
--   @ast class Val(pat: Pat, rhs: Data) extends Enumerator
      def "Enumerator_Val" $
        record [
          "pat">: meta "Pat",
          "rhs">: meta "Data"],
--   @ast class Guard(cond: Data) extends Enumerator
      def "Enumerator_Guard" $
        record [
          "cond">: meta "Data"],
-- }
--
-- @branch trait ImportExportStat extends Stat {
      def "ImportExportStat" $
        union [
          "import">: meta "Import",
          "export">: meta "Export"],
--   def importers: List[Importer]
-- }
-- @ast class Import(importers: List[Importer] @nonEmpty) extends ImportExportStat
      def "Import" $
        record [
          "importers">: list $ meta "Importer"],
-- @ast class Export(importers: List[Importer] @nonEmpty) extends ImportExportStat
      def "Export" $
        record [
          "importers">: list $ meta "Importer"],
--
-- @ast class Importer(ref: Data_Ref, importees: List[Importee] @nonEmpty) extends Tree {
      def "Importer" $
        record [
          "ref">: meta "Data_Ref",
          "importees">: list $ meta "Importee"],
--   checkFields(ref.isStableId)
-- }
--
-- @branch trait Importee extends Tree with Ref
      def "Importee" $
        union [
          "wildcard">: unit,
          "given">: meta "Importee_Given",
          "givenAll">: unit,
          "name">: meta "Importee_Name",
          "rename">: meta "Importee_Rename",
          "unimport">: meta "Importee_Unimport"],
-- object Importee {
--   @ast class Wildcard() extends Importee
--   @ast class Given(tpe: Type) extends Importee
      def "Importee_Given" $
        record [
          "tpe">: meta "Type"],
--   @ast class GivenAll() extends Importee
--   @ast class Name(name: scala.meta.Name) extends Importee {
      def "Importee_Name" $
        record [
          "name">: meta "Name"],
--     checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
--   }
--   @ast class Rename(name: scala.meta.Name, rename: scala.meta.Name) extends Importee {
      def "Importee_Rename" $
        record [
          "name">: meta "Name",
          "rename">: meta "Name"],
--     checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
--     checkFields(rename.is[scala.meta.Name.Quasi] || rename.is[scala.meta.Name.Indeterminate])
--   }
--   @ast class Unimport(name: scala.meta.Name) extends Importee {
      def "Importee_Unimport" $
        record [
          "name">: meta "Name"],
--     checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
--   }
-- }
--
-- @branch trait CaseTree extends Tree {
      def "CaseTree" $
        union [
          "case">: meta "Case",
          "typeCase">: meta "TypeCase"],
--   def pat: Tree
--   def body: Tree
-- }
-- @ast class Case(pat: Pat, cond: Option[Data], body: Data) extends CaseTree
      def "Case" $
        record [
          "pat">: meta "Pat",
          "cond">: optional $ meta "Data",
          "body">: meta "Data"],
-- @ast class TypeCase(pat: Type, body: Type) extends CaseTree
      def "TypeCase" $
        record [
          "pat">: meta "Type",
          "body">: meta "Type"],
--
-- @ast class Source(stats: List[Stat]) extends Tree {
      def "Source" $
        record [
          "stats">: list $ meta "Stat"],
--   // NOTE: This validation has been removed to allow dialects with top-level terms.
--   // Ideally, we should push the validation into a dialect-specific prettyprinter when -- 220 is fixed.
--   // checkFields(stats.forall(_.isTopLevelStat))
-- }
--
-- package internal.trees {
--   // NOTE: Quasi is a base trait for a whole bunch of classes.
--   // Every root, branch and ast trait/class among scala.meta trees (except for quasis themselves)
--   // has a corresponding quasi, e.g. Data_Quasi or Type_Quasi.
--   //
--   // Here's how quasis represent unquotes
--   // (XXX below depends on the position where the unquote occurs, e.g. q"$x" will result in Data_Quasi):
--   //   * $x => XXX.Quasi(0, XXX.Name("x"))
--   //   * ..$xs => XXX.Quasi(1, XXX.Quasi(0, XXX.Name("xs"))
--   //   * ...$xss => XXX.Quasi(2, XXX.Quasi(0, XXX.Name("xss"))
--   //   * ..{$fs($args)} => Complex ellipses aren't supported yet
--   @branch trait Quasi extends Tree {
      def "Quasi" $ --  TODO
        wrap unit]
--     def rank: Int
--     def tree: Tree
--     def pt: Class[_]
--     def become[T <: Quasi: AstInfo]: T
--   }
--
--   @registry object All
-- }
