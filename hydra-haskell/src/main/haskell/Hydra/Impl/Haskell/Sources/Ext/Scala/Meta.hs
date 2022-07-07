module Hydra.Impl.Haskell.Sources.Ext.Scala.Meta where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


scalaMetaModule :: Module Meta
scalaMetaModule = Module scalaMeta []

scalaMetaName :: GraphName
scalaMetaName = GraphName "hydra/ext/scala/meta"

scalaMeta :: Graph Meta
scalaMeta = Graph scalaMetaName elements (const True) hydraCoreName
  where
    def = datatype scalaMetaName
    meta = nominal . qualify scalaMetaName . Name

    elements = [

      def "PredefString" --  See scala/Predef.scala
        string,

      def "ScalaSymbol" $ --  See scala/Symbol.scala
        record [
          field "name" string],

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
          field "ref" $ meta "Ref",
          field "stat" $ meta "Stat",
          field "type" $ meta "Type",
          field "bounds" $ meta "Type.Bounds",
          field "pat" $ meta "Pat",
          field "member" $ meta "Member",
          field "ctor" $ meta "Ctor",
          field "template" $ meta "Template",
          field "mod" $ meta "Mod",
          field "enumerator" $ meta "Enumerator",
          field "importer" $ meta "Importer",
          field "importee" $ meta "Importee",
          field "caseTree" $ meta "CaseTree",
          field "source" $ meta "Source",
          field "quasi" $ meta "Quasi"],
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
          field "name" $ meta "Name",
          field "init" $ meta "Init"],
-- @branch trait Stat extends Tree
      def "Stat" $
        union [
          field "term" $ meta "Data",
          field "decl" $ meta "Decl",
          field "defn" $ meta "Defn",
          field "importExport" $ meta "ImportExportStat"],
--
-- @branch trait Name extends Ref { def value: String }
      def "Name" $
        union [
          field "value" string,
          field "anonymous" unit,
          field "indeterminate" $ meta "PredefString"],
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
          field "null" unit,
--   @ast class Int(value: scala.Int) extends Lit
          field "int" int32,
--   // NOTE: Lit.Double/Float are strings to work the same across JS/JVM. Example:
--   // 1.4f.toString == "1.399999976158142" // in JS
--   // 1.4f.toString == "1.4"               // in JVM
--   // See https://www.scala-js.org/doc/semantics.html-- tostring-of-float-double-and-unit
--   @ast class Double(format: scala.Predef.String) extends Lit { val value = format.toDouble }
          field "double" float64,
--   object Double { def apply(double: scala.Double): Double = Lit.Double(double.toString) }
--   @ast class Float(format: scala.Predef.String) extends Lit { val value = format.toFloat }
          field "float" float32,
--   object Float { def apply(float: scala.Float): Float = Lit.Float(float.toString) }
--   @ast class Byte(value: scala.Byte) extends Lit
          field "byte" int8,
--   @ast class Short(value: scala.Short) extends Lit
          field "short" int16,
--   @ast class Char(value: scala.Char) extends Lit
          field "char" uint16,
--   @ast class Long(value: scala.Long) extends Lit
          field "long" int64,
--   @ast class Boolean(value: scala.Boolean) extends Lit
          field "boolean" boolean,
--   @ast class Unit() extends Lit { def value: Any = () }
          field "unit" unit,
--   @ast class String(value: scala.Predef.String) extends Lit
          field "string" string,
--   @ast class Symbol(value: scala.Symbol) extends Lit
          field "symbol" $ meta "ScalaSymbol"],
-- }
--
-- @branch trait Data extends Stat
      def "Data" $
        union [
          field "lit" $ meta "Lit",
          field "ref" $ meta "Data.Ref",
          field "interpolate" $ meta "Data.Interpolate",
          field "xml" $ meta "Data.Xml",
          field "apply" $ meta "Data.Apply",
          field "applyUsing" $ meta "Data.ApplyUsing",
          field "applyType" $ meta "Data.ApplyType",
          field "assign" $ meta "Data.Assign",
          field "return" $ meta "Data.Return",
          field "throw" $ meta "Data.Throw",
          field "ascribe" $ meta "Data.Ascribe",
          field "annotate" $ meta "Data.Annotate",
          field "tuple" $ meta "Data.Tuple",
          field "block" $ meta "Data.Block",
          field "endMarker" $ meta "Data.EndMarker",
          field "if" $ meta "Data.If",
          field "quotedMacroExpr" $ meta "Data.QuotedMacroExpr",
          field "quotedMacroType" $ meta "Data.QuotedMacroType",
          field "splicedMacroExpr" $ meta "Data.SplicedMacroExpr",
          field "match" $ meta "Data.Match",
          field "try" $ meta "Data.Try",
          field "tryWithHandler" $ meta "Data.TryWithHandler",
          field "functionData" $ meta "Data.FunctionData",
          field "polyFunction" $ meta "Data.PolyFunction",
          field "partialFunction" $ meta "Data.PartialFunction",
          field "while" $ meta "Data.While",
          field "do" $ meta "Data.Do",
          field "for" $ meta "Data.For",
          field "forYield" $ meta "Data.ForYield",
          field "new" $ meta "Data.New",
          field "newAnonymous" $ meta "Data.NewAnonymous",
          field "placeholder" $ meta "Data.Placeholder",
          field "eta" $ meta "Data.Eta",
          field "repeated" $ meta "Data.Repeated",
          field "param" $ meta "Data.Param"],
-- object Data {
--   @branch trait Ref extends Data with scala.meta.Ref
      def "Data.Ref" $
        union [
          field "this" $ meta "Data.This",
          field "super" $ meta "Data.Super",
          field "name" $ meta "Data.Name",
          field "anonymous" $ meta "Data.Anonymous",
          field "select" $ meta "Data.Select",
          field "applyUnary" $ meta "Data.ApplyUnary"],
--   @ast class This(qual: scala.meta.Name) extends Data.Ref
      def "Data.This"
        unit,
--   @ast class Super(thisp: scala.meta.Name, superp: scala.meta.Name) extends Data.Ref
      def "Data.Super" $
        record [
          field "thisp" $ meta "Name",
          field "superp" $ meta "Name"],
--   @ast class Name(value: Predef.String @nonEmpty) extends scala.meta.Name with Data.Ref with Pat
      def "Data.Name" $
        record [
          field "value" $ meta "PredefString"],
--   @ast class Anonymous() extends scala.meta.Name with Data.Ref {
      def "Data.Anonymous"
        unit,
--     def value = ""
--     checkParent(ParentChecks.AnonymousImport)
--   }
--   @ast class Select(qual: Data, name: Data.Name) extends Data.Ref with Pat
      def "Data.Select" $
        record [
          field "qual" $ meta "Data",
          field "name" $ meta "Data.Name"],
--   @ast class Interpolate(prefix: Name, parts: List[Lit] @nonEmpty, args: List[Data]) extends Data {
      def "Data.Interpolate" $
        record [
          field "prefix" $ meta "Data.Name",
          field "parts" $ list $ meta "Lit",
          field "args" $ list $ meta "Data"],
--     checkFields(parts.length == args.length + 1)
--   }
--   @ast class Xml(parts: List[Lit] @nonEmpty, args: List[Data]) extends Data {
      def "Data.Xml" $
        record [
          field "parts" $ list $ meta "Lit",
          field "args" $ list $ meta "Data"],
--     checkFields(parts.length == args.length + 1)
--   }
--   @ast class Apply(fun: Data, args: List[Data]) extends Data
      def "Data.Apply" $
        record [
          field "fun" $ meta "Data",
          field "args" $ list $ meta "Data"],
--   @ast class ApplyUsing(fun: Data, args: List[Data]) extends Data
      def "Data.ApplyUsing" $
        record [
          field "fun" $ meta "Data",
          field "targs" $ list $ meta "Data"],
--   @ast class ApplyType(fun: Data, targs: List[Type] @nonEmpty) extends Data
      def "Data.ApplyType" $
        record [
          field "lhs" $ meta "Data",
          field "op" $ meta "Data.Name",
          field "targs" $ list $ meta "Type",
          field "args" $ list $ meta "Data"],
--   @ast class ApplyInfix(lhs: Data, op: Name, targs: List[Type], args: List[Data]) extends Data
      def "Data.ApplyInfix" $
        record [
          field "lhs" $ meta "Data",
          field "op" $ meta "Data.Name",
          field "targs" $ list $ meta "Type",
          field "args" $ list $ meta "Data"],
--   @ast class ApplyUnary(op: Name, arg: Data) extends Data.Ref {
      def "Data.ApplyUnary" $
        record [
          field "op" $ meta "Data.Name",
          field "arg" $ meta "Data"],
--     checkFields(op.isUnaryOp)
--   }
--   @ast class Assign(lhs: Data, rhs: Data) extends Data {
      def "Data.Assign" $
        record [
          field "lhs" $ meta "Data",
          field "rhs" $ meta "Data"],
--     checkFields(lhs.is[Data.Quasi] || lhs.is[Data.Ref] || lhs.is[Data.Apply])
--     checkParent(ParentChecks.DataAssign)
--   }
--   @ast class Return(expr: Data) extends Data
      def "Data.Return" $
        record [
          field "expr" $ meta "Data"],
--   @ast class Throw(expr: Data) extends Data
      def "Data.Throw" $
        record [
          field "expr" $ meta "Data"],
--   @ast class Ascribe(expr: Data, tpe: Type) extends Data
      def "Data.Ascribe" $
        record [
          field "expr" $ meta "Data",
          field "tpe" $ meta "Type"],
--   @ast class Annotate(expr: Data, annots: List[Mod.Annot] @nonEmpty) extends Data
      def "Data.Annotate" $
        record [
          field "expr" $ meta "Data",
          field "annots" $ list $ meta "Mod.Annot"],
--   @ast class Tuple(args: List[Data] @nonEmpty) extends Data {
      def "Data.Tuple" $
        record [
          field "args" $ list $ meta "Data"],
--     // tuple must have more than one element
--     // however, this element may be Quasi with "hidden" list of elements inside
--     checkFields(args.length > 1 || (args.length == 1 && args.head.is[Data.Quasi]))
--   }
--   @ast class Block(stats: List[Stat]) extends Data {
      def "Data.Block" $
        record [
          field "stats" $ list $ meta "Stat"],
--     // extension group block can have declarations without body too
--     checkFields(stats.forall(st => st.isBlockStat || st.is[Decl]))
--   }
--   @ast class EndMarker(name: Data.Name) extends Data
      def "Data.EndMarker" $
        record [
          field "name" $ meta "Data.Name"],
--   @ast class If(cond: Data, thenp: Data, elsep: Data) extends Data {
      def "Data.If" $
        record [
          field "cond" $ meta "Data",
          field "thenp" $ meta "Data",
          field "elsep" $ meta "Data"],
--     @binaryCompatField(since = "4.4.0")
--     private var _mods: List[Mod] = Nil
--   }
--   @ast class QuotedMacroExpr(body: Data) extends Data
      def "Data.QuotedMacroExpr" $
        record [
          field "body" $ meta "Data"],
--   @ast class QuotedMacroType(tpe: Type) extends Data
      def "Data.QuotedMacroType" $
        record [
          field "tpe" $ meta "Type"],
--   @ast class SplicedMacroExpr(body: Data) extends Data
      def "Data.SplicedMacroExpr" $
        record [
          field "body" $ meta "Data"],
--   @ast class Match(expr: Data, cases: List[Case] @nonEmpty) extends Data {
      def "Data.Match" $
        record [
          field "expr" $ meta "Data",
          field "cases" $ list $ meta "Case"],
--     @binaryCompatField(since = "4.4.5")
--     private var _mods: List[Mod] = Nil
--   }
--   @ast class Try(expr: Data, catchp: List[Case], finallyp: Option[Data]) extends Data
      def "Data.Try" $
        record [
          field "expr" $ meta "Data",
          field "catchp" $ list $ meta "Case",
          field "finallyp" $ optional $ meta "Data"],
--   @ast class TryWithHandler(expr: Data, catchp: Data, finallyp: Option[Data]) extends Data
      def "Data.TryWithHandler" $
        record [
          field "expr" $ meta "Data",
          field "catchp" $ meta "Data",
          field "finallyp" $ optional $ meta "Data"],
--
--   @branch trait FunctionData extends Data {
      def "Data.FunctionData" $
        union [
          field "contextFunction" $ meta "Data.ContextFunction",
          field "function" $ meta "Data.Function"],
--     def params: List[Data.Param]
--     def body: Data
--   }
--   @ast class ContextFunction(params: List[Data.Param], body: Data) extends FunctionData {
      def "Data.ContextFunction" $
        record [
          field "params" $ list $ meta "Data.Param",
          field "body" $ meta "Data"],
--     checkFields(
--       params.forall(param =>
--         param.is[Data.Param.Quasi] ||
--           (param.name.is[scala.meta.Name.Anonymous] ==> param.default.isEmpty)
--       )
--     )
--   }
--   @ast class Function(params: List[Data.Param], body: Data) extends FunctionData {
      def "Data.Function" $
        record [
          field "params" $ list $ meta "Data.Param",
          field "body" $ meta "Data"],
--     checkFields(
--       params.forall(param =>
--         param.is[Data.Param.Quasi] ||
--           (param.name.is[scala.meta.Name.Anonymous] ==> param.default.isEmpty)
--       )
--     )
--     checkFields(
--       params.exists(_.is[Data.Param.Quasi]) ||
--         params.exists(_.mods.exists(_.is[Mod.Implicit])) ==> (params.length == 1)
--     )
--   }
--   @ast class PolyFunction(tparams: List[Type.Param], body: Data) extends Data
      def "Data.PolyFunction" $
        record [
          field "tparams" $ list $ meta "Type.Param",
          field "body" $ meta "Data"],
--   @ast class PartialFunction(cases: List[Case] @nonEmpty) extends Data
      def "Data.PartialFunction" $
        record [
          field "cases" $ list $ meta "Case"],
--   @ast class While(expr: Data, body: Data) extends Data
      def "Data.While" $
        record [
          field "expr" $ meta "Data",
          field "body" $ meta "Data"],
--   @ast class Do(body: Data, expr: Data) extends Data
      def "Data.Do" $
        record [
          field "body" $ meta "Data",
          field "expr" $ meta "Data"],
--   @ast class For(enums: List[Enumerator] @nonEmpty, body: Data) extends Data {
      def "Data.For" $
        record [
          field "enums" $ list $ meta "Enumerator"],
--     checkFields(
--       enums.head.is[Enumerator.Generator] || enums.head.is[Enumerator.CaseGenerator] || enums.head
--         .is[Enumerator.Quasi]
--     )
--   }
--   @ast class ForYield(enums: List[Enumerator] @nonEmpty, body: Data) extends Data
      def "Data.ForYield" $
        record [
          field "enums" $ list $ meta "Enumerator"],
--   @ast class New(init: Init) extends Data
      def "Data.New" $
        record [
          field "init" $ meta "Init"],
--   @ast class NewAnonymous(templ: Template) extends Data
      def "Data.NewAnonymous" $
        record [
          field "templ" $ meta "Template"],
--   @ast class Placeholder() extends Data
      def "Data.Placeholder"
        unit,
--   @ast class Eta(expr: Data) extends Data
      def "Data.Eta" $
        record [
          field "expr" $ meta "Data"],
--   @ast class Repeated(expr: Data) extends Data {
      def "Data.Repeated" $
        record [
          field "expr" $ meta "Data"],
--     checkParent(ParentChecks.DataRepeated)
--   }
--   @ast class Param(mods: List[Mod], name: meta.Name, decltpe: Option[Type], default: Option[Data])
--       extends Member
      def "Data.Param" $
        record [
          field "mods" $ list $ meta "Mod",
          field "name" $ meta "Name",
          field "decltpe" $ optional $ meta "Type",
          field "default" $ optional $ meta "Data"],
--   def fresh(): Data.Name = fresh("fresh")
--   def fresh(prefix: String): Data.Name = Data.Name(prefix + Fresh.nextId())
-- }
--
-- @branch trait Type extends Tree
      def "Type" $
        union [
          field "ref" $ meta "Type.Ref",
          field "anonymousName" $ meta "Type.AnonymousName",
          field "apply" $ meta "Type.Apply",
          field "applyInfix" $ meta "Type.ApplyInfix",
          field "functionType" $ meta "Type.FunctionType",
          field "polyFunction" $ meta "Type.PolyFunction",
          field "implicitFunction" $ meta "Type.ImplicitFunction",
          field "tuple" $ meta "Type.Tuple",
          field "with" $ meta "Type.With",
          field "and" $ meta "Type.And",
          field "or" $ meta "Type.Or",
          field "refine" $ meta "Type.Refine",
          field "existential" $ meta "Type.Existential",
          field "annotate" $ meta "Type.Annotate",
          field "lambda" $ meta "Type.Lambda",
          field "macro" $ meta "Type.Macro",
          field "method" $ meta "Type.Method",
          field "placeholder" $ meta "Type.Placeholder",
          field "byName" $ meta "Type.ByName",
          field "repeated" $ meta "Type.Repeated",
          field "var" $ meta "Type.Var",
          field "typedParam" $ meta "Type.TypedParam",
          field "match" $ meta "Type.Match"],
-- object Type {
--   @branch trait Ref extends Type with scala.meta.Ref
      def "Type.Ref" $
        union [
          field "name" $ meta "Type.Name",
          field "select" $ meta "Type.Select",
          field "project" $ meta "Type.Project",
          field "singleton" $ meta "Type.Singleton"],
--   @ast class Name(value: String @nonEmpty) extends scala.meta.Name with Type.Ref
      def "Type.Name" $
        record [
          field "value" string],
--   @ast class AnonymousName() extends Type
      def "Type.AnonymousName"
        unit,
--   @ast class Select(qual: Data.Ref, name: Type.Name) extends Type.Ref {
      def "Type.Select" $
        record [
          field "qual" $ meta "Data.Ref",
          field "name" $ meta "Type.Name"],
--     checkFields(qual.isPath || qual.is[Data.Super] || qual.is[Data.Ref.Quasi])
--   }
--   @ast class Project(qual: Type, name: Type.Name) extends Type.Ref
      def "Type.Project" $
        record [
          field "qual" $ meta "Type",
          field "name" $ meta "Type.Name"],
--   @ast class Singleton(ref: Data.Ref) extends Type.Ref {
      def "Type.Singleton" $
        record [
          field "ref" $ meta "Data.Ref"],
--     checkFields(ref.isPath || ref.is[Data.Super])
--   }
--   @ast class Apply(tpe: Type, args: List[Type] @nonEmpty) extends Type
      def "Type.Apply" $
        record [
          field "tpe" $ meta "Type",
          field "args" $ list $ meta "Type"],
--   @ast class ApplyInfix(lhs: Type, op: Name, rhs: Type) extends Type
      def "Type.ApplyInfix" $
        record [
          field "lhs" $ meta "Type",
          field "op" $ meta "Type.Name",
          field "rhs" $ meta "Type"],
--   @branch trait FunctionType extends Type {
      def "Type.FunctionType" $
        union [
          field "function" $ meta "Type.Function",
          field "contextFunction" $ meta "Type.ContextFunction"],
--     def params: List[Type]
--     def res: Type
--   }
--   @ast class Function(params: List[Type], res: Type) extends FunctionType
      def "Type.Function" $
        record [
          field "params" $ list $ meta "Type",
          field "res" $ meta "Type"],
--   @ast class PolyFunction(tparams: List[Type.Param], tpe: Type) extends Type
      def "Type.PolyFunction" $
        record [
          field "tparams" $ list $ meta "Type.Param",
          field "tpe" $ meta "Type"],
--   @ast class ContextFunction(params: List[Type], res: Type) extends FunctionType
      def "Type.ContextFunction" $
        record [
          field "params" $ list $ meta "Type",
          field "res" $ meta "Type"],
--   @ast @deprecated("Implicit functions are not supported in any dialect")
--   class ImplicitFunction(
      def "Type.ImplicitFunction" $
        record [
--       params: List[Type],
          field "params" $ list $ meta "Type",
--       res: Type
          field "res" $ meta "Type"],
--   ) extends Type
--   @ast class Tuple(args: List[Type] @nonEmpty) extends Type {
      def "Type.Tuple" $
        record [
          field "args" $ list $ meta "Type"],
--     checkFields(args.length > 1 || (args.length == 1 && args.head.is[Type.Quasi]))
--   }
--   @ast class With(lhs: Type, rhs: Type) extends Type
      def "Type.With" $
        record [
          field "lhs" $ meta "Type",
          field "rhs" $ meta "Type"],
--   @ast class And(lhs: Type, rhs: Type) extends Type
      def "Type.And" $
        record [
          field "lhs" $ meta "Type",
          field "rhs" $ meta "Type"],
--   @ast class Or(lhs: Type, rhs: Type) extends Type
      def "Type.Or" $
        record [
          field "lhs" $ meta "Type",
          field "rhs" $ meta "Type"],
--   @ast class Refine(tpe: Option[Type], stats: List[Stat]) extends Type {
      def "Type.Refine" $
        record [
          field "tpe" $ optional $ meta "Type",
          field "stats" $ list $ meta "Stat"],
--     checkFields(stats.forall(_.isRefineStat))
--   }
--   @ast class Existential(tpe: Type, stats: List[Stat] @nonEmpty) extends Type {
      def "Type.Existential" $
        record [
          field "tpe" $ meta "Type",
          field "stats" $ list $ meta "Stat"],
--     checkFields(stats.forall(_.isExistentialStat))
--   }
--   @ast class Annotate(tpe: Type, annots: List[Mod.Annot] @nonEmpty) extends Type
      def "Type.Annotate" $
        record [
          field "tpe" $ meta "Type",
          field "annots" $ list $ meta "Mod.Annot"],
--   @ast class Lambda(tparams: List[Type.Param], tpe: Type) extends Type {
      def "Type.Lambda" $
        record [
          field "tparams" $ list $ meta "Type.Param",
          field "tpe" $ meta "Type"],
--     checkParent(ParentChecks.LambdaType)
--   }
--   @ast class Macro(body: Data) extends Type
      def "Type.Macro" $
        record [
          field "body" $ meta "Data"],
--   @deprecated("Method type syntax is no longer supported in any dialect", "4.4.3")
--   @ast class Method(paramss: List[List[Data.Param]], tpe: Type) extends Type {
      def "Type.Method" $
        record [
          field "paramss" $ list $ list $ meta "Data.Param",
          field "tpe" $ meta "Type"],
--     checkParent(ParentChecks.TypeMethod)
--   }
--   @ast class Placeholder(bounds: Bounds) extends Type
      def "Type.Placeholder" $
        record [
          field "bounds" $ meta "Type.Bounds"],
--   @ast class Bounds(lo: Option[Type], hi: Option[Type]) extends Tree
      def "Type.Bounds" $
        record [
          field "lo" $ optional $ meta "Type",
          field "hi" $ optional $ meta "Type"],
--   @ast class ByName(tpe: Type) extends Type {
      def "Type.ByName" $
        record [
          field "tpe" $ meta "Type"],
--     checkParent(ParentChecks.TypeByName)
--   }
--   @ast class Repeated(tpe: Type) extends Type {
      def "Type.Repeated" $
        record [
          field "tpe" $ meta "Type"],
--     checkParent(ParentChecks.TypeRepeated)
--   }
--   @ast class Var(name: Name) extends Type with Member.Type {
      def "Type.Var" $
        record [
          field "name" $ meta "Type.Name"],
--     checkFields(name.value(0).isLower)
--     checkParent(ParentChecks.TypeVar)
--   }
--
--   @ast class TypedParam(name: Name, typ: Type) extends Type with Member.Type
      def "Type.TypedParam" $
        record [
          field "name" $ meta "Name",
          field "typ" $ meta "Type"],
--   @ast class Param(
      def "Type.Param" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: meta.Name,
          field "name" $ meta "Name",
--       tparams: List[Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       tbounds: Type.Bounds,
          field "tbounds" $ list $ meta "Type.Bounds",
--       vbounds: List[Type],
          field "vbounds" $ list $ meta "Type",
--       cbounds: List[Type]
          field "cbounds" $ list $ meta "Type"],
--   ) extends Member
--
--   @ast class Match(tpe: Type, cases: List[TypeCase] @nonEmpty) extends Type
      def "Type.Match" $
        record [
          field "tpe" $ meta "Type",
          field "cases" $ list $ meta "TypeCase"],
--   def fresh(): Type.Name = fresh("fresh")
--   def fresh(prefix: String): Type.Name = Type.Name(prefix + Fresh.nextId())
-- }
--
-- @branch trait Pat extends Tree
      def "Pat" $
        union [
          field "var" $ meta "Pat.Var",
          field "wildcard" unit,
          field "seqWildcard" unit,
          field "bind" $ meta "Pat.Bind",
          field "alternative" $ meta "Pat.Alternative",
          field "tuple" $ meta "Pat.Tuple",
          field "repeated" $ meta "Pat.Repeated",
          field "extract" $ meta "Pat.Extract",
          field "extractInfix" $ meta "Pat.ExtractInfix",
          field "interpolate" $ meta "Pat.Interpolate",
          field "xml" $ meta "Pat.Xml",
          field "typed" $ meta "Pat.Typed",
          field "macro" $ meta "Pat.Macro",
          field "given" $ meta "Pat.Given"],
-- object Pat {
--   @ast class Var(name: scala.meta.Data.Name) extends Pat with Member.Data { @
      def "Pat.Var" $
        record [
          field "name" $ meta "Data.Name"],
--     // NOTE: can't do this check here because of things like `val X = 2`
--     // checkFields(name.value(0).isLower)
--     checkParent(ParentChecks.PatVar)
--   }
--   @ast class Wildcard() extends Pat
--   @ast class SeqWildcard() extends Pat {
--     checkParent(ParentChecks.PatSeqWildcard)
--   }
--   @ast class Bind(lhs: Pat, rhs: Pat) extends Pat {
      def "Pat.Bind" $
        record [
          field "lhs" $ meta "Pat",
          field "rhs" $ meta "Pat"],
--     checkFields(lhs.is[Pat.Var] || lhs.is[Pat.Quasi])
--   }
--   @ast class Alternative(lhs: Pat, rhs: Pat) extends Pat
      def "Pat.Alternative" $
        record [
          field "lhs" $ meta "Pat",
          field "rhs" $ meta "Pat"],
--   @ast class Tuple(args: List[Pat] @nonEmpty) extends Pat {
      def "Pat.Tuple" $
        record [
          field "args" $ list $ meta "Pat"],
--     checkFields(args.length > 1 || (args.length == 1 && args.head.is[Pat.Quasi]))
--   }
--   @ast class Repeated(name: scala.meta.Data.Name) extends Pat
      def "Pat.Repeated" $
        record [
          field "name" $ meta "Data.Name"],
--   @ast class Extract(fun: Data, args: List[Pat]) extends Pat {
      def "Pat.Extract" $
        record [
          field "fun" $ meta "Data",
          field "args" $ list $ meta "Pat"],
--     checkFields(fun.isExtractor)
--   }
--   @ast class ExtractInfix(lhs: Pat, op: Data.Name, rhs: List[Pat]) extends Pat
      def "Pat.ExtractInfix" $
        record [
          field "lhs" $ meta "Pat",
          field "op" $ meta "Data.Name",
          field "rhs" $ list $ meta "Pat"],
--   @ast class Interpolate(prefix: Data.Name, parts: List[Lit] @nonEmpty, args: List[Pat])
      def "Pat.Interpolate" $
        record [
          field "prefix" $ meta "Data.Name",
          field "parts" $ list $ meta "Lit"],
--       extends Pat {
--     checkFields(parts.length == args.length + 1)
--   }
--   @ast class Xml(parts: List[Lit] @nonEmpty, args: List[Pat]) extends Pat {
      def "Pat.Xml" $
        record [
          field "parts" $ list $ meta "Lit",
          field "args" $ list $ meta "Pat"],
--     checkFields(parts.length == args.length + 1)
--   }
--   @ast class Typed(lhs: Pat, rhs: Type) extends Pat {
      def "Pat.Typed" $
        record [
          field "lhs" $ meta "Pat",
          field "rhs" $ meta "Type"],
--     checkFields(!rhs.is[Type.Var] && !rhs.is[Type.Placeholder])
--   }
--   @ast class Macro(body: Data) extends Pat {
      def "Pat.Macro" $
        record [
          field "body" $ meta "Data"],
--     checkFields(body.is[Data.QuotedMacroExpr] || body.is[Data.QuotedMacroType])
--   }
--   @ast class Given(tpe: Type) extends Pat
      def "Pat.Given" $
        record [
          field "tpe" $ meta "Type"],
--   def fresh(): Pat.Var = Pat.Var(Data.fresh())
--   def fresh(prefix: String): Pat.Var = Pat.Var(Data.fresh(prefix))
-- }
--
-- @branch trait Member extends Tree {
      def "Member" $
        union [
          field "term" $ meta "Member.Data",
          field "type" $ meta "Member.Type",
          field "termParam" $ meta "Data.Param",
          field "typeParam" $ meta "Type.Param",
          field "self" $ meta "Self"],
--   def name: Name
-- }
-- object Member {
--   @branch trait Data extends Member {
      def "Member.Data" $
        union [
          field "pkg" $ meta "Pkg",
          field "object" $ meta "Pkg.Object"],
--     def name: scala.meta.Data.Name
--   }
--   @branch trait Type extends Member {
      def "Member.Type" $
        record [
--     def name: scala.meta.Type.Name
          field "name" $ meta "Type.Name"],
--   }
-- }
--
-- @branch trait Decl extends Stat
      def "Decl" $
        union [
          field "val" $ meta "Decl.Val",
          field "var" $ meta "Decl.Var",
          field "def" $ meta "Decl.Def",
          field "type" $ meta "Decl.Type",
          field "given" $ meta "Decl.Given"],
-- object Decl {
--   @ast class Val(mods: List[Mod], pats: List[Pat] @nonEmpty, decltpe: scala.meta.Type) extends Decl
      def "Decl.Val" $
        record [
          field "mods" $ list $ meta "Mod",
          field "pats" $ list $ meta "Pat",
          field "decltpe" $ meta "Type"],
--   @ast class Var(mods: List[Mod], pats: List[Pat] @nonEmpty, decltpe: scala.meta.Type) extends Decl
      def "Decl.Var" $
        record [
          field "mods" $ list $ meta "Mod",
          field "pats" $ list $ meta "Pat",
          field "decltpe" $ meta "Type"],
--   @ast class Def(
      def "Decl.Def" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: Data.Name,
          field "name" $ meta "Data.Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       paramss: List[List[Data.Param]],
          field "paramss" $ list $ list $ meta "Data.Param",
--       decltpe: scala.meta.Type
          field "decltpe" $ meta "Type"],
--   ) extends Decl with Member.Data @
      --   @ast class Type(
      def "Decl.Type" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: scala.meta.Type.Name,
          field "name" $ meta "Type.Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       bounds: scala.meta.Type.Bounds
          field "bounds" $ meta "Type.Bounds"],
--   ) extends Decl with Member.Type
--   @ast class Given(
      def "Decl.Given" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: Data.Name,
          field "name" $ meta "Data.Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       sparams: List[List[Data.Param]],
          field "sparams" $ list $ list $ meta "Data.Param",
--       decltpe: scala.meta.Type
          field "decltpe" $ meta "Type"],
--   ) extends Decl with Member.Data @
-- }
--
-- @branch trait Defn extends Stat
      def "Defn" $
        union [
          field "val" $ meta "Defn.Val",
          field "var" $ meta "Defn.Var",
          field "given" $ meta "Defn.Given",
          field "enum" $ meta "Defn.Enum",
          field "enumCase" $ meta "Defn.EnumCase",
          field "repeatedEnumCase" $ meta "Defn.RepeatedEnumCase",
          field "givenAlias" $ meta "Defn.GivenAlias",
          field "extensionGroup" $ meta "Defn.ExtensionGroup",
          field "def" $ meta "Defn.Def",
          field "macro" $ meta "Defn.Macro",
          field "type" $ meta "Defn.Type",
          field "class" $ meta "Defn.Class",
          field "trait" $ meta "Defn.Trait",
          field "object" $ meta "Defn.Object"],
-- object Defn {
--   @ast class Val(
      def "Defn.Val" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       pats: List[Pat] @nonEmpty,
          field "pats" $ list $ meta "Pat",
--       decltpe: Option[scala.meta.Type],
          field "decltpe" $ optional $ meta "Type",
--       rhs: Data
          field "rhs" $ meta "Data"],
--   ) extends Defn {
--     checkFields(pats.forall(!_.is[Data.Name]))
--   }
--   @ast class Var(
      def "Defn.Var" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       pats: List[Pat] @nonEmpty,
          field "pats" $ list $ meta "Pat",
--       decltpe: Option[scala.meta.Type],
          field "decltpe" $ meta "Type",
--       rhs: Option[Data]
          field "rhs" $ optional $ meta "Data"],
--   ) extends Defn {
--     checkFields(pats.forall(!_.is[Data.Name]))
--     checkFields(decltpe.nonEmpty || rhs.nonEmpty)
--     checkFields(rhs.isEmpty ==> pats.forall(_.is[Pat.Var]))
--   }
--   @ast class Given(
      def "Defn.Given" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: scala.meta.Name,
          field "name" $ meta "Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ list $ meta "Type.Param",
--       sparams: List[List[Data.Param]],
          field "sparams" $ list $ list $ meta "Data.Param",
--       templ: Template
          field "templ" $ meta "Template"],
--   ) extends Defn
--   @ast class Enum(
      def "Defn.Enum" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: scala.meta.Type.Name,
          field "name" $ meta "Type.Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       ctor: Ctor.Primary,
          field "ctor" $ meta "Ctor.Primary",
--       templ: Template
          field "template" $ meta "Template"],
--   ) extends Defn with Member.Type
--   @ast class EnumCase(
      def "Defn.EnumCase" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: Data.Name,
          field "name" $ meta "Data.Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       ctor: Ctor.Primary,
          field "ctor" $ meta "Ctor.Primary",
--       inits: List[Init]
          field "inits" $ list $ meta "Init"],
--   ) extends Defn with Member.Data { @
--     checkParent(ParentChecks.EnumCase)
--   }
--   @ast class RepeatedEnumCase(
      def "Defn.RepeatedEnumCase" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       cases: List[Data.Name]
          field "cases" $ list $ meta "Data.Name"],
--   ) extends Defn {
--     checkParent(ParentChecks.EnumCase)
--   }
--   @ast class GivenAlias(
      def "Defn.GivenAlias" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: scala.meta.Name,
          field "name" $ meta "Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ list $ meta "Type.Param",
--       sparams: List[List[Data.Param]],
          field "sparams" $ list $ list $ meta "Data.Param",
--       decltpe: scala.meta.Type,
          field "decltpe" $ meta "Type",
--       body: Data
          field "body" $ meta "Data"],
--   ) extends Defn
--   @ast class ExtensionGroup(
      def "Defn.ExtensionGroup" $
        record [
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       paramss: List[List[Data.Param]],
          field "parmss" $ list $ list $ meta "Data.Param",
--       body: Stat
          field "body" $ meta "Stat"],
--   ) extends Defn
--   @ast class Def(
      def "Defn.Def" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: Data.Name,
          field "name" $ meta "Data.Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       paramss: List[List[Data.Param]],
          field "paramss" $ list $ list $ meta "Data.Param",
--       decltpe: Option[scala.meta.Type],
          field "decltpe" $ optional $ meta "Type",
--       body: Data
          field "body" $ meta "Data"],
--   ) extends Defn with Member.Data { @
--     checkFields(paramss.forall(onlyLastParamCanBeRepeated))
--   }
--   @ast class Macro(
      def "Defn.Macro" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: Data.Name,
          field "name" $ meta "Data.Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       paramss: List[List[Data.Param]],
          field "paramss" $ list $ list $ meta "Data.Param",
--       decltpe: Option[scala.meta.Type],
          field "decltpe" $ optional $ meta "Type",
--       body: Data
          field "body" $ meta "Data"],
--   ) extends Defn with Member.Data @
--   @ast class Type(
      def "Defn.Type" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: scala.meta.Type.Name,
          field "name" $ meta "Type.Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       body: scala.meta.Type
          field "body" $ meta "Type"],
--   ) extends Defn with Member.Type {
--     @binaryCompatField("4.4.0")
--     private var _bounds: scala.meta.Type.Bounds = scala.meta.Type.Bounds(None, None)
--   }
--   @ast class Class(
      def "Defn.Class" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: scala.meta.Type.Name,
          field "name" $ meta "Type.Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       ctor: Ctor.Primary,
          field "ctor" $ meta "Ctor.Primary",
--       templ: Template
          field "template" $ meta "Template"],
--   ) extends Defn with Member.Type
--   @ast class Trait(
      def "Defn.Trait" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: scala.meta.Type.Name,
          field "name" $ meta "Type.Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       ctor: Ctor.Primary,
          field "ctor" $ meta "Ctor.Primary",
--       templ: Template
          field "template" $ meta "Template"],
--   ) extends Defn with Member.Type {
--     checkFields(templ.is[Template.Quasi] || templ.stats.forall(!_.is[Ctor]))
--   }
--   @ast class Object(mods: List[Mod], name: Data.Name, templ: Template)
      def "Defn.Object" $
        record [
          field "name" $ meta "Data.Name"], --  from Member.Data
--       extends Defn with Member.Data { @
--     checkFields(templ.is[Template.Quasi] || templ.stats.forall(!_.is[Ctor]))
--   }
-- }
--
-- @ast class Pkg(ref: Data.Ref, stats: List[Stat]) extends Member.Data with Stat { @
      def "Pkg" $
        record [
          field "name" $ meta "Data.Name", --  from Member.Data
          field "ref" $ meta "Data.Ref",
          field "stats" $ list $ meta "Stat"],
--   checkFields(ref.isQualId)
--   def name: Data.Name = ref match {
--     case name: Data.Name => name
--     case Data.Select(_, name: Data.Name) => name
--   }
-- }
-- object Pkg {
--   @ast class Object(mods: List[Mod], name: Data.Name, templ: Template)
--       extends Member.Data with Stat { @
      def "Pkg.Object" $
        record [
          field "mods" $ list $ meta "Mod",
          field "name" $ meta "Data.Name",
          field "template" $ meta "Template"],
--     checkFields(templ.is[Template.Quasi] || templ.stats.forall(!_.is[Ctor]))
--   }
-- }
--
-- // NOTE: The names of Ctor.Primary and Ctor.Secondary here is always Name.Anonymous.
-- // While seemingly useless, this name is crucial to one of the key principles behind the semantic API:
-- // "every definition and every reference should carry a name".
-- @branch trait Ctor extends Tree with Member
      def "Ctor" $
        union [
          field "primary" $ meta "Ctor.Primary",
          field "secondary" $ meta "Ctor.Secondary"],
-- object Ctor {
--   @ast class Primary(mods: List[Mod], name: Name, paramss: List[List[Data.Param]]) extends Ctor
      def "Ctor.Primary" $
        record [
          field "mods" $ list $ meta "Mod",
          field "name" $ meta "Name",
          field "paramss" $ list $ list $ meta "Data.Param"],
--   @ast class Secondary(
      def "Ctor.Secondary" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: Name,
          field "name" $ meta "Name",
--       paramss: List[List[Data.Param]] @nonEmpty,
          field "paramss" $ list $ list $ meta "Data.Param",
--       init: Init,
          field "init" $ meta "Init",
--       stats: List[Stat]
          field "stats" $ list $ meta "Stat"],
--   ) extends Ctor with Stat {
--     checkFields(stats.forall(_.isBlockStat))
--   }
-- }
--
-- // NOTE: The name here is always Name.Anonymous.
-- // See comments to Ctor.Primary and Ctor.Secondary for justification.
-- @ast class Init(tpe: Type, name: Name, argss: List[List[Data]]) extends Ref {
      def "Init" $
        record [
          field "tpe" $ meta "Type",
          field "name" $ meta "Name",
          field "argss" $ list $ list $ meta "Data"],
--   checkFields(tpe.isConstructable)
--   checkParent(ParentChecks.Init)
-- }
--
-- @ast class Self(name: Name, decltpe: Option[Type]) extends Member
      def "Self"
        unit,
--
-- @ast class Template(
      def "Template" $
        record [
--     early: List[Stat],
          field "early" $ list $ meta "Stat",
--     inits: List[Init],
          field "inits" $ list $ meta "Init",
--     self: Self,
          field "self" $ meta "Self",
--     stats: List[Stat]
          field "stats" $ list $ meta "Stat"],
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
          field "annot" $ meta "Mod.Annot",
          field "private" $ meta "Mod.Private",
          field "protected" $ meta "Mod.Protected",
          field "implicit" unit,
          field "final" unit,
          field "sealed" unit,
          field "open" unit,
          field "super" unit,
          field "override" unit,
          field "case" unit,
          field "abstract" unit,
          field "covariant" unit,
          field "contravariant" unit,
          field "lazy" unit,
          field "valParam" unit,
          field "varParam" unit,
          field "infix" unit,
          field "inline" unit,
          field "using" unit,
          field "opaque" unit,
          field "transparent" unit],
-- object Mod {
--   @ast class Annot(init: Init) extends Mod {
      def "Mod.Annot" $
        record [
          field "init" $ meta "Init"],
--     @deprecated("Use init instead", "1.9.0")
--     def body = init
--   }
--   @ast class Private(within: Ref) extends Mod {
      def "Mod.Private" $
        record [
          field "within" $ meta "Ref"],
--     checkFields(within.isWithin)
--   }
--   @ast class Protected(within: Ref) extends Mod {
      def "Mod.Protected" $
        record [
          field "within" $ meta "Ref"],
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
          field "generator" $ meta "Enumerator.Generator",
          field "caseGenerator" $ meta "Enumerator.CaseGenerator",
          field "val" $ meta "Enumerator.Val",
          field "guard" $ meta "Enumerator.Guard"],
-- object Enumerator {
--   @ast class Generator(pat: Pat, rhs: Data) extends Enumerator
      def "Enumerator.Generator" $
        record [
          field "pat" $ meta "Pat",
          field "rhs" $ meta "Data"],
--   @ast class CaseGenerator(pat: Pat, rhs: Data) extends Enumerator
      def "Enumerator.CaseGenerator" $
        record [
          field "pat" $ meta "Pat",
          field "rhs" $ meta "Data"],
--   @ast class Val(pat: Pat, rhs: Data) extends Enumerator
      def "Enumerator.Val" $
        record [
          field "pat" $ meta "Pat",
          field "rhs" $ meta "Data"],
--   @ast class Guard(cond: Data) extends Enumerator
      def "Enumerator.Guard" $
        record [
          field "cond" $ meta "Data"],
-- }
--
-- @branch trait ImportExportStat extends Stat {
      def "ImportExportStat" $
        union [
          field "import" $ meta "Import",
          field "export" $ meta "Export"],
--   def importers: List[Importer]
-- }
-- @ast class Import(importers: List[Importer] @nonEmpty) extends ImportExportStat
      def "Import" $
        record [
          field "importers" $ list $ meta "Importer"],
-- @ast class Export(importers: List[Importer] @nonEmpty) extends ImportExportStat
      def "Export" $
        record [
          field "importers" $ list $ meta "Importer"],
--
-- @ast class Importer(ref: Data.Ref, importees: List[Importee] @nonEmpty) extends Tree {
      def "Importer" $
        record [
          field "ref" $ meta "Data.Ref",
          field "importees" $ list $ meta "Importee"],
--   checkFields(ref.isStableId)
-- }
--
-- @branch trait Importee extends Tree with Ref
      def "Importee" $
        union [
          field "wildcard" unit,
          field "given" $ meta "Importee.Given",
          field "givenAll" unit,
          field "name" $ meta "Importee.Name",
          field "rename" $ meta "Importee.Rename",
          field "unimport" $ meta "Importee.Unimport"],
-- object Importee {
--   @ast class Wildcard() extends Importee
--   @ast class Given(tpe: Type) extends Importee
      def "Importee.Given" $
        record [
          field "tpe" $ meta "Type"],
--   @ast class GivenAll() extends Importee
--   @ast class Name(name: scala.meta.Name) extends Importee {
      def "Importee.Name" $
        record [
          field "name" $ meta "Name"],
--     checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
--   }
--   @ast class Rename(name: scala.meta.Name, rename: scala.meta.Name) extends Importee {
      def "Importee.Rename" $
        record [
          field "name" $ meta "Name",
          field "rename" $ meta "Name"],
--     checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
--     checkFields(rename.is[scala.meta.Name.Quasi] || rename.is[scala.meta.Name.Indeterminate])
--   }
--   @ast class Unimport(name: scala.meta.Name) extends Importee {
      def "Importee.Unimport" $
        record [
          field "name" $ meta "Name"],
--     checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
--   }
-- }
--
-- @branch trait CaseTree extends Tree {
      def "CaseTree" $
        union [
          field "case" $ meta "Case",
          field "typeCase" $ meta "TypeCase"],
--   def pat: Tree
--   def body: Tree
-- }
-- @ast class Case(pat: Pat, cond: Option[Data], body: Data) extends CaseTree
      def "Case" $
        record [
          field "pat" $ meta "Pat",
          field "cond" $ optional $ meta "Data",
          field "body" $ meta "Data"],
-- @ast class TypeCase(pat: Type, body: Type) extends CaseTree
      def "TypeCase" $
        record [
          field "pat" $ meta "Type",
          field "body" $ meta "Type"],
--
-- @ast class Source(stats: List[Stat]) extends Tree {
      def "Source" $
        record [
          field "stats" $ list $ meta "Stat"],
--   // NOTE: This validation has been removed to allow dialects with top-level terms.
--   // Ideally, we should push the validation into a dialect-specific prettyprinter when -- 220 is fixed.
--   // checkFields(stats.forall(_.isTopLevelStat))
-- }
--
-- package internal.trees {
--   // NOTE: Quasi is a base trait for a whole bunch of classes.
--   // Every root, branch and ast trait/class among scala.meta trees (except for quasis themselves)
--   // has a corresponding quasi, e.g. Data.Quasi or Type.Quasi.
--   //
--   // Here's how quasis represent unquotes
--   // (XXX below depends on the position where the unquote occurs, e.g. q"$x" will result in Data.Quasi):
--   //   * $x => XXX.Quasi(0, XXX.Name("x"))
--   //   * ..$xs => XXX.Quasi(1, XXX.Quasi(0, XXX.Name("xs"))
--   //   * ...$xss => XXX.Quasi(2, XXX.Quasi(0, XXX.Name("xss"))
--   //   * ..{$fs($args)} => Complex ellipses aren't supported yet
--   @branch trait Quasi extends Tree {
      def "Quasi" --  TODO
        unit]
--     def rank: Int
--     def tree: Tree
--     def pt: Class[_]
--     def become[T <: Quasi: AstInfo]: T
--   }
--
--   @registry object All
-- }
