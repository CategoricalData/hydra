module Hydra.Impl.Haskell.Sources.Ext.Scala.Meta where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard

scalaMetaName = "hydra/ext/scala/meta"

scalaMeta :: Graph Meta
scalaMeta = Graph scalaMetaName elements (const True) hydraCoreName
  where
    def = datatype scalaMetaName
    meta = nominal . qualify scalaMetaName

    elements = [

      def "PredefString" --  See scala/Predef.scala
        ""
        string,

      def "ScalaSymbol" --  See scala/Symbol.scala
        "" $
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
      def "Tree" --  Note: ignoring fields of Tree and InternalTree for now
        "" $
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
      def "Ref"
        "" $
        union [
          field "name" $ meta "Name",
          field "init" $ meta "Init"],
-- @branch trait Stat extends Tree
      def "Stat"
        "" $
        union [
          field "term" $ meta "Term",
          field "decl" $ meta "Decl",
          field "defn" $ meta "Defn",
          field "importExport" $ meta "ImportExportStat"],
--
-- @branch trait Name extends Ref { def value: String }
      def "Name"
        "" $
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
-- @branch trait Lit extends Term with Pat with Type {
      def "Lit"
        "" $
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
-- @branch trait Term extends Stat
      def "Term"
        "" $
        union [
          field "lit" $ meta "Lit",
          field "ref" $ meta "Term.Ref",
          field "interpolate" $ meta "Term.Interpolate",
          field "xml" $ meta "Term.Xml",
          field "apply" $ meta "Term.Apply",
          field "applyUsing" $ meta "Term.ApplyUsing",
          field "applyType" $ meta "Term.ApplyType",
          field "assign" $ meta "Term.Assign",
          field "return" $ meta "Term.Return",
          field "throw" $ meta "Term.Throw",
          field "ascribe" $ meta "Term.Ascribe",
          field "annotate" $ meta "Term.Annotate",
          field "tuple" $ meta "Term.Tuple",
          field "block" $ meta "Term.Block",
          field "endMarker" $ meta "Term.EndMarker",
          field "if" $ meta "Term.If",
          field "quotedMacroExpr" $ meta "Term.QuotedMacroExpr",
          field "quotedMacroType" $ meta "Term.QuotedMacroType",
          field "splicedMacroExpr" $ meta "Term.SplicedMacroExpr",
          field "match" $ meta "Term.Match",
          field "try" $ meta "Term.Try",
          field "tryWithHandler" $ meta "Term.TryWithHandler",
          field "functionTerm" $ meta "Term.FunctionTerm",
          field "polyFunction" $ meta "Term.PolyFunction",
          field "partialFunction" $ meta "Term.PartialFunction",
          field "while" $ meta "Term.While",
          field "do" $ meta "Term.Do",
          field "for" $ meta "Term.For",
          field "forYield" $ meta "Term.ForYield",
          field "new" $ meta "Term.New",
          field "newAnonymous" $ meta "Term.NewAnonymous",
          field "placeholder" $ meta "Term.Placeholder",
          field "eta" $ meta "Term.Eta",
          field "repeated" $ meta "Term.Repeated",
          field "param" $ meta "Term.Param"],
-- object Term {
--   @branch trait Ref extends Term with scala.meta.Ref
      def "Term.Ref"
        "" $
        union [
          field "this" $ meta "Term.This",
          field "super" $ meta "Term.Super",
          field "name" $ meta "Term.Name",
          field "anonymous" $ meta "Term.Anonymous",
          field "select" $ meta "Term.Select",
          field "applyUnary" $ meta "Term.ApplyUnary"],
--   @ast class This(qual: scala.meta.Name) extends Term.Ref
      def "Term.This"
        ""
        unit,
--   @ast class Super(thisp: scala.meta.Name, superp: scala.meta.Name) extends Term.Ref
      def "Term.Super"
        "" $
        record [
          field "thisp" $ meta "Name",
          field "superp" $ meta "Name"],
--   @ast class Name(value: Predef.String @nonEmpty) extends scala.meta.Name with Term.Ref with Pat
      def "Term.Name"
        "" $
        record [
          field "value" $ meta "PredefString"],
--   @ast class Anonymous() extends scala.meta.Name with Term.Ref {
      def "Term.Anonymous"
        ""
        unit,
--     def value = ""
--     checkParent(ParentChecks.AnonymousImport)
--   }
--   @ast class Select(qual: Term, name: Term.Name) extends Term.Ref with Pat
      def "Term.Select"
        "" $
        record [
          field "qual" $ meta "Term",
          field "name" $ meta "Term.Name"],
--   @ast class Interpolate(prefix: Name, parts: List[Lit] @nonEmpty, args: List[Term]) extends Term {
      def "Term.Interpolate"
        "" $
        record [
          field "prefix" $ meta "Term.Name",
          field "parts" $ list $ meta "Lit",
          field "args" $ list $ meta "Term"],
--     checkFields(parts.length == args.length + 1)
--   }
--   @ast class Xml(parts: List[Lit] @nonEmpty, args: List[Term]) extends Term {
      def "Term.Xml"
        "" $
        record [
          field "parts" $ list $ meta "Lit",
          field "args" $ list $ meta "Term"],
--     checkFields(parts.length == args.length + 1)
--   }
--   @ast class Apply(fun: Term, args: List[Term]) extends Term
      def "Term.Apply"
        "" $
        record [
          field "fun" $ meta "Term",
          field "args" $ list $ meta "Term"],
--   @ast class ApplyUsing(fun: Term, args: List[Term]) extends Term
      def "Term.ApplyUsing"
        "" $
        record [
          field "fun" $ meta "Term",
          field "targs" $ list $ meta "Term"],
--   @ast class ApplyType(fun: Term, targs: List[Type] @nonEmpty) extends Term
      def "Term.ApplyType"
        "" $
        record [
          field "lhs" $ meta "Term",
          field "op" $ meta "Term.Name",
          field "targs" $ list $ meta "Type",
          field "args" $ list $ meta "Term"],
--   @ast class ApplyInfix(lhs: Term, op: Name, targs: List[Type], args: List[Term]) extends Term
      def "Term.ApplyInfix"
        "" $
        record [
          field "lhs" $ meta "Term",
          field "op" $ meta "Term.Name",
          field "targs" $ list $ meta "Type",
          field "args" $ list $ meta "Term"],
--   @ast class ApplyUnary(op: Name, arg: Term) extends Term.Ref {
      def "Term.ApplyUnary"
        "" $
        record [
          field "op" $ meta "Term.Name",
          field "arg" $ meta "Term"],
--     checkFields(op.isUnaryOp)
--   }
--   @ast class Assign(lhs: Term, rhs: Term) extends Term {
      def "Term.Assign"
        "" $
        record [
          field "lhs" $ meta "Term",
          field "rhs" $ meta "Term"],
--     checkFields(lhs.is[Term.Quasi] || lhs.is[Term.Ref] || lhs.is[Term.Apply])
--     checkParent(ParentChecks.TermAssign)
--   }
--   @ast class Return(expr: Term) extends Term
      def "Term.Return"
        "" $
        record [
          field "expr" $ meta "Term"],
--   @ast class Throw(expr: Term) extends Term
      def "Term.Throw"
        "" $
        record [
          field "expr" $ meta "Term"],
--   @ast class Ascribe(expr: Term, tpe: Type) extends Term
      def "Term.Ascribe"
        "" $
        record [
          field "expr" $ meta "Term",
          field "tpe" $ meta "Type"],
--   @ast class Annotate(expr: Term, annots: List[Mod.Annot] @nonEmpty) extends Term
      def "Term.Annotate"
        "" $
        record [
          field "expr" $ meta "Term",
          field "annots" $ list $ meta "Mod.Annot"],
--   @ast class Tuple(args: List[Term] @nonEmpty) extends Term {
      def "Term.Tuple"
        "" $
        record [
          field "args" $ list $ meta "Term"],
--     // tuple must have more than one element
--     // however, this element may be Quasi with "hidden" list of elements inside
--     checkFields(args.length > 1 || (args.length == 1 && args.head.is[Term.Quasi]))
--   }
--   @ast class Block(stats: List[Stat]) extends Term {
      def "Term.Block"
        "" $
        record [
          field "stats" $ list $ meta "Stat"],
--     // extension group block can have declarations without body too
--     checkFields(stats.forall(st => st.isBlockStat || st.is[Decl]))
--   }
--   @ast class EndMarker(name: Term.Name) extends Term
      def "Term.EndMarker"
        "" $
        record [
          field "name" $ meta "Term.Name"],
--   @ast class If(cond: Term, thenp: Term, elsep: Term) extends Term {
      def "Term.If"
        "" $
        record [
          field "cond" $ meta "Term",
          field "thenp" $ meta "Term",
          field "elsep" $ meta "Term"],
--     @binaryCompatField(since = "4.4.0")
--     private var _mods: List[Mod] = Nil
--   }
--   @ast class QuotedMacroExpr(body: Term) extends Term
      def "Term.QuotedMacroExpr"
        "" $
        record [
          field "body" $ meta "Term"],
--   @ast class QuotedMacroType(tpe: Type) extends Term
      def "Term.QuotedMacroType"
        "" $
        record [
          field "tpe" $ meta "Type"],
--   @ast class SplicedMacroExpr(body: Term) extends Term
      def "Term.SplicedMacroExpr"
        "" $
        record [
          field "body" $ meta "Term"],
--   @ast class Match(expr: Term, cases: List[Case] @nonEmpty) extends Term {
      def "Term.Match"
        "" $
        record [
          field "expr" $ meta "Term",
          field "cases" $ list $ meta "Case"],
--     @binaryCompatField(since = "4.4.5")
--     private var _mods: List[Mod] = Nil
--   }
--   @ast class Try(expr: Term, catchp: List[Case], finallyp: Option[Term]) extends Term
      def "Term.Try"
        "" $
        record [
          field "expr" $ meta "Term",
          field "catchp" $ list $ meta "Case",
          field "finallyp" $ optional $ meta "Term"],
--   @ast class TryWithHandler(expr: Term, catchp: Term, finallyp: Option[Term]) extends Term
      def "Term.TryWithHandler"
        "" $
        record [
          field "expr" $ meta "Term",
          field "catchp" $ meta "Term",
          field "finallyp" $ optional $ meta "Term"],
--
--   @branch trait FunctionTerm extends Term {
      def "Term.FunctionTerm"
        "" $
        union [
          field "contextFunction" $ meta "Term.ContextFunction",
          field "function" $ meta "Term.Function"],
--     def params: List[Term.Param]
--     def body: Term
--   }
--   @ast class ContextFunction(params: List[Term.Param], body: Term) extends FunctionTerm {
      def "Term.ContextFunction"
        "" $
        record [
          field "params" $ list $ meta "Term.Param",
          field "body" $ meta "Term"],
--     checkFields(
--       params.forall(param =>
--         param.is[Term.Param.Quasi] ||
--           (param.name.is[scala.meta.Name.Anonymous] ==> param.default.isEmpty)
--       )
--     )
--   }
--   @ast class Function(params: List[Term.Param], body: Term) extends FunctionTerm {
      def "Term.Function"
        "" $
        record [
          field "params" $ list $ meta "Term.Param",
          field "body" $ meta "Term"],
--     checkFields(
--       params.forall(param =>
--         param.is[Term.Param.Quasi] ||
--           (param.name.is[scala.meta.Name.Anonymous] ==> param.default.isEmpty)
--       )
--     )
--     checkFields(
--       params.exists(_.is[Term.Param.Quasi]) ||
--         params.exists(_.mods.exists(_.is[Mod.Implicit])) ==> (params.length == 1)
--     )
--   }
--   @ast class PolyFunction(tparams: List[Type.Param], body: Term) extends Term
      def "Term.PolyFunction"
        "" $
        record [
          field "tparams" $ list $ meta "Type.Param",
          field "body" $ meta "Term"],
--   @ast class PartialFunction(cases: List[Case] @nonEmpty) extends Term
      def "Term.PartialFunction"
        "" $
        record [
          field "cases" $ list $ meta "Case"],
--   @ast class While(expr: Term, body: Term) extends Term
      def "Term.While"
        "" $
        record [
          field "expr" $ meta "Term",
          field "body" $ meta "Term"],
--   @ast class Do(body: Term, expr: Term) extends Term
      def "Term.Do"
        "" $
        record [
          field "body" $ meta "Term",
          field "expr" $ meta "Term"],
--   @ast class For(enums: List[Enumerator] @nonEmpty, body: Term) extends Term {
      def "Term.For"
        "" $
        record [
          field "enums" $ list $ meta "Enumerator"],
--     checkFields(
--       enums.head.is[Enumerator.Generator] || enums.head.is[Enumerator.CaseGenerator] || enums.head
--         .is[Enumerator.Quasi]
--     )
--   }
--   @ast class ForYield(enums: List[Enumerator] @nonEmpty, body: Term) extends Term
      def "Term.ForYield"
        "" $
        record [
          field "enums" $ list $ meta "Enumerator"],
--   @ast class New(init: Init) extends Term
      def "Term.New"
        "" $
        record [
          field "init" $ meta "Init"],
--   @ast class NewAnonymous(templ: Template) extends Term
      def "Term.NewAnonymous"
        "" $
        record [
          field "templ" $ meta "Template"],
--   @ast class Placeholder() extends Term
      def "Term.Placeholder"
        ""
        unit,
--   @ast class Eta(expr: Term) extends Term
      def "Term.Eta"
        "" $
        record [
          field "expr" $ meta "Term"],
--   @ast class Repeated(expr: Term) extends Term {
      def "Term.Repeated"
        "" $
        record [
          field "expr" $ meta "Term"],
--     checkParent(ParentChecks.TermRepeated)
--   }
--   @ast class Param(mods: List[Mod], name: meta.Name, decltpe: Option[Type], default: Option[Term])
--       extends Member
      def "Term.Param"
        "" $
        record [
          field "mods" $ list $ meta "Mod",
          field "name" $ meta "Name",
          field "decltpe" $ optional $ meta "Type",
          field "default" $ optional $ meta "Term"],
--   def fresh(): Term.Name = fresh("fresh")
--   def fresh(prefix: String): Term.Name = Term.Name(prefix + Fresh.nextId())
-- }
--
-- @branch trait Type extends Tree
      def "Type"
        "" $
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
      def "Type.Ref"
        "" $
        union [
          field "name" $ meta "Type.Name",
          field "select" $ meta "Type.Select",
          field "project" $ meta "Type.Project",
          field "singleton" $ meta "Type.Singleton"],
--   @ast class Name(value: String @nonEmpty) extends scala.meta.Name with Type.Ref
      def "Type.Name"
        "" $
        record [
          field "value" string],
--   @ast class AnonymousName() extends Type
      def "Type.AnonymousName"
        ""
        unit,
--   @ast class Select(qual: Term.Ref, name: Type.Name) extends Type.Ref {
      def "Type.Select"
        "" $
        record [
          field "qual" $ meta "Term.Ref",
          field "name" $ meta "Type.Name"],
--     checkFields(qual.isPath || qual.is[Term.Super] || qual.is[Term.Ref.Quasi])
--   }
--   @ast class Project(qual: Type, name: Type.Name) extends Type.Ref
      def "Type.Project"
        "" $
        record [
          field "qual" $ meta "Type",
          field "name" $ meta "Type.Name"],
--   @ast class Singleton(ref: Term.Ref) extends Type.Ref {
      def "Type.Singleton"
        "" $
        record [
          field "ref" $ meta "Term.Ref"],
--     checkFields(ref.isPath || ref.is[Term.Super])
--   }
--   @ast class Apply(tpe: Type, args: List[Type] @nonEmpty) extends Type
      def "Type.Apply"
        "" $
        record [
          field "tpe" $ meta "Type",
          field "args" $ list $ meta "Type"],
--   @ast class ApplyInfix(lhs: Type, op: Name, rhs: Type) extends Type
      def "Type.ApplyInfix"
        "" $
        record [
          field "lhs" $ meta "Type",
          field "op" $ meta "Type.Name",
          field "rhs" $ meta "Type"],
--   @branch trait FunctionType extends Type {
      def "Type.FunctionType"
        "" $
        union [
          field "function" $ meta "Type.Function",
          field "contextFunction" $ meta "Type.ContextFunction"],
--     def params: List[Type]
--     def res: Type
--   }
--   @ast class Function(params: List[Type], res: Type) extends FunctionType
      def "Type.Function"
        "" $
        record [
          field "params" $ list $ meta "Type",
          field "res" $ meta "Type"],
--   @ast class PolyFunction(tparams: List[Type.Param], tpe: Type) extends Type
      def "Type.PolyFunction"
        "" $
        record [
          field "tparams" $ list $ meta "Type.Param",
          field "tpe" $ meta "Type"],
--   @ast class ContextFunction(params: List[Type], res: Type) extends FunctionType
      def "Type.ContextFunction"
        "" $
        record [
          field "params" $ list $ meta "Type",
          field "res" $ meta "Type"],
--   @ast @deprecated("Implicit functions are not supported in any dialect")
--   class ImplicitFunction(
      def "Type.ImplicitFunction"
        "" $
        record [
--       params: List[Type],
          field "params" $ list $ meta "Type",
--       res: Type
          field "res" $ meta "Type"],
--   ) extends Type
--   @ast class Tuple(args: List[Type] @nonEmpty) extends Type {
      def "Type.Tuple"
        "" $
        record [
          field "args" $ list $ meta "Type"],
--     checkFields(args.length > 1 || (args.length == 1 && args.head.is[Type.Quasi]))
--   }
--   @ast class With(lhs: Type, rhs: Type) extends Type
      def "Type.With"
        "" $
        record [
          field "lhs" $ meta "Type",
          field "rhs" $ meta "Type"],
--   @ast class And(lhs: Type, rhs: Type) extends Type
      def "Type.And"
        "" $
        record [
          field "lhs" $ meta "Type",
          field "rhs" $ meta "Type"],
--   @ast class Or(lhs: Type, rhs: Type) extends Type
      def "Type.Or"
        "" $
        record [
          field "lhs" $ meta "Type",
          field "rhs" $ meta "Type"],
--   @ast class Refine(tpe: Option[Type], stats: List[Stat]) extends Type {
      def "Type.Refine"
        "" $
        record [
          field "tpe" $ optional $ meta "Type",
          field "stats" $ list $ meta "Stat"],
--     checkFields(stats.forall(_.isRefineStat))
--   }
--   @ast class Existential(tpe: Type, stats: List[Stat] @nonEmpty) extends Type {
      def "Type.Existential"
        "" $
        record [
          field "tpe" $ meta "Type",
          field "stats" $ list $ meta "Stat"],
--     checkFields(stats.forall(_.isExistentialStat))
--   }
--   @ast class Annotate(tpe: Type, annots: List[Mod.Annot] @nonEmpty) extends Type
      def "Type.Annotate"
        "" $
        record [
          field "tpe" $ meta "Type",
          field "annots" $ list $ meta "Mod.Annot"],
--   @ast class Lambda(tparams: List[Type.Param], tpe: Type) extends Type {
      def "Type.Lambda"
        "" $
        record [
          field "tparams" $ list $ meta "Type.Param",
          field "tpe" $ meta "Type"],
--     checkParent(ParentChecks.TypeLambda)
--   }
--   @ast class Macro(body: Term) extends Type
      def "Type.Macro"
        "" $
        record [
          field "body" $ meta "Term"],
--   @deprecated("Method type syntax is no longer supported in any dialect", "4.4.3")
--   @ast class Method(paramss: List[List[Term.Param]], tpe: Type) extends Type {
      def "Type.Method"
        "" $
        record [
          field "paramss" $ list $ list $ meta "Term.Param",
          field "tpe" $ meta "Type"],
--     checkParent(ParentChecks.TypeMethod)
--   }
--   @ast class Placeholder(bounds: Bounds) extends Type
      def "Type.Placeholder"
        "" $
        record [
          field "bounds" $ meta "Type.Bounds"],
--   @ast class Bounds(lo: Option[Type], hi: Option[Type]) extends Tree
      def "Type.Bounds"
        "" $
        record [
          field "lo" $ optional $ meta "Type",
          field "hi" $ optional $ meta "Type"],
--   @ast class ByName(tpe: Type) extends Type {
      def "Type.ByName"
        "" $
        record [
          field "tpe" $ meta "Type"],
--     checkParent(ParentChecks.TypeByName)
--   }
--   @ast class Repeated(tpe: Type) extends Type {
      def "Type.Repeated"
        "" $
        record [
          field "tpe" $ meta "Type"],
--     checkParent(ParentChecks.TypeRepeated)
--   }
--   @ast class Var(name: Name) extends Type with Member.Type {
      def "Type.Var"
        "" $
        record [
          field "name" $ meta "Type.Name"],
--     checkFields(name.value(0).isLower)
--     checkParent(ParentChecks.TypeVar)
--   }
--
--   @ast class TypedParam(name: Name, typ: Type) extends Type with Member.Type
      def "Type.TypedParam"
        "" $
        record [
          field "name" $ meta "Name",
          field "typ" $ meta "Type"],
--   @ast class Param(
      def "Type.Param"
        "" $
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
      def "Type.Match"
        "" $
        record [
          field "tpe" $ meta "Type",
          field "cases" $ list $ meta "TypeCase"],
--   def fresh(): Type.Name = fresh("fresh")
--   def fresh(prefix: String): Type.Name = Type.Name(prefix + Fresh.nextId())
-- }
--
-- @branch trait Pat extends Tree
      def "Pat"
        "" $
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
--   @ast class Var(name: scala.meta.Term.Name) extends Pat with Member.Term { @
      def "Pat.Var"
        "" $
        record [
          field "name" $ meta "Term.Name"],
--     // NOTE: can't do this check here because of things like `val X = 2`
--     // checkFields(name.value(0).isLower)
--     checkParent(ParentChecks.PatVar)
--   }
--   @ast class Wildcard() extends Pat
--   @ast class SeqWildcard() extends Pat {
--     checkParent(ParentChecks.PatSeqWildcard)
--   }
--   @ast class Bind(lhs: Pat, rhs: Pat) extends Pat {
      def "Pat.Bind"
        "" $
        record [
          field "lhs" $ meta "Pat",
          field "rhs" $ meta "Pat"],
--     checkFields(lhs.is[Pat.Var] || lhs.is[Pat.Quasi])
--   }
--   @ast class Alternative(lhs: Pat, rhs: Pat) extends Pat
      def "Pat.Alternative"
        "" $
        record [
          field "lhs" $ meta "Pat",
          field "rhs" $ meta "Pat"],
--   @ast class Tuple(args: List[Pat] @nonEmpty) extends Pat {
      def "Pat.Tuple"
        "" $
        record [
          field "args" $ list $ meta "Pat"],
--     checkFields(args.length > 1 || (args.length == 1 && args.head.is[Pat.Quasi]))
--   }
--   @ast class Repeated(name: scala.meta.Term.Name) extends Pat
      def "Pat.Repeated"
        "" $
        record [
          field "name" $ meta "Term.Name"],
--   @ast class Extract(fun: Term, args: List[Pat]) extends Pat {
      def "Pat.Extract"
        "" $
        record [
          field "fun" $ meta "Term",
          field "args" $ list $ meta "Pat"],
--     checkFields(fun.isExtractor)
--   }
--   @ast class ExtractInfix(lhs: Pat, op: Term.Name, rhs: List[Pat]) extends Pat
      def "Pat.ExtractInfix"
        "" $
        record [
          field "lhs" $ meta "Pat",
          field "op" $ meta "Term.Name",
          field "rhs" $ list $ meta "Pat"],
--   @ast class Interpolate(prefix: Term.Name, parts: List[Lit] @nonEmpty, args: List[Pat])
      def "Pat.Interpolate"
        "" $
        record [
          field "prefix" $ meta "Term.Name",
          field "parts" $ list $ meta "Lit"],
--       extends Pat {
--     checkFields(parts.length == args.length + 1)
--   }
--   @ast class Xml(parts: List[Lit] @nonEmpty, args: List[Pat]) extends Pat {
      def "Pat.Xml"
        "" $
        record [
          field "parts" $ list $ meta "Lit",
          field "args" $ list $ meta "Pat"],
--     checkFields(parts.length == args.length + 1)
--   }
--   @ast class Typed(lhs: Pat, rhs: Type) extends Pat {
      def "Pat.Typed"
        "" $
        record [
          field "lhs" $ meta "Pat",
          field "rhs" $ meta "Type"],
--     checkFields(!rhs.is[Type.Var] && !rhs.is[Type.Placeholder])
--   }
--   @ast class Macro(body: Term) extends Pat {
      def "Pat.Macro"
        "" $
        record [
          field "body" $ meta "Term"],
--     checkFields(body.is[Term.QuotedMacroExpr] || body.is[Term.QuotedMacroType])
--   }
--   @ast class Given(tpe: Type) extends Pat
      def "Pat.Given"
        "" $
        record [
          field "tpe" $ meta "Type"],
--   def fresh(): Pat.Var = Pat.Var(Term.fresh())
--   def fresh(prefix: String): Pat.Var = Pat.Var(Term.fresh(prefix))
-- }
--
-- @branch trait Member extends Tree {
      def "Member"
        "" $
        union [
          field "term" $ meta "Member.Term",
          field "type" $ meta "Member.Type",
          field "termParam" $ meta "Term.Param",
          field "typeParam" $ meta "Type.Param",
          field "self" $ meta "Self"],
--   def name: Name
-- }
-- object Member {
--   @branch trait Term extends Member {
      def "Member.Term"
        "" $
        union [
          field "pkg" $ meta "Pkg",
          field "object" $ meta "Pkg.Object"],
--     def name: scala.meta.Term.Name
--   }
--   @branch trait Type extends Member {
      def "Member.Type"
        "" $
        record [
--     def name: scala.meta.Type.Name
          field "name" $ meta "Type.Name"],
--   }
-- }
--
-- @branch trait Decl extends Stat
      def "Decl"
        "" $
        union [
          field "val" $ meta "Decl.Val",
          field "var" $ meta "Decl.Var",
          field "def" $ meta "Decl.Def",
          field "type" $ meta "Decl.Type",
          field "given" $ meta "Decl.Given"],
-- object Decl {
--   @ast class Val(mods: List[Mod], pats: List[Pat] @nonEmpty, decltpe: scala.meta.Type) extends Decl
      def "Decl.Val"
        "" $
        record [
          field "mods" $ list $ meta "Mod",
          field "pats" $ list $ meta "Pat",
          field "decltpe" $ meta "Type"],
--   @ast class Var(mods: List[Mod], pats: List[Pat] @nonEmpty, decltpe: scala.meta.Type) extends Decl
      def "Decl.Var"
        "" $
        record [
          field "mods" $ list $ meta "Mod",
          field "pats" $ list $ meta "Pat",
          field "decltpe" $ meta "Type"],
--   @ast class Def(
      def "Decl.Def"
        "" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: Term.Name,
          field "name" $ meta "Term.Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       paramss: List[List[Term.Param]],
          field "paramss" $ list $ list $ meta "Term.Param",
--       decltpe: scala.meta.Type
          field "decltpe" $ meta "Type"],
--   ) extends Decl with Member.Term @
      --   @ast class Type(
      def "Decl.Type"
        "" $
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
      def "Decl.Given"
        "" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: Term.Name,
          field "name" $ meta "Term.Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       sparams: List[List[Term.Param]],
          field "sparams" $ list $ list $ meta "Term.Param",
--       decltpe: scala.meta.Type
          field "decltpe" $ meta "Type"],
--   ) extends Decl with Member.Term @
-- }
--
-- @branch trait Defn extends Stat
      def "Defn"
        "" $
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
      def "Defn.Val"
        "" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       pats: List[Pat] @nonEmpty,
          field "pats" $ list $ meta "Pat",
--       decltpe: Option[scala.meta.Type],
          field "decltpe" $ optional $ meta "Type",
--       rhs: Term
          field "rhs" $ meta "Term"],
--   ) extends Defn {
--     checkFields(pats.forall(!_.is[Term.Name]))
--   }
--   @ast class Var(
      def "Defn.Var"
        "" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       pats: List[Pat] @nonEmpty,
          field "pats" $ list $ meta "Pat",
--       decltpe: Option[scala.meta.Type],
          field "decltpe" $ meta "Type",
--       rhs: Option[Term]
          field "rhs" $ optional $ meta "Term"],
--   ) extends Defn {
--     checkFields(pats.forall(!_.is[Term.Name]))
--     checkFields(decltpe.nonEmpty || rhs.nonEmpty)
--     checkFields(rhs.isEmpty ==> pats.forall(_.is[Pat.Var]))
--   }
--   @ast class Given(
      def "Defn.Given"
        "" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: scala.meta.Name,
          field "name" $ meta "Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ list $ meta "Type.Param",
--       sparams: List[List[Term.Param]],
          field "sparams" $ list $ list $ meta "Term.Param",
--       templ: Template
          field "templ" $ meta "Template"],
--   ) extends Defn
--   @ast class Enum(
      def "Defn.Enum"
        "" $
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
      def "Defn.EnumCase"
        "" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: Term.Name,
          field "name" $ meta "Term.Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       ctor: Ctor.Primary,
          field "ctor" $ meta "Ctor.Primary",
--       inits: List[Init]
          field "inits" $ list $ meta "Init"],
--   ) extends Defn with Member.Term { @
--     checkParent(ParentChecks.EnumCase)
--   }
--   @ast class RepeatedEnumCase(
      def "Defn.RepeatedEnumCase"
        "" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       cases: List[Term.Name]
          field "cases" $ list $ meta "Term.Name"],
--   ) extends Defn {
--     checkParent(ParentChecks.EnumCase)
--   }
--   @ast class GivenAlias(
      def "Defn.GivenAlias"
        "" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: scala.meta.Name,
          field "name" $ meta "Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ list $ meta "Type.Param",
--       sparams: List[List[Term.Param]],
          field "sparams" $ list $ list $ meta "Term.Param",
--       decltpe: scala.meta.Type,
          field "decltpe" $ meta "Type",
--       body: Term
          field "body" $ meta "Term"],
--   ) extends Defn
--   @ast class ExtensionGroup(
      def "Defn.ExtensionGroup"
        "" $
        record [
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       paramss: List[List[Term.Param]],
          field "parmss" $ list $ list $ meta "Term.Param",
--       body: Stat
          field "body" $ meta "Stat"],
--   ) extends Defn
--   @ast class Def(
      def "Defn.Def"
        "" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: Term.Name,
          field "name" $ meta "Term.Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       paramss: List[List[Term.Param]],
          field "paramss" $ list $ list $ meta "Term.Param",
--       decltpe: Option[scala.meta.Type],
          field "decltpe" $ optional $ meta "Type",
--       body: Term
          field "body" $ meta "Term"],
--   ) extends Defn with Member.Term { @
--     checkFields(paramss.forall(onlyLastParamCanBeRepeated))
--   }
--   @ast class Macro(
      def "Defn.Macro"
        "" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: Term.Name,
          field "name" $ meta "Term.Name",
--       tparams: List[scala.meta.Type.Param],
          field "tparams" $ list $ meta "Type.Param",
--       paramss: List[List[Term.Param]],
          field "paramss" $ list $ list $ meta "Term.Param",
--       decltpe: Option[scala.meta.Type],
          field "decltpe" $ optional $ meta "Type",
--       body: Term
          field "body" $ meta "Term"],
--   ) extends Defn with Member.Term @
--   @ast class Type(
      def "Defn.Type"
        "" $
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
      def "Defn.Class"
        "" $
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
      def "Defn.Trait"
        "" $
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
--   @ast class Object(mods: List[Mod], name: Term.Name, templ: Template)
      def "Defn.Object"
        "" $
        record [
          field "name" $ meta "Term.Name"], --  from Member.Term
--       extends Defn with Member.Term { @
--     checkFields(templ.is[Template.Quasi] || templ.stats.forall(!_.is[Ctor]))
--   }
-- }
--
-- @ast class Pkg(ref: Term.Ref, stats: List[Stat]) extends Member.Term with Stat { @
      def "Pkg"
        "" $
        record [
          field "name" $ meta "Term.Name", --  from Member.Term
          field "ref" $ meta "Term.Ref",
          field "stats" $ list $ meta "Stat"],
--   checkFields(ref.isQualId)
--   def name: Term.Name = ref match {
--     case name: Term.Name => name
--     case Term.Select(_, name: Term.Name) => name
--   }
-- }
-- object Pkg {
--   @ast class Object(mods: List[Mod], name: Term.Name, templ: Template)
--       extends Member.Term with Stat { @
      def "Pkg.Object"
        "" $
        record [
          field "mods" $ list $ meta "Mod",
          field "name" $ meta "Term.Name",
          field "template" $ meta "Template"],
--     checkFields(templ.is[Template.Quasi] || templ.stats.forall(!_.is[Ctor]))
--   }
-- }
--
-- // NOTE: The names of Ctor.Primary and Ctor.Secondary here is always Name.Anonymous.
-- // While seemingly useless, this name is crucial to one of the key principles behind the semantic API:
-- // "every definition and every reference should carry a name".
-- @branch trait Ctor extends Tree with Member
      def "Ctor"
        "" $
        union [
          field "primary" $ meta "Ctor.Primary",
          field "secondary" $ meta "Ctor.Secondary"],
-- object Ctor {
--   @ast class Primary(mods: List[Mod], name: Name, paramss: List[List[Term.Param]]) extends Ctor
      def "Ctor.Primary"
        "" $
        record [
          field "mods" $ list $ meta "Mod",
          field "name" $ meta "Name",
          field "paramss" $ list $ list $ meta "Term.Param"],
--   @ast class Secondary(
      def "Ctor.Secondary"
        "" $
        record [
--       mods: List[Mod],
          field "mods" $ list $ meta "Mod",
--       name: Name,
          field "name" $ meta "Name",
--       paramss: List[List[Term.Param]] @nonEmpty,
          field "paramss" $ list $ list $ meta "Term.Param",
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
-- @ast class Init(tpe: Type, name: Name, argss: List[List[Term]]) extends Ref {
      def "Init"
        "" $
        record [
          field "tpe" $ meta "Type",
          field "name" $ meta "Name",
          field "argss" $ list $ list $ meta "Term"],
--   checkFields(tpe.isConstructable)
--   checkParent(ParentChecks.Init)
-- }
--
-- @ast class Self(name: Name, decltpe: Option[Type]) extends Member
      def "Self"
        ""
        unit,
--
-- @ast class Template(
      def "Template"
        "" $
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
      def "Mod"
        "" $
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
      def "Mod.Annot"
        "" $
        record [
          field "init" $ meta "Init"],
--     @deprecated("Use init instead", "1.9.0")
--     def body = init
--   }
--   @ast class Private(within: Ref) extends Mod {
      def "Mod.Private"
        "" $
        record [
          field "within" $ meta "Ref"],
--     checkFields(within.isWithin)
--   }
--   @ast class Protected(within: Ref) extends Mod {
      def "Mod.Protected"
        "" $
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
      def "Enumerator"
        "" $
        union [
          field "generator" $ meta "Enumerator.Generator",
          field "caseGenerator" $ meta "Enumerator.CaseGenerator",
          field "val" $ meta "Enumerator.Val",
          field "guard" $ meta "Enumerator.Guard"],
-- object Enumerator {
--   @ast class Generator(pat: Pat, rhs: Term) extends Enumerator
      def "Enumerator.Generator"
        "" $
        record [
          field "pat" $ meta "Pat",
          field "rhs" $ meta "Term"],
--   @ast class CaseGenerator(pat: Pat, rhs: Term) extends Enumerator
      def "Enumerator.CaseGenerator"
        "" $
        record [
          field "pat" $ meta "Pat",
          field "rhs" $ meta "Term"],
--   @ast class Val(pat: Pat, rhs: Term) extends Enumerator
      def "Enumerator.Val"
        "" $
        record [
          field "pat" $ meta "Pat",
          field "rhs" $ meta "Term"],
--   @ast class Guard(cond: Term) extends Enumerator
      def "Enumerator.Guard"
        "" $
        record [
          field "cond" $ meta "Term"],
-- }
--
-- @branch trait ImportExportStat extends Stat {
      def "ImportExportStat"
        "" $
        union [
          field "import" $ meta "Import",
          field "export" $ meta "Export"],
--   def importers: List[Importer]
-- }
-- @ast class Import(importers: List[Importer] @nonEmpty) extends ImportExportStat
      def "Import"
        "" $
        record [
          field "importers" $ list $ meta "Importer"],
-- @ast class Export(importers: List[Importer] @nonEmpty) extends ImportExportStat
      def "Export"
        "" $
        record [
          field "importers" $ list $ meta "Importer"],
--
-- @ast class Importer(ref: Term.Ref, importees: List[Importee] @nonEmpty) extends Tree {
      def "Importer"
        "" $
        record [
          field "ref" $ meta "Term.Ref",
          field "importees" $ list $ meta "Importee"],
--   checkFields(ref.isStableId)
-- }
--
-- @branch trait Importee extends Tree with Ref
      def "Importee"
        "" $
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
      def "Importee.Given"
        "" $
        record [
          field "tpe" $ meta "Type"],
--   @ast class GivenAll() extends Importee
--   @ast class Name(name: scala.meta.Name) extends Importee {
      def "Importee.Name"
        "" $
        record [
          field "name" $ meta "Name"],
--     checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
--   }
--   @ast class Rename(name: scala.meta.Name, rename: scala.meta.Name) extends Importee {
      def "Importee.Rename"
        "" $
        record [
          field "name" $ meta "Name",
          field "rename" $ meta "Name"],
--     checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
--     checkFields(rename.is[scala.meta.Name.Quasi] || rename.is[scala.meta.Name.Indeterminate])
--   }
--   @ast class Unimport(name: scala.meta.Name) extends Importee {
      def "Importee.Unimport"
        "" $
        record [
          field "name" $ meta "Name"],
--     checkFields(name.is[scala.meta.Name.Quasi] || name.is[scala.meta.Name.Indeterminate])
--   }
-- }
--
-- @branch trait CaseTree extends Tree {
      def "CaseTree"
        "" $
        union [
          field "case" $ meta "Case",
          field "typeCase" $ meta "TypeCase"],
--   def pat: Tree
--   def body: Tree
-- }
-- @ast class Case(pat: Pat, cond: Option[Term], body: Term) extends CaseTree
      def "Case"
        "" $
        record [
          field "pat" $ meta "Pat",
          field "cond" $ optional $ meta "Term",
          field "body" $ meta "Term"],
-- @ast class TypeCase(pat: Type, body: Type) extends CaseTree
      def "TypeCase"
        "" $
        record [
          field "pat" $ meta "Type",
          field "body" $ meta "Type"],
--
-- @ast class Source(stats: List[Stat]) extends Tree {
      def "Source"
        "" $
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
--   // has a corresponding quasi, e.g. Term.Quasi or Type.Quasi.
--   //
--   // Here's how quasis represent unquotes
--   // (XXX below depends on the position where the unquote occurs, e.g. q"$x" will result in Term.Quasi):
--   //   * $x => XXX.Quasi(0, XXX.Name("x"))
--   //   * ..$xs => XXX.Quasi(1, XXX.Quasi(0, XXX.Name("xs"))
--   //   * ...$xss => XXX.Quasi(2, XXX.Quasi(0, XXX.Name("xss"))
--   //   * ..{$fs($args)} => Complex ellipses aren't supported yet
--   @branch trait Quasi extends Tree {
      def "Quasi" --  TODO
        ""
        unit]
--     def rank: Int
--     def tree: Tree
--     def pt: Class[_]
--     def become[T <: Quasi: AstInfo]: T
--   }
--
--   @registry object All
-- }
