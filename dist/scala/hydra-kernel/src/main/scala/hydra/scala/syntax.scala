package hydra.scala.syntax

import hydra.core.*

type PredefString = scala.Predef.String

case class ScalaSymbol(name: scala.Predef.String)

enum Tree :
   case ref(value: hydra.scala.syntax.Ref) extends Tree
   case stat(value: hydra.scala.syntax.Stat) extends Tree
   case `type`(value: hydra.scala.syntax.Type) extends Tree
   case bounds(value: hydra.scala.syntax.TypeBounds) extends Tree
   case pat(value: hydra.scala.syntax.Pat) extends Tree
   case member(value: hydra.scala.syntax.Member) extends Tree
   case ctor(value: hydra.scala.syntax.Ctor) extends Tree
   case template(value: hydra.scala.syntax.Template) extends Tree
   case mod(value: hydra.scala.syntax.Mod) extends Tree
   case enumerator(value: hydra.scala.syntax.Enumerator) extends Tree
   case importer(value: hydra.scala.syntax.Importer) extends Tree
   case importee(value: hydra.scala.syntax.Importee) extends Tree
   case caseTree(value: hydra.scala.syntax.CaseTree) extends Tree
   case source(value: hydra.scala.syntax.Source) extends Tree
   case quasi(value: hydra.scala.syntax.Quasi) extends Tree

enum Ref :
   case name(value: hydra.scala.syntax.Name) extends Ref
   case init(value: hydra.scala.syntax.Init) extends Ref

enum Stat :
   case term(value: hydra.scala.syntax.Data) extends Stat
   case decl(value: hydra.scala.syntax.Decl) extends Stat
   case defn(value: hydra.scala.syntax.Defn) extends Stat
   case importExport(value: hydra.scala.syntax.ImportExportStat) extends Stat

enum Name :
   case value(value: scala.Predef.String) extends Name
   case anonymous extends Name
   case indeterminate(value: hydra.scala.syntax.PredefString) extends Name

enum Lit :
   case `null` extends Lit
   case int(value: Int) extends Lit
   case double(value: Double) extends Lit
   case float(value: Float) extends Lit
   case byte(value: Byte) extends Lit
   case short(value: Short) extends Lit
   case char(value: Int) extends Lit
   case long(value: Long) extends Lit
   case boolean(value: Boolean) extends Lit
   case unit extends Lit
   case string(value: scala.Predef.String) extends Lit
   case bytes(value: Seq[Int]) extends Lit
   case symbol(value: hydra.scala.syntax.ScalaSymbol) extends Lit

enum Data :
   case lit(value: hydra.scala.syntax.Lit) extends Data
   case ref(value: hydra.scala.syntax.Data_Ref) extends Data
   case interpolate(value: hydra.scala.syntax.Data_Interpolate) extends Data
   case xml(value: hydra.scala.syntax.Data_Xml) extends Data
   case apply(value: hydra.scala.syntax.Data_Apply) extends Data
   case applyUsing(value: hydra.scala.syntax.Data_ApplyUsing) extends Data
   case applyType(value: hydra.scala.syntax.Data_ApplyType) extends Data
   case assign(value: hydra.scala.syntax.Data_Assign) extends Data
   case `return`(value: hydra.scala.syntax.Data_Return) extends Data
   case `throw`(value: hydra.scala.syntax.Data_Throw) extends Data
   case ascribe(value: hydra.scala.syntax.Data_Ascribe) extends Data
   case annotate(value: hydra.scala.syntax.Data_Annotate) extends Data
   case tuple(value: hydra.scala.syntax.Data_Tuple) extends Data
   case block(value: hydra.scala.syntax.Data_Block) extends Data
   case endMarker(value: hydra.scala.syntax.Data_EndMarker) extends Data
   case `if`(value: hydra.scala.syntax.Data_If) extends Data
   case quotedMacroExpr(value: hydra.scala.syntax.Data_QuotedMacroExpr) extends Data
   case quotedMacroType(value: hydra.scala.syntax.Data_QuotedMacroType) extends Data
   case splicedMacroExpr(value: hydra.scala.syntax.Data_SplicedMacroExpr) extends Data
   case `match`(value: hydra.scala.syntax.Data_Match) extends Data
   case `try`(value: hydra.scala.syntax.Data_Try) extends Data
   case tryWithHandler(value: hydra.scala.syntax.Data_TryWithHandler) extends Data
   case functionData(value: hydra.scala.syntax.Data_FunctionData) extends Data
   case polyFunction(value: hydra.scala.syntax.Data_PolyFunction) extends Data
   case partialFunction(value: hydra.scala.syntax.Data_PartialFunction) extends Data
   case `while`(value: hydra.scala.syntax.Data_While) extends Data
   case `do`(value: hydra.scala.syntax.Data_Do) extends Data
   case `for`(value: hydra.scala.syntax.Data_For) extends Data
   case forYield(value: hydra.scala.syntax.Data_ForYield) extends Data
   case `new`(value: hydra.scala.syntax.Data_New) extends Data
   case newAnonymous(value: hydra.scala.syntax.Data_NewAnonymous) extends Data
   case placeholder extends Data
   case eta(value: hydra.scala.syntax.Data_Eta) extends Data
   case repeated(value: hydra.scala.syntax.Data_Repeated) extends Data
   case param(value: hydra.scala.syntax.Data_Param) extends Data

enum Data_Ref :
   case `this`(value: hydra.scala.syntax.Data_This) extends Data_Ref
   case `super`(value: hydra.scala.syntax.Data_Super) extends Data_Ref
   case name(value: hydra.scala.syntax.Data_Name) extends Data_Ref
   case anonymous(value: hydra.scala.syntax.Data_Anonymous) extends Data_Ref
   case select(value: hydra.scala.syntax.Data_Select) extends Data_Ref
   case applyUnary(value: hydra.scala.syntax.Data_ApplyUnary) extends Data_Ref

type Data_This = Unit

case class Data_Super(thisp: hydra.scala.syntax.Name, superp: hydra.scala.syntax.Name)

case class Data_Name(value: hydra.scala.syntax.PredefString)

type Data_Anonymous = Unit

case class Data_Select(qual: hydra.scala.syntax.Data, name: hydra.scala.syntax.Data_Name)

case class Data_Interpolate(prefix: hydra.scala.syntax.Data_Name, parts: Seq[hydra.scala.syntax.Lit],
   args: Seq[hydra.scala.syntax.Data])

case class Data_Xml(parts: Seq[hydra.scala.syntax.Lit], args: Seq[hydra.scala.syntax.Data])

case class Data_Apply(fun: hydra.scala.syntax.Data, args: Seq[hydra.scala.syntax.Data])

case class Data_ApplyUsing(fun: hydra.scala.syntax.Data, targs: Seq[hydra.scala.syntax.Data])

case class Data_ApplyType(lhs: hydra.scala.syntax.Data, op: hydra.scala.syntax.Data_Name,
   targs: Seq[hydra.scala.syntax.Type], args: Seq[hydra.scala.syntax.Data])

case class Data_ApplyInfix(lhs: hydra.scala.syntax.Data, op: hydra.scala.syntax.Data_Name,
   targs: Seq[hydra.scala.syntax.Type], args: Seq[hydra.scala.syntax.Data])

case class Data_ApplyUnary(op: hydra.scala.syntax.Data_Name, arg: hydra.scala.syntax.Data)

case class Data_Assign(lhs: hydra.scala.syntax.Data, rhs: hydra.scala.syntax.Data)

case class Data_Return(expr: hydra.scala.syntax.Data)

case class Data_Throw(expr: hydra.scala.syntax.Data)

case class Data_Ascribe(expr: hydra.scala.syntax.Data, tpe: hydra.scala.syntax.Type)

case class Data_Annotate(expr: hydra.scala.syntax.Data, annots: Seq[hydra.scala.syntax.Mod_Annot])

case class Data_Tuple(args: Seq[hydra.scala.syntax.Data])

case class Data_Block(stats: Seq[hydra.scala.syntax.Stat])

case class Data_EndMarker(name: hydra.scala.syntax.Data_Name)

case class Data_If(cond: hydra.scala.syntax.Data, thenp: hydra.scala.syntax.Data, elsep: hydra.scala.syntax.Data)

case class Data_QuotedMacroExpr(body: hydra.scala.syntax.Data)

case class Data_QuotedMacroType(tpe: hydra.scala.syntax.Type)

case class Data_SplicedMacroExpr(body: hydra.scala.syntax.Data)

case class Data_Match(expr: hydra.scala.syntax.Data, cases: Seq[hydra.scala.syntax.Case])

case class Data_Try(expr: hydra.scala.syntax.Data, catchp: Seq[hydra.scala.syntax.Case],
   finallyp: Option[hydra.scala.syntax.Data])

case class Data_TryWithHandler(expr: hydra.scala.syntax.Data, catchp: hydra.scala.syntax.Data,
   finallyp: Option[hydra.scala.syntax.Data])

enum Data_FunctionData :
   case contextFunction(value: hydra.scala.syntax.Data_ContextFunction) extends Data_FunctionData
   case function(value: hydra.scala.syntax.Data_Function) extends Data_FunctionData

case class Data_ContextFunction(params: Seq[hydra.scala.syntax.Data_Param], body: hydra.scala.syntax.Data)

case class Data_Function(params: Seq[hydra.scala.syntax.Data_Param], body: hydra.scala.syntax.Data)

case class Data_PolyFunction(tparams: Seq[hydra.scala.syntax.Type_Param], body: hydra.scala.syntax.Data)

case class Data_PartialFunction(cases: Seq[hydra.scala.syntax.Case])

case class Data_While(expr: hydra.scala.syntax.Data, body: hydra.scala.syntax.Data)

case class Data_Do(body: hydra.scala.syntax.Data, expr: hydra.scala.syntax.Data)

case class Data_For(enums: Seq[hydra.scala.syntax.Enumerator])

case class Data_ForYield(enums: Seq[hydra.scala.syntax.Enumerator])

case class Data_New(init: hydra.scala.syntax.Init)

case class Data_NewAnonymous(templ: hydra.scala.syntax.Template)

case class Data_Eta(expr: hydra.scala.syntax.Data)

case class Data_Repeated(expr: hydra.scala.syntax.Data)

case class Data_Param(mods: Seq[hydra.scala.syntax.Mod], name: hydra.scala.syntax.Name,
   decltpe: Option[hydra.scala.syntax.Type], default: Option[hydra.scala.syntax.Data])

enum Type :
   case ref(value: hydra.scala.syntax.Type_Ref) extends Type
   case anonymousName(value: hydra.scala.syntax.Type_AnonymousName) extends Type
   case apply(value: hydra.scala.syntax.Type_Apply) extends Type
   case applyInfix(value: hydra.scala.syntax.Type_ApplyInfix) extends Type
   case functionType(value: hydra.scala.syntax.Type_FunctionType) extends Type
   case polyFunction(value: hydra.scala.syntax.Type_PolyFunction) extends Type
   case implicitFunction(value: hydra.scala.syntax.Type_ImplicitFunction) extends Type
   case tuple(value: hydra.scala.syntax.Type_Tuple) extends Type
   case `with`(value: hydra.scala.syntax.Type_With) extends Type
   case and(value: hydra.scala.syntax.Type_And) extends Type
   case or(value: hydra.scala.syntax.Type_Or) extends Type
   case refine(value: hydra.scala.syntax.Type_Refine) extends Type
   case existential(value: hydra.scala.syntax.Type_Existential) extends Type
   case annotate(value: hydra.scala.syntax.Type_Annotate) extends Type
   case lambda(value: hydra.scala.syntax.Type_Lambda) extends Type
   case `macro`(value: hydra.scala.syntax.Type_Macro) extends Type
   case method(value: hydra.scala.syntax.Type_Method) extends Type
   case placeholder(value: hydra.scala.syntax.Type_Placeholder) extends Type
   case byName(value: hydra.scala.syntax.Type_ByName) extends Type
   case repeated(value: hydra.scala.syntax.Type_Repeated) extends Type
   case `var`(value: hydra.scala.syntax.Type_Var) extends Type
   case typedParam(value: hydra.scala.syntax.Type_TypedParam) extends Type
   case `match`(value: hydra.scala.syntax.Type_Match) extends Type

enum Type_Ref :
   case name(value: hydra.scala.syntax.Type_Name) extends Type_Ref
   case select(value: hydra.scala.syntax.Type_Select) extends Type_Ref
   case project(value: hydra.scala.syntax.Type_Project) extends Type_Ref
   case singleton(value: hydra.scala.syntax.Type_Singleton) extends Type_Ref

case class Type_Name(value: scala.Predef.String)

type Type_AnonymousName = Unit

case class Type_Select(qual: hydra.scala.syntax.Data_Ref, name: hydra.scala.syntax.Type_Name)

case class Type_Project(qual: hydra.scala.syntax.Type, name: hydra.scala.syntax.Type_Name)

case class Type_Singleton(ref: hydra.scala.syntax.Data_Ref)

case class Type_Apply(tpe: hydra.scala.syntax.Type, args: Seq[hydra.scala.syntax.Type])

case class Type_ApplyInfix(lhs: hydra.scala.syntax.Type, op: hydra.scala.syntax.Type_Name, rhs: hydra.scala.syntax.Type)

enum Type_FunctionType :
   case function(value: hydra.scala.syntax.Type_Function) extends Type_FunctionType
   case contextFunction(value: hydra.scala.syntax.Type_ContextFunction) extends Type_FunctionType

case class Type_Function(params: Seq[hydra.scala.syntax.Type], res: hydra.scala.syntax.Type)

case class Type_PolyFunction(tparams: Seq[hydra.scala.syntax.Type_Param], tpe: hydra.scala.syntax.Type)

case class Type_ContextFunction(params: Seq[hydra.scala.syntax.Type], res: hydra.scala.syntax.Type)

case class Type_ImplicitFunction(params: Seq[hydra.scala.syntax.Type], res: hydra.scala.syntax.Type)

case class Type_Tuple(args: Seq[hydra.scala.syntax.Type])

case class Type_With(lhs: hydra.scala.syntax.Type, rhs: hydra.scala.syntax.Type)

case class Type_And(lhs: hydra.scala.syntax.Type, rhs: hydra.scala.syntax.Type)

case class Type_Or(lhs: hydra.scala.syntax.Type, rhs: hydra.scala.syntax.Type)

case class Type_Refine(tpe: Option[hydra.scala.syntax.Type], stats: Seq[hydra.scala.syntax.Stat])

case class Type_Existential(tpe: hydra.scala.syntax.Type, stats: Seq[hydra.scala.syntax.Stat])

case class Type_Annotate(tpe: hydra.scala.syntax.Type, annots: Seq[hydra.scala.syntax.Mod_Annot])

case class Type_Lambda(tparams: Seq[hydra.scala.syntax.Type_Param], tpe: hydra.scala.syntax.Type)

case class Type_Macro(body: hydra.scala.syntax.Data)

case class Type_Method(paramss: Seq[Seq[hydra.scala.syntax.Data_Param]], tpe: hydra.scala.syntax.Type)

case class Type_Placeholder(bounds: hydra.scala.syntax.TypeBounds)

case class TypeBounds(lo: Option[hydra.scala.syntax.Type], hi: Option[hydra.scala.syntax.Type])

case class Type_ByName(tpe: hydra.scala.syntax.Type)

case class Type_Repeated(tpe: hydra.scala.syntax.Type)

case class Type_Var(name: hydra.scala.syntax.Type_Name)

case class Type_TypedParam(name: hydra.scala.syntax.Name, typ: hydra.scala.syntax.Type)

case class Type_Param(mods: Seq[hydra.scala.syntax.Mod], name: hydra.scala.syntax.Name,
   tparams: Seq[hydra.scala.syntax.Type_Param], tbounds: Seq[hydra.scala.syntax.TypeBounds],
   vbounds: Seq[hydra.scala.syntax.Type], cbounds: Seq[hydra.scala.syntax.Type])

case class Type_Match(tpe: hydra.scala.syntax.Type, cases: Seq[hydra.scala.syntax.TypeCase])

enum Pat :
   case `var`(value: hydra.scala.syntax.Pat_Var) extends Pat
   case wildcard extends Pat
   case seqWildcard extends Pat
   case bind(value: hydra.scala.syntax.Pat_Bind) extends Pat
   case alternative(value: hydra.scala.syntax.Pat_Alternative) extends Pat
   case tuple(value: hydra.scala.syntax.Pat_Tuple) extends Pat
   case repeated(value: hydra.scala.syntax.Pat_Repeated) extends Pat
   case extract(value: hydra.scala.syntax.Pat_Extract) extends Pat
   case extractInfix(value: hydra.scala.syntax.Pat_ExtractInfix) extends Pat
   case interpolate(value: hydra.scala.syntax.Pat_Interpolate) extends Pat
   case xml(value: hydra.scala.syntax.Pat_Xml) extends Pat
   case typed(value: hydra.scala.syntax.Pat_Typed) extends Pat
   case `macro`(value: hydra.scala.syntax.Pat_Macro) extends Pat
   case `given`(value: hydra.scala.syntax.Pat_Given) extends Pat

case class Pat_Var(name: hydra.scala.syntax.Data_Name)

case class Pat_Bind(lhs: hydra.scala.syntax.Pat, rhs: hydra.scala.syntax.Pat)

case class Pat_Alternative(lhs: hydra.scala.syntax.Pat, rhs: hydra.scala.syntax.Pat)

case class Pat_Tuple(args: Seq[hydra.scala.syntax.Pat])

case class Pat_Repeated(name: hydra.scala.syntax.Data_Name)

case class Pat_Extract(fun: hydra.scala.syntax.Data, args: Seq[hydra.scala.syntax.Pat])

case class Pat_ExtractInfix(lhs: hydra.scala.syntax.Pat, op: hydra.scala.syntax.Data_Name,
   rhs: Seq[hydra.scala.syntax.Pat])

case class Pat_Interpolate(prefix: hydra.scala.syntax.Data_Name, parts: Seq[hydra.scala.syntax.Lit])

case class Pat_Xml(parts: Seq[hydra.scala.syntax.Lit], args: Seq[hydra.scala.syntax.Pat])

case class Pat_Typed(lhs: hydra.scala.syntax.Pat, rhs: hydra.scala.syntax.Type)

case class Pat_Macro(body: hydra.scala.syntax.Data)

case class Pat_Given(tpe: hydra.scala.syntax.Type)

enum Member :
   case term(value: hydra.scala.syntax.Member_Data) extends Member
   case `type`(value: hydra.scala.syntax.Member_Type) extends Member
   case termParam(value: hydra.scala.syntax.Data_Param) extends Member
   case typeParam(value: hydra.scala.syntax.Type_Param) extends Member
   case self(value: hydra.scala.syntax.Self) extends Member

enum Member_Data :
   case pkg(value: hydra.scala.syntax.Pkg) extends Member_Data
   case `object`(value: hydra.scala.syntax.Pkg_Object) extends Member_Data

case class Member_Type(name: hydra.scala.syntax.Type_Name)

enum Decl :
   case `val`(value: hydra.scala.syntax.Decl_Val) extends Decl
   case `var`(value: hydra.scala.syntax.Decl_Var) extends Decl
   case `def`(value: hydra.scala.syntax.Decl_Def) extends Decl
   case `type`(value: hydra.scala.syntax.Decl_Type) extends Decl
   case `given`(value: hydra.scala.syntax.Decl_Given) extends Decl

case class Decl_Val(mods: Seq[hydra.scala.syntax.Mod], pats: Seq[hydra.scala.syntax.Pat],
   decltpe: hydra.scala.syntax.Type)

case class Decl_Var(mods: Seq[hydra.scala.syntax.Mod], pats: Seq[hydra.scala.syntax.Pat],
   decltpe: hydra.scala.syntax.Type)

case class Decl_Def(mods: Seq[hydra.scala.syntax.Mod], name: hydra.scala.syntax.Data_Name,
   tparams: Seq[hydra.scala.syntax.Type_Param], paramss: Seq[Seq[hydra.scala.syntax.Data_Param]],
   decltpe: hydra.scala.syntax.Type)

case class Decl_Type(mods: Seq[hydra.scala.syntax.Mod], name: hydra.scala.syntax.Type_Name,
   tparams: Seq[hydra.scala.syntax.Type_Param], bounds: hydra.scala.syntax.TypeBounds)

case class Decl_Given(mods: Seq[hydra.scala.syntax.Mod], name: hydra.scala.syntax.Data_Name,
   tparams: Seq[hydra.scala.syntax.Type_Param], sparams: Seq[Seq[hydra.scala.syntax.Data_Param]],
   decltpe: hydra.scala.syntax.Type)

enum Defn :
   case `val`(value: hydra.scala.syntax.Defn_Val) extends Defn
   case `var`(value: hydra.scala.syntax.Defn_Var) extends Defn
   case `given`(value: hydra.scala.syntax.Defn_Given) extends Defn
   case `enum`(value: hydra.scala.syntax.Defn_Enum) extends Defn
   case enumCase(value: hydra.scala.syntax.Defn_EnumCase) extends Defn
   case repeatedEnumCase(value: hydra.scala.syntax.Defn_RepeatedEnumCase) extends Defn
   case givenAlias(value: hydra.scala.syntax.Defn_GivenAlias) extends Defn
   case extensionGroup(value: hydra.scala.syntax.Defn_ExtensionGroup) extends Defn
   case `def`(value: hydra.scala.syntax.Defn_Def) extends Defn
   case `macro`(value: hydra.scala.syntax.Defn_Macro) extends Defn
   case `type`(value: hydra.scala.syntax.Defn_Type) extends Defn
   case `class`(value: hydra.scala.syntax.Defn_Class) extends Defn
   case `trait`(value: hydra.scala.syntax.Defn_Trait) extends Defn
   case `object`(value: hydra.scala.syntax.Defn_Object) extends Defn

case class Defn_Val(mods: Seq[hydra.scala.syntax.Mod], pats: Seq[hydra.scala.syntax.Pat],
   decltpe: Option[hydra.scala.syntax.Type], rhs: hydra.scala.syntax.Data)

case class Defn_Var(mods: Seq[hydra.scala.syntax.Mod], pats: Seq[hydra.scala.syntax.Pat],
   decltpe: hydra.scala.syntax.Type, rhs: Option[hydra.scala.syntax.Data])

case class Defn_Given(mods: Seq[hydra.scala.syntax.Mod], name: hydra.scala.syntax.Name,
   tparams: Seq[Seq[hydra.scala.syntax.Type_Param]], sparams: Seq[Seq[hydra.scala.syntax.Data_Param]],
   templ: hydra.scala.syntax.Template)

case class Defn_Enum(mods: Seq[hydra.scala.syntax.Mod], name: hydra.scala.syntax.Type_Name,
   tparams: Seq[hydra.scala.syntax.Type_Param], ctor: hydra.scala.syntax.Ctor_Primary,
   template: hydra.scala.syntax.Template)

case class Defn_EnumCase(mods: Seq[hydra.scala.syntax.Mod], name: hydra.scala.syntax.Data_Name,
   tparams: Seq[hydra.scala.syntax.Type_Param], ctor: hydra.scala.syntax.Ctor_Primary,
   inits: Seq[hydra.scala.syntax.Init])

case class Defn_RepeatedEnumCase(mods: Seq[hydra.scala.syntax.Mod], cases: Seq[hydra.scala.syntax.Data_Name])

case class Defn_GivenAlias(mods: Seq[hydra.scala.syntax.Mod], name: hydra.scala.syntax.Name,
   tparams: Seq[Seq[hydra.scala.syntax.Type_Param]], sparams: Seq[Seq[hydra.scala.syntax.Data_Param]],
   decltpe: hydra.scala.syntax.Type, body: hydra.scala.syntax.Data)

case class Defn_ExtensionGroup(tparams: Seq[hydra.scala.syntax.Type_Param], parmss: Seq[Seq[hydra.scala.syntax.Data_Param]],
   body: hydra.scala.syntax.Stat)

case class Defn_Def(mods: Seq[hydra.scala.syntax.Mod], name: hydra.scala.syntax.Data_Name,
   tparams: Seq[hydra.scala.syntax.Type_Param], paramss: Seq[Seq[hydra.scala.syntax.Data_Param]],
   decltpe: Option[hydra.scala.syntax.Type], body: hydra.scala.syntax.Data)

case class Defn_Macro(mods: Seq[hydra.scala.syntax.Mod], name: hydra.scala.syntax.Data_Name,
   tparams: Seq[hydra.scala.syntax.Type_Param], paramss: Seq[Seq[hydra.scala.syntax.Data_Param]],
   decltpe: Option[hydra.scala.syntax.Type], body: hydra.scala.syntax.Data)

case class Defn_Type(mods: Seq[hydra.scala.syntax.Mod], name: hydra.scala.syntax.Type_Name,
   tparams: Seq[hydra.scala.syntax.Type_Param], body: hydra.scala.syntax.Type)

case class Defn_Class(mods: Seq[hydra.scala.syntax.Mod], name: hydra.scala.syntax.Type_Name,
   tparams: Seq[hydra.scala.syntax.Type_Param], ctor: hydra.scala.syntax.Ctor_Primary,
   template: hydra.scala.syntax.Template)

case class Defn_Trait(mods: Seq[hydra.scala.syntax.Mod], name: hydra.scala.syntax.Type_Name,
   tparams: Seq[hydra.scala.syntax.Type_Param], ctor: hydra.scala.syntax.Ctor_Primary,
   template: hydra.scala.syntax.Template)

case class Defn_Object(name: hydra.scala.syntax.Data_Name)

case class Pkg(name: hydra.scala.syntax.Data_Name, ref: hydra.scala.syntax.Data_Ref,
   stats: Seq[hydra.scala.syntax.Stat])

case class Pkg_Object(mods: Seq[hydra.scala.syntax.Mod], name: hydra.scala.syntax.Data_Name,
   template: hydra.scala.syntax.Template)

enum Ctor :
   case primary(value: hydra.scala.syntax.Ctor_Primary) extends Ctor
   case secondary(value: hydra.scala.syntax.Ctor_Secondary) extends Ctor

case class Ctor_Primary(mods: Seq[hydra.scala.syntax.Mod], name: hydra.scala.syntax.Name,
   paramss: Seq[Seq[hydra.scala.syntax.Data_Param]])

case class Ctor_Secondary(mods: Seq[hydra.scala.syntax.Mod], name: hydra.scala.syntax.Name,
   paramss: Seq[Seq[hydra.scala.syntax.Data_Param]], init: hydra.scala.syntax.Init,
   stats: Seq[hydra.scala.syntax.Stat])

case class Init(tpe: hydra.scala.syntax.Type, name: hydra.scala.syntax.Name, argss: Seq[Seq[hydra.scala.syntax.Data]])

type Self = Unit

case class Template(early: Seq[hydra.scala.syntax.Stat], inits: Seq[hydra.scala.syntax.Init],
   self: hydra.scala.syntax.Self, stats: Seq[hydra.scala.syntax.Stat])

enum Mod :
   case annot(value: hydra.scala.syntax.Mod_Annot) extends Mod
   case `private`(value: hydra.scala.syntax.Mod_Private) extends Mod
   case `protected`(value: hydra.scala.syntax.Mod_Protected) extends Mod
   case `implicit` extends Mod
   case `final` extends Mod
   case `sealed` extends Mod
   case open extends Mod
   case `super` extends Mod
   case `override` extends Mod
   case `case` extends Mod
   case `abstract` extends Mod
   case covariant extends Mod
   case contravariant extends Mod
   case `lazy` extends Mod
   case valParam extends Mod
   case varParam extends Mod
   case infix extends Mod
   case inline extends Mod
   case using extends Mod
   case opaque extends Mod
   case transparent extends Mod

case class Mod_Annot(init: hydra.scala.syntax.Init)

case class Mod_Private(within: hydra.scala.syntax.Ref)

case class Mod_Protected(within: hydra.scala.syntax.Ref)

enum Enumerator :
   case generator(value: hydra.scala.syntax.Enumerator_Generator) extends Enumerator
   case caseGenerator(value: hydra.scala.syntax.Enumerator_CaseGenerator) extends Enumerator
   case `val`(value: hydra.scala.syntax.Enumerator_Val) extends Enumerator
   case guard(value: hydra.scala.syntax.Enumerator_Guard) extends Enumerator

case class Enumerator_Generator(pat: hydra.scala.syntax.Pat, rhs: hydra.scala.syntax.Data)

case class Enumerator_CaseGenerator(pat: hydra.scala.syntax.Pat, rhs: hydra.scala.syntax.Data)

case class Enumerator_Val(pat: hydra.scala.syntax.Pat, rhs: hydra.scala.syntax.Data)

case class Enumerator_Guard(cond: hydra.scala.syntax.Data)

enum ImportExportStat :
   case `import`(value: hydra.scala.syntax.Import) extends ImportExportStat
   case `export`(value: hydra.scala.syntax.Export) extends ImportExportStat

case class Import(importers: Seq[hydra.scala.syntax.Importer])

case class Export(importers: Seq[hydra.scala.syntax.Importer])

case class Importer(ref: hydra.scala.syntax.Data_Ref, importees: Seq[hydra.scala.syntax.Importee])

enum Importee :
   case wildcard extends Importee
   case `given`(value: hydra.scala.syntax.Importee_Given) extends Importee
   case givenAll extends Importee
   case name(value: hydra.scala.syntax.Importee_Name) extends Importee
   case rename(value: hydra.scala.syntax.Importee_Rename) extends Importee
   case unimport(value: hydra.scala.syntax.Importee_Unimport) extends Importee

case class Importee_Given(tpe: hydra.scala.syntax.Type)

case class Importee_Name(name: hydra.scala.syntax.Name)

case class Importee_Rename(name: hydra.scala.syntax.Name, rename: hydra.scala.syntax.Name)

case class Importee_Unimport(name: hydra.scala.syntax.Name)

enum CaseTree :
   case `case`(value: hydra.scala.syntax.Case) extends CaseTree
   case typeCase(value: hydra.scala.syntax.TypeCase) extends CaseTree

case class Case(pat: hydra.scala.syntax.Pat, cond: Option[hydra.scala.syntax.Data], body: hydra.scala.syntax.Data)

case class TypeCase(pat: hydra.scala.syntax.Type, body: hydra.scala.syntax.Type)

case class Source(stats: Seq[hydra.scala.syntax.Stat])

type Quasi = Unit
