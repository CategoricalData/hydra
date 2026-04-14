package hydra.show.core

import hydra.core.*

def binding(el: hydra.core.Binding): scala.Predef.String =
  {
  lazy val name: scala.Predef.String = (el.name)
  lazy val t: hydra.core.Term = (el.term)
  lazy val typeStr: scala.Predef.String = hydra.lib.maybes.maybe[scala.Predef.String, hydra.core.TypeScheme]("")((ts: hydra.core.TypeScheme) =>
    hydra.lib.strings.cat(Seq(":(", hydra.show.core.typeScheme(ts), ")")))(el.`type`)
  hydra.lib.strings.cat(Seq(name, typeStr, " = ", hydra.show.core.term(t)))
}

def caseStatement(cs: hydra.core.CaseStatement): scala.Predef.String =
  {
  lazy val tname: scala.Predef.String = (cs.typeName)
  lazy val mdef: Option[hydra.core.Term] = (cs.default)
  lazy val csCases: Seq[hydra.core.Field] = (cs.cases)
  lazy val defaultField: Seq[hydra.core.Field] = hydra.lib.maybes.maybe[Seq[hydra.core.Field], hydra.core.Term](Seq())((d: hydra.core.Term) => Seq(hydra.core.Field("[default]",
     d)))(mdef)
  lazy val allFields: Seq[hydra.core.Field] = hydra.lib.lists.concat[hydra.core.Field](Seq(csCases, defaultField))
  hydra.lib.strings.cat(Seq("case(", tname, ")", hydra.show.core.fields(allFields)))
}

def either[T0, T1](showA: (T0 => scala.Predef.String))(showB: (T1 => scala.Predef.String))(e: Either[T0, T1]): scala.Predef.String =
  hydra.lib.eithers.either[T0, T1, scala.Predef.String]((a: T0) =>
  hydra.lib.strings.cat2("left(")(hydra.lib.strings.cat2(showA(a))(")")))((b: T1) =>
  hydra.lib.strings.cat2("right(")(hydra.lib.strings.cat2(showB(b))(")")))(e)

def field(field: hydra.core.Field): scala.Predef.String =
  {
  lazy val fname: scala.Predef.String = (field.name)
  lazy val fterm: hydra.core.Term = (field.term)
  hydra.lib.strings.cat(Seq(fname, "=", hydra.show.core.term(fterm)))
}

def fieldType(ft: hydra.core.FieldType): scala.Predef.String =
  {
  lazy val fname: scala.Predef.String = (ft.name)
  lazy val ftyp: hydra.core.Type = (ft.`type`)
  hydra.lib.strings.cat(Seq(fname, ":", hydra.show.core.`type`(ftyp)))
}

def fields(flds: Seq[hydra.core.Field]): scala.Predef.String =
  {
  lazy val fieldStrs: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.core.Field, scala.Predef.String](hydra.show.core.field)(flds)
  hydra.lib.strings.cat(Seq("{", hydra.lib.strings.intercalate(", ")(fieldStrs), "}"))
}

def float(fv: hydra.core.FloatValue): scala.Predef.String =
  fv match
  case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_v) => hydra.lib.strings.cat2(hydra.lib.literals.showBigfloat(v_FloatValue_bigfloat_v))(":bigfloat")
  case hydra.core.FloatValue.float32(v_FloatValue_float32_v) => hydra.lib.strings.cat2(hydra.lib.literals.showFloat32(v_FloatValue_float32_v))(":float32")
  case hydra.core.FloatValue.float64(v_FloatValue_float64_v) => hydra.lib.strings.cat2(hydra.lib.literals.showFloat64(v_FloatValue_float64_v))(":float64")

def floatType(ft: hydra.core.FloatType): scala.Predef.String =
  ft match
  case hydra.core.FloatType.bigfloat => "bigfloat"
  case hydra.core.FloatType.float32 => "float32"
  case hydra.core.FloatType.float64 => "float64"

def injection(inj: hydra.core.Injection): scala.Predef.String =
  {
  lazy val tname: hydra.core.Name = (inj.typeName)
  lazy val f: hydra.core.Field = (inj.field)
  hydra.lib.strings.cat(Seq("inject(", tname, ")", hydra.show.core.fields(Seq(f))))
}

def integer(iv: hydra.core.IntegerValue): scala.Predef.String =
  iv match
  case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_v) => hydra.lib.strings.cat2(hydra.lib.literals.showBigint(v_IntegerValue_bigint_v))(":bigint")
  case hydra.core.IntegerValue.int8(v_IntegerValue_int8_v) => hydra.lib.strings.cat2(hydra.lib.literals.showInt8(v_IntegerValue_int8_v))(":int8")
  case hydra.core.IntegerValue.int16(v_IntegerValue_int16_v) => hydra.lib.strings.cat2(hydra.lib.literals.showInt16(v_IntegerValue_int16_v))(":int16")
  case hydra.core.IntegerValue.int32(v_IntegerValue_int32_v) => hydra.lib.strings.cat2(hydra.lib.literals.showInt32(v_IntegerValue_int32_v))(":int32")
  case hydra.core.IntegerValue.int64(v_IntegerValue_int64_v) => hydra.lib.strings.cat2(hydra.lib.literals.showInt64(v_IntegerValue_int64_v))(":int64")
  case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_v) => hydra.lib.strings.cat2(hydra.lib.literals.showUint8(v_IntegerValue_uint8_v))(":uint8")
  case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_v) => hydra.lib.strings.cat2(hydra.lib.literals.showUint16(v_IntegerValue_uint16_v))(":uint16")
  case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_v) => hydra.lib.strings.cat2(hydra.lib.literals.showUint32(v_IntegerValue_uint32_v))(":uint32")
  case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_v) => hydra.lib.strings.cat2(hydra.lib.literals.showUint64(v_IntegerValue_uint64_v))(":uint64")

def integerType(it: hydra.core.IntegerType): scala.Predef.String =
  it match
  case hydra.core.IntegerType.bigint => "bigint"
  case hydra.core.IntegerType.int8 => "int8"
  case hydra.core.IntegerType.int16 => "int16"
  case hydra.core.IntegerType.int32 => "int32"
  case hydra.core.IntegerType.int64 => "int64"
  case hydra.core.IntegerType.uint8 => "uint8"
  case hydra.core.IntegerType.uint16 => "uint16"
  case hydra.core.IntegerType.uint32 => "uint32"
  case hydra.core.IntegerType.uint64 => "uint64"

def lambda(l: hydra.core.Lambda): scala.Predef.String =
  {
  lazy val v: scala.Predef.String = (l.parameter)
  lazy val mt: Option[hydra.core.Type] = (l.domain)
  lazy val body: hydra.core.Term = (l.body)
  lazy val typeStr: scala.Predef.String = hydra.lib.maybes.maybe[scala.Predef.String, hydra.core.Type]("")((t: hydra.core.Type) => hydra.lib.strings.cat2(":")(hydra.show.core.`type`(t)))(mt)
  hydra.lib.strings.cat(Seq("\u03BB", v, typeStr, ".", hydra.show.core.term(body)))
}

def let(l: hydra.core.Let): scala.Predef.String =
  {
  lazy val bindings: Seq[hydra.core.Binding] = (l.bindings)
  lazy val env: hydra.core.Term = (l.body)
  lazy val bindingStrs: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.core.Binding, scala.Predef.String](hydra.show.core.binding)(bindings)
  hydra.lib.strings.cat(Seq("let ", hydra.lib.strings.intercalate(", ")(bindingStrs), " in ", hydra.show.core.term(env)))
}

def list[T0](f: (T0 => scala.Predef.String))(xs: Seq[T0]): scala.Predef.String =
  {
  lazy val elementStrs: Seq[scala.Predef.String] = hydra.lib.lists.map[T0, scala.Predef.String](f)(xs)
  hydra.lib.strings.cat(Seq("[", hydra.lib.strings.intercalate(", ")(elementStrs), "]"))
}

def literal(l: hydra.core.Literal): scala.Predef.String =
  l match
  case hydra.core.Literal.binary(v_Literal_binary__) => "[binary]"
  case hydra.core.Literal.boolean(v_Literal_boolean_b) => hydra.lib.logic.ifElse[scala.Predef.String](v_Literal_boolean_b)("true")("false")
  case hydra.core.Literal.float(v_Literal_float_fv) => hydra.show.core.float(v_Literal_float_fv)
  case hydra.core.Literal.integer(v_Literal_integer_iv) => hydra.show.core.integer(v_Literal_integer_iv)
  case hydra.core.Literal.string(v_Literal_string_s) => hydra.lib.literals.showString(v_Literal_string_s)

def literalType(lt: hydra.core.LiteralType): scala.Predef.String =
  lt match
  case hydra.core.LiteralType.binary => "binary"
  case hydra.core.LiteralType.boolean => "boolean"
  case hydra.core.LiteralType.float(v_LiteralType_float_ft) => hydra.show.core.floatType(v_LiteralType_float_ft)
  case hydra.core.LiteralType.integer(v_LiteralType_integer_it) => hydra.show.core.integerType(v_LiteralType_integer_it)
  case hydra.core.LiteralType.string => "string"

def map[T0, T1](showK: (T0 => scala.Predef.String))(showV: (T1 => scala.Predef.String))(m: Map[T0, T1]): scala.Predef.String =
  {
  lazy val pairStrs: Seq[scala.Predef.String] = hydra.lib.lists.map[Tuple2[T0, T1], scala.Predef.String]((p: Tuple2[T0, T1]) =>
    hydra.lib.strings.cat(Seq(showK(hydra.lib.pairs.first[T0, T1](p)), ": ", showV(hydra.lib.pairs.second[T0, T1](p)))))(hydra.lib.maps.toList[T0, T1](m))
  hydra.lib.strings.cat(Seq("{", hydra.lib.strings.intercalate(", ")(pairStrs), "}"))
}

def maybe[T0](f: (T0 => scala.Predef.String))(mx: Option[T0]): scala.Predef.String =
  hydra.lib.maybes.maybe[scala.Predef.String, T0]("nothing")((x: T0) =>
  hydra.lib.strings.cat2("just(")(hydra.lib.strings.cat2(f(x))(")")))(mx)

def pair[T0, T1](showA: (T0 => scala.Predef.String))(showB: (T1 => scala.Predef.String))(p: Tuple2[T0, T1]): scala.Predef.String =
  hydra.lib.strings.cat(Seq("(", showA(hydra.lib.pairs.first[T0, T1](p)), ", ", showB(hydra.lib.pairs.second[T0, T1](p)), ")"))

def projection(proj: hydra.core.Projection): scala.Predef.String =
  {
  lazy val tname: scala.Predef.String = (proj.typeName)
  lazy val fname: scala.Predef.String = (proj.field)
  hydra.lib.strings.cat(Seq("project(", tname, "){", fname, "}"))
}

def readTerm(s: scala.Predef.String): Option[hydra.core.Term] = Some(hydra.core.Term.literal(hydra.core.Literal.string(s)))

def set[T0](f: (T0 => scala.Predef.String))(xs: scala.collection.immutable.Set[T0]): scala.Predef.String =
  {
  lazy val elementStrs: Seq[scala.Predef.String] = hydra.lib.lists.map[T0, scala.Predef.String](f)(hydra.lib.sets.toList[T0](xs))
  hydra.lib.strings.cat(Seq("{", hydra.lib.strings.intercalate(", ")(elementStrs), "}"))
}

def term(t: hydra.core.Term): scala.Predef.String =
  {
  def gatherTerms(prev: Seq[hydra.core.Term])(app: hydra.core.Application): Seq[hydra.core.Term] =
    {
    lazy val lhs: hydra.core.Term = (app.function)
    lazy val rhs: hydra.core.Term = (app.argument)
    lhs match
      case hydra.core.Term.application(v_Term_application_app2) => gatherTerms(hydra.lib.lists.cons[hydra.core.Term](rhs)(prev))(v_Term_application_app2)
      case _ => hydra.lib.lists.cons[hydra.core.Term](lhs)(hydra.lib.lists.cons[hydra.core.Term](rhs)(prev))
  }
  t match
    case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.show.core.term(v_Term_annotated_at.body)
    case hydra.core.Term.application(v_Term_application_app) => {
      lazy val terms: Seq[hydra.core.Term] = gatherTerms(Seq())(v_Term_application_app)
      {
        lazy val termStrs: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.core.Term, scala.Predef.String](hydra.show.core.term)(terms)
        hydra.lib.strings.cat(Seq("(", hydra.lib.strings.intercalate(" @ ")(termStrs), ")"))
      }
    }
    case hydra.core.Term.cases(v_Term_cases_v1) => hydra.show.core.caseStatement(v_Term_cases_v1)
    case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term, scala.Predef.String]((l: hydra.core.Term) =>
      hydra.lib.strings.cat(Seq("left(", hydra.show.core.term(l), ")")))((r: hydra.core.Term) =>
      hydra.lib.strings.cat(Seq("right(", hydra.show.core.term(r), ")")))(v_Term_either_e)
    case hydra.core.Term.lambda(v_Term_lambda_v1) => hydra.show.core.lambda(v_Term_lambda_v1)
    case hydra.core.Term.let(v_Term_let_l) => hydra.show.core.let(v_Term_let_l)
    case hydra.core.Term.list(v_Term_list_els) => {
      lazy val termStrs: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.core.Term, scala.Predef.String](hydra.show.core.term)(v_Term_list_els)
      hydra.lib.strings.cat(Seq("[", hydra.lib.strings.intercalate(", ")(termStrs), "]"))
    }
    case hydra.core.Term.literal(v_Term_literal_lit) => hydra.show.core.literal(v_Term_literal_lit)
    case hydra.core.Term.map(v_Term_map_m) => {
      def entry(p: Tuple2[hydra.core.Term, hydra.core.Term]): scala.Predef.String =
        hydra.lib.strings.cat(Seq(hydra.show.core.term(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](p)),
           "=", hydra.show.core.term(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](p))))
      hydra.lib.strings.cat(Seq("{", hydra.lib.strings.intercalate(", ")(hydra.lib.lists.map[Tuple2[hydra.core.Term,
         hydra.core.Term], scala.Predef.String](entry)(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m))),
         "}"))
    }
    case hydra.core.Term.maybe(v_Term_maybe_mt) => hydra.lib.maybes.maybe[scala.Predef.String, hydra.core.Term]("nothing")((t2: hydra.core.Term) =>
      hydra.lib.strings.cat(Seq("just(", hydra.show.core.term(t2), ")")))(v_Term_maybe_mt)
    case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.strings.cat(Seq("(", hydra.show.core.term(hydra.lib.pairs.first[hydra.core.Term,
       hydra.core.Term](v_Term_pair_p)), ", ", hydra.show.core.term(hydra.lib.pairs.second[hydra.core.Term,
       hydra.core.Term](v_Term_pair_p)), ")"))
    case hydra.core.Term.project(v_Term_project_v1) => hydra.show.core.projection(v_Term_project_v1)
    case hydra.core.Term.record(v_Term_record_rec) => {
      lazy val tname: scala.Predef.String = (v_Term_record_rec.typeName)
      {
        lazy val flds: Seq[hydra.core.Field] = (v_Term_record_rec.fields)
        hydra.lib.strings.cat(Seq("record(", tname, ")", hydra.show.core.fields(flds)))
      }
    }
    case hydra.core.Term.set(v_Term_set_s) => hydra.lib.strings.cat(Seq("{", hydra.lib.strings.intercalate(", ")(hydra.lib.lists.map[hydra.core.Term,
       scala.Predef.String](hydra.show.core.term)(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s))),
       "}"))
    case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => {
      lazy val param: scala.Predef.String = (v_Term_typeLambda_ta.parameter)
      {
        lazy val body: hydra.core.Term = (v_Term_typeLambda_ta.body)
        hydra.lib.strings.cat(Seq("\u039B", param, ".", hydra.show.core.term(body)))
      }
    }
    case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => {
      lazy val t2: hydra.core.Term = (v_Term_typeApplication_tt.body)
      {
        lazy val typ: hydra.core.Type = (v_Term_typeApplication_tt.`type`)
        hydra.lib.strings.cat(Seq(hydra.show.core.term(t2), "\u27E8", hydra.show.core.`type`(typ), "\u27E9"))
      }
    }
    case hydra.core.Term.inject(v_Term_inject_v1) => hydra.show.core.injection(v_Term_inject_v1)
    case hydra.core.Term.unit => "unit"
    case hydra.core.Term.unwrap(v_Term_unwrap_tname) => hydra.lib.strings.cat(Seq("unwrap(", v_Term_unwrap_tname, ")"))
    case hydra.core.Term.variable(v_Term_variable_name) => v_Term_variable_name
    case hydra.core.Term.wrap(v_Term_wrap_wt) => {
      lazy val tname: scala.Predef.String = (v_Term_wrap_wt.typeName)
      {
        lazy val term1: hydra.core.Term = (v_Term_wrap_wt.body)
        hydra.lib.strings.cat(Seq("wrap(", tname, "){", hydra.show.core.term(term1), "}"))
      }
    }
}

def `type`(typ: hydra.core.Type): scala.Predef.String =
  {
  def showRowType(flds: Seq[hydra.core.FieldType]): scala.Predef.String =
    {
    lazy val fieldStrs: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.core.FieldType, scala.Predef.String](hydra.show.core.fieldType)(flds)
    hydra.lib.strings.cat(Seq("{", hydra.lib.strings.intercalate(", ")(fieldStrs), "}"))
  }
  def gatherTypes(prev: Seq[hydra.core.Type])(app: hydra.core.ApplicationType): Seq[hydra.core.Type] =
    {
    lazy val lhs: hydra.core.Type = (app.function)
    lazy val rhs: hydra.core.Type = (app.argument)
    lhs match
      case hydra.core.Type.application(v_Type_application_app2) => gatherTypes(hydra.lib.lists.cons[hydra.core.Type](rhs)(prev))(v_Type_application_app2)
      case _ => hydra.lib.lists.cons[hydra.core.Type](lhs)(hydra.lib.lists.cons[hydra.core.Type](rhs)(prev))
  }
  def gatherFunctionTypes(prev: Seq[hydra.core.Type])(t: hydra.core.Type): Seq[hydra.core.Type] =
    t match
    case hydra.core.Type.function(v_Type_function_ft) => {
      lazy val dom: hydra.core.Type = (v_Type_function_ft.domain)
      {
        lazy val cod: hydra.core.Type = (v_Type_function_ft.codomain)
        gatherFunctionTypes(hydra.lib.lists.cons[hydra.core.Type](dom)(prev))(cod)
      }
    }
    case _ => hydra.lib.lists.reverse[hydra.core.Type](hydra.lib.lists.cons[hydra.core.Type](t)(prev))
  typ match
    case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.show.core.`type`(v_Type_annotated_at.body)
    case hydra.core.Type.application(v_Type_application_app) => {
      lazy val types: Seq[hydra.core.Type] = gatherTypes(Seq())(v_Type_application_app)
      {
        lazy val typeStrs: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.core.Type, scala.Predef.String](hydra.show.core.`type`)(types)
        hydra.lib.strings.cat(Seq("(", hydra.lib.strings.intercalate(" @ ")(typeStrs), ")"))
      }
    }
    case hydra.core.Type.either(v_Type_either_et) => {
      lazy val leftTyp: hydra.core.Type = (v_Type_either_et.left)
      {
        lazy val rightTyp: hydra.core.Type = (v_Type_either_et.right)
        hydra.lib.strings.cat(Seq("either<", hydra.show.core.`type`(leftTyp), ", ", hydra.show.core.`type`(rightTyp), ">"))
      }
    }
    case hydra.core.Type.forall(v_Type_forall_ft) => {
      lazy val `var`: scala.Predef.String = (v_Type_forall_ft.parameter)
      {
        lazy val body: hydra.core.Type = (v_Type_forall_ft.body)
        hydra.lib.strings.cat(Seq("(\u2200", `var`, ".", hydra.show.core.`type`(body), ")"))
      }
    }
    case hydra.core.Type.function(v_Type_function_ft) => {
      lazy val types: Seq[hydra.core.Type] = gatherFunctionTypes(Seq())(typ)
      {
        lazy val typeStrs: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.core.Type, scala.Predef.String](hydra.show.core.`type`)(types)
        hydra.lib.strings.cat(Seq("(", hydra.lib.strings.intercalate(" \u2192 ")(typeStrs), ")"))
      }
    }
    case hydra.core.Type.list(v_Type_list_etyp) => hydra.lib.strings.cat(Seq("list<", hydra.show.core.`type`(v_Type_list_etyp), ">"))
    case hydra.core.Type.literal(v_Type_literal_lt) => hydra.show.core.literalType(v_Type_literal_lt)
    case hydra.core.Type.map(v_Type_map_mt) => {
      lazy val keyTyp: hydra.core.Type = (v_Type_map_mt.keys)
      {
        lazy val valTyp: hydra.core.Type = (v_Type_map_mt.values)
        hydra.lib.strings.cat(Seq("map<", hydra.show.core.`type`(keyTyp), ", ", hydra.show.core.`type`(valTyp), ">"))
      }
    }
    case hydra.core.Type.maybe(v_Type_maybe_etyp) => hydra.lib.strings.cat(Seq("maybe<", hydra.show.core.`type`(v_Type_maybe_etyp), ">"))
    case hydra.core.Type.pair(v_Type_pair_pt) => {
      lazy val firstTyp: hydra.core.Type = (v_Type_pair_pt.first)
      {
        lazy val secondTyp: hydra.core.Type = (v_Type_pair_pt.second)
        hydra.lib.strings.cat(Seq("(", hydra.show.core.`type`(firstTyp), ", ", hydra.show.core.`type`(secondTyp), ")"))
      }
    }
    case hydra.core.Type.record(v_Type_record_rt) => hydra.lib.strings.cat2("record")(showRowType(v_Type_record_rt))
    case hydra.core.Type.set(v_Type_set_etyp) => hydra.lib.strings.cat(Seq("set<", hydra.show.core.`type`(v_Type_set_etyp), ">"))
    case hydra.core.Type.union(v_Type_union_rt) => hydra.lib.strings.cat2("union")(showRowType(v_Type_union_rt))
    case hydra.core.Type.unit => "unit"
    case hydra.core.Type.variable(v_Type_variable_name) => v_Type_variable_name
    case hydra.core.Type.void => "void"
    case hydra.core.Type.wrap(v_Type_wrap_wt) => hydra.lib.strings.cat(Seq("wrap(", hydra.show.core.`type`(v_Type_wrap_wt), ")"))
}

def typeScheme(ts: hydra.core.TypeScheme): scala.Predef.String =
  {
  lazy val vars: Seq[hydra.core.Name] = (ts.variables)
  lazy val body: hydra.core.Type = (ts.`type`)
  lazy val varNames: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.core.Name, scala.Predef.String]((x) => x)(vars)
  lazy val fa: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.lists.`null`[hydra.core.Name](vars))("")(hydra.lib.strings.cat(Seq("forall ",
     hydra.lib.strings.intercalate(",")(varNames), ". ")))
  def toConstraintPair(v: hydra.core.Name)(c: hydra.core.Name): scala.Predef.String = hydra.lib.strings.cat(Seq(c, " ", v))
  def toConstraintPairs(p: Tuple2[hydra.core.Name, hydra.core.TypeVariableMetadata]): Seq[scala.Predef.String] =
    hydra.lib.lists.map[hydra.core.Name, scala.Predef.String]((v1: hydra.core.Name) =>
    toConstraintPair(hydra.lib.pairs.first[hydra.core.Name, hydra.core.TypeVariableMetadata](p))(v1))(hydra.lib.sets.toList[hydra.core.Name](hydra.lib.pairs.second[hydra.core.Name,
       hydra.core.TypeVariableMetadata](p).classes))
  lazy val tc: Seq[scala.Predef.String] = hydra.lib.maybes.maybe[Seq[scala.Predef.String], Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata]](Seq())((m: Map[hydra.core.Name, hydra.core.TypeVariableMetadata]) =>
    hydra.lib.lists.concat[scala.Predef.String](hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.TypeVariableMetadata],
       Seq[scala.Predef.String]](toConstraintPairs)(hydra.lib.maps.toList[hydra.core.Name, hydra.core.TypeVariableMetadata](m))))(ts.constraints)
  hydra.lib.strings.cat(Seq("(", fa, hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.lists.`null`[scala.Predef.String](tc))("")(hydra.lib.strings.cat(Seq("(",
     hydra.lib.strings.intercalate(", ")(tc), ") => "))), hydra.show.core.`type`(body), ")"))
}
