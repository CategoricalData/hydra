package hydra.templates

import hydra.context.*

import hydra.core.*

import hydra.error.*

import hydra.lib.eithers

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.sets

import hydra.lib.strings

def graphToSchema(cx: hydra.context.Context)(graph: hydra.graph.Graph)(els: Seq[hydra.core.Binding]): Either[hydra.context.InContext[hydra.error.DecodingError],
   Map[hydra.core.Name, hydra.core.Type]] =
  {
  def toPair(el: hydra.core.Binding): Either[hydra.context.InContext[hydra.error.DecodingError], Tuple2[hydra.core.Name, hydra.core.Type]] =
    {
    val name: hydra.core.Name = (el.name)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.DecodingError], hydra.core.Type, Tuple2[hydra.core.Name,
       hydra.core.Type]](hydra.lib.eithers.bimap[hydra.error.DecodingError, hydra.core.Type, hydra.context.InContext[hydra.error.DecodingError],
       hydra.core.Type]((_wc_e: hydra.error.DecodingError) => hydra.context.InContext(_wc_e, cx))((_wc_a: hydra.core.Type) => _wc_a)(hydra.decode.core.`type`(graph)(el.term)))((t: hydra.core.Type) => Right(Tuple2(name,
       t)))
  }
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.DecodingError], Seq[Tuple2[hydra.core.Name,
     hydra.core.Type]], Map[hydra.core.Name, hydra.core.Type]](hydra.lib.eithers.mapList[hydra.core.Binding,
     Tuple2[hydra.core.Name, hydra.core.Type], hydra.context.InContext[hydra.error.DecodingError]](toPair)(els))((pairs: Seq[Tuple2[hydra.core.Name,
     hydra.core.Type]]) =>
    Right(hydra.lib.maps.fromList[hydra.core.Name, hydra.core.Type](pairs)))
}

def instantiateTemplate(cx: hydra.context.Context)(minimal: Boolean)(schema: Map[hydra.core.Name, hydra.core.Type])(tname: hydra.core.Name)(t: hydra.core.Type): Either[hydra.context.InContext[hydra.error.Error],
   hydra.core.Term] =
  {
  def inst(tn: hydra.core.Name)(v1: hydra.core.Type): Either[hydra.context.InContext[hydra.error.Error],
     hydra.core.Term] = hydra.templates.instantiateTemplate(cx)(minimal)(schema)(tn)(v1)
  def noPoly[T0]: Either[hydra.context.InContext[hydra.error.Error], T0] =
    Left(hydra.context.InContext(hydra.error.Error.other("Polymorphic and function types are not currently supported"), cx))
  def forFloat(ft: hydra.core.FloatType): hydra.core.FloatValue =
    ft match
    case hydra.core.FloatType.bigfloat => hydra.core.FloatValue.bigfloat(BigDecimal(0.0))
    case hydra.core.FloatType.float32 => hydra.core.FloatValue.float32(0.0f)
    case hydra.core.FloatType.float64 => hydra.core.FloatValue.float64(0.0)
  def forInteger(it: hydra.core.IntegerType): hydra.core.IntegerValue =
    it match
    case hydra.core.IntegerType.bigint => hydra.core.IntegerValue.bigint(BigInt(0L))
    case hydra.core.IntegerType.int8 => hydra.core.IntegerValue.int8(0.toByte)
    case hydra.core.IntegerType.int16 => hydra.core.IntegerValue.int16(0.toShort)
    case hydra.core.IntegerType.int32 => hydra.core.IntegerValue.int32(0)
    case hydra.core.IntegerType.int64 => hydra.core.IntegerValue.int64(0L)
    case hydra.core.IntegerType.uint8 => hydra.core.IntegerValue.uint8(0.toByte)
    case hydra.core.IntegerType.uint16 => hydra.core.IntegerValue.uint16(0)
    case hydra.core.IntegerType.uint32 => hydra.core.IntegerValue.uint32(0L)
    case hydra.core.IntegerType.uint64 => hydra.core.IntegerValue.uint64(BigInt(0L))
  def forLiteral(lt: hydra.core.LiteralType): hydra.core.Literal =
    lt match
    case hydra.core.LiteralType.binary => hydra.core.Literal.string("")
    case hydra.core.LiteralType.boolean => hydra.core.Literal.boolean(false)
    case hydra.core.LiteralType.integer(v_LiteralType_integer_it) => hydra.core.Literal.integer(forInteger(v_LiteralType_integer_it))
    case hydra.core.LiteralType.float(v_LiteralType_float_ft) => hydra.core.Literal.float(forFloat(v_LiteralType_float_ft))
    case hydra.core.LiteralType.string => hydra.core.Literal.string("")
  t match
    case hydra.core.Type.annotated(v_Type_annotated_at) => inst(tname)(v_Type_annotated_at.body)
    case hydra.core.Type.application(v_Type_application__) => noPoly
    case hydra.core.Type.function(v_Type_function__) => noPoly
    case hydra.core.Type.forall(v_Type_forall__) => noPoly
    case hydra.core.Type.list(v_Type_list_et) => hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
       hydra.core.Term]](minimal)(Right(hydra.core.Term.list(Seq())))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.core.Term, hydra.core.Term](inst(tname)(v_Type_list_et))((e: hydra.core.Term) => Right(hydra.core.Term.list(Seq(e)))))
    case hydra.core.Type.literal(v_Type_literal_lt) => Right(hydra.core.Term.literal(forLiteral(v_Type_literal_lt)))
    case hydra.core.Type.map(v_Type_map_mt) => {
      val kt: hydra.core.Type = (v_Type_map_mt.keys)
      {
        val vt: hydra.core.Type = (v_Type_map_mt.values)
        hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error], hydra.core.Term]](minimal)(Right(hydra.core.Term.map(hydra.lib.maps.empty[hydra.core.Term,
           hydra.core.Term])))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term,
           hydra.core.Term](inst(tname)(kt))((ke: hydra.core.Term) =>
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, hydra.core.Term](inst(tname)(vt))((ve: hydra.core.Term) =>
          Right(hydra.core.Term.map(hydra.lib.maps.singleton[hydra.core.Term, hydra.core.Term](ke)(ve))))))
      }
    }
    case hydra.core.Type.maybe(v_Type_maybe_ot) => hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
       hydra.core.Term]](minimal)(Right(hydra.core.Term.maybe(None)))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.core.Term, hydra.core.Term](inst(tname)(v_Type_maybe_ot))((e: hydra.core.Term) => Right(hydra.core.Term.maybe(Some(e)))))
    case hydra.core.Type.record(v_Type_record_rt) => {
      def toField(ft: hydra.core.FieldType): Either[hydra.context.InContext[hydra.error.Error], hydra.core.Field] =
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], hydra.core.Term, hydra.core.Field](inst(tname)(ft.`type`))((e: hydra.core.Term) => Right(hydra.core.Field(ft.name,
           e)))
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error], Seq[hydra.core.Field], hydra.core.Term](hydra.lib.eithers.mapList[hydra.core.FieldType,
         hydra.core.Field, hydra.context.InContext[hydra.error.Error]](toField)(v_Type_record_rt))((dfields: Seq[hydra.core.Field]) =>
        Right(hydra.core.Term.record(hydra.core.Record(tname, dfields))))
    }
    case hydra.core.Type.set(v_Type_set_et) => hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.error.Error],
       hydra.core.Term]](minimal)(Right(hydra.core.Term.set(hydra.lib.sets.empty[hydra.core.Term])))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.core.Term, hydra.core.Term](inst(tname)(v_Type_set_et))((e: hydra.core.Term) =>
      Right(hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](Seq(e))))))
    case hydra.core.Type.variable(v_Type_variable_vname) => hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.error.Error],
       hydra.core.Term], hydra.core.Type](Left(hydra.context.InContext(hydra.error.Error.other(hydra.lib.strings.cat2("Type variable ")(hydra.lib.strings.cat2(hydra.show.core.term(hydra.core.Term.variable(v_Type_variable_vname)))(" not found in schema"))),
       cx)))((v1: hydra.core.Type) => inst(v_Type_variable_vname)(v1))(hydra.lib.maps.lookup[hydra.core.Name,
       hydra.core.Type](v_Type_variable_vname)(schema))
    case hydra.core.Type.wrap(v_Type_wrap_wt) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.error.Error],
       hydra.core.Term, hydra.core.Term](inst(tname)(v_Type_wrap_wt))((e: hydra.core.Term) =>
      Right(hydra.core.Term.wrap(hydra.core.WrappedTerm(tname, e))))
}
