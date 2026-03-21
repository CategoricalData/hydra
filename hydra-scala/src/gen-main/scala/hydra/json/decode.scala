package hydra.json.decode

import hydra.core.*

import hydra.json.model.*

import hydra.lib.eithers

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.literals

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.sets

import hydra.lib.strings

def fromJson(types: Map[hydra.core.Name, hydra.core.Type])(tname: hydra.core.Name)(typ: hydra.core.Type)(value: hydra.json.model.Value): Either[scala.Predef.String,
   hydra.core.Term] =
  {
  val stripped: hydra.core.Type = hydra.rewriting.deannotateType(typ)
  stripped match
    case hydra.core.Type.literal(v_Type_literal_lt) => hydra.json.decode.decodeLiteral(v_Type_literal_lt)(value)
    case hydra.core.Type.list(v_Type_list_elemType) => {
      def decodeElem(v: hydra.json.model.Value): Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(v_Type_list_elemType)(v)
      {
        val arrResult: Either[scala.Predef.String, Seq[hydra.json.model.Value]] = hydra.json.decode.expectArray(value)
        eithers.either[scala.Predef.String, Seq[hydra.json.model.Value], Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((arr: Seq[hydra.json.model.Value]) =>
          {
          val decoded: Either[scala.Predef.String, Seq[hydra.core.Term]] = eithers.mapList[hydra.json.model.Value,
             hydra.core.Term, scala.Predef.String](decodeElem)(arr)
          eithers.map[Seq[hydra.core.Term], hydra.core.Term, scala.Predef.String]((ts: Seq[hydra.core.Term]) => hydra.core.Term.list(ts))(decoded)
        })(arrResult)
      }
    }
    case hydra.core.Type.set(v_Type_set_elemType) => {
      def decodeElem(v: hydra.json.model.Value): Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(v_Type_set_elemType)(v)
      {
        val arrResult: Either[scala.Predef.String, Seq[hydra.json.model.Value]] = hydra.json.decode.expectArray(value)
        eithers.either[scala.Predef.String, Seq[hydra.json.model.Value], Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((arr: Seq[hydra.json.model.Value]) =>
          {
          val decoded: Either[scala.Predef.String, Seq[hydra.core.Term]] = eithers.mapList[hydra.json.model.Value,
             hydra.core.Term, scala.Predef.String](decodeElem)(arr)
          eithers.map[Seq[hydra.core.Term], hydra.core.Term, scala.Predef.String]((elems: Seq[hydra.core.Term]) => hydra.core.Term.set(sets.fromList[hydra.core.Term](elems)))(decoded)
        })(arrResult)
      }
    }
    case hydra.core.Type.maybe(v_Type_maybe_innerType) => {
      def decodeJust(arr: Seq[hydra.json.model.Value]): Either[scala.Predef.String, hydra.core.Term] =
        eithers.map[hydra.core.Term, hydra.core.Term, scala.Predef.String]((v: hydra.core.Term) => hydra.core.Term.maybe(Some(v)))(hydra.json.decode.fromJson(types)(tname)(v_Type_maybe_innerType)(lists.head[hydra.json.model.Value](arr)))
      {
        def decodeMaybeArray(arr: Seq[hydra.json.model.Value]): Either[scala.Predef.String, hydra.core.Term] =
          {
          val len: Int = lists.length[hydra.json.model.Value](arr)
          logic.ifElse[Either[scala.Predef.String, hydra.core.Term]](equality.equal[Int](len)(0))(Right(hydra.core.Term.maybe(None)))(logic.ifElse[Either[scala.Predef.String,
             hydra.core.Term]](equality.equal[Int](len)(1))(decodeJust(arr))(Left("expected single-element array for Just")))
        }
        value match
          case hydra.json.model.Value.`null` => Right(hydra.core.Term.maybe(None))
          case hydra.json.model.Value.array(v_Value_array_arr) => decodeMaybeArray(v_Value_array_arr)
          case _ => Left("expected null or single-element array for Maybe")
      }
    }
    case hydra.core.Type.record(v_Type_record_rt) => {
      val objResult: Either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value]] = hydra.json.decode.expectObject(value)
      eithers.either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value], Either[scala.Predef.String,
         hydra.core.Term]]((err: scala.Predef.String) => Left(err))((obj: Map[scala.Predef.String, hydra.json.model.Value]) =>
        {
        def decodeField(ft: hydra.core.FieldType): Either[scala.Predef.String, hydra.core.Field] =
          {
          val fname: hydra.core.Name = (ft.name)
          val ftype: hydra.core.Type = (ft.`type`)
          val mval: Option[hydra.json.model.Value] = maps.lookup[scala.Predef.String, hydra.json.model.Value](fname)(obj)
          val defaultVal: hydra.json.model.Value = hydra.json.model.Value.`null`
          val jsonVal: hydra.json.model.Value = maybes.fromMaybe[hydra.json.model.Value](defaultVal)(mval)
          val decoded: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(ftype)(jsonVal)
          eithers.map[hydra.core.Term, hydra.core.Field, scala.Predef.String]((v: hydra.core.Term) => hydra.core.Field(fname, v))(decoded)
        }
        {
          val decodedFields: Either[scala.Predef.String, Seq[hydra.core.Field]] = eithers.mapList[hydra.core.FieldType,
             hydra.core.Field, scala.Predef.String](decodeField)(v_Type_record_rt)
          eithers.map[Seq[hydra.core.Field], hydra.core.Term, scala.Predef.String]((fs: Seq[hydra.core.Field]) => hydra.core.Term.record(hydra.core.Record(tname,
             fs)))(decodedFields)
        }
      })(objResult)
    }
    case hydra.core.Type.union(v_Type_union_rt) => {
      def decodeVariant(key: scala.Predef.String)(`val`: Option[hydra.json.model.Value])(ftype: hydra.core.Type): Either[scala.Predef.String, hydra.core.Term] =
        {
        val jsonVal: hydra.json.model.Value = maybes.fromMaybe[hydra.json.model.Value](hydra.json.model.Value.`null`)(`val`)
        val decoded: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(ftype)(jsonVal)
        eithers.map[hydra.core.Term, hydra.core.Term, scala.Predef.String]((v: hydra.core.Term) =>
          hydra.core.Term.union(hydra.core.Injection(tname, hydra.core.Field(key, v))))(decoded)
      }
      {
        def tryField(key: scala.Predef.String)(`val`: Option[hydra.json.model.Value])(ft: hydra.core.FieldType): Option[Either[scala.Predef.String,
           hydra.core.Term]] =
          logic.ifElse[Option[Either[scala.Predef.String, hydra.core.Term]]](equality.equal[scala.Predef.String](ft.name)(key))(Some(decodeVariant(key)(`val`)(ft.`type`)))(None)
        {
          def findAndDecode(key: scala.Predef.String)(`val`: Option[hydra.json.model.Value])(fts: Seq[hydra.core.FieldType]): Either[scala.Predef.String,
             hydra.core.Term] =
            logic.ifElse[Either[scala.Predef.String, hydra.core.Term]](lists.`null`[hydra.core.FieldType](fts))(Left(strings.cat(Seq("unknown variant: ",
               key))))(maybes.maybe[Either[scala.Predef.String, hydra.core.Term], Either[scala.Predef.String,
               hydra.core.Term]](findAndDecode(key)(`val`)(lists.tail[hydra.core.FieldType](fts)))((r: Either[scala.Predef.String,
               hydra.core.Term]) => r)(tryField(key)(`val`)(lists.head[hydra.core.FieldType](fts))))
          {
            def decodeSingleKey(obj: Map[scala.Predef.String, hydra.json.model.Value]): Either[scala.Predef.String, hydra.core.Term] =
              findAndDecode(lists.head[scala.Predef.String](maps.keys[scala.Predef.String, hydra.json.model.Value](obj)))(maps.lookup[scala.Predef.String,
                 hydra.json.model.Value](lists.head[scala.Predef.String](maps.keys[scala.Predef.String,
                 hydra.json.model.Value](obj)))(obj))(v_Type_union_rt)
            {
              def processUnion(obj: Map[scala.Predef.String, hydra.json.model.Value]): Either[scala.Predef.String, hydra.core.Term] =
                logic.ifElse[Either[scala.Predef.String, hydra.core.Term]](equality.equal[Int](lists.length[scala.Predef.String](maps.keys[scala.Predef.String,
                   hydra.json.model.Value](obj)))(1))(decodeSingleKey(obj))(Left("expected single-key object for union"))
              {
                val objResult: Either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value]] = hydra.json.decode.expectObject(value)
                eithers.either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value],
                   Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((obj: Map[scala.Predef.String,
                   hydra.json.model.Value]) => processUnion(obj))(objResult)
              }
            }
          }
        }
      }
    }
    case hydra.core.Type.unit => {
      val objResult: Either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value]] = hydra.json.decode.expectObject(value)
      eithers.map[Map[scala.Predef.String, hydra.json.model.Value], hydra.core.Term, scala.Predef.String]((_2: Map[scala.Predef.String,
         hydra.json.model.Value]) => hydra.core.Term.unit)(objResult)
    }
    case hydra.core.Type.wrap(v_Type_wrap_wn) => {
      val decoded: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(v_Type_wrap_wn)(value)
      eithers.map[hydra.core.Term, hydra.core.Term, scala.Predef.String]((v: hydra.core.Term) => hydra.core.Term.wrap(hydra.core.WrappedTerm(tname,
         v)))(decoded)
    }
    case hydra.core.Type.map(v_Type_map_mt) => {
      val keyType: hydra.core.Type = (v_Type_map_mt.keys)
      {
        val valType: hydra.core.Type = (v_Type_map_mt.values)
        {
          val arrResult: Either[scala.Predef.String, Seq[hydra.json.model.Value]] = hydra.json.decode.expectArray(value)
          eithers.either[scala.Predef.String, Seq[hydra.json.model.Value], Either[scala.Predef.String,
             hydra.core.Term]]((err: scala.Predef.String) => Left(err))((arr: Seq[hydra.json.model.Value]) =>
            {
            def decodeEntry(entryJson: hydra.json.model.Value): Either[scala.Predef.String, Tuple2[hydra.core.Term, hydra.core.Term]] =
              {
              val objResult: Either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value]] = hydra.json.decode.expectObject(entryJson)
              eithers.either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value], Either[scala.Predef.String,
                 Tuple2[hydra.core.Term, hydra.core.Term]]]((err: scala.Predef.String) => Left(err))((entryObj: Map[scala.Predef.String,
                 hydra.json.model.Value]) =>
                {
                val keyJson: Option[hydra.json.model.Value] = maps.lookup[scala.Predef.String, hydra.json.model.Value]("@key")(entryObj)
                {
                  val valJson: Option[hydra.json.model.Value] = maps.lookup[scala.Predef.String, hydra.json.model.Value]("@value")(entryObj)
                  maybes.maybe[Either[scala.Predef.String, Tuple2[hydra.core.Term, hydra.core.Term]],
                     hydra.json.model.Value](Left("missing @key in map entry"))((kj: hydra.json.model.Value) =>
                    maybes.maybe[Either[scala.Predef.String, Tuple2[hydra.core.Term, hydra.core.Term]],
                       hydra.json.model.Value](Left("missing @value in map entry"))((vj: hydra.json.model.Value) =>
                    {
                    val decodedKey: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(keyType)(kj)
                    {
                      val decodedVal: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(valType)(vj)
                      eithers.either[scala.Predef.String, hydra.core.Term, Either[scala.Predef.String,
                         Tuple2[hydra.core.Term, hydra.core.Term]]]((err: scala.Predef.String) => Left(err))((k: hydra.core.Term) =>
                        eithers.map[hydra.core.Term, Tuple2[hydra.core.Term, hydra.core.Term], scala.Predef.String]((v: hydra.core.Term) => Tuple2(k,
                           v))(decodedVal))(decodedKey)
                    }
                  })(valJson))(keyJson)
                }
              })(objResult)
            }
            {
              val entries: Either[scala.Predef.String, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]] = eithers.mapList[hydra.json.model.Value,
                 Tuple2[hydra.core.Term, hydra.core.Term], scala.Predef.String](decodeEntry)(arr)
              eithers.map[Seq[Tuple2[hydra.core.Term, hydra.core.Term]], hydra.core.Term, scala.Predef.String]((es: Seq[Tuple2[hydra.core.Term,
                 hydra.core.Term]]) =>
                hydra.core.Term.map(maps.fromList[hydra.core.Term, hydra.core.Term](es)))(entries)
            }
          })(arrResult)
        }
      }
    }
    case hydra.core.Type.pair(v_Type_pair_pt) => {
      val firstType: hydra.core.Type = (v_Type_pair_pt.first)
      {
        val secondType: hydra.core.Type = (v_Type_pair_pt.second)
        {
          val objResult: Either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value]] = hydra.json.decode.expectObject(value)
          eithers.either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value], Either[scala.Predef.String,
             hydra.core.Term]]((err: scala.Predef.String) => Left(err))((obj: Map[scala.Predef.String,
             hydra.json.model.Value]) =>
            {
            val firstJson: Option[hydra.json.model.Value] = maps.lookup[scala.Predef.String, hydra.json.model.Value]("@first")(obj)
            {
              val secondJson: Option[hydra.json.model.Value] = maps.lookup[scala.Predef.String, hydra.json.model.Value]("@second")(obj)
              maybes.maybe[Either[scala.Predef.String, hydra.core.Term], hydra.json.model.Value](Left("missing @first in pair"))((fj: hydra.json.model.Value) =>
                maybes.maybe[Either[scala.Predef.String, hydra.core.Term], hydra.json.model.Value](Left("missing @second in pair"))((sj: hydra.json.model.Value) =>
                {
                val decodedFirst: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(firstType)(fj)
                {
                  val decodedSecond: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(secondType)(sj)
                  eithers.either[scala.Predef.String, hydra.core.Term, Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((f: hydra.core.Term) =>
                    eithers.map[hydra.core.Term, hydra.core.Term, scala.Predef.String]((s: hydra.core.Term) => hydra.core.Term.pair(Tuple2(f,
                       s)))(decodedSecond))(decodedFirst)
                }
              })(secondJson))(firstJson)
            }
          })(objResult)
        }
      }
    }
    case hydra.core.Type.either(v_Type_either_et) => {
      val leftType: hydra.core.Type = (v_Type_either_et.left)
      {
        val rightType: hydra.core.Type = (v_Type_either_et.right)
        {
          val objResult: Either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value]] = hydra.json.decode.expectObject(value)
          eithers.either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value], Either[scala.Predef.String,
             hydra.core.Term]]((err: scala.Predef.String) => Left(err))((obj: Map[scala.Predef.String,
             hydra.json.model.Value]) =>
            {
            val leftJson: Option[hydra.json.model.Value] = maps.lookup[scala.Predef.String, hydra.json.model.Value]("@left")(obj)
            {
              val rightJson: Option[hydra.json.model.Value] = maps.lookup[scala.Predef.String, hydra.json.model.Value]("@right")(obj)
              maybes.maybe[Either[scala.Predef.String, hydra.core.Term], hydra.json.model.Value](maybes.maybe[Either[scala.Predef.String,
                 hydra.core.Term], hydra.json.model.Value](Left("expected @left or @right in Either"))((rj: hydra.json.model.Value) =>
                {
                val decoded: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(rightType)(rj)
                eithers.map[hydra.core.Term, hydra.core.Term, scala.Predef.String]((v: hydra.core.Term) => hydra.core.Term.either(Right(v)))(decoded)
              })(rightJson))((lj: hydra.json.model.Value) =>
                {
                val decoded: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(leftType)(lj)
                eithers.map[hydra.core.Term, hydra.core.Term, scala.Predef.String]((v: hydra.core.Term) => hydra.core.Term.either(Left(v)))(decoded)
              })(leftJson)
            }
          })(objResult)
        }
      }
    }
    case hydra.core.Type.variable(v_Type_variable_name) => {
      val lookedUp: Option[hydra.core.Type] = maps.lookup[hydra.core.Name, hydra.core.Type](v_Type_variable_name)(types)
      maybes.maybe[Either[scala.Predef.String, hydra.core.Term], hydra.core.Type](Left(strings.cat(Seq("unknown type variable: ",
         v_Type_variable_name))))((resolvedType: hydra.core.Type) =>
        hydra.json.decode.fromJson(types)(v_Type_variable_name)(resolvedType)(value))(lookedUp)
    }
    case _ => Left(strings.cat(Seq("unsupported type for JSON decoding: ", hydra.show.core.`type`(typ))))
}

def decodeLiteral(lt: hydra.core.LiteralType)(value: hydra.json.model.Value): Either[scala.Predef.String, hydra.core.Term] =
  lt match
  case hydra.core.LiteralType.binary => {
    val strResult: Either[scala.Predef.String, scala.Predef.String] = hydra.json.decode.expectString(value)
    eithers.map[scala.Predef.String, hydra.core.Term, scala.Predef.String]((s: scala.Predef.String) =>
      hydra.core.Term.literal(hydra.core.Literal.binary(literals.stringToBinary(s))))(strResult)
  }
  case hydra.core.LiteralType.boolean => value match
    case hydra.json.model.Value.boolean(v_Value_boolean_b) => Right(hydra.core.Term.literal(hydra.core.Literal.boolean(v_Value_boolean_b)))
    case _ => Left("expected boolean")
  case hydra.core.LiteralType.float(v_LiteralType_float_ft) => hydra.json.decode.decodeFloat(v_LiteralType_float_ft)(value)
  case hydra.core.LiteralType.integer(v_LiteralType_integer_it) => hydra.json.decode.decodeInteger(v_LiteralType_integer_it)(value)
  case hydra.core.LiteralType.string => {
    val strResult: Either[scala.Predef.String, scala.Predef.String] = hydra.json.decode.expectString(value)
    eithers.map[scala.Predef.String, hydra.core.Term, scala.Predef.String]((s: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(s)))(strResult)
  }

def decodeFloat(ft: hydra.core.FloatType)(value: hydra.json.model.Value): Either[scala.Predef.String, hydra.core.Term] =
  ft match
  case hydra.core.FloatType.bigfloat => {
    val numResult: Either[scala.Predef.String, BigDecimal] = hydra.json.decode.expectNumber(value)
    eithers.map[BigDecimal, hydra.core.Term, scala.Predef.String]((n: BigDecimal) =>
      hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(n))))(numResult)
  }
  case hydra.core.FloatType.float32 => {
    val strResult: Either[scala.Predef.String, scala.Predef.String] = hydra.json.decode.expectString(value)
    eithers.either[scala.Predef.String, scala.Predef.String, Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((s: scala.Predef.String) =>
      {
      val parsed: Option[Float] = literals.readFloat32(s)
      maybes.maybe[Either[scala.Predef.String, hydra.core.Term], Float](Left(strings.cat(Seq("invalid float32: ", s))))((v: Float) =>
        Right(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(v)))))(parsed)
    })(strResult)
  }
  case hydra.core.FloatType.float64 => {
    val numResult: Either[scala.Predef.String, BigDecimal] = hydra.json.decode.expectNumber(value)
    eithers.map[BigDecimal, hydra.core.Term, scala.Predef.String]((n: BigDecimal) =>
      hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(literals.bigfloatToFloat64(n)))))(numResult)
  }

def decodeInteger(it: hydra.core.IntegerType)(value: hydra.json.model.Value): Either[scala.Predef.String, hydra.core.Term] =
  it match
  case hydra.core.IntegerType.bigint => {
    val strResult: Either[scala.Predef.String, scala.Predef.String] = hydra.json.decode.expectString(value)
    eithers.either[scala.Predef.String, scala.Predef.String, Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((s: scala.Predef.String) =>
      {
      val parsed: Option[BigInt] = literals.readBigint(s)
      maybes.maybe[Either[scala.Predef.String, hydra.core.Term], BigInt](Left(strings.cat(Seq("invalid bigint: ", s))))((v: BigInt) =>
        Right(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(v)))))(parsed)
    })(strResult)
  }
  case hydra.core.IntegerType.int64 => {
    val strResult: Either[scala.Predef.String, scala.Predef.String] = hydra.json.decode.expectString(value)
    eithers.either[scala.Predef.String, scala.Predef.String, Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((s: scala.Predef.String) =>
      {
      val parsed: Option[Long] = literals.readInt64(s)
      maybes.maybe[Either[scala.Predef.String, hydra.core.Term], Long](Left(strings.cat(Seq("invalid int64: ", s))))((v: Long) =>
        Right(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int64(v)))))(parsed)
    })(strResult)
  }
  case hydra.core.IntegerType.uint32 => {
    val strResult: Either[scala.Predef.String, scala.Predef.String] = hydra.json.decode.expectString(value)
    eithers.either[scala.Predef.String, scala.Predef.String, Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((s: scala.Predef.String) =>
      {
      val parsed: Option[Long] = literals.readUint32(s)
      maybes.maybe[Either[scala.Predef.String, hydra.core.Term], Long](Left(strings.cat(Seq("invalid uint32: ", s))))((v: Long) =>
        Right(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint32(v)))))(parsed)
    })(strResult)
  }
  case hydra.core.IntegerType.uint64 => {
    val strResult: Either[scala.Predef.String, scala.Predef.String] = hydra.json.decode.expectString(value)
    eithers.either[scala.Predef.String, scala.Predef.String, Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((s: scala.Predef.String) =>
      {
      val parsed: Option[BigInt] = literals.readUint64(s)
      maybes.maybe[Either[scala.Predef.String, hydra.core.Term], BigInt](Left(strings.cat(Seq("invalid uint64: ", s))))((v: BigInt) =>
        Right(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint64(v)))))(parsed)
    })(strResult)
  }
  case hydra.core.IntegerType.int8 => {
    val numResult: Either[scala.Predef.String, BigDecimal] = hydra.json.decode.expectNumber(value)
    eithers.map[BigDecimal, hydra.core.Term, scala.Predef.String]((n: BigDecimal) =>
      hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int8(literals.bigintToInt8(literals.bigfloatToBigint(n))))))(numResult)
  }
  case hydra.core.IntegerType.int16 => {
    val numResult: Either[scala.Predef.String, BigDecimal] = hydra.json.decode.expectNumber(value)
    eithers.map[BigDecimal, hydra.core.Term, scala.Predef.String]((n: BigDecimal) =>
      hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int16(literals.bigintToInt16(literals.bigfloatToBigint(n))))))(numResult)
  }
  case hydra.core.IntegerType.int32 => {
    val numResult: Either[scala.Predef.String, BigDecimal] = hydra.json.decode.expectNumber(value)
    eithers.map[BigDecimal, hydra.core.Term, scala.Predef.String]((n: BigDecimal) =>
      hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(literals.bigintToInt32(literals.bigfloatToBigint(n))))))(numResult)
  }
  case hydra.core.IntegerType.uint8 => {
    val numResult: Either[scala.Predef.String, BigDecimal] = hydra.json.decode.expectNumber(value)
    eithers.map[BigDecimal, hydra.core.Term, scala.Predef.String]((n: BigDecimal) =>
      hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint8(literals.bigintToUint8(literals.bigfloatToBigint(n))))))(numResult)
  }
  case hydra.core.IntegerType.uint16 => {
    val numResult: Either[scala.Predef.String, BigDecimal] = hydra.json.decode.expectNumber(value)
    eithers.map[BigDecimal, hydra.core.Term, scala.Predef.String]((n: BigDecimal) =>
      hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint16(literals.bigintToUint16(literals.bigfloatToBigint(n))))))(numResult)
  }

def expectString(value: hydra.json.model.Value): Either[scala.Predef.String, scala.Predef.String] =
  value match
  case hydra.json.model.Value.string(v_Value_string_s) => Right(v_Value_string_s)
  case _ => Left("expected string")

def expectArray(value: hydra.json.model.Value): Either[scala.Predef.String, Seq[hydra.json.model.Value]] =
  value match
  case hydra.json.model.Value.array(v_Value_array_arr) => Right(v_Value_array_arr)
  case _ => Left("expected array")

def expectObject(value: hydra.json.model.Value): Either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value]] =
  value match
  case hydra.json.model.Value.`object`(v_Value_object_obj) => Right(v_Value_object_obj)
  case _ => Left("expected object")

def expectNumber(value: hydra.json.model.Value): Either[scala.Predef.String, BigDecimal] =
  value match
  case hydra.json.model.Value.number(v_Value_number_n) => Right(v_Value_number_n)
  case _ => Left("expected number")
