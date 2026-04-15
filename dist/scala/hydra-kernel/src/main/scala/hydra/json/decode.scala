package hydra.json.decode

import hydra.core.*

import hydra.json.model.*

def decodeFloat(ft: hydra.core.FloatType)(value: hydra.json.model.Value): Either[scala.Predef.String, hydra.core.Term] =
  ft match
  case hydra.core.FloatType.bigfloat => value match
    case hydra.json.model.Value.number(v_Value_number_n) => Right(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.bigfloat(v_Value_number_n))))
    case _ => Left("expected number for bigfloat")
  case hydra.core.FloatType.float32 => {
    lazy val strResult: Either[scala.Predef.String, scala.Predef.String] = hydra.json.decode.expectString(value)
    hydra.lib.eithers.either[scala.Predef.String, scala.Predef.String, Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((s: scala.Predef.String) =>
      {
      lazy val parsed: Option[Float] = hydra.lib.literals.readFloat32(s)
      hydra.lib.maybes.maybe[Either[scala.Predef.String, hydra.core.Term], Float](Left(hydra.lib.strings.cat(Seq("invalid float32: ", s))))((v: Float) =>
        Right(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float32(v)))))(parsed)
    })(strResult)
  }
  case hydra.core.FloatType.float64 => value match
    case hydra.json.model.Value.number(v_Value_number_n) => Right(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(hydra.lib.literals.bigfloatToFloat64(v_Value_number_n)))))
    case hydra.json.model.Value.string(v_Value_string_s) => hydra.lib.maybes.maybe[Either[scala.Predef.String,
       hydra.core.Term], Double](Left(hydra.lib.strings.cat(Seq("invalid float64 sentinel: ", v_Value_string_s))))((v: Double) =>
      Right(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(v)))))(hydra.json.decode.parseSpecialFloat(v_Value_string_s))
    case _ => Left("expected number or special float string for float64")

def decodeInteger(it: hydra.core.IntegerType)(value: hydra.json.model.Value): Either[scala.Predef.String, hydra.core.Term] =
  it match
  case hydra.core.IntegerType.bigint => {
    lazy val strResult: Either[scala.Predef.String, scala.Predef.String] = hydra.json.decode.expectString(value)
    hydra.lib.eithers.either[scala.Predef.String, scala.Predef.String, Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((s: scala.Predef.String) =>
      {
      lazy val parsed: Option[BigInt] = hydra.lib.literals.readBigint(s)
      hydra.lib.maybes.maybe[Either[scala.Predef.String, hydra.core.Term], BigInt](Left(hydra.lib.strings.cat(Seq("invalid bigint: ", s))))((v: BigInt) =>
        Right(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.bigint(v)))))(parsed)
    })(strResult)
  }
  case hydra.core.IntegerType.int64 => {
    lazy val strResult: Either[scala.Predef.String, scala.Predef.String] = hydra.json.decode.expectString(value)
    hydra.lib.eithers.either[scala.Predef.String, scala.Predef.String, Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((s: scala.Predef.String) =>
      {
      lazy val parsed: Option[Long] = hydra.lib.literals.readInt64(s)
      hydra.lib.maybes.maybe[Either[scala.Predef.String, hydra.core.Term], Long](Left(hydra.lib.strings.cat(Seq("invalid int64: ", s))))((v: Long) =>
        Right(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int64(v)))))(parsed)
    })(strResult)
  }
  case hydra.core.IntegerType.uint32 => {
    lazy val strResult: Either[scala.Predef.String, scala.Predef.String] = hydra.json.decode.expectString(value)
    hydra.lib.eithers.either[scala.Predef.String, scala.Predef.String, Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((s: scala.Predef.String) =>
      {
      lazy val parsed: Option[Long] = hydra.lib.literals.readUint32(s)
      hydra.lib.maybes.maybe[Either[scala.Predef.String, hydra.core.Term], Long](Left(hydra.lib.strings.cat(Seq("invalid uint32: ", s))))((v: Long) =>
        Right(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint32(v)))))(parsed)
    })(strResult)
  }
  case hydra.core.IntegerType.uint64 => {
    lazy val strResult: Either[scala.Predef.String, scala.Predef.String] = hydra.json.decode.expectString(value)
    hydra.lib.eithers.either[scala.Predef.String, scala.Predef.String, Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((s: scala.Predef.String) =>
      {
      lazy val parsed: Option[BigInt] = hydra.lib.literals.readUint64(s)
      hydra.lib.maybes.maybe[Either[scala.Predef.String, hydra.core.Term], BigInt](Left(hydra.lib.strings.cat(Seq("invalid uint64: ", s))))((v: BigInt) =>
        Right(hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint64(v)))))(parsed)
    })(strResult)
  }
  case hydra.core.IntegerType.int8 => {
    lazy val numResult: Either[scala.Predef.String, BigDecimal] = hydra.json.decode.expectNumber(value)
    hydra.lib.eithers.map[BigDecimal, hydra.core.Term, scala.Predef.String]((n: BigDecimal) =>
      hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int8(hydra.lib.literals.bigintToInt8(hydra.lib.literals.bigfloatToBigint(n))))))(numResult)
  }
  case hydra.core.IntegerType.int16 => {
    lazy val numResult: Either[scala.Predef.String, BigDecimal] = hydra.json.decode.expectNumber(value)
    hydra.lib.eithers.map[BigDecimal, hydra.core.Term, scala.Predef.String]((n: BigDecimal) =>
      hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int16(hydra.lib.literals.bigintToInt16(hydra.lib.literals.bigfloatToBigint(n))))))(numResult)
  }
  case hydra.core.IntegerType.int32 => {
    lazy val numResult: Either[scala.Predef.String, BigDecimal] = hydra.json.decode.expectNumber(value)
    hydra.lib.eithers.map[BigDecimal, hydra.core.Term, scala.Predef.String]((n: BigDecimal) =>
      hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(hydra.lib.literals.bigintToInt32(hydra.lib.literals.bigfloatToBigint(n))))))(numResult)
  }
  case hydra.core.IntegerType.uint8 => {
    lazy val numResult: Either[scala.Predef.String, BigDecimal] = hydra.json.decode.expectNumber(value)
    hydra.lib.eithers.map[BigDecimal, hydra.core.Term, scala.Predef.String]((n: BigDecimal) =>
      hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint8(hydra.lib.literals.bigintToUint8(hydra.lib.literals.bigfloatToBigint(n))))))(numResult)
  }
  case hydra.core.IntegerType.uint16 => {
    lazy val numResult: Either[scala.Predef.String, BigDecimal] = hydra.json.decode.expectNumber(value)
    hydra.lib.eithers.map[BigDecimal, hydra.core.Term, scala.Predef.String]((n: BigDecimal) =>
      hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.uint16(hydra.lib.literals.bigintToUint16(hydra.lib.literals.bigfloatToBigint(n))))))(numResult)
  }

def decodeLiteral(lt: hydra.core.LiteralType)(value: hydra.json.model.Value): Either[scala.Predef.String, hydra.core.Term] =
  lt match
  case hydra.core.LiteralType.binary => {
    lazy val strResult: Either[scala.Predef.String, scala.Predef.String] = hydra.json.decode.expectString(value)
    hydra.lib.eithers.map[scala.Predef.String, hydra.core.Term, scala.Predef.String]((s: scala.Predef.String) =>
      hydra.core.Term.literal(hydra.core.Literal.binary(hydra.lib.literals.stringToBinary(s))))(strResult)
  }
  case hydra.core.LiteralType.boolean => value match
    case hydra.json.model.Value.boolean(v_Value_boolean_b) => Right(hydra.core.Term.literal(hydra.core.Literal.boolean(v_Value_boolean_b)))
    case _ => Left("expected boolean")
  case hydra.core.LiteralType.decimal => value match
    case hydra.json.model.Value.number(v_Value_number_n) => Right(hydra.core.Term.literal(hydra.core.Literal.decimal(hydra.lib.literals.float64ToDecimal(hydra.lib.literals.bigfloatToFloat64(v_Value_number_n)))))
    case _ => Left("expected number for decimal")
  case hydra.core.LiteralType.float(v_LiteralType_float_ft) => hydra.json.decode.decodeFloat(v_LiteralType_float_ft)(value)
  case hydra.core.LiteralType.integer(v_LiteralType_integer_it) => hydra.json.decode.decodeInteger(v_LiteralType_integer_it)(value)
  case hydra.core.LiteralType.string => {
    lazy val strResult: Either[scala.Predef.String, scala.Predef.String] = hydra.json.decode.expectString(value)
    hydra.lib.eithers.map[scala.Predef.String, hydra.core.Term, scala.Predef.String]((s: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(s)))(strResult)
  }

def expectArray(value: hydra.json.model.Value): Either[scala.Predef.String, Seq[hydra.json.model.Value]] =
  value match
  case hydra.json.model.Value.array(v_Value_array_arr) => Right(v_Value_array_arr)
  case _ => Left("expected array")

def expectNumber(value: hydra.json.model.Value): Either[scala.Predef.String, BigDecimal] =
  value match
  case hydra.json.model.Value.number(v_Value_number_n) => Right(v_Value_number_n)
  case _ => Left("expected number")

def expectObject(value: hydra.json.model.Value): Either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value]] =
  value match
  case hydra.json.model.Value.`object`(v_Value_object_obj) => Right(v_Value_object_obj)
  case _ => Left("expected object")

def expectString(value: hydra.json.model.Value): Either[scala.Predef.String, scala.Predef.String] =
  value match
  case hydra.json.model.Value.string(v_Value_string_s) => Right(v_Value_string_s)
  case _ => Left("expected string")

def fromJson(types: Map[hydra.core.Name, hydra.core.Type])(tname: hydra.core.Name)(typ: hydra.core.Type)(value: hydra.json.model.Value): Either[scala.Predef.String,
   hydra.core.Term] =
  {
  lazy val stripped: hydra.core.Type = hydra.strip.deannotateType(typ)
  stripped match
    case hydra.core.Type.literal(v_Type_literal_lt) => hydra.json.decode.decodeLiteral(v_Type_literal_lt)(value)
    case hydra.core.Type.list(v_Type_list_elemType) => {
      def decodeElem(v: hydra.json.model.Value): Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(v_Type_list_elemType)(v)
      {
        lazy val arrResult: Either[scala.Predef.String, Seq[hydra.json.model.Value]] = hydra.json.decode.expectArray(value)
        hydra.lib.eithers.either[scala.Predef.String, Seq[hydra.json.model.Value], Either[scala.Predef.String,
           hydra.core.Term]]((err: scala.Predef.String) => Left(err))((arr: Seq[hydra.json.model.Value]) =>
          {
          lazy val decoded: Either[scala.Predef.String, Seq[hydra.core.Term]] = hydra.lib.eithers.mapList[hydra.json.model.Value,
             hydra.core.Term, scala.Predef.String](decodeElem)(arr)
          hydra.lib.eithers.map[Seq[hydra.core.Term], hydra.core.Term, scala.Predef.String]((ts: Seq[hydra.core.Term]) => hydra.core.Term.list(ts))(decoded)
        })(arrResult)
      }
    }
    case hydra.core.Type.set(v_Type_set_elemType) => {
      def decodeElem(v: hydra.json.model.Value): Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(v_Type_set_elemType)(v)
      {
        lazy val arrResult: Either[scala.Predef.String, Seq[hydra.json.model.Value]] = hydra.json.decode.expectArray(value)
        hydra.lib.eithers.either[scala.Predef.String, Seq[hydra.json.model.Value], Either[scala.Predef.String,
           hydra.core.Term]]((err: scala.Predef.String) => Left(err))((arr: Seq[hydra.json.model.Value]) =>
          {
          lazy val decoded: Either[scala.Predef.String, Seq[hydra.core.Term]] = hydra.lib.eithers.mapList[hydra.json.model.Value,
             hydra.core.Term, scala.Predef.String](decodeElem)(arr)
          hydra.lib.eithers.map[Seq[hydra.core.Term], hydra.core.Term, scala.Predef.String]((elems: Seq[hydra.core.Term]) =>
            hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](elems)))(decoded)
        })(arrResult)
      }
    }
    case hydra.core.Type.maybe(v_Type_maybe_innerType) => {
      lazy val innerStripped: hydra.core.Type = hydra.strip.deannotateType(v_Type_maybe_innerType)
      {
        lazy val isNestedMaybe: Boolean = innerStripped match
          case hydra.core.Type.maybe(v_Type_maybe__) => true
          case _ => false
        hydra.lib.logic.ifElse[Either[scala.Predef.String, hydra.core.Term]](isNestedMaybe)({
          def decodeJust(arr: Seq[hydra.json.model.Value]): Either[scala.Predef.String, hydra.core.Term] =
            hydra.lib.eithers.map[hydra.core.Term, hydra.core.Term, scala.Predef.String]((v: hydra.core.Term) => hydra.core.Term.maybe(Some(v)))(hydra.json.decode.fromJson(types)(tname)(v_Type_maybe_innerType)(hydra.lib.lists.head[hydra.json.model.Value](arr)))
          {
            def decodeMaybeArray(arr: Seq[hydra.json.model.Value]): Either[scala.Predef.String, hydra.core.Term] =
              {
              lazy val len: Int = hydra.lib.lists.length[hydra.json.model.Value](arr)
              hydra.lib.logic.ifElse[Either[scala.Predef.String, hydra.core.Term]](hydra.lib.equality.equal[Int](len)(0))(Right(hydra.core.Term.maybe(None)))(hydra.lib.logic.ifElse[Either[scala.Predef.String,
                 hydra.core.Term]](hydra.lib.equality.equal[Int](len)(1))(decodeJust(arr))(Left("expected single-element array for Just")))
            }
            value match
              case hydra.json.model.Value.`null` => Right(hydra.core.Term.maybe(None))
              case hydra.json.model.Value.array(v_Value_array_arr) => decodeMaybeArray(v_Value_array_arr)
              case _ => Left("expected null or single-element array for nested Maybe")
          }
        })(value match
          case hydra.json.model.Value.`null` => Right(hydra.core.Term.maybe(None))
          case _ => hydra.lib.eithers.map[hydra.core.Term, hydra.core.Term, scala.Predef.String]((v: hydra.core.Term) => hydra.core.Term.maybe(Some(v)))(hydra.json.decode.fromJson(types)(tname)(v_Type_maybe_innerType)(value)))
      }
    }
    case hydra.core.Type.record(v_Type_record_rt) => {
      lazy val objResult: Either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value]] = hydra.json.decode.expectObject(value)
      hydra.lib.eithers.either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value],
         Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((obj: Map[scala.Predef.String,
         hydra.json.model.Value]) =>
        {
        def decodeField(ft: hydra.core.FieldType): Either[scala.Predef.String, hydra.core.Field] =
          {
          lazy val fname: hydra.core.Name = (ft.name)
          lazy val ftype: hydra.core.Type = (ft.`type`)
          lazy val mval: Option[hydra.json.model.Value] = hydra.lib.maps.lookup[scala.Predef.String, hydra.json.model.Value](fname)(obj)
          lazy val defaultVal: hydra.json.model.Value = hydra.json.model.Value.`null`
          lazy val jsonVal: hydra.json.model.Value = hydra.lib.maybes.fromMaybe[hydra.json.model.Value](defaultVal)(mval)
          lazy val decoded: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(ftype)(jsonVal)
          hydra.lib.eithers.map[hydra.core.Term, hydra.core.Field, scala.Predef.String]((v: hydra.core.Term) => hydra.core.Field(fname, v))(decoded)
        }
        {
          lazy val decodedFields: Either[scala.Predef.String, Seq[hydra.core.Field]] = hydra.lib.eithers.mapList[hydra.core.FieldType,
             hydra.core.Field, scala.Predef.String](decodeField)(v_Type_record_rt)
          hydra.lib.eithers.map[Seq[hydra.core.Field], hydra.core.Term, scala.Predef.String]((fs: Seq[hydra.core.Field]) => hydra.core.Term.record(hydra.core.Record(tname,
             fs)))(decodedFields)
        }
      })(objResult)
    }
    case hydra.core.Type.union(v_Type_union_rt) => {
      def decodeVariant(key: scala.Predef.String)(`val`: Option[hydra.json.model.Value])(ftype: hydra.core.Type): Either[scala.Predef.String, hydra.core.Term] =
        {
        lazy val jsonVal: hydra.json.model.Value = hydra.lib.maybes.fromMaybe[hydra.json.model.Value](hydra.json.model.Value.`null`)(`val`)
        lazy val decoded: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(ftype)(jsonVal)
        hydra.lib.eithers.map[hydra.core.Term, hydra.core.Term, scala.Predef.String]((v: hydra.core.Term) =>
          hydra.core.Term.inject(hydra.core.Injection(tname, hydra.core.Field(key, v))))(decoded)
      }
      {
        def tryField(key: scala.Predef.String)(`val`: Option[hydra.json.model.Value])(ft: hydra.core.FieldType): Option[Either[scala.Predef.String,
           hydra.core.Term]] =
          hydra.lib.logic.ifElse[Option[Either[scala.Predef.String, hydra.core.Term]]](hydra.lib.equality.equal[scala.Predef.String](ft.name)(key))(Some(decodeVariant(key)(`val`)(ft.`type`)))(None)
        {
          def findAndDecode(key: scala.Predef.String)(`val`: Option[hydra.json.model.Value])(fts: Seq[hydra.core.FieldType]): Either[scala.Predef.String,
             hydra.core.Term] =
            hydra.lib.logic.ifElse[Either[scala.Predef.String, hydra.core.Term]](hydra.lib.lists.`null`[hydra.core.FieldType](fts))(Left(hydra.lib.strings.cat(Seq("unknown variant: ",
               key))))(hydra.lib.maybes.maybe[Either[scala.Predef.String, hydra.core.Term], Either[scala.Predef.String,
               hydra.core.Term]](findAndDecode(key)(`val`)(hydra.lib.lists.tail[hydra.core.FieldType](fts)))((r: Either[scala.Predef.String,
               hydra.core.Term]) => r)(tryField(key)(`val`)(hydra.lib.lists.head[hydra.core.FieldType](fts))))
          {
            def decodeSingleKey(obj: Map[scala.Predef.String, hydra.json.model.Value]): Either[scala.Predef.String, hydra.core.Term] =
              findAndDecode(hydra.lib.lists.head[scala.Predef.String](hydra.lib.maps.keys[scala.Predef.String,
                 hydra.json.model.Value](obj)))(hydra.lib.maps.lookup[scala.Predef.String, hydra.json.model.Value](hydra.lib.lists.head[scala.Predef.String](hydra.lib.maps.keys[scala.Predef.String,
                 hydra.json.model.Value](obj)))(obj))(v_Type_union_rt)
            {
              def processUnion(obj: Map[scala.Predef.String, hydra.json.model.Value]): Either[scala.Predef.String, hydra.core.Term] =
                hydra.lib.logic.ifElse[Either[scala.Predef.String, hydra.core.Term]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[scala.Predef.String](hydra.lib.maps.keys[scala.Predef.String,
                   hydra.json.model.Value](obj)))(1))(decodeSingleKey(obj))(Left("expected single-key object for union"))
              {
                lazy val objResult: Either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value]] = hydra.json.decode.expectObject(value)
                hydra.lib.eithers.either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value],
                   Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((obj: Map[scala.Predef.String,
                   hydra.json.model.Value]) => processUnion(obj))(objResult)
              }
            }
          }
        }
      }
    }
    case hydra.core.Type.unit => {
      lazy val objResult: Either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value]] = hydra.json.decode.expectObject(value)
      hydra.lib.eithers.map[Map[scala.Predef.String, hydra.json.model.Value], hydra.core.Term, scala.Predef.String]((_2: Map[scala.Predef.String,
         hydra.json.model.Value]) => hydra.core.Term.unit)(objResult)
    }
    case hydra.core.Type.wrap(v_Type_wrap_wn) => {
      lazy val decoded: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(v_Type_wrap_wn)(value)
      hydra.lib.eithers.map[hydra.core.Term, hydra.core.Term, scala.Predef.String]((v: hydra.core.Term) => hydra.core.Term.wrap(hydra.core.WrappedTerm(tname,
         v)))(decoded)
    }
    case hydra.core.Type.map(v_Type_map_mt) => {
      lazy val keyType: hydra.core.Type = (v_Type_map_mt.keys)
      {
        lazy val valType: hydra.core.Type = (v_Type_map_mt.values)
        {
          lazy val arrResult: Either[scala.Predef.String, Seq[hydra.json.model.Value]] = hydra.json.decode.expectArray(value)
          hydra.lib.eithers.either[scala.Predef.String, Seq[hydra.json.model.Value], Either[scala.Predef.String,
             hydra.core.Term]]((err: scala.Predef.String) => Left(err))((arr: Seq[hydra.json.model.Value]) =>
            {
            def decodeEntry(entryJson: hydra.json.model.Value): Either[scala.Predef.String, Tuple2[hydra.core.Term, hydra.core.Term]] =
              {
              lazy val objResult: Either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value]] = hydra.json.decode.expectObject(entryJson)
              hydra.lib.eithers.either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value],
                 Either[scala.Predef.String, Tuple2[hydra.core.Term, hydra.core.Term]]]((err: scala.Predef.String) => Left(err))((entryObj: Map[scala.Predef.String,
                 hydra.json.model.Value]) =>
                {
                lazy val keyJson: Option[hydra.json.model.Value] = hydra.lib.maps.lookup[scala.Predef.String, hydra.json.model.Value]("@key")(entryObj)
                {
                  lazy val valJson: Option[hydra.json.model.Value] = hydra.lib.maps.lookup[scala.Predef.String, hydra.json.model.Value]("@value")(entryObj)
                  hydra.lib.maybes.maybe[Either[scala.Predef.String, Tuple2[hydra.core.Term, hydra.core.Term]],
                     hydra.json.model.Value](Left("missing @key in map entry"))((kj: hydra.json.model.Value) =>
                    hydra.lib.maybes.maybe[Either[scala.Predef.String, Tuple2[hydra.core.Term, hydra.core.Term]],
                       hydra.json.model.Value](Left("missing @value in map entry"))((vj: hydra.json.model.Value) =>
                    {
                    lazy val decodedKey: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(keyType)(kj)
                    {
                      lazy val decodedVal: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(valType)(vj)
                      hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[scala.Predef.String,
                         Tuple2[hydra.core.Term, hydra.core.Term]]]((err: scala.Predef.String) => Left(err))((k: hydra.core.Term) =>
                        hydra.lib.eithers.map[hydra.core.Term, Tuple2[hydra.core.Term, hydra.core.Term],
                           scala.Predef.String]((v: hydra.core.Term) => Tuple2(k, v))(decodedVal))(decodedKey)
                    }
                  })(valJson))(keyJson)
                }
              })(objResult)
            }
            {
              lazy val entries: Either[scala.Predef.String, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]] = hydra.lib.eithers.mapList[hydra.json.model.Value,
                 Tuple2[hydra.core.Term, hydra.core.Term], scala.Predef.String](decodeEntry)(arr)
              hydra.lib.eithers.map[Seq[Tuple2[hydra.core.Term, hydra.core.Term]], hydra.core.Term, scala.Predef.String]((es: Seq[Tuple2[hydra.core.Term,
                 hydra.core.Term]]) =>
                hydra.core.Term.map(hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](es)))(entries)
            }
          })(arrResult)
        }
      }
    }
    case hydra.core.Type.pair(v_Type_pair_pt) => {
      lazy val firstType: hydra.core.Type = (v_Type_pair_pt.first)
      {
        lazy val secondType: hydra.core.Type = (v_Type_pair_pt.second)
        {
          lazy val objResult: Either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value]] = hydra.json.decode.expectObject(value)
          hydra.lib.eithers.either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value],
             Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((obj: Map[scala.Predef.String,
             hydra.json.model.Value]) =>
            {
            lazy val firstJson: Option[hydra.json.model.Value] = hydra.lib.maps.lookup[scala.Predef.String, hydra.json.model.Value]("@first")(obj)
            {
              lazy val secondJson: Option[hydra.json.model.Value] = hydra.lib.maps.lookup[scala.Predef.String, hydra.json.model.Value]("@second")(obj)
              hydra.lib.maybes.maybe[Either[scala.Predef.String, hydra.core.Term], hydra.json.model.Value](Left("missing @first in pair"))((fj: hydra.json.model.Value) =>
                hydra.lib.maybes.maybe[Either[scala.Predef.String, hydra.core.Term], hydra.json.model.Value](Left("missing @second in pair"))((sj: hydra.json.model.Value) =>
                {
                lazy val decodedFirst: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(firstType)(fj)
                {
                  lazy val decodedSecond: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(secondType)(sj)
                  hydra.lib.eithers.either[scala.Predef.String, hydra.core.Term, Either[scala.Predef.String,
                     hydra.core.Term]]((err: scala.Predef.String) => Left(err))((f: hydra.core.Term) =>
                    hydra.lib.eithers.map[hydra.core.Term, hydra.core.Term, scala.Predef.String]((s: hydra.core.Term) => hydra.core.Term.pair(Tuple2(f,
                       s)))(decodedSecond))(decodedFirst)
                }
              })(secondJson))(firstJson)
            }
          })(objResult)
        }
      }
    }
    case hydra.core.Type.either(v_Type_either_et) => {
      lazy val leftType: hydra.core.Type = (v_Type_either_et.left)
      {
        lazy val rightType: hydra.core.Type = (v_Type_either_et.right)
        {
          lazy val objResult: Either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value]] = hydra.json.decode.expectObject(value)
          hydra.lib.eithers.either[scala.Predef.String, Map[scala.Predef.String, hydra.json.model.Value],
             Either[scala.Predef.String, hydra.core.Term]]((err: scala.Predef.String) => Left(err))((obj: Map[scala.Predef.String,
             hydra.json.model.Value]) =>
            {
            lazy val leftJson: Option[hydra.json.model.Value] = hydra.lib.maps.lookup[scala.Predef.String, hydra.json.model.Value]("@left")(obj)
            {
              lazy val rightJson: Option[hydra.json.model.Value] = hydra.lib.maps.lookup[scala.Predef.String, hydra.json.model.Value]("@right")(obj)
              hydra.lib.maybes.maybe[Either[scala.Predef.String, hydra.core.Term], hydra.json.model.Value](hydra.lib.maybes.maybe[Either[scala.Predef.String,
                 hydra.core.Term], hydra.json.model.Value](Left("expected @left or @right in Either"))((rj: hydra.json.model.Value) =>
                {
                lazy val decoded: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(rightType)(rj)
                hydra.lib.eithers.map[hydra.core.Term, hydra.core.Term, scala.Predef.String]((v: hydra.core.Term) => hydra.core.Term.either(Right(v)))(decoded)
              })(rightJson))((lj: hydra.json.model.Value) =>
                {
                lazy val decoded: Either[scala.Predef.String, hydra.core.Term] = hydra.json.decode.fromJson(types)(tname)(leftType)(lj)
                hydra.lib.eithers.map[hydra.core.Term, hydra.core.Term, scala.Predef.String]((v: hydra.core.Term) => hydra.core.Term.either(Left(v)))(decoded)
              })(leftJson)
            }
          })(objResult)
        }
      }
    }
    case hydra.core.Type.variable(v_Type_variable_name) => {
      lazy val lookedUp: Option[hydra.core.Type] = hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Type](v_Type_variable_name)(types)
      hydra.lib.maybes.maybe[Either[scala.Predef.String, hydra.core.Term], hydra.core.Type](Left(hydra.lib.strings.cat(Seq("unknown type variable: ",
         v_Type_variable_name))))((resolvedType: hydra.core.Type) =>
        hydra.json.decode.fromJson(types)(v_Type_variable_name)(resolvedType)(value))(lookedUp)
    }
    case _ => Left(hydra.lib.strings.cat(Seq("unsupported type for JSON decoding: ", hydra.show.core.`type`(typ))))
}

def parseSpecialFloat(s: scala.Predef.String): Option[Double] =
  hydra.lib.logic.ifElse[Option[Double]](hydra.lib.logic.or(hydra.lib.equality.equal[scala.Predef.String](s)("NaN"))(hydra.lib.logic.or(hydra.lib.equality.equal[scala.Predef.String](s)("Infinity"))(hydra.lib.logic.or(hydra.lib.equality.equal[scala.Predef.String](s)("-Infinity"))(hydra.lib.equality.equal[scala.Predef.String](s)("-0.0")))))(hydra.lib.literals.readFloat64(s))(None)
