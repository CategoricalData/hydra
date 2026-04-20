package hydra.json.encode

import hydra.core.*

import hydra.json.model.*

def encodeFloat(fv: hydra.core.FloatValue): Either[scala.Predef.String, hydra.json.model.Value] =
  fv match
  case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_bf) => {
    lazy val s: scala.Predef.String = hydra.lib.literals.showBigfloat(v_FloatValue_bigfloat_bf)
    hydra.lib.logic.ifElse[Either[scala.Predef.String, hydra.json.model.Value]](hydra.json.encode.requiresJsonStringSentinel(s))(Left(hydra.lib.strings.cat(Seq("JSON cannot represent bigfloat value: ",
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       s))))(Right(hydra.json.model.Value.number(hydra.lib.literals.float64ToDecimal(hydra.lib.literals.bigfloatToFloat64(v_FloatValue_bigfloat_bf)))))
  }
  case hydra.core.FloatValue.float32(v_FloatValue_float32_f) => Right(hydra.json.model.Value.string(hydra.lib.literals.showFloat32(v_FloatValue_float32_f)))
  case hydra.core.FloatValue.float64(v_FloatValue_float64_f) => {
    lazy val s: scala.Predef.String = hydra.lib.literals.showFloat64(v_FloatValue_float64_f)
    hydra.lib.logic.ifElse[Either[scala.Predef.String, hydra.json.model.Value]](hydra.json.encode.requiresJsonStringSentinel(s))(Right(hydra.json.model.Value.string(s)))(Right(hydra.json.model.Value.number(hydra.lib.literals.float64ToDecimal(v_FloatValue_float64_f))))
  }

def encodeInteger[T0](iv: hydra.core.IntegerValue): Either[T0, hydra.json.model.Value] =
  iv match
  case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_bi) => Right(hydra.json.model.Value.string(hydra.lib.literals.showBigint(v_IntegerValue_bigint_bi)))
  case hydra.core.IntegerValue.int64(v_IntegerValue_int64_i) => Right(hydra.json.model.Value.string(hydra.lib.literals.showInt64(v_IntegerValue_int64_i)))
  case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_i) => Right(hydra.json.model.Value.string(hydra.lib.literals.showUint32(v_IntegerValue_uint32_i)))
  case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_i) => Right(hydra.json.model.Value.string(hydra.lib.literals.showUint64(v_IntegerValue_uint64_i)))
  case hydra.core.IntegerValue.int8(v_IntegerValue_int8_i) => Right(hydra.json.model.Value.number(hydra.lib.literals.bigintToDecimal(hydra.lib.literals.int8ToBigint(v_IntegerValue_int8_i))))
  case hydra.core.IntegerValue.int16(v_IntegerValue_int16_i) => Right(hydra.json.model.Value.number(hydra.lib.literals.bigintToDecimal(hydra.lib.literals.int16ToBigint(v_IntegerValue_int16_i))))
  case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(hydra.json.model.Value.number(hydra.lib.literals.bigintToDecimal(hydra.lib.literals.int32ToBigint(v_IntegerValue_int32_i))))
  case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_i) => Right(hydra.json.model.Value.number(hydra.lib.literals.bigintToDecimal(hydra.lib.literals.uint8ToBigint(v_IntegerValue_uint8_i))))
  case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_i) => Right(hydra.json.model.Value.number(hydra.lib.literals.bigintToDecimal(hydra.lib.literals.uint16ToBigint(v_IntegerValue_uint16_i))))

def encodeLiteral(lit: hydra.core.Literal): Either[scala.Predef.String, hydra.json.model.Value] =
  lit match
  case hydra.core.Literal.binary(v_Literal_binary_b) => Right(hydra.json.model.Value.string(hydra.lib.literals.binaryToString(v_Literal_binary_b)))
  case hydra.core.Literal.boolean(v_Literal_boolean_b) => Right(hydra.json.model.Value.boolean(v_Literal_boolean_b))
  case hydra.core.Literal.decimal(v_Literal_decimal_d) => Right(hydra.json.model.Value.number(v_Literal_decimal_d))
  case hydra.core.Literal.float(v_Literal_float_f) => hydra.json.encode.encodeFloat(v_Literal_float_f)
  case hydra.core.Literal.integer(v_Literal_integer_i) => hydra.json.encode.encodeInteger(v_Literal_integer_i)
  case hydra.core.Literal.string(v_Literal_string_s) => Right(hydra.json.model.Value.string(v_Literal_string_s))

def requiresJsonStringSentinel(s: scala.Predef.String): Boolean =
  hydra.lib.logic.or(hydra.lib.equality.equal[scala.Predef.String](s)("NaN"))(hydra.lib.logic.or(hydra.lib.equality.equal[scala.Predef.String](s)("Infinity"))(hydra.lib.logic.or(hydra.lib.equality.equal[scala.Predef.String](s)("-Infinity"))(hydra.lib.equality.equal[scala.Predef.String](s)("-0.0"))))

def toJson(types: Map[hydra.core.Name, hydra.core.Type])(tname: hydra.core.Name)(typ: hydra.core.Type)(term: hydra.core.Term): Either[scala.Predef.String,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.json.model.Value] =
  {
  lazy val stripped: hydra.core.Type = hydra.strip.deannotateType(typ)
  lazy val strippedTerm: hydra.core.Term = hydra.strip.deannotateTerm(term)
  stripped match
    case hydra.core.Type.literal(v_Type_literal__) => strippedTerm match
      case hydra.core.Term.literal(v_Term_literal_lit) => hydra.json.encode.encodeLiteral(v_Term_literal_lit)
      case _ => Left("expected literal term")
    case hydra.core.Type.list(v_Type_list_elemType) => strippedTerm match
      case hydra.core.Term.list(v_Term_list_terms) => {
        lazy val results: Either[scala.Predef.String, Seq[hydra.json.model.Value]] = hydra.lib.eithers.mapList[hydra.core.Term,
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           hydra.json.model.Value, scala.Predef.String]((t: hydra.core.Term) =>
          hydra.json.encode.toJson(types)(tname)(v_Type_list_elemType)(t))(v_Term_list_terms)
        hydra.lib.eithers.map[Seq[hydra.json.model.Value], hydra.json.model.Value,
           scala.Predef.String]((vs: Seq[hydra.json.model.Value]) => hydra.json.model.Value.array(vs))(results)
      }
      case _ => Left("expected list term")
    case hydra.core.Type.set(v_Type_set_elemType) => strippedTerm match
      case hydra.core.Term.set(v_Term_set_vals) => {
        lazy val terms: Seq[hydra.core.Term] = hydra.lib.sets.toList[hydra.core.Term](v_Term_set_vals)
        {
          lazy val results: Either[scala.Predef.String, Seq[hydra.json.model.Value]] = hydra.lib.eithers.mapList[hydra.core.Term,
            
            
            
            
            
            
            
            
            
            
            
            
            
            
             hydra.json.model.Value, scala.Predef.String]((t: hydra.core.Term) =>
            hydra.json.encode.toJson(types)(tname)(v_Type_set_elemType)(t))(terms)
          hydra.lib.eithers.map[Seq[hydra.json.model.Value], hydra.json.model.Value,
             scala.Predef.String]((vs: Seq[hydra.json.model.Value]) => hydra.json.model.Value.array(vs))(results)
        }
      }
      case _ => Left("expected set term")
    case hydra.core.Type.maybe(v_Type_maybe_innerType) => {
      lazy val innerStripped: hydra.core.Type = hydra.strip.deannotateType(v_Type_maybe_innerType)
      {
        lazy val isNestedMaybe: Boolean = innerStripped match
          case hydra.core.Type.maybe(v_Type_maybe__) => true
          case _ => false
        strippedTerm match
          case hydra.core.Term.maybe(v_Term_maybe_opt) => hydra.lib.maybes.maybe[Either[scala.Predef.String,
             hydra.json.model.Value], hydra.core.Term](Right(hydra.json.model.Value.`null`))((v: hydra.core.Term) =>
            {
            lazy val encoded: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(types)(tname)(v_Type_maybe_innerType)(v)
            hydra.lib.logic.ifElse[Either[scala.Predef.String, hydra.json.model.Value]](isNestedMaybe)(hydra.lib.eithers.map[hydra.json.model.Value,
              
              
              
              
              
              
              
              
              
              
              
              
              
              
               hydra.json.model.Value, scala.Predef.String]((ev: hydra.json.model.Value) => hydra.json.model.Value.array(Seq(ev)))(encoded))(encoded)
          })(v_Term_maybe_opt)
          case _ => Left("expected maybe term")
      }
    }
    case hydra.core.Type.record(v_Type_record_rt) => strippedTerm match
      case hydra.core.Term.record(v_Term_record_r) => {
        def isSimpleMaybe(ftype: hydra.core.Type): Boolean =
          hydra.strip.deannotateType(ftype) match
          case hydra.core.Type.maybe(v_Type_maybe_innerT) => hydra.strip.deannotateType(v_Type_maybe_innerT) match
            case hydra.core.Type.maybe(v_Type_maybe__) => false
            case _ => true
          case _ => false
        {
          def encodeFieldWithType(ft: hydra.core.FieldType)(f: hydra.core.Field): Either[scala.Predef.String,
             Option[Tuple2[scala.Predef.String, hydra.json.model.Value]]] =
            {
            lazy val fname: scala.Predef.String = (f.name)
            lazy val fterm: hydra.core.Term = (f.term)
            lazy val ftype: hydra.core.Type = (ft.`type`)
            hydra.lib.logic.ifElse[Either[scala.Predef.String, Option[Tuple2[scala.Predef.String,
               hydra.json.model.Value]]]](isSimpleMaybe(ftype))(hydra.strip.deannotateTerm(fterm) match
              case hydra.core.Term.maybe(v_Term_maybe_opt) => hydra.lib.maybes.maybe[Either[scala.Predef.String,
                 Option[Tuple2[scala.Predef.String, hydra.json.model.Value]]], hydra.core.Term](Right(None))((v: hydra.core.Term) =>
                {
                lazy val innerType: hydra.core.Type = hydra.strip.deannotateType(ftype) match
                  case hydra.core.Type.maybe(v_Type_maybe_it) => v_Type_maybe_it
                  case _ => ftype
                {
                  lazy val encoded: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(types)(tname)(innerType)(v)
                  hydra.lib.eithers.map[hydra.json.model.Value, Option[Tuple2[scala.Predef.String,
                     hydra.json.model.Value]], scala.Predef.String]((ev: hydra.json.model.Value) => Some(Tuple2(fname,
                     ev)))(encoded)
                }
              })(v_Term_maybe_opt)
              case _ => Left("expected maybe term for optional field"))({
              lazy val encoded: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(types)(tname)(ftype)(fterm)
              hydra.lib.eithers.map[hydra.json.model.Value, Option[Tuple2[scala.Predef.String,
                 hydra.json.model.Value]], scala.Predef.String]((ev: hydra.json.model.Value) => Some(Tuple2(fname,
                 ev)))(encoded)
            })
          }
          {
            lazy val fieldTypes: Seq[hydra.core.FieldType] = v_Type_record_rt
            {
              lazy val fields: Seq[hydra.core.Field] = (v_Term_record_r.fields)
              {
                lazy val encodedPairs: Either[scala.Predef.String, Seq[Option[Tuple2[scala.Predef.String,
                   hydra.json.model.Value]]]] = hydra.lib.eithers.mapList[Tuple2[hydra.core.FieldType,
                   hydra.core.Field], Option[Tuple2[scala.Predef.String, hydra.json.model.Value]],
                   scala.Predef.String]((ftf: Tuple2[hydra.core.FieldType, hydra.core.Field]) =>
                  encodeFieldWithType(hydra.lib.pairs.first[hydra.core.FieldType,
                     hydra.core.Field](ftf))(hydra.lib.pairs.second[hydra.core.FieldType,
                     hydra.core.Field](ftf)))(hydra.lib.lists.zip[hydra.core.FieldType,
                     hydra.core.Field](fieldTypes)(fields))
                hydra.lib.eithers.map[Seq[Option[Tuple2[scala.Predef.String, hydra.json.model.Value]]],
                   hydra.json.model.Value, scala.Predef.String]((pairs: Seq[Option[Tuple2[scala.Predef.String,
                   hydra.json.model.Value]]]) =>
                  hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String,
                     hydra.json.model.Value](hydra.lib.maybes.cat[Tuple2[scala.Predef.String,
                     hydra.json.model.Value]](pairs))))(encodedPairs)
              }
            }
          }
        }
      }
      case _ => Left("expected record term")
    case hydra.core.Type.union(v_Type_union_rt) => strippedTerm match
      case hydra.core.Term.inject(v_Term_inject_inj) => {
        lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
        {
          lazy val fname: scala.Predef.String = (field.name)
          {
            lazy val fterm: hydra.core.Term = (field.term)
            {
              lazy val ftypeResult: Either[scala.Predef.String, hydra.core.Type] = hydra.lib.maybes.maybe[Either[scala.Predef.String,
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                 hydra.core.Type], hydra.core.FieldType](Left(hydra.lib.strings.cat(Seq("unknown variant: ",
                 fname))))((ft: hydra.core.FieldType) => Right(ft.`type`))(hydra.lib.lists.find[hydra.core.FieldType]((ft: hydra.core.FieldType) =>
                hydra.lib.equality.equal[scala.Predef.String](ft.name)(fname))(v_Type_union_rt))
              hydra.lib.eithers.either[scala.Predef.String, hydra.core.Type, Either[scala.Predef.String,
                 hydra.json.model.Value]]((err: scala.Predef.String) => Left(err))((ftype: hydra.core.Type) =>
                {
                lazy val encodedUnion: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(types)(tname)(ftype)(fterm)
                hydra.lib.eithers.map[hydra.json.model.Value, hydra.json.model.Value,
                   scala.Predef.String]((v: hydra.json.model.Value) =>
                  hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String,
                     hydra.json.model.Value](Seq(Tuple2(fname, v)))))(encodedUnion)
              })(ftypeResult)
            }
          }
        }
      }
      case _ => Left("expected union term")
    case hydra.core.Type.unit => Right(hydra.json.model.Value.`object`(hydra.lib.maps.empty[scala.Predef.String,
       hydra.json.model.Value]))
    case hydra.core.Type.wrap(v_Type_wrap_wn) => strippedTerm match
      case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.json.encode.toJson(types)(tname)(v_Type_wrap_wn)(v_Term_wrap_wt.body)
      case _ => Left("expected wrapped term")
    case hydra.core.Type.map(v_Type_map_mt) => {
      lazy val keyType: hydra.core.Type = (v_Type_map_mt.keys)
      {
        lazy val valType: hydra.core.Type = (v_Type_map_mt.values)
        strippedTerm match
          case hydra.core.Term.map(v_Term_map_m) => {
            def encodeEntry(kv: Tuple2[hydra.core.Term, hydra.core.Term]): Either[scala.Predef.String,
               hydra.json.model.Value] =
              {
              lazy val k: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](kv)
              lazy val v: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kv)
              lazy val encodedK: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(types)(tname)(keyType)(k)
              lazy val encodedV: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(types)(tname)(valType)(v)
              hydra.lib.eithers.either[scala.Predef.String, hydra.json.model.Value,
                 Either[scala.Predef.String, hydra.json.model.Value]]((err: scala.Predef.String) => Left(err))((ek: hydra.json.model.Value) =>
                hydra.lib.eithers.map[hydra.json.model.Value, hydra.json.model.Value,
                   scala.Predef.String]((ev: hydra.json.model.Value) =>
                hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String,
                   hydra.json.model.Value](Seq(Tuple2("@key", ek), Tuple2("@value",
                   ev)))))(encodedV))(encodedK)
            }
            {
              lazy val entries: Either[scala.Predef.String, Seq[hydra.json.model.Value]] = hydra.lib.eithers.mapList[Tuple2[hydra.core.Term,
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                 hydra.core.Term], hydra.json.model.Value, scala.Predef.String](encodeEntry)(hydra.lib.maps.toList[hydra.core.Term,
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                 hydra.core.Term](v_Term_map_m))
              hydra.lib.eithers.map[Seq[hydra.json.model.Value], hydra.json.model.Value,
                 scala.Predef.String]((es: Seq[hydra.json.model.Value]) => hydra.json.model.Value.array(es))(entries)
            }
          }
          case _ => Left("expected map term")
      }
    }
    case hydra.core.Type.pair(v_Type_pair_pt) => {
      lazy val firstType: hydra.core.Type = (v_Type_pair_pt.first)
      {
        lazy val secondType: hydra.core.Type = (v_Type_pair_pt.second)
        strippedTerm match
          case hydra.core.Term.pair(v_Term_pair_p) => {
            lazy val first: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)
            {
              lazy val second: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p)
              {
                lazy val encodedFirst: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(types)(tname)(firstType)(first)
                {
                  lazy val encodedSecond: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(types)(tname)(secondType)(second)
                  hydra.lib.eithers.either[scala.Predef.String, hydra.json.model.Value,
                     Either[scala.Predef.String, hydra.json.model.Value]]((err: scala.Predef.String) => Left(err))((ef: hydra.json.model.Value) =>
                    hydra.lib.eithers.map[hydra.json.model.Value, hydra.json.model.Value,
                       scala.Predef.String]((es: hydra.json.model.Value) =>
                    hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String,
                       hydra.json.model.Value](Seq(Tuple2("@first", ef), Tuple2("@second",
                       es)))))(encodedSecond))(encodedFirst)
                }
              }
            }
          }
          case _ => Left("expected pair term")
      }
    }
    case hydra.core.Type.either(v_Type_either_et) => {
      lazy val leftType: hydra.core.Type = (v_Type_either_et.left)
      {
        lazy val rightType: hydra.core.Type = (v_Type_either_et.right)
        strippedTerm match
          case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term,
             hydra.core.Term, Either[scala.Predef.String, hydra.json.model.Value]]((l: hydra.core.Term) =>
            {
            lazy val encodedL: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(types)(tname)(leftType)(l)
            hydra.lib.eithers.map[hydra.json.model.Value, hydra.json.model.Value,
               scala.Predef.String]((v: hydra.json.model.Value) =>
              hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String,
                 hydra.json.model.Value](Seq(Tuple2("@left", v)))))(encodedL)
          })((r: hydra.core.Term) =>
            {
            lazy val encodedR: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJson(types)(tname)(rightType)(r)
            hydra.lib.eithers.map[hydra.json.model.Value, hydra.json.model.Value,
               scala.Predef.String]((v: hydra.json.model.Value) =>
              hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String,
                 hydra.json.model.Value](Seq(Tuple2("@right", v)))))(encodedR)
          })(v_Term_either_e)
          case _ => Left("expected either term")
      }
    }
    case hydra.core.Type.variable(v_Type_variable_name) => {
      lazy val lookedUp: Option[hydra.core.Type] = hydra.lib.maps.lookup[hydra.core.Name,
         hydra.core.Type](v_Type_variable_name)(types)
      hydra.lib.maybes.maybe[Either[scala.Predef.String, hydra.json.model.Value],
         hydra.core.Type](hydra.json.encode.toJsonUntyped(term))((resolvedType: hydra.core.Type) =>
        hydra.json.encode.toJson(types)(v_Type_variable_name)(resolvedType)(term))(lookedUp)
    }
    case _ => Left(hydra.lib.strings.cat(Seq("unsupported type for JSON encoding: ", hydra.show.core.`type`(typ))))
}

def toJsonUntyped(term: hydra.core.Term): Either[scala.Predef.String, hydra.json.model.Value] =
  {
  lazy val stripped: hydra.core.Term = hydra.strip.deannotateTerm(term)
  stripped match
    case hydra.core.Term.literal(v_Term_literal_lit) => hydra.json.encode.encodeLiteral(v_Term_literal_lit)
    case hydra.core.Term.list(v_Term_list_terms) => {
      lazy val results: Either[scala.Predef.String, Seq[hydra.json.model.Value]] = hydra.lib.eithers.mapList[hydra.core.Term,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.json.model.Value, scala.Predef.String]((t: hydra.core.Term) => hydra.json.encode.toJsonUntyped(t))(v_Term_list_terms)
      hydra.lib.eithers.map[Seq[hydra.json.model.Value], hydra.json.model.Value, scala.Predef.String]((vs: Seq[hydra.json.model.Value]) => hydra.json.model.Value.array(vs))(results)
    }
    case hydra.core.Term.set(v_Term_set_vals) => {
      lazy val terms: Seq[hydra.core.Term] = hydra.lib.sets.toList[hydra.core.Term](v_Term_set_vals)
      {
        lazy val results: Either[scala.Predef.String, Seq[hydra.json.model.Value]] = hydra.lib.eithers.mapList[hydra.core.Term,
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           hydra.json.model.Value, scala.Predef.String]((t: hydra.core.Term) => hydra.json.encode.toJsonUntyped(t))(terms)
        hydra.lib.eithers.map[Seq[hydra.json.model.Value], hydra.json.model.Value,
           scala.Predef.String]((vs: Seq[hydra.json.model.Value]) => hydra.json.model.Value.array(vs))(results)
      }
    }
    case hydra.core.Term.maybe(v_Term_maybe_opt) => hydra.lib.maybes.maybe[Either[scala.Predef.String,
       hydra.json.model.Value], hydra.core.Term](Right(hydra.json.model.Value.`null`))((v: hydra.core.Term) =>
      {
      lazy val encodedMaybe: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJsonUntyped(v)
      hydra.lib.eithers.map[hydra.json.model.Value, hydra.json.model.Value, scala.Predef.String]((encoded: hydra.json.model.Value) => hydra.json.model.Value.array(Seq(encoded)))(encodedMaybe)
    })(v_Term_maybe_opt)
    case hydra.core.Term.record(v_Term_record_r) => {
      def encodeField(f: hydra.core.Field): Either[scala.Predef.String, Tuple2[scala.Predef.String,
         hydra.json.model.Value]] =
        {
        lazy val fname: scala.Predef.String = (f.name)
        lazy val fterm: hydra.core.Term = (f.term)
        lazy val encodedField: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJsonUntyped(fterm)
        hydra.lib.eithers.map[hydra.json.model.Value, Tuple2[scala.Predef.String,
           hydra.json.model.Value], scala.Predef.String]((v: hydra.json.model.Value) => Tuple2(fname,
           v))(encodedField)
      }
      {
        lazy val fields: Seq[hydra.core.Field] = (v_Term_record_r.fields)
        {
          lazy val encodedFields: Either[scala.Predef.String, Seq[Tuple2[scala.Predef.String,
             hydra.json.model.Value]]] = hydra.lib.eithers.mapList[hydra.core.Field,
             Tuple2[scala.Predef.String, hydra.json.model.Value], scala.Predef.String](encodeField)(fields)
          hydra.lib.eithers.map[Seq[Tuple2[scala.Predef.String, hydra.json.model.Value]],
             hydra.json.model.Value, scala.Predef.String]((fs: Seq[Tuple2[scala.Predef.String,
             hydra.json.model.Value]]) =>
            hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String,
               hydra.json.model.Value](fs)))(encodedFields)
        }
      }
    }
    case hydra.core.Term.inject(v_Term_inject_inj) => {
      lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
      {
        lazy val fname: scala.Predef.String = (field.name)
        {
          lazy val fterm: hydra.core.Term = (field.term)
          {
            lazy val encodedUnion: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJsonUntyped(fterm)
            hydra.lib.eithers.map[hydra.json.model.Value, hydra.json.model.Value,
               scala.Predef.String]((v: hydra.json.model.Value) =>
              hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String,
                 hydra.json.model.Value](Seq(Tuple2(fname, v)))))(encodedUnion)
          }
        }
      }
    }
    case hydra.core.Term.unit => Right(hydra.json.model.Value.`object`(hydra.lib.maps.empty[scala.Predef.String,
       hydra.json.model.Value]))
    case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.json.encode.toJsonUntyped(v_Term_wrap_wt.body)
    case hydra.core.Term.map(v_Term_map_m) => {
      def encodeEntry(kv: Tuple2[hydra.core.Term, hydra.core.Term]): Either[scala.Predef.String,
         hydra.json.model.Value] =
        {
        lazy val k: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](kv)
        lazy val v: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kv)
        lazy val encodedK: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJsonUntyped(k)
        lazy val encodedV: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJsonUntyped(v)
        hydra.lib.eithers.either[scala.Predef.String, hydra.json.model.Value, Either[scala.Predef.String,
           hydra.json.model.Value]]((err: scala.Predef.String) => Left(err))((ek: hydra.json.model.Value) =>
          hydra.lib.eithers.map[hydra.json.model.Value, hydra.json.model.Value, scala.Predef.String]((ev: hydra.json.model.Value) =>
          hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String,
             hydra.json.model.Value](Seq(Tuple2("@key", ek), Tuple2("@value", ev)))))(encodedV))(encodedK)
      }
      {
        lazy val entries: Either[scala.Predef.String, Seq[hydra.json.model.Value]] = hydra.lib.eithers.mapList[Tuple2[hydra.core.Term,
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           hydra.core.Term], hydra.json.model.Value, scala.Predef.String](encodeEntry)(hydra.lib.maps.toList[hydra.core.Term,
             
             
             
             
             
             
             
             
             
             
             
             
             
             
           hydra.core.Term](v_Term_map_m))
        hydra.lib.eithers.map[Seq[hydra.json.model.Value], hydra.json.model.Value,
           scala.Predef.String]((es: Seq[hydra.json.model.Value]) => hydra.json.model.Value.array(es))(entries)
      }
    }
    case hydra.core.Term.pair(v_Term_pair_p) => {
      lazy val first: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)
      {
        lazy val second: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p)
        {
          lazy val encodedFirst: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJsonUntyped(first)
          {
            lazy val encodedSecond: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJsonUntyped(second)
            hydra.lib.eithers.either[scala.Predef.String, hydra.json.model.Value,
               Either[scala.Predef.String, hydra.json.model.Value]]((err: scala.Predef.String) => Left(err))((ef: hydra.json.model.Value) =>
              hydra.lib.eithers.map[hydra.json.model.Value, hydra.json.model.Value,
                 scala.Predef.String]((es: hydra.json.model.Value) =>
              hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String,
                 hydra.json.model.Value](Seq(Tuple2("@first", ef), Tuple2("@second",
                 es)))))(encodedSecond))(encodedFirst)
          }
        }
      }
    }
    case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term,
       hydra.core.Term, Either[scala.Predef.String, hydra.json.model.Value]]((l: hydra.core.Term) =>
      {
      lazy val encodedL: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJsonUntyped(l)
      hydra.lib.eithers.map[hydra.json.model.Value, hydra.json.model.Value, scala.Predef.String]((v: hydra.json.model.Value) =>
        hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String,
           hydra.json.model.Value](Seq(Tuple2("@left", v)))))(encodedL)
    })((r: hydra.core.Term) =>
      {
      lazy val encodedR: Either[scala.Predef.String, hydra.json.model.Value] = hydra.json.encode.toJsonUntyped(r)
      hydra.lib.eithers.map[hydra.json.model.Value, hydra.json.model.Value, scala.Predef.String]((v: hydra.json.model.Value) =>
        hydra.json.model.Value.`object`(hydra.lib.maps.fromList[scala.Predef.String,
           hydra.json.model.Value](Seq(Tuple2("@right", v)))))(encodedR)
    })(v_Term_either_e)
    case _ => Left(hydra.lib.strings.cat(Seq("unsupported term variant for JSON encoding: ",
       hydra.show.core.term(term))))
}
