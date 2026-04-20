package hydra.decode.classes

import hydra.classes.*

import hydra.core.*

import hydra.errors.*

def typeClass(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.classes.TypeClass] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.classes.TypeClass]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.inject(v_Term_inject_inj) => {
    lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.classes.TypeClass])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       hydra.classes.TypeClass]](Seq(Tuple2("equality", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.classes.TypeClass, hydra.errors.DecodingError]((t: Unit) => hydra.classes.TypeClass.equality)(hydra.extract.core.decodeUnit(cx)(input))),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         Tuple2("ordering", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[Unit, hydra.classes.TypeClass, hydra.errors.DecodingError]((t: Unit) => hydra.classes.TypeClass.ordering)(hydra.extract.core.decodeUnit(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.classes.TypeClass],
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.classes.TypeClass]](Left(hydra.lib.strings.cat(Seq("no such field ",
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.classes.TypeClass])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.classes.TypeClass]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))
