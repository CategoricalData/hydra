package hydra.eval.lib.maybes

import hydra.core.*

import hydra.errors.*

def apply[T0, T1](cx: T0)(g: T1)(funOptTerm: hydra.core.Term)(argOptTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  funOptTerm match
  case hydra.core.Term.maybe(v_Term_maybe_mf) => argOptTerm match
    case hydra.core.Term.maybe(v_Term_maybe_mx) => Right(hydra.core.Term.maybe(hydra.lib.maybes.bind[hydra.core.Term,
       hydra.core.Term](v_Term_maybe_mf)((f: hydra.core.Term) =>
      hydra.lib.maybes.map[hydra.core.Term, hydra.core.Term]((x: hydra.core.Term) => hydra.core.Term.application(hydra.core.Application(f,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         x)))(v_Term_maybe_mx))))
    case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("optional value",
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       hydra.show.core.term(argOptTerm)))))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("optional function",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(funOptTerm)))))

def bind[T0, T1](cx: T0)(g: T1)(optTerm: hydra.core.Term)(funTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  optTerm match
  case hydra.core.Term.maybe(v_Term_maybe_m) => Right(hydra.lib.maybes.maybe[hydra.core.Term,
     hydra.core.Term](hydra.core.Term.maybe(None))((`val`: hydra.core.Term) =>
    hydra.core.Term.application(hydra.core.Application(funTerm, `val`)))(v_Term_maybe_m))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("optional value",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(optTerm)))))

def cases[T0, T1](cx: T0)(g: T1)(optTerm: hydra.core.Term)(defaultTerm: hydra.core.Term)(funTerm: hydra.core.Term): Either[hydra.errors.Error,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term] =
  optTerm match
  case hydra.core.Term.maybe(v_Term_maybe_m) => Right(hydra.lib.maybes.maybe[hydra.core.Term,
     hydra.core.Term](defaultTerm)((`val`: hydra.core.Term) =>
    hydra.core.Term.application(hydra.core.Application(funTerm, `val`)))(v_Term_maybe_m))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("optional value",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(optTerm)))))

def cat[T0](cx: T0)(g: hydra.graph.Graph)(listTerm: hydra.core.Term): Either[hydra.errors.Error, Seq[hydra.core.Term]] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], Seq[hydra.core.Term]](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  Right(hydra.lib.lists.foldl[Seq[hydra.core.Term], hydra.core.Term]((acc: Seq[hydra.core.Term]) =>
  (el: hydra.core.Term) =>
  el match
  case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.lib.maybes.maybe[Seq[hydra.core.Term],
     hydra.core.Term](acc)((v: hydra.core.Term) =>
    hydra.lib.lists.concat2[hydra.core.Term](acc)(hydra.lib.lists.pure[hydra.core.Term](v)))(v_Term_maybe_m)
  case _ => acc)(Seq())(elements)))

def compose[T0, T1, T2](cx: T0)(g: T1)(funF: hydra.core.Term)(funG: hydra.core.Term)(xTerm: hydra.core.Term): Either[T2,
   hydra.core.Term] =
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.maybes.bind"),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.application(hydra.core.Application(funF, xTerm)))), funG)))

def fromJust[T0, T1](cx: T0)(g: T1)(optTerm: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
  optTerm match
  case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.lib.maybes.maybe[Either[hydra.errors.Error,
     hydra.core.Term], hydra.core.Term](Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("Just value",
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.show.core.term(optTerm))))))((`val`: hydra.core.Term) => Right(`val`))(v_Term_maybe_m)
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("optional value",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(optTerm)))))

def fromMaybe[T0, T1](cx: T0)(g: T1)(defaultTerm: hydra.core.Term)(optTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  optTerm match
  case hydra.core.Term.maybe(v_Term_maybe_m) => Right(hydra.lib.maybes.maybe[hydra.core.Term,
     hydra.core.Term](defaultTerm)((`val`: hydra.core.Term) => `val`)(v_Term_maybe_m))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("optional value",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(optTerm)))))

def isJust[T0, T1](cx: T0)(g: T1)(optTerm: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
  optTerm match
  case hydra.core.Term.maybe(v_Term_maybe_m) => Right(hydra.lib.maybes.maybe[hydra.core.Term,
     hydra.core.Term](hydra.core.Term.literal(hydra.core.Literal.boolean(false)))((_x: hydra.core.Term) => hydra.core.Term.literal(hydra.core.Literal.boolean(true)))(v_Term_maybe_m))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("optional value",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(optTerm)))))

def isNothing[T0, T1](cx: T0)(g: T1)(optTerm: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
  optTerm match
  case hydra.core.Term.maybe(v_Term_maybe_m) => Right(hydra.lib.maybes.maybe[hydra.core.Term,
     hydra.core.Term](hydra.core.Term.literal(hydra.core.Literal.boolean(true)))((_x: hydra.core.Term) => hydra.core.Term.literal(hydra.core.Literal.boolean(false)))(v_Term_maybe_m))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("optional value",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(optTerm)))))

def map[T0, T1](cx: T0)(g: T1)(funTerm: hydra.core.Term)(optTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  optTerm match
  case hydra.core.Term.maybe(v_Term_maybe_m) => Right(hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.core.Term,
     hydra.core.Term]((`val`: hydra.core.Term) =>
    hydra.core.Term.application(hydra.core.Application(funTerm, `val`)))(v_Term_maybe_m)))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("optional value",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(optTerm)))))

def mapMaybe[T0](cx: T0)(g: hydra.graph.Graph)(funTerm: hydra.core.Term)(listTerm: hydra.core.Term): Either[hydra.errors.Error,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.maybes.cat"),
     hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Term, hydra.core.Term]((el: hydra.core.Term) =>
  hydra.core.Term.application(hydra.core.Application(funTerm, el)))(elements))))))

def maybe[T0, T1](cx: T0)(g: T1)(defaultTerm: hydra.core.Term)(funTerm: hydra.core.Term)(optTerm: hydra.core.Term): Either[hydra.errors.Error,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term] =
  optTerm match
  case hydra.core.Term.maybe(v_Term_maybe_m) => Right(hydra.lib.maybes.maybe[hydra.core.Term,
     hydra.core.Term](defaultTerm)((`val`: hydra.core.Term) =>
    hydra.core.Term.application(hydra.core.Application(funTerm, `val`)))(v_Term_maybe_m))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("optional value",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(optTerm)))))

def pure[T0, T1, T2](cx: T0)(g: T1)(x: hydra.core.Term): Either[T2, hydra.core.Term] = Right(hydra.core.Term.maybe(Some(x)))

def toList[T0, T1](cx: T0)(g: T1)(optTerm: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
  optTerm match
  case hydra.core.Term.maybe(v_Term_maybe_m) => Right(hydra.core.Term.list(hydra.lib.maybes.maybe[Seq[hydra.core.Term],
     hydra.core.Term](Seq())((`val`: hydra.core.Term) => hydra.lib.lists.pure[hydra.core.Term](`val`))(v_Term_maybe_m)))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("optional value",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(optTerm)))))
