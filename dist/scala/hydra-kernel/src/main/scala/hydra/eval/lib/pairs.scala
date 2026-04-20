package hydra.eval.lib.pairs

import hydra.core.*

import hydra.errors.*

def bimap[T0, T1](cx: T0)(g: T1)(firstFun: hydra.core.Term)(secondFun: hydra.core.Term)(pairTerm: hydra.core.Term): Either[hydra.errors.Error,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term] =
  pairTerm match
  case hydra.core.Term.pair(v_Term_pair_p) => {
    lazy val fst: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)
    {
      lazy val snd: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p)
      Right(hydra.core.Term.pair(Tuple2(hydra.core.Term.application(hydra.core.Application(firstFun,
         fst)), hydra.core.Term.application(hydra.core.Application(secondFun, snd)))))
    }
  }
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("pair value",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(pairTerm)))))

def first[T0, T1](cx: T0)(g: T1)(pairTerm: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
  pairTerm match
  case hydra.core.Term.pair(v_Term_pair_p) => Right(hydra.lib.pairs.first[hydra.core.Term,
     hydra.core.Term](v_Term_pair_p))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("pair value",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(pairTerm)))))

def second[T0, T1](cx: T0)(g: T1)(pairTerm: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
  pairTerm match
  case hydra.core.Term.pair(v_Term_pair_p) => Right(hydra.lib.pairs.second[hydra.core.Term,
     hydra.core.Term](v_Term_pair_p))
  case _ => Left(hydra.errors.Error.extraction(hydra.errors.ExtractionError.unexpectedShape(hydra.errors.UnexpectedShapeError("pair value",
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.show.core.term(pairTerm)))))
