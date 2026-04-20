package hydra.eval.lib.lists

import hydra.core.*

def apply[T0](cx: T0)(g: hydra.graph.Graph)(funsTerm: hydra.core.Term)(argsTerm: hydra.core.Term): Either[hydra.errors.Error,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(funsTerm))((funs: Seq[hydra.core.Term]) =>
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(argsTerm))((arguments: Seq[hydra.core.Term]) =>
  {
  def applyOne(f: hydra.core.Term): Seq[hydra.core.Term] =
    hydra.lib.lists.map[hydra.core.Term, hydra.core.Term]((arg: hydra.core.Term) => hydra.core.Term.application(hydra.core.Application(f,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       arg)))(arguments)
  Right(hydra.core.Term.list(hydra.lib.lists.concat[hydra.core.Term](hydra.lib.lists.map[hydra.core.Term,
     Seq[hydra.core.Term]](applyOne)(funs))))
}))

def bind[T0](cx: T0)(g: hydra.graph.Graph)(listTerm: hydra.core.Term)(funTerm: hydra.core.Term): Either[hydra.errors.Error,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.concat"),
     hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Term, hydra.core.Term]((el: hydra.core.Term) =>
  hydra.core.Term.application(hydra.core.Application(funTerm, el)))(elements))))))

def concat2[T0, T1, T2](cx: T0)(g: T1)(list1: hydra.core.Term)(list2: hydra.core.Term): Either[T2, hydra.core.Term] =
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.concat"),
     hydra.core.Term.list(Seq(list1, list2)))))

def dropWhile[T0, T1, T2](cx: T0)(g: T1)(predTerm: hydra.core.Term)(listTerm: hydra.core.Term): Either[T2,
   hydra.core.Term] =
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.second"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.span"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     predTerm)), listTerm)))))

def elem[T0, T1, T2](cx: T0)(g: T1)(x: hydra.core.Term)(listTerm: hydra.core.Term): Either[T2, hydra.core.Term] =
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.maybes.isJust"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.find"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.equality.equal"),
     x)))), listTerm)))))

def filter[T0](cx: T0)(g: hydra.graph.Graph)(predTerm: hydra.core.Term)(listTerm: hydra.core.Term): Either[hydra.errors.Error,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.concat"),
     hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Term, hydra.core.Term]((el: hydra.core.Term) =>
  hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.logic.ifElse"),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.application(hydra.core.Application(predTerm, el)))), hydra.core.Term.list(hydra.lib.lists.pure[hydra.core.Term](el)))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.list(Seq()))))(elements))))))

def find[T0, T1, T2](cx: T0)(g: T1)(predTerm: hydra.core.Term)(listTerm: hydra.core.Term): Either[T2, hydra.core.Term] =
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.maybeHead"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.filter"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     predTerm)), listTerm)))))

def foldl(cx: hydra.context.Context)(g: hydra.graph.Graph)(funTerm: hydra.core.Term)(initTerm: hydra.core.Term)(listTerm: hydra.core.Term): Either[hydra.errors.Error,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  hydra.lib.lists.foldl[Either[hydra.errors.Error, hydra.core.Term], hydra.core.Term]((acc: Either[hydra.errors.Error,
     hydra.core.Term]) =>
  (el: hydra.core.Term) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](acc)((reducedAcc: hydra.core.Term) =>
  hydra.reduction.reduceTerm(cx)(g)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(funTerm,
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     reducedAcc)), el)))))(Right(initTerm))(elements))

def foldr(cx: hydra.context.Context)(g: hydra.graph.Graph)(funTerm: hydra.core.Term)(initTerm: hydra.core.Term)(listTerm: hydra.core.Term): Either[hydra.errors.Error,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  hydra.lib.lists.foldr[hydra.core.Term, Either[hydra.errors.Error, hydra.core.Term]]((el: hydra.core.Term) =>
  (acc: Either[hydra.errors.Error, hydra.core.Term]) =>
  hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Term, hydra.core.Term](acc)((reducedAcc: hydra.core.Term) =>
  hydra.reduction.reduceTerm(cx)(g)(true)(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(funTerm,
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     el)), reducedAcc)))))(Right(initTerm))(elements))

def group[T0, T1, T2](cx: T0)(g: T1)(listTerm: hydra.core.Term): Either[T2, hydra.core.Term] =
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.lambda(hydra.core.Lambda("foldResult",
     None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.logic.ifElse"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.null"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.first"),
     hydra.core.Term.variable("foldResult"))))))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.second"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.variable("foldResult"))))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.concat2"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.second"),
     hydra.core.Term.variable("foldResult"))))), hydra.core.Term.list(Seq(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.first"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.variable("foldResult"))))))))))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.foldl"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.lambda(hydra.core.Lambda("acc", None, hydra.core.Term.lambda(hydra.core.Lambda("el",
     None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.maybes.maybe"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.pair(Tuple2(hydra.core.Term.list(Seq(hydra.core.Term.variable("el"))),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.second"),
     hydra.core.Term.variable("acc"))))))), hydra.core.Term.lambda(hydra.core.Lambda("h",
     None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.logic.ifElse"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.equality.equal"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.variable("el"))), hydra.core.Term.variable("h"))))), hydra.core.Term.pair(Tuple2(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.concat2"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.first"),
     hydra.core.Term.variable("acc"))))), hydra.core.Term.list(Seq(hydra.core.Term.variable("el"))))),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.second"),
     hydra.core.Term.variable("acc"))))))), hydra.core.Term.pair(Tuple2(hydra.core.Term.list(Seq(hydra.core.Term.variable("el"))),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.concat2"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.second"),
     hydra.core.Term.variable("acc"))))), hydra.core.Term.list(Seq(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.first"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.variable("acc"))))))))))))))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.maybeHead"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.first"),
     hydra.core.Term.variable("acc"))))))))))))), hydra.core.Term.pair(Tuple2(hydra.core.Term.list(Seq()),
     hydra.core.Term.list(Seq()))))), listTerm)))))

def intercalate[T0, T1, T2](cx: T0)(g: T1)(sep: hydra.core.Term)(listsTerm: hydra.core.Term): Either[T2,
   hydra.core.Term] =
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.concat"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.intersperse"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     sep)), listsTerm)))))

def intersperse[T0](cx: T0)(g: hydra.graph.Graph)(sep: hydra.core.Term)(listTerm: hydra.core.Term): Either[hydra.errors.Error,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  Right(hydra.core.Term.list(hydra.lib.maybes.maybe[Seq[hydra.core.Term], Tuple2[hydra.core.Term,
     Seq[hydra.core.Term]]](Seq())((p: Tuple2[hydra.core.Term, Seq[hydra.core.Term]]) =>
  hydra.lib.lists.cons[hydra.core.Term](hydra.lib.pairs.first[hydra.core.Term, Seq[hydra.core.Term]](p))(hydra.lib.lists.concat[hydra.core.Term](hydra.lib.lists.map[hydra.core.Term,
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     Seq[hydra.core.Term]]((el: hydra.core.Term) => Seq(sep, el))(hydra.lib.pairs.second[hydra.core.Term,
     Seq[hydra.core.Term]](p)))))(hydra.lib.lists.uncons[hydra.core.Term](elements)))))

def map[T0](cx: T0)(g: hydra.graph.Graph)(funTerm: hydra.core.Term)(listTerm: hydra.core.Term): Either[hydra.errors.Error,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  Right(hydra.core.Term.list(hydra.lib.lists.reverse[hydra.core.Term](hydra.lib.lists.foldl[Seq[hydra.core.Term],
     hydra.core.Term]((acc: Seq[hydra.core.Term]) =>
  (el: hydra.core.Term) =>
  hydra.lib.lists.cons[hydra.core.Term](hydra.core.Term.application(hydra.core.Application(funTerm,
     el)))(acc))(Seq())(elements)))))

def maybeHead[T0](cx: T0)(g: hydra.graph.Graph)(listTerm: hydra.core.Term): Either[hydra.errors.Error,
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  Right(hydra.core.Term.maybe(hydra.lib.maybes.maybe[Option[hydra.core.Term], Tuple2[hydra.core.Term,
     Seq[hydra.core.Term]]](None)((p: Tuple2[hydra.core.Term, Seq[hydra.core.Term]]) =>
  Some(hydra.lib.pairs.first[hydra.core.Term, Seq[hydra.core.Term]](p)))(hydra.lib.lists.uncons[hydra.core.Term](elements)))))

def nub[T0](cx: T0)(g: hydra.graph.Graph)(listTerm: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.foldl"),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.lambda(hydra.core.Lambda("acc", None, hydra.core.Term.lambda(hydra.core.Lambda("x",
     None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.logic.ifElse"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.elem"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.variable("x"))), hydra.core.Term.variable("acc"))))), hydra.core.Term.variable("acc"))),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.concat2"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.variable("acc"))), hydra.core.Term.list(Seq(hydra.core.Term.variable("x"))))))))))))),
     hydra.core.Term.list(Seq()))), listTerm))))

def partition[T0](cx: T0)(g: hydra.graph.Graph)(predTerm: hydra.core.Term)(listTerm: hydra.core.Term): Either[hydra.errors.Error,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  {
  lazy val initialState: hydra.core.Term = hydra.core.Term.pair(Tuple2(hydra.core.Term.list(Seq()),
     hydra.core.Term.list(Seq())))
  {
    lazy val finalState: hydra.core.Term = hydra.lib.lists.foldl[hydra.core.Term,
       hydra.core.Term]((acc: hydra.core.Term) =>
      (el: hydra.core.Term) =>
      {
      lazy val yeses: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.first"),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         acc))
      {
        lazy val nos: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.second"),
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           acc))
        hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.logic.ifElse"),
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           hydra.core.Term.application(hydra.core.Application(predTerm, el)))), hydra.core.Term.pair(Tuple2(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.concat2"),
             
             
             
             
             
             
             
             
             
             
             
             
             
             
           yeses)), hydra.core.Term.list(Seq(el)))), nos)))), hydra.core.Term.pair(Tuple2(yeses,
           hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.concat2"),
             
             
             
             
             
             
             
             
             
             
             
             
             
             
           nos)), hydra.core.Term.list(Seq(el))))))))
      }
    })(initialState)(elements)
    Right(finalState)
  }
})

def pure[T0, T1, T2](cx: T0)(g: T1)(x: hydra.core.Term): Either[T2, hydra.core.Term] = Right(hydra.core.Term.list(Seq(x)))

def replicate[T0, T1, T2](cx: T0)(g: T1)(n: hydra.core.Term)(x: hydra.core.Term): Either[T2, hydra.core.Term] =
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.map"),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.lambda(hydra.core.Lambda("_", None, x)))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.range"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(1))))),
     n)))))

def singleton[T0, T1, T2](cx: T0)(g: T1)(x: hydra.core.Term): Either[T2, hydra.core.Term] = Right(hydra.core.Term.list(Seq(x)))

def sort[T0, T1, T2](cx: T0)(g: T1)(listTerm: hydra.core.Term): Either[T2, hydra.core.Term] =
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.sortOn"),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.variable("hydra.lib.equality.identity"))), listTerm)))

def sortOn[T0](cx: T0)(g: hydra.graph.Graph)(projTerm: hydra.core.Term)(listTerm: hydra.core.Term): Either[hydra.errors.Error,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  Right(hydra.lib.lists.foldl[hydra.core.Term, hydra.core.Term]((sorted: hydra.core.Term) =>
  (x: hydra.core.Term) =>
  {
  lazy val splitResult: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.span"),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.lambda(hydra.core.Lambda("y", None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.equality.lte"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.application(hydra.core.Application(projTerm, hydra.core.Term.variable("y"))))),
     hydra.core.Term.application(hydra.core.Application(projTerm, x)))))))), sorted))
  {
    lazy val before: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.first"),
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       splitResult))
    {
      lazy val after: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.second"),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         splitResult))
      hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.concat2"),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         before)), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.cons"),
           
           
           
           
           
           
           
           
           
           
           
           
           
           
         x)), after))))
    }
  }
})(hydra.core.Term.list(Seq()))(elements)))

def span[T0](cx: T0)(g: hydra.graph.Graph)(predTerm: hydra.core.Term)(listTerm: hydra.core.Term): Either[hydra.errors.Error,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  {
  lazy val initialState: hydra.core.Term = hydra.core.Term.pair(Tuple2(hydra.core.Term.pair(Tuple2(hydra.core.Term.literal(hydra.core.Literal.boolean(true)),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.list(Seq()))), hydra.core.Term.list(Seq())))
  {
    lazy val finalState: hydra.core.Term = hydra.lib.lists.foldl[hydra.core.Term,
       hydra.core.Term]((acc: hydra.core.Term) =>
      (el: hydra.core.Term) =>
      {
      lazy val takingLeft: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.first"),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         acc))
      {
        lazy val right: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.second"),
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           acc))
        {
          lazy val taking: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.first"),
            
            
            
            
            
            
            
            
            
            
            
            
            
            
             takingLeft))
          {
            lazy val left: hydra.core.Term = hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.second"),
              
              
              
              
              
              
              
              
              
              
              
              
              
              
               takingLeft))
            hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.logic.ifElse"),
              
              
              
              
              
              
              
              
              
              
              
              
              
              
               hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.logic.and"),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
               taking)), hydra.core.Term.application(hydra.core.Application(predTerm,
               el)))))), hydra.core.Term.pair(Tuple2(hydra.core.Term.pair(Tuple2(hydra.core.Term.literal(hydra.core.Literal.boolean(true)),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
               hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.concat2"),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
               left)), hydra.core.Term.list(Seq(el)))))), right)))), hydra.core.Term.pair(Tuple2(hydra.core.Term.pair(Tuple2(hydra.core.Term.literal(hydra.core.Literal.boolean(false)),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
               left)), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.lists.concat2"),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
               right)), hydra.core.Term.list(Seq(el))))))))
          }
        }
      }
    })(initialState)(elements)
    Right(hydra.core.Term.pair(Tuple2(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.second"),
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.first"),
       finalState)))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.pairs.second"),
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       finalState)))))
  }
})

def uncons[T0](cx: T0)(g: hydra.graph.Graph)(listTerm: hydra.core.Term): Either[hydra.errors.Error, hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm))((elements: Seq[hydra.core.Term]) =>
  Right(hydra.core.Term.maybe(hydra.lib.maybes.maybe[Option[hydra.core.Term], hydra.core.Term](None)((h: hydra.core.Term) =>
  Some(hydra.core.Term.pair(Tuple2(h, hydra.core.Term.list(hydra.lib.lists.drop[hydra.core.Term](1)(elements))))))(hydra.lib.lists.maybeAt[hydra.core.Term](0)(elements)))))

def zipWith[T0](cx: T0)(g: hydra.graph.Graph)(funTerm: hydra.core.Term)(listTerm1: hydra.core.Term)(listTerm2: hydra.core.Term): Either[hydra.errors.Error,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term] =
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm1))((elements1: Seq[hydra.core.Term]) =>
  hydra.lib.eithers.bind[hydra.errors.Error, Seq[hydra.core.Term], hydra.core.Term](hydra.extract.core.list(g)(listTerm2))((elements2: Seq[hydra.core.Term]) =>
  Right(hydra.core.Term.list(hydra.lib.lists.map[Tuple2[hydra.core.Term, hydra.core.Term],
     hydra.core.Term]((p: Tuple2[hydra.core.Term, hydra.core.Term]) =>
  {
  lazy val a: hydra.core.Term = hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](p)
  {
    lazy val b: hydra.core.Term = hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](p)
    hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(funTerm,
       a)), b))
  }
})(hydra.lib.lists.zip[hydra.core.Term, hydra.core.Term](elements1)(elements2))))))
