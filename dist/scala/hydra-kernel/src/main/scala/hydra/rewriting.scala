package hydra.rewriting

import hydra.coders.*

import hydra.core.*

import hydra.paths.*

def applyInsideTypeLambdasAndAnnotations(f: (hydra.core.Term => hydra.core.Term))(term0: hydra.core.Term): hydra.core.Term =
  term0 match
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.rewriting.applyInsideTypeLambdasAndAnnotations(f)(v_Term_annotated_at.body),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     (v_Term_annotated_at.annotation)))
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_tl.parameter,
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.rewriting.applyInsideTypeLambdasAndAnnotations(f)(v_Term_typeLambda_tl.body)))
  case _ => f(term0)

def foldOverTerm[T0](order: hydra.coders.TraversalOrder)(fld: (T0 => hydra.core.Term => T0))(b0: T0)(term: hydra.core.Term): T0 =
  order match
  case hydra.coders.TraversalOrder.pre => hydra.lib.lists.foldl[T0, hydra.core.Term]((v1: T0) =>
    (v2: hydra.core.Term) => hydra.rewriting.foldOverTerm(order)(fld)(v1)(v2))(fld(b0)(term))(hydra.rewriting.subterms(term))
  case hydra.coders.TraversalOrder.post => fld(hydra.lib.lists.foldl[T0, hydra.core.Term]((v1: T0) =>
    (v2: hydra.core.Term) => hydra.rewriting.foldOverTerm(order)(fld)(v1)(v2))(b0)(hydra.rewriting.subterms(term)))(term)

def foldOverType[T0](order: hydra.coders.TraversalOrder)(fld: (T0 => hydra.core.Type => T0))(b0: T0)(typ: hydra.core.Type): T0 =
  order match
  case hydra.coders.TraversalOrder.pre => hydra.lib.lists.foldl[T0, hydra.core.Type]((v1: T0) =>
    (v2: hydra.core.Type) => hydra.rewriting.foldOverType(order)(fld)(v1)(v2))(fld(b0)(typ))(hydra.rewriting.subtypes(typ))
  case hydra.coders.TraversalOrder.post => fld(hydra.lib.lists.foldl[T0, hydra.core.Type]((v1: T0) =>
    (v2: hydra.core.Type) => hydra.rewriting.foldOverType(order)(fld)(v1)(v2))(b0)(hydra.rewriting.subtypes(typ)))(typ)

def foldTermWithGraphAndPath[T0](f: ((T0 => hydra.core.Term => T0) => Seq[hydra.paths.SubtermStep] => hydra.graph.Graph => T0 => hydra.core.Term => T0))(cx0: hydra.graph.Graph)(val0: T0)(term0: hydra.core.Term): T0 =
  {
  def wrapper[T1](recurse: (T0 => hydra.core.Term => Tuple2[T0, T1]))(path: Seq[hydra.paths.SubtermStep])(cx: hydra.graph.Graph)(`val`: T0)(term: hydra.core.Term): Tuple2[T0,
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term] =
    {
    def recurseForUser(valIn: T0)(subterm: hydra.core.Term): T0 =
      {
      lazy val r: Tuple2[T0, T1] = recurse(valIn)(subterm)
      hydra.lib.pairs.first[T0, T1](r)
    }
    Tuple2(f(recurseForUser)(path)(cx)(`val`)(term), term)
  }
  lazy val result: Tuple2[T0, hydra.core.Term] = hydra.rewriting.rewriteAndFoldTermWithGraphAndPath(wrapper)(cx0)(val0)(term0)
  hydra.lib.pairs.first[T0, hydra.core.Term](result)
}

def mapBeneathTypeAnnotations(f: (hydra.core.Type => hydra.core.Type))(t: hydra.core.Type): hydra.core.Type =
  t match
  case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.core.Type.annotated(hydra.core.AnnotatedType(hydra.rewriting.mapBeneathTypeAnnotations(f)(v_Type_annotated_at.body),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     (v_Type_annotated_at.annotation)))
  case _ => f(t)

def rewriteAndFoldTerm[T0](f: ((T0 => hydra.core.Term => Tuple2[T0, hydra.core.Term]) => T0 => hydra.core.Term => Tuple2[T0,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term]))(term0: T0)(v1: hydra.core.Term): Tuple2[T0, hydra.core.Term] =
  {
  def fsub[T1](recurse: (T1 => hydra.core.Term => Tuple2[T1, hydra.core.Term]))(val0: T1)(term02: hydra.core.Term): Tuple2[T1,
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term] =
    {
    def forSingle[T2, T3, T4, T5, T6](rec: (T2 => T3 => Tuple2[T4, T5]))(cons: (T5 => T6))(`val`: T2)(term: T3): Tuple2[T4,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       T6] =
      {
      lazy val r: Tuple2[T4, T5] = rec(`val`)(term)
      Tuple2(hydra.lib.pairs.first[T4, T5](r), cons(hydra.lib.pairs.second[T4, T5](r)))
    }
    def forMany[T2, T3, T4, T5](rec: (T2 => T3 => Tuple2[T2, T4]))(cons: (Seq[T4] => T5))(`val`: T2)(els: Seq[T3]): Tuple2[T2,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       T5] =
      {
      lazy val rr: Tuple2[T2, Seq[T4]] = hydra.lib.lists.foldl[Tuple2[T2, Seq[T4]], T3]((r: Tuple2[T2, Seq[T4]]) =>
        (el: T3) =>
        {
        lazy val r2: Tuple2[T2, T4] = rec(hydra.lib.pairs.first[T2, Seq[T4]](r))(el)
        Tuple2(hydra.lib.pairs.first[T2, T4](r2), hydra.lib.lists.cons[T4](hydra.lib.pairs.second[T2,
           T4](r2))(hydra.lib.pairs.second[T2, Seq[T4]](r)))
      })(Tuple2(`val`, Seq()))(els)
      Tuple2(hydra.lib.pairs.first[T2, Seq[T4]](rr), cons(hydra.lib.lists.reverse[T4](hydra.lib.pairs.second[T2,
         Seq[T4]](rr))))
    }
    def forField(`val`: T1)(field: hydra.core.Field): Tuple2[T1, hydra.core.Field] =
      {
      lazy val r: Tuple2[T1, hydra.core.Term] = recurse(`val`)(field.term)
      Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](r), hydra.core.Field(field.name,
         hydra.lib.pairs.second[T1, hydra.core.Term](r)))
    }
    def forFields(v1: T1)(v2: Seq[hydra.core.Field]): Tuple2[T1, Seq[hydra.core.Field]] = forMany(forField)((x: Seq[hydra.core.Field]) => x)(v1)(v2)
    def forPair(`val`: T1)(kv: Tuple2[hydra.core.Term, hydra.core.Term]): Tuple2[T1,
       Tuple2[hydra.core.Term, hydra.core.Term]] =
      {
      lazy val rk: Tuple2[T1, hydra.core.Term] = recurse(`val`)(hydra.lib.pairs.first[hydra.core.Term,
         hydra.core.Term](kv))
      lazy val rv: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.pairs.first[T1,
         hydra.core.Term](rk))(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kv))
      Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rv), Tuple2(hydra.lib.pairs.second[T1,
         hydra.core.Term](rk), hydra.lib.pairs.second[T1, hydra.core.Term](rv)))
    }
    def forBinding(`val`: T1)(binding: hydra.core.Binding): Tuple2[T1, hydra.core.Binding] =
      {
      lazy val r: Tuple2[T1, hydra.core.Term] = recurse(`val`)(binding.term)
      Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](r), hydra.core.Binding(binding.name,
         hydra.lib.pairs.second[T1, hydra.core.Term](r), (binding.`type`)))
    }
    lazy val dflt: Tuple2[T1, hydra.core.Term] = Tuple2(val0, term02)
    term02 match
      case hydra.core.Term.annotated(v_Term_annotated_at) => forSingle(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.annotated(hydra.core.AnnotatedTerm(t, (v_Term_annotated_at.annotation))))(val0)(v_Term_annotated_at.body)
      case hydra.core.Term.application(v_Term_application_a) => {
        lazy val rlhs: Tuple2[T1, hydra.core.Term] = recurse(val0)(v_Term_application_a.function)
        {
          lazy val rrhs: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.pairs.first[T1,
             hydra.core.Term](rlhs))(v_Term_application_a.argument)
          Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rrhs), hydra.core.Term.application(hydra.core.Application(hydra.lib.pairs.second[T1,
            
            
            
            
            
            
            
            
            
            
            
            
            
            
             hydra.core.Term](rlhs), hydra.lib.pairs.second[T1, hydra.core.Term](rrhs))))
        }
      }
      case hydra.core.Term.cases(v_Term_cases_cs) => {
        lazy val rmd: Option[Tuple2[T1, hydra.core.Term]] = hydra.lib.maybes.map[hydra.core.Term,
           Tuple2[T1, hydra.core.Term]]((v1: hydra.core.Term) => recurse(val0)(v1))(v_Term_cases_cs.default)
        {
          lazy val val1: T1 = hydra.lib.maybes.maybe[T1, Tuple2[T1, hydra.core.Term]](val0)(hydra.lib.pairs.first[T1,
             hydra.core.Term])(rmd)
          {
            lazy val rcases: Tuple2[T1, Seq[hydra.core.Field]] = forFields(val1)(v_Term_cases_cs.cases)
            Tuple2(hydra.lib.pairs.first[T1, Seq[hydra.core.Field]](rcases), hydra.core.Term.cases(hydra.core.CaseStatement(v_Term_cases_cs.typeName,
              
              
              
              
              
              
              
              
              
              
              
              
              
              
               hydra.lib.maybes.map[Tuple2[T1, hydra.core.Term], hydra.core.Term](hydra.lib.pairs.second[T1,
               hydra.core.Term])(rmd), hydra.lib.pairs.second[T1, Seq[hydra.core.Field]](rcases))))
          }
        }
      }
      case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term,
         hydra.core.Term, Tuple2[T1, hydra.core.Term]]((l: hydra.core.Term) =>
        {
        lazy val rl: Tuple2[T1, hydra.core.Term] = recurse(val0)(l)
        Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rl), hydra.core.Term.either(Left(hydra.lib.pairs.second[T1,
           hydra.core.Term](rl))))
      })((r: hydra.core.Term) =>
        {
        lazy val rr: Tuple2[T1, hydra.core.Term] = recurse(val0)(r)
        Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rr), hydra.core.Term.either(Right(hydra.lib.pairs.second[T1,
           hydra.core.Term](rr))))
      })(v_Term_either_e)
      case hydra.core.Term.lambda(v_Term_lambda_l) => {
        lazy val rl: Tuple2[T1, hydra.core.Term] = recurse(val0)(v_Term_lambda_l.body)
        Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rl), hydra.core.Term.lambda(hydra.core.Lambda(v_Term_lambda_l.parameter,
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           (v_Term_lambda_l.domain), hydra.lib.pairs.second[T1, hydra.core.Term](rl))))
      }
      case hydra.core.Term.let(v_Term_let_l) => {
        lazy val renv: Tuple2[T1, hydra.core.Term] = recurse(val0)(v_Term_let_l.body)
        forMany(forBinding)((bins: Seq[hydra.core.Binding]) =>
          hydra.core.Term.let(hydra.core.Let(bins, hydra.lib.pairs.second[T1, hydra.core.Term](renv))))(hydra.lib.pairs.first[T1,
            
            
            
            
            
            
            
            
            
            
            
            
            
            
             hydra.core.Term](renv))(v_Term_let_l.bindings)
      }
      case hydra.core.Term.list(v_Term_list_els) => forMany(recurse)((x: Seq[hydra.core.Term]) => hydra.core.Term.list(x))(val0)(v_Term_list_els)
      case hydra.core.Term.map(v_Term_map_m) => forMany(forPair)((pairs: Seq[Tuple2[hydra.core.Term,
         hydra.core.Term]]) =>
        hydra.core.Term.map(hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](pairs)))(val0)(hydra.lib.maps.toList[hydra.core.Term,
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           hydra.core.Term](v_Term_map_m))
      case hydra.core.Term.maybe(v_Term_maybe_mt) => hydra.lib.maybes.maybe[Tuple2[T1,
         hydra.core.Term], hydra.core.Term](dflt)((t: hydra.core.Term) =>
        forSingle(recurse)((t1: hydra.core.Term) => hydra.core.Term.maybe(Some(t1)))(val0)(t))(v_Term_maybe_mt)
      case hydra.core.Term.pair(v_Term_pair_p) => {
        lazy val rf: Tuple2[T1, hydra.core.Term] = recurse(val0)(hydra.lib.pairs.first[hydra.core.Term,
           hydra.core.Term](v_Term_pair_p))
        {
          lazy val rs: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.pairs.first[T1,
             hydra.core.Term](rf))(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p))
          Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rs), hydra.core.Term.pair(Tuple2(hydra.lib.pairs.second[T1,
             hydra.core.Term](rf), hydra.lib.pairs.second[T1, hydra.core.Term](rs))))
        }
      }
      case hydra.core.Term.project(v_Term_project_p) => Tuple2(val0, hydra.core.Term.project(v_Term_project_p))
      case hydra.core.Term.record(v_Term_record_r) => forMany(forField)((fields: Seq[hydra.core.Field]) =>
        hydra.core.Term.record(hydra.core.Record(v_Term_record_r.typeName, fields)))(val0)(v_Term_record_r.fields)
      case hydra.core.Term.set(v_Term_set_els) => forMany(recurse)((e: Seq[hydra.core.Term]) =>
        hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](e)))(val0)(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_els))
      case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => forSingle(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(t, (v_Term_typeApplication_ta.`type`))))(val0)(v_Term_typeApplication_ta.body)
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => forSingle(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_tl.parameter,
           t)))(val0)(v_Term_typeLambda_tl.body)
      case hydra.core.Term.inject(v_Term_inject_inj) => forSingle(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.inject(hydra.core.Injection(v_Term_inject_inj.typeName, hydra.core.Field(v_Term_inject_inj.field.name,
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           t))))(val0)(v_Term_inject_inj.field.term)
      case hydra.core.Term.unwrap(v_Term_unwrap_n) => Tuple2(val0, hydra.core.Term.unwrap(v_Term_unwrap_n))
      case hydra.core.Term.wrap(v_Term_wrap_wt) => forSingle(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.wrap(hydra.core.WrappedTerm(v_Term_wrap_wt.typeName, t)))(val0)(v_Term_wrap_wt.body)
      case _ => dflt
  }
  def recurse(v1: T0)(v2: hydra.core.Term): Tuple2[T0, hydra.core.Term] = f((v12: T0) => (v22: hydra.core.Term) => fsub(recurse)(v12)(v22))(v1)(v2)
  recurse(term0)(v1)
}

def rewriteAndFoldTermWithGraph[T0](f: ((T0 => hydra.core.Term => Tuple2[T0, hydra.core.Term]) => hydra.graph.Graph => T0 => hydra.core.Term => Tuple2[T0,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term]))(cx0: hydra.graph.Graph)(val0: T0)(term0: hydra.core.Term): Tuple2[T0,
   hydra.core.Term] =
  {
  def wrapper[T1](lowLevelRecurse: (Tuple2[T0, hydra.graph.Graph] => hydra.core.Term => Tuple2[Tuple2[T0,
     T1], hydra.core.Term]))(valAndCx: Tuple2[T0, hydra.graph.Graph])(term: hydra.core.Term): Tuple2[Tuple2[T0,
     hydra.graph.Graph], hydra.core.Term] =
    {
    lazy val `val`: T0 = hydra.lib.pairs.first[T0, hydra.graph.Graph](valAndCx)
    lazy val cx: hydra.graph.Graph = hydra.lib.pairs.second[T0, hydra.graph.Graph](valAndCx)
    lazy val cx1: hydra.graph.Graph = term match
      case hydra.core.Term.lambda(v_Term_lambda_l) => hydra.scoping.extendGraphForLambda(cx)(v_Term_lambda_l)
      case hydra.core.Term.let(v_Term_let_l) => hydra.scoping.extendGraphForLet((_x: hydra.graph.Graph) => (_2: hydra.core.Binding) => None)(cx)(v_Term_let_l)
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.scoping.extendGraphForTypeLambda(cx)(v_Term_typeLambda_tl)
      case _ => cx
    def recurseForUser(newVal: T0)(subterm: hydra.core.Term): Tuple2[T0, hydra.core.Term] =
      {
      lazy val result: Tuple2[Tuple2[T0, T1], hydra.core.Term] = lowLevelRecurse(Tuple2(newVal, cx1))(subterm)
      Tuple2(hydra.lib.pairs.first[T0, T1](hydra.lib.pairs.first[Tuple2[T0, T1], hydra.core.Term](result)),
         hydra.lib.pairs.second[Tuple2[T0, T1], hydra.core.Term](result))
    }
    lazy val fResult: Tuple2[T0, hydra.core.Term] = f(recurseForUser)(cx1)(`val`)(term)
    Tuple2(Tuple2(hydra.lib.pairs.first[T0, hydra.core.Term](fResult), cx), hydra.lib.pairs.second[T0,
       hydra.core.Term](fResult))
  }
  lazy val result: Tuple2[Tuple2[T0, hydra.graph.Graph], hydra.core.Term] = hydra.rewriting.rewriteAndFoldTerm(wrapper)(Tuple2(val0,
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     cx0))(term0)
  Tuple2(hydra.lib.pairs.first[T0, hydra.graph.Graph](hydra.lib.pairs.first[Tuple2[T0,
     hydra.graph.Graph], hydra.core.Term](result)), hydra.lib.pairs.second[Tuple2[T0,
     hydra.graph.Graph], hydra.core.Term](result))
}

def rewriteAndFoldTermWithGraphAndPath[T0](f: ((T0 => hydra.core.Term => Tuple2[T0,
   hydra.core.Term]) => Seq[hydra.paths.SubtermStep] => hydra.graph.Graph => T0 => hydra.core.Term => Tuple2[T0,
   hydra.core.Term]))(cx0: hydra.graph.Graph)(val0: T0)(term0: hydra.core.Term): Tuple2[T0,
   hydra.core.Term] =
  {
  def wrapper[T1](recurse: (Seq[hydra.paths.SubtermStep] => Tuple2[hydra.graph.Graph,
     T0] => hydra.core.Term => Tuple2[Tuple2[T1, T0], hydra.core.Term]))(path: Seq[hydra.paths.SubtermStep])(cxAndVal: Tuple2[hydra.graph.Graph,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     T0])(term: hydra.core.Term): Tuple2[Tuple2[hydra.graph.Graph, T0], hydra.core.Term] =
    {
    lazy val cx: hydra.graph.Graph = hydra.lib.pairs.first[hydra.graph.Graph, T0](cxAndVal)
    lazy val `val`: T0 = hydra.lib.pairs.second[hydra.graph.Graph, T0](cxAndVal)
    lazy val cx1: hydra.graph.Graph = term match
      case hydra.core.Term.lambda(v_Term_lambda_l) => hydra.scoping.extendGraphForLambda(cx)(v_Term_lambda_l)
      case hydra.core.Term.let(v_Term_let_l) => hydra.scoping.extendGraphForLet((_x: hydra.graph.Graph) => (_2: hydra.core.Binding) => None)(cx)(v_Term_let_l)
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.scoping.extendGraphForTypeLambda(cx)(v_Term_typeLambda_tl)
      case _ => cx
    def recurseForUser(valIn: T0)(termIn: hydra.core.Term): Tuple2[T0, hydra.core.Term] =
      {
      lazy val result: Tuple2[Tuple2[T1, T0], hydra.core.Term] = recurse(path)(Tuple2(cx1, valIn))(termIn)
      Tuple2(hydra.lib.pairs.second[T1, T0](hydra.lib.pairs.first[Tuple2[T1, T0],
         hydra.core.Term](result)), hydra.lib.pairs.second[Tuple2[T1, T0], hydra.core.Term](result))
    }
    lazy val fResult: Tuple2[T0, hydra.core.Term] = f(recurseForUser)(path)(cx1)(`val`)(term)
    Tuple2(Tuple2(cx, hydra.lib.pairs.first[T0, hydra.core.Term](fResult)), hydra.lib.pairs.second[T0,
       hydra.core.Term](fResult))
  }
  lazy val result: Tuple2[Tuple2[hydra.graph.Graph, T0], hydra.core.Term] = hydra.rewriting.rewriteAndFoldTermWithPath(wrapper)(Tuple2(cx0,
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     val0))(term0)
  Tuple2(hydra.lib.pairs.second[hydra.graph.Graph, T0](hydra.lib.pairs.first[Tuple2[hydra.graph.Graph,
     T0], hydra.core.Term](result)), hydra.lib.pairs.second[Tuple2[hydra.graph.Graph,
     T0], hydra.core.Term](result))
}

def rewriteAndFoldTermWithPath[T0](f: ((Seq[hydra.paths.SubtermStep] => T0 => hydra.core.Term => Tuple2[T0,
   hydra.core.Term]) => Seq[hydra.paths.SubtermStep] => T0 => hydra.core.Term => Tuple2[T0,
   hydra.core.Term]))(term0: T0)(v1: hydra.core.Term): Tuple2[T0, hydra.core.Term] =
  {
  def fsub[T1](recurse: (Seq[hydra.paths.SubtermStep] => T1 => hydra.core.Term => Tuple2[T1,
     hydra.core.Term]))(path: Seq[hydra.paths.SubtermStep])(val0: T1)(term02: hydra.core.Term): Tuple2[T1,
     hydra.core.Term] =
    {
    def forSingleWithAccessor[T2, T3, T4, T5, T6](rec: (Seq[hydra.paths.SubtermStep] => T2 => T3 => Tuple2[T4,
       T5]))(cons: (T5 => T6))(accessor: hydra.paths.SubtermStep)(`val`: T2)(term: T3): Tuple2[T4,
       T6] =
      {
      lazy val r: Tuple2[T4, T5] = rec(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(accessor)))(`val`)(term)
      Tuple2(hydra.lib.pairs.first[T4, T5](r), cons(hydra.lib.pairs.second[T4, T5](r)))
    }
    def forManyWithAccessors[T2, T3, T4, T5](rec: (Seq[hydra.paths.SubtermStep] => T2 => T3 => Tuple2[T2,
       T4]))(cons: (Seq[T4] => T5))(`val`: T2)(accessorTermPairs: Seq[Tuple2[hydra.paths.SubtermStep,
       T3]]): Tuple2[T2, T5] =
      {
      lazy val rr: Tuple2[T2, Seq[T4]] = hydra.lib.lists.foldl[Tuple2[T2, Seq[T4]],
         Tuple2[hydra.paths.SubtermStep, T3]]((r: Tuple2[T2, Seq[T4]]) =>
        (atp: Tuple2[hydra.paths.SubtermStep, T3]) =>
        {
        lazy val r2: Tuple2[T2, T4] = rec(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(hydra.lib.pairs.first[hydra.paths.SubtermStep,
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           T3](atp))))(hydra.lib.pairs.first[T2, Seq[T4]](r))(hydra.lib.pairs.second[hydra.paths.SubtermStep,
           T3](atp))
        Tuple2(hydra.lib.pairs.first[T2, T4](r2), hydra.lib.lists.cons[T4](hydra.lib.pairs.second[T2,
           T4](r2))(hydra.lib.pairs.second[T2, Seq[T4]](r)))
      })(Tuple2(`val`, Seq()))(accessorTermPairs)
      Tuple2(hydra.lib.pairs.first[T2, Seq[T4]](rr), cons(hydra.lib.lists.reverse[T4](hydra.lib.pairs.second[T2,
         Seq[T4]](rr))))
    }
    def forFieldWithAccessor(mkAccessor: (hydra.core.Name => hydra.paths.SubtermStep))(`val`: T1)(field: hydra.core.Field): Tuple2[T1,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       hydra.core.Field] =
      {
      lazy val r: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(mkAccessor(field.name))))(`val`)(field.term)
      Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](r), hydra.core.Field(field.name,
         hydra.lib.pairs.second[T1, hydra.core.Term](r)))
    }
    def forFieldsWithAccessor(mkAccessor: (hydra.core.Name => hydra.paths.SubtermStep))(v1: T1)(v2: Seq[Tuple2[hydra.paths.SubtermStep,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       hydra.core.Field]]): Tuple2[T1, Seq[hydra.core.Field]] =
      forManyWithAccessors((path1: Seq[hydra.paths.SubtermStep]) =>
      (val1: T1) =>
      (field1: hydra.core.Field) => forFieldWithAccessor(mkAccessor)(val1)(field1))((x: Seq[hydra.core.Field]) => x)(v1)(v2)
    def forPairWithAccessors(keyAccessor: hydra.paths.SubtermStep)(valAccessor: hydra.paths.SubtermStep)(`val`: T1)(kv: Tuple2[hydra.core.Term,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       hydra.core.Term]): Tuple2[T1, Tuple2[hydra.core.Term, hydra.core.Term]] =
      {
      lazy val rk: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(keyAccessor)))(`val`)(hydra.lib.pairs.first[hydra.core.Term,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Term](kv))
      lazy val rv: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(valAccessor)))(hydra.lib.pairs.first[T1,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Term](rk))(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kv))
      Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rv), Tuple2(hydra.lib.pairs.second[T1,
         hydra.core.Term](rk), hydra.lib.pairs.second[T1, hydra.core.Term](rv)))
    }
    def forBindingWithAccessor(`val`: T1)(binding: hydra.core.Binding): Tuple2[T1, hydra.core.Binding] =
      {
      lazy val r: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(hydra.paths.SubtermStep.letBinding(binding.name))))(`val`)(binding.term)
      Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](r), hydra.core.Binding(binding.name,
         hydra.lib.pairs.second[T1, hydra.core.Term](r), (binding.`type`)))
    }
    lazy val dflt: Tuple2[T1, hydra.core.Term] = Tuple2(val0, term02)
    term02 match
      case hydra.core.Term.annotated(v_Term_annotated_at) => forSingleWithAccessor(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.annotated(hydra.core.AnnotatedTerm(t, (v_Term_annotated_at.annotation))))(hydra.paths.SubtermStep.annotatedBody)(val0)(v_Term_annotated_at.body)
      case hydra.core.Term.application(v_Term_application_a) => {
        lazy val rlhs: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(hydra.paths.SubtermStep.applicationFunction)))(val0)(v_Term_application_a.function)
        {
          lazy val rrhs: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(hydra.paths.SubtermStep.applicationArgument)))(hydra.lib.pairs.first[T1,
            
            
            
            
            
            
            
            
            
            
            
            
            
            
             hydra.core.Term](rlhs))(v_Term_application_a.argument)
          Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rrhs), hydra.core.Term.application(hydra.core.Application(hydra.lib.pairs.second[T1,
            
            
            
            
            
            
            
            
            
            
            
            
            
            
             hydra.core.Term](rlhs), hydra.lib.pairs.second[T1, hydra.core.Term](rrhs))))
        }
      }
      case hydra.core.Term.cases(v_Term_cases_cs) => {
        lazy val rmd: Option[Tuple2[T1, hydra.core.Term]] = hydra.lib.maybes.map[hydra.core.Term,
           Tuple2[T1, hydra.core.Term]]((`def`: hydra.core.Term) =>
          recurse(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(hydra.paths.SubtermStep.unionCasesDefault)))(val0)(`def`))(v_Term_cases_cs.default)
        {
          lazy val val1: T1 = hydra.lib.maybes.maybe[T1, Tuple2[T1, hydra.core.Term]](val0)(hydra.lib.pairs.first[T1,
             hydra.core.Term])(rmd)
          {
            lazy val rcases: Tuple2[T1, Seq[hydra.core.Term]] = forManyWithAccessors(recurse)((x: Seq[hydra.core.Term]) => x)(val1)(hydra.lib.lists.map[hydra.core.Field,
              
              
              
              
              
              
              
              
              
              
              
              
              
              
               Tuple2[hydra.paths.SubtermStep, hydra.core.Term]]((f2: hydra.core.Field) =>
              Tuple2(hydra.paths.SubtermStep.unionCasesBranch(f2.name), (f2.term)))(v_Term_cases_cs.cases))
            Tuple2(hydra.lib.pairs.first[T1, Seq[hydra.core.Term]](rcases), hydra.core.Term.cases(hydra.core.CaseStatement(v_Term_cases_cs.typeName,
              
              
              
              
              
              
              
              
              
              
              
              
              
              
               hydra.lib.maybes.map[Tuple2[T1, hydra.core.Term], hydra.core.Term](hydra.lib.pairs.second[T1,
               hydra.core.Term])(rmd), hydra.lib.lists.map[Tuple2[hydra.core.Name,
               hydra.core.Term], hydra.core.Field]((ft: Tuple2[hydra.core.Name, hydra.core.Term]) =>
              hydra.core.Field(hydra.lib.pairs.first[hydra.core.Name, hydra.core.Term](ft),
                 hydra.lib.pairs.second[hydra.core.Name, hydra.core.Term](ft)))(hydra.lib.lists.zip[hydra.core.Name,
                 hydra.core.Term](hydra.lib.lists.map[hydra.core.Field, hydra.core.Name]((x: hydra.core.Field) => (x.name))(v_Term_cases_cs.cases))(hydra.lib.pairs.second[T1,
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                 Seq[hydra.core.Term]](rcases))))))
          }
        }
      }
      case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term,
         hydra.core.Term, Tuple2[T1, hydra.core.Term]]((l: hydra.core.Term) =>
        {
        lazy val rl: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(hydra.paths.SubtermStep.sumTerm)))(val0)(l)
        Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rl), hydra.core.Term.either(Left(hydra.lib.pairs.second[T1,
           hydra.core.Term](rl))))
      })((r: hydra.core.Term) =>
        {
        lazy val rr: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(hydra.paths.SubtermStep.sumTerm)))(val0)(r)
        Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rr), hydra.core.Term.either(Right(hydra.lib.pairs.second[T1,
           hydra.core.Term](rr))))
      })(v_Term_either_e)
      case hydra.core.Term.lambda(v_Term_lambda_l) => {
        lazy val rl: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(hydra.paths.SubtermStep.lambdaBody)))(val0)(v_Term_lambda_l.body)
        Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rl), hydra.core.Term.lambda(hydra.core.Lambda(v_Term_lambda_l.parameter,
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           (v_Term_lambda_l.domain), hydra.lib.pairs.second[T1, hydra.core.Term](rl))))
      }
      case hydra.core.Term.let(v_Term_let_l) => {
        lazy val renv: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(hydra.paths.SubtermStep.letBody)))(val0)(v_Term_let_l.body)
        {
          lazy val rbindings: Tuple2[T1, Seq[hydra.core.Binding]] = hydra.lib.lists.foldl[Tuple2[T1,
             Seq[hydra.core.Binding]], hydra.core.Binding]((r: Tuple2[T1, Seq[hydra.core.Binding]]) =>
            (binding: hydra.core.Binding) =>
            {
            lazy val rb: Tuple2[T1, hydra.core.Binding] = forBindingWithAccessor(hydra.lib.pairs.first[T1,
               Seq[hydra.core.Binding]](r))(binding)
            Tuple2(hydra.lib.pairs.first[T1, hydra.core.Binding](rb), hydra.lib.lists.cons[hydra.core.Binding](hydra.lib.pairs.second[T1,
              
              
              
              
              
              
              
              
              
              
              
              
              
              
               hydra.core.Binding](rb))(hydra.lib.pairs.second[T1, Seq[hydra.core.Binding]](r)))
          })(Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](renv), Seq()))(v_Term_let_l.bindings)
          Tuple2(hydra.lib.pairs.first[T1, Seq[hydra.core.Binding]](rbindings), hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.reverse[hydra.core.Binding](hydra.lib.pairs.second[T1,
            
            
            
            
            
            
            
            
            
            
            
            
            
            
             Seq[hydra.core.Binding]](rbindings)), hydra.lib.pairs.second[T1, hydra.core.Term](renv))))
        }
      }
      case hydra.core.Term.list(v_Term_list_els) => {
        lazy val idx: Int = 0
        {
          lazy val rr: Tuple2[Int, Tuple2[T1, Seq[hydra.core.Term]]] = hydra.lib.lists.foldl[Tuple2[Int,
             Tuple2[T1, Seq[hydra.core.Term]]], hydra.core.Term]((r: Tuple2[Int, Tuple2[T1,
             Seq[hydra.core.Term]]]) =>
            (el: hydra.core.Term) =>
            {
            lazy val r2: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(hydra.paths.SubtermStep.listElement(hydra.lib.pairs.first[Int,
              
              
              
              
              
              
              
              
              
              
              
              
              
              
               Tuple2[T1, Seq[hydra.core.Term]]](r)))))(hydra.lib.pairs.first[T1,
               Seq[hydra.core.Term]](hydra.lib.pairs.second[Int, Tuple2[T1, Seq[hydra.core.Term]]](r)))(el)
            Tuple2(hydra.lib.math.add(hydra.lib.pairs.first[Int, Tuple2[T1, Seq[hydra.core.Term]]](r))(1),
               Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](r2), hydra.lib.lists.cons[hydra.core.Term](hydra.lib.pairs.second[T1,
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
               hydra.core.Term](r2))(hydra.lib.pairs.second[T1, Seq[hydra.core.Term]](hydra.lib.pairs.second[Int,
               Tuple2[T1, Seq[hydra.core.Term]]](r)))))
          })(Tuple2(idx, Tuple2(val0, Seq())))(v_Term_list_els)
          Tuple2(hydra.lib.pairs.first[T1, Seq[hydra.core.Term]](hydra.lib.pairs.second[Int,
             Tuple2[T1, Seq[hydra.core.Term]]](rr)), hydra.core.Term.list(hydra.lib.lists.reverse[hydra.core.Term](hydra.lib.pairs.second[T1,
               
               
               
               
               
               
               
               
               
               
               
               
               
               
             Seq[hydra.core.Term]](hydra.lib.pairs.second[Int, Tuple2[T1, Seq[hydra.core.Term]]](rr)))))
        }
      }
      case hydra.core.Term.map(v_Term_map_m) => {
        lazy val idx: Int = 0
        {
          lazy val rr: Tuple2[Int, Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]] = hydra.lib.lists.foldl[Tuple2[Int,
            
            
            
            
            
            
            
            
            
            
            
            
            
            
             Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]], Tuple2[hydra.core.Term,
             hydra.core.Term]]((r: Tuple2[Int, Tuple2[T1, Seq[Tuple2[hydra.core.Term,
             hydra.core.Term]]]]) =>
            (kv: Tuple2[hydra.core.Term, hydra.core.Term]) =>
            {
            lazy val rk: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(hydra.paths.SubtermStep.mapKey(hydra.lib.pairs.first[Int,
              
              
              
              
              
              
              
              
              
              
              
              
              
              
               Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]](r)))))(hydra.lib.pairs.first[T1,
               Seq[Tuple2[hydra.core.Term, hydra.core.Term]]](hydra.lib.pairs.second[Int,
               Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]](r)))(hydra.lib.pairs.first[hydra.core.Term,
               hydra.core.Term](kv))
            {
              lazy val rv: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(hydra.paths.SubtermStep.mapValue(hydra.lib.pairs.first[Int,
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                 Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]](r)))))(hydra.lib.pairs.first[T1,
                 hydra.core.Term](rk))(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](kv))
              Tuple2(hydra.lib.math.add(hydra.lib.pairs.first[Int, Tuple2[T1, Seq[Tuple2[hydra.core.Term,
                 hydra.core.Term]]]](r))(1), Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rv),
                 hydra.lib.lists.cons[Tuple2[hydra.core.Term, hydra.core.Term]](Tuple2(hydra.lib.pairs.second[T1,
                 hydra.core.Term](rk), hydra.lib.pairs.second[T1, hydra.core.Term](rv)))(hydra.lib.pairs.second[T1,
                 Seq[Tuple2[hydra.core.Term, hydra.core.Term]]](hydra.lib.pairs.second[Int,
                 Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]](r)))))
            }
          })(Tuple2(idx, Tuple2(val0, Seq())))(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m))
          Tuple2(hydra.lib.pairs.first[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]](hydra.lib.pairs.second[Int,
             Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]](rr)), hydra.core.Term.map(hydra.lib.maps.fromList[hydra.core.Term,
               
               
               
               
               
               
               
               
               
               
               
               
               
               
             hydra.core.Term](hydra.lib.lists.reverse[Tuple2[hydra.core.Term, hydra.core.Term]](hydra.lib.pairs.second[T1,
               
               
               
               
               
               
               
               
               
               
               
               
               
               
             Seq[Tuple2[hydra.core.Term, hydra.core.Term]]](hydra.lib.pairs.second[Int,
             Tuple2[T1, Seq[Tuple2[hydra.core.Term, hydra.core.Term]]]](rr))))))
        }
      }
      case hydra.core.Term.maybe(v_Term_maybe_mt) => hydra.lib.maybes.maybe[Tuple2[T1,
         hydra.core.Term], hydra.core.Term](dflt)((t: hydra.core.Term) =>
        forSingleWithAccessor(recurse)((t1: hydra.core.Term) => hydra.core.Term.maybe(Some(t1)))(hydra.paths.SubtermStep.maybeTerm)(val0)(t))(v_Term_maybe_mt)
      case hydra.core.Term.pair(v_Term_pair_p) => {
        lazy val rf: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(hydra.paths.SubtermStep.productTerm(0))))(val0)(hydra.lib.pairs.first[hydra.core.Term,
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           hydra.core.Term](v_Term_pair_p))
        {
          lazy val rs: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(hydra.paths.SubtermStep.productTerm(1))))(hydra.lib.pairs.first[T1,
            
            
            
            
            
            
            
            
            
            
            
            
            
            
             hydra.core.Term](rf))(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p))
          Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](rs), hydra.core.Term.pair(Tuple2(hydra.lib.pairs.second[T1,
             hydra.core.Term](rf), hydra.lib.pairs.second[T1, hydra.core.Term](rs))))
        }
      }
      case hydra.core.Term.project(v_Term_project_p) => Tuple2(val0, hydra.core.Term.project(v_Term_project_p))
      case hydra.core.Term.record(v_Term_record_r) => {
        lazy val rfields: Tuple2[T1, Seq[hydra.core.Term]] = forManyWithAccessors(recurse)((x: Seq[hydra.core.Term]) => x)(val0)(hydra.lib.lists.map[hydra.core.Field,
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           Tuple2[hydra.paths.SubtermStep, hydra.core.Term]]((f2: hydra.core.Field) =>
          Tuple2(hydra.paths.SubtermStep.recordField(f2.name), (f2.term)))(v_Term_record_r.fields))
        Tuple2(hydra.lib.pairs.first[T1, Seq[hydra.core.Term]](rfields), hydra.core.Term.record(hydra.core.Record(v_Term_record_r.typeName,
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.Term], hydra.core.Field]((ft: Tuple2[hydra.core.Name,
           hydra.core.Term]) =>
          hydra.core.Field(hydra.lib.pairs.first[hydra.core.Name, hydra.core.Term](ft),
             hydra.lib.pairs.second[hydra.core.Name, hydra.core.Term](ft)))(hydra.lib.lists.zip[hydra.core.Name,
             hydra.core.Term](hydra.lib.lists.map[hydra.core.Field, hydra.core.Name]((x: hydra.core.Field) => (x.name))(v_Term_record_r.fields))(hydra.lib.pairs.second[T1,
               
               
               
               
               
               
               
               
               
               
               
               
               
               
             Seq[hydra.core.Term]](rfields))))))
      }
      case hydra.core.Term.set(v_Term_set_els) => {
        lazy val idx: Int = 0
        {
          lazy val rr: Tuple2[Int, Tuple2[T1, Seq[hydra.core.Term]]] = hydra.lib.lists.foldl[Tuple2[Int,
             Tuple2[T1, Seq[hydra.core.Term]]], hydra.core.Term]((r: Tuple2[Int, Tuple2[T1,
             Seq[hydra.core.Term]]]) =>
            (el: hydra.core.Term) =>
            {
            lazy val r2: Tuple2[T1, hydra.core.Term] = recurse(hydra.lib.lists.concat2[hydra.paths.SubtermStep](path)(Seq(hydra.paths.SubtermStep.setElement(hydra.lib.pairs.first[Int,
              
              
              
              
              
              
              
              
              
              
              
              
              
              
               Tuple2[T1, Seq[hydra.core.Term]]](r)))))(hydra.lib.pairs.first[T1,
               Seq[hydra.core.Term]](hydra.lib.pairs.second[Int, Tuple2[T1, Seq[hydra.core.Term]]](r)))(el)
            Tuple2(hydra.lib.math.add(hydra.lib.pairs.first[Int, Tuple2[T1, Seq[hydra.core.Term]]](r))(1),
               Tuple2(hydra.lib.pairs.first[T1, hydra.core.Term](r2), hydra.lib.lists.cons[hydra.core.Term](hydra.lib.pairs.second[T1,
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
               hydra.core.Term](r2))(hydra.lib.pairs.second[T1, Seq[hydra.core.Term]](hydra.lib.pairs.second[Int,
               Tuple2[T1, Seq[hydra.core.Term]]](r)))))
          })(Tuple2(idx, Tuple2(val0, Seq())))(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_els))
          Tuple2(hydra.lib.pairs.first[T1, Seq[hydra.core.Term]](hydra.lib.pairs.second[Int,
             Tuple2[T1, Seq[hydra.core.Term]]](rr)), hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](hydra.lib.lists.reverse[hydra.core.Term](hydra.lib.pairs.second[T1,
               
               
               
               
               
               
               
               
               
               
               
               
               
               
             Seq[hydra.core.Term]](hydra.lib.pairs.second[Int, Tuple2[T1, Seq[hydra.core.Term]]](rr))))))
        }
      }
      case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => forSingleWithAccessor(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(t, (v_Term_typeApplication_ta.`type`))))(hydra.paths.SubtermStep.typeApplicationTerm)(val0)(v_Term_typeApplication_ta.body)
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => forSingleWithAccessor(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_tl.parameter,
           t)))(hydra.paths.SubtermStep.typeLambdaBody)(val0)(v_Term_typeLambda_tl.body)
      case hydra.core.Term.inject(v_Term_inject_inj) => forSingleWithAccessor(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.inject(hydra.core.Injection(v_Term_inject_inj.typeName, hydra.core.Field(v_Term_inject_inj.field.name,
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           t))))(hydra.paths.SubtermStep.injectionTerm)(val0)(v_Term_inject_inj.field.term)
      case hydra.core.Term.unwrap(v_Term_unwrap_n) => Tuple2(val0, hydra.core.Term.unwrap(v_Term_unwrap_n))
      case hydra.core.Term.wrap(v_Term_wrap_wt) => forSingleWithAccessor(recurse)((t: hydra.core.Term) =>
        hydra.core.Term.wrap(hydra.core.WrappedTerm(v_Term_wrap_wt.typeName, t)))(hydra.paths.SubtermStep.wrappedTerm)(val0)(v_Term_wrap_wt.body)
      case _ => dflt
  }
  def recurse(v1: Seq[hydra.paths.SubtermStep])(v2: T0)(v3: hydra.core.Term): Tuple2[T0, hydra.core.Term] =
    f((v12: Seq[hydra.paths.SubtermStep]) =>
    (v22: T0) => (v32: hydra.core.Term) => fsub(recurse)(v12)(v22)(v32))(v1)(v2)(v3)
  recurse(Seq())(term0)(v1)
}

def rewriteTerm(f: ((hydra.core.Term => hydra.core.Term) => hydra.core.Term => hydra.core.Term))(term0: hydra.core.Term): hydra.core.Term =
  {
  def fsub(recurse: (hydra.core.Term => hydra.core.Term))(term: hydra.core.Term): hydra.core.Term =
    {
    def forField(f2: hydra.core.Field): hydra.core.Field = hydra.core.Field(f2.name, recurse(f2.term))
    def forLet(lt: hydra.core.Let): hydra.core.Let =
      {
      def mapBinding(b: hydra.core.Binding): hydra.core.Binding = hydra.core.Binding(b.name,
         recurse(b.term), (b.`type`))
      hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding](mapBinding)(lt.bindings),
         recurse(lt.body))
    }
    def forMap(m: Map[hydra.core.Term, hydra.core.Term]): Map[hydra.core.Term, hydra.core.Term] =
      {
      def forPair(p: Tuple2[hydra.core.Term, hydra.core.Term]): Tuple2[hydra.core.Term, hydra.core.Term] =
        Tuple2(recurse(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](p)),
           recurse(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](p)))
      hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](hydra.lib.lists.map[Tuple2[hydra.core.Term,
         hydra.core.Term], Tuple2[hydra.core.Term, hydra.core.Term]](forPair)(hydra.lib.maps.toList[hydra.core.Term,
         hydra.core.Term](m)))
    }
    term match
      case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.core.Term.annotated(hydra.core.AnnotatedTerm(recurse(v_Term_annotated_at.body),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         (v_Term_annotated_at.annotation)))
      case hydra.core.Term.application(v_Term_application_a) => hydra.core.Term.application(hydra.core.Application(recurse(v_Term_application_a.function),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         recurse(v_Term_application_a.argument)))
      case hydra.core.Term.cases(v_Term_cases_cs) => hydra.core.Term.cases(hydra.core.CaseStatement(v_Term_cases_cs.typeName,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.lib.maybes.map[hydra.core.Term, hydra.core.Term](recurse)(v_Term_cases_cs.default),
         hydra.lib.lists.map[hydra.core.Field, hydra.core.Field](forField)(v_Term_cases_cs.cases)))
      case hydra.core.Term.either(v_Term_either_e) => hydra.core.Term.either(hydra.lib.eithers.either[hydra.core.Term,
         hydra.core.Term, Either[hydra.core.Term, hydra.core.Term]]((l: hydra.core.Term) => Left(recurse(l)))((r: hydra.core.Term) => Right(recurse(r)))(v_Term_either_e))
      case hydra.core.Term.lambda(v_Term_lambda_l) => hydra.core.Term.lambda(hydra.core.Lambda(v_Term_lambda_l.parameter,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         (v_Term_lambda_l.domain), recurse(v_Term_lambda_l.body)))
      case hydra.core.Term.let(v_Term_let_lt) => hydra.core.Term.let(forLet(v_Term_let_lt))
      case hydra.core.Term.list(v_Term_list_els) => hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Term,
         hydra.core.Term](recurse)(v_Term_list_els))
      case hydra.core.Term.literal(v_Term_literal_v) => hydra.core.Term.literal(v_Term_literal_v)
      case hydra.core.Term.map(v_Term_map_m) => hydra.core.Term.map(forMap(v_Term_map_m))
      case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.core.Term,
         hydra.core.Term](recurse)(v_Term_maybe_m))
      case hydra.core.Term.pair(v_Term_pair_p) => hydra.core.Term.pair(Tuple2(recurse(hydra.lib.pairs.first[hydra.core.Term,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Term](v_Term_pair_p)), recurse(hydra.lib.pairs.second[hydra.core.Term,
         hydra.core.Term](v_Term_pair_p))))
      case hydra.core.Term.project(v_Term_project_p) => hydra.core.Term.project(v_Term_project_p)
      case hydra.core.Term.record(v_Term_record_r) => hydra.core.Term.record(hydra.core.Record(v_Term_record_r.typeName,
         hydra.lib.lists.map[hydra.core.Field, hydra.core.Field](forField)(v_Term_record_r.fields)))
      case hydra.core.Term.set(v_Term_set_s) => hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](hydra.lib.lists.map[hydra.core.Term,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Term](recurse)(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s))))
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(recurse(v_Term_typeApplication_tt.body),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         (v_Term_typeApplication_tt.`type`)))
      case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_ta.parameter,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         recurse(v_Term_typeLambda_ta.body)))
      case hydra.core.Term.inject(v_Term_inject_i) => hydra.core.Term.inject(hydra.core.Injection(v_Term_inject_i.typeName,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         forField(v_Term_inject_i.field)))
      case hydra.core.Term.unit => hydra.core.Term.unit
      case hydra.core.Term.unwrap(v_Term_unwrap_n) => hydra.core.Term.unwrap(v_Term_unwrap_n)
      case hydra.core.Term.variable(v_Term_variable_v) => hydra.core.Term.variable(v_Term_variable_v)
      case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.core.Term.wrap(hydra.core.WrappedTerm(v_Term_wrap_wt.typeName,
         recurse(v_Term_wrap_wt.body)))
  }
  def recurse(v1: hydra.core.Term): hydra.core.Term = f((v12: hydra.core.Term) => fsub(recurse)(v12))(v1)
  recurse(term0)
}

def rewriteTermM[T0](f: ((hydra.core.Term => Either[T0, hydra.core.Term]) => hydra.core.Term => Either[T0,
   hydra.core.Term]))(term0: hydra.core.Term): Either[T0, hydra.core.Term] =
  {
  def fsub[T1](recurse: (hydra.core.Term => Either[T1, hydra.core.Term]))(term: hydra.core.Term): Either[T1,
     hydra.core.Term] =
    {
    def forField(field: hydra.core.Field): Either[T1, hydra.core.Field] =
      hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Field](recurse(field.term))((t: hydra.core.Term) => Right(hydra.core.Field(field.name,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         t)))
    def forPair(kv: Tuple2[hydra.core.Term, hydra.core.Term]): Either[T1, Tuple2[hydra.core.Term, hydra.core.Term]] =
      hydra.lib.eithers.bind[T1, hydra.core.Term, Tuple2[hydra.core.Term, hydra.core.Term]](recurse(hydra.lib.pairs.first[hydra.core.Term,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Term](kv)))((k: hydra.core.Term) =>
      hydra.lib.eithers.bind[T1, hydra.core.Term, Tuple2[hydra.core.Term, hydra.core.Term]](recurse(hydra.lib.pairs.second[hydra.core.Term,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Term](kv)))((v: hydra.core.Term) => Right(Tuple2(k, v))))
    def mapBinding(b: hydra.core.Binding): Either[T1, hydra.core.Binding] =
      hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Binding](recurse(b.term))((v: hydra.core.Term) => Right(hydra.core.Binding(b.name,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         v, (b.`type`))))
    term match
      case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.lib.eithers.bind[T1,
         hydra.core.Term, hydra.core.Term](recurse(v_Term_annotated_at.body))((ex: hydra.core.Term) =>
        Right(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(ex, (v_Term_annotated_at.annotation)))))
      case hydra.core.Term.application(v_Term_application_app) => hydra.lib.eithers.bind[T1,
         hydra.core.Term, hydra.core.Term](recurse(v_Term_application_app.function))((lhs: hydra.core.Term) =>
        hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Term](recurse(v_Term_application_app.argument))((rhs: hydra.core.Term) =>
        Right(hydra.core.Term.application(hydra.core.Application(lhs, rhs)))))
      case hydra.core.Term.cases(v_Term_cases_cs) => {
        lazy val n: hydra.core.Name = (v_Term_cases_cs.typeName)
        {
          lazy val `def`: Option[hydra.core.Term] = (v_Term_cases_cs.default)
          {
            lazy val csCases: Seq[hydra.core.Field] = (v_Term_cases_cs.cases)
            hydra.lib.eithers.bind[T1, Option[hydra.core.Term], hydra.core.Term](hydra.lib.maybes.maybe[Either[T1,
               Option[hydra.core.Term]], hydra.core.Term](Right(None))((t: hydra.core.Term) =>
              hydra.lib.eithers.map[hydra.core.Term, Option[hydra.core.Term], T1](hydra.lib.maybes.pure[hydra.core.Term])(recurse(t)))(`def`))((rdef: Option[hydra.core.Term]) =>
              hydra.lib.eithers.map[Seq[hydra.core.Field], hydra.core.Term, T1]((rcases: Seq[hydra.core.Field]) =>
              hydra.core.Term.cases(hydra.core.CaseStatement(n, rdef, rcases)))(hydra.lib.eithers.mapList[hydra.core.Field,
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                 hydra.core.Field, T1](forField)(csCases)))
          }
        }
      }
      case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.bind[T1, Either[hydra.core.Term,
         hydra.core.Term], hydra.core.Term](hydra.lib.eithers.either[hydra.core.Term,
         hydra.core.Term, Either[T1, Either[hydra.core.Term, hydra.core.Term]]]((l: hydra.core.Term) =>
        hydra.lib.eithers.map[hydra.core.Term, Either[hydra.core.Term, hydra.core.Term],
           T1]((x: hydra.core.Term) => Left(x))(recurse(l)))((r: hydra.core.Term) =>
        hydra.lib.eithers.map[hydra.core.Term, Either[hydra.core.Term, hydra.core.Term],
           T1]((x: hydra.core.Term) => Right(x))(recurse(r)))(v_Term_either_e))((re: Either[hydra.core.Term,
           hydra.core.Term]) => Right(hydra.core.Term.either(re)))
      case hydra.core.Term.lambda(v_Term_lambda_l) => {
        lazy val v: hydra.core.Name = (v_Term_lambda_l.parameter)
        {
          lazy val d: Option[hydra.core.Type] = (v_Term_lambda_l.domain)
          {
            lazy val body: hydra.core.Term = (v_Term_lambda_l.body)
            hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Term](recurse(body))((rbody: hydra.core.Term) =>
              Right(hydra.core.Term.lambda(hydra.core.Lambda(v, d, rbody))))
          }
        }
      }
      case hydra.core.Term.let(v_Term_let_lt) => {
        lazy val bindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
        {
          lazy val env: hydra.core.Term = (v_Term_let_lt.body)
          hydra.lib.eithers.bind[T1, Seq[hydra.core.Binding], hydra.core.Term](hydra.lib.eithers.mapList[hydra.core.Binding,
            
            
            
            
            
            
            
            
            
            
            
            
            
            
             hydra.core.Binding, T1](mapBinding)(bindings))((rbindings: Seq[hydra.core.Binding]) =>
            hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Term](recurse(env))((renv: hydra.core.Term) => Right(hydra.core.Term.let(hydra.core.Let(rbindings,
              
              
              
              
              
              
              
              
              
              
              
              
              
              
               renv)))))
        }
      }
      case hydra.core.Term.list(v_Term_list_els) => hydra.lib.eithers.bind[T1, Seq[hydra.core.Term],
         hydra.core.Term](hydra.lib.eithers.mapList[hydra.core.Term, hydra.core.Term,
         T1](recurse)(v_Term_list_els))((rels: Seq[hydra.core.Term]) => Right(hydra.core.Term.list(rels)))
      case hydra.core.Term.literal(v_Term_literal_v) => Right(hydra.core.Term.literal(v_Term_literal_v))
      case hydra.core.Term.map(v_Term_map_m) => hydra.lib.eithers.bind[T1, Seq[Tuple2[hydra.core.Term,
         hydra.core.Term]], hydra.core.Term](hydra.lib.eithers.mapList[Tuple2[hydra.core.Term,
         hydra.core.Term], Tuple2[hydra.core.Term, hydra.core.Term], T1](forPair)(hydra.lib.maps.toList[hydra.core.Term,
         hydra.core.Term](v_Term_map_m)))((pairs: Seq[Tuple2[hydra.core.Term, hydra.core.Term]]) =>
        Right(hydra.core.Term.map(hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](pairs))))
      case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.lib.eithers.bind[T1, Option[hydra.core.Term],
         hydra.core.Term](hydra.lib.eithers.mapMaybe[hydra.core.Term, hydra.core.Term,
         T1](recurse)(v_Term_maybe_m))((rm: Option[hydra.core.Term]) => Right(hydra.core.Term.maybe(rm)))
      case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.eithers.bind[T1, hydra.core.Term,
         hydra.core.Term](recurse(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)))((rf: hydra.core.Term) =>
        hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Term](recurse(hydra.lib.pairs.second[hydra.core.Term,
           hydra.core.Term](v_Term_pair_p)))((rs: hydra.core.Term) => Right(hydra.core.Term.pair(Tuple2(rf,
           rs)))))
      case hydra.core.Term.project(v_Term_project_p) => Right(hydra.core.Term.project(v_Term_project_p))
      case hydra.core.Term.record(v_Term_record_r) => {
        lazy val n: hydra.core.Name = (v_Term_record_r.typeName)
        {
          lazy val fields: Seq[hydra.core.Field] = (v_Term_record_r.fields)
          hydra.lib.eithers.map[Seq[hydra.core.Field], hydra.core.Term, T1]((rfields: Seq[hydra.core.Field]) => hydra.core.Term.record(hydra.core.Record(n,
            
            
            
            
            
            
            
            
            
            
            
            
            
            
             rfields)))(hydra.lib.eithers.mapList[hydra.core.Field, hydra.core.Field,
             T1](forField)(fields))
        }
      }
      case hydra.core.Term.set(v_Term_set_s) => hydra.lib.eithers.bind[T1, Seq[hydra.core.Term],
         hydra.core.Term](hydra.lib.eithers.mapList[hydra.core.Term, hydra.core.Term,
         T1](recurse)(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s)))((rlist: Seq[hydra.core.Term]) =>
        Right(hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](rlist))))
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.lib.eithers.bind[T1,
         hydra.core.Term, hydra.core.Term](recurse(v_Term_typeApplication_tt.body))((t: hydra.core.Term) =>
        Right(hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(t, (v_Term_typeApplication_tt.`type`)))))
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
        lazy val v: hydra.core.Name = (v_Term_typeLambda_tl.parameter)
        {
          lazy val body: hydra.core.Term = (v_Term_typeLambda_tl.body)
          hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Term](recurse(body))((rbody: hydra.core.Term) =>
            Right(hydra.core.Term.typeLambda(hydra.core.TypeLambda(v, rbody))))
        }
      }
      case hydra.core.Term.inject(v_Term_inject_i) => {
        lazy val n: hydra.core.Name = (v_Term_inject_i.typeName)
        {
          lazy val field: hydra.core.Field = (v_Term_inject_i.field)
          hydra.lib.eithers.map[hydra.core.Field, hydra.core.Term, T1]((rfield: hydra.core.Field) => hydra.core.Term.inject(hydra.core.Injection(n,
            
            
            
            
            
            
            
            
            
            
            
            
            
            
             rfield)))(forField(field))
        }
      }
      case hydra.core.Term.unit => Right(hydra.core.Term.unit)
      case hydra.core.Term.unwrap(v_Term_unwrap_n) => Right(hydra.core.Term.unwrap(v_Term_unwrap_n))
      case hydra.core.Term.variable(v_Term_variable_v) => Right(hydra.core.Term.variable(v_Term_variable_v))
      case hydra.core.Term.wrap(v_Term_wrap_wt) => {
        lazy val name: hydra.core.Name = (v_Term_wrap_wt.typeName)
        {
          lazy val t: hydra.core.Term = (v_Term_wrap_wt.body)
          hydra.lib.eithers.bind[T1, hydra.core.Term, hydra.core.Term](recurse(t))((rt: hydra.core.Term) =>
            Right(hydra.core.Term.wrap(hydra.core.WrappedTerm(name, rt))))
        }
      }
  }
  def recurse(v1: hydra.core.Term): Either[T0, hydra.core.Term] = f((v12: hydra.core.Term) => fsub(recurse)(v12))(v1)
  recurse(term0)
}

def rewriteTermWithContext[T0](f: ((T0 => hydra.core.Term => hydra.core.Term) => T0 => hydra.core.Term => hydra.core.Term))(cx0: T0)(term0: hydra.core.Term): hydra.core.Term =
  {
  def forSubterms[T1](recurse0: (T1 => hydra.core.Term => hydra.core.Term))(cx: T1)(term: hydra.core.Term): hydra.core.Term =
    {
    def recurse(v1: hydra.core.Term): hydra.core.Term = recurse0(cx)(v1)
    def forField(field: hydra.core.Field): hydra.core.Field = hydra.core.Field(field.name, recurse(field.term))
    def forLet(lt: hydra.core.Let): hydra.core.Let =
      {
      def mapBinding(b: hydra.core.Binding): hydra.core.Binding = hydra.core.Binding(b.name,
         recurse(b.term), (b.`type`))
      hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding](mapBinding)(lt.bindings),
         recurse(lt.body))
    }
    def forMap(m: Map[hydra.core.Term, hydra.core.Term]): Map[hydra.core.Term, hydra.core.Term] =
      {
      def forPair(p: Tuple2[hydra.core.Term, hydra.core.Term]): Tuple2[hydra.core.Term, hydra.core.Term] =
        Tuple2(recurse(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](p)),
           recurse(hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](p)))
      hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](hydra.lib.lists.map[Tuple2[hydra.core.Term,
         hydra.core.Term], Tuple2[hydra.core.Term, hydra.core.Term]](forPair)(hydra.lib.maps.toList[hydra.core.Term,
         hydra.core.Term](m)))
    }
    term match
      case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.core.Term.annotated(hydra.core.AnnotatedTerm(recurse(v_Term_annotated_at.body),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         (v_Term_annotated_at.annotation)))
      case hydra.core.Term.application(v_Term_application_a) => hydra.core.Term.application(hydra.core.Application(recurse(v_Term_application_a.function),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         recurse(v_Term_application_a.argument)))
      case hydra.core.Term.cases(v_Term_cases_cs) => hydra.core.Term.cases(hydra.core.CaseStatement(v_Term_cases_cs.typeName,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.lib.maybes.map[hydra.core.Term, hydra.core.Term](recurse)(v_Term_cases_cs.default),
         hydra.lib.lists.map[hydra.core.Field, hydra.core.Field](forField)(v_Term_cases_cs.cases)))
      case hydra.core.Term.either(v_Term_either_e) => hydra.core.Term.either(hydra.lib.eithers.either[hydra.core.Term,
         hydra.core.Term, Either[hydra.core.Term, hydra.core.Term]]((l: hydra.core.Term) => Left(recurse(l)))((r: hydra.core.Term) => Right(recurse(r)))(v_Term_either_e))
      case hydra.core.Term.lambda(v_Term_lambda_l) => hydra.core.Term.lambda(hydra.core.Lambda(v_Term_lambda_l.parameter,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         (v_Term_lambda_l.domain), recurse(v_Term_lambda_l.body)))
      case hydra.core.Term.let(v_Term_let_lt) => hydra.core.Term.let(forLet(v_Term_let_lt))
      case hydra.core.Term.list(v_Term_list_els) => hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Term,
         hydra.core.Term](recurse)(v_Term_list_els))
      case hydra.core.Term.literal(v_Term_literal_v) => hydra.core.Term.literal(v_Term_literal_v)
      case hydra.core.Term.map(v_Term_map_m) => hydra.core.Term.map(forMap(v_Term_map_m))
      case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.core.Term,
         hydra.core.Term](recurse)(v_Term_maybe_m))
      case hydra.core.Term.pair(v_Term_pair_p) => hydra.core.Term.pair(Tuple2(recurse(hydra.lib.pairs.first[hydra.core.Term,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Term](v_Term_pair_p)), recurse(hydra.lib.pairs.second[hydra.core.Term,
         hydra.core.Term](v_Term_pair_p))))
      case hydra.core.Term.project(v_Term_project_p) => hydra.core.Term.project(v_Term_project_p)
      case hydra.core.Term.record(v_Term_record_r) => hydra.core.Term.record(hydra.core.Record(v_Term_record_r.typeName,
         hydra.lib.lists.map[hydra.core.Field, hydra.core.Field](forField)(v_Term_record_r.fields)))
      case hydra.core.Term.set(v_Term_set_s) => hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](hydra.lib.lists.map[hydra.core.Term,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Term](recurse)(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s))))
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(recurse(v_Term_typeApplication_tt.body),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         (v_Term_typeApplication_tt.`type`)))
      case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(v_Term_typeLambda_ta.parameter,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         recurse(v_Term_typeLambda_ta.body)))
      case hydra.core.Term.inject(v_Term_inject_i) => hydra.core.Term.inject(hydra.core.Injection(v_Term_inject_i.typeName,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         forField(v_Term_inject_i.field)))
      case hydra.core.Term.unit => hydra.core.Term.unit
      case hydra.core.Term.unwrap(v_Term_unwrap_n) => hydra.core.Term.unwrap(v_Term_unwrap_n)
      case hydra.core.Term.variable(v_Term_variable_v) => hydra.core.Term.variable(v_Term_variable_v)
      case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.core.Term.wrap(hydra.core.WrappedTerm(v_Term_wrap_wt.typeName,
         recurse(v_Term_wrap_wt.body)))
  }
  def rewrite(cx: T0)(term: hydra.core.Term): hydra.core.Term = f((v1: T0) => (v2: hydra.core.Term) => forSubterms(rewrite)(v1)(v2))(cx)(term)
  rewrite(cx0)(term0)
}

def rewriteTermWithContextM[T0, T1](f: ((T0 => hydra.core.Term => Either[T1, hydra.core.Term]) => T0 => hydra.core.Term => Either[T1,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Term]))(cx0: T0)(term0: hydra.core.Term): Either[T1, hydra.core.Term] =
  {
  def forSubterms[T2, T3](recurse0: (T2 => hydra.core.Term => Either[T3, hydra.core.Term]))(cx: T2)(term: hydra.core.Term): Either[T3,
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term] =
    {
    def recurse(v1: hydra.core.Term): Either[T3, hydra.core.Term] = recurse0(cx)(v1)
    def forField(field: hydra.core.Field): Either[T3, hydra.core.Field] =
      hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Field](recurse(field.term))((t: hydra.core.Term) => Right(hydra.core.Field(field.name,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         t)))
    def forPair(kv: Tuple2[hydra.core.Term, hydra.core.Term]): Either[T3, Tuple2[hydra.core.Term, hydra.core.Term]] =
      hydra.lib.eithers.bind[T3, hydra.core.Term, Tuple2[hydra.core.Term, hydra.core.Term]](recurse(hydra.lib.pairs.first[hydra.core.Term,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Term](kv)))((k: hydra.core.Term) =>
      hydra.lib.eithers.bind[T3, hydra.core.Term, Tuple2[hydra.core.Term, hydra.core.Term]](recurse(hydra.lib.pairs.second[hydra.core.Term,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Term](kv)))((v: hydra.core.Term) => Right(Tuple2(k, v))))
    def mapBinding(b: hydra.core.Binding): Either[T3, hydra.core.Binding] =
      hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Binding](recurse(b.term))((v: hydra.core.Term) => Right(hydra.core.Binding(b.name,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         v, (b.`type`))))
    term match
      case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.lib.eithers.bind[T3,
         hydra.core.Term, hydra.core.Term](recurse(v_Term_annotated_at.body))((ex: hydra.core.Term) =>
        Right(hydra.core.Term.annotated(hydra.core.AnnotatedTerm(ex, (v_Term_annotated_at.annotation)))))
      case hydra.core.Term.application(v_Term_application_app) => hydra.lib.eithers.bind[T3,
         hydra.core.Term, hydra.core.Term](recurse(v_Term_application_app.function))((lhs: hydra.core.Term) =>
        hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Term](recurse(v_Term_application_app.argument))((rhs: hydra.core.Term) =>
        Right(hydra.core.Term.application(hydra.core.Application(lhs, rhs)))))
      case hydra.core.Term.cases(v_Term_cases_cs) => {
        lazy val n: hydra.core.Name = (v_Term_cases_cs.typeName)
        {
          lazy val `def`: Option[hydra.core.Term] = (v_Term_cases_cs.default)
          {
            lazy val csCases: Seq[hydra.core.Field] = (v_Term_cases_cs.cases)
            hydra.lib.eithers.bind[T3, Option[hydra.core.Term], hydra.core.Term](hydra.lib.maybes.maybe[Either[T3,
               Option[hydra.core.Term]], hydra.core.Term](Right(None))((t: hydra.core.Term) =>
              hydra.lib.eithers.map[hydra.core.Term, Option[hydra.core.Term], T3](hydra.lib.maybes.pure[hydra.core.Term])(recurse(t)))(`def`))((rdef: Option[hydra.core.Term]) =>
              hydra.lib.eithers.map[Seq[hydra.core.Field], hydra.core.Term, T3]((rcases: Seq[hydra.core.Field]) =>
              hydra.core.Term.cases(hydra.core.CaseStatement(n, rdef, rcases)))(hydra.lib.eithers.mapList[hydra.core.Field,
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                 hydra.core.Field, T3](forField)(csCases)))
          }
        }
      }
      case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.bind[T3, Either[hydra.core.Term,
         hydra.core.Term], hydra.core.Term](hydra.lib.eithers.either[hydra.core.Term,
         hydra.core.Term, Either[T3, Either[hydra.core.Term, hydra.core.Term]]]((l: hydra.core.Term) =>
        hydra.lib.eithers.map[hydra.core.Term, Either[hydra.core.Term, hydra.core.Term],
           T3]((x: hydra.core.Term) => Left(x))(recurse(l)))((r: hydra.core.Term) =>
        hydra.lib.eithers.map[hydra.core.Term, Either[hydra.core.Term, hydra.core.Term],
           T3]((x: hydra.core.Term) => Right(x))(recurse(r)))(v_Term_either_e))((re: Either[hydra.core.Term,
           hydra.core.Term]) => Right(hydra.core.Term.either(re)))
      case hydra.core.Term.lambda(v_Term_lambda_l) => {
        lazy val v: hydra.core.Name = (v_Term_lambda_l.parameter)
        {
          lazy val d: Option[hydra.core.Type] = (v_Term_lambda_l.domain)
          {
            lazy val body: hydra.core.Term = (v_Term_lambda_l.body)
            hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Term](recurse(body))((rbody: hydra.core.Term) =>
              Right(hydra.core.Term.lambda(hydra.core.Lambda(v, d, rbody))))
          }
        }
      }
      case hydra.core.Term.let(v_Term_let_lt) => {
        lazy val bindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
        {
          lazy val body: hydra.core.Term = (v_Term_let_lt.body)
          hydra.lib.eithers.bind[T3, Seq[hydra.core.Binding], hydra.core.Term](hydra.lib.eithers.mapList[hydra.core.Binding,
            
            
            
            
            
            
            
            
            
            
            
            
            
            
             hydra.core.Binding, T3](mapBinding)(bindings))((rbindings: Seq[hydra.core.Binding]) =>
            hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Term](recurse(body))((rbody: hydra.core.Term) => Right(hydra.core.Term.let(hydra.core.Let(rbindings,
              
              
              
              
              
              
              
              
              
              
              
              
              
              
               rbody)))))
        }
      }
      case hydra.core.Term.list(v_Term_list_els) => hydra.lib.eithers.bind[T3, Seq[hydra.core.Term],
         hydra.core.Term](hydra.lib.eithers.mapList[hydra.core.Term, hydra.core.Term,
         T3](recurse)(v_Term_list_els))((rels: Seq[hydra.core.Term]) => Right(hydra.core.Term.list(rels)))
      case hydra.core.Term.literal(v_Term_literal_v) => Right(hydra.core.Term.literal(v_Term_literal_v))
      case hydra.core.Term.map(v_Term_map_m) => hydra.lib.eithers.bind[T3, Seq[Tuple2[hydra.core.Term,
         hydra.core.Term]], hydra.core.Term](hydra.lib.eithers.mapList[Tuple2[hydra.core.Term,
         hydra.core.Term], Tuple2[hydra.core.Term, hydra.core.Term], T3](forPair)(hydra.lib.maps.toList[hydra.core.Term,
         hydra.core.Term](v_Term_map_m)))((pairs: Seq[Tuple2[hydra.core.Term, hydra.core.Term]]) =>
        Right(hydra.core.Term.map(hydra.lib.maps.fromList[hydra.core.Term, hydra.core.Term](pairs))))
      case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.lib.eithers.bind[T3, Option[hydra.core.Term],
         hydra.core.Term](hydra.lib.eithers.mapMaybe[hydra.core.Term, hydra.core.Term,
         T3](recurse)(v_Term_maybe_m))((rm: Option[hydra.core.Term]) => Right(hydra.core.Term.maybe(rm)))
      case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.eithers.bind[T3, hydra.core.Term,
         hydra.core.Term](recurse(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](v_Term_pair_p)))((rfirst: hydra.core.Term) =>
        hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Term](recurse(hydra.lib.pairs.second[hydra.core.Term,
           hydra.core.Term](v_Term_pair_p)))((rsecond: hydra.core.Term) => Right(hydra.core.Term.pair(Tuple2(rfirst,
           rsecond)))))
      case hydra.core.Term.project(v_Term_project_p) => Right(hydra.core.Term.project(v_Term_project_p))
      case hydra.core.Term.record(v_Term_record_r) => {
        lazy val n: hydra.core.Name = (v_Term_record_r.typeName)
        {
          lazy val fields: Seq[hydra.core.Field] = (v_Term_record_r.fields)
          hydra.lib.eithers.map[Seq[hydra.core.Field], hydra.core.Term, T3]((rfields: Seq[hydra.core.Field]) => hydra.core.Term.record(hydra.core.Record(n,
            
            
            
            
            
            
            
            
            
            
            
            
            
            
             rfields)))(hydra.lib.eithers.mapList[hydra.core.Field, hydra.core.Field,
             T3](forField)(fields))
        }
      }
      case hydra.core.Term.set(v_Term_set_s) => hydra.lib.eithers.bind[T3, Seq[hydra.core.Term],
         hydra.core.Term](hydra.lib.eithers.mapList[hydra.core.Term, hydra.core.Term,
         T3](recurse)(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s)))((rlist: Seq[hydra.core.Term]) =>
        Right(hydra.core.Term.set(hydra.lib.sets.fromList[hydra.core.Term](rlist))))
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.lib.eithers.bind[T3,
         hydra.core.Term, hydra.core.Term](recurse(v_Term_typeApplication_tt.body))((t: hydra.core.Term) =>
        Right(hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(t, (v_Term_typeApplication_tt.`type`)))))
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
        lazy val v: hydra.core.Name = (v_Term_typeLambda_tl.parameter)
        {
          lazy val body: hydra.core.Term = (v_Term_typeLambda_tl.body)
          hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Term](recurse(body))((rbody: hydra.core.Term) =>
            Right(hydra.core.Term.typeLambda(hydra.core.TypeLambda(v, rbody))))
        }
      }
      case hydra.core.Term.inject(v_Term_inject_i) => {
        lazy val n: hydra.core.Name = (v_Term_inject_i.typeName)
        {
          lazy val field: hydra.core.Field = (v_Term_inject_i.field)
          hydra.lib.eithers.map[hydra.core.Field, hydra.core.Term, T3]((rfield: hydra.core.Field) => hydra.core.Term.inject(hydra.core.Injection(n,
            
            
            
            
            
            
            
            
            
            
            
            
            
            
             rfield)))(forField(field))
        }
      }
      case hydra.core.Term.unit => Right(hydra.core.Term.unit)
      case hydra.core.Term.unwrap(v_Term_unwrap_n) => Right(hydra.core.Term.unwrap(v_Term_unwrap_n))
      case hydra.core.Term.variable(v_Term_variable_v) => Right(hydra.core.Term.variable(v_Term_variable_v))
      case hydra.core.Term.wrap(v_Term_wrap_wt) => {
        lazy val name: hydra.core.Name = (v_Term_wrap_wt.typeName)
        {
          lazy val t: hydra.core.Term = (v_Term_wrap_wt.body)
          hydra.lib.eithers.bind[T3, hydra.core.Term, hydra.core.Term](recurse(t))((rt: hydra.core.Term) =>
            Right(hydra.core.Term.wrap(hydra.core.WrappedTerm(name, rt))))
        }
      }
  }
  def rewrite(cx: T0)(term: hydra.core.Term): Either[T1, hydra.core.Term] = f((v1: T0) => (v2: hydra.core.Term) => forSubterms(rewrite)(v1)(v2))(cx)(term)
  rewrite(cx0)(term0)
}

def rewriteTermWithGraph[T0](f: ((hydra.core.Term => T0) => hydra.graph.Graph => hydra.core.Term => T0))(cx0: hydra.graph.Graph)(term0: hydra.core.Term): T0 =
  {
  def f2(recurse: (hydra.graph.Graph => hydra.core.Term => T0))(cx: hydra.graph.Graph)(term: hydra.core.Term): T0 =
    {
    def recurse1(term2: hydra.core.Term): T0 = recurse(cx)(term2)
    term match
      case hydra.core.Term.lambda(v_Term_lambda_l) => {
        lazy val cx1: hydra.graph.Graph = hydra.scoping.extendGraphForLambda(cx)(v_Term_lambda_l)
        {
          def recurse2(term2: hydra.core.Term): T0 = recurse(cx1)(term2)
          f(recurse2)(cx1)(term)
        }
      }
      case hydra.core.Term.let(v_Term_let_l) => {
        lazy val cx1: hydra.graph.Graph = hydra.scoping.extendGraphForLet((_x: hydra.graph.Graph) => (_2: hydra.core.Binding) => None)(cx)(v_Term_let_l)
        {
          def recurse2(term2: hydra.core.Term): T0 = recurse(cx1)(term2)
          f(recurse2)(cx1)(term)
        }
      }
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
        lazy val cx1: hydra.graph.Graph = hydra.scoping.extendGraphForTypeLambda(cx)(v_Term_typeLambda_tl)
        {
          def recurse2(term2: hydra.core.Term): T0 = recurse(cx1)(term2)
          f(recurse2)(cx1)(term)
        }
      }
      case _ => f(recurse1)(cx)(term)
  }
  def rewrite(cx: hydra.graph.Graph)(term: hydra.core.Term): T0 = f2(rewrite)(cx)(term)
  rewrite(cx0)(term0)
}

def rewriteType(f: ((hydra.core.Type => hydra.core.Type) => hydra.core.Type => hydra.core.Type))(typ0: hydra.core.Type): hydra.core.Type =
  {
  def fsub(recurse: (hydra.core.Type => hydra.core.Type))(typ: hydra.core.Type): hydra.core.Type =
    {
    def forField(field: hydra.core.FieldType): hydra.core.FieldType = hydra.core.FieldType(field.name,
       recurse(field.`type`))
    typ match
      case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.core.Type.annotated(hydra.core.AnnotatedType(recurse(v_Type_annotated_at.body),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         (v_Type_annotated_at.annotation)))
      case hydra.core.Type.application(v_Type_application_app) => hydra.core.Type.application(hydra.core.ApplicationType(recurse(v_Type_application_app.function),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         recurse(v_Type_application_app.argument)))
      case hydra.core.Type.either(v_Type_either_et) => hydra.core.Type.either(hydra.core.EitherType(recurse(v_Type_either_et.left),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         recurse(v_Type_either_et.right)))
      case hydra.core.Type.pair(v_Type_pair_pt) => hydra.core.Type.pair(hydra.core.PairType(recurse(v_Type_pair_pt.first),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         recurse(v_Type_pair_pt.second)))
      case hydra.core.Type.function(v_Type_function_fun) => hydra.core.Type.function(hydra.core.FunctionType(recurse(v_Type_function_fun.domain),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         recurse(v_Type_function_fun.codomain)))
      case hydra.core.Type.forall(v_Type_forall_lt) => hydra.core.Type.forall(hydra.core.ForallType(v_Type_forall_lt.parameter,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         recurse(v_Type_forall_lt.body)))
      case hydra.core.Type.list(v_Type_list_t) => hydra.core.Type.list(recurse(v_Type_list_t))
      case hydra.core.Type.literal(v_Type_literal_lt) => hydra.core.Type.literal(v_Type_literal_lt)
      case hydra.core.Type.map(v_Type_map_mt) => hydra.core.Type.map(hydra.core.MapType(recurse(v_Type_map_mt.keys),
         recurse(v_Type_map_mt.values)))
      case hydra.core.Type.maybe(v_Type_maybe_t) => hydra.core.Type.maybe(recurse(v_Type_maybe_t))
      case hydra.core.Type.record(v_Type_record_rt) => hydra.core.Type.record(hydra.lib.lists.map[hydra.core.FieldType,
         hydra.core.FieldType](forField)(v_Type_record_rt))
      case hydra.core.Type.set(v_Type_set_t) => hydra.core.Type.set(recurse(v_Type_set_t))
      case hydra.core.Type.union(v_Type_union_rt) => hydra.core.Type.union(hydra.lib.lists.map[hydra.core.FieldType,
         hydra.core.FieldType](forField)(v_Type_union_rt))
      case hydra.core.Type.unit => hydra.core.Type.unit
      case hydra.core.Type.variable(v_Type_variable_v) => hydra.core.Type.variable(v_Type_variable_v)
      case hydra.core.Type.void => hydra.core.Type.void
      case hydra.core.Type.wrap(v_Type_wrap_wt) => hydra.core.Type.wrap(recurse(v_Type_wrap_wt))
  }
  def recurse(v1: hydra.core.Type): hydra.core.Type = f((v12: hydra.core.Type) => fsub(recurse)(v12))(v1)
  recurse(typ0)
}

def rewriteTypeM[T0](f: ((hydra.core.Type => Either[T0, hydra.core.Type]) => hydra.core.Type => Either[T0,
   hydra.core.Type]))(typ0: hydra.core.Type): Either[T0, hydra.core.Type] =
  {
  def fsub[T1](recurse: (hydra.core.Type => Either[T1, hydra.core.Type]))(typ: hydra.core.Type): Either[T1,
     hydra.core.Type] =
    typ match
    case hydra.core.Type.annotated(v_Type_annotated_at) => hydra.lib.eithers.bind[T1,
       hydra.core.Type, hydra.core.Type](recurse(v_Type_annotated_at.body))((t: hydra.core.Type) =>
      Right(hydra.core.Type.annotated(hydra.core.AnnotatedType(t, (v_Type_annotated_at.annotation)))))
    case hydra.core.Type.application(v_Type_application_at) => hydra.lib.eithers.bind[T1,
       hydra.core.Type, hydra.core.Type](recurse(v_Type_application_at.function))((lhs: hydra.core.Type) =>
      hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_application_at.argument))((rhs: hydra.core.Type) =>
      Right(hydra.core.Type.application(hydra.core.ApplicationType(lhs, rhs)))))
    case hydra.core.Type.either(v_Type_either_et) => hydra.lib.eithers.bind[T1, hydra.core.Type,
       hydra.core.Type](recurse(v_Type_either_et.left))((left: hydra.core.Type) =>
      hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_either_et.right))((right: hydra.core.Type) =>
      Right(hydra.core.Type.either(hydra.core.EitherType(left, right)))))
    case hydra.core.Type.pair(v_Type_pair_pt) => hydra.lib.eithers.bind[T1, hydra.core.Type,
       hydra.core.Type](recurse(v_Type_pair_pt.first))((pairFirst: hydra.core.Type) =>
      hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_pair_pt.second))((pairSecond: hydra.core.Type) =>
      Right(hydra.core.Type.pair(hydra.core.PairType(pairFirst, pairSecond)))))
    case hydra.core.Type.function(v_Type_function_ft) => hydra.lib.eithers.bind[T1,
       hydra.core.Type, hydra.core.Type](recurse(v_Type_function_ft.domain))((dom: hydra.core.Type) =>
      hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_function_ft.codomain))((cod: hydra.core.Type) =>
      Right(hydra.core.Type.function(hydra.core.FunctionType(dom, cod)))))
    case hydra.core.Type.forall(v_Type_forall_ft) => hydra.lib.eithers.bind[T1, hydra.core.Type,
       hydra.core.Type](recurse(v_Type_forall_ft.body))((b: hydra.core.Type) =>
      Right(hydra.core.Type.forall(hydra.core.ForallType(v_Type_forall_ft.parameter, b))))
    case hydra.core.Type.list(v_Type_list_t) => hydra.lib.eithers.bind[T1, hydra.core.Type,
       hydra.core.Type](recurse(v_Type_list_t))((rt: hydra.core.Type) => Right(hydra.core.Type.list(rt)))
    case hydra.core.Type.literal(v_Type_literal_lt) => Right(hydra.core.Type.literal(v_Type_literal_lt))
    case hydra.core.Type.map(v_Type_map_mt) => hydra.lib.eithers.bind[T1, hydra.core.Type,
       hydra.core.Type](recurse(v_Type_map_mt.keys))((kt: hydra.core.Type) =>
      hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.Type](recurse(v_Type_map_mt.values))((vt: hydra.core.Type) => Right(hydra.core.Type.map(hydra.core.MapType(kt,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         vt)))))
    case hydra.core.Type.maybe(v_Type_maybe_t) => hydra.lib.eithers.bind[T1, hydra.core.Type,
       hydra.core.Type](recurse(v_Type_maybe_t))((rt: hydra.core.Type) => Right(hydra.core.Type.maybe(rt)))
    case hydra.core.Type.record(v_Type_record_rt) => {
      def forField(f2: hydra.core.FieldType): Either[T1, hydra.core.FieldType] =
        hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.FieldType](recurse(f2.`type`))((t: hydra.core.Type) => Right(hydra.core.FieldType(f2.name,
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           t)))
      hydra.lib.eithers.bind[T1, Seq[hydra.core.FieldType], hydra.core.Type](hydra.lib.eithers.mapList[hydra.core.FieldType,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.FieldType, T1](forField)(v_Type_record_rt))((rfields: Seq[hydra.core.FieldType]) => Right(hydra.core.Type.record(rfields)))
    }
    case hydra.core.Type.set(v_Type_set_t) => hydra.lib.eithers.bind[T1, hydra.core.Type,
       hydra.core.Type](recurse(v_Type_set_t))((rt: hydra.core.Type) => Right(hydra.core.Type.set(rt)))
    case hydra.core.Type.union(v_Type_union_rt) => {
      def forField(f2: hydra.core.FieldType): Either[T1, hydra.core.FieldType] =
        hydra.lib.eithers.bind[T1, hydra.core.Type, hydra.core.FieldType](recurse(f2.`type`))((t: hydra.core.Type) => Right(hydra.core.FieldType(f2.name,
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           t)))
      hydra.lib.eithers.bind[T1, Seq[hydra.core.FieldType], hydra.core.Type](hydra.lib.eithers.mapList[hydra.core.FieldType,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.FieldType, T1](forField)(v_Type_union_rt))((rfields: Seq[hydra.core.FieldType]) => Right(hydra.core.Type.union(rfields)))
    }
    case hydra.core.Type.unit => Right(hydra.core.Type.unit)
    case hydra.core.Type.variable(v_Type_variable_v) => Right(hydra.core.Type.variable(v_Type_variable_v))
    case hydra.core.Type.void => Right(hydra.core.Type.void)
    case hydra.core.Type.wrap(v_Type_wrap_wt) => hydra.lib.eithers.bind[T1, hydra.core.Type,
       hydra.core.Type](recurse(v_Type_wrap_wt))((t: hydra.core.Type) => Right(hydra.core.Type.wrap(t)))
  def recurse(v1: hydra.core.Type): Either[T0, hydra.core.Type] = f((v12: hydra.core.Type) => fsub(recurse)(v12))(v1)
  recurse(typ0)
}

def subterms(v1: hydra.core.Term): Seq[hydra.core.Term] =
  v1 match
  case hydra.core.Term.annotated(v_Term_annotated_at) => Seq(v_Term_annotated_at.body)
  case hydra.core.Term.application(v_Term_application_p) => Seq(v_Term_application_p.function,
     (v_Term_application_p.argument))
  case hydra.core.Term.cases(v_Term_cases_cs) => hydra.lib.lists.concat2[hydra.core.Term](hydra.lib.maybes.maybe[Seq[hydra.core.Term],
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term](Seq())((t: hydra.core.Term) => Seq(t))(v_Term_cases_cs.default))(hydra.lib.lists.map[hydra.core.Field,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term]((x: hydra.core.Field) => (x.term))(v_Term_cases_cs.cases))
  case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term,
     hydra.core.Term, Seq[hydra.core.Term]]((l: hydra.core.Term) => Seq(l))((r: hydra.core.Term) => Seq(r))(v_Term_either_e)
  case hydra.core.Term.lambda(v_Term_lambda_l) => Seq(v_Term_lambda_l.body)
  case hydra.core.Term.let(v_Term_let_lt) => hydra.lib.lists.cons[hydra.core.Term](v_Term_let_lt.body)(hydra.lib.lists.map[hydra.core.Binding,
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term]((x: hydra.core.Binding) => (x.term))(v_Term_let_lt.bindings))
  case hydra.core.Term.list(v_Term_list_l) => v_Term_list_l
  case hydra.core.Term.literal(v_Term_literal__) => Seq()
  case hydra.core.Term.map(v_Term_map_m) => hydra.lib.lists.concat[hydra.core.Term](hydra.lib.lists.map[Tuple2[hydra.core.Term,
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term], Seq[hydra.core.Term]]((p: Tuple2[hydra.core.Term, hydra.core.Term]) =>
    Seq(hydra.lib.pairs.first[hydra.core.Term, hydra.core.Term](p), hydra.lib.pairs.second[hydra.core.Term,
       hydra.core.Term](p)))(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m)))
  case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.lib.maybes.maybe[Seq[hydra.core.Term],
     hydra.core.Term](Seq())((t: hydra.core.Term) => Seq(t))(v_Term_maybe_m)
  case hydra.core.Term.pair(v_Term_pair_p) => Seq(hydra.lib.pairs.first[hydra.core.Term,
     hydra.core.Term](v_Term_pair_p), hydra.lib.pairs.second[hydra.core.Term, hydra.core.Term](v_Term_pair_p))
  case hydra.core.Term.project(v_Term_project__) => Seq()
  case hydra.core.Term.record(v_Term_record_rt) => hydra.lib.lists.map[hydra.core.Field,
     hydra.core.Term]((x: hydra.core.Field) => (x.term))(v_Term_record_rt.fields)
  case hydra.core.Term.set(v_Term_set_l) => hydra.lib.sets.toList[hydra.core.Term](v_Term_set_l)
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => Seq(v_Term_typeApplication_ta.body)
  case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => Seq(v_Term_typeLambda_ta.body)
  case hydra.core.Term.inject(v_Term_inject_ut) => Seq(v_Term_inject_ut.field.term)
  case hydra.core.Term.unit => Seq()
  case hydra.core.Term.unwrap(v_Term_unwrap__) => Seq()
  case hydra.core.Term.variable(v_Term_variable__) => Seq()
  case hydra.core.Term.wrap(v_Term_wrap_n) => Seq(v_Term_wrap_n.body)

def subtermsWithSteps(v1: hydra.core.Term): Seq[Tuple2[hydra.paths.SubtermStep, hydra.core.Term]] =
  v1 match
  case hydra.core.Term.annotated(v_Term_annotated_at) => Seq(Tuple2(hydra.paths.SubtermStep.annotatedBody,
     (v_Term_annotated_at.body)))
  case hydra.core.Term.application(v_Term_application_p) => Seq(Tuple2(hydra.paths.SubtermStep.applicationFunction,
     (v_Term_application_p.function)), Tuple2(hydra.paths.SubtermStep.applicationArgument,
     (v_Term_application_p.argument)))
  case hydra.core.Term.cases(v_Term_cases_cs) => hydra.lib.lists.concat2[Tuple2[hydra.paths.SubtermStep,
     hydra.core.Term]](hydra.lib.maybes.maybe[Seq[Tuple2[hydra.paths.SubtermStep,
     hydra.core.Term]], hydra.core.Term](Seq())((t: hydra.core.Term) => Seq(Tuple2(hydra.paths.SubtermStep.unionCasesDefault,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     t)))(v_Term_cases_cs.default))(hydra.lib.lists.map[hydra.core.Field, Tuple2[hydra.paths.SubtermStep,
     hydra.core.Term]]((f: hydra.core.Field) =>
    Tuple2(hydra.paths.SubtermStep.unionCasesBranch(f.name), (f.term)))(v_Term_cases_cs.cases))
  case hydra.core.Term.either(v_Term_either_e) => Seq()
  case hydra.core.Term.lambda(v_Term_lambda_l) => Seq(Tuple2(hydra.paths.SubtermStep.lambdaBody,
     (v_Term_lambda_l.body)))
  case hydra.core.Term.let(v_Term_let_lt) => hydra.lib.lists.cons[Tuple2[hydra.paths.SubtermStep,
     hydra.core.Term]](Tuple2(hydra.paths.SubtermStep.letBody, (v_Term_let_lt.body)))(hydra.lib.lists.map[hydra.core.Binding,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     Tuple2[hydra.paths.SubtermStep, hydra.core.Term]]((b: hydra.core.Binding) => Tuple2(hydra.paths.SubtermStep.letBinding(b.name),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     (b.term)))(v_Term_let_lt.bindings))
  case hydra.core.Term.list(v_Term_list_l) => hydra.lib.lists.map[hydra.core.Term,
     Tuple2[hydra.paths.SubtermStep, hydra.core.Term]]((e: hydra.core.Term) => Tuple2(hydra.paths.SubtermStep.listElement(0),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     e))(v_Term_list_l)
  case hydra.core.Term.literal(v_Term_literal__) => Seq()
  case hydra.core.Term.map(v_Term_map_m) => hydra.lib.lists.concat[Tuple2[hydra.paths.SubtermStep,
     hydra.core.Term]](hydra.lib.lists.map[Tuple2[hydra.core.Term, hydra.core.Term],
     Seq[Tuple2[hydra.paths.SubtermStep, hydra.core.Term]]]((p: Tuple2[hydra.core.Term,
     hydra.core.Term]) =>
    Seq(Tuple2(hydra.paths.SubtermStep.mapKey(0), hydra.lib.pairs.first[hydra.core.Term,
       hydra.core.Term](p)), Tuple2(hydra.paths.SubtermStep.mapValue(0), hydra.lib.pairs.second[hydra.core.Term,
       hydra.core.Term](p))))(hydra.lib.maps.toList[hydra.core.Term, hydra.core.Term](v_Term_map_m)))
  case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.lib.maybes.maybe[Seq[Tuple2[hydra.paths.SubtermStep,
     hydra.core.Term]], hydra.core.Term](Seq())((t: hydra.core.Term) => Seq(Tuple2(hydra.paths.SubtermStep.maybeTerm,
     t)))(v_Term_maybe_m)
  case hydra.core.Term.pair(v_Term_pair_p) => Seq()
  case hydra.core.Term.project(v_Term_project__) => Seq()
  case hydra.core.Term.record(v_Term_record_rt) => hydra.lib.lists.map[hydra.core.Field,
     Tuple2[hydra.paths.SubtermStep, hydra.core.Term]]((f: hydra.core.Field) => Tuple2(hydra.paths.SubtermStep.recordField(f.name),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     (f.term)))(v_Term_record_rt.fields)
  case hydra.core.Term.set(v_Term_set_s) => hydra.lib.lists.map[hydra.core.Term, Tuple2[hydra.paths.SubtermStep,
     hydra.core.Term]]((e: hydra.core.Term) => Tuple2(hydra.paths.SubtermStep.listElement(0),
     e))(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s))
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => Seq(Tuple2(hydra.paths.SubtermStep.typeApplicationTerm,
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     (v_Term_typeApplication_ta.body)))
  case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => Seq(Tuple2(hydra.paths.SubtermStep.typeLambdaBody,
     (v_Term_typeLambda_ta.body)))
  case hydra.core.Term.inject(v_Term_inject_ut) => Seq(Tuple2(hydra.paths.SubtermStep.injectionTerm,
     (v_Term_inject_ut.field.term)))
  case hydra.core.Term.unit => Seq()
  case hydra.core.Term.unwrap(v_Term_unwrap__) => Seq()
  case hydra.core.Term.variable(v_Term_variable__) => Seq()
  case hydra.core.Term.wrap(v_Term_wrap_n) => Seq(Tuple2(hydra.paths.SubtermStep.wrappedTerm, (v_Term_wrap_n.body)))

def subtypes(v1: hydra.core.Type): Seq[hydra.core.Type] =
  v1 match
  case hydra.core.Type.annotated(v_Type_annotated_at) => Seq(v_Type_annotated_at.body)
  case hydra.core.Type.application(v_Type_application_at) => Seq(v_Type_application_at.function,
     (v_Type_application_at.argument))
  case hydra.core.Type.either(v_Type_either_et) => Seq(v_Type_either_et.left, (v_Type_either_et.right))
  case hydra.core.Type.pair(v_Type_pair_pt) => Seq(v_Type_pair_pt.first, (v_Type_pair_pt.second))
  case hydra.core.Type.function(v_Type_function_ft) => Seq(v_Type_function_ft.domain, (v_Type_function_ft.codomain))
  case hydra.core.Type.forall(v_Type_forall_lt) => Seq(v_Type_forall_lt.body)
  case hydra.core.Type.list(v_Type_list_lt) => Seq(v_Type_list_lt)
  case hydra.core.Type.literal(v_Type_literal__) => Seq()
  case hydra.core.Type.map(v_Type_map_mt) => Seq(v_Type_map_mt.keys, (v_Type_map_mt.values))
  case hydra.core.Type.maybe(v_Type_maybe_ot) => Seq(v_Type_maybe_ot)
  case hydra.core.Type.record(v_Type_record_rt) => hydra.lib.lists.map[hydra.core.FieldType,
     hydra.core.Type]((x: hydra.core.FieldType) => (x.`type`))(v_Type_record_rt)
  case hydra.core.Type.set(v_Type_set_st) => Seq(v_Type_set_st)
  case hydra.core.Type.union(v_Type_union_rt) => hydra.lib.lists.map[hydra.core.FieldType,
     hydra.core.Type]((x: hydra.core.FieldType) => (x.`type`))(v_Type_union_rt)
  case hydra.core.Type.unit => Seq()
  case hydra.core.Type.variable(v_Type_variable__) => Seq()
  case hydra.core.Type.void => Seq()
  case hydra.core.Type.wrap(v_Type_wrap_nt) => Seq(v_Type_wrap_nt)
