package hydra.variables

import hydra.coders.*

import hydra.core.*

def freeTypeVariablesInTerm(term0: hydra.core.Term): scala.collection.immutable.Set[hydra.core.Name] =
  {
  def allOf[T0](sets: Seq[scala.collection.immutable.Set[T0]]): scala.collection.immutable.Set[T0] =
    hydra.lib.lists.foldl[scala.collection.immutable.Set[T0], scala.collection.immutable.Set[T0]](hydra.lib.sets.union[T0])(hydra.lib.sets.empty[T0])(sets)
  def tryType(tvars: scala.collection.immutable.Set[hydra.core.Name])(typ: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
    hydra.lib.sets.difference[hydra.core.Name](hydra.variables.freeVariablesInType(typ))(tvars)
  def getAll(vars: scala.collection.immutable.Set[hydra.core.Name])(term: hydra.core.Term): scala.collection.immutable.Set[hydra.core.Name] =
    {
    def recurse(v1: hydra.core.Term): scala.collection.immutable.Set[hydra.core.Name] = getAll(vars)(v1)
    lazy val dflt: scala.collection.immutable.Set[hydra.core.Name] = allOf(hydra.lib.lists.map[hydra.core.Term,
       scala.collection.immutable.Set[hydra.core.Name]](recurse)(hydra.rewriting.subterms(term)))
    term match
      case hydra.core.Term.lambda(v_Term_lambda_l) => {
        lazy val domt: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.maybes.maybe[scala.collection.immutable.Set[hydra.core.Name],
          
          
          
          
          
          
          
          
          
          
          
          
          
          
           hydra.core.Type](hydra.lib.sets.empty[hydra.core.Name])((v1: hydra.core.Type) => tryType(vars)(v1))(v_Term_lambda_l.domain)
        hydra.lib.sets.union[hydra.core.Name](domt)(recurse(v_Term_lambda_l.body))
      }
      case hydra.core.Term.let(v_Term_let_l) => {
        def forBinding(b: hydra.core.Binding): scala.collection.immutable.Set[hydra.core.Name] =
          {
          lazy val newVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.maybes.maybe[scala.collection.immutable.Set[hydra.core.Name],
            
            
            
            
            
            
            
            
            
            
            
            
            
            
             hydra.core.TypeScheme](vars)((ts: hydra.core.TypeScheme) =>
            hydra.lib.sets.union[hydra.core.Name](vars)(hydra.lib.sets.fromList[hydra.core.Name](ts.variables)))(b.`type`)
          hydra.lib.sets.union[hydra.core.Name](getAll(newVars)(b.term))(hydra.lib.maybes.maybe[scala.collection.immutable.Set[hydra.core.Name],
            
            
            
            
            
            
            
            
            
            
            
            
            
            
             hydra.core.TypeScheme](hydra.lib.sets.empty[hydra.core.Name])((ts: hydra.core.TypeScheme) => tryType(newVars)(ts.`type`))(b.`type`))
        }
        hydra.lib.sets.union[hydra.core.Name](allOf(hydra.lib.lists.map[hydra.core.Binding,
           scala.collection.immutable.Set[hydra.core.Name]](forBinding)(v_Term_let_l.bindings)))(recurse(v_Term_let_l.body))
      }
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.lib.sets.union[hydra.core.Name](tryType(vars)(v_Term_typeApplication_tt.`type`))(recurse(v_Term_typeApplication_tt.body))
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.lib.sets.union[hydra.core.Name](tryType(vars)(hydra.core.Type.variable(v_Term_typeLambda_tl.parameter)))(recurse(v_Term_typeLambda_tl.body))
      case _ => dflt
  }
  getAll(hydra.lib.sets.empty[hydra.core.Name])(term0)
}

def freeVariablesInTerm(term: hydra.core.Term): scala.collection.immutable.Set[hydra.core.Name] =
  {
  def dfltVars[T0](_x: T0): scala.collection.immutable.Set[hydra.core.Name] =
    hydra.lib.lists.foldl[scala.collection.immutable.Set[hydra.core.Name], hydra.core.Term]((s: scala.collection.immutable.Set[hydra.core.Name]) =>
    (t: hydra.core.Term) =>
    hydra.lib.sets.union[hydra.core.Name](s)(hydra.variables.freeVariablesInTerm(t)))(hydra.lib.sets.empty[hydra.core.Name])(hydra.rewriting.subterms(term))
  term match
    case hydra.core.Term.lambda(v_Term_lambda_l) => hydra.lib.sets.delete[hydra.core.Name](v_Term_lambda_l.parameter)(hydra.variables.freeVariablesInTerm(v_Term_lambda_l.body))
    case hydra.core.Term.let(v_Term_let_l) => hydra.lib.sets.difference[hydra.core.Name](dfltVars(()))(hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.map[hydra.core.Binding,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       hydra.core.Name]((x: hydra.core.Binding) => (x.name))(v_Term_let_l.bindings)))
    case hydra.core.Term.variable(v_Term_variable_v) => hydra.lib.sets.singleton[hydra.core.Name](v_Term_variable_v)
    case _ => dfltVars(())
}

def freeVariablesInType(typ: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
  {
  lazy val dfltVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.lists.foldl[scala.collection.immutable.Set[hydra.core.Name],
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Type]((s: scala.collection.immutable.Set[hydra.core.Name]) =>
    (t: hydra.core.Type) =>
    hydra.lib.sets.union[hydra.core.Name](s)(hydra.variables.freeVariablesInType(t)))(hydra.lib.sets.empty[hydra.core.Name])(hydra.rewriting.subtypes(typ))
  typ match
    case hydra.core.Type.forall(v_Type_forall_lt) => hydra.lib.sets.delete[hydra.core.Name](v_Type_forall_lt.parameter)(hydra.variables.freeVariablesInType(v_Type_forall_lt.body))
    case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.sets.singleton[hydra.core.Name](v_Type_variable_v)
    case _ => dfltVars
}

def freeVariablesInTypeOrdered(typ: hydra.core.Type): Seq[hydra.core.Name] =
  {
  def collectVars(boundVars: scala.collection.immutable.Set[hydra.core.Name])(t: hydra.core.Type): Seq[hydra.core.Name] =
    t match
    case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.logic.ifElse[Seq[hydra.core.Name]](hydra.lib.sets.member[hydra.core.Name](v_Type_variable_v)(boundVars))(Seq())(Seq(v_Type_variable_v))
    case hydra.core.Type.forall(v_Type_forall_ft) => collectVars(hydra.lib.sets.insert[hydra.core.Name](v_Type_forall_ft.parameter)(boundVars))(v_Type_forall_ft.body)
    case _ => hydra.lib.lists.concat[hydra.core.Name](hydra.lib.lists.map[hydra.core.Type,
       Seq[hydra.core.Name]]((v1: hydra.core.Type) => collectVars(boundVars)(v1))(hydra.rewriting.subtypes(t)))
  hydra.lib.lists.nub[hydra.core.Name](collectVars(hydra.lib.sets.empty[hydra.core.Name])(typ))
}

def freeVariablesInTypeScheme(ts: hydra.core.TypeScheme): scala.collection.immutable.Set[hydra.core.Name] =
  {
  lazy val vars: Seq[hydra.core.Name] = (ts.variables)
  lazy val t: hydra.core.Type = (ts.`type`)
  hydra.lib.sets.difference[hydra.core.Name](hydra.variables.freeVariablesInType(t))(hydra.lib.sets.fromList[hydra.core.Name](vars))
}

def freeVariablesInTypeSchemeSimple(ts: hydra.core.TypeScheme): scala.collection.immutable.Set[hydra.core.Name] =
  {
  lazy val vars: Seq[hydra.core.Name] = (ts.variables)
  lazy val t: hydra.core.Type = (ts.`type`)
  hydra.lib.sets.difference[hydra.core.Name](hydra.variables.freeVariablesInTypeSimple(t))(hydra.lib.sets.fromList[hydra.core.Name](vars))
}

def freeVariablesInTypeSimple(typ: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
  {
  def helper(types: scala.collection.immutable.Set[hydra.core.Name])(typ2: hydra.core.Type): scala.collection.immutable.Set[hydra.core.Name] =
    typ2 match
    case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.sets.insert[hydra.core.Name](v_Type_variable_v)(types)
    case _ => types
  hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)(helper)(hydra.lib.sets.empty[hydra.core.Name])(typ)
}

def isFreeVariableInTerm(v: hydra.core.Name)(term: hydra.core.Term): Boolean =
  hydra.lib.logic.not(hydra.lib.sets.member[hydra.core.Name](v)(hydra.variables.freeVariablesInTerm(term)))

def normalizeTypeVariablesInTerm(term: hydra.core.Term): hydra.core.Term =
  {
  def replaceName[T0](subst: Map[T0, T0])(v: T0): T0 = hydra.lib.maybes.fromMaybe[T0](v)(hydra.lib.maps.lookup[T0,
     T0](v)(subst))
  def substType(subst: Map[hydra.core.Name, hydra.core.Name])(typ: hydra.core.Type): hydra.core.Type =
    {
    def rewrite(recurse: (hydra.core.Type => hydra.core.Type))(typ2: hydra.core.Type): hydra.core.Type =
      typ2 match
      case hydra.core.Type.variable(v_Type_variable_v) => hydra.core.Type.variable(replaceName(subst)(v_Type_variable_v))
      case _ => recurse(typ2)
    hydra.rewriting.rewriteType(rewrite)(typ)
  }
  def rewriteWithSubst(state: Tuple2[Tuple2[Map[hydra.core.Name, hydra.core.Name],
     scala.collection.immutable.Set[hydra.core.Name]], Int])(term0: hydra.core.Term): hydra.core.Term =
    {
    lazy val sb: Tuple2[Map[hydra.core.Name, hydra.core.Name], scala.collection.immutable.Set[hydra.core.Name]] = hydra.lib.pairs.first[Tuple2[Map[hydra.core.Name,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       hydra.core.Name], scala.collection.immutable.Set[hydra.core.Name]], Int](state)
    lazy val next: Int = hydra.lib.pairs.second[Tuple2[Map[hydra.core.Name, hydra.core.Name],
       scala.collection.immutable.Set[hydra.core.Name]], Int](state)
    lazy val subst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.pairs.first[Map[hydra.core.Name,
       hydra.core.Name], scala.collection.immutable.Set[hydra.core.Name]](sb)
    lazy val boundVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.pairs.second[Map[hydra.core.Name,
       hydra.core.Name], scala.collection.immutable.Set[hydra.core.Name]](sb)
    def rewrite(recurse: (hydra.core.Term => hydra.core.Term))(term2: hydra.core.Term): hydra.core.Term =
      term2 match
      case hydra.core.Term.lambda(v_Term_lambda_l) => {
        lazy val domain: Option[hydra.core.Type] = (v_Term_lambda_l.domain)
        hydra.core.Term.lambda(hydra.core.Lambda(v_Term_lambda_l.parameter, hydra.lib.maybes.map[hydra.core.Type,
           hydra.core.Type]((v1: hydra.core.Type) => substType(subst)(v1))(domain),
           rewriteWithSubst(Tuple2(Tuple2(subst, boundVars), next))(v_Term_lambda_l.body)))
      }
      case hydra.core.Term.let(v_Term_let_lt) => {
        lazy val bindings0: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
        {
          lazy val body0: hydra.core.Term = (v_Term_let_lt.body)
          {
            def step(acc: Seq[hydra.core.Binding])(bs: Seq[hydra.core.Binding]): Seq[hydra.core.Binding] =
              hydra.lib.maybes.maybe[Seq[hydra.core.Binding], Tuple2[hydra.core.Binding,
                 Seq[hydra.core.Binding]]](hydra.lib.lists.reverse[hydra.core.Binding](acc))((uc: Tuple2[hydra.core.Binding,
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                 Seq[hydra.core.Binding]]) =>
              {
              lazy val b: hydra.core.Binding = hydra.lib.pairs.first[hydra.core.Binding, Seq[hydra.core.Binding]](uc)
              {
                lazy val tl: Seq[hydra.core.Binding] = hydra.lib.pairs.second[hydra.core.Binding,
                   Seq[hydra.core.Binding]](uc)
                {
                  lazy val noType: Seq[hydra.core.Binding] = {
                    lazy val newVal: hydra.core.Term = rewriteWithSubst(Tuple2(Tuple2(subst, boundVars), next))(b.term)
                    {
                      lazy val b1: hydra.core.Binding = hydra.core.Binding(b.name, newVal, None)
                      step(hydra.lib.lists.cons[hydra.core.Binding](b1)(acc))(tl)
                    }
                  }
                  {
                    def withType(ts: hydra.core.TypeScheme): Seq[hydra.core.Binding] =
                      {
                      lazy val vars: Seq[hydra.core.Name] = (ts.variables)
                      lazy val typ: hydra.core.Type = (ts.`type`)
                      lazy val k: Int = hydra.lib.lists.length[hydra.core.Name](vars)
                      def gen(i: Int)(rem: Int)(acc2: Seq[hydra.core.Name]): Seq[hydra.core.Name] =
                        {
                        lazy val ti: hydra.core.Name = hydra.lib.strings.cat2("t")(hydra.lib.literals.showInt32(hydra.lib.math.add(next)(i)))
                        hydra.lib.logic.ifElse[Seq[hydra.core.Name]](hydra.lib.equality.equal[Int](rem)(0))(hydra.lib.lists.reverse[hydra.core.Name](acc2))(gen(hydra.lib.math.add(i)(1))(hydra.lib.math.sub(rem)(1))(hydra.lib.lists.cons[hydra.core.Name](ti)(acc2)))
                      }
                      lazy val newVars: Seq[hydra.core.Name] = gen(0)(k)(Seq())
                      lazy val newSubst: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maps.union[hydra.core.Name,
                         hydra.core.Name](hydra.lib.maps.fromList[hydra.core.Name,
                         hydra.core.Name](hydra.lib.lists.zip[hydra.core.Name, hydra.core.Name](vars)(newVars)))(subst)
                      lazy val newBound: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.union[hydra.core.Name](boundVars)(hydra.lib.sets.fromList[hydra.core.Name](newVars))
                      lazy val newVal: hydra.core.Term = rewriteWithSubst(Tuple2(Tuple2(newSubst,
                         newBound), hydra.lib.math.add(next)(k)))(b.term)
                      def renameConstraintKeys[T0](constraintMap: Map[hydra.core.Name, T0]): Map[hydra.core.Name, T0] =
                        hydra.lib.maps.fromList[hydra.core.Name, T0](hydra.lib.lists.map[Tuple2[hydra.core.Name,
                           T0], Tuple2[hydra.core.Name, T0]]((p: Tuple2[hydra.core.Name,
                           T0]) =>
                        {
                        lazy val oldName: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, T0](p)
                        {
                          lazy val meta: T0 = hydra.lib.pairs.second[hydra.core.Name, T0](p)
                          {
                            lazy val newName: hydra.core.Name = hydra.lib.maybes.fromMaybe[hydra.core.Name](oldName)(hydra.lib.maps.lookup[hydra.core.Name,
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                               hydra.core.Name](oldName)(newSubst))
                            Tuple2(newName, meta)
                          }
                        }
                      })(hydra.lib.maps.toList[hydra.core.Name, T0](constraintMap)))
                      lazy val oldConstraints: Option[Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = (ts.constraints)
                      lazy val newConstraints: Option[Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = hydra.lib.maybes.map[Map[hydra.core.Name,
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                         hydra.core.TypeVariableMetadata], Map[hydra.core.Name, hydra.core.TypeVariableMetadata]](renameConstraintKeys)(oldConstraints)
                      lazy val b1: hydra.core.Binding = hydra.core.Binding(b.name,
                         newVal, Some(hydra.core.TypeScheme(newVars, substType(newSubst)(typ),
                         newConstraints)))
                      step(hydra.lib.lists.cons[hydra.core.Binding](b1)(acc))(tl)
                    }
                    hydra.lib.maybes.maybe[Seq[hydra.core.Binding], hydra.core.TypeScheme](noType)((ts: hydra.core.TypeScheme) => withType(ts))(b.`type`)
                  }
                }
              }
            })(hydra.lib.lists.uncons[hydra.core.Binding](bs))
            {
              lazy val bindings1: Seq[hydra.core.Binding] = step(Seq())(bindings0)
              hydra.core.Term.let(hydra.core.Let(bindings1, rewriteWithSubst(Tuple2(Tuple2(subst,
                 boundVars), next))(body0)))
            }
          }
        }
      }
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(rewriteWithSubst(Tuple2(Tuple2(subst,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         boundVars), next))(v_Term_typeApplication_tt.body), substType(subst)(v_Term_typeApplication_tt.`type`)))
      case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(replaceName(subst)(v_Term_typeLambda_ta.parameter),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         rewriteWithSubst(Tuple2(Tuple2(subst, boundVars), next))(v_Term_typeLambda_ta.body)))
      case _ => recurse(term2)
    hydra.rewriting.rewriteTerm(rewrite)(term0)
  }
  rewriteWithSubst(Tuple2(Tuple2(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name],
     hydra.lib.sets.empty[hydra.core.Name]), 0))(term)
}

def replaceFreeTermVariable(vold: hydra.core.Name)(tnew: hydra.core.Term)(term: hydra.core.Term): hydra.core.Term =
  {
  def rewrite(recurse: (hydra.core.Term => hydra.core.Term))(t: hydra.core.Term): hydra.core.Term =
    t match
    case hydra.core.Term.lambda(v_Term_lambda_l) => {
      lazy val v: hydra.core.Name = (v_Term_lambda_l.parameter)
      hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Name](v)(vold))(t)(recurse(t))
    }
    case hydra.core.Term.variable(v_Term_variable_v) => hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Name](v_Term_variable_v)(vold))(tnew)(hydra.core.Term.variable(v_Term_variable_v))
    case _ => recurse(t)
  hydra.rewriting.rewriteTerm(rewrite)(term)
}

def replaceFreeTypeVariable(v: hydra.core.Name)(rep: hydra.core.Type)(typ: hydra.core.Type): hydra.core.Type =
  {
  def mapExpr(recurse: (hydra.core.Type => hydra.core.Type))(t: hydra.core.Type): hydra.core.Type =
    t match
    case hydra.core.Type.forall(v_Type_forall_ft) => hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.equality.equal[hydra.core.Name](v)(v_Type_forall_ft.parameter))(t)(hydra.core.Type.forall(hydra.core.ForallType(v_Type_forall_ft.parameter,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       recurse(v_Type_forall_ft.body))))
    case hydra.core.Type.variable(v_Type_variable_v_) => hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.equality.equal[hydra.core.Name](v)(`v_Type_variable_v_`))(rep)(t)
    case _ => recurse(t)
  hydra.rewriting.rewriteType(mapExpr)(typ)
}

def substituteTypeVariables(subst: Map[hydra.core.Name, hydra.core.Name])(typ: hydra.core.Type): hydra.core.Type =
  {
  def replace(recurse: (hydra.core.Type => hydra.core.Type))(typ2: hydra.core.Type): hydra.core.Type =
    typ2 match
    case hydra.core.Type.variable(v_Type_variable_n) => hydra.core.Type.variable(hydra.lib.maybes.fromMaybe[hydra.core.Name](v_Type_variable_n)(hydra.lib.maps.lookup[hydra.core.Name,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       hydra.core.Name](v_Type_variable_n)(subst)))
    case _ => recurse(typ2)
  hydra.rewriting.rewriteType(replace)(typ)
}

def substituteTypeVariablesInTerm(subst: Map[hydra.core.Name, hydra.core.Name])(term: hydra.core.Term): hydra.core.Term =
  {
  def st(v1: hydra.core.Type): hydra.core.Type = hydra.variables.substituteTypeVariables(subst)(v1)
  def stOpt(mt: Option[hydra.core.Type]): Option[hydra.core.Type] = hydra.lib.maybes.map[hydra.core.Type,
     hydra.core.Type](st)(mt)
  def stScheme(ts: hydra.core.TypeScheme): hydra.core.TypeScheme = hydra.core.TypeScheme(ts.variables,
     st(ts.`type`), (ts.constraints))
  def stSchemeOpt(mts: Option[hydra.core.TypeScheme]): Option[hydra.core.TypeScheme] =
    hydra.lib.maybes.map[hydra.core.TypeScheme, hydra.core.TypeScheme](stScheme)(mts)
  def replace(recurse: (hydra.core.Term => hydra.core.Term))(t: hydra.core.Term): hydra.core.Term =
    t match
    case hydra.core.Term.lambda(v_Term_lambda_l) => hydra.core.Term.lambda(hydra.core.Lambda(v_Term_lambda_l.parameter,
       stOpt(v_Term_lambda_l.domain), recurse(v_Term_lambda_l.body)))
    case hydra.core.Term.let(v_Term_let_lt) => {
      def mapBinding(b: hydra.core.Binding): hydra.core.Binding = hydra.core.Binding(b.name,
         recurse(b.term), stSchemeOpt(b.`type`))
      hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding](mapBinding)(v_Term_let_lt.bindings),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         recurse(v_Term_let_lt.body)))
    }
    case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(recurse(v_Term_typeApplication_tt.body),
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       st(v_Term_typeApplication_tt.`type`)))
    case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.core.Term.typeLambda(hydra.core.TypeLambda(hydra.lib.maybes.fromMaybe[hydra.core.Name](v_Term_typeLambda_tl.parameter)(hydra.lib.maps.lookup[hydra.core.Name,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       hydra.core.Name](v_Term_typeLambda_tl.parameter)(subst)), recurse(v_Term_typeLambda_tl.body)))
    case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.core.Term.annotated(hydra.core.AnnotatedTerm(recurse(v_Term_annotated_at.body),
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       (v_Term_annotated_at.annotation)))
    case _ => recurse(t)
  hydra.rewriting.rewriteTerm(replace)(term)
}

def substituteVariable(from: hydra.core.Name)(to: hydra.core.Name)(term: hydra.core.Term): hydra.core.Term =
  {
  def replace(recurse: (hydra.core.Term => hydra.core.Term))(term2: hydra.core.Term): hydra.core.Term =
    term2 match
    case hydra.core.Term.variable(v_Term_variable_x) => hydra.core.Term.variable(hydra.lib.logic.ifElse[hydra.core.Name](hydra.lib.equality.equal[hydra.core.Name](v_Term_variable_x)(from))(to)(v_Term_variable_x))
    case hydra.core.Term.lambda(v_Term_lambda_l) => hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Name](v_Term_lambda_l.parameter)(from))(term2)(recurse(term2))
    case _ => recurse(term2)
  hydra.rewriting.rewriteTerm(replace)(term)
}

def substituteVariables(subst: Map[hydra.core.Name, hydra.core.Name])(term: hydra.core.Term): hydra.core.Term =
  {
  def replace(recurse: (hydra.core.Term => hydra.core.Term))(term2: hydra.core.Term): hydra.core.Term =
    term2 match
    case hydra.core.Term.variable(v_Term_variable_n) => hydra.core.Term.variable(hydra.lib.maybes.fromMaybe[hydra.core.Name](v_Term_variable_n)(hydra.lib.maps.lookup[hydra.core.Name,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       hydra.core.Name](v_Term_variable_n)(subst)))
    case hydra.core.Term.lambda(v_Term_lambda_l) => hydra.lib.maybes.maybe[hydra.core.Term,
       hydra.core.Name](recurse(term2))((_x: hydra.core.Name) => term2)(hydra.lib.maps.lookup[hydra.core.Name,
       hydra.core.Name](v_Term_lambda_l.parameter)(subst))
    case _ => recurse(term2)
  hydra.rewriting.rewriteTerm(replace)(term)
}

def unshadowVariables(term0: hydra.core.Term): hydra.core.Term =
  {
  def freshName[T0](base: hydra.core.Name)(i: Int)(m: Map[hydra.core.Name, T0]): hydra.core.Name =
    {
    lazy val candidate: hydra.core.Name = hydra.lib.strings.cat2(base)(hydra.lib.literals.showInt32(i))
    hydra.lib.logic.ifElse[hydra.core.Name](hydra.lib.maps.member[hydra.core.Name,
       T0](candidate)(m))(freshName(base)(hydra.lib.math.add(i)(1))(m))(candidate)
  }
  def f(recurse: (Map[hydra.core.Name, hydra.core.Name] => hydra.core.Term => hydra.core.Term))(m: Map[hydra.core.Name,
     hydra.core.Name])(term: hydra.core.Term): hydra.core.Term =
    term match
    case hydra.core.Term.lambda(v_Term_lambda_l) => {
      lazy val v: hydra.core.Name = (v_Term_lambda_l.parameter)
      {
        lazy val domain: Option[hydra.core.Type] = (v_Term_lambda_l.domain)
        {
          lazy val body: hydra.core.Term = (v_Term_lambda_l.body)
          hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.maps.member[hydra.core.Name, hydra.core.Name](v)(m))({
            lazy val v2: hydra.core.Name = freshName(v)(2)(m)
            {
              lazy val m2: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.maps.insert[hydra.core.Name,
                 hydra.core.Name](v)(v2)(hydra.lib.maps.insert[hydra.core.Name, hydra.core.Name](v2)(v2)(m))
              hydra.core.Term.lambda(hydra.core.Lambda(v2, domain, f(recurse)(m2)(body)))
            }
          })(hydra.core.Term.lambda(hydra.core.Lambda(v, domain, f(recurse)(hydra.lib.maps.insert[hydra.core.Name,
             hydra.core.Name](v)(v)(m))(body))))
        }
      }
    }
    case hydra.core.Term.let(v_Term_let_lt) => {
      lazy val m2: Map[hydra.core.Name, hydra.core.Name] = hydra.lib.lists.foldl[Map[hydra.core.Name,
         hydra.core.Name], hydra.core.Binding]((acc: Map[hydra.core.Name, hydra.core.Name]) =>
        (b: hydra.core.Binding) =>
        {
        lazy val bname: hydra.core.Name = (b.name)
        hydra.lib.logic.ifElse[Map[hydra.core.Name, hydra.core.Name]](hydra.lib.maps.member[hydra.core.Name,
           hydra.core.Name](bname)(acc))(acc)(hydra.lib.maps.insert[hydra.core.Name,
           hydra.core.Name](bname)(bname)(acc))
      })(m)(v_Term_let_lt.bindings)
      recurse(m2)(term)
    }
    case hydra.core.Term.variable(v_Term_variable_v) => hydra.core.Term.variable(hydra.lib.maybes.maybe[hydra.core.Name,
       hydra.core.Name](v_Term_variable_v)((renamed: hydra.core.Name) => renamed)(hydra.lib.maps.lookup[hydra.core.Name,
       hydra.core.Name](v_Term_variable_v)(m)))
    case _ => recurse(m)(term)
  hydra.rewriting.rewriteTermWithContext(f)(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Name])(term0)
}
