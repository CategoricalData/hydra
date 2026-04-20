package hydra.substitution

import hydra.core.*

import hydra.graph.*

import hydra.typing.*

def composeTypeSubst(s1: hydra.typing.TypeSubst)(s2: hydra.typing.TypeSubst): hydra.typing.TypeSubst =
  hydra.lib.logic.ifElse[hydra.typing.TypeSubst](hydra.lib.maps.`null`[hydra.core.Name,
     hydra.core.Type](s1))(s2)(hydra.lib.logic.ifElse[hydra.typing.TypeSubst](hydra.lib.maps.`null`[hydra.core.Name,
     hydra.core.Type](s2))(s1)(hydra.substitution.composeTypeSubstNonEmpty(s1)(s2)))

def composeTypeSubstList(v1: Seq[hydra.typing.TypeSubst]): hydra.typing.TypeSubst =
  hydra.lib.lists.foldl[hydra.typing.TypeSubst, hydra.typing.TypeSubst](hydra.substitution.composeTypeSubst)(hydra.substitution.idTypeSubst)(v1)

def composeTypeSubstNonEmpty(s1: hydra.typing.TypeSubst)(s2: hydra.typing.TypeSubst): hydra.typing.TypeSubst =
  {
  def isExtra[T0](k: hydra.core.Name)(v: T0): Boolean =
    hydra.lib.maybes.isNothing[hydra.core.Type](hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Type](k)(s1))
  lazy val withExtra: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maps.filterWithKey[hydra.core.Name,
     hydra.core.Type](isExtra)(s2)
  hydra.lib.maps.union[hydra.core.Name, hydra.core.Type](withExtra)(hydra.lib.maps.map[hydra.core.Type,
     hydra.core.Type, hydra.core.Name]((v1: hydra.core.Type) => hydra.substitution.substInType(s2)(v1))(s1))
}

lazy val idTypeSubst: hydra.typing.TypeSubst = hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type]

def singletonTypeSubst(v: hydra.core.Name)(t: hydra.core.Type): hydra.typing.TypeSubst = hydra.lib.maps.singleton[hydra.core.Name,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   hydra.core.Type](v)(t)

def substInClassConstraints(subst: hydra.typing.TypeSubst)(constraints: Map[hydra.core.Name,
   hydra.core.TypeVariableMetadata]): Map[hydra.core.Name, hydra.core.TypeVariableMetadata] =
  {
  lazy val substMap: Map[hydra.core.Name, hydra.core.Type] = subst
  def insertOrMerge[T0](varName: T0)(metadata: hydra.core.TypeVariableMetadata)(acc: Map[T0,
     hydra.core.TypeVariableMetadata]): Map[T0, hydra.core.TypeVariableMetadata] =
    hydra.lib.maybes.maybe[Map[T0, hydra.core.TypeVariableMetadata], hydra.core.TypeVariableMetadata](hydra.lib.maps.insert[T0,
      
      
      
      
      
      
      
      
      
      
      
      
      
      
       hydra.core.TypeVariableMetadata](varName)(metadata)(acc))((existing: hydra.core.TypeVariableMetadata) =>
    {
    lazy val merged: hydra.core.TypeVariableMetadata = hydra.core.TypeVariableMetadata(hydra.lib.sets.union[hydra.core.Name](existing.classes)(metadata.classes))
    hydra.lib.maps.insert[T0, hydra.core.TypeVariableMetadata](varName)(merged)(acc)
  })(hydra.lib.maps.lookup[T0, hydra.core.TypeVariableMetadata](varName)(acc))
  hydra.lib.lists.foldl[Map[hydra.core.Name, hydra.core.TypeVariableMetadata], Tuple2[hydra.core.Name,
     hydra.core.TypeVariableMetadata]]((acc: Map[hydra.core.Name, hydra.core.TypeVariableMetadata]) =>
    (pair: Tuple2[hydra.core.Name, hydra.core.TypeVariableMetadata]) =>
    {
    lazy val varName: hydra.core.Name = hydra.lib.pairs.first[hydra.core.Name, hydra.core.TypeVariableMetadata](pair)
    {
      lazy val metadata: hydra.core.TypeVariableMetadata = hydra.lib.pairs.second[hydra.core.Name,
         hydra.core.TypeVariableMetadata](pair)
      hydra.lib.maybes.maybe[Map[hydra.core.Name, hydra.core.TypeVariableMetadata],
         hydra.core.Type](insertOrMerge(varName)(metadata)(acc))((targetType: hydra.core.Type) =>
        {
        lazy val freeVars: Seq[hydra.core.Name] = hydra.lib.sets.toList[hydra.core.Name](hydra.variables.freeVariablesInType(targetType))
        hydra.lib.lists.foldl[Map[hydra.core.Name, hydra.core.TypeVariableMetadata],
           hydra.core.Name]((acc2: Map[hydra.core.Name, hydra.core.TypeVariableMetadata]) =>
          (freeVar: hydra.core.Name) => insertOrMerge(freeVar)(metadata)(acc2))(acc)(freeVars)
      })(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Type](varName)(substMap))
    }
  })(hydra.lib.maps.empty[hydra.core.Name, hydra.core.TypeVariableMetadata])(hydra.lib.maps.toList[hydra.core.Name,
     hydra.core.TypeVariableMetadata](constraints))
}

def substInContext(subst: hydra.typing.TypeSubst)(cx: hydra.graph.Graph): hydra.graph.Graph =
  {
  lazy val newBoundTypes: Map[hydra.core.Name, hydra.core.TypeScheme] = hydra.lib.maps.map[hydra.core.TypeScheme,
     hydra.core.TypeScheme, hydra.core.Name]((v1: hydra.core.TypeScheme) => hydra.substitution.substInTypeScheme(subst)(v1))(cx.boundTypes)
  lazy val newClassConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata] = hydra.substitution.substInClassConstraints(subst)(cx.classConstraints)
  lazy val cx2: hydra.graph.Graph = hydra.graph.Graph(cx.boundTerms, newBoundTypes,
     (cx.classConstraints), (cx.lambdaVariables), (cx.metadata), (cx.primitives),
     (cx.schemaTypes), (cx.typeVariables))
  hydra.graph.Graph(cx2.boundTerms, (cx2.boundTypes), newClassConstraints, (cx2.lambdaVariables),
     (cx2.metadata), (cx2.primitives), (cx2.schemaTypes), (cx2.typeVariables))
}

def substInType(subst: hydra.typing.TypeSubst)(typ0: hydra.core.Type): hydra.core.Type =
  hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.maps.`null`[hydra.core.Name, hydra.core.Type](subst))(typ0)(hydra.substitution.substInTypeNonEmpty(subst)(typ0))

def substInTypeNonEmpty(subst: hydra.typing.TypeSubst)(typ0: hydra.core.Type): hydra.core.Type =
  {
  def rewrite(recurse: (hydra.core.Type => hydra.core.Type))(typ: hydra.core.Type): hydra.core.Type =
    typ match
    case hydra.core.Type.forall(v_Type_forall_lt) => hydra.lib.maybes.maybe[hydra.core.Type,
       hydra.core.Type](recurse(typ))((styp: hydra.core.Type) =>
      hydra.core.Type.forall(hydra.core.ForallType(v_Type_forall_lt.parameter, hydra.substitution.substInType(removeVar(v_Type_forall_lt.parameter))(v_Type_forall_lt.body))))(hydra.lib.maps.lookup[hydra.core.Name,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Type](v_Type_forall_lt.parameter)(subst))
    case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.maybes.maybe[hydra.core.Type,
       hydra.core.Type](typ)((styp: hydra.core.Type) => styp)(hydra.lib.maps.lookup[hydra.core.Name,
       hydra.core.Type](v_Type_variable_v)(subst))
    case _ => recurse(typ)
  def removeVar(v: hydra.core.Name): hydra.typing.TypeSubst = hydra.lib.maps.delete[hydra.core.Name,
     hydra.core.Type](v)(subst)
  hydra.rewriting.rewriteType(rewrite)(typ0)
}

def substInTypeScheme(subst: hydra.typing.TypeSubst)(ts: hydra.core.TypeScheme): hydra.core.TypeScheme =
  hydra.core.TypeScheme(ts.variables, hydra.substitution.substInType(subst)(ts.`type`),
     hydra.lib.maybes.map[Map[hydra.core.Name, hydra.core.TypeVariableMetadata], Map[hydra.core.Name,
     hydra.core.TypeVariableMetadata]]((v1: Map[hydra.core.Name, hydra.core.TypeVariableMetadata]) => hydra.substitution.substInClassConstraints(subst)(v1))(ts.constraints))

def substTypesInTerm(subst: hydra.typing.TypeSubst)(term0: hydra.core.Term): hydra.core.Term =
  {
  def rewrite(recurse: (hydra.core.Term => hydra.core.Term))(term: hydra.core.Term): hydra.core.Term =
    {
    lazy val dflt: hydra.core.Term = recurse(term)
    def forLambda(l: hydra.core.Lambda): hydra.core.Term =
      hydra.core.Term.lambda(hydra.core.Lambda(l.parameter, hydra.lib.maybes.map[hydra.core.Type,
         hydra.core.Type]((v1: hydra.core.Type) => hydra.substitution.substInType(subst)(v1))(l.domain),
         hydra.substitution.substTypesInTerm(subst)(l.body)))
    def forLet(l: hydra.core.Let): hydra.core.Term =
      {
      def rewriteBinding(b: hydra.core.Binding): hydra.core.Binding =
        hydra.core.Binding(b.name, hydra.substitution.substTypesInTerm(subst)(b.term),
           hydra.lib.maybes.map[hydra.core.TypeScheme, hydra.core.TypeScheme]((v1: hydra.core.TypeScheme) => hydra.substitution.substInTypeScheme(subst)(v1))(b.`type`))
      hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding](rewriteBinding)(l.bindings),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.substitution.substTypesInTerm(subst)(l.body)))
    }
    def forTypeApplication(tt: hydra.core.TypeApplicationTerm): hydra.core.Term =
      hydra.core.Term.typeApplication(hydra.core.TypeApplicationTerm(hydra.substitution.substTypesInTerm(subst)(tt.body),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.substitution.substInType(subst)(tt.`type`)))
    def forTypeLambda(ta: hydra.core.TypeLambda): hydra.core.Term =
      {
      lazy val param: hydra.core.Name = (ta.parameter)
      lazy val subst2: hydra.typing.TypeSubst = hydra.lib.maps.delete[hydra.core.Name, hydra.core.Type](param)(subst)
      hydra.core.Term.typeLambda(hydra.core.TypeLambda(param, hydra.substitution.substTypesInTerm(subst2)(ta.body)))
    }
    term match
      case hydra.core.Term.lambda(v_Term_lambda_l) => forLambda(v_Term_lambda_l)
      case hydra.core.Term.let(v_Term_let_l) => forLet(v_Term_let_l)
      case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => forTypeApplication(v_Term_typeApplication_ta)
      case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => forTypeLambda(v_Term_typeLambda_tl)
      case _ => dflt
  }
  hydra.rewriting.rewriteTerm(rewrite)(term0)
}

def substituteInBinding(subst: hydra.typing.TermSubst)(b: hydra.core.Binding): hydra.core.Binding =
  hydra.core.Binding(b.name, hydra.substitution.substituteInTerm(subst)(b.term), (b.`type`))

def substituteInConstraint(subst: hydra.typing.TypeSubst)(c: hydra.typing.TypeConstraint): hydra.typing.TypeConstraint =
  hydra.typing.TypeConstraint(hydra.substitution.substInType(subst)(c.left), hydra.substitution.substInType(subst)(c.right),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     (c.comment))

def substituteInConstraints(subst: hydra.typing.TypeSubst)(cs: Seq[hydra.typing.TypeConstraint]): Seq[hydra.typing.TypeConstraint] =
  hydra.lib.lists.map[hydra.typing.TypeConstraint, hydra.typing.TypeConstraint]((v1: hydra.typing.TypeConstraint) => hydra.substitution.substituteInConstraint(subst)(v1))(cs)

def substituteInTerm(subst: hydra.typing.TermSubst)(term0: hydra.core.Term): hydra.core.Term =
  {
  lazy val s: Map[hydra.core.Name, hydra.core.Term] = subst
  def rewrite(recurse: (hydra.core.Term => hydra.core.Term))(term: hydra.core.Term): hydra.core.Term =
    {
    def withLambda(l: hydra.core.Lambda): hydra.core.Term =
      {
      lazy val v: hydra.core.Name = (l.parameter)
      lazy val subst2: hydra.typing.TermSubst = hydra.lib.maps.delete[hydra.core.Name, hydra.core.Term](v)(s)
      hydra.core.Term.lambda(hydra.core.Lambda(v, (l.domain), hydra.substitution.substituteInTerm(subst2)(l.body)))
    }
    def withLet(lt: hydra.core.Let): hydra.core.Term =
      {
      lazy val bindings: Seq[hydra.core.Binding] = (lt.bindings)
      lazy val names: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.map[hydra.core.Binding,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.core.Name]((x: hydra.core.Binding) => (x.name))(bindings))
      lazy val subst2: hydra.typing.TermSubst = hydra.lib.maps.filterWithKey[hydra.core.Name,
         hydra.core.Term]((k: hydra.core.Name) =>
        (v: hydra.core.Term) =>
        hydra.lib.logic.not(hydra.lib.sets.member[hydra.core.Name](k)(names)))(s)
      def rewriteBinding(b: hydra.core.Binding): hydra.core.Binding =
        hydra.core.Binding(b.name, hydra.substitution.substituteInTerm(subst2)(b.term), (b.`type`))
      hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding, hydra.core.Binding](rewriteBinding)(bindings),
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         hydra.substitution.substituteInTerm(subst2)(lt.body)))
    }
    term match
      case hydra.core.Term.lambda(v_Term_lambda_l) => withLambda(v_Term_lambda_l)
      case hydra.core.Term.let(v_Term_let_l) => withLet(v_Term_let_l)
      case hydra.core.Term.variable(v_Term_variable_name) => hydra.lib.maybes.maybe[hydra.core.Term,
         hydra.core.Term](recurse(term))((sterm: hydra.core.Term) => sterm)(hydra.lib.maps.lookup[hydra.core.Name,
         hydra.core.Term](v_Term_variable_name)(s))
      case _ => recurse(term)
  }
  hydra.rewriting.rewriteTerm(rewrite)(term0)
}
