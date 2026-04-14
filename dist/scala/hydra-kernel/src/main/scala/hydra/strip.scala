package hydra.strip

import hydra.core.*

def deannotateAndDetypeTerm(t: hydra.core.Term): hydra.core.Term =
  t match
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.strip.deannotateAndDetypeTerm(v_Term_annotated_at.body)
  case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.strip.deannotateAndDetypeTerm(v_Term_typeApplication_tt.body)
  case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => hydra.strip.deannotateAndDetypeTerm(v_Term_typeLambda_ta.body)
  case _ => t

def deannotateTerm(t: hydra.core.Term): hydra.core.Term =
  t match
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.strip.deannotateTerm(v_Term_annotated_at.body)
  case _ => t

def deannotateType(t: hydra.core.Type): hydra.core.Type =
  t match
  case hydra.core.Type.annotated(v_Type_annotated_arg_) => hydra.strip.deannotateType(`v_Type_annotated_arg_`.body)
  case _ => t

def deannotateTypeParameters(t: hydra.core.Type): hydra.core.Type =
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.forall(v_Type_forall_lt) => hydra.strip.deannotateTypeParameters(v_Type_forall_lt.body)
  case _ => t

def deannotateTypeRecursive(typ: hydra.core.Type): hydra.core.Type =
  {
  def strip[T0](recurse: (T0 => hydra.core.Type))(typ2: T0): hydra.core.Type =
    {
    lazy val rewritten: hydra.core.Type = recurse(typ2)
    rewritten match
      case hydra.core.Type.annotated(v_Type_annotated_at) => (v_Type_annotated_at.body)
      case _ => rewritten
  }
  hydra.rewriting.rewriteType(strip)(typ)
}

def deannotateTypeSchemeRecursive(ts: hydra.core.TypeScheme): hydra.core.TypeScheme =
  {
  lazy val vars: Seq[hydra.core.Name] = (ts.variables)
  lazy val typ: hydra.core.Type = (ts.`type`)
  lazy val constraints: Option[Map[hydra.core.Name, hydra.core.TypeVariableMetadata]] = (ts.constraints)
  hydra.core.TypeScheme(vars, hydra.strip.deannotateTypeRecursive(typ), constraints)
}

def detypeTerm(t: hydra.core.Term): hydra.core.Term =
  t match
  case hydra.core.Term.annotated(v_Term_annotated_at) => {
    lazy val subj: hydra.core.Term = (v_Term_annotated_at.body)
    {
      lazy val ann: Map[hydra.core.Name, hydra.core.Term] = (v_Term_annotated_at.annotation)
      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.strip.detypeTerm(subj), ann))
    }
  }
  case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => hydra.strip.deannotateAndDetypeTerm(v_Term_typeApplication_tt.body)
  case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => hydra.strip.deannotateAndDetypeTerm(v_Term_typeLambda_ta.body)
  case _ => t

def removeTermAnnotations(term: hydra.core.Term): hydra.core.Term =
  {
  def remove(recurse: (hydra.core.Term => hydra.core.Term))(term2: hydra.core.Term): hydra.core.Term =
    {
    lazy val rewritten: hydra.core.Term = recurse(term2)
    term2 match
      case hydra.core.Term.annotated(v_Term_annotated_at) => (v_Term_annotated_at.body)
      case _ => rewritten
  }
  hydra.rewriting.rewriteTerm(remove)(term)
}

def removeTypeAnnotations(typ: hydra.core.Type): hydra.core.Type =
  {
  def remove[T0](recurse: (T0 => hydra.core.Type))(typ2: T0): hydra.core.Type =
    {
    lazy val rewritten: hydra.core.Type = recurse(typ2)
    rewritten match
      case hydra.core.Type.annotated(v_Type_annotated_at) => (v_Type_annotated_at.body)
      case _ => rewritten
  }
  hydra.rewriting.rewriteType(remove)(typ)
}

def removeTypeAnnotationsFromTerm(term: hydra.core.Term): hydra.core.Term =
  {
  def strip[T0](recurse: (T0 => hydra.core.Term))(term2: T0): hydra.core.Term =
    {
    lazy val rewritten: hydra.core.Term = recurse(term2)
    def stripBinding(b: hydra.core.Binding): hydra.core.Binding = hydra.core.Binding(b.name, (b.term), None)
    rewritten match
      case hydra.core.Term.let(v_Term_let_lt) => hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding,
         hydra.core.Binding](stripBinding)(v_Term_let_lt.bindings), (v_Term_let_lt.body)))
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => (v_Term_typeApplication_tt.body)
      case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => (v_Term_typeLambda_ta.body)
      case _ => rewritten
  }
  hydra.rewriting.rewriteTerm(strip)(term)
}

def removeTypesFromTerm(term: hydra.core.Term): hydra.core.Term =
  {
  def strip[T0](recurse: (T0 => hydra.core.Term))(term2: T0): hydra.core.Term =
    {
    lazy val rewritten: hydra.core.Term = recurse(term2)
    def stripBinding(b: hydra.core.Binding): hydra.core.Binding = hydra.core.Binding(b.name, (b.term), None)
    rewritten match
      case hydra.core.Term.lambda(v_Term_lambda_l) => hydra.core.Term.lambda(hydra.core.Lambda(v_Term_lambda_l.parameter, None, (v_Term_lambda_l.body)))
      case hydra.core.Term.let(v_Term_let_lt) => hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding,
         hydra.core.Binding](stripBinding)(v_Term_let_lt.bindings), (v_Term_let_lt.body)))
      case hydra.core.Term.typeApplication(v_Term_typeApplication_tt) => (v_Term_typeApplication_tt.body)
      case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => (v_Term_typeLambda_ta.body)
      case _ => rewritten
  }
  hydra.rewriting.rewriteTerm(strip)(term)
}

def stripTypeLambdas(t: hydra.core.Term): hydra.core.Term =
  t match
  case hydra.core.Term.annotated(v_Term_annotated_at) => {
    lazy val subj: hydra.core.Term = (v_Term_annotated_at.body)
    {
      lazy val ann: Map[hydra.core.Name, hydra.core.Term] = (v_Term_annotated_at.annotation)
      hydra.core.Term.annotated(hydra.core.AnnotatedTerm(hydra.strip.stripTypeLambdas(subj), ann))
    }
  }
  case hydra.core.Term.typeLambda(v_Term_typeLambda_ta) => hydra.strip.stripTypeLambdas(v_Term_typeLambda_ta.body)
  case _ => t
