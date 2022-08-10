package hydra.evaluation;

/**
 * A typeclass-like construct providing common functions for working with annotations
 */
public class AnnotationClass<M> {
  public final M default_;
  
  public final java.util.function.Function<M, java.util.function.Function<M, Boolean>> equal;
  
  public final java.util.function.Function<M, java.util.function.Function<M, hydra.core.Comparison>> compare;
  
  public final java.util.function.Function<M, String> show;
  
  public final java.util.function.Function<String, java.util.Optional<M>> read;
  
  public final java.util.function.Function<hydra.core.Term<M>, hydra.core.Term<M>> termExpr;
  
  public final java.util.function.Function<hydra.core.Term<M>, M> termMeta;
  
  public final java.util.function.Function<hydra.core.Type<M>, hydra.core.Type<M>> typeExpr;
  
  public final java.util.function.Function<hydra.core.Type<M>, M> typeMeta;
  
  public final java.util.function.Function<Context<M>, java.util.function.Function<hydra.core.Term<M>, Result<java.util.Optional<String>>>> termDescription;
  
  public final java.util.function.Function<Context<M>, java.util.function.Function<hydra.core.Type<M>, Result<java.util.Optional<String>>>> typeDescription;
  
  public final java.util.function.Function<Context<M>, java.util.function.Function<hydra.core.Term<M>, Result<java.util.Optional<hydra.core.Type<M>>>>> termType;
  
  public final java.util.function.Function<Context<M>, java.util.function.Function<java.util.Optional<String>, java.util.function.Function<hydra.core.Term<M>, hydra.core.Term<M>>>> setTermDescription;
  
  public final java.util.function.Function<Context<M>, java.util.function.Function<java.util.Optional<hydra.core.Type<M>>, java.util.function.Function<hydra.core.Term<M>, hydra.core.Term<M>>>> setTermType;
  
  public final java.util.function.Function<Context<M>, java.util.function.Function<M, Result<java.util.Optional<hydra.core.Type<M>>>>> typeOf;
  
  public final java.util.function.Function<Context<M>, java.util.function.Function<java.util.Optional<hydra.core.Type<M>>, java.util.function.Function<M, M>>> setTypeOf;
  
  public AnnotationClass (M default_, java.util.function.Function<M, java.util.function.Function<M, Boolean>> equal, java.util.function.Function<M, java.util.function.Function<M, hydra.core.Comparison>> compare, java.util.function.Function<M, String> show, java.util.function.Function<String, java.util.Optional<M>> read, java.util.function.Function<hydra.core.Term<M>, hydra.core.Term<M>> termExpr, java.util.function.Function<hydra.core.Term<M>, M> termMeta, java.util.function.Function<hydra.core.Type<M>, hydra.core.Type<M>> typeExpr, java.util.function.Function<hydra.core.Type<M>, M> typeMeta, java.util.function.Function<Context<M>, java.util.function.Function<hydra.core.Term<M>, Result<java.util.Optional<String>>>> termDescription, java.util.function.Function<Context<M>, java.util.function.Function<hydra.core.Type<M>, Result<java.util.Optional<String>>>> typeDescription, java.util.function.Function<Context<M>, java.util.function.Function<hydra.core.Term<M>, Result<java.util.Optional<hydra.core.Type<M>>>>> termType, java.util.function.Function<Context<M>, java.util.function.Function<java.util.Optional<String>, java.util.function.Function<hydra.core.Term<M>, hydra.core.Term<M>>>> setTermDescription, java.util.function.Function<Context<M>, java.util.function.Function<java.util.Optional<hydra.core.Type<M>>, java.util.function.Function<hydra.core.Term<M>, hydra.core.Term<M>>>> setTermType, java.util.function.Function<Context<M>, java.util.function.Function<M, Result<java.util.Optional<hydra.core.Type<M>>>>> typeOf, java.util.function.Function<Context<M>, java.util.function.Function<java.util.Optional<hydra.core.Type<M>>, java.util.function.Function<M, M>>> setTypeOf) {
    this.default_ = default_;
    this.equal = equal;
    this.compare = compare;
    this.show = show;
    this.read = read;
    this.termExpr = termExpr;
    this.termMeta = termMeta;
    this.typeExpr = typeExpr;
    this.typeMeta = typeMeta;
    this.termDescription = termDescription;
    this.typeDescription = typeDescription;
    this.termType = termType;
    this.setTermDescription = setTermDescription;
    this.setTermType = setTermType;
    this.typeOf = typeOf;
    this.setTypeOf = setTypeOf;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotationClass)) {
      return false;
    }
    AnnotationClass o = (AnnotationClass) (other);
    return default_.equals(o.default_) && equal.equals(o.equal) && compare.equals(o.compare) && show.equals(o.show) && read.equals(o.read) && termExpr.equals(o.termExpr) && termMeta.equals(o.termMeta) && typeExpr.equals(o.typeExpr) && typeMeta.equals(o.typeMeta) && termDescription.equals(o.termDescription) && typeDescription.equals(o.typeDescription) && termType.equals(o.termType) && setTermDescription.equals(o.setTermDescription) && setTermType.equals(o.setTermType) && typeOf.equals(o.typeOf) && setTypeOf.equals(o.setTypeOf);
  }
  
  @Override
  public int hashCode() {
    return 2 * default_.hashCode() + 3 * equal.hashCode() + 5 * compare.hashCode() + 7 * show.hashCode() + 11 * read.hashCode() + 13 * termExpr.hashCode() + 17 * termMeta.hashCode() + 19 * typeExpr.hashCode() + 23 * typeMeta.hashCode() + 29 * termDescription.hashCode() + 31 * typeDescription.hashCode() + 37 * termType.hashCode() + 41 * setTermDescription.hashCode() + 43 * setTermType.hashCode() + 47 * typeOf.hashCode() + 53 * setTypeOf.hashCode();
  }
  
  public AnnotationClass withDefault(M default_) {
    return new AnnotationClass(default_, equal, compare, show, read, termExpr, termMeta, typeExpr, typeMeta, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withEqual(java.util.function.Function<M, java.util.function.Function<M, Boolean>> equal) {
    return new AnnotationClass(default_, equal, compare, show, read, termExpr, termMeta, typeExpr, typeMeta, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withCompare(java.util.function.Function<M, java.util.function.Function<M, hydra.core.Comparison>> compare) {
    return new AnnotationClass(default_, equal, compare, show, read, termExpr, termMeta, typeExpr, typeMeta, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withShow(java.util.function.Function<M, String> show) {
    return new AnnotationClass(default_, equal, compare, show, read, termExpr, termMeta, typeExpr, typeMeta, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withRead(java.util.function.Function<String, java.util.Optional<M>> read) {
    return new AnnotationClass(default_, equal, compare, show, read, termExpr, termMeta, typeExpr, typeMeta, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withTermExpr(java.util.function.Function<hydra.core.Term<M>, hydra.core.Term<M>> termExpr) {
    return new AnnotationClass(default_, equal, compare, show, read, termExpr, termMeta, typeExpr, typeMeta, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withTermMeta(java.util.function.Function<hydra.core.Term<M>, M> termMeta) {
    return new AnnotationClass(default_, equal, compare, show, read, termExpr, termMeta, typeExpr, typeMeta, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withTypeExpr(java.util.function.Function<hydra.core.Type<M>, hydra.core.Type<M>> typeExpr) {
    return new AnnotationClass(default_, equal, compare, show, read, termExpr, termMeta, typeExpr, typeMeta, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withTypeMeta(java.util.function.Function<hydra.core.Type<M>, M> typeMeta) {
    return new AnnotationClass(default_, equal, compare, show, read, termExpr, termMeta, typeExpr, typeMeta, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withTermDescription(java.util.function.Function<Context<M>, java.util.function.Function<hydra.core.Term<M>, Result<java.util.Optional<String>>>> termDescription) {
    return new AnnotationClass(default_, equal, compare, show, read, termExpr, termMeta, typeExpr, typeMeta, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withTypeDescription(java.util.function.Function<Context<M>, java.util.function.Function<hydra.core.Type<M>, Result<java.util.Optional<String>>>> typeDescription) {
    return new AnnotationClass(default_, equal, compare, show, read, termExpr, termMeta, typeExpr, typeMeta, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withTermType(java.util.function.Function<Context<M>, java.util.function.Function<hydra.core.Term<M>, Result<java.util.Optional<hydra.core.Type<M>>>>> termType) {
    return new AnnotationClass(default_, equal, compare, show, read, termExpr, termMeta, typeExpr, typeMeta, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withSetTermDescription(java.util.function.Function<Context<M>, java.util.function.Function<java.util.Optional<String>, java.util.function.Function<hydra.core.Term<M>, hydra.core.Term<M>>>> setTermDescription) {
    return new AnnotationClass(default_, equal, compare, show, read, termExpr, termMeta, typeExpr, typeMeta, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withSetTermType(java.util.function.Function<Context<M>, java.util.function.Function<java.util.Optional<hydra.core.Type<M>>, java.util.function.Function<hydra.core.Term<M>, hydra.core.Term<M>>>> setTermType) {
    return new AnnotationClass(default_, equal, compare, show, read, termExpr, termMeta, typeExpr, typeMeta, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withTypeOf(java.util.function.Function<Context<M>, java.util.function.Function<M, Result<java.util.Optional<hydra.core.Type<M>>>>> typeOf) {
    return new AnnotationClass(default_, equal, compare, show, read, termExpr, termMeta, typeExpr, typeMeta, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withSetTypeOf(java.util.function.Function<Context<M>, java.util.function.Function<java.util.Optional<hydra.core.Type<M>>, java.util.function.Function<M, M>>> setTypeOf) {
    return new AnnotationClass(default_, equal, compare, show, read, termExpr, termMeta, typeExpr, typeMeta, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
}