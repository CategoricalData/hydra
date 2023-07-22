package hydra.graph;

/**
 * A typeclass-like construct providing common functions for working with annotations
 */
public class AnnotationClass<A> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/graph.AnnotationClass");
  
  public final A default_;
  
  public final java.util.function.Function<A, java.util.function.Function<A, Boolean>> equal;
  
  public final java.util.function.Function<A, java.util.function.Function<A, hydra.graph.Comparison>> compare;
  
  public final java.util.function.Function<A, String> show;
  
  public final java.util.function.Function<String, java.util.Optional<A>> read;
  
  public final java.util.function.Function<hydra.core.Term<A>, A> termAnnotation;
  
  public final java.util.function.Function<hydra.core.Type<A>, A> typeAnnotation;
  
  public final java.util.function.Function<hydra.core.Term<A>, hydra.compute.Flow<hydra.graph.Graph<A>, java.util.Optional<String>>> termDescription;
  
  public final java.util.function.Function<hydra.core.Type<A>, hydra.compute.Flow<hydra.graph.Graph<A>, java.util.Optional<String>>> typeDescription;
  
  public final java.util.function.Function<hydra.core.Term<A>, hydra.compute.Flow<hydra.graph.Graph<A>, java.util.Optional<hydra.core.Type<A>>>> termType;
  
  public final java.util.function.Function<java.util.Optional<String>, java.util.function.Function<hydra.core.Term<A>, hydra.core.Term<A>>> setTermDescription;
  
  public final java.util.function.Function<java.util.Optional<hydra.core.Type<A>>, java.util.function.Function<hydra.core.Term<A>, hydra.core.Term<A>>> setTermType;
  
  public final java.util.function.Function<A, hydra.compute.Flow<hydra.graph.Graph<A>, java.util.Optional<hydra.core.Type<A>>>> typeOf;
  
  public final java.util.function.Function<java.util.Optional<hydra.core.Type<A>>, java.util.function.Function<A, A>> setTypeOf;
  
  public AnnotationClass (A default_, java.util.function.Function<A, java.util.function.Function<A, Boolean>> equal, java.util.function.Function<A, java.util.function.Function<A, hydra.graph.Comparison>> compare, java.util.function.Function<A, String> show, java.util.function.Function<String, java.util.Optional<A>> read, java.util.function.Function<hydra.core.Term<A>, A> termAnnotation, java.util.function.Function<hydra.core.Type<A>, A> typeAnnotation, java.util.function.Function<hydra.core.Term<A>, hydra.compute.Flow<hydra.graph.Graph<A>, java.util.Optional<String>>> termDescription, java.util.function.Function<hydra.core.Type<A>, hydra.compute.Flow<hydra.graph.Graph<A>, java.util.Optional<String>>> typeDescription, java.util.function.Function<hydra.core.Term<A>, hydra.compute.Flow<hydra.graph.Graph<A>, java.util.Optional<hydra.core.Type<A>>>> termType, java.util.function.Function<java.util.Optional<String>, java.util.function.Function<hydra.core.Term<A>, hydra.core.Term<A>>> setTermDescription, java.util.function.Function<java.util.Optional<hydra.core.Type<A>>, java.util.function.Function<hydra.core.Term<A>, hydra.core.Term<A>>> setTermType, java.util.function.Function<A, hydra.compute.Flow<hydra.graph.Graph<A>, java.util.Optional<hydra.core.Type<A>>>> typeOf, java.util.function.Function<java.util.Optional<hydra.core.Type<A>>, java.util.function.Function<A, A>> setTypeOf) {
    this.default_ = default_;
    this.equal = equal;
    this.compare = compare;
    this.show = show;
    this.read = read;
    this.termAnnotation = termAnnotation;
    this.typeAnnotation = typeAnnotation;
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
    return default_.equals(o.default_) && equal.equals(o.equal) && compare.equals(o.compare) && show.equals(o.show) && read.equals(o.read) && termAnnotation.equals(o.termAnnotation) && typeAnnotation.equals(o.typeAnnotation) && termDescription.equals(o.termDescription) && typeDescription.equals(o.typeDescription) && termType.equals(o.termType) && setTermDescription.equals(o.setTermDescription) && setTermType.equals(o.setTermType) && typeOf.equals(o.typeOf) && setTypeOf.equals(o.setTypeOf);
  }
  
  @Override
  public int hashCode() {
    return 2 * default_.hashCode() + 3 * equal.hashCode() + 5 * compare.hashCode() + 7 * show.hashCode() + 11 * read.hashCode() + 13 * termAnnotation.hashCode() + 17 * typeAnnotation.hashCode() + 19 * termDescription.hashCode() + 23 * typeDescription.hashCode() + 29 * termType.hashCode() + 31 * setTermDescription.hashCode() + 37 * setTermType.hashCode() + 41 * typeOf.hashCode() + 43 * setTypeOf.hashCode();
  }
  
  public AnnotationClass withDefault(A default_) {
    return new AnnotationClass(default_, equal, compare, show, read, termAnnotation, typeAnnotation, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withEqual(java.util.function.Function<A, java.util.function.Function<A, Boolean>> equal) {
    return new AnnotationClass(default_, equal, compare, show, read, termAnnotation, typeAnnotation, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withCompare(java.util.function.Function<A, java.util.function.Function<A, hydra.graph.Comparison>> compare) {
    return new AnnotationClass(default_, equal, compare, show, read, termAnnotation, typeAnnotation, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withShow(java.util.function.Function<A, String> show) {
    return new AnnotationClass(default_, equal, compare, show, read, termAnnotation, typeAnnotation, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withRead(java.util.function.Function<String, java.util.Optional<A>> read) {
    return new AnnotationClass(default_, equal, compare, show, read, termAnnotation, typeAnnotation, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withTermAnnotation(java.util.function.Function<hydra.core.Term<A>, A> termAnnotation) {
    return new AnnotationClass(default_, equal, compare, show, read, termAnnotation, typeAnnotation, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withTypeAnnotation(java.util.function.Function<hydra.core.Type<A>, A> typeAnnotation) {
    return new AnnotationClass(default_, equal, compare, show, read, termAnnotation, typeAnnotation, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withTermDescription(java.util.function.Function<hydra.core.Term<A>, hydra.compute.Flow<hydra.graph.Graph<A>, java.util.Optional<String>>> termDescription) {
    return new AnnotationClass(default_, equal, compare, show, read, termAnnotation, typeAnnotation, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withTypeDescription(java.util.function.Function<hydra.core.Type<A>, hydra.compute.Flow<hydra.graph.Graph<A>, java.util.Optional<String>>> typeDescription) {
    return new AnnotationClass(default_, equal, compare, show, read, termAnnotation, typeAnnotation, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withTermType(java.util.function.Function<hydra.core.Term<A>, hydra.compute.Flow<hydra.graph.Graph<A>, java.util.Optional<hydra.core.Type<A>>>> termType) {
    return new AnnotationClass(default_, equal, compare, show, read, termAnnotation, typeAnnotation, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withSetTermDescription(java.util.function.Function<java.util.Optional<String>, java.util.function.Function<hydra.core.Term<A>, hydra.core.Term<A>>> setTermDescription) {
    return new AnnotationClass(default_, equal, compare, show, read, termAnnotation, typeAnnotation, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withSetTermType(java.util.function.Function<java.util.Optional<hydra.core.Type<A>>, java.util.function.Function<hydra.core.Term<A>, hydra.core.Term<A>>> setTermType) {
    return new AnnotationClass(default_, equal, compare, show, read, termAnnotation, typeAnnotation, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withTypeOf(java.util.function.Function<A, hydra.compute.Flow<hydra.graph.Graph<A>, java.util.Optional<hydra.core.Type<A>>>> typeOf) {
    return new AnnotationClass(default_, equal, compare, show, read, termAnnotation, typeAnnotation, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
  
  public AnnotationClass withSetTypeOf(java.util.function.Function<java.util.Optional<hydra.core.Type<A>>, java.util.function.Function<A, A>> setTypeOf) {
    return new AnnotationClass(default_, equal, compare, show, read, termAnnotation, typeAnnotation, termDescription, typeDescription, termType, setTermDescription, setTermType, typeOf, setTypeOf);
  }
}