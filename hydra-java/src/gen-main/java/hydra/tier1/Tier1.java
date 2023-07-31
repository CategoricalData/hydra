package hydra.tier1;

/**
 * A module for all tier-1 functions and constants. These are generated functions and constants which DSL functions and the implementations of primitive functions are allowed to depend upon. Higher tiers of generated code may not be depended upon, as these tiers may themselves need to depend on DSL functions or primitive functions.
 */
public interface Tier1 {
  String ignoredVariable = "_";
  
  hydra.core.Name placeholderName = new hydra.core.Name("Placeholder");
  
  static <A, X> java.util.function.Function<X, X> skipAnnotations(java.util.function.Function<X, java.util.Optional<hydra.core.Annotated<X, A>>> getAnn) {
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<X, X>> skip = new java.util.concurrent.atomic.AtomicReference<>();
    skip.set((java.util.function.Function<X, X>) (t1 -> ((((getAnn)).apply((t1))).map((java.util.function.Function<hydra.core.Annotated<X, A>, X>) (ann -> (skip.get()).apply(((ann)).subject)))).orElse((t1))));
    return (java.util.function.Function<X, X>) (t -> (skip.get()).apply((t)));
  }
  
  static <A> hydra.core.Term<A> stripTerm(hydra.core.Term<A> x) {
    return (hydra.tier1.Tier1.skipAnnotations((java.util.function.Function<hydra.core.Term<A>, java.util.Optional<hydra.core.Annotated<hydra.core.Term<A>, A>>>) (v1 -> ((v1)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public java.util.Optional<hydra.core.Annotated<hydra.core.Term<A>, A>> otherwise(hydra.core.Term<A> instance) {
        return java.util.Optional.empty();
      }
      
      @Override
      public java.util.Optional<hydra.core.Annotated<hydra.core.Term<A>, A>> visit(hydra.core.Term.Annotated<A> instance) {
        return java.util.Optional.of((instance.value));
      }
    })))).apply((x));
  }
  
  static <A> hydra.core.Type<A> stripType(hydra.core.Type<A> x) {
    return (hydra.tier1.Tier1.skipAnnotations((java.util.function.Function<hydra.core.Type<A>, java.util.Optional<hydra.core.Annotated<hydra.core.Type<A>, A>>>) (v1 -> ((v1)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.Optional<hydra.core.Annotated<hydra.core.Type<A>, A>> otherwise(hydra.core.Type<A> instance) {
        return java.util.Optional.empty();
      }
      
      @Override
      public java.util.Optional<hydra.core.Annotated<hydra.core.Type<A>, A>> visit(hydra.core.Type.Annotated<A> instance) {
        return java.util.Optional.of((instance.value));
      }
    })))).apply((x));
  }
  
  static hydra.core.Name unqualifyName(hydra.module.QualifiedName qname) {
    String prefix = ((((qname)).namespace).map((java.util.function.Function<hydra.module.Namespace, String>) (n -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      ((n)).value,
      "."))))).orElse("");
    return new hydra.core.Name(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      (prefix),
      ((qname)).local)));
  }
}