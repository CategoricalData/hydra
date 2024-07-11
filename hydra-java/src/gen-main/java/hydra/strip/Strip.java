// Note: this is an automatically generated file. Do not edit.

package hydra.strip;

import hydra.core.Annotated;
import hydra.core.Term;
import hydra.core.Type;
import hydra.util.Opt;

/**
 * Several functions for stripping annotations from types and terms.
 */
public interface Strip {
  static <A, X> java.util.function.Function<X, X> skipAnnotations(java.util.function.Function<X, Opt<Annotated<X, A>>> getAnn) {
    return (java.util.function.Function<X, X>) (t -> {
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<X, X>> skip = new java.util.concurrent.atomic.AtomicReference<>();
      skip.set((java.util.function.Function<X, X>) (t1 -> ((((getAnn)).apply((t1))).map((java.util.function.Function<hydra.core.Annotated<X, A>, X>) (ann -> (skip.get()).apply(((ann)).subject)))).orElse((t1))));
      return ((skip)).get().apply((t));
    });
  }
  
  static <A> hydra.core.Term<A> stripTerm(hydra.core.Term<A> x) {
    return (hydra.strip.Strip.skipAnnotations((java.util.function.Function<hydra.core.Term<A>, Opt<Annotated<Term<A>, A>>>) (v1 -> ((v1)).accept(new hydra.core.Term.PartialVisitor<A, Opt<Annotated<Term<A>, A>>>() {
      @Override
      public Opt<Annotated<Term<A>, A>> otherwise(hydra.core.Term<A> instance) {
        return Opt.empty();
      }
      
      @Override
      public Opt<Annotated<Term<A>, A>> visit(hydra.core.Term.Annotated<A> instance) {
        return Opt.of((instance.value));
      }
    })))).apply((x));
  }
  
  static <A> hydra.core.Type<A> stripType(hydra.core.Type<A> x) {
    return (hydra.strip.Strip.skipAnnotations((java.util.function.Function<hydra.core.Type<A>, Opt<Annotated<Type<A>, A>>>) (v1 -> ((v1)).accept(new hydra.core.Type.PartialVisitor<A, Opt<Annotated<Type<A>, A>>>() {
      @Override
      public Opt<Annotated<Type<A>, A>> otherwise(hydra.core.Type<A> instance) {
        return Opt.empty();
      }
      
      @Override
      public Opt<Annotated<Type<A>, A>> visit(hydra.core.Type.Annotated<A> instance) {
        return Opt.of((instance.value));
      }
    })))).apply((x));
  }
  
  static <A> hydra.core.Type<A> stripTypeParameters(hydra.core.Type<A> t) {
    return (hydra.strip.Strip.stripType((t))).accept(new hydra.core.Type.PartialVisitor<A, hydra.core.Type<A>>() {
      @Override
      public hydra.core.Type<A> otherwise(hydra.core.Type<A> instance) {
        return (t);
      }
      
      @Override
      public hydra.core.Type<A> visit(hydra.core.Type.Lambda<A> instance) {
        return hydra.strip.Strip.stripTypeParameters(((instance.value)).body);
      }
    });
  }
}
