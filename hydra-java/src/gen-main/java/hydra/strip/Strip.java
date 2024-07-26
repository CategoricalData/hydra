// Note: this is an automatically generated file. Do not edit.

package hydra.strip;

import hydra.core.AnnotatedTerm;
import hydra.core.AnnotatedType;
import hydra.core.Term;
import hydra.core.Type;
import hydra.util.Opt;

/**
 * Several functions for stripping annotations from types and terms.
 */
public interface Strip {
  static java.util.function.Function<Term, Term> skipTermAnnotations(java.util.function.Function<Term, Opt<AnnotatedTerm>> getAnn) {
    return (java.util.function.Function<Term, Term>) (t -> {
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Term, Term>> skip = new java.util.concurrent.atomic.AtomicReference<>();
      skip.set((java.util.function.Function<Term, Term>) (t1 -> ((((getAnn)).apply((t1))).map((java.util.function.Function<hydra.core.AnnotatedTerm, Term>) (ann -> (skip.get()).apply(((ann)).subject)))).orElse((t1))));
      return ((skip)).get().apply((t));
    });
  }

  static java.util.function.Function<Type, Type> skipTypeAnnotations(java.util.function.Function<Type, Opt<AnnotatedType>> getAnn) {
    return (java.util.function.Function<Type, Type>) (t -> {
      java.util.concurrent.atomic.AtomicReference<java.util.function.Function<Type, Type>> skip = new java.util.concurrent.atomic.AtomicReference<>();
      skip.set((java.util.function.Function<Type, Type>) (t1 -> ((((getAnn)).apply((t1))).map((java.util.function.Function<hydra.core.AnnotatedType, Type>) (ann -> (skip.get()).apply(((ann)).subject)))).orElse((t1))));
      return ((skip)).get().apply((t));
    });
  }
  
  static  hydra.core.Term stripTerm(hydra.core.Term x) {
    return (hydra.strip.Strip.skipTermAnnotations((java.util.function.Function<hydra.core.Term, Opt<AnnotatedTerm>>) (v1 -> ((v1)).accept(new hydra.core.Term.PartialVisitor<Opt<AnnotatedTerm>>() {
      @Override
      public Opt<AnnotatedTerm> otherwise(hydra.core.Term instance) {
        return Opt.empty();
      }
      
      @Override
      public Opt<AnnotatedTerm> visit(hydra.core.Term.Annotated instance) {
        return Opt.of((instance.value));
      }
    })))).apply((x));
  }
  
  static  hydra.core.Type stripType(hydra.core.Type x) {
    return (hydra.strip.Strip.skipTypeAnnotations((java.util.function.Function<hydra.core.Type, Opt<AnnotatedType>>) (v1 -> ((v1)).accept(new hydra.core.Type.PartialVisitor<Opt<AnnotatedType>>() {
      @Override
      public Opt<AnnotatedType> otherwise(hydra.core.Type instance) {
        return Opt.empty();
      }
      
      @Override
      public Opt<AnnotatedType> visit(hydra.core.Type.Annotated instance) {
        return Opt.of((instance.value));
      }
    })))).apply((x));
  }
  
  static  hydra.core.Type stripTypeParameters(hydra.core.Type t) {
    return (hydra.strip.Strip.stripType((t))).accept(new hydra.core.Type.PartialVisitor<hydra.core.Type>() {
      @Override
      public hydra.core.Type otherwise(hydra.core.Type instance) {
        return (t);
      }
      
      @Override
      public hydra.core.Type visit(hydra.core.Type.Lambda instance) {
        return hydra.strip.Strip.stripTypeParameters(((instance.value)).body);
      }
    });
  }
}
