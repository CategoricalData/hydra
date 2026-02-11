// Note: this is an automatically generated file. Do not edit.

package hydra.eval.lib.maybes;

/**
 * Evaluation-level implementations of Maybe functions for the Hydra interpreter.
 */
public interface Maybes {
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> apply(hydra.core.Term funOptTerm, hydra.core.Term argOptTerm) {
    return (funOptTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "optional function",
          hydra.show.core.Core.term(funOptTerm));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Maybe mf) {
        return (argOptTerm).accept(new hydra.core.Term.PartialVisitor<>() {
          @Override
          public hydra.compute.Flow<T0, hydra.core.Term> otherwise(hydra.core.Term instance) {
            return hydra.monads.Monads.unexpected(
              "optional value",
              hydra.show.core.Core.term(argOptTerm));
          }
          
          @Override
          public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Maybe mx) {
            return hydra.lib.flows.Pure.apply(new hydra.core.Term.Maybe(hydra.lib.maybes.Bind.apply(
              (mf).value,
              (java.util.function.Function<hydra.core.Term, hydra.util.Maybe<hydra.core.Term>>) (f -> hydra.lib.maybes.Map.apply(
                (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (x -> new hydra.core.Term.Application(new hydra.core.Application(f, x))),
                (mx).value)))));
          }
        });
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> bind(hydra.core.Term optTerm, hydra.core.Term funTerm) {
    return (optTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "optional value",
          hydra.show.core.Core.term(optTerm));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.flows.Pure.apply(hydra.lib.maybes.Maybe.apply(
          new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())),
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, val))),
          (m).value));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> cases(hydra.core.Term optTerm, hydra.core.Term defaultTerm, hydra.core.Term funTerm) {
    return (optTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "optional value",
          hydra.show.core.Core.term(optTerm));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.flows.Pure.apply(hydra.lib.maybes.Maybe.apply(
          defaultTerm,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, val))),
          (m).value));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> compose(hydra.core.Term funF, hydra.core.Term funG, hydra.core.Term xTerm) {
    return hydra.lib.flows.Pure.apply(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maybes.bind"))), new hydra.core.Term.Application(new hydra.core.Application(funF, xTerm)))), funG)));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> map(hydra.core.Term funTerm, hydra.core.Term optTerm) {
    return (optTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "optional value",
          hydra.show.core.Core.term(optTerm));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, val))),
          (m).value)));
      }
    });
  }
  
  static hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term> mapMaybe(hydra.core.Term funTerm, hydra.core.Term listTerm) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.list(listTerm),
      (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.core.Term>>) (elements -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Primitive(new hydra.core.Name("hydra.lib.maybes.cat"))), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (el -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, el))),
        elements)))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> maybe(hydra.core.Term defaultTerm, hydra.core.Term funTerm, hydra.core.Term optTerm) {
    return (optTerm).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "optional value",
          hydra.show.core.Core.term(optTerm));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.core.Term.Maybe m) {
        return hydra.lib.flows.Pure.apply(hydra.lib.maybes.Maybe.apply(
          defaultTerm,
          (java.util.function.Function<hydra.core.Term, hydra.core.Term>) (val -> new hydra.core.Term.Application(new hydra.core.Application(funTerm, val))),
          (m).value));
      }
    });
  }
}
