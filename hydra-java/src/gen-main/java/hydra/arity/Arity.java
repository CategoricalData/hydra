// Note: this is an automatically generated file. Do not edit.

package hydra.arity;

/**
 * Functions dealing with arguments and arity.
 */
public interface Arity {
  static Integer functionArity(hydra.core.Function v1) {
    return ((v1)).accept(new hydra.core.Function.Visitor<>() {
      @Override
      public Integer visit(hydra.core.Function.Elimination ignored) {
        return 1;
      }
      
      @Override
      public Integer visit(hydra.core.Function.Lambda arg_) {
        return ((java.util.function.Function<Integer, Integer>) (i -> hydra.lib.math.Add.apply(
          1,
          (i)))).apply(((java.util.function.Function<hydra.core.Lambda, Integer>) (arg_2 -> hydra.arity.Arity.termArity(((arg_2)).body))).apply(((arg_)).value));
      }
      
      @Override
      public Integer visit(hydra.core.Function.Primitive ignored) {
        return 42;
      }
    });
  }
  
  static Integer primitiveArity(hydra.graph.Primitive arg_) {
    return ((java.util.function.Function<hydra.core.TypeScheme, Integer>) (arg_2 -> hydra.arity.Arity.typeArity(((arg_2)).type))).apply(((arg_)).type);
  }
  
  static Integer termArity(hydra.core.Term v1) {
    return ((v1)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Term instance) {
        return 0;
      }
      
      @Override
      public Integer visit(hydra.core.Term.Application arg_) {
        return ((java.util.function.Function<hydra.core.Term, Integer>) (arg_2 -> ((java.util.function.Function<Integer, Integer>) (xapp -> hydra.lib.math.Sub.apply(
          (xapp),
          1))).apply(hydra.arity.Arity.termArity((arg_2))))).apply((((arg_)).value).function);
      }
      
      @Override
      public Integer visit(hydra.core.Term.Function v12) {
        return hydra.arity.Arity.functionArity(((v12)).value);
      }
    });
  }
  
  static Integer typeArity(hydra.core.Type v1) {
    return ((v1)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Type instance) {
        return 0;
      }
      
      @Override
      public Integer visit(hydra.core.Type.Annotated arg_) {
        return hydra.arity.Arity.typeArity((((arg_)).value).body);
      }
      
      @Override
      public Integer visit(hydra.core.Type.Application arg_) {
        return hydra.arity.Arity.typeArity((((arg_)).value).function);
      }
      
      @Override
      public Integer visit(hydra.core.Type.Forall arg_) {
        return hydra.arity.Arity.typeArity((((arg_)).value).body);
      }
      
      @Override
      public Integer visit(hydra.core.Type.Function f) {
        return hydra.lib.math.Add.apply(
          1,
          hydra.arity.Arity.typeArity((((f)).value).codomain));
      }
    });
  }
  
  static Integer typeSchemeArity(hydra.core.TypeScheme ts) {
    return hydra.arity.Arity.typeArity(((ts)).type);
  }
  
  static java.util.List<hydra.core.Type> uncurryType(hydra.core.Type t) {
    return ((t)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Type> otherwise(hydra.core.Type instance) {
        return java.util.List.of((t));
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Annotated arg_) {
        return hydra.arity.Arity.uncurryType((((arg_)).value).body);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Application arg_) {
        return hydra.arity.Arity.uncurryType((((arg_)).value).function);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Forall arg_) {
        return hydra.arity.Arity.uncurryType((((arg_)).value).body);
      }
      
      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Function ft) {
        return hydra.lib.lists.Cons.apply(
          (((ft)).value).domain,
          hydra.arity.Arity.uncurryType((((ft)).value).codomain));
      }
    });
  }
}
