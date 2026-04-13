// Note: this is an automatically generated file. Do not edit.

package hydra;

/**
 * Functions dealing with arguments and arity.
 */
public interface Arity {
  static Integer primitiveArity(hydra.graph.Primitive arg_) {
    return hydra.Arity.typeArity((arg_).type.type);
  }

  static Integer termArity(hydra.core.Term v1) {
    return (v1).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Term instance) {
        return 0;
      }

      @Override
      public Integer visit(hydra.core.Term.Application arg_) {
        return hydra.lib.math.Sub.apply(
          hydra.Arity.termArity((arg_).value.function),
          1);
      }

      @Override
      public Integer visit(hydra.core.Term.Cases ignored) {
        return 1;
      }

      @Override
      public Integer visit(hydra.core.Term.Lambda arg_) {
        return hydra.lib.math.Add.apply(
          1,
          hydra.Arity.termArity((arg_).value.body));
      }

      @Override
      public Integer visit(hydra.core.Term.Project ignored) {
        return 1;
      }

      @Override
      public Integer visit(hydra.core.Term.Unwrap ignored) {
        return 1;
      }
    });
  }

  static Integer typeArity(hydra.core.Type v1) {
    return (v1).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Integer otherwise(hydra.core.Type instance) {
        return 0;
      }

      @Override
      public Integer visit(hydra.core.Type.Annotated arg_) {
        return hydra.Arity.typeArity((arg_).value.body);
      }

      @Override
      public Integer visit(hydra.core.Type.Application arg_) {
        return hydra.Arity.typeArity((arg_).value.function);
      }

      @Override
      public Integer visit(hydra.core.Type.Forall arg_) {
        return hydra.Arity.typeArity((arg_).value.body);
      }

      @Override
      public Integer visit(hydra.core.Type.Function f) {
        return hydra.lib.math.Add.apply(
          1,
          hydra.Arity.typeArity((f).value.codomain));
      }
    });
  }

  static Integer typeSchemeArity(hydra.core.TypeScheme arg_) {
    return hydra.Arity.typeArity((arg_).type);
  }

  static java.util.List<hydra.core.Type> uncurryType(hydra.core.Type t) {
    return (t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.Type> otherwise(hydra.core.Type instance) {
        return java.util.Arrays.asList(t);
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Annotated arg_) {
        return hydra.Arity.uncurryType((arg_).value.body);
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Application arg_) {
        return hydra.Arity.uncurryType((arg_).value.function);
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Forall arg_) {
        return hydra.Arity.uncurryType((arg_).value.body);
      }

      @Override
      public java.util.List<hydra.core.Type> visit(hydra.core.Type.Function ft) {
        return hydra.lib.lists.Cons.apply(
          (ft).value.domain,
          hydra.Arity.uncurryType((ft).value.codomain));
      }
    });
  }
}
