// Note: this is an automatically generated file. Do not edit.

package hydra.show;

/**
 * String representations of hydra.variants types
 */
public interface Variants {
  static String termVariant(hydra.variants.TermVariant v1) {
    return (v1).accept(new hydra.variants.TermVariant.PartialVisitor<>() {
      @Override
      public String visit(hydra.variants.TermVariant.Annotated ignored) {
        return "annotated";
      }

      @Override
      public String visit(hydra.variants.TermVariant.Application ignored) {
        return "application";
      }

      @Override
      public String visit(hydra.variants.TermVariant.Cases ignored) {
        return "cases";
      }

      @Override
      public String visit(hydra.variants.TermVariant.Either ignored) {
        return "either";
      }

      @Override
      public String visit(hydra.variants.TermVariant.Lambda ignored) {
        return "lambda";
      }

      @Override
      public String visit(hydra.variants.TermVariant.Let ignored) {
        return "let";
      }

      @Override
      public String visit(hydra.variants.TermVariant.List ignored) {
        return "list";
      }

      @Override
      public String visit(hydra.variants.TermVariant.Literal ignored) {
        return "literal";
      }

      @Override
      public String visit(hydra.variants.TermVariant.Map ignored) {
        return "map";
      }

      @Override
      public String visit(hydra.variants.TermVariant.Maybe ignored) {
        return "maybe";
      }

      @Override
      public String visit(hydra.variants.TermVariant.Pair ignored) {
        return "pair";
      }

      @Override
      public String visit(hydra.variants.TermVariant.Project ignored) {
        return "project";
      }

      @Override
      public String visit(hydra.variants.TermVariant.Record ignored) {
        return "record";
      }

      @Override
      public String visit(hydra.variants.TermVariant.Set ignored) {
        return "set";
      }

      @Override
      public String visit(hydra.variants.TermVariant.TypeLambda ignored) {
        return "typeLambda";
      }

      @Override
      public String visit(hydra.variants.TermVariant.TypeApplication ignored) {
        return "typeApplication";
      }

      @Override
      public String visit(hydra.variants.TermVariant.Inject ignored) {
        return "inject";
      }

      @Override
      public String visit(hydra.variants.TermVariant.Unit ignored) {
        return "unit";
      }

      @Override
      public String visit(hydra.variants.TermVariant.Unwrap ignored) {
        return "unwrap";
      }

      @Override
      public String visit(hydra.variants.TermVariant.Variable ignored) {
        return "variable";
      }

      @Override
      public String visit(hydra.variants.TermVariant.Wrap ignored) {
        return "wrap";
      }
    });
  }

  static String typeVariant(hydra.variants.TypeVariant v1) {
    return (v1).accept(new hydra.variants.TypeVariant.PartialVisitor<>() {
      @Override
      public String visit(hydra.variants.TypeVariant.Annotated ignored) {
        return "annotated";
      }

      @Override
      public String visit(hydra.variants.TypeVariant.Application ignored) {
        return "application";
      }

      @Override
      public String visit(hydra.variants.TypeVariant.Either ignored) {
        return "either";
      }

      @Override
      public String visit(hydra.variants.TypeVariant.Forall ignored) {
        return "forall";
      }

      @Override
      public String visit(hydra.variants.TypeVariant.Function ignored) {
        return "function";
      }

      @Override
      public String visit(hydra.variants.TypeVariant.List ignored) {
        return "list";
      }

      @Override
      public String visit(hydra.variants.TypeVariant.Literal ignored) {
        return "literal";
      }

      @Override
      public String visit(hydra.variants.TypeVariant.Map ignored) {
        return "map";
      }

      @Override
      public String visit(hydra.variants.TypeVariant.Maybe ignored) {
        return "maybe";
      }

      @Override
      public String visit(hydra.variants.TypeVariant.Pair ignored) {
        return "pair";
      }

      @Override
      public String visit(hydra.variants.TypeVariant.Record ignored) {
        return "record";
      }

      @Override
      public String visit(hydra.variants.TypeVariant.Set ignored) {
        return "set";
      }

      @Override
      public String visit(hydra.variants.TypeVariant.Union ignored) {
        return "union";
      }

      @Override
      public String visit(hydra.variants.TypeVariant.Unit ignored) {
        return "unit";
      }

      @Override
      public String visit(hydra.variants.TypeVariant.Variable ignored) {
        return "variable";
      }

      @Override
      public String visit(hydra.variants.TypeVariant.Void_ ignored) {
        return "void";
      }

      @Override
      public String visit(hydra.variants.TypeVariant.Wrap ignored) {
        return "wrap";
      }
    });
  }
}
