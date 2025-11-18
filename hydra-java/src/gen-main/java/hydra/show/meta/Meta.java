// Note: this is an automatically generated file. Do not edit.

package hydra.show.meta;

/**
 * String representations of hydra.meta types
 */
public interface Meta {
  static String termVariant(hydra.meta.TermVariant v1) {
    return ((v1)).accept(new hydra.meta.TermVariant.Visitor<>() {
      @Override
      public String visit(hydra.meta.TermVariant.Annotated ignored) {
        return "annotated";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.Application ignored) {
        return "application";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.Either ignored) {
        return "either";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.Function ignored) {
        return "function";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.Let ignored) {
        return "let";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.List ignored) {
        return "list";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.Literal ignored) {
        return "literal";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.Map ignored) {
        return "map";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.Maybe ignored) {
        return "maybe";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.Pair ignored) {
        return "pair";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.Product ignored) {
        return "product";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.Record ignored) {
        return "record";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.Set ignored) {
        return "set";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.Sum ignored) {
        return "sum";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.TypeLambda ignored) {
        return "typeLambda";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.TypeApplication ignored) {
        return "typeApplication";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.Union ignored) {
        return "union";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.Unit ignored) {
        return "unit";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.Variable ignored) {
        return "variable";
      }
      
      @Override
      public String visit(hydra.meta.TermVariant.Wrap ignored) {
        return "wrap";
      }
    });
  }
  
  static String typeVariant(hydra.meta.TypeVariant v1) {
    return ((v1)).accept(new hydra.meta.TypeVariant.Visitor<>() {
      @Override
      public String visit(hydra.meta.TypeVariant.Annotated ignored) {
        return "annotated";
      }
      
      @Override
      public String visit(hydra.meta.TypeVariant.Application ignored) {
        return "application";
      }
      
      @Override
      public String visit(hydra.meta.TypeVariant.Either ignored) {
        return "either";
      }
      
      @Override
      public String visit(hydra.meta.TypeVariant.Forall ignored) {
        return "forall";
      }
      
      @Override
      public String visit(hydra.meta.TypeVariant.Function ignored) {
        return "function";
      }
      
      @Override
      public String visit(hydra.meta.TypeVariant.List ignored) {
        return "list";
      }
      
      @Override
      public String visit(hydra.meta.TypeVariant.Literal ignored) {
        return "literal";
      }
      
      @Override
      public String visit(hydra.meta.TypeVariant.Map ignored) {
        return "map";
      }
      
      @Override
      public String visit(hydra.meta.TypeVariant.Maybe ignored) {
        return "maybe";
      }
      
      @Override
      public String visit(hydra.meta.TypeVariant.Pair ignored) {
        return "pair";
      }
      
      @Override
      public String visit(hydra.meta.TypeVariant.Product ignored) {
        return "product";
      }
      
      @Override
      public String visit(hydra.meta.TypeVariant.Record ignored) {
        return "record";
      }
      
      @Override
      public String visit(hydra.meta.TypeVariant.Set ignored) {
        return "set";
      }
      
      @Override
      public String visit(hydra.meta.TypeVariant.Sum ignored) {
        return "sum";
      }
      
      @Override
      public String visit(hydra.meta.TypeVariant.Union ignored) {
        return "union";
      }
      
      @Override
      public String visit(hydra.meta.TypeVariant.Unit ignored) {
        return "unit";
      }
      
      @Override
      public String visit(hydra.meta.TypeVariant.Variable ignored) {
        return "variable";
      }
      
      @Override
      public String visit(hydra.meta.TypeVariant.Wrap ignored) {
        return "wrap";
      }
    });
  }
}
