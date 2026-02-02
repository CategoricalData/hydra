// Note: this is an automatically generated file. Do not edit.

package hydra.encode.coders;

/**
 * Term encoders for hydra.coders
 */
public interface Coders {
  static hydra.core.Term coderDirection(hydra.coders.CoderDirection v1) {
    return ((v1)).accept(new hydra.coders.CoderDirection.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.coders.CoderDirection.Encode y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.coders.CoderDirection"), new hydra.core.Field(new hydra.core.Name("encode"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.coders.CoderDirection.Decode y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.coders.CoderDirection"), new hydra.core.Field(new hydra.core.Name("decode"), new hydra.core.Term.Unit(true))));
      }
    });
  }
  
  static hydra.core.Term languageName(hydra.coders.LanguageName x) {
    return new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.coders.LanguageName"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((x)).value))));
  }
  
  static hydra.core.Term traversalOrder(hydra.coders.TraversalOrder v1) {
    return ((v1)).accept(new hydra.coders.TraversalOrder.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.coders.TraversalOrder.Pre y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.coders.TraversalOrder"), new hydra.core.Field(new hydra.core.Name("pre"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.coders.TraversalOrder.Post y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.coders.TraversalOrder"), new hydra.core.Field(new hydra.core.Name("post"), new hydra.core.Term.Unit(true))));
      }
    });
  }
}
