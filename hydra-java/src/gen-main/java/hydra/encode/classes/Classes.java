// Note: this is an automatically generated file. Do not edit.

package hydra.encode.classes;

/**
 * Term encoders for hydra.classes
 */
public interface Classes {
  static hydra.core.Term typeClass(hydra.classes.TypeClass v1) {
    return ((v1)).accept(new hydra.classes.TypeClass.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.classes.TypeClass.Equality y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.classes.TypeClass"), new hydra.core.Field(new hydra.core.Name("equality"), new hydra.core.Term.Unit())));
      }
      
      @Override
      public hydra.core.Term visit(hydra.classes.TypeClass.Ordering y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.classes.TypeClass"), new hydra.core.Field(new hydra.core.Name("ordering"), new hydra.core.Term.Unit())));
      }
    });
  }
}
