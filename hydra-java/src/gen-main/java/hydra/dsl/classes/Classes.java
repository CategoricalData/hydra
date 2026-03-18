// Note: this is an automatically generated file. Do not edit.

package hydra.dsl.classes;

/**
 * DSL functions for hydra.classes
 */
public interface Classes {
  static hydra.phantoms.TTerm<hydra.classes.TypeClass> typeClassEquality() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.classes.TypeClass"), new hydra.core.Field(new hydra.core.Name("equality"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.classes.TypeClass> typeClassOrdering() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.classes.TypeClass"), new hydra.core.Field(new hydra.core.Name("ordering"), new hydra.core.Term.Unit()))));
  }
}
