// Note: this is an automatically generated file. Do not edit.

package hydra.dsl.json.model;

/**
 * DSL functions for hydra.json.model
 */
public interface Model {
  static hydra.phantoms.TTerm<hydra.json.model.Value> valueArray(hydra.phantoms.TTerm<hydra.util.ConsList<hydra.json.model.Value>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.json.model.Value"), new hydra.core.Field(new hydra.core.Name("array"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.json.model.Value> valueBoolean(hydra.phantoms.TTerm<Boolean> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.json.model.Value"), new hydra.core.Field(new hydra.core.Name("boolean"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.json.model.Value> valueNull() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.json.model.Value"), new hydra.core.Field(new hydra.core.Name("null"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.json.model.Value> valueNumber(hydra.phantoms.TTerm<java.math.BigDecimal> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.json.model.Value"), new hydra.core.Field(new hydra.core.Name("number"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.json.model.Value> valueObject(hydra.phantoms.TTerm<hydra.util.PersistentMap<String, hydra.json.model.Value>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.json.model.Value"), new hydra.core.Field(new hydra.core.Name("object"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.json.model.Value> valueString(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.json.model.Value"), new hydra.core.Field(new hydra.core.Name("string"), (x).value))));
  }
}
