// Note: this is an automatically generated file. Do not edit.

package hydra.dsl.yaml;

/**
 * DSL functions for hydra.yaml.model
 */
public interface Model {
  static hydra.phantoms.TTerm<hydra.yaml.model.Node> nodeMapping(hydra.phantoms.TTerm<java.util.Map<hydra.yaml.model.Node, hydra.yaml.model.Node>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.yaml.model.Node"), new hydra.core.Field(new hydra.core.Name("mapping"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.yaml.model.Node> nodeScalar(hydra.phantoms.TTerm<hydra.yaml.model.Scalar> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.yaml.model.Node"), new hydra.core.Field(new hydra.core.Name("scalar"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.yaml.model.Node> nodeSequence(hydra.phantoms.TTerm<java.util.List<hydra.yaml.model.Node>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.yaml.model.Node"), new hydra.core.Field(new hydra.core.Name("sequence"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.yaml.model.Scalar> scalarBool(hydra.phantoms.TTerm<Boolean> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.yaml.model.Scalar"), new hydra.core.Field(new hydra.core.Name("bool"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.yaml.model.Scalar> scalarDecimal(hydra.phantoms.TTerm<java.math.BigDecimal> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.yaml.model.Scalar"), new hydra.core.Field(new hydra.core.Name("decimal"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.yaml.model.Scalar> scalarFloat(hydra.phantoms.TTerm<java.math.BigDecimal> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.yaml.model.Scalar"), new hydra.core.Field(new hydra.core.Name("float"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.yaml.model.Scalar> scalarInt(hydra.phantoms.TTerm<java.math.BigInteger> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.yaml.model.Scalar"), new hydra.core.Field(new hydra.core.Name("int"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.yaml.model.Scalar> scalarNull() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.yaml.model.Scalar"), new hydra.core.Field(new hydra.core.Name("null"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.yaml.model.Scalar> scalarStr(hydra.phantoms.TTerm<String> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.yaml.model.Scalar"), new hydra.core.Field(new hydra.core.Name("str"), (x).value))));
  }
}
