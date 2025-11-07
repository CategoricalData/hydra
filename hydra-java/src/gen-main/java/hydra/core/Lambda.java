// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A function abstraction (lambda)
 */
public class Lambda implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.Lambda");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETER = new hydra.core.Name("parameter");
  
  public static final hydra.core.Name FIELD_NAME_DOMAIN = new hydra.core.Name("domain");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  /**
   * The parameter of the lambda
   */
  public final hydra.core.Name parameter;
  
  /**
   * An optional domain type for the lambda
   */
  public final hydra.util.Opt<hydra.core.Type> domain;
  
  /**
   * The body of the lambda
   */
  public final hydra.core.Term body;
  
  public Lambda (hydra.core.Name parameter, hydra.util.Opt<hydra.core.Type> domain, hydra.core.Term body) {
    java.util.Objects.requireNonNull((parameter));
    java.util.Objects.requireNonNull((domain));
    java.util.Objects.requireNonNull((body));
    this.parameter = parameter;
    this.domain = domain;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Lambda)) {
      return false;
    }
    Lambda o = (Lambda) (other);
    return parameter.equals(o.parameter) && domain.equals(o.domain) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameter.hashCode() + 3 * domain.hashCode() + 5 * body.hashCode();
  }
  
  public Lambda withParameter(hydra.core.Name parameter) {
    java.util.Objects.requireNonNull((parameter));
    return new Lambda(parameter, domain, body);
  }
  
  public Lambda withDomain(hydra.util.Opt<hydra.core.Type> domain) {
    java.util.Objects.requireNonNull((domain));
    return new Lambda(parameter, domain, body);
  }
  
  public Lambda withBody(hydra.core.Term body) {
    java.util.Objects.requireNonNull((body));
    return new Lambda(parameter, domain, body);
  }
}
