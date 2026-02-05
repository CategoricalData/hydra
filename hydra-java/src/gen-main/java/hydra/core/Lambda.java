// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A function abstraction (lambda)
 */
public class Lambda implements Serializable, Comparable<Lambda> {
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
  public final hydra.util.Maybe<hydra.core.Type> domain;
  
  /**
   * The body of the lambda
   */
  public final hydra.core.Term body;
  
  public Lambda (hydra.core.Name parameter, hydra.util.Maybe<hydra.core.Type> domain, hydra.core.Term body) {
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
    return java.util.Objects.equals(
      this.parameter,
      o.parameter) && java.util.Objects.equals(
      this.domain,
      o.domain) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(parameter) + 3 * java.util.Objects.hashCode(domain) + 5 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Lambda other) {
    int cmp = 0;
    cmp = ((Comparable) (parameter)).compareTo(other.parameter);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      domain.hashCode(),
      other.domain.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (body)).compareTo(other.body);
  }
  
  public Lambda withParameter(hydra.core.Name parameter) {
    return new Lambda(parameter, domain, body);
  }
  
  public Lambda withDomain(hydra.util.Maybe<hydra.core.Type> domain) {
    return new Lambda(parameter, domain, body);
  }
  
  public Lambda withBody(hydra.core.Term body) {
    return new Lambda(parameter, domain, body);
  }
}
