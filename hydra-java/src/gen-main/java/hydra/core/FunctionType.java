// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A function type, also known as an arrow type
 */
public class FunctionType<A> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.FunctionType");
  
  public final hydra.core.Type<A> domain;
  
  public final hydra.core.Type<A> codomain;
  
  public FunctionType (hydra.core.Type<A> domain, hydra.core.Type<A> codomain) {
    java.util.Objects.requireNonNull((domain));
    java.util.Objects.requireNonNull((codomain));
    this.domain = domain;
    this.codomain = codomain;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionType)) {
      return false;
    }
    FunctionType o = (FunctionType) (other);
    return domain.equals(o.domain) && codomain.equals(o.codomain);
  }
  
  @Override
  public int hashCode() {
    return 2 * domain.hashCode() + 3 * codomain.hashCode();
  }
  
  public FunctionType withDomain(hydra.core.Type<A> domain) {
    java.util.Objects.requireNonNull((domain));
    return new FunctionType(domain, codomain);
  }
  
  public FunctionType withCodomain(hydra.core.Type<A> codomain) {
    java.util.Objects.requireNonNull((codomain));
    return new FunctionType(domain, codomain);
  }
}