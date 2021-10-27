package hydra.core;

/**
 * A function type, also known as an arrow type
 */
public class FunctionType {
  public final hydra.core.Type domain;
  
  public final hydra.core.Type codomain;
  
  /**
   * Constructs an immutable FunctionType object
   */
  public FunctionType(hydra.core.Type domain, hydra.core.Type codomain) {
    this.domain = domain;
    this.codomain = codomain;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionType)) {
        return false;
    }
    FunctionType o = (FunctionType) other;
    return domain.equals(o.domain)
        && codomain.equals(o.codomain);
  }
  
  @Override
  public int hashCode() {
    return 2 * domain.hashCode()
        + 3 * codomain.hashCode();
  }
  
  /**
   * Construct a new immutable FunctionType object in which domain is overridden
   */
  public FunctionType withDomain(hydra.core.Type domain) {
    return new FunctionType(domain, codomain);
  }
  
  /**
   * Construct a new immutable FunctionType object in which codomain is overridden
   */
  public FunctionType withCodomain(hydra.core.Type codomain) {
    return new FunctionType(domain, codomain);
  }
}
