package hydra.core;

/**
 * A function type, also known as an arrow type
 */
public class FunctionType {
  /**
   * @type hydra/core.Type
   */
  public final Type domain;
  
  /**
   * @type hydra/core.Type
   */
  public final Type codomain;
  
  /**
   * Constructs an immutable FunctionType object
   */
  public FunctionType(Type domain, Type codomain) {
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
  public FunctionType withDomain(Type domain) {
    return new FunctionType(domain, codomain);
  }
  
  /**
   * Construct a new immutable FunctionType object in which codomain is overridden
   */
  public FunctionType withCodomain(Type codomain) {
    return new FunctionType(domain, codomain);
  }
}
