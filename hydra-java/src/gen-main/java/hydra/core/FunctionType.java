package hydra.core;

/**
 * A function type, also known as an arrow type
 */
public class FunctionType<M> {
  public final Type<M> domain;
  
  public final Type<M> codomain;
  
  public FunctionType (Type<M> domain, Type<M> codomain) {
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
  
  public FunctionType withDomain(Type<M> domain) {
    return new FunctionType(domain, codomain);
  }
  
  public FunctionType withCodomain(Type<M> codomain) {
    return new FunctionType(domain, codomain);
  }
}