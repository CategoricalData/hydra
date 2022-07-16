package hydra.ext.haskell.ast;

public class Type_Function {
  public final Type domain;
  
  public final Type codomain;
  
  public Type_Function (Type domain, Type codomain) {
    this.domain = domain;
    this.codomain = codomain;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Function)) {
      return false;
    }
    Type_Function o = (Type_Function) (other);
    return domain.equals(o.domain) && codomain.equals(o.codomain);
  }
  
  @Override
  public int hashCode() {
    return 2 * domain.hashCode() + 3 * codomain.hashCode();
  }
  
  public Type_Function withDomain(Type domain) {
    return new Type_Function(domain, codomain);
  }
  
  public Type_Function withCodomain(Type codomain) {
    return new Type_Function(domain, codomain);
  }
}