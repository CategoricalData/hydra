package hydra.ext.haskell.ast;

/**
 * A type assertion
 */
public class Assertion {
  public final Name name;
  
  public final java.util.List<Type> types;
  
  public Assertion (Name name, java.util.List<Type> types) {
    this.name = name;
    this.types = types;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Assertion)) {
      return false;
    }
    Assertion o = (Assertion) (other);
    return name.equals(o.name) && types.equals(o.types);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * types.hashCode();
  }
  
  public Assertion withName(Name name) {
    return new Assertion(name, types);
  }
  
  public Assertion withTypes(java.util.List<Type> types) {
    return new Assertion(name, types);
  }
}