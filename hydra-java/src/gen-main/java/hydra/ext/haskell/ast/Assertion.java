package hydra.ext.haskell.ast;

/**
 * A type assertion
 */
public class Assertion {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Assertion");
  
  public final hydra.ext.haskell.ast.Name name;
  
  public final java.util.List<hydra.ext.haskell.ast.Type> types;
  
  public Assertion (hydra.ext.haskell.ast.Name name, java.util.List<hydra.ext.haskell.ast.Type> types) {
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
  
  public Assertion withName(hydra.ext.haskell.ast.Name name) {
    return new Assertion(name, types);
  }
  
  public Assertion withTypes(java.util.List<hydra.ext.haskell.ast.Type> types) {
    return new Assertion(name, types);
  }
}