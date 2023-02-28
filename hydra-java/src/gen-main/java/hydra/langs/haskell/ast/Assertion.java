package hydra.langs.haskell.ast;

/**
 * A type assertion
 */
public class Assertion {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/haskell/ast.Assertion");
  
  public final hydra.langs.haskell.ast.Name name;
  
  public final java.util.List<hydra.langs.haskell.ast.Type> types;
  
  public Assertion (hydra.langs.haskell.ast.Name name, java.util.List<hydra.langs.haskell.ast.Type> types) {
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
  
  public Assertion withName(hydra.langs.haskell.ast.Name name) {
    return new Assertion(name, types);
  }
  
  public Assertion withTypes(java.util.List<hydra.langs.haskell.ast.Type> types) {
    return new Assertion(name, types);
  }
}