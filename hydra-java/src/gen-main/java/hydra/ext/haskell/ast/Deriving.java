package hydra.ext.haskell.ast;

/**
 * A 'deriving' statement
 */
public class Deriving {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Deriving");
  
  /**
   * A 'deriving' statement
   */
  public final java.util.List<hydra.ext.haskell.ast.Name> value;
  
  public Deriving (java.util.List<hydra.ext.haskell.ast.Name> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Deriving)) {
      return false;
    }
    Deriving o = (Deriving) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}