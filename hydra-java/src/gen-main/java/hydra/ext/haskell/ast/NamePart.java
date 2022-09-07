package hydra.ext.haskell.ast;

public class NamePart {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.NamePart");
  
  public final String value;
  
  public NamePart (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NamePart)) {
      return false;
    }
    NamePart o = (NamePart) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}