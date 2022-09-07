package hydra.ext.java.syntax;

public class Block {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Block");
  
  public final java.util.List<hydra.ext.java.syntax.BlockStatement> value;
  
  public Block (java.util.List<hydra.ext.java.syntax.BlockStatement> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Block)) {
      return false;
    }
    Block o = (Block) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}