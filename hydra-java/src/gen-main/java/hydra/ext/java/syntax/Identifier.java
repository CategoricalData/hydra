package hydra.ext.java.syntax;

public class Identifier {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Identifier");
  
  public final String value;
  
  public Identifier (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Identifier)) {
      return false;
    }
    Identifier o = (Identifier) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}