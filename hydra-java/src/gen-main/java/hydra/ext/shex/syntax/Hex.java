package hydra.ext.shex.syntax;

public class Hex {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.Hex");
  
  public final String value;
  
  public Hex (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Hex)) {
      return false;
    }
    Hex o = (Hex) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}