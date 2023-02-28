package hydra.langs.shex.syntax;

public class Echar {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.Echar");
  
  public final String value;
  
  public Echar (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Echar)) {
      return false;
    }
    Echar o = (Echar) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}