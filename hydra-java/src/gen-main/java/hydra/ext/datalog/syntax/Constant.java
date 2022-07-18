package hydra.ext.datalog.syntax;

public class Constant {
  public final String value;
  
  public Constant (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Constant)) {
      return false;
    }
    Constant o = (Constant) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}