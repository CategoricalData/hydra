package hydra.ext.java.syntax;

public class Catches {
  public final java.util.List<hydra.ext.java.syntax.CatchClause> value;
  
  public Catches (java.util.List<hydra.ext.java.syntax.CatchClause> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Catches)) {
      return false;
    }
    Catches o = (Catches) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}