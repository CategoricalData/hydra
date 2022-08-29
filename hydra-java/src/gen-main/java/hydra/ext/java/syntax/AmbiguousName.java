package hydra.ext.java.syntax;

public class AmbiguousName {
  public final java.util.List<hydra.ext.java.syntax.Identifier> value;
  
  public AmbiguousName (java.util.List<hydra.ext.java.syntax.Identifier> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AmbiguousName)) {
      return false;
    }
    AmbiguousName o = (AmbiguousName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}