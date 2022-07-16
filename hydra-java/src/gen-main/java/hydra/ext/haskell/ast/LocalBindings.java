package hydra.ext.haskell.ast;

public class LocalBindings {
  public final java.util.List<LocalBinding> value;
  
  public LocalBindings (java.util.List<LocalBinding> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LocalBindings)) {
      return false;
    }
    LocalBindings o = (LocalBindings) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}