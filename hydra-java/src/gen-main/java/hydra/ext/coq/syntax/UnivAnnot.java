package hydra.ext.coq.syntax;

public class UnivAnnot {
  public final java.util.List<UniverseLevel> value;
  
  public UnivAnnot (java.util.List<UniverseLevel> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnivAnnot)) {
      return false;
    }
    UnivAnnot o = (UnivAnnot) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}