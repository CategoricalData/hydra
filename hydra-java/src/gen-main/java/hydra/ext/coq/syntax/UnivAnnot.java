package hydra.ext.coq.syntax;

public class UnivAnnot {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.UnivAnnot");
  
  public final java.util.List<hydra.ext.coq.syntax.UniverseLevel> value;
  
  public UnivAnnot (java.util.List<hydra.ext.coq.syntax.UniverseLevel> value) {
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