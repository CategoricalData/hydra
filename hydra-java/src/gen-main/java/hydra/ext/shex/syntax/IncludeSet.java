package hydra.ext.shex.syntax;

public class IncludeSet {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.IncludeSet");
  
  public final java.util.List<hydra.ext.shex.syntax.ShapeExprLabel> value;
  
  public IncludeSet (java.util.List<hydra.ext.shex.syntax.ShapeExprLabel> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IncludeSet)) {
      return false;
    }
    IncludeSet o = (IncludeSet) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}