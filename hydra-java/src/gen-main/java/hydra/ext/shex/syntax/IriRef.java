package hydra.ext.shex.syntax;

public class IriRef {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.IriRef");
  
  public final java.util.List<hydra.ext.shex.syntax.IriRef_Elmt> value;
  
  public IriRef (java.util.List<hydra.ext.shex.syntax.IriRef_Elmt> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IriRef)) {
      return false;
    }
    IriRef o = (IriRef) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}