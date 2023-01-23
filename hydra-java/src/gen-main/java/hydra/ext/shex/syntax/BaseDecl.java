package hydra.ext.shex.syntax;

public class BaseDecl {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.BaseDecl");
  
  public final hydra.ext.shex.syntax.IriRef value;
  
  public BaseDecl (hydra.ext.shex.syntax.IriRef value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BaseDecl)) {
      return false;
    }
    BaseDecl o = (BaseDecl) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}