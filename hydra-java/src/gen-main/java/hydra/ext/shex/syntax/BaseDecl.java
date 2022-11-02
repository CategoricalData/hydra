package hydra.ext.shex.syntax;

public class BaseDecl {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.BaseDecl");
  
  public final hydra.ext.shex.syntax.IriRef iriRef;
  
  public BaseDecl (hydra.ext.shex.syntax.IriRef iriRef) {
    this.iriRef = iriRef;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BaseDecl)) {
      return false;
    }
    BaseDecl o = (BaseDecl) (other);
    return iriRef.equals(o.iriRef);
  }
  
  @Override
  public int hashCode() {
    return 2 * iriRef.hashCode();
  }
}