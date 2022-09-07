package hydra.ext.scala.meta;

public class Pat_Bind {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Pat.Bind");
  
  public final hydra.ext.scala.meta.Pat lhs;
  
  public final hydra.ext.scala.meta.Pat rhs;
  
  public Pat_Bind (hydra.ext.scala.meta.Pat lhs, hydra.ext.scala.meta.Pat rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Bind)) {
      return false;
    }
    Pat_Bind o = (Pat_Bind) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Pat_Bind withLhs(hydra.ext.scala.meta.Pat lhs) {
    return new Pat_Bind(lhs, rhs);
  }
  
  public Pat_Bind withRhs(hydra.ext.scala.meta.Pat rhs) {
    return new Pat_Bind(lhs, rhs);
  }
}