package hydra.ext.scala.meta;

public class Pat_Alternative {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Pat.Alternative");
  
  public final hydra.ext.scala.meta.Pat lhs;
  
  public final hydra.ext.scala.meta.Pat rhs;
  
  public Pat_Alternative (hydra.ext.scala.meta.Pat lhs, hydra.ext.scala.meta.Pat rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Alternative)) {
      return false;
    }
    Pat_Alternative o = (Pat_Alternative) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Pat_Alternative withLhs(hydra.ext.scala.meta.Pat lhs) {
    return new Pat_Alternative(lhs, rhs);
  }
  
  public Pat_Alternative withRhs(hydra.ext.scala.meta.Pat rhs) {
    return new Pat_Alternative(lhs, rhs);
  }
}