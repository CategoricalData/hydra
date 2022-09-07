package hydra.ext.scala.meta;

public class Type_Bounds {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Type.Bounds");
  
  public final java.util.Optional<hydra.ext.scala.meta.Type> lo;
  
  public final java.util.Optional<hydra.ext.scala.meta.Type> hi;
  
  public Type_Bounds (java.util.Optional<hydra.ext.scala.meta.Type> lo, java.util.Optional<hydra.ext.scala.meta.Type> hi) {
    this.lo = lo;
    this.hi = hi;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Bounds)) {
      return false;
    }
    Type_Bounds o = (Type_Bounds) (other);
    return lo.equals(o.lo) && hi.equals(o.hi);
  }
  
  @Override
  public int hashCode() {
    return 2 * lo.hashCode() + 3 * hi.hashCode();
  }
  
  public Type_Bounds withLo(java.util.Optional<hydra.ext.scala.meta.Type> lo) {
    return new Type_Bounds(lo, hi);
  }
  
  public Type_Bounds withHi(java.util.Optional<hydra.ext.scala.meta.Type> hi) {
    return new Type_Bounds(lo, hi);
  }
}