// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_Bounds implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Type.Bounds");
  
  public static final hydra.core.Name FIELD_NAME_LO = new hydra.core.Name("lo");
  
  public static final hydra.core.Name FIELD_NAME_HI = new hydra.core.Name("hi");
  
  public final hydra.util.Opt<hydra.ext.scala.meta.Type> lo;
  
  public final hydra.util.Opt<hydra.ext.scala.meta.Type> hi;
  
  public Type_Bounds (hydra.util.Opt<hydra.ext.scala.meta.Type> lo, hydra.util.Opt<hydra.ext.scala.meta.Type> hi) {
    java.util.Objects.requireNonNull((lo));
    java.util.Objects.requireNonNull((hi));
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
  
  public Type_Bounds withLo(hydra.util.Opt<hydra.ext.scala.meta.Type> lo) {
    java.util.Objects.requireNonNull((lo));
    return new Type_Bounds(lo, hi);
  }
  
  public Type_Bounds withHi(hydra.util.Opt<hydra.ext.scala.meta.Type> hi) {
    java.util.Objects.requireNonNull((hi));
    return new Type_Bounds(lo, hi);
  }
}
