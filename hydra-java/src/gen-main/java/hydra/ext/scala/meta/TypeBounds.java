// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class TypeBounds implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.TypeBounds");
  
  public static final hydra.core.Name FIELD_NAME_LO = new hydra.core.Name("lo");
  
  public static final hydra.core.Name FIELD_NAME_HI = new hydra.core.Name("hi");
  
  public final hydra.util.Opt<hydra.ext.scala.meta.Type> lo;
  
  public final hydra.util.Opt<hydra.ext.scala.meta.Type> hi;
  
  public TypeBounds (hydra.util.Opt<hydra.ext.scala.meta.Type> lo, hydra.util.Opt<hydra.ext.scala.meta.Type> hi) {
    java.util.Objects.requireNonNull((lo));
    java.util.Objects.requireNonNull((hi));
    this.lo = lo;
    this.hi = hi;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeBounds)) {
      return false;
    }
    TypeBounds o = (TypeBounds) (other);
    return lo.equals(o.lo) && hi.equals(o.hi);
  }
  
  @Override
  public int hashCode() {
    return 2 * lo.hashCode() + 3 * hi.hashCode();
  }
  
  public TypeBounds withLo(hydra.util.Opt<hydra.ext.scala.meta.Type> lo) {
    java.util.Objects.requireNonNull((lo));
    return new TypeBounds(lo, hi);
  }
  
  public TypeBounds withHi(hydra.util.Opt<hydra.ext.scala.meta.Type> hi) {
    java.util.Objects.requireNonNull((hi));
    return new TypeBounds(lo, hi);
  }
}