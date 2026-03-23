// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class TypeBounds implements Serializable, Comparable<TypeBounds> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.TypeBounds");

  public static final hydra.core.Name LO = new hydra.core.Name("lo");

  public static final hydra.core.Name HI = new hydra.core.Name("hi");

  public final hydra.util.Maybe<hydra.ext.scala.meta.Type> lo;

  public final hydra.util.Maybe<hydra.ext.scala.meta.Type> hi;

  public TypeBounds (hydra.util.Maybe<hydra.ext.scala.meta.Type> lo, hydra.util.Maybe<hydra.ext.scala.meta.Type> hi) {
    this.lo = lo;
    this.hi = hi;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeBounds)) {
      return false;
    }
    TypeBounds o = (TypeBounds) other;
    return java.util.Objects.equals(
      this.lo,
      o.lo) && java.util.Objects.equals(
      this.hi,
      o.hi);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(lo) + 3 * java.util.Objects.hashCode(hi);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeBounds other) {
    int cmp = 0;
    cmp = ((Comparable) lo).compareTo(other.lo);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) hi).compareTo(other.hi);
  }

  public TypeBounds withLo(hydra.util.Maybe<hydra.ext.scala.meta.Type> lo) {
    return new TypeBounds(lo, hi);
  }

  public TypeBounds withHi(hydra.util.Maybe<hydra.ext.scala.meta.Type> hi) {
    return new TypeBounds(lo, hi);
  }
}
