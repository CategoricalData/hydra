// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Type_Bounds implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Type.Bounds");
  
  public final java.util.Optional<hydra.langs.scala.meta.Type> lo;
  
  public final java.util.Optional<hydra.langs.scala.meta.Type> hi;
  
  public Type_Bounds (java.util.Optional<hydra.langs.scala.meta.Type> lo, java.util.Optional<hydra.langs.scala.meta.Type> hi) {
    if (lo == null) {
      throw new IllegalArgumentException("null value for 'lo' argument");
    }
    if (hi == null) {
      throw new IllegalArgumentException("null value for 'hi' argument");
    }
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
  
  public Type_Bounds withLo(java.util.Optional<hydra.langs.scala.meta.Type> lo) {
    if (lo == null) {
      throw new IllegalArgumentException("null value for 'lo' argument");
    }
    return new Type_Bounds(lo, hi);
  }
  
  public Type_Bounds withHi(java.util.Optional<hydra.langs.scala.meta.Type> hi) {
    if (hi == null) {
      throw new IllegalArgumentException("null value for 'hi' argument");
    }
    return new Type_Bounds(lo, hi);
  }
}