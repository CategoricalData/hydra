// Note: this is an automatically generated file. Do not edit.

package hydra.compute;

import java.io.Serializable;

/**
 * A two-level bidirectional encoder which adapts types to types and terms to terms
 */
public class Adapter<S1, S2, T1, T2, V1, V2> implements Serializable, Comparable<Adapter<S1, S2, T1, T2, V1, V2>> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.compute.Adapter");
  
  public static final hydra.core.Name FIELD_NAME_IS_LOSSY = new hydra.core.Name("isLossy");
  
  public static final hydra.core.Name FIELD_NAME_SOURCE = new hydra.core.Name("source");
  
  public static final hydra.core.Name FIELD_NAME_TARGET = new hydra.core.Name("target");
  
  public static final hydra.core.Name FIELD_NAME_CODER = new hydra.core.Name("coder");
  
  /**
   * Whether information may be lost in the course of this adaptation
   */
  public final Boolean isLossy;
  
  /**
   * The source type
   */
  public final T1 source;
  
  /**
   * The target type
   */
  public final T2 target;
  
  /**
   * The coder for transforming instances of the source type to instances of the target type
   */
  public final hydra.compute.Coder<S1, S2, V1, V2> coder;
  
  public Adapter (Boolean isLossy, T1 source, T2 target, hydra.compute.Coder<S1, S2, V1, V2> coder) {
    this.isLossy = isLossy;
    this.source = source;
    this.target = target;
    this.coder = coder;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Adapter)) {
      return false;
    }
    Adapter o = (Adapter) (other);
    return java.util.Objects.equals(
      this.isLossy,
      o.isLossy) && java.util.Objects.equals(
      this.source,
      o.source) && java.util.Objects.equals(
      this.target,
      o.target) && java.util.Objects.equals(
      this.coder,
      o.coder);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(isLossy) + 3 * java.util.Objects.hashCode(source) + 5 * java.util.Objects.hashCode(target) + 7 * java.util.Objects.hashCode(coder);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Adapter other) {
    int cmp = 0;
    cmp = ((Comparable) (isLossy)).compareTo(other.isLossy);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (source)).compareTo(other.source);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (target)).compareTo(other.target);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (coder)).compareTo(other.coder);
  }
  
  public Adapter withIsLossy(Boolean isLossy) {
    return new Adapter(isLossy, source, target, coder);
  }
  
  public Adapter withSource(T1 source) {
    return new Adapter(isLossy, source, target, coder);
  }
  
  public Adapter withTarget(T2 target) {
    return new Adapter(isLossy, source, target, coder);
  }
  
  public Adapter withCoder(hydra.compute.Coder<S1, S2, V1, V2> coder) {
    return new Adapter(isLossy, source, target, coder);
  }
}
