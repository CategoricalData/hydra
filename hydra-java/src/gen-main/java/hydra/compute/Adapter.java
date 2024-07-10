// Note: this is an automatically generated file. Do not edit.

package hydra.compute;

/**
 * A two-level bidirectional encoder which adapts types to types and terms to terms
 */
public class Adapter<S1, S2, T1, T2, V1, V2> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/compute.Adapter");
  
  public final Boolean isLossy;
  
  public final T1 source;
  
  public final T2 target;
  
  public final hydra.compute.Coder<S1, S2, V1, V2> coder;
  
  public Adapter (Boolean isLossy, T1 source, T2 target, hydra.compute.Coder<S1, S2, V1, V2> coder) {
    if (isLossy == null) {
      throw new IllegalArgumentException("null value for 'isLossy' argument");
    }
    if (source == null) {
      throw new IllegalArgumentException("null value for 'source' argument");
    }
    if (target == null) {
      throw new IllegalArgumentException("null value for 'target' argument");
    }
    if (coder == null) {
      throw new IllegalArgumentException("null value for 'coder' argument");
    }
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
    return isLossy.equals(o.isLossy) && source.equals(o.source) && target.equals(o.target) && coder.equals(o.coder);
  }
  
  @Override
  public int hashCode() {
    return 2 * isLossy.hashCode() + 3 * source.hashCode() + 5 * target.hashCode() + 7 * coder.hashCode();
  }
  
  public Adapter withIsLossy(Boolean isLossy) {
    if (isLossy == null) {
      throw new IllegalArgumentException("null value for 'isLossy' argument");
    }
    return new Adapter(isLossy, source, target, coder);
  }
  
  public Adapter withSource(T1 source) {
    if (source == null) {
      throw new IllegalArgumentException("null value for 'source' argument");
    }
    return new Adapter(isLossy, source, target, coder);
  }
  
  public Adapter withTarget(T2 target) {
    if (target == null) {
      throw new IllegalArgumentException("null value for 'target' argument");
    }
    return new Adapter(isLossy, source, target, coder);
  }
  
  public Adapter withCoder(hydra.compute.Coder<S1, S2, V1, V2> coder) {
    if (coder == null) {
      throw new IllegalArgumentException("null value for 'coder' argument");
    }
    return new Adapter(isLossy, source, target, coder);
  }
}