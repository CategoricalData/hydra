// Note: this is an automatically generated file. Do not edit.

package hydra.compute;

/**
 * A two-level bidirectional encoder which adapts types to types and terms to terms
 */
public class Adapter<S1, S2, T1, T2, V1, V2> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.compute.Adapter");
  
  public static final hydra.core.Name FIELD_NAME_IS_LOSSY = new hydra.core.Name("isLossy");
  
  public static final hydra.core.Name FIELD_NAME_SOURCE = new hydra.core.Name("source");
  
  public static final hydra.core.Name FIELD_NAME_TARGET = new hydra.core.Name("target");
  
  public static final hydra.core.Name FIELD_NAME_CODER = new hydra.core.Name("coder");
  
  public final Boolean isLossy;
  
  public final T1 source;
  
  public final T2 target;
  
  public final hydra.compute.Coder<S1, S2, V1, V2> coder;
  
  public Adapter (Boolean isLossy, T1 source, T2 target, hydra.compute.Coder<S1, S2, V1, V2> coder) {
    java.util.Objects.requireNonNull((isLossy));
    java.util.Objects.requireNonNull((source));
    java.util.Objects.requireNonNull((target));
    java.util.Objects.requireNonNull((coder));
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
    java.util.Objects.requireNonNull((isLossy));
    return new Adapter(isLossy, source, target, coder);
  }
  
  public Adapter withSource(T1 source) {
    java.util.Objects.requireNonNull((source));
    return new Adapter(isLossy, source, target, coder);
  }
  
  public Adapter withTarget(T2 target) {
    java.util.Objects.requireNonNull((target));
    return new Adapter(isLossy, source, target, coder);
  }
  
  public Adapter withCoder(hydra.compute.Coder<S1, S2, V1, V2> coder) {
    java.util.Objects.requireNonNull((coder));
    return new Adapter(isLossy, source, target, coder);
  }
}