package hydra.adapter;

public class Adapter<S, T, V> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/adapter.Adapter");
  
  public final Boolean isLossy;
  
  public final T source;
  
  public final T target;
  
  public final hydra.compute.Coder<S, V, V> coder;
  
  public Adapter (Boolean isLossy, T source, T target, hydra.compute.Coder<S, V, V> coder) {
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
    return new Adapter(isLossy, source, target, coder);
  }
  
  public Adapter withSource(T source) {
    return new Adapter(isLossy, source, target, coder);
  }
  
  public Adapter withTarget(T target) {
    return new Adapter(isLossy, source, target, coder);
  }
  
  public Adapter withCoder(hydra.compute.Coder<S, V, V> coder) {
    return new Adapter(isLossy, source, target, coder);
  }
}