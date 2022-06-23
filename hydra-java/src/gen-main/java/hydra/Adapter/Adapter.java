package hydra.adapter;

public class Adapter<T, V> {
  public final Boolean isLossy;
  
  public final T source;
  
  public final T target;
  
  public final hydra.evaluation.Step<V, V> step;
  
  public Adapter (Boolean isLossy, T source, T target, hydra.evaluation.Step<V, V> step) {
    this.isLossy = isLossy;
    this.source = source;
    this.target = target;
    this.step = step;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Adapter)) {
      return false;
    }
    Adapter o = (Adapter) (other);
    return isLossy.equals(o.isLossy) && source.equals(o.source) && target.equals(o.target) && step.equals(o.step);
  }
  
  @Override
  public int hashCode() {
    return 2 * isLossy.hashCode() + 3 * source.hashCode() + 5 * target.hashCode() + 7 * step.hashCode();
  }
  
  public Adapter withIsLossy(Boolean isLossy) {
    return new Adapter(isLossy, source, target, step);
  }
  
  public Adapter withSource(T source) {
    return new Adapter(isLossy, source, target, step);
  }
  
  public Adapter withTarget(T target) {
    return new Adapter(isLossy, source, target, step);
  }
  
  public Adapter withStep(hydra.evaluation.Step<V, V> step) {
    return new Adapter(isLossy, source, target, step);
  }
}