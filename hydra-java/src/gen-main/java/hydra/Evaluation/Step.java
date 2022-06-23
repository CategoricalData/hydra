package hydra.evaluation;

/**
 * A qualified bidirectional transformation
 */
public class Step<A, B> {
  public final java.util.function.Function<A, Result<B>> out;
  
  public final java.util.function.Function<B, Result<A>> in;
  
  public Step (java.util.function.Function<A, Result<B>> out, java.util.function.Function<B, Result<A>> in) {
    this.out = out;
    this.in = in;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Step)) {
      return false;
    }
    Step o = (Step) (other);
    return out.equals(o.out) && in.equals(o.in);
  }
  
  @Override
  public int hashCode() {
    return 2 * out.hashCode() + 3 * in.hashCode();
  }
  
  public Step withOut(java.util.function.Function<A, Result<B>> out) {
    return new Step(out, in);
  }
  
  public Step withIn(java.util.function.Function<B, Result<A>> in) {
    return new Step(out, in);
  }
}