package hydra.evaluation;

/**
 * A variant of the State monad with built-in logging and error handling
 */
public class Flow<S, A> {
  public final java.util.function.Function<S, java.util.function.Function<hydra.evaluation.Trace, hydra.evaluation.FlowWrapper<S, A>>> value;
  
  public Flow (java.util.function.Function<S, java.util.function.Function<hydra.evaluation.Trace, hydra.evaluation.FlowWrapper<S, A>>> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Flow)) {
      return false;
    }
    Flow o = (Flow) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}