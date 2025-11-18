// Note: this is an automatically generated file. Do not edit.

package hydra.compute;

/**
 * The result of evaluating a Flow
 */
public class FlowState<S, V> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.compute.FlowState");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name FIELD_NAME_STATE = new hydra.core.Name("state");
  
  public static final hydra.core.Name FIELD_NAME_TRACE = new hydra.core.Name("trace");
  
  /**
   * The resulting value, or nothing in the case of failure
   */
  public final hydra.util.Maybe<V> value;
  
  /**
   * The final state
   */
  public final S state;
  
  /**
   * The trace (log) produced during evaluation
   */
  public final hydra.compute.Trace trace;
  
  public FlowState (hydra.util.Maybe<V> value, S state, hydra.compute.Trace trace) {
    java.util.Objects.requireNonNull((value));
    java.util.Objects.requireNonNull((state));
    java.util.Objects.requireNonNull((trace));
    this.value = value;
    this.state = state;
    this.trace = trace;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FlowState)) {
      return false;
    }
    FlowState o = (FlowState) (other);
    return value.equals(o.value) && state.equals(o.state) && trace.equals(o.trace);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode() + 3 * state.hashCode() + 5 * trace.hashCode();
  }
  
  public FlowState withValue(hydra.util.Maybe<V> value) {
    java.util.Objects.requireNonNull((value));
    return new FlowState(value, state, trace);
  }
  
  public FlowState withState(S state) {
    java.util.Objects.requireNonNull((state));
    return new FlowState(value, state, trace);
  }
  
  public FlowState withTrace(hydra.compute.Trace trace) {
    java.util.Objects.requireNonNull((trace));
    return new FlowState(value, state, trace);
  }
}
