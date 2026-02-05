// Note: this is an automatically generated file. Do not edit.

package hydra.compute;

import java.io.Serializable;

/**
 * The result of evaluating a Flow
 */
public class FlowState<S, V> implements Serializable, Comparable<FlowState<S, V>> {
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
    return java.util.Objects.equals(
      this.value,
      o.value) && java.util.Objects.equals(
      this.state,
      o.state) && java.util.Objects.equals(
      this.trace,
      o.trace);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value) + 3 * java.util.Objects.hashCode(state) + 5 * java.util.Objects.hashCode(trace);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FlowState other) {
    int cmp = 0;
    cmp = Integer.compare(
      value.hashCode(),
      other.value.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (state)).compareTo(other.state);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (trace)).compareTo(other.trace);
  }
  
  public FlowState withValue(hydra.util.Maybe<V> value) {
    return new FlowState(value, state, trace);
  }
  
  public FlowState withState(S state) {
    return new FlowState(value, state, trace);
  }
  
  public FlowState withTrace(hydra.compute.Trace trace) {
    return new FlowState(value, state, trace);
  }
}
