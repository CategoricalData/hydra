// Note: this is an automatically generated file. Do not edit.

package hydra.compute;

import java.io.Serializable;

/**
 * The result of evaluating a Flow
 */
public class FlowState<S, X> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/compute.FlowState");
  
  public final java.util.Optional<X> value;
  
  public final S state;
  
  public final hydra.compute.Trace trace;
  
  public FlowState (java.util.Optional<X> value, S state, hydra.compute.Trace trace) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    if (state == null) {
      throw new IllegalArgumentException("null value for 'state' argument");
    }
    if (trace == null) {
      throw new IllegalArgumentException("null value for 'trace' argument");
    }
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
  
  public FlowState withValue(java.util.Optional<X> value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    return new FlowState(value, state, trace);
  }
  
  public FlowState withState(S state) {
    if (state == null) {
      throw new IllegalArgumentException("null value for 'state' argument");
    }
    return new FlowState(value, state, trace);
  }
  
  public FlowState withTrace(hydra.compute.Trace trace) {
    if (trace == null) {
      throw new IllegalArgumentException("null value for 'trace' argument");
    }
    return new FlowState(value, state, trace);
  }
}