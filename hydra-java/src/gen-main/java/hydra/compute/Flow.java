// Note: this is an automatically generated file. Do not edit.

package hydra.compute;

import java.io.Serializable;

/**
 * A variant of the State monad with built-in logging and error handling
 */
public class Flow<S, V> implements Serializable, Comparable<Flow<S, V>> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.compute.Flow");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.function.Function<S, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<S, V>>> value;
  
  public Flow (java.util.function.Function<S, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<S, V>>> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Flow)) {
      return false;
    }
    Flow o = (Flow) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Flow other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
