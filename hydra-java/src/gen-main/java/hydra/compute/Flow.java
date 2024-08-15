// Note: this is an automatically generated file. Do not edit.

package hydra.compute;

/**
 * A variant of the State monad with built-in logging and error handling
 */
public class Flow<S, X> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/compute.Flow");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.function.Function<S, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<S, X>>> value;
  
  public Flow (java.util.function.Function<S, java.util.function.Function<hydra.compute.Trace, hydra.compute.FlowState<S, X>>> value) {
    java.util.Objects.requireNonNull((value));
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