package hydra.evaluation;

/**
 * A helper object for specifying and unmarshalling an argument to a primitive function
 */
public class InputSpec<A, M> {
  public final hydra.core.Type<M> type;
  
  public final java.util.function.Function<hydra.core.Term<M>, Result<A>> unmarshal;
  
  public InputSpec (hydra.core.Type<M> type, java.util.function.Function<hydra.core.Term<M>, Result<A>> unmarshal) {
    this.type = type;
    this.unmarshal = unmarshal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InputSpec)) {
      return false;
    }
    InputSpec o = (InputSpec) (other);
    return type.equals(o.type) && unmarshal.equals(o.unmarshal);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * unmarshal.hashCode();
  }
  
  public InputSpec withType(hydra.core.Type<M> type) {
    return new InputSpec(type, unmarshal);
  }
  
  public InputSpec withUnmarshal(java.util.function.Function<hydra.core.Term<M>, Result<A>> unmarshal) {
    return new InputSpec(type, unmarshal);
  }
}