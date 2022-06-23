package hydra.evaluation;

/**
 * A helper object for specifying and marshalling the output of a primitive function
 */
public class OutputSpec<A, M> {
  public final hydra.core.Type<M> type;
  
  public final java.util.function.Function<A, hydra.core.Term<M>> marshal;
  
  public OutputSpec (hydra.core.Type<M> type, java.util.function.Function<A, hydra.core.Term<M>> marshal) {
    this.type = type;
    this.marshal = marshal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OutputSpec)) {
      return false;
    }
    OutputSpec o = (OutputSpec) (other);
    return type.equals(o.type) && marshal.equals(o.marshal);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * marshal.hashCode();
  }
  
  public OutputSpec withType(hydra.core.Type<M> type) {
    return new OutputSpec(type, marshal);
  }
  
  public OutputSpec withMarshal(java.util.function.Function<A, hydra.core.Term<M>> marshal) {
    return new OutputSpec(type, marshal);
  }
}