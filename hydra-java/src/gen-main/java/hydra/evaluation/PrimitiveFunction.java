package hydra.evaluation;

/**
 * A built-in function
 */
public class PrimitiveFunction<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/evaluation.PrimitiveFunction");
  
  public final hydra.core.Name name;
  
  public final hydra.core.FunctionType<M> type;
  
  public final java.util.function.Function<java.util.List<hydra.core.Term<M>>, hydra.evaluation.Flow<hydra.evaluation.Context<M>, hydra.core.Term<M>>> implementation;
  
  public PrimitiveFunction (hydra.core.Name name, hydra.core.FunctionType<M> type, java.util.function.Function<java.util.List<hydra.core.Term<M>>, hydra.evaluation.Flow<hydra.evaluation.Context<M>, hydra.core.Term<M>>> implementation) {
    this.name = name;
    this.type = type;
    this.implementation = implementation;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrimitiveFunction)) {
      return false;
    }
    PrimitiveFunction o = (PrimitiveFunction) (other);
    return name.equals(o.name) && type.equals(o.type) && implementation.equals(o.implementation);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * type.hashCode() + 5 * implementation.hashCode();
  }
  
  public PrimitiveFunction withName(hydra.core.Name name) {
    return new PrimitiveFunction(name, type, implementation);
  }
  
  public PrimitiveFunction withType(hydra.core.FunctionType<M> type) {
    return new PrimitiveFunction(name, type, implementation);
  }
  
  public PrimitiveFunction withImplementation(java.util.function.Function<java.util.List<hydra.core.Term<M>>, hydra.evaluation.Flow<hydra.evaluation.Context<M>, hydra.core.Term<M>>> implementation) {
    return new PrimitiveFunction(name, type, implementation);
  }
}