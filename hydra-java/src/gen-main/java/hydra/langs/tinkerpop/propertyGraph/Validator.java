package hydra.langs.tinkerpop.propertyGraph;

/**
 * A function which checks a value for conformance with a type
 */
public class Validator<T, V> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/propertyGraph.Validator");
  
  public final java.util.function.Function<T, java.util.function.Function<V, Boolean>> value;
  
  public Validator (java.util.function.Function<T, java.util.function.Function<V, Boolean>> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Validator)) {
      return false;
    }
    Validator o = (Validator) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}