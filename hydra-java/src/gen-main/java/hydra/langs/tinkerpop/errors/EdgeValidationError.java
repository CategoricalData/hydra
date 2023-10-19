package hydra.langs.tinkerpop.errors;

import java.io.Serializable;

public class EdgeValidationError<T, V> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/errors.EdgeValidationError");
  
  /**
   * The id of the edge which failed validation
   */
  public final V id;
  
  /**
   * A specific validation error for the edge
   */
  public final hydra.langs.tinkerpop.errors.BadEdge<T, V> error;
  
  public EdgeValidationError (V id, hydra.langs.tinkerpop.errors.BadEdge<T, V> error) {
    this.id = id;
    this.error = error;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeValidationError)) {
      return false;
    }
    EdgeValidationError o = (EdgeValidationError) (other);
    return id.equals(o.id) && error.equals(o.error);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * error.hashCode();
  }
  
  public EdgeValidationError withId(V id) {
    return new EdgeValidationError(id, error);
  }
  
  public EdgeValidationError withError(hydra.langs.tinkerpop.errors.BadEdge<T, V> error) {
    return new EdgeValidationError(id, error);
  }
}