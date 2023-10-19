package hydra.langs.tinkerpop.errors;

import java.io.Serializable;

public class VertexValidationError<T, V> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/errors.VertexValidationError");
  
  /**
   * The id of the vertex which failed validation
   */
  public final V id;
  
  /**
   * A specific validation error for the vertex
   */
  public final hydra.langs.tinkerpop.errors.BadVertex<T, V> error;
  
  public VertexValidationError (V id, hydra.langs.tinkerpop.errors.BadVertex<T, V> error) {
    this.id = id;
    this.error = error;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexValidationError)) {
      return false;
    }
    VertexValidationError o = (VertexValidationError) (other);
    return id.equals(o.id) && error.equals(o.error);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * error.hashCode();
  }
  
  public VertexValidationError withId(V id) {
    return new VertexValidationError(id, error);
  }
  
  public VertexValidationError withError(hydra.langs.tinkerpop.errors.BadVertex<T, V> error) {
    return new VertexValidationError(id, error);
  }
}