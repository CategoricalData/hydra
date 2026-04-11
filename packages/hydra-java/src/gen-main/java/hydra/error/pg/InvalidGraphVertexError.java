// Note: this is an automatically generated file. Do not edit.

package hydra.error.pg;

import java.io.Serializable;

/**
 * An invalid vertex within a graph, identified by its id
 */
public class InvalidGraphVertexError<V> implements Serializable, Comparable<InvalidGraphVertexError<V>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.pg.InvalidGraphVertexError");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name ERROR = new hydra.core.Name("error");

  /**
   * The id of the invalid vertex
   */
  public final V id;

  /**
   * The specific error
   */
  public final hydra.error.pg.InvalidVertexError error;

  public InvalidGraphVertexError (V id, hydra.error.pg.InvalidVertexError error) {
    this.id = id;
    this.error = error;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InvalidGraphVertexError)) {
      return false;
    }
    InvalidGraphVertexError o = (InvalidGraphVertexError) other;
    return java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.error,
      o.error);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(id) + 3 * java.util.Objects.hashCode(error);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InvalidGraphVertexError other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      id,
      other.id);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      error,
      other.error);
  }

  public InvalidGraphVertexError withId(V id) {
    return new InvalidGraphVertexError(id, error);
  }

  public InvalidGraphVertexError withError(hydra.error.pg.InvalidVertexError error) {
    return new InvalidGraphVertexError(id, error);
  }
}
