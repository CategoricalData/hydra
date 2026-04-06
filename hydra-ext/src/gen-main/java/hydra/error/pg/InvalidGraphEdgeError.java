// Note: this is an automatically generated file. Do not edit.

package hydra.error.pg;

import java.io.Serializable;

/**
 * An invalid edge within a graph, identified by its id
 */
public class InvalidGraphEdgeError<V> implements Serializable, Comparable<InvalidGraphEdgeError<V>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.pg.InvalidGraphEdgeError");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name ERROR = new hydra.core.Name("error");

  /**
   * The id of the invalid edge
   */
  public final V id;

  /**
   * The specific error
   */
  public final hydra.error.pg.InvalidEdgeError error;

  public InvalidGraphEdgeError (V id, hydra.error.pg.InvalidEdgeError error) {
    this.id = id;
    this.error = error;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InvalidGraphEdgeError)) {
      return false;
    }
    InvalidGraphEdgeError o = (InvalidGraphEdgeError) other;
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
  public int compareTo(InvalidGraphEdgeError other) {
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

  public InvalidGraphEdgeError withId(V id) {
    return new InvalidGraphEdgeError(id, error);
  }

  public InvalidGraphEdgeError withError(hydra.error.pg.InvalidEdgeError error) {
    return new InvalidGraphEdgeError(id, error);
  }
}
