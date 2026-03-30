// Note: this is an automatically generated file. Do not edit.

package hydra.error.pg;

import java.io.Serializable;

/**
 * An error indicating that a vertex label does not exist in the schema
 */
public class NoSuchVertexLabelError implements Serializable, Comparable<NoSuchVertexLabelError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.pg.NoSuchVertexLabelError");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  /**
   * The vertex label that was not found
   */
  public final hydra.pg.model.VertexLabel label;

  public NoSuchVertexLabelError (hydra.pg.model.VertexLabel label) {
    this.label = label;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NoSuchVertexLabelError)) {
      return false;
    }
    NoSuchVertexLabelError o = (NoSuchVertexLabelError) other;
    return java.util.Objects.equals(
      this.label,
      o.label);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(label);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NoSuchVertexLabelError other) {
    return ((Comparable) label).compareTo(other.label);
  }
}
