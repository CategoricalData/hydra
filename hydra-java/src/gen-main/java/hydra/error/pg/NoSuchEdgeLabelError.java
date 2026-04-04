// Note: this is an automatically generated file. Do not edit.

package hydra.error.pg;

import java.io.Serializable;

/**
 * An error indicating that an edge label does not exist in the schema
 */
public class NoSuchEdgeLabelError implements Serializable, Comparable<NoSuchEdgeLabelError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.pg.NoSuchEdgeLabelError");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  /**
   * The edge label that was not found
   */
  public final hydra.pg.model.EdgeLabel label;

  public NoSuchEdgeLabelError (hydra.pg.model.EdgeLabel label) {
    this.label = label;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NoSuchEdgeLabelError)) {
      return false;
    }
    NoSuchEdgeLabelError o = (NoSuchEdgeLabelError) other;
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
  public int compareTo(NoSuchEdgeLabelError other) {
    return hydra.util.Comparing.compare(
      label,
      other.label);
  }
}
