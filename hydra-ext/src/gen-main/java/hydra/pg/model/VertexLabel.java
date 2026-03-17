// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * The label of a vertex. The default (null) vertex is represented by the empty string
 */
public class VertexLabel implements Serializable, Comparable<VertexLabel> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.VertexLabel");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public VertexLabel (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexLabel)) {
      return false;
    }
    VertexLabel o = (VertexLabel) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VertexLabel other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
