// Note: this is an automatically generated file. Do not edit.

package hydra.graphviz.dot;

import java.io.Serializable;

public class Port implements Serializable, Comparable<Port> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.graphviz.dot.Port");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name POSITION = new hydra.core.Name("position");

  public final hydra.util.Maybe<hydra.graphviz.dot.Id> id;

  public final hydra.util.Maybe<hydra.graphviz.dot.CompassPt> position;

  public Port (hydra.util.Maybe<hydra.graphviz.dot.Id> id, hydra.util.Maybe<hydra.graphviz.dot.CompassPt> position) {
    this.id = id;
    this.position = position;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Port)) {
      return false;
    }
    Port o = (Port) other;
    return java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.position,
      o.position);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(id) + 3 * java.util.Objects.hashCode(position);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Port other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      id,
      other.id);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      position,
      other.position);
  }

  public Port withId(hydra.util.Maybe<hydra.graphviz.dot.Id> id) {
    return new Port(id, position);
  }

  public Port withPosition(hydra.util.Maybe<hydra.graphviz.dot.CompassPt> position) {
    return new Port(id, position);
  }
}
