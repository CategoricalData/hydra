// Note: this is an automatically generated file. Do not edit.

package hydra.paths;

import java.io.Serializable;

/**
 * A sequence of subtype steps forming a path through a type
 */
public class SubtypePath implements Serializable, Comparable<SubtypePath> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.paths.SubtypePath");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.util.ConsList<hydra.paths.SubtypeStep> value;

  public SubtypePath (hydra.util.ConsList<hydra.paths.SubtypeStep> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubtypePath)) {
      return false;
    }
    SubtypePath o = (SubtypePath) other;
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
  public int compareTo(SubtypePath other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
