// Note: this is an automatically generated file. Do not edit.

package hydra.paths;

import java.io.Serializable;

/**
 * A sequence of subterm steps forming a path through a term
 */
public class SubtermPath implements Serializable, Comparable<SubtermPath> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.paths.SubtermPath");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.util.ConsList<hydra.paths.SubtermStep> value;

  public SubtermPath (hydra.util.ConsList<hydra.paths.SubtermStep> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubtermPath)) {
      return false;
    }
    SubtermPath o = (SubtermPath) other;
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
  public int compareTo(SubtermPath other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
