// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class MaybeSequencePattern implements Serializable, Comparable<MaybeSequencePattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.MaybeSequencePattern");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<hydra.python.syntax.MaybeStarPattern> value;

  public MaybeSequencePattern (java.util.List<hydra.python.syntax.MaybeStarPattern> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MaybeSequencePattern)) {
      return false;
    }
    MaybeSequencePattern o = (MaybeSequencePattern) other;
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
  public int compareTo(MaybeSequencePattern other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
