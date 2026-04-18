// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public class UnivAnnot implements Serializable, Comparable<UnivAnnot> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.UnivAnnot");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<hydra.coq.syntax.UniverseLevel> value;

  public UnivAnnot (java.util.List<hydra.coq.syntax.UniverseLevel> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnivAnnot)) {
      return false;
    }
    UnivAnnot o = (UnivAnnot) other;
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
  public int compareTo(UnivAnnot other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
