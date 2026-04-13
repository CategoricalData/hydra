// Note: this is an automatically generated file. Do not edit.

package hydra.rdf.syntax;

import java.io.Serializable;

public class Dataset implements Serializable, Comparable<Dataset> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.rdf.syntax.Dataset");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.Set<hydra.rdf.syntax.Quad> value;

  public Dataset (java.util.Set<hydra.rdf.syntax.Quad> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Dataset)) {
      return false;
    }
    Dataset o = (Dataset) other;
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
  public int compareTo(Dataset other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
