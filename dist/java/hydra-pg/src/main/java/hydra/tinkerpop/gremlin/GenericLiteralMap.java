// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class GenericLiteralMap implements Serializable, Comparable<GenericLiteralMap> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.GenericLiteralMap");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<hydra.tinkerpop.gremlin.MapEntry> value;

  public GenericLiteralMap (java.util.List<hydra.tinkerpop.gremlin.MapEntry> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenericLiteralMap)) {
      return false;
    }
    GenericLiteralMap o = (GenericLiteralMap) other;
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
  public int compareTo(GenericLiteralMap other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
