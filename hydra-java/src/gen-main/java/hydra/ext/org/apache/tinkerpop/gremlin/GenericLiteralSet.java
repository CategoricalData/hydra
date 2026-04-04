// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class GenericLiteralSet implements Serializable, Comparable<GenericLiteralSet> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralSet");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral> value;

  public GenericLiteralSet (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenericLiteralSet)) {
      return false;
    }
    GenericLiteralSet o = (GenericLiteralSet) other;
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
  public int compareTo(GenericLiteralSet other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
