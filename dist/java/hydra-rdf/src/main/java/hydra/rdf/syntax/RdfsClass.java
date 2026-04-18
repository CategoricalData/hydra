// Note: this is an automatically generated file. Do not edit.

package hydra.rdf.syntax;

import java.io.Serializable;

/**
 * Stand-in for rdfs:Class
 */
public class RdfsClass implements Serializable, Comparable<RdfsClass> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.rdf.syntax.RdfsClass");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.lang.Void value;

  public RdfsClass (java.lang.Void value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RdfsClass)) {
      return false;
    }
    RdfsClass o = (RdfsClass) other;
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
  public int compareTo(RdfsClass other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
