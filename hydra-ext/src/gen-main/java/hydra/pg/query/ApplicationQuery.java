// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class ApplicationQuery implements Serializable, Comparable<ApplicationQuery> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.query.ApplicationQuery");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.pg.query.Query> value;
  
  public ApplicationQuery (java.util.List<hydra.pg.query.Query> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ApplicationQuery)) {
      return false;
    }
    ApplicationQuery o = (ApplicationQuery) other;
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
  public int compareTo(ApplicationQuery other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
