// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public class ApplicationQuery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.ApplicationQuery");
  
  public final java.util.List<hydra.langs.tinkerpop.queries.Query> value;
  
  public ApplicationQuery (java.util.List<hydra.langs.tinkerpop.queries.Query> value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ApplicationQuery)) {
      return false;
    }
    ApplicationQuery o = (ApplicationQuery) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}