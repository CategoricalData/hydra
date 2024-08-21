// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class ApplicationQuery implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/pg/query.ApplicationQuery");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.pg.query.Query> value;
  
  public ApplicationQuery (java.util.List<hydra.pg.query.Query> value) {
    java.util.Objects.requireNonNull((value));
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