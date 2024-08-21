// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.queries;

import java.io.Serializable;

public class ApplicationQuery implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/queries.ApplicationQuery");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.org.apache.tinkerpop.queries.Query> value;
  
  public ApplicationQuery (java.util.List<hydra.ext.org.apache.tinkerpop.queries.Query> value) {
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