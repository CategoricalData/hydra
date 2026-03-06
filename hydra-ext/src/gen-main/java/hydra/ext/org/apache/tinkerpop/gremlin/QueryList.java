// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class QueryList implements Serializable, Comparable<QueryList> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.QueryList");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.Query> value;
  
  public QueryList (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.Query> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QueryList)) {
      return false;
    }
    QueryList o = (QueryList) other;
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
  public int compareTo(QueryList other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
