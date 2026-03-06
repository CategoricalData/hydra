// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class DateLiteral implements Serializable, Comparable<DateLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.DateLiteral");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> value;
  
  public DateLiteral (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DateLiteral)) {
      return false;
    }
    DateLiteral o = (DateLiteral) other;
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
  public int compareTo(DateLiteral other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
