// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public class VertexPropertyValue implements Serializable, Comparable<VertexPropertyValue> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.graphson.syntax.VertexPropertyValue");
  
  public static final hydra.core.Name ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.util.Maybe<hydra.pg.graphson.syntax.Value> id;
  
  public final hydra.pg.graphson.syntax.Value value;
  
  public VertexPropertyValue (hydra.util.Maybe<hydra.pg.graphson.syntax.Value> id, hydra.pg.graphson.syntax.Value value) {
    this.id = id;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexPropertyValue)) {
      return false;
    }
    VertexPropertyValue o = (VertexPropertyValue) other;
    return java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(id) + 3 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VertexPropertyValue other) {
    int cmp = 0;
    cmp = Integer.compare(
      id.hashCode(),
      other.id.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) value).compareTo(other.value);
  }
  
  public VertexPropertyValue withId(hydra.util.Maybe<hydra.pg.graphson.syntax.Value> id) {
    return new VertexPropertyValue(id, value);
  }
  
  public VertexPropertyValue withValue(hydra.pg.graphson.syntax.Value value) {
    return new VertexPropertyValue(id, value);
  }
}
