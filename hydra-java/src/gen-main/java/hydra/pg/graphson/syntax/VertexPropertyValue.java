// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public class VertexPropertyValue implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.graphson.syntax.VertexPropertyValue");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.util.Opt<hydra.pg.graphson.syntax.Value> id;
  
  public final hydra.pg.graphson.syntax.Value value;
  
  public VertexPropertyValue (hydra.util.Opt<hydra.pg.graphson.syntax.Value> id, hydra.pg.graphson.syntax.Value value) {
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((value));
    this.id = id;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexPropertyValue)) {
      return false;
    }
    VertexPropertyValue o = (VertexPropertyValue) (other);
    return id.equals(o.id) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * value.hashCode();
  }
  
  public VertexPropertyValue withId(hydra.util.Opt<hydra.pg.graphson.syntax.Value> id) {
    java.util.Objects.requireNonNull((id));
    return new VertexPropertyValue(id, value);
  }
  
  public VertexPropertyValue withValue(hydra.pg.graphson.syntax.Value value) {
    java.util.Objects.requireNonNull((value));
    return new VertexPropertyValue(id, value);
  }
}