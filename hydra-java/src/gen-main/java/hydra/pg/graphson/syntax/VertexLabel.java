// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public class VertexLabel implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.graphson.syntax.VertexLabel");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public VertexLabel (String value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexLabel)) {
      return false;
    }
    VertexLabel o = (VertexLabel) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}