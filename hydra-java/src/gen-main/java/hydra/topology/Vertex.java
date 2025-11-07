// Note: this is an automatically generated file. Do not edit.

package hydra.topology;

/**
 * A graph vertex, represented as a 32-bit integer identifier
 */
public class Vertex {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.topology.Vertex");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final Integer value;
  
  public Vertex (Integer value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Vertex)) {
      return false;
    }
    Vertex o = (Vertex) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
