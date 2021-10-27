package hydra.graph;

/**
 * A unique identifier for a graph within a graph set
 * 
 * @type string
 */
public class GraphName {
  public final String value;
  
  /**
   * Constructs an immutable GraphName object
   */
  public GraphName(String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GraphName)) {
        return false;
    }
    GraphName o = (GraphName) other;
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
