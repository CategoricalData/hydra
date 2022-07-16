package hydra.ext.rdf.syntax;

public class Graph {
  public final java.util.Set<Triple> value;
  
  public Graph (java.util.Set<Triple> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Graph)) {
      return false;
    }
    Graph o = (Graph) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}