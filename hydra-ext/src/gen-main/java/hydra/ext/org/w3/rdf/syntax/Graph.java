// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.rdf.syntax;

import java.io.Serializable;

public class Graph implements Serializable, Comparable<Graph> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.rdf.syntax.Graph");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public final hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Triple> value;
  
  public Graph (hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Triple> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Graph)) {
      return false;
    }
    Graph o = (Graph) other;
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
  public int compareTo(Graph other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
