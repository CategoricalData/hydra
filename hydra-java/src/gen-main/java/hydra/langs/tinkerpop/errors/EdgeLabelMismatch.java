package hydra.langs.tinkerpop.errors;

import java.io.Serializable;

public class EdgeLabelMismatch implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/errors.EdgeLabelMismatch");
  
  /**
   * The expected edge label, based on the edge type
   */
  public final hydra.langs.tinkerpop.propertyGraph.EdgeLabel expected;
  
  /**
   * The actual edge label
   */
  public final hydra.langs.tinkerpop.propertyGraph.EdgeLabel actual;
  
  public EdgeLabelMismatch (hydra.langs.tinkerpop.propertyGraph.EdgeLabel expected, hydra.langs.tinkerpop.propertyGraph.EdgeLabel actual) {
    this.expected = expected;
    this.actual = actual;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeLabelMismatch)) {
      return false;
    }
    EdgeLabelMismatch o = (EdgeLabelMismatch) (other);
    return expected.equals(o.expected) && actual.equals(o.actual);
  }
  
  @Override
  public int hashCode() {
    return 2 * expected.hashCode() + 3 * actual.hashCode();
  }
  
  public EdgeLabelMismatch withExpected(hydra.langs.tinkerpop.propertyGraph.EdgeLabel expected) {
    return new EdgeLabelMismatch(expected, actual);
  }
  
  public EdgeLabelMismatch withActual(hydra.langs.tinkerpop.propertyGraph.EdgeLabel actual) {
    return new EdgeLabelMismatch(expected, actual);
  }
}