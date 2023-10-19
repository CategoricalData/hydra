package hydra.langs.tinkerpop.errors;

import java.io.Serializable;

public class VertexLabelMismatch implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/errors.VertexLabelMismatch");
  
  /**
   * The expected vertex label, based on the vertex type
   */
  public final hydra.langs.tinkerpop.propertyGraph.VertexLabel expected;
  
  /**
   * The actual vertex label
   */
  public final hydra.langs.tinkerpop.propertyGraph.VertexLabel actual;
  
  public VertexLabelMismatch (hydra.langs.tinkerpop.propertyGraph.VertexLabel expected, hydra.langs.tinkerpop.propertyGraph.VertexLabel actual) {
    this.expected = expected;
    this.actual = actual;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VertexLabelMismatch)) {
      return false;
    }
    VertexLabelMismatch o = (VertexLabelMismatch) (other);
    return expected.equals(o.expected) && actual.equals(o.actual);
  }
  
  @Override
  public int hashCode() {
    return 2 * expected.hashCode() + 3 * actual.hashCode();
  }
  
  public VertexLabelMismatch withExpected(hydra.langs.tinkerpop.propertyGraph.VertexLabel expected) {
    return new VertexLabelMismatch(expected, actual);
  }
  
  public VertexLabelMismatch withActual(hydra.langs.tinkerpop.propertyGraph.VertexLabel actual) {
    return new VertexLabelMismatch(expected, actual);
  }
}