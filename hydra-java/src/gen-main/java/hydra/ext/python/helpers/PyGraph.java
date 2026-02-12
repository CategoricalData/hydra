// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.helpers;

import java.io.Serializable;

/**
 * Combined graph and metadata state for Python code generation
 */
public class PyGraph implements Serializable, Comparable<PyGraph> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.helpers.PyGraph");
  
  public static final hydra.core.Name FIELD_NAME_GRAPH = new hydra.core.Name("graph");
  
  public static final hydra.core.Name FIELD_NAME_METADATA = new hydra.core.Name("metadata");
  
  /**
   * The Hydra graph being processed
   */
  public final hydra.graph.Graph graph;
  
  /**
   * Accumulated module metadata
   */
  public final hydra.ext.python.helpers.PythonModuleMetadata metadata;
  
  public PyGraph (hydra.graph.Graph graph, hydra.ext.python.helpers.PythonModuleMetadata metadata) {
    this.graph = graph;
    this.metadata = metadata;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PyGraph)) {
      return false;
    }
    PyGraph o = (PyGraph) other;
    return java.util.Objects.equals(
      this.graph,
      o.graph) && java.util.Objects.equals(
      this.metadata,
      o.metadata);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(graph) + 3 * java.util.Objects.hashCode(metadata);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PyGraph other) {
    int cmp = 0;
    cmp = ((Comparable) graph).compareTo(other.graph);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) metadata).compareTo(other.metadata);
  }
  
  public PyGraph withGraph(hydra.graph.Graph graph) {
    return new PyGraph(graph, metadata);
  }
  
  public PyGraph withMetadata(hydra.ext.python.helpers.PythonModuleMetadata metadata) {
    return new PyGraph(graph, metadata);
  }
}
