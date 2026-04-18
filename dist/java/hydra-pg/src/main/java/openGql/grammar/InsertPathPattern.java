// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class InsertPathPattern implements Serializable, Comparable<InsertPathPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.InsertPathPattern");

  public static final hydra.core.Name START_NODE = new hydra.core.Name("startNode");

  public static final hydra.core.Name EDGES_AND_NODES = new hydra.core.Name("edgesAndNodes");

  public final hydra.util.Maybe<openGql.grammar.InsertElementPatternFiller> startNode;

  public final java.util.List<openGql.grammar.InsertEdgeAndNode> edgesAndNodes;

  public InsertPathPattern (hydra.util.Maybe<openGql.grammar.InsertElementPatternFiller> startNode, java.util.List<openGql.grammar.InsertEdgeAndNode> edgesAndNodes) {
    this.startNode = startNode;
    this.edgesAndNodes = edgesAndNodes;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InsertPathPattern)) {
      return false;
    }
    InsertPathPattern o = (InsertPathPattern) other;
    return java.util.Objects.equals(
      this.startNode,
      o.startNode) && java.util.Objects.equals(
      this.edgesAndNodes,
      o.edgesAndNodes);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(startNode) + 3 * java.util.Objects.hashCode(edgesAndNodes);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InsertPathPattern other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      startNode,
      other.startNode);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      edgesAndNodes,
      other.edgesAndNodes);
  }

  public InsertPathPattern withStartNode(hydra.util.Maybe<openGql.grammar.InsertElementPatternFiller> startNode) {
    return new InsertPathPattern(startNode, edgesAndNodes);
  }

  public InsertPathPattern withEdgesAndNodes(java.util.List<openGql.grammar.InsertEdgeAndNode> edgesAndNodes) {
    return new InsertPathPattern(startNode, edgesAndNodes);
  }
}
