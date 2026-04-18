// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class PathElementListStep implements Serializable, Comparable<PathElementListStep> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PathElementListStep");

  public static final hydra.core.Name EDGE_REFERENCE = new hydra.core.Name("edgeReference");

  public static final hydra.core.Name NODE_REFERENCE = new hydra.core.Name("nodeReference");

  public final openGql.grammar.PrimaryValueExpression edgeReference;

  public final openGql.grammar.PrimaryValueExpression nodeReference;

  public PathElementListStep (openGql.grammar.PrimaryValueExpression edgeReference, openGql.grammar.PrimaryValueExpression nodeReference) {
    this.edgeReference = edgeReference;
    this.nodeReference = nodeReference;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PathElementListStep)) {
      return false;
    }
    PathElementListStep o = (PathElementListStep) other;
    return java.util.Objects.equals(
      this.edgeReference,
      o.edgeReference) && java.util.Objects.equals(
      this.nodeReference,
      o.nodeReference);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(edgeReference) + 3 * java.util.Objects.hashCode(nodeReference);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PathElementListStep other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      edgeReference,
      other.edgeReference);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      nodeReference,
      other.nodeReference);
  }

  public PathElementListStep withEdgeReference(openGql.grammar.PrimaryValueExpression edgeReference) {
    return new PathElementListStep(edgeReference, nodeReference);
  }

  public PathElementListStep withNodeReference(openGql.grammar.PrimaryValueExpression nodeReference) {
    return new PathElementListStep(edgeReference, nodeReference);
  }
}
