// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public class PathPattern implements Serializable, Comparable<PathPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.PathPattern");

  public static final hydra.core.Name PATH_NAME = new hydra.core.Name("pathName");

  public static final hydra.core.Name START_NODE = new hydra.core.Name("startNode");

  public static final hydra.core.Name EDGE = new hydra.core.Name("edge");

  public static final hydra.core.Name END_NODE = new hydra.core.Name("endNode");

  public static final hydra.core.Name CONDITION = new hydra.core.Name("condition");

  public final String pathName;

  public final com.gdblab.pathAlgebra.syntax.NodePattern startNode;

  public final com.gdblab.pathAlgebra.syntax.EdgePattern edge;

  public final com.gdblab.pathAlgebra.syntax.NodePattern endNode;

  public final hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.ComplexCondition> condition;

  public PathPattern (String pathName, com.gdblab.pathAlgebra.syntax.NodePattern startNode, com.gdblab.pathAlgebra.syntax.EdgePattern edge, com.gdblab.pathAlgebra.syntax.NodePattern endNode, hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.ComplexCondition> condition) {
    this.pathName = pathName;
    this.startNode = startNode;
    this.edge = edge;
    this.endNode = endNode;
    this.condition = condition;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PathPattern)) {
      return false;
    }
    PathPattern o = (PathPattern) other;
    return java.util.Objects.equals(
      this.pathName,
      o.pathName) && java.util.Objects.equals(
      this.startNode,
      o.startNode) && java.util.Objects.equals(
      this.edge,
      o.edge) && java.util.Objects.equals(
      this.endNode,
      o.endNode) && java.util.Objects.equals(
      this.condition,
      o.condition);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pathName) + 3 * java.util.Objects.hashCode(startNode) + 5 * java.util.Objects.hashCode(edge) + 7 * java.util.Objects.hashCode(endNode) + 11 * java.util.Objects.hashCode(condition);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PathPattern other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      pathName,
      other.pathName);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      startNode,
      other.startNode);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      edge,
      other.edge);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      endNode,
      other.endNode);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      condition,
      other.condition);
  }

  public PathPattern withPathName(String pathName) {
    return new PathPattern(pathName, startNode, edge, endNode, condition);
  }

  public PathPattern withStartNode(com.gdblab.pathAlgebra.syntax.NodePattern startNode) {
    return new PathPattern(pathName, startNode, edge, endNode, condition);
  }

  public PathPattern withEdge(com.gdblab.pathAlgebra.syntax.EdgePattern edge) {
    return new PathPattern(pathName, startNode, edge, endNode, condition);
  }

  public PathPattern withEndNode(com.gdblab.pathAlgebra.syntax.NodePattern endNode) {
    return new PathPattern(pathName, startNode, edge, endNode, condition);
  }

  public PathPattern withCondition(hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.ComplexCondition> condition) {
    return new PathPattern(pathName, startNode, edge, endNode, condition);
  }
}
