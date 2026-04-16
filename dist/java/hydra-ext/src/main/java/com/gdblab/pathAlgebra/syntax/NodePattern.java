// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public class NodePattern implements Serializable, Comparable<NodePattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.NodePattern");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public final hydra.util.Maybe<String> variable;

  public NodePattern (hydra.util.Maybe<String> variable) {
    this.variable = variable;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodePattern)) {
      return false;
    }
    NodePattern o = (NodePattern) other;
    return java.util.Objects.equals(
      this.variable,
      o.variable);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variable);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NodePattern other) {
    return hydra.util.Comparing.compare(
      variable,
      other.variable);
  }
}
