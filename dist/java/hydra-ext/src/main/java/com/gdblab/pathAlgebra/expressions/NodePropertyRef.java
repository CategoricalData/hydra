// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Reference to a node property: node.property
 */
public class NodePropertyRef implements Serializable, Comparable<NodePropertyRef> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.NodePropertyRef");

  public static final hydra.core.Name ELEMENT = new hydra.core.Name("element");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public final com.gdblab.pathAlgebra.expressions.PathElement element;

  public final String property;

  public NodePropertyRef (com.gdblab.pathAlgebra.expressions.PathElement element, String property) {
    this.element = element;
    this.property = property;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NodePropertyRef)) {
      return false;
    }
    NodePropertyRef o = (NodePropertyRef) other;
    return java.util.Objects.equals(
      this.element,
      o.element) && java.util.Objects.equals(
      this.property,
      o.property);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(element) + 3 * java.util.Objects.hashCode(property);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NodePropertyRef other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      element,
      other.element);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      property,
      other.property);
  }

  public NodePropertyRef withElement(com.gdblab.pathAlgebra.expressions.PathElement element) {
    return new NodePropertyRef(element, property);
  }

  public NodePropertyRef withProperty(String property) {
    return new NodePropertyRef(element, property);
  }
}
