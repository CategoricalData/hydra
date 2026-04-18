// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Reference to a property graph
 */
public class GraphReference implements Serializable, Comparable<GraphReference> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.GraphReference");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public GraphReference (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GraphReference)) {
      return false;
    }
    GraphReference o = (GraphReference) other;
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
  public int compareTo(GraphReference other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
