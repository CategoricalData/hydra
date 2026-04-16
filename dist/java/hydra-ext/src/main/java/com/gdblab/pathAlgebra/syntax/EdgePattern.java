// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public class EdgePattern implements Serializable, Comparable<EdgePattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.EdgePattern");

  public static final hydra.core.Name DIRECTION = new hydra.core.Name("direction");

  public static final hydra.core.Name RPQ = new hydra.core.Name("rpq");

  public final com.gdblab.pathAlgebra.syntax.EdgeDirection direction;

  public final hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.Rpq> rpq;

  public EdgePattern (com.gdblab.pathAlgebra.syntax.EdgeDirection direction, hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.Rpq> rpq) {
    this.direction = direction;
    this.rpq = rpq;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgePattern)) {
      return false;
    }
    EdgePattern o = (EdgePattern) other;
    return java.util.Objects.equals(
      this.direction,
      o.direction) && java.util.Objects.equals(
      this.rpq,
      o.rpq);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(direction) + 3 * java.util.Objects.hashCode(rpq);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EdgePattern other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      direction,
      other.direction);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      rpq,
      other.rpq);
  }

  public EdgePattern withDirection(com.gdblab.pathAlgebra.syntax.EdgeDirection direction) {
    return new EdgePattern(direction, rpq);
  }

  public EdgePattern withRpq(hydra.util.Maybe<com.gdblab.pathAlgebra.syntax.Rpq> rpq) {
    return new EdgePattern(direction, rpq);
  }
}
