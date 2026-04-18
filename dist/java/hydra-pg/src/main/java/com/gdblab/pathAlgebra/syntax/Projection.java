// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public class Projection implements Serializable, Comparable<Projection> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.Projection");

  public static final hydra.core.Name PART_PROJ = new hydra.core.Name("partProj");

  public static final hydra.core.Name GROUP_PROJ = new hydra.core.Name("groupProj");

  public static final hydra.core.Name PATH_PROJ = new hydra.core.Name("pathProj");

  public final com.gdblab.pathAlgebra.syntax.PartProj partProj;

  public final com.gdblab.pathAlgebra.syntax.GroupProj groupProj;

  public final com.gdblab.pathAlgebra.syntax.PathProj pathProj;

  public Projection (com.gdblab.pathAlgebra.syntax.PartProj partProj, com.gdblab.pathAlgebra.syntax.GroupProj groupProj, com.gdblab.pathAlgebra.syntax.PathProj pathProj) {
    this.partProj = partProj;
    this.groupProj = groupProj;
    this.pathProj = pathProj;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Projection)) {
      return false;
    }
    Projection o = (Projection) other;
    return java.util.Objects.equals(
      this.partProj,
      o.partProj) && java.util.Objects.equals(
      this.groupProj,
      o.groupProj) && java.util.Objects.equals(
      this.pathProj,
      o.pathProj);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(partProj) + 3 * java.util.Objects.hashCode(groupProj) + 5 * java.util.Objects.hashCode(pathProj);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Projection other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      partProj,
      other.partProj);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      groupProj,
      other.groupProj);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      pathProj,
      other.pathProj);
  }

  public Projection withPartProj(com.gdblab.pathAlgebra.syntax.PartProj partProj) {
    return new Projection(partProj, groupProj, pathProj);
  }

  public Projection withGroupProj(com.gdblab.pathAlgebra.syntax.GroupProj groupProj) {
    return new Projection(partProj, groupProj, pathProj);
  }

  public Projection withPathProj(com.gdblab.pathAlgebra.syntax.PathProj pathProj) {
    return new Projection(partProj, groupProj, pathProj);
  }
}
