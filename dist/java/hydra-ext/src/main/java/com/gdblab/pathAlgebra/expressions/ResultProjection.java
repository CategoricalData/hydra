// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Extract specific values from paths for RETURN clause
 */
public class ResultProjection implements Serializable, Comparable<ResultProjection> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.ResultProjection");

  public static final hydra.core.Name PROJECTIONS = new hydra.core.Name("projections");

  public final java.util.List<com.gdblab.pathAlgebra.expressions.PropertyExtraction> projections;

  public ResultProjection (java.util.List<com.gdblab.pathAlgebra.expressions.PropertyExtraction> projections) {
    this.projections = projections;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ResultProjection)) {
      return false;
    }
    ResultProjection o = (ResultProjection) other;
    return java.util.Objects.equals(
      this.projections,
      o.projections);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(projections);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ResultProjection other) {
    return hydra.util.Comparing.compare(
      projections,
      other.projections);
  }
}
