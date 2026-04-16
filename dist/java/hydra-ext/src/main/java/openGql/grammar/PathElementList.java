// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class PathElementList implements Serializable, Comparable<PathElementList> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PathElementList");

  public static final hydra.core.Name START = new hydra.core.Name("start");

  public static final hydra.core.Name STEPS = new hydra.core.Name("steps");

  public final openGql.grammar.PrimaryValueExpression start;

  public final java.util.List<openGql.grammar.PathElementListStep> steps;

  public PathElementList (openGql.grammar.PrimaryValueExpression start, java.util.List<openGql.grammar.PathElementListStep> steps) {
    this.start = start;
    this.steps = steps;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PathElementList)) {
      return false;
    }
    PathElementList o = (PathElementList) other;
    return java.util.Objects.equals(
      this.start,
      o.start) && java.util.Objects.equals(
      this.steps,
      o.steps);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(start) + 3 * java.util.Objects.hashCode(steps);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PathElementList other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      start,
      other.start);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      steps,
      other.steps);
  }

  public PathElementList withStart(openGql.grammar.PrimaryValueExpression start) {
    return new PathElementList(start, steps);
  }

  public PathElementList withSteps(java.util.List<openGql.grammar.PathElementListStep> steps) {
    return new PathElementList(start, steps);
  }
}
