// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Projection operator: π_(#P,#G,#A)(solutionSpace)
 */
public class ProjectionExpression implements Serializable, Comparable<ProjectionExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.ProjectionExpression");

  public static final hydra.core.Name PARTITIONS = new hydra.core.Name("partitions");

  public static final hydra.core.Name GROUPS = new hydra.core.Name("groups");

  public static final hydra.core.Name PATHS = new hydra.core.Name("paths");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public final com.gdblab.pathAlgebra.expressions.ProjectionSpec partitions;

  public final com.gdblab.pathAlgebra.expressions.ProjectionSpec groups;

  public final com.gdblab.pathAlgebra.expressions.ProjectionSpec paths;

  public final com.gdblab.pathAlgebra.expressions.SolutionSpaceExpression expression;

  public ProjectionExpression (com.gdblab.pathAlgebra.expressions.ProjectionSpec partitions, com.gdblab.pathAlgebra.expressions.ProjectionSpec groups, com.gdblab.pathAlgebra.expressions.ProjectionSpec paths, com.gdblab.pathAlgebra.expressions.SolutionSpaceExpression expression) {
    this.partitions = partitions;
    this.groups = groups;
    this.paths = paths;
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ProjectionExpression)) {
      return false;
    }
    ProjectionExpression o = (ProjectionExpression) other;
    return java.util.Objects.equals(
      this.partitions,
      o.partitions) && java.util.Objects.equals(
      this.groups,
      o.groups) && java.util.Objects.equals(
      this.paths,
      o.paths) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(partitions) + 3 * java.util.Objects.hashCode(groups) + 5 * java.util.Objects.hashCode(paths) + 7 * java.util.Objects.hashCode(expression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ProjectionExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      partitions,
      other.partitions);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      groups,
      other.groups);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      paths,
      other.paths);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expression,
      other.expression);
  }

  public ProjectionExpression withPartitions(com.gdblab.pathAlgebra.expressions.ProjectionSpec partitions) {
    return new ProjectionExpression(partitions, groups, paths, expression);
  }

  public ProjectionExpression withGroups(com.gdblab.pathAlgebra.expressions.ProjectionSpec groups) {
    return new ProjectionExpression(partitions, groups, paths, expression);
  }

  public ProjectionExpression withPaths(com.gdblab.pathAlgebra.expressions.ProjectionSpec paths) {
    return new ProjectionExpression(partitions, groups, paths, expression);
  }

  public ProjectionExpression withExpression(com.gdblab.pathAlgebra.expressions.SolutionSpaceExpression expression) {
    return new ProjectionExpression(partitions, groups, paths, expression);
  }
}
