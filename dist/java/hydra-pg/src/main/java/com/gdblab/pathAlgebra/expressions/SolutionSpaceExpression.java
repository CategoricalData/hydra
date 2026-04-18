// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Expressions that work with solution spaces
 */
public abstract class SolutionSpaceExpression implements Serializable, Comparable<SolutionSpaceExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.SolutionSpaceExpression");

  public static final hydra.core.Name GROUP_BY = new hydra.core.Name("groupBy");

  public static final hydra.core.Name ORDER_BY = new hydra.core.Name("orderBy");

  private SolutionSpaceExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(GroupBy instance) ;

    R visit(OrderBy instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SolutionSpaceExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(GroupBy instance) {
      return otherwise(instance);
    }

    default R visit(OrderBy instance) {
      return otherwise(instance);
    }
  }

  public static final class GroupBy extends com.gdblab.pathAlgebra.expressions.SolutionSpaceExpression implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.GroupByExpression value;

    public GroupBy (com.gdblab.pathAlgebra.expressions.GroupByExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GroupBy)) {
        return false;
      }
      GroupBy o = (GroupBy) other;
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
    public int compareTo(SolutionSpaceExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      GroupBy o = (GroupBy) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class OrderBy extends com.gdblab.pathAlgebra.expressions.SolutionSpaceExpression implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.OrderByExpression value;

    public OrderBy (com.gdblab.pathAlgebra.expressions.OrderByExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OrderBy)) {
        return false;
      }
      OrderBy o = (OrderBy) other;
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
    public int compareTo(SolutionSpaceExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      OrderBy o = (OrderBy) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
