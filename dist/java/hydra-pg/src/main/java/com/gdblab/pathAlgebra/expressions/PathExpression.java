// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * A path algebra expression that evaluates to a set of paths
 */
public abstract class PathExpression implements Serializable, Comparable<PathExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.PathExpression");

  public static final hydra.core.Name BASE = new hydra.core.Name("base");

  public static final hydra.core.Name SELECTION = new hydra.core.Name("selection");

  public static final hydra.core.Name JOIN = new hydra.core.Name("join");

  public static final hydra.core.Name UNION = new hydra.core.Name("union");

  public static final hydra.core.Name RECURSIVE = new hydra.core.Name("recursive");

  public static final hydra.core.Name GROUP_BY = new hydra.core.Name("groupBy");

  public static final hydra.core.Name ORDER_BY = new hydra.core.Name("orderBy");

  public static final hydra.core.Name PROJECTION = new hydra.core.Name("projection");

  private PathExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Base instance) ;

    R visit(Selection instance) ;

    R visit(Join instance) ;

    R visit(Union instance) ;

    R visit(Recursive instance) ;

    R visit(GroupBy instance) ;

    R visit(OrderBy instance) ;

    R visit(Projection instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PathExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Base instance) {
      return otherwise(instance);
    }

    default R visit(Selection instance) {
      return otherwise(instance);
    }

    default R visit(Join instance) {
      return otherwise(instance);
    }

    default R visit(Union instance) {
      return otherwise(instance);
    }

    default R visit(Recursive instance) {
      return otherwise(instance);
    }

    default R visit(GroupBy instance) {
      return otherwise(instance);
    }

    default R visit(OrderBy instance) {
      return otherwise(instance);
    }

    default R visit(Projection instance) {
      return otherwise(instance);
    }
  }

  /**
   * Base case: extract paths from graph
   */
  public static final class Base extends com.gdblab.pathAlgebra.expressions.PathExpression implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.BaseExpression value;

    public Base (com.gdblab.pathAlgebra.expressions.BaseExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Base)) {
        return false;
      }
      Base o = (Base) other;
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
    public int compareTo(PathExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Base o = (Base) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Selection operator (σ): filter paths by condition
   */
  public static final class Selection extends com.gdblab.pathAlgebra.expressions.PathExpression implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.SelectionExpression value;

    public Selection (com.gdblab.pathAlgebra.expressions.SelectionExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Selection)) {
        return false;
      }
      Selection o = (Selection) other;
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
    public int compareTo(PathExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Selection o = (Selection) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Join operator (⊲⊳): concatenate compatible paths
   */
  public static final class Join extends com.gdblab.pathAlgebra.expressions.PathExpression implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.JoinExpression value;

    public Join (com.gdblab.pathAlgebra.expressions.JoinExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Join)) {
        return false;
      }
      Join o = (Join) other;
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
    public int compareTo(PathExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Join o = (Join) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Union operator (∪): combine path sets
   */
  public static final class Union extends com.gdblab.pathAlgebra.expressions.PathExpression implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.UnionExpression value;

    public Union (com.gdblab.pathAlgebra.expressions.UnionExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) other;
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
    public int compareTo(PathExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Union o = (Union) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Recursive operator (φ): compute transitive closure with semantics
   */
  public static final class Recursive extends com.gdblab.pathAlgebra.expressions.PathExpression implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.RecursiveExpression value;

    public Recursive (com.gdblab.pathAlgebra.expressions.RecursiveExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Recursive)) {
        return false;
      }
      Recursive o = (Recursive) other;
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
    public int compareTo(PathExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Recursive o = (Recursive) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Group-by operator (γ): organize paths into solution space
   */
  public static final class GroupBy extends com.gdblab.pathAlgebra.expressions.PathExpression implements Serializable {
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
    public int compareTo(PathExpression other) {
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

  /**
   * Order-by operator (τ): sort solution space
   */
  public static final class OrderBy extends com.gdblab.pathAlgebra.expressions.PathExpression implements Serializable {
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
    public int compareTo(PathExpression other) {
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

  /**
   * Projection operator (π): extract paths from solution space
   */
  public static final class Projection extends com.gdblab.pathAlgebra.expressions.PathExpression implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.ProjectionExpression value;

    public Projection (com.gdblab.pathAlgebra.expressions.ProjectionExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Projection)) {
        return false;
      }
      Projection o = (Projection) other;
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
    public int compareTo(PathExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Projection o = (Projection) other;
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
