// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Base path expressions that extract paths from graph
 */
public abstract class BaseExpression implements Serializable, Comparable<BaseExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.BaseExpression");

  public static final hydra.core.Name PATHS0 = new hydra.core.Name("paths0");

  public static final hydra.core.Name PATHS1 = new hydra.core.Name("paths1");

  public static final hydra.core.Name PATHS_STAR = new hydra.core.Name("pathsStar");

  private BaseExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Paths0 instance) ;

    R visit(Paths1 instance) ;

    R visit(PathsStar instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BaseExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Paths0 instance) {
      return otherwise(instance);
    }

    default R visit(Paths1 instance) {
      return otherwise(instance);
    }

    default R visit(PathsStar instance) {
      return otherwise(instance);
    }
  }

  /**
   * Paths0(G): all paths of length 0 (nodes)
   */
  public static final class Paths0 extends com.gdblab.pathAlgebra.expressions.BaseExpression implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.GraphReference value;

    public Paths0 (com.gdblab.pathAlgebra.expressions.GraphReference value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Paths0)) {
        return false;
      }
      Paths0 o = (Paths0) other;
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
    public int compareTo(BaseExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Paths0 o = (Paths0) other;
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
   * Paths1(G): all paths of length 1 (edges)
   */
  public static final class Paths1 extends com.gdblab.pathAlgebra.expressions.BaseExpression implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.GraphReference value;

    public Paths1 (com.gdblab.pathAlgebra.expressions.GraphReference value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Paths1)) {
        return false;
      }
      Paths1 o = (Paths1) other;
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
    public int compareTo(BaseExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Paths1 o = (Paths1) other;
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
   * Paths*(G): all paths in graph (infinite without restrictions)
   */
  public static final class PathsStar extends com.gdblab.pathAlgebra.expressions.BaseExpression implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.GraphReference value;

    public PathsStar (com.gdblab.pathAlgebra.expressions.GraphReference value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PathsStar)) {
        return false;
      }
      PathsStar o = (PathsStar) other;
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
    public int compareTo(BaseExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PathsStar o = (PathsStar) other;
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
