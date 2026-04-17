// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * References to elements within a path
 */
public abstract class PathElement implements Serializable, Comparable<PathElement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.PathElement");

  public static final hydra.core.Name NODE = new hydra.core.Name("node");

  public static final hydra.core.Name EDGE = new hydra.core.Name("edge");

  public static final hydra.core.Name FIRST = new hydra.core.Name("first");

  public static final hydra.core.Name LAST = new hydra.core.Name("last");

  private PathElement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Node instance) ;

    R visit(Edge instance) ;

    R visit(First instance) ;

    R visit(Last instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PathElement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Node instance) {
      return otherwise(instance);
    }

    default R visit(Edge instance) {
      return otherwise(instance);
    }

    default R visit(First instance) {
      return otherwise(instance);
    }

    default R visit(Last instance) {
      return otherwise(instance);
    }
  }

  public static final class Node extends com.gdblab.pathAlgebra.expressions.PathElement implements Serializable {
    public final Integer value;

    public Node (Integer value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Node)) {
        return false;
      }
      Node o = (Node) other;
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
    public int compareTo(PathElement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Node o = (Node) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Edge extends com.gdblab.pathAlgebra.expressions.PathElement implements Serializable {
    public final Integer value;

    public Edge (Integer value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Edge)) {
        return false;
      }
      Edge o = (Edge) other;
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
    public int compareTo(PathElement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Edge o = (Edge) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class First extends com.gdblab.pathAlgebra.expressions.PathElement implements Serializable {
    public First () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof First)) {
        return false;
      }
      First o = (First) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PathElement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Last extends com.gdblab.pathAlgebra.expressions.PathElement implements Serializable {
    public Last () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Last)) {
        return false;
      }
      Last o = (Last) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PathElement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
