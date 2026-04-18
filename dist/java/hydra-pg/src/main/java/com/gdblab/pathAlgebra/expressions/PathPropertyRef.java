// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Reference to path-level properties: length, etc.
 */
public abstract class PathPropertyRef implements Serializable, Comparable<PathPropertyRef> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.PathPropertyRef");

  public static final hydra.core.Name LENGTH = new hydra.core.Name("length");

  public static final hydra.core.Name START_NODE = new hydra.core.Name("startNode");

  public static final hydra.core.Name END_NODE = new hydra.core.Name("endNode");

  private PathPropertyRef () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Length instance) ;

    R visit(StartNode instance) ;

    R visit(EndNode instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PathPropertyRef instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Length instance) {
      return otherwise(instance);
    }

    default R visit(StartNode instance) {
      return otherwise(instance);
    }

    default R visit(EndNode instance) {
      return otherwise(instance);
    }
  }

  public static final class Length extends com.gdblab.pathAlgebra.expressions.PathPropertyRef implements Serializable {
    public Length () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Length)) {
        return false;
      }
      Length o = (Length) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PathPropertyRef other) {
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

  public static final class StartNode extends com.gdblab.pathAlgebra.expressions.PathPropertyRef implements Serializable {
    public StartNode () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StartNode)) {
        return false;
      }
      StartNode o = (StartNode) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PathPropertyRef other) {
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

  public static final class EndNode extends com.gdblab.pathAlgebra.expressions.PathPropertyRef implements Serializable {
    public EndNode () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EndNode)) {
        return false;
      }
      EndNode o = (EndNode) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PathPropertyRef other) {
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
