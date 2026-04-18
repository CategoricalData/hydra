// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Path semantics for recursive operations
 */
public abstract class PathSemantics implements Serializable, Comparable<PathSemantics> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.PathSemantics");

  public static final hydra.core.Name WALK = new hydra.core.Name("walk");

  public static final hydra.core.Name TRAIL = new hydra.core.Name("trail");

  public static final hydra.core.Name ACYCLIC = new hydra.core.Name("acyclic");

  public static final hydra.core.Name SIMPLE = new hydra.core.Name("simple");

  public static final hydra.core.Name SHORTEST = new hydra.core.Name("shortest");

  private PathSemantics () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Walk instance) ;

    R visit(Trail instance) ;

    R visit(Acyclic instance) ;

    R visit(Simple instance) ;

    R visit(Shortest instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PathSemantics instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Walk instance) {
      return otherwise(instance);
    }

    default R visit(Trail instance) {
      return otherwise(instance);
    }

    default R visit(Acyclic instance) {
      return otherwise(instance);
    }

    default R visit(Simple instance) {
      return otherwise(instance);
    }

    default R visit(Shortest instance) {
      return otherwise(instance);
    }
  }

  public static final class Walk extends com.gdblab.pathAlgebra.expressions.PathSemantics implements Serializable {
    public Walk () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Walk)) {
        return false;
      }
      Walk o = (Walk) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PathSemantics other) {
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

  public static final class Trail extends com.gdblab.pathAlgebra.expressions.PathSemantics implements Serializable {
    public Trail () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Trail)) {
        return false;
      }
      Trail o = (Trail) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PathSemantics other) {
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

  public static final class Acyclic extends com.gdblab.pathAlgebra.expressions.PathSemantics implements Serializable {
    public Acyclic () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Acyclic)) {
        return false;
      }
      Acyclic o = (Acyclic) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PathSemantics other) {
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

  public static final class Simple extends com.gdblab.pathAlgebra.expressions.PathSemantics implements Serializable {
    public Simple () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PathSemantics other) {
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

  public static final class Shortest extends com.gdblab.pathAlgebra.expressions.PathSemantics implements Serializable {
    public Shortest () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Shortest)) {
        return false;
      }
      Shortest o = (Shortest) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PathSemantics other) {
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
