// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public abstract class EdgeDirection implements Serializable, Comparable<EdgeDirection> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.EdgeDirection");

  public static final hydra.core.Name OUTGOING = new hydra.core.Name("outgoing");

  public static final hydra.core.Name INCOMING = new hydra.core.Name("incoming");

  public static final hydra.core.Name UNDIRECTED = new hydra.core.Name("undirected");

  private EdgeDirection () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Outgoing instance) ;

    R visit(Incoming instance) ;

    R visit(Undirected instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EdgeDirection instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Outgoing instance) {
      return otherwise(instance);
    }

    default R visit(Incoming instance) {
      return otherwise(instance);
    }

    default R visit(Undirected instance) {
      return otherwise(instance);
    }
  }

  public static final class Outgoing extends com.gdblab.pathAlgebra.syntax.EdgeDirection implements Serializable {
    public Outgoing () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Outgoing)) {
        return false;
      }
      Outgoing o = (Outgoing) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(EdgeDirection other) {
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

  public static final class Incoming extends com.gdblab.pathAlgebra.syntax.EdgeDirection implements Serializable {
    public Incoming () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Incoming)) {
        return false;
      }
      Incoming o = (Incoming) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(EdgeDirection other) {
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

  public static final class Undirected extends com.gdblab.pathAlgebra.syntax.EdgeDirection implements Serializable {
    public Undirected () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Undirected)) {
        return false;
      }
      Undirected o = (Undirected) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(EdgeDirection other) {
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
