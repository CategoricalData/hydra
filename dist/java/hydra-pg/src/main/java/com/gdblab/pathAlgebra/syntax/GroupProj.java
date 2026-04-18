// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public abstract class GroupProj implements Serializable, Comparable<GroupProj> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.GroupProj");

  public static final hydra.core.Name ALL = new hydra.core.Name("all");

  public static final hydra.core.Name LIMITED = new hydra.core.Name("limited");

  private GroupProj () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(All instance) ;

    R visit(Limited instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GroupProj instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(All instance) {
      return otherwise(instance);
    }

    default R visit(Limited instance) {
      return otherwise(instance);
    }
  }

  public static final class All extends com.gdblab.pathAlgebra.syntax.GroupProj implements Serializable {
    public All () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof All)) {
        return false;
      }
      All o = (All) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GroupProj other) {
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

  public static final class Limited extends com.gdblab.pathAlgebra.syntax.GroupProj implements Serializable {
    public final java.math.BigInteger value;

    public Limited (java.math.BigInteger value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Limited)) {
        return false;
      }
      Limited o = (Limited) other;
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
    public int compareTo(GroupProj other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Limited o = (Limited) other;
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
