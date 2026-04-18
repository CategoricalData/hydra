// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public abstract class ComplexCondition implements Serializable, Comparable<ComplexCondition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.ComplexCondition");

  public static final hydra.core.Name SIMPLE = new hydra.core.Name("simple");

  public static final hydra.core.Name COMPOUND = new hydra.core.Name("compound");

  private ComplexCondition () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Simple instance) ;

    R visit(Compound instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ComplexCondition instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Simple instance) {
      return otherwise(instance);
    }

    default R visit(Compound instance) {
      return otherwise(instance);
    }
  }

  public static final class Simple extends com.gdblab.pathAlgebra.syntax.ComplexCondition implements Serializable {
    public final com.gdblab.pathAlgebra.syntax.Condition value;

    public Simple (com.gdblab.pathAlgebra.syntax.Condition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) other;
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
    public int compareTo(ComplexCondition other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Simple o = (Simple) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Compound extends com.gdblab.pathAlgebra.syntax.ComplexCondition implements Serializable {
    public final com.gdblab.pathAlgebra.syntax.CompoundComplexCondition value;

    public Compound (com.gdblab.pathAlgebra.syntax.CompoundComplexCondition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Compound)) {
        return false;
      }
      Compound o = (Compound) other;
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
    public int compareTo(ComplexCondition other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Compound o = (Compound) other;
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
