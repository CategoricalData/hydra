// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Conditions for filtering paths
 */
public abstract class SelectionCondition implements Serializable, Comparable<SelectionCondition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.SelectionCondition");

  public static final hydra.core.Name SIMPLE = new hydra.core.Name("simple");

  public static final hydra.core.Name AND = new hydra.core.Name("and");

  public static final hydra.core.Name OR = new hydra.core.Name("or");

  public static final hydra.core.Name NOT = new hydra.core.Name("not");

  private SelectionCondition () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Simple instance) ;

    R visit(And instance) ;

    R visit(Or instance) ;

    R visit(Not instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SelectionCondition instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Simple instance) {
      return otherwise(instance);
    }

    default R visit(And instance) {
      return otherwise(instance);
    }

    default R visit(Or instance) {
      return otherwise(instance);
    }

    default R visit(Not instance) {
      return otherwise(instance);
    }
  }

  public static final class Simple extends com.gdblab.pathAlgebra.expressions.SelectionCondition implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.SimpleCondition value;

    public Simple (com.gdblab.pathAlgebra.expressions.SimpleCondition value) {
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
    public int compareTo(SelectionCondition other) {
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

  public static final class And extends com.gdblab.pathAlgebra.expressions.SelectionCondition implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.AndCondition value;

    public And (com.gdblab.pathAlgebra.expressions.AndCondition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof And)) {
        return false;
      }
      And o = (And) other;
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
    public int compareTo(SelectionCondition other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      And o = (And) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Or extends com.gdblab.pathAlgebra.expressions.SelectionCondition implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.OrCondition value;

    public Or (com.gdblab.pathAlgebra.expressions.OrCondition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Or)) {
        return false;
      }
      Or o = (Or) other;
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
    public int compareTo(SelectionCondition other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Or o = (Or) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Not extends com.gdblab.pathAlgebra.expressions.SelectionCondition implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.NotCondition value;

    public Not (com.gdblab.pathAlgebra.expressions.NotCondition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Not)) {
        return false;
      }
      Not o = (Not) other;
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
    public int compareTo(SelectionCondition other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Not o = (Not) other;
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
