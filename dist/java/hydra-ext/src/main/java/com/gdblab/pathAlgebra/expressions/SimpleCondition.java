// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.expressions;

import java.io.Serializable;

/**
 * Atomic selection conditions
 */
public abstract class SimpleCondition implements Serializable, Comparable<SimpleCondition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.expressions.SimpleCondition");

  public static final hydra.core.Name LABEL_EQUALS = new hydra.core.Name("labelEquals");

  public static final hydra.core.Name PROPERTY_EQUALS = new hydra.core.Name("propertyEquals");

  public static final hydra.core.Name PROPERTY_COMPARISON = new hydra.core.Name("propertyComparison");

  public static final hydra.core.Name LENGTH_EQUALS = new hydra.core.Name("lengthEquals");

  private SimpleCondition () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(LabelEquals instance) ;

    R visit(PropertyEquals instance) ;

    R visit(PropertyComparison instance) ;

    R visit(LengthEquals instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SimpleCondition instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(LabelEquals instance) {
      return otherwise(instance);
    }

    default R visit(PropertyEquals instance) {
      return otherwise(instance);
    }

    default R visit(PropertyComparison instance) {
      return otherwise(instance);
    }

    default R visit(LengthEquals instance) {
      return otherwise(instance);
    }
  }

  public static final class LabelEquals extends com.gdblab.pathAlgebra.expressions.SimpleCondition implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.LabelCondition value;

    public LabelEquals (com.gdblab.pathAlgebra.expressions.LabelCondition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LabelEquals)) {
        return false;
      }
      LabelEquals o = (LabelEquals) other;
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
    public int compareTo(SimpleCondition other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LabelEquals o = (LabelEquals) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PropertyEquals extends com.gdblab.pathAlgebra.expressions.SimpleCondition implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.PropertyCondition value;

    public PropertyEquals (com.gdblab.pathAlgebra.expressions.PropertyCondition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PropertyEquals)) {
        return false;
      }
      PropertyEquals o = (PropertyEquals) other;
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
    public int compareTo(SimpleCondition other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PropertyEquals o = (PropertyEquals) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PropertyComparison extends com.gdblab.pathAlgebra.expressions.SimpleCondition implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition value;

    public PropertyComparison (com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PropertyComparison)) {
        return false;
      }
      PropertyComparison o = (PropertyComparison) other;
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
    public int compareTo(SimpleCondition other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PropertyComparison o = (PropertyComparison) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class LengthEquals extends com.gdblab.pathAlgebra.expressions.SimpleCondition implements Serializable {
    public final com.gdblab.pathAlgebra.expressions.LengthCondition value;

    public LengthEquals (com.gdblab.pathAlgebra.expressions.LengthCondition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LengthEquals)) {
        return false;
      }
      LengthEquals o = (LengthEquals) other;
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
    public int compareTo(SimpleCondition other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LengthEquals o = (LengthEquals) other;
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
