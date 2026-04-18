// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ApproximateNumericType implements Serializable, Comparable<ApproximateNumericType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ApproximateNumericType");

  public static final hydra.core.Name FLOAT16 = new hydra.core.Name("float16");

  public static final hydra.core.Name FLOAT32 = new hydra.core.Name("float32");

  public static final hydra.core.Name FLOAT64 = new hydra.core.Name("float64");

  public static final hydra.core.Name FLOAT128 = new hydra.core.Name("float128");

  public static final hydra.core.Name FLOAT256 = new hydra.core.Name("float256");

  public static final hydra.core.Name FLOAT_WITH_PRECISION = new hydra.core.Name("floatWithPrecision");

  public static final hydra.core.Name REAL = new hydra.core.Name("real");

  public static final hydra.core.Name DOUBLE_WITH_PRECISION = new hydra.core.Name("doubleWithPrecision");

  private ApproximateNumericType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Float16 instance) ;

    R visit(Float32 instance) ;

    R visit(Float64 instance) ;

    R visit(Float128 instance) ;

    R visit(Float256 instance) ;

    R visit(FloatWithPrecision instance) ;

    R visit(Real instance) ;

    R visit(DoubleWithPrecision instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ApproximateNumericType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Float16 instance) {
      return otherwise(instance);
    }

    default R visit(Float32 instance) {
      return otherwise(instance);
    }

    default R visit(Float64 instance) {
      return otherwise(instance);
    }

    default R visit(Float128 instance) {
      return otherwise(instance);
    }

    default R visit(Float256 instance) {
      return otherwise(instance);
    }

    default R visit(FloatWithPrecision instance) {
      return otherwise(instance);
    }

    default R visit(Real instance) {
      return otherwise(instance);
    }

    default R visit(DoubleWithPrecision instance) {
      return otherwise(instance);
    }
  }

  public static final class Float16 extends openGql.grammar.ApproximateNumericType implements Serializable {
    public final openGql.grammar.Float16Type value;

    public Float16 (openGql.grammar.Float16Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float16)) {
        return false;
      }
      Float16 o = (Float16) other;
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
    public int compareTo(ApproximateNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Float16 o = (Float16) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Float32 extends openGql.grammar.ApproximateNumericType implements Serializable {
    public final openGql.grammar.Float32Type value;

    public Float32 (openGql.grammar.Float32Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float32)) {
        return false;
      }
      Float32 o = (Float32) other;
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
    public int compareTo(ApproximateNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Float32 o = (Float32) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Float64 extends openGql.grammar.ApproximateNumericType implements Serializable {
    public final openGql.grammar.Float64Type value;

    public Float64 (openGql.grammar.Float64Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float64)) {
        return false;
      }
      Float64 o = (Float64) other;
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
    public int compareTo(ApproximateNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Float64 o = (Float64) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Float128 extends openGql.grammar.ApproximateNumericType implements Serializable {
    public final openGql.grammar.Float128Type value;

    public Float128 (openGql.grammar.Float128Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float128)) {
        return false;
      }
      Float128 o = (Float128) other;
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
    public int compareTo(ApproximateNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Float128 o = (Float128) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Float256 extends openGql.grammar.ApproximateNumericType implements Serializable {
    public final openGql.grammar.Float256Type value;

    public Float256 (openGql.grammar.Float256Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float256)) {
        return false;
      }
      Float256 o = (Float256) other;
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
    public int compareTo(ApproximateNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Float256 o = (Float256) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class FloatWithPrecision extends openGql.grammar.ApproximateNumericType implements Serializable {
    public final openGql.grammar.FloatTypeWithPrecision value;

    public FloatWithPrecision (openGql.grammar.FloatTypeWithPrecision value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FloatWithPrecision)) {
        return false;
      }
      FloatWithPrecision o = (FloatWithPrecision) other;
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
    public int compareTo(ApproximateNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      FloatWithPrecision o = (FloatWithPrecision) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Real extends openGql.grammar.ApproximateNumericType implements Serializable {
    public final openGql.grammar.RealType value;

    public Real (openGql.grammar.RealType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Real)) {
        return false;
      }
      Real o = (Real) other;
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
    public int compareTo(ApproximateNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Real o = (Real) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DoubleWithPrecision extends openGql.grammar.ApproximateNumericType implements Serializable {
    public final openGql.grammar.DoubleTypeWithPrecision value;

    public DoubleWithPrecision (openGql.grammar.DoubleTypeWithPrecision value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DoubleWithPrecision)) {
        return false;
      }
      DoubleWithPrecision o = (DoubleWithPrecision) other;
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
    public int compareTo(ApproximateNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DoubleWithPrecision o = (DoubleWithPrecision) other;
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
