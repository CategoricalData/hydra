// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class VerboseBinaryExactNumericType implements Serializable, Comparable<VerboseBinaryExactNumericType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.VerboseBinaryExactNumericType");

  public static final hydra.core.Name INTEGER8 = new hydra.core.Name("integer8");

  public static final hydra.core.Name INTEGER16 = new hydra.core.Name("integer16");

  public static final hydra.core.Name INTEGER32 = new hydra.core.Name("integer32");

  public static final hydra.core.Name INTEGER64 = new hydra.core.Name("integer64");

  public static final hydra.core.Name INTEGER128 = new hydra.core.Name("integer128");

  public static final hydra.core.Name INTEGER256 = new hydra.core.Name("integer256");

  public static final hydra.core.Name SMALL_INTEGER = new hydra.core.Name("smallInteger");

  public static final hydra.core.Name INTEGER_WITH_PRECISION = new hydra.core.Name("integerWithPrecision");

  public static final hydra.core.Name BIG_INTEGER = new hydra.core.Name("bigInteger");

  private VerboseBinaryExactNumericType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Integer8 instance) ;

    R visit(Integer16 instance) ;

    R visit(Integer32 instance) ;

    R visit(Integer64 instance) ;

    R visit(Integer128 instance) ;

    R visit(Integer256 instance) ;

    R visit(SmallInteger instance) ;

    R visit(IntegerWithPrecision instance) ;

    R visit(BigInteger instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(VerboseBinaryExactNumericType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Integer8 instance) {
      return otherwise(instance);
    }

    default R visit(Integer16 instance) {
      return otherwise(instance);
    }

    default R visit(Integer32 instance) {
      return otherwise(instance);
    }

    default R visit(Integer64 instance) {
      return otherwise(instance);
    }

    default R visit(Integer128 instance) {
      return otherwise(instance);
    }

    default R visit(Integer256 instance) {
      return otherwise(instance);
    }

    default R visit(SmallInteger instance) {
      return otherwise(instance);
    }

    default R visit(IntegerWithPrecision instance) {
      return otherwise(instance);
    }

    default R visit(BigInteger instance) {
      return otherwise(instance);
    }
  }

  public static final class Integer8 extends openGql.grammar.VerboseBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Integer8Type value;

    public Integer8 (openGql.grammar.Integer8Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Integer8)) {
        return false;
      }
      Integer8 o = (Integer8) other;
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
    public int compareTo(VerboseBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Integer8 o = (Integer8) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Integer16 extends openGql.grammar.VerboseBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Integer16Type value;

    public Integer16 (openGql.grammar.Integer16Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Integer16)) {
        return false;
      }
      Integer16 o = (Integer16) other;
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
    public int compareTo(VerboseBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Integer16 o = (Integer16) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Integer32 extends openGql.grammar.VerboseBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Integer32Type value;

    public Integer32 (openGql.grammar.Integer32Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Integer32)) {
        return false;
      }
      Integer32 o = (Integer32) other;
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
    public int compareTo(VerboseBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Integer32 o = (Integer32) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Integer64 extends openGql.grammar.VerboseBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Integer64Type value;

    public Integer64 (openGql.grammar.Integer64Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Integer64)) {
        return false;
      }
      Integer64 o = (Integer64) other;
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
    public int compareTo(VerboseBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Integer64 o = (Integer64) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Integer128 extends openGql.grammar.VerboseBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Integer128Type value;

    public Integer128 (openGql.grammar.Integer128Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Integer128)) {
        return false;
      }
      Integer128 o = (Integer128) other;
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
    public int compareTo(VerboseBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Integer128 o = (Integer128) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Integer256 extends openGql.grammar.VerboseBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Integer256Type value;

    public Integer256 (openGql.grammar.Integer256Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Integer256)) {
        return false;
      }
      Integer256 o = (Integer256) other;
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
    public int compareTo(VerboseBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Integer256 o = (Integer256) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class SmallInteger extends openGql.grammar.VerboseBinaryExactNumericType implements Serializable {
    public final openGql.grammar.SmallIntegerType value;

    public SmallInteger (openGql.grammar.SmallIntegerType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SmallInteger)) {
        return false;
      }
      SmallInteger o = (SmallInteger) other;
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
    public int compareTo(VerboseBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SmallInteger o = (SmallInteger) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class IntegerWithPrecision extends openGql.grammar.VerboseBinaryExactNumericType implements Serializable {
    public final openGql.grammar.IntegerWithPrecision value;

    public IntegerWithPrecision (openGql.grammar.IntegerWithPrecision value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IntegerWithPrecision)) {
        return false;
      }
      IntegerWithPrecision o = (IntegerWithPrecision) other;
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
    public int compareTo(VerboseBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      IntegerWithPrecision o = (IntegerWithPrecision) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class BigInteger extends openGql.grammar.VerboseBinaryExactNumericType implements Serializable {
    public final openGql.grammar.BigIntegerType value;

    public BigInteger (openGql.grammar.BigIntegerType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BigInteger)) {
        return false;
      }
      BigInteger o = (BigInteger) other;
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
    public int compareTo(VerboseBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BigInteger o = (BigInteger) other;
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
