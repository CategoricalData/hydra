// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SignedBinaryExactNumericType implements Serializable, Comparable<SignedBinaryExactNumericType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SignedBinaryExactNumericType");

  public static final hydra.core.Name INT8 = new hydra.core.Name("int8");

  public static final hydra.core.Name INT16 = new hydra.core.Name("int16");

  public static final hydra.core.Name INT32 = new hydra.core.Name("int32");

  public static final hydra.core.Name INT64 = new hydra.core.Name("int64");

  public static final hydra.core.Name INT128 = new hydra.core.Name("int128");

  public static final hydra.core.Name INT256 = new hydra.core.Name("int256");

  public static final hydra.core.Name SMALL_INT = new hydra.core.Name("smallInt");

  public static final hydra.core.Name INT_WITH_PRECISION = new hydra.core.Name("intWithPrecision");

  public static final hydra.core.Name BIG_INT = new hydra.core.Name("bigInt");

  public static final hydra.core.Name SIGNED_VERBOSE_TYPE = new hydra.core.Name("signedVerboseType");

  private SignedBinaryExactNumericType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Int8 instance) ;

    R visit(Int16 instance) ;

    R visit(Int32 instance) ;

    R visit(Int64 instance) ;

    R visit(Int128 instance) ;

    R visit(Int256 instance) ;

    R visit(SmallInt instance) ;

    R visit(IntWithPrecision instance) ;

    R visit(BigInt instance) ;

    R visit(SignedVerboseType instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SignedBinaryExactNumericType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Int8 instance) {
      return otherwise(instance);
    }

    default R visit(Int16 instance) {
      return otherwise(instance);
    }

    default R visit(Int32 instance) {
      return otherwise(instance);
    }

    default R visit(Int64 instance) {
      return otherwise(instance);
    }

    default R visit(Int128 instance) {
      return otherwise(instance);
    }

    default R visit(Int256 instance) {
      return otherwise(instance);
    }

    default R visit(SmallInt instance) {
      return otherwise(instance);
    }

    default R visit(IntWithPrecision instance) {
      return otherwise(instance);
    }

    default R visit(BigInt instance) {
      return otherwise(instance);
    }

    default R visit(SignedVerboseType instance) {
      return otherwise(instance);
    }
  }

  public static final class Int8 extends openGql.grammar.SignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Int8Type value;

    public Int8 (openGql.grammar.Int8Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int8)) {
        return false;
      }
      Int8 o = (Int8) other;
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
    public int compareTo(SignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Int8 o = (Int8) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Int16 extends openGql.grammar.SignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Int16Type value;

    public Int16 (openGql.grammar.Int16Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int16)) {
        return false;
      }
      Int16 o = (Int16) other;
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
    public int compareTo(SignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Int16 o = (Int16) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Int32 extends openGql.grammar.SignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Int32Type value;

    public Int32 (openGql.grammar.Int32Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int32)) {
        return false;
      }
      Int32 o = (Int32) other;
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
    public int compareTo(SignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Int32 o = (Int32) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Int64 extends openGql.grammar.SignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Int64Type value;

    public Int64 (openGql.grammar.Int64Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int64)) {
        return false;
      }
      Int64 o = (Int64) other;
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
    public int compareTo(SignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Int64 o = (Int64) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Int128 extends openGql.grammar.SignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Int128Type value;

    public Int128 (openGql.grammar.Int128Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int128)) {
        return false;
      }
      Int128 o = (Int128) other;
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
    public int compareTo(SignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Int128 o = (Int128) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Int256 extends openGql.grammar.SignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Int256Type value;

    public Int256 (openGql.grammar.Int256Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int256)) {
        return false;
      }
      Int256 o = (Int256) other;
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
    public int compareTo(SignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Int256 o = (Int256) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class SmallInt extends openGql.grammar.SignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.SmallIntType value;

    public SmallInt (openGql.grammar.SmallIntType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SmallInt)) {
        return false;
      }
      SmallInt o = (SmallInt) other;
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
    public int compareTo(SignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SmallInt o = (SmallInt) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class IntWithPrecision extends openGql.grammar.SignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.IntWithPrecision value;

    public IntWithPrecision (openGql.grammar.IntWithPrecision value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IntWithPrecision)) {
        return false;
      }
      IntWithPrecision o = (IntWithPrecision) other;
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
    public int compareTo(SignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      IntWithPrecision o = (IntWithPrecision) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class BigInt extends openGql.grammar.SignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.BigIntType value;

    public BigInt (openGql.grammar.BigIntType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BigInt)) {
        return false;
      }
      BigInt o = (BigInt) other;
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
    public int compareTo(SignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BigInt o = (BigInt) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class SignedVerboseType extends openGql.grammar.SignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.SignedVerboseBinaryExactNumericType value;

    public SignedVerboseType (openGql.grammar.SignedVerboseBinaryExactNumericType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SignedVerboseType)) {
        return false;
      }
      SignedVerboseType o = (SignedVerboseType) other;
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
    public int compareTo(SignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SignedVerboseType o = (SignedVerboseType) other;
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
