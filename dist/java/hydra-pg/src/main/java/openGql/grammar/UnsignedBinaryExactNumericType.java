// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class UnsignedBinaryExactNumericType implements Serializable, Comparable<UnsignedBinaryExactNumericType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.UnsignedBinaryExactNumericType");

  public static final hydra.core.Name UINT8 = new hydra.core.Name("uint8");

  public static final hydra.core.Name UINT16 = new hydra.core.Name("uint16");

  public static final hydra.core.Name UINT32 = new hydra.core.Name("uint32");

  public static final hydra.core.Name UINT64 = new hydra.core.Name("uint64");

  public static final hydra.core.Name UINT128 = new hydra.core.Name("uint128");

  public static final hydra.core.Name UINT256 = new hydra.core.Name("uint256");

  public static final hydra.core.Name U_SMALL_INT = new hydra.core.Name("uSmallInt");

  public static final hydra.core.Name UINT_WITH_PRECISION = new hydra.core.Name("uintWithPrecision");

  public static final hydra.core.Name U_BIG_INT = new hydra.core.Name("uBigInt");

  public static final hydra.core.Name UNSIGNED = new hydra.core.Name("unsigned");

  private UnsignedBinaryExactNumericType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Uint8 instance) ;

    R visit(Uint16 instance) ;

    R visit(Uint32 instance) ;

    R visit(Uint64 instance) ;

    R visit(Uint128 instance) ;

    R visit(Uint256 instance) ;

    R visit(USmallInt instance) ;

    R visit(UintWithPrecision instance) ;

    R visit(UBigInt instance) ;

    R visit(Unsigned instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnsignedBinaryExactNumericType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Uint8 instance) {
      return otherwise(instance);
    }

    default R visit(Uint16 instance) {
      return otherwise(instance);
    }

    default R visit(Uint32 instance) {
      return otherwise(instance);
    }

    default R visit(Uint64 instance) {
      return otherwise(instance);
    }

    default R visit(Uint128 instance) {
      return otherwise(instance);
    }

    default R visit(Uint256 instance) {
      return otherwise(instance);
    }

    default R visit(USmallInt instance) {
      return otherwise(instance);
    }

    default R visit(UintWithPrecision instance) {
      return otherwise(instance);
    }

    default R visit(UBigInt instance) {
      return otherwise(instance);
    }

    default R visit(Unsigned instance) {
      return otherwise(instance);
    }
  }

  public static final class Uint8 extends openGql.grammar.UnsignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Uint8Type value;

    public Uint8 (openGql.grammar.Uint8Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint8)) {
        return false;
      }
      Uint8 o = (Uint8) other;
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
    public int compareTo(UnsignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Uint8 o = (Uint8) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Uint16 extends openGql.grammar.UnsignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Uint16Type value;

    public Uint16 (openGql.grammar.Uint16Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint16)) {
        return false;
      }
      Uint16 o = (Uint16) other;
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
    public int compareTo(UnsignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Uint16 o = (Uint16) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Uint32 extends openGql.grammar.UnsignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Uint32Type value;

    public Uint32 (openGql.grammar.Uint32Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint32)) {
        return false;
      }
      Uint32 o = (Uint32) other;
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
    public int compareTo(UnsignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Uint32 o = (Uint32) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Uint64 extends openGql.grammar.UnsignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Uint64Type value;

    public Uint64 (openGql.grammar.Uint64Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint64)) {
        return false;
      }
      Uint64 o = (Uint64) other;
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
    public int compareTo(UnsignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Uint64 o = (Uint64) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Uint128 extends openGql.grammar.UnsignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Uint128Type value;

    public Uint128 (openGql.grammar.Uint128Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint128)) {
        return false;
      }
      Uint128 o = (Uint128) other;
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
    public int compareTo(UnsignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Uint128 o = (Uint128) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Uint256 extends openGql.grammar.UnsignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.Uint256Type value;

    public Uint256 (openGql.grammar.Uint256Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint256)) {
        return false;
      }
      Uint256 o = (Uint256) other;
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
    public int compareTo(UnsignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Uint256 o = (Uint256) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class USmallInt extends openGql.grammar.UnsignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.USmallIntType value;

    public USmallInt (openGql.grammar.USmallIntType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof USmallInt)) {
        return false;
      }
      USmallInt o = (USmallInt) other;
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
    public int compareTo(UnsignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      USmallInt o = (USmallInt) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class UintWithPrecision extends openGql.grammar.UnsignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.UintWithPrecision value;

    public UintWithPrecision (openGql.grammar.UintWithPrecision value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UintWithPrecision)) {
        return false;
      }
      UintWithPrecision o = (UintWithPrecision) other;
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
    public int compareTo(UnsignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UintWithPrecision o = (UintWithPrecision) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class UBigInt extends openGql.grammar.UnsignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.UBigIntType value;

    public UBigInt (openGql.grammar.UBigIntType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UBigInt)) {
        return false;
      }
      UBigInt o = (UBigInt) other;
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
    public int compareTo(UnsignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UBigInt o = (UBigInt) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Unsigned extends openGql.grammar.UnsignedBinaryExactNumericType implements Serializable {
    public final openGql.grammar.VerboseBinaryExactNumericType value;

    public Unsigned (openGql.grammar.VerboseBinaryExactNumericType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unsigned)) {
        return false;
      }
      Unsigned o = (Unsigned) other;
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
    public int compareTo(UnsignedBinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Unsigned o = (Unsigned) other;
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
