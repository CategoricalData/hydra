// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ByteStringType implements Serializable, Comparable<ByteStringType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ByteStringType");

  public static final hydra.core.Name BYTES_TYPE = new hydra.core.Name("bytesType");

  public static final hydra.core.Name BINARY_TYPE = new hydra.core.Name("binaryType");

  public static final hydra.core.Name VARBINARY_TYPE = new hydra.core.Name("varbinaryType");

  private ByteStringType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(BytesType instance) ;

    R visit(BinaryType instance) ;

    R visit(VarbinaryType instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ByteStringType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(BytesType instance) {
      return otherwise(instance);
    }

    default R visit(BinaryType instance) {
      return otherwise(instance);
    }

    default R visit(VarbinaryType instance) {
      return otherwise(instance);
    }
  }

  public static final class BytesType extends openGql.grammar.ByteStringType implements Serializable {
    public final openGql.grammar.BytesType value;

    public BytesType (openGql.grammar.BytesType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BytesType)) {
        return false;
      }
      BytesType o = (BytesType) other;
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
    public int compareTo(ByteStringType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BytesType o = (BytesType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class BinaryType extends openGql.grammar.ByteStringType implements Serializable {
    public final openGql.grammar.BinaryType value;

    public BinaryType (openGql.grammar.BinaryType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BinaryType)) {
        return false;
      }
      BinaryType o = (BinaryType) other;
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
    public int compareTo(ByteStringType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BinaryType o = (BinaryType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class VarbinaryType extends openGql.grammar.ByteStringType implements Serializable {
    public final openGql.grammar.VarbinaryType value;

    public VarbinaryType (openGql.grammar.VarbinaryType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof VarbinaryType)) {
        return false;
      }
      VarbinaryType o = (VarbinaryType) other;
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
    public int compareTo(ByteStringType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      VarbinaryType o = (VarbinaryType) other;
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
