// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class PredefinedType implements Serializable, Comparable<PredefinedType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PredefinedType");

  public static final hydra.core.Name BOOLEAN_TYPE = new hydra.core.Name("booleanType");

  public static final hydra.core.Name CHARACTER_STRING_TYPE = new hydra.core.Name("characterStringType");

  public static final hydra.core.Name BYTE_STRING_TYPE = new hydra.core.Name("byteStringType");

  public static final hydra.core.Name NUMERIC_TYPE = new hydra.core.Name("numericType");

  public static final hydra.core.Name TEMPORAL_TYPE = new hydra.core.Name("temporalType");

  public static final hydra.core.Name REFERENCE_VALUE_TYPE = new hydra.core.Name("referenceValueType");

  public static final hydra.core.Name IMMATERIAL_VALUE_TYPE = new hydra.core.Name("immaterialValueType");

  private PredefinedType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(BooleanType instance) ;

    R visit(CharacterStringType instance) ;

    R visit(ByteStringType instance) ;

    R visit(NumericType instance) ;

    R visit(TemporalType instance) ;

    R visit(ReferenceValueType instance) ;

    R visit(ImmaterialValueType instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PredefinedType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(BooleanType instance) {
      return otherwise(instance);
    }

    default R visit(CharacterStringType instance) {
      return otherwise(instance);
    }

    default R visit(ByteStringType instance) {
      return otherwise(instance);
    }

    default R visit(NumericType instance) {
      return otherwise(instance);
    }

    default R visit(TemporalType instance) {
      return otherwise(instance);
    }

    default R visit(ReferenceValueType instance) {
      return otherwise(instance);
    }

    default R visit(ImmaterialValueType instance) {
      return otherwise(instance);
    }
  }

  public static final class BooleanType extends openGql.grammar.PredefinedType implements Serializable {
    public final openGql.grammar.BooleanType value;

    public BooleanType (openGql.grammar.BooleanType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BooleanType)) {
        return false;
      }
      BooleanType o = (BooleanType) other;
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
    public int compareTo(PredefinedType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BooleanType o = (BooleanType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CharacterStringType extends openGql.grammar.PredefinedType implements Serializable {
    public final openGql.grammar.CharacterStringType value;

    public CharacterStringType (openGql.grammar.CharacterStringType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CharacterStringType)) {
        return false;
      }
      CharacterStringType o = (CharacterStringType) other;
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
    public int compareTo(PredefinedType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CharacterStringType o = (CharacterStringType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ByteStringType extends openGql.grammar.PredefinedType implements Serializable {
    public final openGql.grammar.ByteStringType value;

    public ByteStringType (openGql.grammar.ByteStringType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ByteStringType)) {
        return false;
      }
      ByteStringType o = (ByteStringType) other;
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
    public int compareTo(PredefinedType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ByteStringType o = (ByteStringType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NumericType extends openGql.grammar.PredefinedType implements Serializable {
    public final openGql.grammar.NumericType value;

    public NumericType (openGql.grammar.NumericType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NumericType)) {
        return false;
      }
      NumericType o = (NumericType) other;
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
    public int compareTo(PredefinedType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NumericType o = (NumericType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class TemporalType extends openGql.grammar.PredefinedType implements Serializable {
    public final openGql.grammar.TemporalType value;

    public TemporalType (openGql.grammar.TemporalType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TemporalType)) {
        return false;
      }
      TemporalType o = (TemporalType) other;
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
    public int compareTo(PredefinedType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TemporalType o = (TemporalType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ReferenceValueType extends openGql.grammar.PredefinedType implements Serializable {
    public final openGql.grammar.ReferenceValueType value;

    public ReferenceValueType (openGql.grammar.ReferenceValueType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ReferenceValueType)) {
        return false;
      }
      ReferenceValueType o = (ReferenceValueType) other;
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
    public int compareTo(PredefinedType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ReferenceValueType o = (ReferenceValueType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ImmaterialValueType extends openGql.grammar.PredefinedType implements Serializable {
    public final openGql.grammar.ImmaterialValueType value;

    public ImmaterialValueType (openGql.grammar.ImmaterialValueType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ImmaterialValueType)) {
        return false;
      }
      ImmaterialValueType o = (ImmaterialValueType) other;
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
    public int compareTo(PredefinedType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ImmaterialValueType o = (ImmaterialValueType) other;
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
