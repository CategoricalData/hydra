// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class CharacterStringType implements Serializable, Comparable<CharacterStringType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CharacterStringType");

  public static final hydra.core.Name STRING_TYPE = new hydra.core.Name("stringType");

  public static final hydra.core.Name CHAR_TYPE = new hydra.core.Name("charType");

  public static final hydra.core.Name VARCHAR_TYPE = new hydra.core.Name("varcharType");

  private CharacterStringType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(StringType instance) ;

    R visit(CharType instance) ;

    R visit(VarcharType instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CharacterStringType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(StringType instance) {
      return otherwise(instance);
    }

    default R visit(CharType instance) {
      return otherwise(instance);
    }

    default R visit(VarcharType instance) {
      return otherwise(instance);
    }
  }

  public static final class StringType extends openGql.grammar.CharacterStringType implements Serializable {
    public final openGql.grammar.StringType value;

    public StringType (openGql.grammar.StringType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StringType)) {
        return false;
      }
      StringType o = (StringType) other;
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
    public int compareTo(CharacterStringType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      StringType o = (StringType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CharType extends openGql.grammar.CharacterStringType implements Serializable {
    public final openGql.grammar.CharType value;

    public CharType (openGql.grammar.CharType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CharType)) {
        return false;
      }
      CharType o = (CharType) other;
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
    public int compareTo(CharacterStringType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CharType o = (CharType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class VarcharType extends openGql.grammar.CharacterStringType implements Serializable {
    public final openGql.grammar.VarcharType value;

    public VarcharType (openGql.grammar.VarcharType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof VarcharType)) {
        return false;
      }
      VarcharType o = (VarcharType) other;
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
    public int compareTo(CharacterStringType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      VarcharType o = (VarcharType) other;
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
