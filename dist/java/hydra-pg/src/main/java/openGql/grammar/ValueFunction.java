// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ValueFunction implements Serializable, Comparable<ValueFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ValueFunction");

  public static final hydra.core.Name NUMERIC = new hydra.core.Name("numeric");

  public static final hydra.core.Name DATETIME_SUBTRACTION = new hydra.core.Name("datetimeSubtraction");

  public static final hydra.core.Name DATETIME = new hydra.core.Name("datetime");

  public static final hydra.core.Name DURATION = new hydra.core.Name("duration");

  public static final hydra.core.Name CHARACTER_OR_BYTE_STRING = new hydra.core.Name("characterOrByteString");

  public static final hydra.core.Name LIST = new hydra.core.Name("list");

  private ValueFunction () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Numeric instance) ;

    R visit(DatetimeSubtraction instance) ;

    R visit(Datetime instance) ;

    R visit(Duration instance) ;

    R visit(CharacterOrByteString instance) ;

    R visit(List instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ValueFunction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Numeric instance) {
      return otherwise(instance);
    }

    default R visit(DatetimeSubtraction instance) {
      return otherwise(instance);
    }

    default R visit(Datetime instance) {
      return otherwise(instance);
    }

    default R visit(Duration instance) {
      return otherwise(instance);
    }

    default R visit(CharacterOrByteString instance) {
      return otherwise(instance);
    }

    default R visit(List instance) {
      return otherwise(instance);
    }
  }

  public static final class Numeric extends openGql.grammar.ValueFunction implements Serializable {
    public final openGql.grammar.NumericValueFunction value;

    public Numeric (openGql.grammar.NumericValueFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Numeric)) {
        return false;
      }
      Numeric o = (Numeric) other;
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
    public int compareTo(ValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Numeric o = (Numeric) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DatetimeSubtraction extends openGql.grammar.ValueFunction implements Serializable {
    public final openGql.grammar.DatetimeSubtraction value;

    public DatetimeSubtraction (openGql.grammar.DatetimeSubtraction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DatetimeSubtraction)) {
        return false;
      }
      DatetimeSubtraction o = (DatetimeSubtraction) other;
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
    public int compareTo(ValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DatetimeSubtraction o = (DatetimeSubtraction) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Datetime extends openGql.grammar.ValueFunction implements Serializable {
    public final openGql.grammar.DatetimeValueFunction value;

    public Datetime (openGql.grammar.DatetimeValueFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Datetime)) {
        return false;
      }
      Datetime o = (Datetime) other;
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
    public int compareTo(ValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Datetime o = (Datetime) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Duration extends openGql.grammar.ValueFunction implements Serializable {
    public final openGql.grammar.DurationValueFunction value;

    public Duration (openGql.grammar.DurationValueFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Duration)) {
        return false;
      }
      Duration o = (Duration) other;
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
    public int compareTo(ValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Duration o = (Duration) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CharacterOrByteString extends openGql.grammar.ValueFunction implements Serializable {
    public final openGql.grammar.CharacterOrByteStringFunction value;

    public CharacterOrByteString (openGql.grammar.CharacterOrByteStringFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CharacterOrByteString)) {
        return false;
      }
      CharacterOrByteString o = (CharacterOrByteString) other;
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
    public int compareTo(ValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CharacterOrByteString o = (CharacterOrByteString) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class List extends openGql.grammar.ValueFunction implements Serializable {
    public final openGql.grammar.ListValueFunction value;

    public List (openGql.grammar.ListValueFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) other;
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
    public int compareTo(ValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      List o = (List) other;
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
