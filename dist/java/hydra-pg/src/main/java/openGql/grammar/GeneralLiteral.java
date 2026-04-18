// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class GeneralLiteral implements Serializable, Comparable<GeneralLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.GeneralLiteral");

  public static final hydra.core.Name BOOLEAN = new hydra.core.Name("boolean");

  public static final hydra.core.Name CHARACTER_STRING = new hydra.core.Name("characterString");

  public static final hydra.core.Name BYTE_STRING = new hydra.core.Name("byteString");

  public static final hydra.core.Name TEMPORAL = new hydra.core.Name("temporal");

  public static final hydra.core.Name DURATION = new hydra.core.Name("duration");

  public static final hydra.core.Name NULL_LITERAL = new hydra.core.Name("nullLiteral");

  public static final hydra.core.Name LIST = new hydra.core.Name("list");

  public static final hydra.core.Name RECORD = new hydra.core.Name("record");

  private GeneralLiteral () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Boolean_ instance) ;

    R visit(CharacterString instance) ;

    R visit(ByteString instance) ;

    R visit(Temporal instance) ;

    R visit(Duration instance) ;

    R visit(NullLiteral instance) ;

    R visit(List instance) ;

    R visit(Record instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GeneralLiteral instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Boolean_ instance) {
      return otherwise(instance);
    }

    default R visit(CharacterString instance) {
      return otherwise(instance);
    }

    default R visit(ByteString instance) {
      return otherwise(instance);
    }

    default R visit(Temporal instance) {
      return otherwise(instance);
    }

    default R visit(Duration instance) {
      return otherwise(instance);
    }

    default R visit(NullLiteral instance) {
      return otherwise(instance);
    }

    default R visit(List instance) {
      return otherwise(instance);
    }

    default R visit(Record instance) {
      return otherwise(instance);
    }
  }

  public static final class Boolean_ extends openGql.grammar.GeneralLiteral implements Serializable {
    public final openGql.grammar.BooleanLiteral value;

    public Boolean_ (openGql.grammar.BooleanLiteral value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Boolean_)) {
        return false;
      }
      Boolean_ o = (Boolean_) other;
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
    public int compareTo(GeneralLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Boolean_ o = (Boolean_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CharacterString extends openGql.grammar.GeneralLiteral implements Serializable {
    public final String value;

    public CharacterString (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CharacterString)) {
        return false;
      }
      CharacterString o = (CharacterString) other;
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
    public int compareTo(GeneralLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CharacterString o = (CharacterString) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ByteString extends openGql.grammar.GeneralLiteral implements Serializable {
    public final String value;

    public ByteString (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ByteString)) {
        return false;
      }
      ByteString o = (ByteString) other;
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
    public int compareTo(GeneralLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ByteString o = (ByteString) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Temporal extends openGql.grammar.GeneralLiteral implements Serializable {
    public final openGql.grammar.TemporalLiteral value;

    public Temporal (openGql.grammar.TemporalLiteral value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Temporal)) {
        return false;
      }
      Temporal o = (Temporal) other;
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
    public int compareTo(GeneralLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Temporal o = (Temporal) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Duration extends openGql.grammar.GeneralLiteral implements Serializable {
    public final String value;

    public Duration (String value) {
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
    public int compareTo(GeneralLiteral other) {
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

  public static final class NullLiteral extends openGql.grammar.GeneralLiteral implements Serializable {
    public final java.lang.Void value;

    public NullLiteral (java.lang.Void value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NullLiteral)) {
        return false;
      }
      NullLiteral o = (NullLiteral) other;
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
    public int compareTo(GeneralLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NullLiteral o = (NullLiteral) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class List extends openGql.grammar.GeneralLiteral implements Serializable {
    public final openGql.grammar.ListValueConstructorByEnumeration value;

    public List (openGql.grammar.ListValueConstructorByEnumeration value) {
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
    public int compareTo(GeneralLiteral other) {
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

  public static final class Record extends openGql.grammar.GeneralLiteral implements Serializable {
    public final hydra.util.Maybe<java.util.List<openGql.grammar.Field>> value;

    public Record (hydra.util.Maybe<java.util.List<openGql.grammar.Field>> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Record)) {
        return false;
      }
      Record o = (Record) other;
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
    public int compareTo(GeneralLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Record o = (Record) other;
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
