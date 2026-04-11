// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public abstract class Lit implements Serializable, Comparable<Lit> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Lit");

  public static final hydra.core.Name NULL = new hydra.core.Name("null");

  public static final hydra.core.Name INT = new hydra.core.Name("int");

  public static final hydra.core.Name DOUBLE = new hydra.core.Name("double");

  public static final hydra.core.Name FLOAT = new hydra.core.Name("float");

  public static final hydra.core.Name BYTE = new hydra.core.Name("byte");

  public static final hydra.core.Name SHORT = new hydra.core.Name("short");

  public static final hydra.core.Name CHAR = new hydra.core.Name("char");

  public static final hydra.core.Name LONG = new hydra.core.Name("long");

  public static final hydra.core.Name BOOLEAN = new hydra.core.Name("boolean");

  public static final hydra.core.Name UNIT = new hydra.core.Name("unit");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  public static final hydra.core.Name BYTES = new hydra.core.Name("bytes");

  public static final hydra.core.Name SYMBOL = new hydra.core.Name("symbol");

  private Lit () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Null instance) ;

    R visit(Int instance) ;

    R visit(Double_ instance) ;

    R visit(Float_ instance) ;

    R visit(Byte_ instance) ;

    R visit(Short_ instance) ;

    R visit(Char instance) ;

    R visit(Long_ instance) ;

    R visit(Boolean_ instance) ;

    R visit(Unit instance) ;

    R visit(String_ instance) ;

    R visit(Bytes instance) ;

    R visit(Symbol instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Lit instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Null instance) {
      return otherwise(instance);
    }

    default R visit(Int instance) {
      return otherwise(instance);
    }

    default R visit(Double_ instance) {
      return otherwise(instance);
    }

    default R visit(Float_ instance) {
      return otherwise(instance);
    }

    default R visit(Byte_ instance) {
      return otherwise(instance);
    }

    default R visit(Short_ instance) {
      return otherwise(instance);
    }

    default R visit(Char instance) {
      return otherwise(instance);
    }

    default R visit(Long_ instance) {
      return otherwise(instance);
    }

    default R visit(Boolean_ instance) {
      return otherwise(instance);
    }

    default R visit(Unit instance) {
      return otherwise(instance);
    }

    default R visit(String_ instance) {
      return otherwise(instance);
    }

    default R visit(Bytes instance) {
      return otherwise(instance);
    }

    default R visit(Symbol instance) {
      return otherwise(instance);
    }
  }

  public static final class Null extends hydra.ext.scala.syntax.Lit implements Serializable {
    public Null () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Null)) {
        return false;
      }
      Null o = (Null) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Lit other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Int extends hydra.ext.scala.syntax.Lit implements Serializable {
    public final Integer value;

    public Int (Integer value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int)) {
        return false;
      }
      Int o = (Int) other;
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
    public int compareTo(Lit other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Int o = (Int) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Double_ extends hydra.ext.scala.syntax.Lit implements Serializable {
    public final Double value;

    public Double_ (Double value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Double_)) {
        return false;
      }
      Double_ o = (Double_) other;
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
    public int compareTo(Lit other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Double_ o = (Double_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Float_ extends hydra.ext.scala.syntax.Lit implements Serializable {
    public final Float value;

    public Float_ (Float value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float_)) {
        return false;
      }
      Float_ o = (Float_) other;
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
    public int compareTo(Lit other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Float_ o = (Float_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Byte_ extends hydra.ext.scala.syntax.Lit implements Serializable {
    public final Byte value;

    public Byte_ (Byte value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Byte_)) {
        return false;
      }
      Byte_ o = (Byte_) other;
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
    public int compareTo(Lit other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Byte_ o = (Byte_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Short_ extends hydra.ext.scala.syntax.Lit implements Serializable {
    public final Short value;

    public Short_ (Short value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Short_)) {
        return false;
      }
      Short_ o = (Short_) other;
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
    public int compareTo(Lit other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Short_ o = (Short_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Char extends hydra.ext.scala.syntax.Lit implements Serializable {
    public final Character value;

    public Char (Character value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Char)) {
        return false;
      }
      Char o = (Char) other;
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
    public int compareTo(Lit other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Char o = (Char) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Long_ extends hydra.ext.scala.syntax.Lit implements Serializable {
    public final Long value;

    public Long_ (Long value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Long_)) {
        return false;
      }
      Long_ o = (Long_) other;
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
    public int compareTo(Lit other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Long_ o = (Long_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Boolean_ extends hydra.ext.scala.syntax.Lit implements Serializable {
    public final Boolean value;

    public Boolean_ (Boolean value) {
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
    public int compareTo(Lit other) {
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

  public static final class Unit extends hydra.ext.scala.syntax.Lit implements Serializable {
    public Unit () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unit)) {
        return false;
      }
      Unit o = (Unit) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Lit other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class String_ extends hydra.ext.scala.syntax.Lit implements Serializable {
    public final String value;

    public String_ (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) other;
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
    public int compareTo(Lit other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      String_ o = (String_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Bytes extends hydra.ext.scala.syntax.Lit implements Serializable {
    public final java.util.List<Integer> value;

    public Bytes (java.util.List<Integer> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bytes)) {
        return false;
      }
      Bytes o = (Bytes) other;
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
    public int compareTo(Lit other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Bytes o = (Bytes) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Symbol extends hydra.ext.scala.syntax.Lit implements Serializable {
    public final hydra.ext.scala.syntax.ScalaSymbol value;

    public Symbol (hydra.ext.scala.syntax.ScalaSymbol value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Symbol)) {
        return false;
      }
      Symbol o = (Symbol) other;
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
    public int compareTo(Lit other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Symbol o = (Symbol) other;
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
