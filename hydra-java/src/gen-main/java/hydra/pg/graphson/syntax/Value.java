// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public abstract class Value implements Serializable, Comparable<Value> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.graphson.syntax.Value");

  public static final hydra.core.Name BIG_DECIMAL = new hydra.core.Name("bigDecimal");

  public static final hydra.core.Name BIG_INTEGER = new hydra.core.Name("bigInteger");

  public static final hydra.core.Name BINARY = new hydra.core.Name("binary");

  public static final hydra.core.Name BOOLEAN = new hydra.core.Name("boolean");

  public static final hydra.core.Name BYTE = new hydra.core.Name("byte");

  public static final hydra.core.Name CHAR = new hydra.core.Name("char");

  public static final hydra.core.Name COMPOSITE = new hydra.core.Name("composite");

  public static final hydra.core.Name DATE_TIME = new hydra.core.Name("dateTime");

  public static final hydra.core.Name DOUBLE = new hydra.core.Name("double");

  public static final hydra.core.Name DURATION = new hydra.core.Name("duration");

  public static final hydra.core.Name FLOAT = new hydra.core.Name("float");

  public static final hydra.core.Name INTEGER = new hydra.core.Name("integer");

  public static final hydra.core.Name LIST = new hydra.core.Name("list");

  public static final hydra.core.Name LONG = new hydra.core.Name("long");

  public static final hydra.core.Name MAP = new hydra.core.Name("map");

  public static final hydra.core.Name NULL = new hydra.core.Name("null");

  public static final hydra.core.Name PRIMITIVE = new hydra.core.Name("primitive");

  public static final hydra.core.Name SET = new hydra.core.Name("set");

  public static final hydra.core.Name SHORT = new hydra.core.Name("short");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  public static final hydra.core.Name UUID = new hydra.core.Name("uuid");

  private Value () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(BigDecimal instance) ;

    R visit(BigInteger instance) ;

    R visit(Binary instance) ;

    R visit(Boolean_ instance) ;

    R visit(Byte_ instance) ;

    R visit(Char instance) ;

    R visit(Composite instance) ;

    R visit(DateTime instance) ;

    R visit(Double_ instance) ;

    R visit(Duration instance) ;

    R visit(Float_ instance) ;

    R visit(Integer_ instance) ;

    R visit(List instance) ;

    R visit(Long_ instance) ;

    R visit(Map instance) ;

    R visit(Null instance) ;

    R visit(Primitive instance) ;

    R visit(Set instance) ;

    R visit(Short_ instance) ;

    R visit(String_ instance) ;

    R visit(Uuid instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Value instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(BigDecimal instance) {
      return otherwise(instance);
    }

    default R visit(BigInteger instance) {
      return otherwise(instance);
    }

    default R visit(Binary instance) {
      return otherwise(instance);
    }

    default R visit(Boolean_ instance) {
      return otherwise(instance);
    }

    default R visit(Byte_ instance) {
      return otherwise(instance);
    }

    default R visit(Char instance) {
      return otherwise(instance);
    }

    default R visit(Composite instance) {
      return otherwise(instance);
    }

    default R visit(DateTime instance) {
      return otherwise(instance);
    }

    default R visit(Double_ instance) {
      return otherwise(instance);
    }

    default R visit(Duration instance) {
      return otherwise(instance);
    }

    default R visit(Float_ instance) {
      return otherwise(instance);
    }

    default R visit(Integer_ instance) {
      return otherwise(instance);
    }

    default R visit(List instance) {
      return otherwise(instance);
    }

    default R visit(Long_ instance) {
      return otherwise(instance);
    }

    default R visit(Map instance) {
      return otherwise(instance);
    }

    default R visit(Null instance) {
      return otherwise(instance);
    }

    default R visit(Primitive instance) {
      return otherwise(instance);
    }

    default R visit(Set instance) {
      return otherwise(instance);
    }

    default R visit(Short_ instance) {
      return otherwise(instance);
    }

    default R visit(String_ instance) {
      return otherwise(instance);
    }

    default R visit(Uuid instance) {
      return otherwise(instance);
    }
  }

  public static final class BigDecimal extends hydra.pg.graphson.syntax.Value implements Serializable {
    public final hydra.pg.graphson.syntax.BigDecimalValue value;

    public BigDecimal (hydra.pg.graphson.syntax.BigDecimalValue value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BigDecimal)) {
        return false;
      }
      BigDecimal o = (BigDecimal) other;
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BigDecimal o = (BigDecimal) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class BigInteger extends hydra.pg.graphson.syntax.Value implements Serializable {
    public final java.math.BigInteger value;

    public BigInteger (java.math.BigInteger value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BigInteger)) {
        return false;
      }
      BigInteger o = (BigInteger) other;
      return this.value.compareTo(o.value) == 0;
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BigInteger o = (BigInteger) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Binary extends hydra.pg.graphson.syntax.Value implements Serializable {
    public final String value;

    public Binary (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Binary)) {
        return false;
      }
      Binary o = (Binary) other;
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Binary o = (Binary) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Boolean_ extends hydra.pg.graphson.syntax.Value implements Serializable {
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Boolean_ o = (Boolean_) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Byte_ extends hydra.pg.graphson.syntax.Value implements Serializable {
    public final Short value;

    public Byte_ (Short value) {
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Byte_ o = (Byte_) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Char extends hydra.pg.graphson.syntax.Value implements Serializable {
    public final Long value;

    public Char (Long value) {
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Char o = (Char) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Composite extends hydra.pg.graphson.syntax.Value implements Serializable {
    public final hydra.pg.graphson.syntax.CompositeTypedValue value;

    public Composite (hydra.pg.graphson.syntax.CompositeTypedValue value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Composite)) {
        return false;
      }
      Composite o = (Composite) other;
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Composite o = (Composite) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DateTime extends hydra.pg.graphson.syntax.Value implements Serializable {
    public final hydra.pg.graphson.syntax.DateTime value;

    public DateTime (hydra.pg.graphson.syntax.DateTime value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DateTime)) {
        return false;
      }
      DateTime o = (DateTime) other;
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DateTime o = (DateTime) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Double_ extends hydra.pg.graphson.syntax.Value implements Serializable {
    public final hydra.pg.graphson.syntax.DoubleValue value;

    public Double_ (hydra.pg.graphson.syntax.DoubleValue value) {
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Double_ o = (Double_) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Duration extends hydra.pg.graphson.syntax.Value implements Serializable {
    public final hydra.pg.graphson.syntax.Duration value;

    public Duration (hydra.pg.graphson.syntax.Duration value) {
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Duration o = (Duration) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Float_ extends hydra.pg.graphson.syntax.Value implements Serializable {
    public final hydra.pg.graphson.syntax.FloatValue value;

    public Float_ (hydra.pg.graphson.syntax.FloatValue value) {
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Float_ o = (Float_) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Integer_ extends hydra.pg.graphson.syntax.Value implements Serializable {
    public final Integer value;

    public Integer_ (Integer value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Integer_)) {
        return false;
      }
      Integer_ o = (Integer_) other;
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Integer_ o = (Integer_) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class List extends hydra.pg.graphson.syntax.Value implements Serializable {
    public final hydra.util.ConsList<hydra.pg.graphson.syntax.Value> value;

    public List (hydra.util.ConsList<hydra.pg.graphson.syntax.Value> value) {
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      List o = (List) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Long_ extends hydra.pg.graphson.syntax.Value implements Serializable {
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Long_ o = (Long_) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Map extends hydra.pg.graphson.syntax.Value implements Serializable {
    public final hydra.pg.graphson.syntax.Map value;

    public Map (hydra.pg.graphson.syntax.Map value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) other;
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Map o = (Map) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Null extends hydra.pg.graphson.syntax.Value implements Serializable {
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
    public int compareTo(Value other) {
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

  public static final class Primitive extends hydra.pg.graphson.syntax.Value implements Serializable {
    public final hydra.pg.graphson.syntax.PrimitiveTypedValue value;

    public Primitive (hydra.pg.graphson.syntax.PrimitiveTypedValue value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primitive)) {
        return false;
      }
      Primitive o = (Primitive) other;
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Primitive o = (Primitive) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Set extends hydra.pg.graphson.syntax.Value implements Serializable {
    public final hydra.util.ConsList<hydra.pg.graphson.syntax.Value> value;

    public Set (hydra.util.ConsList<hydra.pg.graphson.syntax.Value> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) other;
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Set o = (Set) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Short_ extends hydra.pg.graphson.syntax.Value implements Serializable {
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Short_ o = (Short_) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class String_ extends hydra.pg.graphson.syntax.Value implements Serializable {
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      String_ o = (String_) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Uuid extends hydra.pg.graphson.syntax.Value implements Serializable {
    public final hydra.pg.graphson.syntax.Uuid value;

    public Uuid (hydra.pg.graphson.syntax.Uuid value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uuid)) {
        return false;
      }
      Uuid o = (Uuid) other;
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
    public int compareTo(Value other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Uuid o = (Uuid) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
