// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class ValueMapArgs implements Serializable, Comparable<ValueMapArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ValueMapArgs");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  public static final hydra.core.Name BOOLEAN = new hydra.core.Name("boolean");

  private ValueMapArgs () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(String_ instance) ;

    R visit(Boolean_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ValueMapArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(String_ instance) {
      return otherwise(instance);
    }

    default R visit(Boolean_ instance) {
      return otherwise(instance);
    }
  }

  public static final class String_ extends hydra.ext.org.apache.tinkerpop.gremlin.ValueMapArgs implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value;

    public String_ (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value) {
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
    public int compareTo(ValueMapArgs other) {
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

  public static final class Boolean_ extends hydra.ext.org.apache.tinkerpop.gremlin.ValueMapArgs implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.ValueMapBooleanArgs value;

    public Boolean_ (hydra.ext.org.apache.tinkerpop.gremlin.ValueMapBooleanArgs value) {
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
    public int compareTo(ValueMapArgs other) {
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
}
