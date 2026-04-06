// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class StringListNullPredicateRightHandSide implements Serializable, Comparable<StringListNullPredicateRightHandSide> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.StringListNullPredicateRightHandSide");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  public static final hydra.core.Name LIST = new hydra.core.Name("list");

  public static final hydra.core.Name NULL = new hydra.core.Name("null");

  private StringListNullPredicateRightHandSide () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(String_ instance) ;

    R visit(List instance) ;

    R visit(Null instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StringListNullPredicateRightHandSide instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(String_ instance) {
      return otherwise(instance);
    }

    default R visit(List instance) {
      return otherwise(instance);
    }

    default R visit(Null instance) {
      return otherwise(instance);
    }
  }

  public static final class String_ extends hydra.ext.cypher.openCypher.StringListNullPredicateRightHandSide implements Serializable {
    public final hydra.ext.cypher.openCypher.StringPredicateExpression value;

    public String_ (hydra.ext.cypher.openCypher.StringPredicateExpression value) {
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
    public int compareTo(StringListNullPredicateRightHandSide other) {
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

  public static final class List extends hydra.ext.cypher.openCypher.StringListNullPredicateRightHandSide implements Serializable {
    public final hydra.ext.cypher.openCypher.ListPredicateExpression value;

    public List (hydra.ext.cypher.openCypher.ListPredicateExpression value) {
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
    public int compareTo(StringListNullPredicateRightHandSide other) {
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

  public static final class Null extends hydra.ext.cypher.openCypher.StringListNullPredicateRightHandSide implements Serializable {
    public final hydra.ext.cypher.openCypher.NullPredicateExpression value;

    public Null (hydra.ext.cypher.openCypher.NullPredicateExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Null)) {
        return false;
      }
      Null o = (Null) other;
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
    public int compareTo(StringListNullPredicateRightHandSide other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Null o = (Null) other;
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
