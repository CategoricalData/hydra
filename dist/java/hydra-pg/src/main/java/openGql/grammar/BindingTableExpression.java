// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class BindingTableExpression implements Serializable, Comparable<BindingTableExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.BindingTableExpression");

  public static final hydra.core.Name NESTED = new hydra.core.Name("nested");

  public static final hydra.core.Name OBJECT = new hydra.core.Name("object");

  public static final hydra.core.Name TABLE = new hydra.core.Name("table");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  private BindingTableExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Nested instance) ;

    R visit(Object_ instance) ;

    R visit(Table instance) ;

    R visit(Name instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BindingTableExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Nested instance) {
      return otherwise(instance);
    }

    default R visit(Object_ instance) {
      return otherwise(instance);
    }

    default R visit(Table instance) {
      return otherwise(instance);
    }

    default R visit(Name instance) {
      return otherwise(instance);
    }
  }

  public static final class Nested extends openGql.grammar.BindingTableExpression implements Serializable {
    public final java.lang.Void value;

    public Nested (java.lang.Void value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nested)) {
        return false;
      }
      Nested o = (Nested) other;
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
    public int compareTo(BindingTableExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Nested o = (Nested) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Object_ extends openGql.grammar.BindingTableExpression implements Serializable {
    public final openGql.grammar.ObjectExpressionPrimary value;

    public Object_ (openGql.grammar.ObjectExpressionPrimary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Object_)) {
        return false;
      }
      Object_ o = (Object_) other;
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
    public int compareTo(BindingTableExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Object_ o = (Object_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Table extends openGql.grammar.BindingTableExpression implements Serializable {
    public final openGql.grammar.BindingTableReference value;

    public Table (openGql.grammar.BindingTableReference value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Table)) {
        return false;
      }
      Table o = (Table) other;
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
    public int compareTo(BindingTableExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Table o = (Table) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Name extends openGql.grammar.BindingTableExpression implements Serializable {
    public final String value;

    public Name (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Name)) {
        return false;
      }
      Name o = (Name) other;
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
    public int compareTo(BindingTableExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Name o = (Name) other;
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
