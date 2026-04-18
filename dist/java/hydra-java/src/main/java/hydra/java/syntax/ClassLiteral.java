// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public abstract class ClassLiteral implements Serializable, Comparable<ClassLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.ClassLiteral");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name NUMERIC_TYPE = new hydra.core.Name("numericType");

  public static final hydra.core.Name BOOLEAN = new hydra.core.Name("boolean");

  public static final hydra.core.Name VOID = new hydra.core.Name("void");

  private ClassLiteral () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Type instance) ;

    R visit(NumericType instance) ;

    R visit(Boolean_ instance) ;

    R visit(Void_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClassLiteral instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Type instance) {
      return otherwise(instance);
    }

    default R visit(NumericType instance) {
      return otherwise(instance);
    }

    default R visit(Boolean_ instance) {
      return otherwise(instance);
    }

    default R visit(Void_ instance) {
      return otherwise(instance);
    }
  }

  public static final class Type extends hydra.java.syntax.ClassLiteral implements Serializable {
    public final hydra.java.syntax.TypeNameArray value;

    public Type (hydra.java.syntax.TypeNameArray value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) other;
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
    public int compareTo(ClassLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Type o = (Type) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NumericType extends hydra.java.syntax.ClassLiteral implements Serializable {
    public final hydra.java.syntax.NumericTypeArray value;

    public NumericType (hydra.java.syntax.NumericTypeArray value) {
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
    public int compareTo(ClassLiteral other) {
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

  public static final class Boolean_ extends hydra.java.syntax.ClassLiteral implements Serializable {
    public final hydra.java.syntax.BooleanArray value;

    public Boolean_ (hydra.java.syntax.BooleanArray value) {
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
    public int compareTo(ClassLiteral other) {
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

  public static final class Void_ extends hydra.java.syntax.ClassLiteral implements Serializable {
    public Void_ () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Void_)) {
        return false;
      }
      Void_ o = (Void_) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ClassLiteral other) {
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
}
