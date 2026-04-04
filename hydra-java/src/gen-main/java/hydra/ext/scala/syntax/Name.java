// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public abstract class Name implements Serializable, Comparable<Name> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Name");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name ANONYMOUS = new hydra.core.Name("anonymous");

  public static final hydra.core.Name INDETERMINATE = new hydra.core.Name("indeterminate");

  private Name () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Value instance) ;

    R visit(Anonymous instance) ;

    R visit(Indeterminate instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Name instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Value instance) {
      return otherwise(instance);
    }

    default R visit(Anonymous instance) {
      return otherwise(instance);
    }

    default R visit(Indeterminate instance) {
      return otherwise(instance);
    }
  }

  public static final class Value extends hydra.ext.scala.syntax.Name implements Serializable {
    public final String value;

    public Value (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Value)) {
        return false;
      }
      Value o = (Value) other;
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
    public int compareTo(Name other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Value o = (Value) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Anonymous extends hydra.ext.scala.syntax.Name implements Serializable {
    public Anonymous () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Anonymous)) {
        return false;
      }
      Anonymous o = (Anonymous) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Name other) {
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

  public static final class Indeterminate extends hydra.ext.scala.syntax.Name implements Serializable {
    public final hydra.ext.scala.syntax.PredefString value;

    public Indeterminate (hydra.ext.scala.syntax.PredefString value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Indeterminate)) {
        return false;
      }
      Indeterminate o = (Indeterminate) other;
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
    public int compareTo(Name other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Indeterminate o = (Indeterminate) other;
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
