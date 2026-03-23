// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public abstract class Data_Ref implements Serializable, Comparable<Data_Ref> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Data_Ref");

  public static final hydra.core.Name THIS = new hydra.core.Name("this");

  public static final hydra.core.Name SUPER = new hydra.core.Name("super");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name ANONYMOUS = new hydra.core.Name("anonymous");

  public static final hydra.core.Name SELECT = new hydra.core.Name("select");

  public static final hydra.core.Name APPLY_UNARY = new hydra.core.Name("applyUnary");

  private Data_Ref () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(This instance) ;

    R visit(Super instance) ;

    R visit(Name instance) ;

    R visit(Anonymous instance) ;

    R visit(Select instance) ;

    R visit(ApplyUnary instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Data_Ref instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(This instance) {
      return otherwise(instance);
    }

    default R visit(Super instance) {
      return otherwise(instance);
    }

    default R visit(Name instance) {
      return otherwise(instance);
    }

    default R visit(Anonymous instance) {
      return otherwise(instance);
    }

    default R visit(Select instance) {
      return otherwise(instance);
    }

    default R visit(ApplyUnary instance) {
      return otherwise(instance);
    }
  }

  public static final class This extends hydra.ext.scala.meta.Data_Ref implements Serializable {
    public final hydra.ext.scala.meta.Data_This value;

    public This (hydra.ext.scala.meta.Data_This value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof This)) {
        return false;
      }
      This o = (This) other;
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
    public int compareTo(Data_Ref other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      This o = (This) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Super extends hydra.ext.scala.meta.Data_Ref implements Serializable {
    public final hydra.ext.scala.meta.Data_Super value;

    public Super (hydra.ext.scala.meta.Data_Super value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Super)) {
        return false;
      }
      Super o = (Super) other;
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
    public int compareTo(Data_Ref other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Super o = (Super) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Name extends hydra.ext.scala.meta.Data_Ref implements Serializable {
    public final hydra.ext.scala.meta.Data_Name value;

    public Name (hydra.ext.scala.meta.Data_Name value) {
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
    public int compareTo(Data_Ref other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Name o = (Name) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Anonymous extends hydra.ext.scala.meta.Data_Ref implements Serializable {
    public final hydra.ext.scala.meta.Data_Anonymous value;

    public Anonymous (hydra.ext.scala.meta.Data_Anonymous value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Anonymous)) {
        return false;
      }
      Anonymous o = (Anonymous) other;
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
    public int compareTo(Data_Ref other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Anonymous o = (Anonymous) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Select extends hydra.ext.scala.meta.Data_Ref implements Serializable {
    public final hydra.ext.scala.meta.Data_Select value;

    public Select (hydra.ext.scala.meta.Data_Select value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Select)) {
        return false;
      }
      Select o = (Select) other;
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
    public int compareTo(Data_Ref other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Select o = (Select) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ApplyUnary extends hydra.ext.scala.meta.Data_Ref implements Serializable {
    public final hydra.ext.scala.meta.Data_ApplyUnary value;

    public ApplyUnary (hydra.ext.scala.meta.Data_ApplyUnary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ApplyUnary)) {
        return false;
      }
      ApplyUnary o = (ApplyUnary) other;
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
    public int compareTo(Data_Ref other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ApplyUnary o = (ApplyUnary) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
