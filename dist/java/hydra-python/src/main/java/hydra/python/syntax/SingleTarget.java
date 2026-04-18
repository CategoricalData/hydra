// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public abstract class SingleTarget implements Serializable, Comparable<SingleTarget> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.SingleTarget");

  public static final hydra.core.Name SUBSCRIPT_ATTRIBUTE_TARGET = new hydra.core.Name("subscriptAttributeTarget");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name PARENS = new hydra.core.Name("parens");

  private SingleTarget () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(SubscriptAttributeTarget instance) ;

    R visit(Name instance) ;

    R visit(Parens instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SingleTarget instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(SubscriptAttributeTarget instance) {
      return otherwise(instance);
    }

    default R visit(Name instance) {
      return otherwise(instance);
    }

    default R visit(Parens instance) {
      return otherwise(instance);
    }
  }

  public static final class SubscriptAttributeTarget extends hydra.python.syntax.SingleTarget implements Serializable {
    public final hydra.python.syntax.SingleSubscriptAttributeTarget value;

    public SubscriptAttributeTarget (hydra.python.syntax.SingleSubscriptAttributeTarget value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SubscriptAttributeTarget)) {
        return false;
      }
      SubscriptAttributeTarget o = (SubscriptAttributeTarget) other;
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
    public int compareTo(SingleTarget other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SubscriptAttributeTarget o = (SubscriptAttributeTarget) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Name extends hydra.python.syntax.SingleTarget implements Serializable {
    public final hydra.python.syntax.Name value;

    public Name (hydra.python.syntax.Name value) {
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
    public int compareTo(SingleTarget other) {
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

  public static final class Parens extends hydra.python.syntax.SingleTarget implements Serializable {
    public final hydra.python.syntax.SingleTarget value;

    public Parens (hydra.python.syntax.SingleTarget value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parens)) {
        return false;
      }
      Parens o = (Parens) other;
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
    public int compareTo(SingleTarget other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Parens o = (Parens) other;
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
