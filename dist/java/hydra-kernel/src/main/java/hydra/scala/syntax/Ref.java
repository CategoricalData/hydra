// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public abstract class Ref implements Serializable, Comparable<Ref> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Ref");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name INIT = new hydra.core.Name("init");

  private Ref () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Name instance) ;

    R visit(Init instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Ref instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Name instance) {
      return otherwise(instance);
    }

    default R visit(Init instance) {
      return otherwise(instance);
    }
  }

  public static final class Name extends hydra.scala.syntax.Ref implements Serializable {
    public final hydra.scala.syntax.Name value;

    public Name (hydra.scala.syntax.Name value) {
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
    public int compareTo(Ref other) {
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

  public static final class Init extends hydra.scala.syntax.Ref implements Serializable {
    public final hydra.scala.syntax.Init value;

    public Init (hydra.scala.syntax.Init value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Init)) {
        return false;
      }
      Init o = (Init) other;
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
    public int compareTo(Ref other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Init o = (Init) other;
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
