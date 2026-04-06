// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public abstract class Ctor implements Serializable, Comparable<Ctor> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Ctor");

  public static final hydra.core.Name PRIMARY = new hydra.core.Name("primary");

  public static final hydra.core.Name SECONDARY = new hydra.core.Name("secondary");

  private Ctor () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Primary instance) ;

    R visit(Secondary instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Ctor instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Primary instance) {
      return otherwise(instance);
    }

    default R visit(Secondary instance) {
      return otherwise(instance);
    }
  }

  public static final class Primary extends hydra.ext.scala.syntax.Ctor implements Serializable {
    public final hydra.ext.scala.syntax.Ctor_Primary value;

    public Primary (hydra.ext.scala.syntax.Ctor_Primary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primary)) {
        return false;
      }
      Primary o = (Primary) other;
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
    public int compareTo(Ctor other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Primary o = (Primary) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Secondary extends hydra.ext.scala.syntax.Ctor implements Serializable {
    public final hydra.ext.scala.syntax.Ctor_Secondary value;

    public Secondary (hydra.ext.scala.syntax.Ctor_Secondary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Secondary)) {
        return false;
      }
      Secondary o = (Secondary) other;
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
    public int compareTo(Ctor other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Secondary o = (Secondary) other;
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
