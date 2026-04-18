// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class PrimitiveNotations implements Serializable, Comparable<PrimitiveNotations> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.PrimitiveNotations");

  public static final hydra.core.Name NUMBER = new hydra.core.Name("number");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  private PrimitiveNotations () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Number_ instance) ;

    R visit(String_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PrimitiveNotations instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Number_ instance) {
      return otherwise(instance);
    }

    default R visit(String_ instance) {
      return otherwise(instance);
    }
  }

  public static final class Number_ extends hydra.coq.syntax.PrimitiveNotations implements Serializable {
    public final hydra.coq.syntax.Number_ value;

    public Number_ (hydra.coq.syntax.Number_ value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Number_)) {
        return false;
      }
      Number_ o = (Number_) other;
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
    public int compareTo(PrimitiveNotations other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Number_ o = (Number_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class String_ extends hydra.coq.syntax.PrimitiveNotations implements Serializable {
    public final hydra.coq.syntax.String_ value;

    public String_ (hydra.coq.syntax.String_ value) {
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
    public int compareTo(PrimitiveNotations other) {
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
}
