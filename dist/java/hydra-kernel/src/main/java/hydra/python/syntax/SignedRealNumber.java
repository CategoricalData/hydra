// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public abstract class SignedRealNumber implements Serializable, Comparable<SignedRealNumber> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.SignedRealNumber");

  public static final hydra.core.Name SIGN = new hydra.core.Name("sign");

  public static final hydra.core.Name NUMBER = new hydra.core.Name("number");

  private SignedRealNumber () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Sign instance) ;

    R visit(Number_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SignedRealNumber instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Sign instance) {
      return otherwise(instance);
    }

    default R visit(Number_ instance) {
      return otherwise(instance);
    }
  }

  public static final class Sign extends hydra.python.syntax.SignedRealNumber implements Serializable {
    public final hydra.python.syntax.PlusOrMinus value;

    public Sign (hydra.python.syntax.PlusOrMinus value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sign)) {
        return false;
      }
      Sign o = (Sign) other;
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
    public int compareTo(SignedRealNumber other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sign o = (Sign) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Number_ extends hydra.python.syntax.SignedRealNumber implements Serializable {
    public final hydra.python.syntax.RealNumber value;

    public Number_ (hydra.python.syntax.RealNumber value) {
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
    public int compareTo(SignedRealNumber other) {
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
}
