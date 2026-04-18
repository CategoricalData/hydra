// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class Term10 implements Serializable, Comparable<Term10> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Term10");

  public static final hydra.core.Name APPLICATION = new hydra.core.Name("application");

  public static final hydra.core.Name ONE_TERM = new hydra.core.Name("oneTerm");

  private Term10 () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Application instance) ;

    R visit(OneTerm instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Term10 instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Application instance) {
      return otherwise(instance);
    }

    default R visit(OneTerm instance) {
      return otherwise(instance);
    }
  }

  public static final class Application extends hydra.coq.syntax.Term10 implements Serializable {
    public final hydra.coq.syntax.Application value;

    public Application (hydra.coq.syntax.Application value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Application)) {
        return false;
      }
      Application o = (Application) other;
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
    public int compareTo(Term10 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Application o = (Application) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class OneTerm extends hydra.coq.syntax.Term10 implements Serializable {
    public final hydra.coq.syntax.OneTerm value;

    public OneTerm (hydra.coq.syntax.OneTerm value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OneTerm)) {
        return false;
      }
      OneTerm o = (OneTerm) other;
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
    public int compareTo(Term10 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      OneTerm o = (OneTerm) other;
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
