// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class OneTerm implements Serializable, Comparable<OneTerm> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.OneTerm");

  public static final hydra.core.Name EXPLICIT = new hydra.core.Name("explicit");

  public static final hydra.core.Name TERM1 = new hydra.core.Name("term1");

  private OneTerm () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Explicit instance) ;

    R visit(Term1 instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OneTerm instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Explicit instance) {
      return otherwise(instance);
    }

    default R visit(Term1 instance) {
      return otherwise(instance);
    }
  }

  public static final class Explicit extends hydra.coq.syntax.OneTerm implements Serializable {
    public final hydra.coq.syntax.QualidAnnotated value;

    public Explicit (hydra.coq.syntax.QualidAnnotated value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Explicit)) {
        return false;
      }
      Explicit o = (Explicit) other;
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
    public int compareTo(OneTerm other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Explicit o = (Explicit) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Term1 extends hydra.coq.syntax.OneTerm implements Serializable {
    public final hydra.coq.syntax.Term1 value;

    public Term1 (hydra.coq.syntax.Term1 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term1)) {
        return false;
      }
      Term1 o = (Term1) other;
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
    public int compareTo(OneTerm other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Term1 o = (Term1) other;
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
