// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class Arg implements Serializable, Comparable<Arg> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Arg");

  public static final hydra.core.Name IDENT = new hydra.core.Name("ident");

  public static final hydra.core.Name NATURAL = new hydra.core.Name("natural");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  private Arg () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Ident instance) ;

    R visit(Natural instance) ;

    R visit(Term instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Arg instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Ident instance) {
      return otherwise(instance);
    }

    default R visit(Natural instance) {
      return otherwise(instance);
    }

    default R visit(Term instance) {
      return otherwise(instance);
    }
  }

  public static final class Ident extends hydra.coq.syntax.Arg implements Serializable {
    public final hydra.coq.syntax.IdentArg value;

    public Ident (hydra.coq.syntax.IdentArg value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ident)) {
        return false;
      }
      Ident o = (Ident) other;
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
    public int compareTo(Arg other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Ident o = (Ident) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Natural extends hydra.coq.syntax.Arg implements Serializable {
    public final hydra.coq.syntax.NaturalArg value;

    public Natural (hydra.coq.syntax.NaturalArg value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Natural)) {
        return false;
      }
      Natural o = (Natural) other;
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
    public int compareTo(Arg other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Natural o = (Natural) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Term extends hydra.coq.syntax.Arg implements Serializable {
    public final hydra.coq.syntax.Term1 value;

    public Term (hydra.coq.syntax.Term1 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term)) {
        return false;
      }
      Term o = (Term) other;
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
    public int compareTo(Arg other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Term o = (Term) other;
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
