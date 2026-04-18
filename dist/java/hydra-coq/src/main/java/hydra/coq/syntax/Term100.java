// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class Term100 implements Serializable, Comparable<Term100> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Term100");

  public static final hydra.core.Name CAST = new hydra.core.Name("cast");

  public static final hydra.core.Name TERM10 = new hydra.core.Name("term10");

  private Term100 () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Cast instance) ;

    R visit(Term10 instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Term100 instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Cast instance) {
      return otherwise(instance);
    }

    default R visit(Term10 instance) {
      return otherwise(instance);
    }
  }

  public static final class Cast extends hydra.coq.syntax.Term100 implements Serializable {
    public final hydra.coq.syntax.TypeCast value;

    public Cast (hydra.coq.syntax.TypeCast value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cast)) {
        return false;
      }
      Cast o = (Cast) other;
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
    public int compareTo(Term100 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Cast o = (Cast) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Term10 extends hydra.coq.syntax.Term100 implements Serializable {
    public final hydra.coq.syntax.Term10 value;

    public Term10 (hydra.coq.syntax.Term10 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term10)) {
        return false;
      }
      Term10 o = (Term10) other;
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
    public int compareTo(Term100 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Term10 o = (Term10) other;
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
