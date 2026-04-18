// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class Universe implements Serializable, Comparable<Universe> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Universe");

  public static final hydra.core.Name MAX = new hydra.core.Name("max");

  public static final hydra.core.Name EXPR = new hydra.core.Name("expr");

  private Universe () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Max instance) ;

    R visit(Expr instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Universe instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Max instance) {
      return otherwise(instance);
    }

    default R visit(Expr instance) {
      return otherwise(instance);
    }
  }

  public static final class Max extends hydra.coq.syntax.Universe implements Serializable {
    public final java.util.List<hydra.coq.syntax.Universe_Expr> value;

    public Max (java.util.List<hydra.coq.syntax.Universe_Expr> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Max)) {
        return false;
      }
      Max o = (Max) other;
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
    public int compareTo(Universe other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Max o = (Max) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Expr extends hydra.coq.syntax.Universe implements Serializable {
    public final hydra.coq.syntax.Universe_Expr value;

    public Expr (hydra.coq.syntax.Universe_Expr value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Expr)) {
        return false;
      }
      Expr o = (Expr) other;
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
    public int compareTo(Universe other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Expr o = (Expr) other;
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
