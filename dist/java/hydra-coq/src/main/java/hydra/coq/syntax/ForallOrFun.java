// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class ForallOrFun implements Serializable, Comparable<ForallOrFun> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.ForallOrFun");

  public static final hydra.core.Name FORALL = new hydra.core.Name("forall");

  public static final hydra.core.Name FUN = new hydra.core.Name("fun");

  private ForallOrFun () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Forall instance) ;

    R visit(Fun instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ForallOrFun instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Forall instance) {
      return otherwise(instance);
    }

    default R visit(Fun instance) {
      return otherwise(instance);
    }
  }

  public static final class Forall extends hydra.coq.syntax.ForallOrFun implements Serializable {
    public final hydra.coq.syntax.Forall value;

    public Forall (hydra.coq.syntax.Forall value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Forall)) {
        return false;
      }
      Forall o = (Forall) other;
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
    public int compareTo(ForallOrFun other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Forall o = (Forall) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Fun extends hydra.coq.syntax.ForallOrFun implements Serializable {
    public final hydra.coq.syntax.Fun value;

    public Fun (hydra.coq.syntax.Fun value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fun)) {
        return false;
      }
      Fun o = (Fun) other;
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
    public int compareTo(ForallOrFun other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Fun o = (Fun) other;
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
