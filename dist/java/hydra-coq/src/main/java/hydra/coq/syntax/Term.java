// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class Term implements Serializable, Comparable<Term> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Term");

  public static final hydra.core.Name FORALL_OR_FUN = new hydra.core.Name("forallOrFun");

  public static final hydra.core.Name LET = new hydra.core.Name("let");

  public static final hydra.core.Name IF = new hydra.core.Name("if");

  public static final hydra.core.Name FIX = new hydra.core.Name("fix");

  public static final hydra.core.Name COFIX = new hydra.core.Name("cofix");

  public static final hydra.core.Name TERM100 = new hydra.core.Name("term100");

  private Term () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ForallOrFun instance) ;

    R visit(Let instance) ;

    R visit(If instance) ;

    R visit(Fix instance) ;

    R visit(Cofix instance) ;

    R visit(Term100 instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Term instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ForallOrFun instance) {
      return otherwise(instance);
    }

    default R visit(Let instance) {
      return otherwise(instance);
    }

    default R visit(If instance) {
      return otherwise(instance);
    }

    default R visit(Fix instance) {
      return otherwise(instance);
    }

    default R visit(Cofix instance) {
      return otherwise(instance);
    }

    default R visit(Term100 instance) {
      return otherwise(instance);
    }
  }

  public static final class ForallOrFun extends hydra.coq.syntax.Term implements Serializable {
    public final hydra.coq.syntax.ForallOrFun value;

    public ForallOrFun (hydra.coq.syntax.ForallOrFun value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ForallOrFun)) {
        return false;
      }
      ForallOrFun o = (ForallOrFun) other;
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
    public int compareTo(Term other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ForallOrFun o = (ForallOrFun) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Let extends hydra.coq.syntax.Term implements Serializable {
    public final hydra.coq.syntax.Let value;

    public Let (hydra.coq.syntax.Let value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Let)) {
        return false;
      }
      Let o = (Let) other;
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
    public int compareTo(Term other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Let o = (Let) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class If extends hydra.coq.syntax.Term implements Serializable {
    public final hydra.coq.syntax.If value;

    public If (hydra.coq.syntax.If value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof If)) {
        return false;
      }
      If o = (If) other;
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
    public int compareTo(Term other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      If o = (If) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Fix extends hydra.coq.syntax.Term implements Serializable {
    public final hydra.coq.syntax.Fix value;

    public Fix (hydra.coq.syntax.Fix value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fix)) {
        return false;
      }
      Fix o = (Fix) other;
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
    public int compareTo(Term other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Fix o = (Fix) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Cofix extends hydra.coq.syntax.Term implements Serializable {
    public final hydra.coq.syntax.Cofix value;

    public Cofix (hydra.coq.syntax.Cofix value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cofix)) {
        return false;
      }
      Cofix o = (Cofix) other;
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
    public int compareTo(Term other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Cofix o = (Cofix) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Term100 extends hydra.coq.syntax.Term implements Serializable {
    public final hydra.coq.syntax.Term100 value;

    public Term100 (hydra.coq.syntax.Term100 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term100)) {
        return false;
      }
      Term100 o = (Term100) other;
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
    public int compareTo(Term other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Term100 o = (Term100) other;
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
