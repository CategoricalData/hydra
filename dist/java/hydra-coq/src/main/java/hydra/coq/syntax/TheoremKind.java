// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * The kind of theorem command
 */
public abstract class TheoremKind implements Serializable, Comparable<TheoremKind> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.TheoremKind");

  public static final hydra.core.Name THEOREM = new hydra.core.Name("theorem");

  public static final hydra.core.Name LEMMA = new hydra.core.Name("lemma");

  public static final hydra.core.Name PROPOSITION = new hydra.core.Name("proposition");

  public static final hydra.core.Name COROLLARY = new hydra.core.Name("corollary");

  public static final hydra.core.Name EXAMPLE = new hydra.core.Name("example");

  private TheoremKind () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Theorem instance) ;

    R visit(Lemma instance) ;

    R visit(Proposition instance) ;

    R visit(Corollary instance) ;

    R visit(Example instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TheoremKind instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Theorem instance) {
      return otherwise(instance);
    }

    default R visit(Lemma instance) {
      return otherwise(instance);
    }

    default R visit(Proposition instance) {
      return otherwise(instance);
    }

    default R visit(Corollary instance) {
      return otherwise(instance);
    }

    default R visit(Example instance) {
      return otherwise(instance);
    }
  }

  public static final class Theorem extends hydra.coq.syntax.TheoremKind implements Serializable {
    public Theorem () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Theorem)) {
        return false;
      }
      Theorem o = (Theorem) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TheoremKind other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Lemma extends hydra.coq.syntax.TheoremKind implements Serializable {
    public Lemma () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lemma)) {
        return false;
      }
      Lemma o = (Lemma) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TheoremKind other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Proposition extends hydra.coq.syntax.TheoremKind implements Serializable {
    public Proposition () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Proposition)) {
        return false;
      }
      Proposition o = (Proposition) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TheoremKind other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Corollary extends hydra.coq.syntax.TheoremKind implements Serializable {
    public Corollary () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Corollary)) {
        return false;
      }
      Corollary o = (Corollary) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TheoremKind other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Example extends hydra.coq.syntax.TheoremKind implements Serializable {
    public Example () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Example)) {
        return false;
      }
      Example o = (Example) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TheoremKind other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
