// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class Pattern10 implements Serializable, Comparable<Pattern10> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Pattern10");

  public static final hydra.core.Name AS = new hydra.core.Name("as");

  public static final hydra.core.Name PATTERNS = new hydra.core.Name("patterns");

  public static final hydra.core.Name QUALIID = new hydra.core.Name("qualiid");

  private Pattern10 () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(As instance) ;

    R visit(Patterns instance) ;

    R visit(Qualiid instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Pattern10 instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(As instance) {
      return otherwise(instance);
    }

    default R visit(Patterns instance) {
      return otherwise(instance);
    }

    default R visit(Qualiid instance) {
      return otherwise(instance);
    }
  }

  public static final class As extends hydra.coq.syntax.Pattern10 implements Serializable {
    public final hydra.coq.syntax.Pattern10_As value;

    public As (hydra.coq.syntax.Pattern10_As value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof As)) {
        return false;
      }
      As o = (As) other;
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
    public int compareTo(Pattern10 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      As o = (As) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Patterns extends hydra.coq.syntax.Pattern10 implements Serializable {
    public final hydra.coq.syntax.Pattern10_Patterns value;

    public Patterns (hydra.coq.syntax.Pattern10_Patterns value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Patterns)) {
        return false;
      }
      Patterns o = (Patterns) other;
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
    public int compareTo(Pattern10 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Patterns o = (Patterns) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Qualiid extends hydra.coq.syntax.Pattern10 implements Serializable {
    public final hydra.coq.syntax.Pattern10_Qualid value;

    public Qualiid (hydra.coq.syntax.Pattern10_Qualid value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Qualiid)) {
        return false;
      }
      Qualiid o = (Qualiid) other;
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
    public int compareTo(Pattern10 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Qualiid o = (Qualiid) other;
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
