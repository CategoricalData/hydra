// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class Pattern implements Serializable, Comparable<Pattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Pattern");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  private Pattern () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Pattern_ instance) ;

    R visit(Term instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Pattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Pattern_ instance) {
      return otherwise(instance);
    }

    default R visit(Term instance) {
      return otherwise(instance);
    }
  }

  public static final class Pattern_ extends hydra.coq.syntax.Pattern implements Serializable {
    public final hydra.coq.syntax.Pattern10 value;

    public Pattern_ (hydra.coq.syntax.Pattern10 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pattern_)) {
        return false;
      }
      Pattern_ o = (Pattern_) other;
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
    public int compareTo(Pattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Pattern_ o = (Pattern_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Term extends hydra.coq.syntax.Pattern implements Serializable {
    public final hydra.util.Maybe<hydra.coq.syntax.Term> value;

    public Term (hydra.util.Maybe<hydra.coq.syntax.Term> value) {
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
    public int compareTo(Pattern other) {
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
