// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public abstract class MaybeStarPattern implements Serializable, Comparable<MaybeStarPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.MaybeStarPattern");

  public static final hydra.core.Name STAR = new hydra.core.Name("star");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  private MaybeStarPattern () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Star instance) ;

    R visit(Pattern instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MaybeStarPattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Star instance) {
      return otherwise(instance);
    }

    default R visit(Pattern instance) {
      return otherwise(instance);
    }
  }

  public static final class Star extends hydra.python.syntax.MaybeStarPattern implements Serializable {
    public final hydra.python.syntax.StarPattern value;

    public Star (hydra.python.syntax.StarPattern value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Star)) {
        return false;
      }
      Star o = (Star) other;
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
    public int compareTo(MaybeStarPattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Star o = (Star) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Pattern extends hydra.python.syntax.MaybeStarPattern implements Serializable {
    public final hydra.python.syntax.Pattern value;

    public Pattern (hydra.python.syntax.Pattern value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pattern)) {
        return false;
      }
      Pattern o = (Pattern) other;
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
    public int compareTo(MaybeStarPattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Pattern o = (Pattern) other;
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
