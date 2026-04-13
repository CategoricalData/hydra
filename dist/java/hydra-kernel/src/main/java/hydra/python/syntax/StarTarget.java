// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public abstract class StarTarget implements Serializable, Comparable<StarTarget> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.StarTarget");

  public static final hydra.core.Name STARRED = new hydra.core.Name("starred");

  public static final hydra.core.Name UNSTARRED = new hydra.core.Name("unstarred");

  private StarTarget () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Starred instance) ;

    R visit(Unstarred instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StarTarget instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Starred instance) {
      return otherwise(instance);
    }

    default R visit(Unstarred instance) {
      return otherwise(instance);
    }
  }

  public static final class Starred extends hydra.python.syntax.StarTarget implements Serializable {
    public final hydra.python.syntax.StarTarget value;

    public Starred (hydra.python.syntax.StarTarget value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Starred)) {
        return false;
      }
      Starred o = (Starred) other;
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
    public int compareTo(StarTarget other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Starred o = (Starred) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Unstarred extends hydra.python.syntax.StarTarget implements Serializable {
    public final hydra.python.syntax.TargetWithStarAtom value;

    public Unstarred (hydra.python.syntax.TargetWithStarAtom value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unstarred)) {
        return false;
      }
      Unstarred o = (Unstarred) other;
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
    public int compareTo(StarTarget other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Unstarred o = (Unstarred) other;
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
