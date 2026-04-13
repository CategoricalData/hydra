// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public abstract class DoubleStarredKvpair implements Serializable, Comparable<DoubleStarredKvpair> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.DoubleStarredKvpair");

  public static final hydra.core.Name STARRED = new hydra.core.Name("starred");

  public static final hydra.core.Name PAIR = new hydra.core.Name("pair");

  private DoubleStarredKvpair () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Starred instance) ;

    R visit(Pair instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DoubleStarredKvpair instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Starred instance) {
      return otherwise(instance);
    }

    default R visit(Pair instance) {
      return otherwise(instance);
    }
  }

  public static final class Starred extends hydra.python.syntax.DoubleStarredKvpair implements Serializable {
    public final hydra.python.syntax.BitwiseOr value;

    public Starred (hydra.python.syntax.BitwiseOr value) {
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
    public int compareTo(DoubleStarredKvpair other) {
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

  public static final class Pair extends hydra.python.syntax.DoubleStarredKvpair implements Serializable {
    public final hydra.python.syntax.Kvpair value;

    public Pair (hydra.python.syntax.Kvpair value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pair)) {
        return false;
      }
      Pair o = (Pair) other;
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
    public int compareTo(DoubleStarredKvpair other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Pair o = (Pair) other;
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
