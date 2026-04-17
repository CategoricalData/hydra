// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class PathSearchPrefix implements Serializable, Comparable<PathSearchPrefix> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PathSearchPrefix");

  public static final hydra.core.Name ALL = new hydra.core.Name("all");

  public static final hydra.core.Name ANY = new hydra.core.Name("any");

  public static final hydra.core.Name SHORTEST = new hydra.core.Name("shortest");

  private PathSearchPrefix () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(All instance) ;

    R visit(Any instance) ;

    R visit(Shortest instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PathSearchPrefix instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(All instance) {
      return otherwise(instance);
    }

    default R visit(Any instance) {
      return otherwise(instance);
    }

    default R visit(Shortest instance) {
      return otherwise(instance);
    }
  }

  public static final class All extends openGql.grammar.PathSearchPrefix implements Serializable {
    public final openGql.grammar.AllPathSearch value;

    public All (openGql.grammar.AllPathSearch value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof All)) {
        return false;
      }
      All o = (All) other;
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
    public int compareTo(PathSearchPrefix other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      All o = (All) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Any extends openGql.grammar.PathSearchPrefix implements Serializable {
    public final openGql.grammar.AnyPathSearch value;

    public Any (openGql.grammar.AnyPathSearch value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Any)) {
        return false;
      }
      Any o = (Any) other;
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
    public int compareTo(PathSearchPrefix other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Any o = (Any) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Shortest extends openGql.grammar.PathSearchPrefix implements Serializable {
    public final openGql.grammar.ShortestPathSearch value;

    public Shortest (openGql.grammar.ShortestPathSearch value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Shortest)) {
        return false;
      }
      Shortest o = (Shortest) other;
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
    public int compareTo(PathSearchPrefix other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Shortest o = (Shortest) other;
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
