// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class FullEdgePattern implements Serializable, Comparable<FullEdgePattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.FullEdgePattern");

  public static final hydra.core.Name POINTING_LEFT = new hydra.core.Name("pointingLeft");

  public static final hydra.core.Name UNDIRECTED = new hydra.core.Name("undirected");

  public static final hydra.core.Name POINTING_RIGHT = new hydra.core.Name("pointingRight");

  public static final hydra.core.Name LEFT_OR_UNDIRECTED = new hydra.core.Name("leftOrUndirected");

  public static final hydra.core.Name UNDIRECTED_OR_RIGHT = new hydra.core.Name("undirectedOrRight");

  public static final hydra.core.Name LEFT_OR_RIGHT = new hydra.core.Name("leftOrRight");

  public static final hydra.core.Name ANY_DIRECTION = new hydra.core.Name("anyDirection");

  private FullEdgePattern () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(PointingLeft instance) ;

    R visit(Undirected instance) ;

    R visit(PointingRight instance) ;

    R visit(LeftOrUndirected instance) ;

    R visit(UndirectedOrRight instance) ;

    R visit(LeftOrRight instance) ;

    R visit(AnyDirection instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(FullEdgePattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(PointingLeft instance) {
      return otherwise(instance);
    }

    default R visit(Undirected instance) {
      return otherwise(instance);
    }

    default R visit(PointingRight instance) {
      return otherwise(instance);
    }

    default R visit(LeftOrUndirected instance) {
      return otherwise(instance);
    }

    default R visit(UndirectedOrRight instance) {
      return otherwise(instance);
    }

    default R visit(LeftOrRight instance) {
      return otherwise(instance);
    }

    default R visit(AnyDirection instance) {
      return otherwise(instance);
    }
  }

  public static final class PointingLeft extends openGql.grammar.FullEdgePattern implements Serializable {
    public final openGql.grammar.ElementPatternFiller value;

    public PointingLeft (openGql.grammar.ElementPatternFiller value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PointingLeft)) {
        return false;
      }
      PointingLeft o = (PointingLeft) other;
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
    public int compareTo(FullEdgePattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PointingLeft o = (PointingLeft) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Undirected extends openGql.grammar.FullEdgePattern implements Serializable {
    public final openGql.grammar.ElementPatternFiller value;

    public Undirected (openGql.grammar.ElementPatternFiller value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Undirected)) {
        return false;
      }
      Undirected o = (Undirected) other;
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
    public int compareTo(FullEdgePattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Undirected o = (Undirected) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PointingRight extends openGql.grammar.FullEdgePattern implements Serializable {
    public final openGql.grammar.ElementPatternFiller value;

    public PointingRight (openGql.grammar.ElementPatternFiller value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PointingRight)) {
        return false;
      }
      PointingRight o = (PointingRight) other;
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
    public int compareTo(FullEdgePattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PointingRight o = (PointingRight) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class LeftOrUndirected extends openGql.grammar.FullEdgePattern implements Serializable {
    public final openGql.grammar.ElementPatternFiller value;

    public LeftOrUndirected (openGql.grammar.ElementPatternFiller value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LeftOrUndirected)) {
        return false;
      }
      LeftOrUndirected o = (LeftOrUndirected) other;
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
    public int compareTo(FullEdgePattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LeftOrUndirected o = (LeftOrUndirected) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class UndirectedOrRight extends openGql.grammar.FullEdgePattern implements Serializable {
    public final openGql.grammar.ElementPatternFiller value;

    public UndirectedOrRight (openGql.grammar.ElementPatternFiller value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UndirectedOrRight)) {
        return false;
      }
      UndirectedOrRight o = (UndirectedOrRight) other;
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
    public int compareTo(FullEdgePattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UndirectedOrRight o = (UndirectedOrRight) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class LeftOrRight extends openGql.grammar.FullEdgePattern implements Serializable {
    public final openGql.grammar.ElementPatternFiller value;

    public LeftOrRight (openGql.grammar.ElementPatternFiller value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LeftOrRight)) {
        return false;
      }
      LeftOrRight o = (LeftOrRight) other;
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
    public int compareTo(FullEdgePattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LeftOrRight o = (LeftOrRight) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class AnyDirection extends openGql.grammar.FullEdgePattern implements Serializable {
    public final openGql.grammar.ElementPatternFiller value;

    public AnyDirection (openGql.grammar.ElementPatternFiller value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnyDirection)) {
        return false;
      }
      AnyDirection o = (AnyDirection) other;
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
    public int compareTo(FullEdgePattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AnyDirection o = (AnyDirection) other;
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
