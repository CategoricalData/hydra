// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SimplifiedPathPatternExpression implements Serializable, Comparable<SimplifiedPathPatternExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SimplifiedPathPatternExpression");

  public static final hydra.core.Name LEFT = new hydra.core.Name("left");

  public static final hydra.core.Name UNDIRECTED = new hydra.core.Name("undirected");

  public static final hydra.core.Name RIGHT = new hydra.core.Name("right");

  public static final hydra.core.Name LEFT_OR_UNDIRECTED = new hydra.core.Name("leftOrUndirected");

  public static final hydra.core.Name UNDIRECTED_OR_RIGHT = new hydra.core.Name("undirectedOrRight");

  public static final hydra.core.Name LEFT_OR_RIGHT = new hydra.core.Name("leftOrRight");

  public static final hydra.core.Name ANY_DIRECTION = new hydra.core.Name("anyDirection");

  private SimplifiedPathPatternExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Left instance) ;

    R visit(Undirected instance) ;

    R visit(Right instance) ;

    R visit(LeftOrUndirected instance) ;

    R visit(UndirectedOrRight instance) ;

    R visit(LeftOrRight instance) ;

    R visit(AnyDirection instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SimplifiedPathPatternExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Left instance) {
      return otherwise(instance);
    }

    default R visit(Undirected instance) {
      return otherwise(instance);
    }

    default R visit(Right instance) {
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

  public static final class Left extends openGql.grammar.SimplifiedPathPatternExpression implements Serializable {
    public final openGql.grammar.SimplifiedContents value;

    public Left (openGql.grammar.SimplifiedContents value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Left)) {
        return false;
      }
      Left o = (Left) other;
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
    public int compareTo(SimplifiedPathPatternExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Left o = (Left) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Undirected extends openGql.grammar.SimplifiedPathPatternExpression implements Serializable {
    public final openGql.grammar.SimplifiedContents value;

    public Undirected (openGql.grammar.SimplifiedContents value) {
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
    public int compareTo(SimplifiedPathPatternExpression other) {
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

  public static final class Right extends openGql.grammar.SimplifiedPathPatternExpression implements Serializable {
    public final openGql.grammar.SimplifiedContents value;

    public Right (openGql.grammar.SimplifiedContents value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Right)) {
        return false;
      }
      Right o = (Right) other;
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
    public int compareTo(SimplifiedPathPatternExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Right o = (Right) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class LeftOrUndirected extends openGql.grammar.SimplifiedPathPatternExpression implements Serializable {
    public final openGql.grammar.SimplifiedContents value;

    public LeftOrUndirected (openGql.grammar.SimplifiedContents value) {
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
    public int compareTo(SimplifiedPathPatternExpression other) {
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

  public static final class UndirectedOrRight extends openGql.grammar.SimplifiedPathPatternExpression implements Serializable {
    public final openGql.grammar.SimplifiedContents value;

    public UndirectedOrRight (openGql.grammar.SimplifiedContents value) {
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
    public int compareTo(SimplifiedPathPatternExpression other) {
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

  public static final class LeftOrRight extends openGql.grammar.SimplifiedPathPatternExpression implements Serializable {
    public final openGql.grammar.SimplifiedContents value;

    public LeftOrRight (openGql.grammar.SimplifiedContents value) {
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
    public int compareTo(SimplifiedPathPatternExpression other) {
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

  public static final class AnyDirection extends openGql.grammar.SimplifiedPathPatternExpression implements Serializable {
    public final openGql.grammar.SimplifiedContents value;

    public AnyDirection (openGql.grammar.SimplifiedContents value) {
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
    public int compareTo(SimplifiedPathPatternExpression other) {
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
