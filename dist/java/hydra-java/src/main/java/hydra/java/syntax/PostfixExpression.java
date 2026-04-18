// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public abstract class PostfixExpression implements Serializable, Comparable<PostfixExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.PostfixExpression");

  public static final hydra.core.Name PRIMARY = new hydra.core.Name("primary");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name POST_INCREMENT = new hydra.core.Name("postIncrement");

  public static final hydra.core.Name POST_DECREMENT = new hydra.core.Name("postDecrement");

  private PostfixExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Primary instance) ;

    R visit(Name instance) ;

    R visit(PostIncrement instance) ;

    R visit(PostDecrement instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PostfixExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Primary instance) {
      return otherwise(instance);
    }

    default R visit(Name instance) {
      return otherwise(instance);
    }

    default R visit(PostIncrement instance) {
      return otherwise(instance);
    }

    default R visit(PostDecrement instance) {
      return otherwise(instance);
    }
  }

  public static final class Primary extends hydra.java.syntax.PostfixExpression implements Serializable {
    public final hydra.java.syntax.Primary value;

    public Primary (hydra.java.syntax.Primary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primary)) {
        return false;
      }
      Primary o = (Primary) other;
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
    public int compareTo(PostfixExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Primary o = (Primary) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Name extends hydra.java.syntax.PostfixExpression implements Serializable {
    public final hydra.java.syntax.ExpressionName value;

    public Name (hydra.java.syntax.ExpressionName value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Name)) {
        return false;
      }
      Name o = (Name) other;
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
    public int compareTo(PostfixExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Name o = (Name) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PostIncrement extends hydra.java.syntax.PostfixExpression implements Serializable {
    public final hydra.java.syntax.PostIncrementExpression value;

    public PostIncrement (hydra.java.syntax.PostIncrementExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PostIncrement)) {
        return false;
      }
      PostIncrement o = (PostIncrement) other;
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
    public int compareTo(PostfixExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PostIncrement o = (PostIncrement) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PostDecrement extends hydra.java.syntax.PostfixExpression implements Serializable {
    public final hydra.java.syntax.PostDecrementExpression value;

    public PostDecrement (hydra.java.syntax.PostDecrementExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PostDecrement)) {
        return false;
      }
      PostDecrement o = (PostDecrement) other;
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
    public int compareTo(PostfixExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PostDecrement o = (PostDecrement) other;
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
