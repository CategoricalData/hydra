// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public abstract class StatementExpression implements Serializable, Comparable<StatementExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.StatementExpression");

  public static final hydra.core.Name ASSIGNMENT = new hydra.core.Name("assignment");

  public static final hydra.core.Name PRE_INCREMENT = new hydra.core.Name("preIncrement");

  public static final hydra.core.Name PRE_DECREMENT = new hydra.core.Name("preDecrement");

  public static final hydra.core.Name POST_INCREMENT = new hydra.core.Name("postIncrement");

  public static final hydra.core.Name POST_DECREMENT = new hydra.core.Name("postDecrement");

  public static final hydra.core.Name METHOD_INVOCATION = new hydra.core.Name("methodInvocation");

  public static final hydra.core.Name CLASS_INSTANCE_CREATION = new hydra.core.Name("classInstanceCreation");

  private StatementExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Assignment instance) ;

    R visit(PreIncrement instance) ;

    R visit(PreDecrement instance) ;

    R visit(PostIncrement instance) ;

    R visit(PostDecrement instance) ;

    R visit(MethodInvocation instance) ;

    R visit(ClassInstanceCreation instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StatementExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Assignment instance) {
      return otherwise(instance);
    }

    default R visit(PreIncrement instance) {
      return otherwise(instance);
    }

    default R visit(PreDecrement instance) {
      return otherwise(instance);
    }

    default R visit(PostIncrement instance) {
      return otherwise(instance);
    }

    default R visit(PostDecrement instance) {
      return otherwise(instance);
    }

    default R visit(MethodInvocation instance) {
      return otherwise(instance);
    }

    default R visit(ClassInstanceCreation instance) {
      return otherwise(instance);
    }
  }

  public static final class Assignment extends hydra.java.syntax.StatementExpression implements Serializable {
    public final hydra.java.syntax.Assignment value;

    public Assignment (hydra.java.syntax.Assignment value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Assignment)) {
        return false;
      }
      Assignment o = (Assignment) other;
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
    public int compareTo(StatementExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Assignment o = (Assignment) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PreIncrement extends hydra.java.syntax.StatementExpression implements Serializable {
    public final hydra.java.syntax.PreIncrementExpression value;

    public PreIncrement (hydra.java.syntax.PreIncrementExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PreIncrement)) {
        return false;
      }
      PreIncrement o = (PreIncrement) other;
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
    public int compareTo(StatementExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PreIncrement o = (PreIncrement) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PreDecrement extends hydra.java.syntax.StatementExpression implements Serializable {
    public final hydra.java.syntax.PreDecrementExpression value;

    public PreDecrement (hydra.java.syntax.PreDecrementExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PreDecrement)) {
        return false;
      }
      PreDecrement o = (PreDecrement) other;
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
    public int compareTo(StatementExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PreDecrement o = (PreDecrement) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PostIncrement extends hydra.java.syntax.StatementExpression implements Serializable {
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
    public int compareTo(StatementExpression other) {
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

  public static final class PostDecrement extends hydra.java.syntax.StatementExpression implements Serializable {
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
    public int compareTo(StatementExpression other) {
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

  public static final class MethodInvocation extends hydra.java.syntax.StatementExpression implements Serializable {
    public final hydra.java.syntax.MethodInvocation value;

    public MethodInvocation (hydra.java.syntax.MethodInvocation value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MethodInvocation)) {
        return false;
      }
      MethodInvocation o = (MethodInvocation) other;
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
    public int compareTo(StatementExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MethodInvocation o = (MethodInvocation) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ClassInstanceCreation extends hydra.java.syntax.StatementExpression implements Serializable {
    public final hydra.java.syntax.ClassInstanceCreationExpression value;

    public ClassInstanceCreation (hydra.java.syntax.ClassInstanceCreationExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassInstanceCreation)) {
        return false;
      }
      ClassInstanceCreation o = (ClassInstanceCreation) other;
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
    public int compareTo(StatementExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ClassInstanceCreation o = (ClassInstanceCreation) other;
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
