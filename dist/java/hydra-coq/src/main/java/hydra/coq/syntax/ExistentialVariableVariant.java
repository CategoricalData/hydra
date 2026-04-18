// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class ExistentialVariableVariant implements Serializable, Comparable<ExistentialVariableVariant> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.ExistentialVariableVariant");

  public static final hydra.core.Name PLACEHOLDER = new hydra.core.Name("placeholder");

  public static final hydra.core.Name INSIDE1 = new hydra.core.Name("inside1");

  public static final hydra.core.Name INSIDE2 = new hydra.core.Name("inside2");

  public static final hydra.core.Name OUTSIDE = new hydra.core.Name("outside");

  private ExistentialVariableVariant () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Placeholder instance) ;

    R visit(Inside1 instance) ;

    R visit(Inside2 instance) ;

    R visit(Outside instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ExistentialVariableVariant instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Placeholder instance) {
      return otherwise(instance);
    }

    default R visit(Inside1 instance) {
      return otherwise(instance);
    }

    default R visit(Inside2 instance) {
      return otherwise(instance);
    }

    default R visit(Outside instance) {
      return otherwise(instance);
    }
  }

  public static final class Placeholder extends hydra.coq.syntax.ExistentialVariableVariant implements Serializable {
    public Placeholder () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Placeholder)) {
        return false;
      }
      Placeholder o = (Placeholder) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ExistentialVariableVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Inside1 extends hydra.coq.syntax.ExistentialVariableVariant implements Serializable {
    public Inside1 () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Inside1)) {
        return false;
      }
      Inside1 o = (Inside1) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ExistentialVariableVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Inside2 extends hydra.coq.syntax.ExistentialVariableVariant implements Serializable {
    public Inside2 () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Inside2)) {
        return false;
      }
      Inside2 o = (Inside2) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ExistentialVariableVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Outside extends hydra.coq.syntax.ExistentialVariableVariant implements Serializable {
    public final hydra.util.Maybe<hydra.coq.syntax.IdentArg> value;

    public Outside (hydra.util.Maybe<hydra.coq.syntax.IdentArg> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Outside)) {
        return false;
      }
      Outside o = (Outside) other;
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
    public int compareTo(ExistentialVariableVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Outside o = (Outside) other;
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
