// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public abstract class Primary implements Serializable, Comparable<Primary> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.Primary");

  public static final hydra.core.Name NO_NEW_ARRAY = new hydra.core.Name("noNewArray");

  public static final hydra.core.Name ARRAY_CREATION = new hydra.core.Name("arrayCreation");

  private Primary () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(NoNewArray instance) ;

    R visit(ArrayCreation instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Primary instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(NoNewArray instance) {
      return otherwise(instance);
    }

    default R visit(ArrayCreation instance) {
      return otherwise(instance);
    }
  }

  public static final class NoNewArray extends hydra.java.syntax.Primary implements Serializable {
    public final hydra.java.syntax.PrimaryNoNewArrayExpression value;

    public NoNewArray (hydra.java.syntax.PrimaryNoNewArrayExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NoNewArray)) {
        return false;
      }
      NoNewArray o = (NoNewArray) other;
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
    public int compareTo(Primary other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NoNewArray o = (NoNewArray) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ArrayCreation extends hydra.java.syntax.Primary implements Serializable {
    public final hydra.java.syntax.ArrayCreationExpression value;

    public ArrayCreation (hydra.java.syntax.ArrayCreationExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ArrayCreation)) {
        return false;
      }
      ArrayCreation o = (ArrayCreation) other;
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
    public int compareTo(Primary other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ArrayCreation o = (ArrayCreation) other;
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
