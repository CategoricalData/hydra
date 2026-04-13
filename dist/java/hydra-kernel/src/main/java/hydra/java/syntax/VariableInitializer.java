// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public abstract class VariableInitializer implements Serializable, Comparable<VariableInitializer> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.VariableInitializer");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public static final hydra.core.Name ARRAY_INITIALIZER = new hydra.core.Name("arrayInitializer");

  private VariableInitializer () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Expression instance) ;

    R visit(ArrayInitializer instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(VariableInitializer instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Expression instance) {
      return otherwise(instance);
    }

    default R visit(ArrayInitializer instance) {
      return otherwise(instance);
    }
  }

  public static final class Expression extends hydra.java.syntax.VariableInitializer implements Serializable {
    public final hydra.java.syntax.Expression value;

    public Expression (hydra.java.syntax.Expression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Expression)) {
        return false;
      }
      Expression o = (Expression) other;
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
    public int compareTo(VariableInitializer other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Expression o = (Expression) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ArrayInitializer extends hydra.java.syntax.VariableInitializer implements Serializable {
    public final hydra.java.syntax.ArrayInitializer value;

    public ArrayInitializer (hydra.java.syntax.ArrayInitializer value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ArrayInitializer)) {
        return false;
      }
      ArrayInitializer o = (ArrayInitializer) other;
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
    public int compareTo(VariableInitializer other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ArrayInitializer o = (ArrayInitializer) other;
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
