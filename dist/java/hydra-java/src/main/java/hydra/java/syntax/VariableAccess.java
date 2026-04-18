// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public abstract class VariableAccess implements Serializable, Comparable<VariableAccess> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.VariableAccess");

  public static final hydra.core.Name EXPRESSION_NAME = new hydra.core.Name("expressionName");

  public static final hydra.core.Name FIELD_ACCESS = new hydra.core.Name("fieldAccess");

  private VariableAccess () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ExpressionName instance) ;

    R visit(FieldAccess instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(VariableAccess instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ExpressionName instance) {
      return otherwise(instance);
    }

    default R visit(FieldAccess instance) {
      return otherwise(instance);
    }
  }

  public static final class ExpressionName extends hydra.java.syntax.VariableAccess implements Serializable {
    public final hydra.java.syntax.ExpressionName value;

    public ExpressionName (hydra.java.syntax.ExpressionName value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ExpressionName)) {
        return false;
      }
      ExpressionName o = (ExpressionName) other;
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
    public int compareTo(VariableAccess other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ExpressionName o = (ExpressionName) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class FieldAccess extends hydra.java.syntax.VariableAccess implements Serializable {
    public final hydra.java.syntax.FieldAccess value;

    public FieldAccess (hydra.java.syntax.FieldAccess value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FieldAccess)) {
        return false;
      }
      FieldAccess o = (FieldAccess) other;
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
    public int compareTo(VariableAccess other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      FieldAccess o = (FieldAccess) other;
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
