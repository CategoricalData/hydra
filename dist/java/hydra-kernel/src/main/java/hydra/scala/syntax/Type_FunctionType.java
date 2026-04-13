// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public abstract class Type_FunctionType implements Serializable, Comparable<Type_FunctionType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Type_FunctionType");

  public static final hydra.core.Name FUNCTION = new hydra.core.Name("function");

  public static final hydra.core.Name CONTEXT_FUNCTION = new hydra.core.Name("contextFunction");

  private Type_FunctionType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Function instance) ;

    R visit(ContextFunction instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Type_FunctionType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Function instance) {
      return otherwise(instance);
    }

    default R visit(ContextFunction instance) {
      return otherwise(instance);
    }
  }

  public static final class Function extends hydra.scala.syntax.Type_FunctionType implements Serializable {
    public final hydra.scala.syntax.Type_Function value;

    public Function (hydra.scala.syntax.Type_Function value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Function)) {
        return false;
      }
      Function o = (Function) other;
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
    public int compareTo(Type_FunctionType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Function o = (Function) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ContextFunction extends hydra.scala.syntax.Type_FunctionType implements Serializable {
    public final hydra.scala.syntax.Type_ContextFunction value;

    public ContextFunction (hydra.scala.syntax.Type_ContextFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ContextFunction)) {
        return false;
      }
      ContextFunction o = (ContextFunction) other;
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
    public int compareTo(Type_FunctionType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ContextFunction o = (ContextFunction) other;
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
