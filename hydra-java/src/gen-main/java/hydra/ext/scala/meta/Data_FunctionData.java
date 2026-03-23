// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public abstract class Data_FunctionData implements Serializable, Comparable<Data_FunctionData> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Data_FunctionData");

  public static final hydra.core.Name CONTEXT_FUNCTION = new hydra.core.Name("contextFunction");

  public static final hydra.core.Name FUNCTION = new hydra.core.Name("function");

  private Data_FunctionData () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ContextFunction instance) ;

    R visit(Function instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Data_FunctionData instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ContextFunction instance) {
      return otherwise(instance);
    }

    default R visit(Function instance) {
      return otherwise(instance);
    }
  }

  public static final class ContextFunction extends hydra.ext.scala.meta.Data_FunctionData implements Serializable {
    public final hydra.ext.scala.meta.Data_ContextFunction value;

    public ContextFunction (hydra.ext.scala.meta.Data_ContextFunction value) {
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
    public int compareTo(Data_FunctionData other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ContextFunction o = (ContextFunction) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Function extends hydra.ext.scala.meta.Data_FunctionData implements Serializable {
    public final hydra.ext.scala.meta.Data_Function value;

    public Function (hydra.ext.scala.meta.Data_Function value) {
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
    public int compareTo(Data_FunctionData other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Function o = (Function) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
