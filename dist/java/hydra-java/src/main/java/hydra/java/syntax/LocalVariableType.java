// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public abstract class LocalVariableType implements Serializable, Comparable<LocalVariableType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.LocalVariableType");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name VAR = new hydra.core.Name("var");

  private LocalVariableType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Type instance) ;

    R visit(Var instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LocalVariableType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Type instance) {
      return otherwise(instance);
    }

    default R visit(Var instance) {
      return otherwise(instance);
    }
  }

  public static final class Type extends hydra.java.syntax.LocalVariableType implements Serializable {
    public final hydra.java.syntax.UnannType value;

    public Type (hydra.java.syntax.UnannType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) other;
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
    public int compareTo(LocalVariableType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Type o = (Type) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Var extends hydra.java.syntax.LocalVariableType implements Serializable {
    public Var () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Var)) {
        return false;
      }
      Var o = (Var) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(LocalVariableType other) {
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
}
