// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * The kind of variable declaration
 */
public abstract class VariableKind implements Serializable, Comparable<VariableKind> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.VariableKind");

  public static final hydra.core.Name VAR = new hydra.core.Name("var");

  public static final hydra.core.Name LET = new hydra.core.Name("let");

  public static final hydra.core.Name CONST = new hydra.core.Name("const");

  private VariableKind () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Var instance) ;

    R visit(Let instance) ;

    R visit(Const instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(VariableKind instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Var instance) {
      return otherwise(instance);
    }

    default R visit(Let instance) {
      return otherwise(instance);
    }

    default R visit(Const instance) {
      return otherwise(instance);
    }
  }

  public static final class Var extends hydra.javaScript.syntax.VariableKind implements Serializable {
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
    public int compareTo(VariableKind other) {
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

  public static final class Let extends hydra.javaScript.syntax.VariableKind implements Serializable {
    public Let () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Let)) {
        return false;
      }
      Let o = (Let) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(VariableKind other) {
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

  public static final class Const extends hydra.javaScript.syntax.VariableKind implements Serializable {
    public Const () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Const)) {
        return false;
      }
      Const o = (Const) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(VariableKind other) {
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
