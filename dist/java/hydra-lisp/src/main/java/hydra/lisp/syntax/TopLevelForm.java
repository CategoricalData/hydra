// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * A top-level form in a Lisp program
 */
public abstract class TopLevelForm implements Serializable, Comparable<TopLevelForm> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.TopLevelForm");

  public static final hydra.core.Name FUNCTION = new hydra.core.Name("function");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public static final hydra.core.Name CONSTANT = new hydra.core.Name("constant");

  public static final hydra.core.Name RECORD_TYPE = new hydra.core.Name("recordType");

  public static final hydra.core.Name MACRO = new hydra.core.Name("macro");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  private TopLevelForm () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Function instance) ;

    R visit(Variable instance) ;

    R visit(Constant instance) ;

    R visit(RecordType instance) ;

    R visit(Macro instance) ;

    R visit(Expression instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TopLevelForm instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Function instance) {
      return otherwise(instance);
    }

    default R visit(Variable instance) {
      return otherwise(instance);
    }

    default R visit(Constant instance) {
      return otherwise(instance);
    }

    default R visit(RecordType instance) {
      return otherwise(instance);
    }

    default R visit(Macro instance) {
      return otherwise(instance);
    }

    default R visit(Expression instance) {
      return otherwise(instance);
    }
  }

  /**
   * A named function definition
   */
  public static final class Function extends hydra.lisp.syntax.TopLevelForm implements Serializable {
    public final hydra.lisp.syntax.FunctionDefinition value;

    public Function (hydra.lisp.syntax.FunctionDefinition value) {
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
    public int compareTo(TopLevelForm other) {
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

  /**
   * A global variable definition
   */
  public static final class Variable extends hydra.lisp.syntax.TopLevelForm implements Serializable {
    public final hydra.lisp.syntax.VariableDefinition value;

    public Variable (hydra.lisp.syntax.VariableDefinition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) other;
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
    public int compareTo(TopLevelForm other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Variable o = (Variable) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A constant definition
   */
  public static final class Constant extends hydra.lisp.syntax.TopLevelForm implements Serializable {
    public final hydra.lisp.syntax.ConstantDefinition value;

    public Constant (hydra.lisp.syntax.ConstantDefinition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Constant)) {
        return false;
      }
      Constant o = (Constant) other;
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
    public int compareTo(TopLevelForm other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Constant o = (Constant) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A record/struct type definition
   */
  public static final class RecordType extends hydra.lisp.syntax.TopLevelForm implements Serializable {
    public final hydra.lisp.syntax.RecordTypeDefinition value;

    public RecordType (hydra.lisp.syntax.RecordTypeDefinition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RecordType)) {
        return false;
      }
      RecordType o = (RecordType) other;
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
    public int compareTo(TopLevelForm other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      RecordType o = (RecordType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A macro definition
   */
  public static final class Macro extends hydra.lisp.syntax.TopLevelForm implements Serializable {
    public final hydra.lisp.syntax.MacroDefinition value;

    public Macro (hydra.lisp.syntax.MacroDefinition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Macro)) {
        return false;
      }
      Macro o = (Macro) other;
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
    public int compareTo(TopLevelForm other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Macro o = (Macro) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A bare expression at the top level
   */
  public static final class Expression extends hydra.lisp.syntax.TopLevelForm implements Serializable {
    public final hydra.lisp.syntax.Expression value;

    public Expression (hydra.lisp.syntax.Expression value) {
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
    public int compareTo(TopLevelForm other) {
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
}
