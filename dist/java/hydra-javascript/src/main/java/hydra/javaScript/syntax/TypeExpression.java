// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A type expression
 */
public abstract class TypeExpression implements Serializable, Comparable<TypeExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.TypeExpression");

  public static final hydra.core.Name IDENTIFIER = new hydra.core.Name("identifier");

  public static final hydra.core.Name LITERAL = new hydra.core.Name("literal");

  public static final hydra.core.Name ARRAY = new hydra.core.Name("array");

  public static final hydra.core.Name FUNCTION = new hydra.core.Name("function");

  public static final hydra.core.Name OBJECT = new hydra.core.Name("object");

  public static final hydra.core.Name UNION = new hydra.core.Name("union");

  public static final hydra.core.Name PARAMETERIZED = new hydra.core.Name("parameterized");

  public static final hydra.core.Name OPTIONAL = new hydra.core.Name("optional");

  public static final hydra.core.Name ANY = new hydra.core.Name("any");

  public static final hydra.core.Name VOID = new hydra.core.Name("void");

  public static final hydra.core.Name NEVER = new hydra.core.Name("never");

  private TypeExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Identifier instance) ;

    R visit(Literal instance) ;

    R visit(Array instance) ;

    R visit(Function instance) ;

    R visit(Object_ instance) ;

    R visit(Union instance) ;

    R visit(Parameterized instance) ;

    R visit(Optional instance) ;

    R visit(Any instance) ;

    R visit(Void_ instance) ;

    R visit(Never instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Identifier instance) {
      return otherwise(instance);
    }

    default R visit(Literal instance) {
      return otherwise(instance);
    }

    default R visit(Array instance) {
      return otherwise(instance);
    }

    default R visit(Function instance) {
      return otherwise(instance);
    }

    default R visit(Object_ instance) {
      return otherwise(instance);
    }

    default R visit(Union instance) {
      return otherwise(instance);
    }

    default R visit(Parameterized instance) {
      return otherwise(instance);
    }

    default R visit(Optional instance) {
      return otherwise(instance);
    }

    default R visit(Any instance) {
      return otherwise(instance);
    }

    default R visit(Void_ instance) {
      return otherwise(instance);
    }

    default R visit(Never instance) {
      return otherwise(instance);
    }
  }

  /**
   * A named type (e.g., 'string', 'number', 'MyClass')
   */
  public static final class Identifier extends hydra.javaScript.syntax.TypeExpression implements Serializable {
    public final hydra.javaScript.syntax.Identifier value;

    public Identifier (hydra.javaScript.syntax.Identifier value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Identifier)) {
        return false;
      }
      Identifier o = (Identifier) other;
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
    public int compareTo(TypeExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Identifier o = (Identifier) other;
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
   * A literal type (e.g., 'hello', 42)
   */
  public static final class Literal extends hydra.javaScript.syntax.TypeExpression implements Serializable {
    public final hydra.javaScript.syntax.Literal value;

    public Literal (hydra.javaScript.syntax.Literal value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) other;
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
    public int compareTo(TypeExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Literal o = (Literal) other;
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
   * An array type
   */
  public static final class Array extends hydra.javaScript.syntax.TypeExpression implements Serializable {
    public final hydra.javaScript.syntax.ArrayTypeExpression value;

    public Array (hydra.javaScript.syntax.ArrayTypeExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Array)) {
        return false;
      }
      Array o = (Array) other;
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
    public int compareTo(TypeExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Array o = (Array) other;
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
   * A function type
   */
  public static final class Function extends hydra.javaScript.syntax.TypeExpression implements Serializable {
    public final hydra.javaScript.syntax.FunctionTypeExpression value;

    public Function (hydra.javaScript.syntax.FunctionTypeExpression value) {
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
    public int compareTo(TypeExpression other) {
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
   * An object type
   */
  public static final class Object_ extends hydra.javaScript.syntax.TypeExpression implements Serializable {
    public final java.util.List<hydra.javaScript.syntax.PropertySignature> value;

    public Object_ (java.util.List<hydra.javaScript.syntax.PropertySignature> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Object_)) {
        return false;
      }
      Object_ o = (Object_) other;
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
    public int compareTo(TypeExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Object_ o = (Object_) other;
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
   * A union type (A | B)
   */
  public static final class Union extends hydra.javaScript.syntax.TypeExpression implements Serializable {
    public final java.util.List<hydra.javaScript.syntax.TypeExpression> value;

    public Union (java.util.List<hydra.javaScript.syntax.TypeExpression> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) other;
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
    public int compareTo(TypeExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Union o = (Union) other;
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
   * A parameterized type (e.g., Array&lt;T&gt;, Map&lt;K, V&gt;)
   */
  public static final class Parameterized extends hydra.javaScript.syntax.TypeExpression implements Serializable {
    public final hydra.javaScript.syntax.ParameterizedTypeExpression value;

    public Parameterized (hydra.javaScript.syntax.ParameterizedTypeExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parameterized)) {
        return false;
      }
      Parameterized o = (Parameterized) other;
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
    public int compareTo(TypeExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Parameterized o = (Parameterized) other;
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
   * An optional type (?T)
   */
  public static final class Optional extends hydra.javaScript.syntax.TypeExpression implements Serializable {
    public final hydra.javaScript.syntax.TypeExpression value;

    public Optional (hydra.javaScript.syntax.TypeExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Optional)) {
        return false;
      }
      Optional o = (Optional) other;
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
    public int compareTo(TypeExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Optional o = (Optional) other;
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
   * The 'any' type
   */
  public static final class Any extends hydra.javaScript.syntax.TypeExpression implements Serializable {
    public Any () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Any)) {
        return false;
      }
      Any o = (Any) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeExpression other) {
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

  /**
   * The 'void' type
   */
  public static final class Void_ extends hydra.javaScript.syntax.TypeExpression implements Serializable {
    public Void_ () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Void_)) {
        return false;
      }
      Void_ o = (Void_) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeExpression other) {
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

  /**
   * The 'never' type
   */
  public static final class Never extends hydra.javaScript.syntax.TypeExpression implements Serializable {
    public Never () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Never)) {
        return false;
      }
      Never o = (Never) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeExpression other) {
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
