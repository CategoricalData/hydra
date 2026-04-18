// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A JavaScript expression
 */
public abstract class Expression implements Serializable, Comparable<Expression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.Expression");

  public static final hydra.core.Name IDENTIFIER = new hydra.core.Name("identifier");

  public static final hydra.core.Name LITERAL = new hydra.core.Name("literal");

  public static final hydra.core.Name ARRAY = new hydra.core.Name("array");

  public static final hydra.core.Name OBJECT = new hydra.core.Name("object");

  public static final hydra.core.Name FUNCTION = new hydra.core.Name("function");

  public static final hydra.core.Name ARROW = new hydra.core.Name("arrow");

  public static final hydra.core.Name CALL = new hydra.core.Name("call");

  public static final hydra.core.Name MEMBER = new hydra.core.Name("member");

  public static final hydra.core.Name CONDITIONAL = new hydra.core.Name("conditional");

  public static final hydra.core.Name BINARY = new hydra.core.Name("binary");

  public static final hydra.core.Name UNARY = new hydra.core.Name("unary");

  public static final hydra.core.Name ASSIGNMENT = new hydra.core.Name("assignment");

  public static final hydra.core.Name SEQUENCE = new hydra.core.Name("sequence");

  public static final hydra.core.Name THIS = new hydra.core.Name("this");

  public static final hydra.core.Name NEW = new hydra.core.Name("new");

  public static final hydra.core.Name YIELD = new hydra.core.Name("yield");

  public static final hydra.core.Name AWAIT = new hydra.core.Name("await");

  public static final hydra.core.Name SPREAD = new hydra.core.Name("spread");

  public static final hydra.core.Name PARENTHESIZED = new hydra.core.Name("parenthesized");

  private Expression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Identifier instance) ;

    R visit(Literal instance) ;

    R visit(Array instance) ;

    R visit(Object_ instance) ;

    R visit(Function instance) ;

    R visit(Arrow instance) ;

    R visit(Call instance) ;

    R visit(Member instance) ;

    R visit(Conditional instance) ;

    R visit(Binary instance) ;

    R visit(Unary instance) ;

    R visit(Assignment instance) ;

    R visit(Sequence instance) ;

    R visit(This instance) ;

    R visit(New instance) ;

    R visit(Yield instance) ;

    R visit(Await instance) ;

    R visit(Spread instance) ;

    R visit(Parenthesized instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Expression instance) {
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

    default R visit(Object_ instance) {
      return otherwise(instance);
    }

    default R visit(Function instance) {
      return otherwise(instance);
    }

    default R visit(Arrow instance) {
      return otherwise(instance);
    }

    default R visit(Call instance) {
      return otherwise(instance);
    }

    default R visit(Member instance) {
      return otherwise(instance);
    }

    default R visit(Conditional instance) {
      return otherwise(instance);
    }

    default R visit(Binary instance) {
      return otherwise(instance);
    }

    default R visit(Unary instance) {
      return otherwise(instance);
    }

    default R visit(Assignment instance) {
      return otherwise(instance);
    }

    default R visit(Sequence instance) {
      return otherwise(instance);
    }

    default R visit(This instance) {
      return otherwise(instance);
    }

    default R visit(New instance) {
      return otherwise(instance);
    }

    default R visit(Yield instance) {
      return otherwise(instance);
    }

    default R visit(Await instance) {
      return otherwise(instance);
    }

    default R visit(Spread instance) {
      return otherwise(instance);
    }

    default R visit(Parenthesized instance) {
      return otherwise(instance);
    }
  }

  /**
   * A simple identifier
   */
  public static final class Identifier extends hydra.javaScript.syntax.Expression implements Serializable {
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
    public int compareTo(Expression other) {
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
   * A literal value
   */
  public static final class Literal extends hydra.javaScript.syntax.Expression implements Serializable {
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
    public int compareTo(Expression other) {
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
   * An array expression [a, b, c]
   */
  public static final class Array extends hydra.javaScript.syntax.Expression implements Serializable {
    public final java.util.List<hydra.javaScript.syntax.ArrayElement> value;

    public Array (java.util.List<hydra.javaScript.syntax.ArrayElement> value) {
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
    public int compareTo(Expression other) {
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
   * An object expression {a: 1, b: 2}
   */
  public static final class Object_ extends hydra.javaScript.syntax.Expression implements Serializable {
    public final java.util.List<hydra.javaScript.syntax.Property> value;

    public Object_ (java.util.List<hydra.javaScript.syntax.Property> value) {
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
    public int compareTo(Expression other) {
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
   * A function expression
   */
  public static final class Function extends hydra.javaScript.syntax.Expression implements Serializable {
    public final hydra.javaScript.syntax.FunctionExpression value;

    public Function (hydra.javaScript.syntax.FunctionExpression value) {
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
    public int compareTo(Expression other) {
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
   * An arrow function expression
   */
  public static final class Arrow extends hydra.javaScript.syntax.Expression implements Serializable {
    public final hydra.javaScript.syntax.ArrowFunctionExpression value;

    public Arrow (hydra.javaScript.syntax.ArrowFunctionExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Arrow)) {
        return false;
      }
      Arrow o = (Arrow) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Arrow o = (Arrow) other;
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
   * A function call expression
   */
  public static final class Call extends hydra.javaScript.syntax.Expression implements Serializable {
    public final hydra.javaScript.syntax.CallExpression value;

    public Call (hydra.javaScript.syntax.CallExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Call)) {
        return false;
      }
      Call o = (Call) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Call o = (Call) other;
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
   * A member access expression (obj.prop or obj[prop])
   */
  public static final class Member extends hydra.javaScript.syntax.Expression implements Serializable {
    public final hydra.javaScript.syntax.MemberExpression value;

    public Member (hydra.javaScript.syntax.MemberExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Member)) {
        return false;
      }
      Member o = (Member) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Member o = (Member) other;
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
   * A conditional (ternary) expression
   */
  public static final class Conditional extends hydra.javaScript.syntax.Expression implements Serializable {
    public final hydra.javaScript.syntax.ConditionalExpression value;

    public Conditional (hydra.javaScript.syntax.ConditionalExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Conditional)) {
        return false;
      }
      Conditional o = (Conditional) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Conditional o = (Conditional) other;
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
   * A binary operation expression
   */
  public static final class Binary extends hydra.javaScript.syntax.Expression implements Serializable {
    public final hydra.javaScript.syntax.BinaryExpression value;

    public Binary (hydra.javaScript.syntax.BinaryExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Binary)) {
        return false;
      }
      Binary o = (Binary) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Binary o = (Binary) other;
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
   * A unary operation expression
   */
  public static final class Unary extends hydra.javaScript.syntax.Expression implements Serializable {
    public final hydra.javaScript.syntax.UnaryExpression value;

    public Unary (hydra.javaScript.syntax.UnaryExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unary)) {
        return false;
      }
      Unary o = (Unary) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Unary o = (Unary) other;
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
   * An assignment expression
   */
  public static final class Assignment extends hydra.javaScript.syntax.Expression implements Serializable {
    public final hydra.javaScript.syntax.AssignmentExpression value;

    public Assignment (hydra.javaScript.syntax.AssignmentExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Assignment)) {
        return false;
      }
      Assignment o = (Assignment) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Assignment o = (Assignment) other;
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
   * A sequence expression (a, b, c)
   */
  public static final class Sequence extends hydra.javaScript.syntax.Expression implements Serializable {
    public final java.util.List<hydra.javaScript.syntax.Expression> value;

    public Sequence (java.util.List<hydra.javaScript.syntax.Expression> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence)) {
        return false;
      }
      Sequence o = (Sequence) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sequence o = (Sequence) other;
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
   * The 'this' keyword
   */
  public static final class This extends hydra.javaScript.syntax.Expression implements Serializable {
    public This () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof This)) {
        return false;
      }
      This o = (This) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Expression other) {
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
   * A 'new' expression
   */
  public static final class New extends hydra.javaScript.syntax.Expression implements Serializable {
    public final hydra.javaScript.syntax.CallExpression value;

    public New (hydra.javaScript.syntax.CallExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof New)) {
        return false;
      }
      New o = (New) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      New o = (New) other;
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
   * A yield expression
   */
  public static final class Yield extends hydra.javaScript.syntax.Expression implements Serializable {
    public final hydra.util.Maybe<hydra.javaScript.syntax.Expression> value;

    public Yield (hydra.util.Maybe<hydra.javaScript.syntax.Expression> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Yield)) {
        return false;
      }
      Yield o = (Yield) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Yield o = (Yield) other;
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
   * An await expression
   */
  public static final class Await extends hydra.javaScript.syntax.Expression implements Serializable {
    public final hydra.javaScript.syntax.Expression value;

    public Await (hydra.javaScript.syntax.Expression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Await)) {
        return false;
      }
      Await o = (Await) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Await o = (Await) other;
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
   * A spread expression (...x)
   */
  public static final class Spread extends hydra.javaScript.syntax.Expression implements Serializable {
    public final hydra.javaScript.syntax.SpreadElement value;

    public Spread (hydra.javaScript.syntax.SpreadElement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Spread)) {
        return false;
      }
      Spread o = (Spread) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Spread o = (Spread) other;
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
   * A parenthesized expression
   */
  public static final class Parenthesized extends hydra.javaScript.syntax.Expression implements Serializable {
    public final hydra.javaScript.syntax.Expression value;

    public Parenthesized (hydra.javaScript.syntax.Expression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parenthesized)) {
        return false;
      }
      Parenthesized o = (Parenthesized) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Parenthesized o = (Parenthesized) other;
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
