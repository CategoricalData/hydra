// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * An element in an array expression
 */
public abstract class ArrayElement implements Serializable, Comparable<ArrayElement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ArrayElement");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public static final hydra.core.Name SPREAD = new hydra.core.Name("spread");

  public static final hydra.core.Name HOLE = new hydra.core.Name("hole");

  private ArrayElement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Expression instance) ;

    R visit(Spread instance) ;

    R visit(Hole instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ArrayElement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Expression instance) {
      return otherwise(instance);
    }

    default R visit(Spread instance) {
      return otherwise(instance);
    }

    default R visit(Hole instance) {
      return otherwise(instance);
    }
  }

  /**
   * A regular expression element
   */
  public static final class Expression extends hydra.javaScript.syntax.ArrayElement implements Serializable {
    public final hydra.javaScript.syntax.Expression value;

    public Expression (hydra.javaScript.syntax.Expression value) {
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
    public int compareTo(ArrayElement other) {
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

  /**
   * A spread element ...x
   */
  public static final class Spread extends hydra.javaScript.syntax.ArrayElement implements Serializable {
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
    public int compareTo(ArrayElement other) {
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
   * An empty slot (elision)
   */
  public static final class Hole extends hydra.javaScript.syntax.ArrayElement implements Serializable {
    public Hole () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Hole)) {
        return false;
      }
      Hole o = (Hole) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ArrayElement other) {
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
