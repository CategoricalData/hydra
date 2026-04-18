// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public abstract class LiteralExpressionOrAttribute implements Serializable, Comparable<LiteralExpressionOrAttribute> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.LiteralExpressionOrAttribute");

  public static final hydra.core.Name LITERAL = new hydra.core.Name("literal");

  public static final hydra.core.Name ATTRIBUTE = new hydra.core.Name("attribute");

  private LiteralExpressionOrAttribute () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Literal instance) ;

    R visit(Attribute instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LiteralExpressionOrAttribute instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Literal instance) {
      return otherwise(instance);
    }

    default R visit(Attribute instance) {
      return otherwise(instance);
    }
  }

  public static final class Literal extends hydra.python.syntax.LiteralExpressionOrAttribute implements Serializable {
    public final hydra.python.syntax.LiteralExpression value;

    public Literal (hydra.python.syntax.LiteralExpression value) {
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
    public int compareTo(LiteralExpressionOrAttribute other) {
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

  public static final class Attribute extends hydra.python.syntax.LiteralExpressionOrAttribute implements Serializable {
    public final hydra.python.syntax.Attribute value;

    public Attribute (hydra.python.syntax.Attribute value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Attribute)) {
        return false;
      }
      Attribute o = (Attribute) other;
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
    public int compareTo(LiteralExpressionOrAttribute other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Attribute o = (Attribute) other;
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
