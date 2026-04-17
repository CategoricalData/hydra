// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class NonParenthesizedPrimaryValueExpression implements Serializable, Comparable<NonParenthesizedPrimaryValueExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NonParenthesizedPrimaryValueExpression");

  public static final hydra.core.Name SPECIAL = new hydra.core.Name("special");

  public static final hydra.core.Name BINDING_VARIABLE = new hydra.core.Name("bindingVariable");

  private NonParenthesizedPrimaryValueExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Special instance) ;

    R visit(BindingVariable instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NonParenthesizedPrimaryValueExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Special instance) {
      return otherwise(instance);
    }

    default R visit(BindingVariable instance) {
      return otherwise(instance);
    }
  }

  public static final class Special extends openGql.grammar.NonParenthesizedPrimaryValueExpression implements Serializable {
    public final openGql.grammar.NonParenthesizedPrimaryValueExpressionSpecialCase value;

    public Special (openGql.grammar.NonParenthesizedPrimaryValueExpressionSpecialCase value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Special)) {
        return false;
      }
      Special o = (Special) other;
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
    public int compareTo(NonParenthesizedPrimaryValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Special o = (Special) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class BindingVariable extends openGql.grammar.NonParenthesizedPrimaryValueExpression implements Serializable {
    public final String value;

    public BindingVariable (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BindingVariable)) {
        return false;
      }
      BindingVariable o = (BindingVariable) other;
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
    public int compareTo(NonParenthesizedPrimaryValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BindingVariable o = (BindingVariable) other;
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
