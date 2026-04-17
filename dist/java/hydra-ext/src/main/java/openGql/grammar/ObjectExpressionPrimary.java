// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ObjectExpressionPrimary implements Serializable, Comparable<ObjectExpressionPrimary> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ObjectExpressionPrimary");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public static final hydra.core.Name PARENTHESIZED = new hydra.core.Name("parenthesized");

  public static final hydra.core.Name NON_PARENTHESIZED = new hydra.core.Name("nonParenthesized");

  private ObjectExpressionPrimary () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Variable instance) ;

    R visit(Parenthesized instance) ;

    R visit(NonParenthesized instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ObjectExpressionPrimary instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Variable instance) {
      return otherwise(instance);
    }

    default R visit(Parenthesized instance) {
      return otherwise(instance);
    }

    default R visit(NonParenthesized instance) {
      return otherwise(instance);
    }
  }

  public static final class Variable extends openGql.grammar.ObjectExpressionPrimary implements Serializable {
    public final openGql.grammar.PrimaryValueExpression value;

    public Variable (openGql.grammar.PrimaryValueExpression value) {
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
    public int compareTo(ObjectExpressionPrimary other) {
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

  public static final class Parenthesized extends openGql.grammar.ObjectExpressionPrimary implements Serializable {
    public final openGql.grammar.ValueExpression value;

    public Parenthesized (openGql.grammar.ValueExpression value) {
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
    public int compareTo(ObjectExpressionPrimary other) {
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

  public static final class NonParenthesized extends openGql.grammar.ObjectExpressionPrimary implements Serializable {
    public final openGql.grammar.NonParenthesizedPrimaryValueExpressionSpecialCase value;

    public NonParenthesized (openGql.grammar.NonParenthesizedPrimaryValueExpressionSpecialCase value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NonParenthesized)) {
        return false;
      }
      NonParenthesized o = (NonParenthesized) other;
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
    public int compareTo(ObjectExpressionPrimary other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NonParenthesized o = (NonParenthesized) other;
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
