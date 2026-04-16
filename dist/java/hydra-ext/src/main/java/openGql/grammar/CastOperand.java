// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class CastOperand implements Serializable, Comparable<CastOperand> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CastOperand");

  public static final hydra.core.Name VALUE_EXPRESSION = new hydra.core.Name("valueExpression");

  public static final hydra.core.Name NULL_LITERAL = new hydra.core.Name("nullLiteral");

  private CastOperand () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ValueExpression instance) ;

    R visit(NullLiteral instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CastOperand instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ValueExpression instance) {
      return otherwise(instance);
    }

    default R visit(NullLiteral instance) {
      return otherwise(instance);
    }
  }

  public static final class ValueExpression extends openGql.grammar.CastOperand implements Serializable {
    public final openGql.grammar.ValueExpression value;

    public ValueExpression (openGql.grammar.ValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ValueExpression)) {
        return false;
      }
      ValueExpression o = (ValueExpression) other;
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
    public int compareTo(CastOperand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ValueExpression o = (ValueExpression) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NullLiteral extends openGql.grammar.CastOperand implements Serializable {
    public NullLiteral () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NullLiteral)) {
        return false;
      }
      NullLiteral o = (NullLiteral) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CastOperand other) {
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
