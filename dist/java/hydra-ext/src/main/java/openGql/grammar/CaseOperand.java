// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class CaseOperand implements Serializable, Comparable<CaseOperand> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CaseOperand");

  public static final hydra.core.Name VALUE_EXPRESSION = new hydra.core.Name("valueExpression");

  public static final hydra.core.Name ELEMENT_REFERENCE = new hydra.core.Name("elementReference");

  private CaseOperand () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ValueExpression instance) ;

    R visit(ElementReference instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CaseOperand instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ValueExpression instance) {
      return otherwise(instance);
    }

    default R visit(ElementReference instance) {
      return otherwise(instance);
    }
  }

  public static final class ValueExpression extends openGql.grammar.CaseOperand implements Serializable {
    public final openGql.grammar.NonParenthesizedPrimaryValueExpression value;

    public ValueExpression (openGql.grammar.NonParenthesizedPrimaryValueExpression value) {
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
    public int compareTo(CaseOperand other) {
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

  public static final class ElementReference extends openGql.grammar.CaseOperand implements Serializable {
    public final String value;

    public ElementReference (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ElementReference)) {
        return false;
      }
      ElementReference o = (ElementReference) other;
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
    public int compareTo(CaseOperand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ElementReference o = (ElementReference) other;
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
