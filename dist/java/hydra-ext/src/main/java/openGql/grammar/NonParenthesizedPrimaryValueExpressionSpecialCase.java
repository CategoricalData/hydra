// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class NonParenthesizedPrimaryValueExpressionSpecialCase implements Serializable, Comparable<NonParenthesizedPrimaryValueExpressionSpecialCase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NonParenthesizedPrimaryValueExpressionSpecialCase");

  public static final hydra.core.Name AGGREGATE_FUNCTION = new hydra.core.Name("aggregateFunction");

  public static final hydra.core.Name UNSIGNED_VALUE_SPECIFICATION = new hydra.core.Name("unsignedValueSpecification");

  public static final hydra.core.Name PATH_VALUE_CONSTRUCTOR = new hydra.core.Name("pathValueConstructor");

  public static final hydra.core.Name PROPERTY_REFERENCE = new hydra.core.Name("propertyReference");

  public static final hydra.core.Name VALUE_QUERY_EXPRESSION = new hydra.core.Name("valueQueryExpression");

  public static final hydra.core.Name CASE_EXPRESSION = new hydra.core.Name("caseExpression");

  public static final hydra.core.Name CAST_SPECIFICATION = new hydra.core.Name("castSpecification");

  public static final hydra.core.Name ELEMENT_ID_FUNCTION = new hydra.core.Name("elementIdFunction");

  public static final hydra.core.Name LET_VALUE_EXPRESSION = new hydra.core.Name("letValueExpression");

  private NonParenthesizedPrimaryValueExpressionSpecialCase () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(AggregateFunction instance) ;

    R visit(UnsignedValueSpecification instance) ;

    R visit(PathValueConstructor instance) ;

    R visit(PropertyReference instance) ;

    R visit(ValueQueryExpression instance) ;

    R visit(CaseExpression instance) ;

    R visit(CastSpecification instance) ;

    R visit(ElementIdFunction instance) ;

    R visit(LetValueExpression instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NonParenthesizedPrimaryValueExpressionSpecialCase instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(AggregateFunction instance) {
      return otherwise(instance);
    }

    default R visit(UnsignedValueSpecification instance) {
      return otherwise(instance);
    }

    default R visit(PathValueConstructor instance) {
      return otherwise(instance);
    }

    default R visit(PropertyReference instance) {
      return otherwise(instance);
    }

    default R visit(ValueQueryExpression instance) {
      return otherwise(instance);
    }

    default R visit(CaseExpression instance) {
      return otherwise(instance);
    }

    default R visit(CastSpecification instance) {
      return otherwise(instance);
    }

    default R visit(ElementIdFunction instance) {
      return otherwise(instance);
    }

    default R visit(LetValueExpression instance) {
      return otherwise(instance);
    }
  }

  public static final class AggregateFunction extends openGql.grammar.NonParenthesizedPrimaryValueExpressionSpecialCase implements Serializable {
    public final openGql.grammar.AggregateFunction value;

    public AggregateFunction (openGql.grammar.AggregateFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AggregateFunction)) {
        return false;
      }
      AggregateFunction o = (AggregateFunction) other;
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
    public int compareTo(NonParenthesizedPrimaryValueExpressionSpecialCase other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AggregateFunction o = (AggregateFunction) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class UnsignedValueSpecification extends openGql.grammar.NonParenthesizedPrimaryValueExpressionSpecialCase implements Serializable {
    public final openGql.grammar.UnsignedValueSpecification value;

    public UnsignedValueSpecification (openGql.grammar.UnsignedValueSpecification value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnsignedValueSpecification)) {
        return false;
      }
      UnsignedValueSpecification o = (UnsignedValueSpecification) other;
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
    public int compareTo(NonParenthesizedPrimaryValueExpressionSpecialCase other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UnsignedValueSpecification o = (UnsignedValueSpecification) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PathValueConstructor extends openGql.grammar.NonParenthesizedPrimaryValueExpressionSpecialCase implements Serializable {
    public final openGql.grammar.PathElementList value;

    public PathValueConstructor (openGql.grammar.PathElementList value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PathValueConstructor)) {
        return false;
      }
      PathValueConstructor o = (PathValueConstructor) other;
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
    public int compareTo(NonParenthesizedPrimaryValueExpressionSpecialCase other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PathValueConstructor o = (PathValueConstructor) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PropertyReference extends openGql.grammar.NonParenthesizedPrimaryValueExpressionSpecialCase implements Serializable {
    public final openGql.grammar.PropertyReference value;

    public PropertyReference (openGql.grammar.PropertyReference value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PropertyReference)) {
        return false;
      }
      PropertyReference o = (PropertyReference) other;
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
    public int compareTo(NonParenthesizedPrimaryValueExpressionSpecialCase other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PropertyReference o = (PropertyReference) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ValueQueryExpression extends openGql.grammar.NonParenthesizedPrimaryValueExpressionSpecialCase implements Serializable {
    public final openGql.grammar.ProcedureBody value;

    public ValueQueryExpression (openGql.grammar.ProcedureBody value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ValueQueryExpression)) {
        return false;
      }
      ValueQueryExpression o = (ValueQueryExpression) other;
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
    public int compareTo(NonParenthesizedPrimaryValueExpressionSpecialCase other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ValueQueryExpression o = (ValueQueryExpression) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CaseExpression extends openGql.grammar.NonParenthesizedPrimaryValueExpressionSpecialCase implements Serializable {
    public final openGql.grammar.CaseExpression value;

    public CaseExpression (openGql.grammar.CaseExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CaseExpression)) {
        return false;
      }
      CaseExpression o = (CaseExpression) other;
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
    public int compareTo(NonParenthesizedPrimaryValueExpressionSpecialCase other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CaseExpression o = (CaseExpression) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CastSpecification extends openGql.grammar.NonParenthesizedPrimaryValueExpressionSpecialCase implements Serializable {
    public final openGql.grammar.CastSpecification value;

    public CastSpecification (openGql.grammar.CastSpecification value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CastSpecification)) {
        return false;
      }
      CastSpecification o = (CastSpecification) other;
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
    public int compareTo(NonParenthesizedPrimaryValueExpressionSpecialCase other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CastSpecification o = (CastSpecification) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ElementIdFunction extends openGql.grammar.NonParenthesizedPrimaryValueExpressionSpecialCase implements Serializable {
    public final String value;

    public ElementIdFunction (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ElementIdFunction)) {
        return false;
      }
      ElementIdFunction o = (ElementIdFunction) other;
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
    public int compareTo(NonParenthesizedPrimaryValueExpressionSpecialCase other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ElementIdFunction o = (ElementIdFunction) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class LetValueExpression extends openGql.grammar.NonParenthesizedPrimaryValueExpressionSpecialCase implements Serializable {
    public final openGql.grammar.LetValueExpression value;

    public LetValueExpression (openGql.grammar.LetValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LetValueExpression)) {
        return false;
      }
      LetValueExpression o = (LetValueExpression) other;
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
    public int compareTo(NonParenthesizedPrimaryValueExpressionSpecialCase other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LetValueExpression o = (LetValueExpression) other;
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
