// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class WhenOperand implements Serializable, Comparable<WhenOperand> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.WhenOperand");

  public static final hydra.core.Name VALUE_EXPRESSION = new hydra.core.Name("valueExpression");

  public static final hydra.core.Name COMPARISON = new hydra.core.Name("comparison");

  public static final hydra.core.Name NULL_PREDICATE = new hydra.core.Name("nullPredicate");

  public static final hydra.core.Name VALUE_TYPE_PREDICATE = new hydra.core.Name("valueTypePredicate");

  public static final hydra.core.Name NORMALIZED_PREDICATE = new hydra.core.Name("normalizedPredicate");

  public static final hydra.core.Name DIRECTED_PREDICATE = new hydra.core.Name("directedPredicate");

  public static final hydra.core.Name LABELED_PREDICATE = new hydra.core.Name("labeledPredicate");

  public static final hydra.core.Name SOURCE_PREDICATE = new hydra.core.Name("sourcePredicate");

  public static final hydra.core.Name DESTINATION_PREDICATE = new hydra.core.Name("destinationPredicate");

  private WhenOperand () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ValueExpression instance) ;

    R visit(Comparison instance) ;

    R visit(NullPredicate instance) ;

    R visit(ValueTypePredicate instance) ;

    R visit(NormalizedPredicate instance) ;

    R visit(DirectedPredicate instance) ;

    R visit(LabeledPredicate instance) ;

    R visit(SourcePredicate instance) ;

    R visit(DestinationPredicate instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(WhenOperand instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ValueExpression instance) {
      return otherwise(instance);
    }

    default R visit(Comparison instance) {
      return otherwise(instance);
    }

    default R visit(NullPredicate instance) {
      return otherwise(instance);
    }

    default R visit(ValueTypePredicate instance) {
      return otherwise(instance);
    }

    default R visit(NormalizedPredicate instance) {
      return otherwise(instance);
    }

    default R visit(DirectedPredicate instance) {
      return otherwise(instance);
    }

    default R visit(LabeledPredicate instance) {
      return otherwise(instance);
    }

    default R visit(SourcePredicate instance) {
      return otherwise(instance);
    }

    default R visit(DestinationPredicate instance) {
      return otherwise(instance);
    }
  }

  public static final class ValueExpression extends openGql.grammar.WhenOperand implements Serializable {
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
    public int compareTo(WhenOperand other) {
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

  public static final class Comparison extends openGql.grammar.WhenOperand implements Serializable {
    public final openGql.grammar.ComparisonPredicatePart2 value;

    public Comparison (openGql.grammar.ComparisonPredicatePart2 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Comparison)) {
        return false;
      }
      Comparison o = (Comparison) other;
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
    public int compareTo(WhenOperand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Comparison o = (Comparison) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NullPredicate extends openGql.grammar.WhenOperand implements Serializable {
    public final openGql.grammar.NullPredicatePart2 value;

    public NullPredicate (openGql.grammar.NullPredicatePart2 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NullPredicate)) {
        return false;
      }
      NullPredicate o = (NullPredicate) other;
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
    public int compareTo(WhenOperand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NullPredicate o = (NullPredicate) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ValueTypePredicate extends openGql.grammar.WhenOperand implements Serializable {
    public final openGql.grammar.ValueTypePredicatePart2 value;

    public ValueTypePredicate (openGql.grammar.ValueTypePredicatePart2 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ValueTypePredicate)) {
        return false;
      }
      ValueTypePredicate o = (ValueTypePredicate) other;
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
    public int compareTo(WhenOperand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ValueTypePredicate o = (ValueTypePredicate) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NormalizedPredicate extends openGql.grammar.WhenOperand implements Serializable {
    public final openGql.grammar.NormalizedPredicatePart2 value;

    public NormalizedPredicate (openGql.grammar.NormalizedPredicatePart2 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NormalizedPredicate)) {
        return false;
      }
      NormalizedPredicate o = (NormalizedPredicate) other;
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
    public int compareTo(WhenOperand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NormalizedPredicate o = (NormalizedPredicate) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DirectedPredicate extends openGql.grammar.WhenOperand implements Serializable {
    public final openGql.grammar.DirectedPredicatePart2 value;

    public DirectedPredicate (openGql.grammar.DirectedPredicatePart2 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DirectedPredicate)) {
        return false;
      }
      DirectedPredicate o = (DirectedPredicate) other;
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
    public int compareTo(WhenOperand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DirectedPredicate o = (DirectedPredicate) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class LabeledPredicate extends openGql.grammar.WhenOperand implements Serializable {
    public final openGql.grammar.LabeledPredicatePart2 value;

    public LabeledPredicate (openGql.grammar.LabeledPredicatePart2 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LabeledPredicate)) {
        return false;
      }
      LabeledPredicate o = (LabeledPredicate) other;
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
    public int compareTo(WhenOperand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LabeledPredicate o = (LabeledPredicate) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class SourcePredicate extends openGql.grammar.WhenOperand implements Serializable {
    public final openGql.grammar.SourcePredicate value;

    public SourcePredicate (openGql.grammar.SourcePredicate value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SourcePredicate)) {
        return false;
      }
      SourcePredicate o = (SourcePredicate) other;
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
    public int compareTo(WhenOperand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SourcePredicate o = (SourcePredicate) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DestinationPredicate extends openGql.grammar.WhenOperand implements Serializable {
    public final openGql.grammar.DestinationPredicate value;

    public DestinationPredicate (openGql.grammar.DestinationPredicate value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DestinationPredicate)) {
        return false;
      }
      DestinationPredicate o = (DestinationPredicate) other;
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
    public int compareTo(WhenOperand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DestinationPredicate o = (DestinationPredicate) other;
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
