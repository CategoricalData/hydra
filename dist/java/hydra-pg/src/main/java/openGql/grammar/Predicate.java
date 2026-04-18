// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class Predicate implements Serializable, Comparable<Predicate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.Predicate");

  public static final hydra.core.Name EXISTS_PREDICATE = new hydra.core.Name("existsPredicate");

  public static final hydra.core.Name NULL_PREDICATE = new hydra.core.Name("nullPredicate");

  public static final hydra.core.Name VALUE_TYPE_PREDICATE = new hydra.core.Name("valueTypePredicate");

  public static final hydra.core.Name DIRECTED_PREDICATE = new hydra.core.Name("directedPredicate");

  public static final hydra.core.Name LABELED_PREDICATE = new hydra.core.Name("labeledPredicate");

  public static final hydra.core.Name SOURCE_DESTINATION_PREDICATE = new hydra.core.Name("sourceDestinationPredicate");

  public static final hydra.core.Name ALL_DIFFERENT_PREDICATE = new hydra.core.Name("allDifferentPredicate");

  public static final hydra.core.Name SAME_PREDICATE = new hydra.core.Name("samePredicate");

  public static final hydra.core.Name PROPERTY_EXISTS_PREDICATE = new hydra.core.Name("propertyExistsPredicate");

  private Predicate () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ExistsPredicate instance) ;

    R visit(NullPredicate instance) ;

    R visit(ValueTypePredicate instance) ;

    R visit(DirectedPredicate instance) ;

    R visit(LabeledPredicate instance) ;

    R visit(SourceDestinationPredicate instance) ;

    R visit(AllDifferentPredicate instance) ;

    R visit(SamePredicate instance) ;

    R visit(PropertyExistsPredicate instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Predicate instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ExistsPredicate instance) {
      return otherwise(instance);
    }

    default R visit(NullPredicate instance) {
      return otherwise(instance);
    }

    default R visit(ValueTypePredicate instance) {
      return otherwise(instance);
    }

    default R visit(DirectedPredicate instance) {
      return otherwise(instance);
    }

    default R visit(LabeledPredicate instance) {
      return otherwise(instance);
    }

    default R visit(SourceDestinationPredicate instance) {
      return otherwise(instance);
    }

    default R visit(AllDifferentPredicate instance) {
      return otherwise(instance);
    }

    default R visit(SamePredicate instance) {
      return otherwise(instance);
    }

    default R visit(PropertyExistsPredicate instance) {
      return otherwise(instance);
    }
  }

  public static final class ExistsPredicate extends openGql.grammar.Predicate implements Serializable {
    public final openGql.grammar.ExistsPredicate value;

    public ExistsPredicate (openGql.grammar.ExistsPredicate value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ExistsPredicate)) {
        return false;
      }
      ExistsPredicate o = (ExistsPredicate) other;
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
    public int compareTo(Predicate other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ExistsPredicate o = (ExistsPredicate) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NullPredicate extends openGql.grammar.Predicate implements Serializable {
    public final openGql.grammar.NullPredicate value;

    public NullPredicate (openGql.grammar.NullPredicate value) {
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
    public int compareTo(Predicate other) {
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

  public static final class ValueTypePredicate extends openGql.grammar.Predicate implements Serializable {
    public final openGql.grammar.ValueTypePredicate value;

    public ValueTypePredicate (openGql.grammar.ValueTypePredicate value) {
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
    public int compareTo(Predicate other) {
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

  public static final class DirectedPredicate extends openGql.grammar.Predicate implements Serializable {
    public final openGql.grammar.DirectedPredicate value;

    public DirectedPredicate (openGql.grammar.DirectedPredicate value) {
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
    public int compareTo(Predicate other) {
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

  public static final class LabeledPredicate extends openGql.grammar.Predicate implements Serializable {
    public final openGql.grammar.LabeledPredicate value;

    public LabeledPredicate (openGql.grammar.LabeledPredicate value) {
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
    public int compareTo(Predicate other) {
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

  public static final class SourceDestinationPredicate extends openGql.grammar.Predicate implements Serializable {
    public final openGql.grammar.SourceDestinationPredicate value;

    public SourceDestinationPredicate (openGql.grammar.SourceDestinationPredicate value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SourceDestinationPredicate)) {
        return false;
      }
      SourceDestinationPredicate o = (SourceDestinationPredicate) other;
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
    public int compareTo(Predicate other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SourceDestinationPredicate o = (SourceDestinationPredicate) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class AllDifferentPredicate extends openGql.grammar.Predicate implements Serializable {
    public final openGql.grammar.AllDifferentPredicate value;

    public AllDifferentPredicate (openGql.grammar.AllDifferentPredicate value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AllDifferentPredicate)) {
        return false;
      }
      AllDifferentPredicate o = (AllDifferentPredicate) other;
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
    public int compareTo(Predicate other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AllDifferentPredicate o = (AllDifferentPredicate) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class SamePredicate extends openGql.grammar.Predicate implements Serializable {
    public final openGql.grammar.SamePredicate value;

    public SamePredicate (openGql.grammar.SamePredicate value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SamePredicate)) {
        return false;
      }
      SamePredicate o = (SamePredicate) other;
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
    public int compareTo(Predicate other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SamePredicate o = (SamePredicate) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PropertyExistsPredicate extends openGql.grammar.Predicate implements Serializable {
    public final openGql.grammar.PropertyExistsPredicate value;

    public PropertyExistsPredicate (openGql.grammar.PropertyExistsPredicate value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PropertyExistsPredicate)) {
        return false;
      }
      PropertyExistsPredicate o = (PropertyExistsPredicate) other;
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
    public int compareTo(Predicate other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PropertyExistsPredicate o = (PropertyExistsPredicate) other;
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
