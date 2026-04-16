// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ValueExpression implements Serializable, Comparable<ValueExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ValueExpression");

  public static final hydra.core.Name SIGNED = new hydra.core.Name("signed");

  public static final hydra.core.Name MULT_DIV = new hydra.core.Name("multDiv");

  public static final hydra.core.Name ADD_SUBTRACT = new hydra.core.Name("addSubtract");

  public static final hydra.core.Name CONCATENATION = new hydra.core.Name("concatenation");

  public static final hydra.core.Name NOT = new hydra.core.Name("not");

  public static final hydra.core.Name IS_NOT = new hydra.core.Name("isNot");

  public static final hydra.core.Name CONJUNCTIVE = new hydra.core.Name("conjunctive");

  public static final hydra.core.Name DISJUNCTIVE = new hydra.core.Name("disjunctive");

  public static final hydra.core.Name COMPARISON = new hydra.core.Name("comparison");

  public static final hydra.core.Name PREDICATE = new hydra.core.Name("predicate");

  public static final hydra.core.Name NORMALIZED_PREDICATE = new hydra.core.Name("normalizedPredicate");

  public static final hydra.core.Name PROPERTY_GRAPH = new hydra.core.Name("propertyGraph");

  public static final hydra.core.Name BINDING_TABLE = new hydra.core.Name("bindingTable");

  public static final hydra.core.Name VALUE_FUNCTION = new hydra.core.Name("valueFunction");

  public static final hydra.core.Name PRIMARY = new hydra.core.Name("primary");

  private ValueExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Signed instance) ;

    R visit(MultDiv instance) ;

    R visit(AddSubtract instance) ;

    R visit(Concatenation instance) ;

    R visit(Not instance) ;

    R visit(IsNot instance) ;

    R visit(Conjunctive instance) ;

    R visit(Disjunctive instance) ;

    R visit(Comparison instance) ;

    R visit(Predicate instance) ;

    R visit(NormalizedPredicate instance) ;

    R visit(PropertyGraph instance) ;

    R visit(BindingTable instance) ;

    R visit(ValueFunction instance) ;

    R visit(Primary instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ValueExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Signed instance) {
      return otherwise(instance);
    }

    default R visit(MultDiv instance) {
      return otherwise(instance);
    }

    default R visit(AddSubtract instance) {
      return otherwise(instance);
    }

    default R visit(Concatenation instance) {
      return otherwise(instance);
    }

    default R visit(Not instance) {
      return otherwise(instance);
    }

    default R visit(IsNot instance) {
      return otherwise(instance);
    }

    default R visit(Conjunctive instance) {
      return otherwise(instance);
    }

    default R visit(Disjunctive instance) {
      return otherwise(instance);
    }

    default R visit(Comparison instance) {
      return otherwise(instance);
    }

    default R visit(Predicate instance) {
      return otherwise(instance);
    }

    default R visit(NormalizedPredicate instance) {
      return otherwise(instance);
    }

    default R visit(PropertyGraph instance) {
      return otherwise(instance);
    }

    default R visit(BindingTable instance) {
      return otherwise(instance);
    }

    default R visit(ValueFunction instance) {
      return otherwise(instance);
    }

    default R visit(Primary instance) {
      return otherwise(instance);
    }
  }

  public static final class Signed extends openGql.grammar.ValueExpression implements Serializable {
    public final openGql.grammar.SignedExpr value;

    public Signed (openGql.grammar.SignedExpr value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Signed)) {
        return false;
      }
      Signed o = (Signed) other;
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
    public int compareTo(ValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Signed o = (Signed) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class MultDiv extends openGql.grammar.ValueExpression implements Serializable {
    public final openGql.grammar.MultDivExpr value;

    public MultDiv (openGql.grammar.MultDivExpr value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultDiv)) {
        return false;
      }
      MultDiv o = (MultDiv) other;
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
    public int compareTo(ValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MultDiv o = (MultDiv) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class AddSubtract extends openGql.grammar.ValueExpression implements Serializable {
    public final openGql.grammar.AddSubtractExpr value;

    public AddSubtract (openGql.grammar.AddSubtractExpr value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AddSubtract)) {
        return false;
      }
      AddSubtract o = (AddSubtract) other;
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
    public int compareTo(ValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AddSubtract o = (AddSubtract) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Concatenation extends openGql.grammar.ValueExpression implements Serializable {
    public final openGql.grammar.ConcatenationExpr value;

    public Concatenation (openGql.grammar.ConcatenationExpr value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Concatenation)) {
        return false;
      }
      Concatenation o = (Concatenation) other;
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
    public int compareTo(ValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Concatenation o = (Concatenation) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Not extends openGql.grammar.ValueExpression implements Serializable {
    public final openGql.grammar.ValueExpression value;

    public Not (openGql.grammar.ValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Not)) {
        return false;
      }
      Not o = (Not) other;
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
    public int compareTo(ValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Not o = (Not) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class IsNot extends openGql.grammar.ValueExpression implements Serializable {
    public final openGql.grammar.IsNotExpr value;

    public IsNot (openGql.grammar.IsNotExpr value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IsNot)) {
        return false;
      }
      IsNot o = (IsNot) other;
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
    public int compareTo(ValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      IsNot o = (IsNot) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Conjunctive extends openGql.grammar.ValueExpression implements Serializable {
    public final openGql.grammar.ConjunctiveExpr value;

    public Conjunctive (openGql.grammar.ConjunctiveExpr value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Conjunctive)) {
        return false;
      }
      Conjunctive o = (Conjunctive) other;
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
    public int compareTo(ValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Conjunctive o = (Conjunctive) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Disjunctive extends openGql.grammar.ValueExpression implements Serializable {
    public final openGql.grammar.DisjunctiveExpr value;

    public Disjunctive (openGql.grammar.DisjunctiveExpr value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Disjunctive)) {
        return false;
      }
      Disjunctive o = (Disjunctive) other;
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
    public int compareTo(ValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Disjunctive o = (Disjunctive) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Comparison extends openGql.grammar.ValueExpression implements Serializable {
    public final openGql.grammar.ComparisonExpr value;

    public Comparison (openGql.grammar.ComparisonExpr value) {
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
    public int compareTo(ValueExpression other) {
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

  public static final class Predicate extends openGql.grammar.ValueExpression implements Serializable {
    public final openGql.grammar.Predicate value;

    public Predicate (openGql.grammar.Predicate value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Predicate)) {
        return false;
      }
      Predicate o = (Predicate) other;
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
    public int compareTo(ValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Predicate o = (Predicate) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NormalizedPredicate extends openGql.grammar.ValueExpression implements Serializable {
    public final openGql.grammar.NormalizedPredicateExpr value;

    public NormalizedPredicate (openGql.grammar.NormalizedPredicateExpr value) {
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
    public int compareTo(ValueExpression other) {
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

  public static final class PropertyGraph extends openGql.grammar.ValueExpression implements Serializable {
    public final openGql.grammar.GraphExpression value;

    public PropertyGraph (openGql.grammar.GraphExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PropertyGraph)) {
        return false;
      }
      PropertyGraph o = (PropertyGraph) other;
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
    public int compareTo(ValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PropertyGraph o = (PropertyGraph) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class BindingTable extends openGql.grammar.ValueExpression implements Serializable {
    public final openGql.grammar.BindingTableExpression value;

    public BindingTable (openGql.grammar.BindingTableExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BindingTable)) {
        return false;
      }
      BindingTable o = (BindingTable) other;
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
    public int compareTo(ValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BindingTable o = (BindingTable) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ValueFunction extends openGql.grammar.ValueExpression implements Serializable {
    public final openGql.grammar.ValueFunction value;

    public ValueFunction (openGql.grammar.ValueFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ValueFunction)) {
        return false;
      }
      ValueFunction o = (ValueFunction) other;
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
    public int compareTo(ValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ValueFunction o = (ValueFunction) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Primary extends openGql.grammar.ValueExpression implements Serializable {
    public final openGql.grammar.PrimaryValueExpression value;

    public Primary (openGql.grammar.PrimaryValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primary)) {
        return false;
      }
      Primary o = (Primary) other;
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
    public int compareTo(ValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Primary o = (Primary) other;
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
