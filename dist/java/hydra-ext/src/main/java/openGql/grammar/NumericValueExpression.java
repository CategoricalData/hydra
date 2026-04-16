// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class NumericValueExpression implements Serializable, Comparable<NumericValueExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NumericValueExpression");

  public static final hydra.core.Name SIGNED = new hydra.core.Name("signed");

  public static final hydra.core.Name MULTIPLICATION_OR_DIVISION = new hydra.core.Name("multiplicationOrDivision");

  public static final hydra.core.Name ADDITION_OR_SUBTRACTION = new hydra.core.Name("additionOrSubtraction");

  public static final hydra.core.Name PRIMARY = new hydra.core.Name("primary");

  public static final hydra.core.Name FUNCTION = new hydra.core.Name("function");

  private NumericValueExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Signed instance) ;

    R visit(MultiplicationOrDivision instance) ;

    R visit(AdditionOrSubtraction instance) ;

    R visit(Primary instance) ;

    R visit(Function instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NumericValueExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Signed instance) {
      return otherwise(instance);
    }

    default R visit(MultiplicationOrDivision instance) {
      return otherwise(instance);
    }

    default R visit(AdditionOrSubtraction instance) {
      return otherwise(instance);
    }

    default R visit(Primary instance) {
      return otherwise(instance);
    }

    default R visit(Function instance) {
      return otherwise(instance);
    }
  }

  public static final class Signed extends openGql.grammar.NumericValueExpression implements Serializable {
    public final openGql.grammar.SignedNumericValueExpression value;

    public Signed (openGql.grammar.SignedNumericValueExpression value) {
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
    public int compareTo(NumericValueExpression other) {
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

  public static final class MultiplicationOrDivision extends openGql.grammar.NumericValueExpression implements Serializable {
    public final openGql.grammar.MulDivNumericValueExpression value;

    public MultiplicationOrDivision (openGql.grammar.MulDivNumericValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultiplicationOrDivision)) {
        return false;
      }
      MultiplicationOrDivision o = (MultiplicationOrDivision) other;
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
    public int compareTo(NumericValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MultiplicationOrDivision o = (MultiplicationOrDivision) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class AdditionOrSubtraction extends openGql.grammar.NumericValueExpression implements Serializable {
    public final openGql.grammar.AddSubNumericValueExpression value;

    public AdditionOrSubtraction (openGql.grammar.AddSubNumericValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AdditionOrSubtraction)) {
        return false;
      }
      AdditionOrSubtraction o = (AdditionOrSubtraction) other;
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
    public int compareTo(NumericValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AdditionOrSubtraction o = (AdditionOrSubtraction) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Primary extends openGql.grammar.NumericValueExpression implements Serializable {
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
    public int compareTo(NumericValueExpression other) {
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

  public static final class Function extends openGql.grammar.NumericValueExpression implements Serializable {
    public final openGql.grammar.NumericValueFunction value;

    public Function (openGql.grammar.NumericValueFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Function)) {
        return false;
      }
      Function o = (Function) other;
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
    public int compareTo(NumericValueExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Function o = (Function) other;
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
