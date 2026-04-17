// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class NumericValueFunction implements Serializable, Comparable<NumericValueFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NumericValueFunction");

  public static final hydra.core.Name LENGTH = new hydra.core.Name("length");

  public static final hydra.core.Name CARDINALITY = new hydra.core.Name("cardinality");

  public static final hydra.core.Name ABSOLUTE_VALUE = new hydra.core.Name("absoluteValue");

  public static final hydra.core.Name MODULUS = new hydra.core.Name("modulus");

  public static final hydra.core.Name TRIGONOMETRIC = new hydra.core.Name("trigonometric");

  public static final hydra.core.Name LOGARITHM = new hydra.core.Name("logarithm");

  public static final hydra.core.Name COMMON_LOGARITHM = new hydra.core.Name("commonLogarithm");

  public static final hydra.core.Name NATURAL_LOGARITHM = new hydra.core.Name("naturalLogarithm");

  public static final hydra.core.Name EXPONENTIAL = new hydra.core.Name("exponential");

  public static final hydra.core.Name POWER = new hydra.core.Name("power");

  public static final hydra.core.Name SQUARE_ROOT = new hydra.core.Name("squareRoot");

  public static final hydra.core.Name FLOOR = new hydra.core.Name("floor");

  public static final hydra.core.Name CEILING = new hydra.core.Name("ceiling");

  private NumericValueFunction () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Length instance) ;

    R visit(Cardinality instance) ;

    R visit(AbsoluteValue instance) ;

    R visit(Modulus instance) ;

    R visit(Trigonometric instance) ;

    R visit(Logarithm instance) ;

    R visit(CommonLogarithm instance) ;

    R visit(NaturalLogarithm instance) ;

    R visit(Exponential instance) ;

    R visit(Power instance) ;

    R visit(SquareRoot instance) ;

    R visit(Floor instance) ;

    R visit(Ceiling instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NumericValueFunction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Length instance) {
      return otherwise(instance);
    }

    default R visit(Cardinality instance) {
      return otherwise(instance);
    }

    default R visit(AbsoluteValue instance) {
      return otherwise(instance);
    }

    default R visit(Modulus instance) {
      return otherwise(instance);
    }

    default R visit(Trigonometric instance) {
      return otherwise(instance);
    }

    default R visit(Logarithm instance) {
      return otherwise(instance);
    }

    default R visit(CommonLogarithm instance) {
      return otherwise(instance);
    }

    default R visit(NaturalLogarithm instance) {
      return otherwise(instance);
    }

    default R visit(Exponential instance) {
      return otherwise(instance);
    }

    default R visit(Power instance) {
      return otherwise(instance);
    }

    default R visit(SquareRoot instance) {
      return otherwise(instance);
    }

    default R visit(Floor instance) {
      return otherwise(instance);
    }

    default R visit(Ceiling instance) {
      return otherwise(instance);
    }
  }

  public static final class Length extends openGql.grammar.NumericValueFunction implements Serializable {
    public final openGql.grammar.LengthExpression value;

    public Length (openGql.grammar.LengthExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Length)) {
        return false;
      }
      Length o = (Length) other;
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
    public int compareTo(NumericValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Length o = (Length) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Cardinality extends openGql.grammar.NumericValueFunction implements Serializable {
    public final openGql.grammar.CardinalityExpression value;

    public Cardinality (openGql.grammar.CardinalityExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cardinality)) {
        return false;
      }
      Cardinality o = (Cardinality) other;
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
    public int compareTo(NumericValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Cardinality o = (Cardinality) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class AbsoluteValue extends openGql.grammar.NumericValueFunction implements Serializable {
    public final openGql.grammar.ValueExpression value;

    public AbsoluteValue (openGql.grammar.ValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AbsoluteValue)) {
        return false;
      }
      AbsoluteValue o = (AbsoluteValue) other;
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
    public int compareTo(NumericValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AbsoluteValue o = (AbsoluteValue) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Modulus extends openGql.grammar.NumericValueFunction implements Serializable {
    public final openGql.grammar.ModulusExpression value;

    public Modulus (openGql.grammar.ModulusExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Modulus)) {
        return false;
      }
      Modulus o = (Modulus) other;
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
    public int compareTo(NumericValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Modulus o = (Modulus) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Trigonometric extends openGql.grammar.NumericValueFunction implements Serializable {
    public final openGql.grammar.TrigonometricFunction value;

    public Trigonometric (openGql.grammar.TrigonometricFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Trigonometric)) {
        return false;
      }
      Trigonometric o = (Trigonometric) other;
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
    public int compareTo(NumericValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Trigonometric o = (Trigonometric) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Logarithm extends openGql.grammar.NumericValueFunction implements Serializable {
    public final openGql.grammar.GeneralLogarithmFunction value;

    public Logarithm (openGql.grammar.GeneralLogarithmFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Logarithm)) {
        return false;
      }
      Logarithm o = (Logarithm) other;
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
    public int compareTo(NumericValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Logarithm o = (Logarithm) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CommonLogarithm extends openGql.grammar.NumericValueFunction implements Serializable {
    public final openGql.grammar.NumericValueExpression value;

    public CommonLogarithm (openGql.grammar.NumericValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CommonLogarithm)) {
        return false;
      }
      CommonLogarithm o = (CommonLogarithm) other;
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
    public int compareTo(NumericValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CommonLogarithm o = (CommonLogarithm) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NaturalLogarithm extends openGql.grammar.NumericValueFunction implements Serializable {
    public final openGql.grammar.NumericValueExpression value;

    public NaturalLogarithm (openGql.grammar.NumericValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NaturalLogarithm)) {
        return false;
      }
      NaturalLogarithm o = (NaturalLogarithm) other;
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
    public int compareTo(NumericValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NaturalLogarithm o = (NaturalLogarithm) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Exponential extends openGql.grammar.NumericValueFunction implements Serializable {
    public final openGql.grammar.NumericValueExpression value;

    public Exponential (openGql.grammar.NumericValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Exponential)) {
        return false;
      }
      Exponential o = (Exponential) other;
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
    public int compareTo(NumericValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Exponential o = (Exponential) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Power extends openGql.grammar.NumericValueFunction implements Serializable {
    public final openGql.grammar.PowerFunction value;

    public Power (openGql.grammar.PowerFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Power)) {
        return false;
      }
      Power o = (Power) other;
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
    public int compareTo(NumericValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Power o = (Power) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class SquareRoot extends openGql.grammar.NumericValueFunction implements Serializable {
    public final openGql.grammar.NumericValueExpression value;

    public SquareRoot (openGql.grammar.NumericValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SquareRoot)) {
        return false;
      }
      SquareRoot o = (SquareRoot) other;
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
    public int compareTo(NumericValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SquareRoot o = (SquareRoot) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Floor extends openGql.grammar.NumericValueFunction implements Serializable {
    public final openGql.grammar.NumericValueExpression value;

    public Floor (openGql.grammar.NumericValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Floor)) {
        return false;
      }
      Floor o = (Floor) other;
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
    public int compareTo(NumericValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Floor o = (Floor) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Ceiling extends openGql.grammar.NumericValueFunction implements Serializable {
    public final openGql.grammar.NumericValueExpression value;

    public Ceiling (openGql.grammar.NumericValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ceiling)) {
        return false;
      }
      Ceiling o = (Ceiling) other;
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
    public int compareTo(NumericValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Ceiling o = (Ceiling) other;
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
