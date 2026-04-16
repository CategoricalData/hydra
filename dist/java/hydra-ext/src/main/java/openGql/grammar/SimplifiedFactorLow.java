// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SimplifiedFactorLow implements Serializable, Comparable<SimplifiedFactorLow> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SimplifiedFactorLow");

  public static final hydra.core.Name FACTOR_HIGH = new hydra.core.Name("factorHigh");

  public static final hydra.core.Name CONJUNCTION = new hydra.core.Name("conjunction");

  private SimplifiedFactorLow () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(FactorHigh instance) ;

    R visit(Conjunction instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SimplifiedFactorLow instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(FactorHigh instance) {
      return otherwise(instance);
    }

    default R visit(Conjunction instance) {
      return otherwise(instance);
    }
  }

  public static final class FactorHigh extends openGql.grammar.SimplifiedFactorLow implements Serializable {
    public final openGql.grammar.SimplifiedFactorHigh value;

    public FactorHigh (openGql.grammar.SimplifiedFactorHigh value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FactorHigh)) {
        return false;
      }
      FactorHigh o = (FactorHigh) other;
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
    public int compareTo(SimplifiedFactorLow other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      FactorHigh o = (FactorHigh) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Conjunction extends openGql.grammar.SimplifiedFactorLow implements Serializable {
    public final openGql.grammar.SimplifiedConjunction value;

    public Conjunction (openGql.grammar.SimplifiedConjunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Conjunction)) {
        return false;
      }
      Conjunction o = (Conjunction) other;
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
    public int compareTo(SimplifiedFactorLow other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Conjunction o = (Conjunction) other;
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
