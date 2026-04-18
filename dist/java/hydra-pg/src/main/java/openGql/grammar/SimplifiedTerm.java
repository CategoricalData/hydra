// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SimplifiedTerm implements Serializable, Comparable<SimplifiedTerm> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SimplifiedTerm");

  public static final hydra.core.Name FACTOR_LOW = new hydra.core.Name("factorLow");

  public static final hydra.core.Name CONCATENATION = new hydra.core.Name("concatenation");

  private SimplifiedTerm () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(FactorLow instance) ;

    R visit(Concatenation instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SimplifiedTerm instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(FactorLow instance) {
      return otherwise(instance);
    }

    default R visit(Concatenation instance) {
      return otherwise(instance);
    }
  }

  public static final class FactorLow extends openGql.grammar.SimplifiedTerm implements Serializable {
    public final openGql.grammar.SimplifiedFactorLow value;

    public FactorLow (openGql.grammar.SimplifiedFactorLow value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FactorLow)) {
        return false;
      }
      FactorLow o = (FactorLow) other;
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
    public int compareTo(SimplifiedTerm other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      FactorLow o = (FactorLow) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Concatenation extends openGql.grammar.SimplifiedTerm implements Serializable {
    public final openGql.grammar.SimplifiedConcatenation value;

    public Concatenation (openGql.grammar.SimplifiedConcatenation value) {
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
    public int compareTo(SimplifiedTerm other) {
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
}
