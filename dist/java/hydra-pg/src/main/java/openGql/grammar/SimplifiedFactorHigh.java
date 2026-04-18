// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SimplifiedFactorHigh implements Serializable, Comparable<SimplifiedFactorHigh> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SimplifiedFactorHigh");

  public static final hydra.core.Name TERTIARY = new hydra.core.Name("tertiary");

  public static final hydra.core.Name QUANTIFIED = new hydra.core.Name("quantified");

  public static final hydra.core.Name QUESTIONED = new hydra.core.Name("questioned");

  private SimplifiedFactorHigh () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Tertiary instance) ;

    R visit(Quantified instance) ;

    R visit(Questioned instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SimplifiedFactorHigh instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Tertiary instance) {
      return otherwise(instance);
    }

    default R visit(Quantified instance) {
      return otherwise(instance);
    }

    default R visit(Questioned instance) {
      return otherwise(instance);
    }
  }

  public static final class Tertiary extends openGql.grammar.SimplifiedFactorHigh implements Serializable {
    public final openGql.grammar.SimplifiedTertiary value;

    public Tertiary (openGql.grammar.SimplifiedTertiary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tertiary)) {
        return false;
      }
      Tertiary o = (Tertiary) other;
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
    public int compareTo(SimplifiedFactorHigh other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Tertiary o = (Tertiary) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Quantified extends openGql.grammar.SimplifiedFactorHigh implements Serializable {
    public final openGql.grammar.SimplifiedQuantified value;

    public Quantified (openGql.grammar.SimplifiedQuantified value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Quantified)) {
        return false;
      }
      Quantified o = (Quantified) other;
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
    public int compareTo(SimplifiedFactorHigh other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Quantified o = (Quantified) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Questioned extends openGql.grammar.SimplifiedFactorHigh implements Serializable {
    public final openGql.grammar.SimplifiedTertiary value;

    public Questioned (openGql.grammar.SimplifiedTertiary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Questioned)) {
        return false;
      }
      Questioned o = (Questioned) other;
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
    public int compareTo(SimplifiedFactorHigh other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Questioned o = (Questioned) other;
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
