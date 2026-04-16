// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class AggregateFunction implements Serializable, Comparable<AggregateFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.AggregateFunction");

  public static final hydra.core.Name COUNT_ALL = new hydra.core.Name("countAll");

  public static final hydra.core.Name GENERAL_SET_FUNCTION = new hydra.core.Name("generalSetFunction");

  public static final hydra.core.Name BINARY_SET_FUNCTION = new hydra.core.Name("binarySetFunction");

  private AggregateFunction () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(CountAll instance) ;

    R visit(GeneralSetFunction instance) ;

    R visit(BinarySetFunction instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AggregateFunction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(CountAll instance) {
      return otherwise(instance);
    }

    default R visit(GeneralSetFunction instance) {
      return otherwise(instance);
    }

    default R visit(BinarySetFunction instance) {
      return otherwise(instance);
    }
  }

  public static final class CountAll extends openGql.grammar.AggregateFunction implements Serializable {
    public CountAll () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CountAll)) {
        return false;
      }
      CountAll o = (CountAll) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AggregateFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class GeneralSetFunction extends openGql.grammar.AggregateFunction implements Serializable {
    public final openGql.grammar.GeneralSetFunction value;

    public GeneralSetFunction (openGql.grammar.GeneralSetFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GeneralSetFunction)) {
        return false;
      }
      GeneralSetFunction o = (GeneralSetFunction) other;
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
    public int compareTo(AggregateFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      GeneralSetFunction o = (GeneralSetFunction) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class BinarySetFunction extends openGql.grammar.AggregateFunction implements Serializable {
    public final openGql.grammar.BinarySetFunction value;

    public BinarySetFunction (openGql.grammar.BinarySetFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BinarySetFunction)) {
        return false;
      }
      BinarySetFunction o = (BinarySetFunction) other;
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
    public int compareTo(AggregateFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BinarySetFunction o = (BinarySetFunction) other;
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
