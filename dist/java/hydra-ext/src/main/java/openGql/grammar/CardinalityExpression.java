// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class CardinalityExpression implements Serializable, Comparable<CardinalityExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CardinalityExpression");

  public static final hydra.core.Name CARDINALITY = new hydra.core.Name("cardinality");

  public static final hydra.core.Name SIZE = new hydra.core.Name("size");

  private CardinalityExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Cardinality instance) ;

    R visit(Size instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CardinalityExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Cardinality instance) {
      return otherwise(instance);
    }

    default R visit(Size instance) {
      return otherwise(instance);
    }
  }

  public static final class Cardinality extends openGql.grammar.CardinalityExpression implements Serializable {
    public final openGql.grammar.ValueExpression value;

    public Cardinality (openGql.grammar.ValueExpression value) {
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
    public int compareTo(CardinalityExpression other) {
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

  public static final class Size extends openGql.grammar.CardinalityExpression implements Serializable {
    public final openGql.grammar.ValueExpression value;

    public Size (openGql.grammar.ValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Size)) {
        return false;
      }
      Size o = (Size) other;
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
    public int compareTo(CardinalityExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Size o = (Size) other;
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
