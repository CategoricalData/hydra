// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class OrderByAndPageStatement implements Serializable, Comparable<OrderByAndPageStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.OrderByAndPageStatement");

  public static final hydra.core.Name ORDER_BY_AND_OPTIONAL_OFFSET_AND_LIMIT = new hydra.core.Name("orderByAndOptionalOffsetAndLimit");

  public static final hydra.core.Name OFFSET_AND_OPTIONAL_LIMIT = new hydra.core.Name("offsetAndOptionalLimit");

  public static final hydra.core.Name LIMIT_ONLY = new hydra.core.Name("limitOnly");

  private OrderByAndPageStatement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(OrderByAndOptionalOffsetAndLimit instance) ;

    R visit(OffsetAndOptionalLimit instance) ;

    R visit(LimitOnly instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OrderByAndPageStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(OrderByAndOptionalOffsetAndLimit instance) {
      return otherwise(instance);
    }

    default R visit(OffsetAndOptionalLimit instance) {
      return otherwise(instance);
    }

    default R visit(LimitOnly instance) {
      return otherwise(instance);
    }
  }

  public static final class OrderByAndOptionalOffsetAndLimit extends openGql.grammar.OrderByAndPageStatement implements Serializable {
    public final openGql.grammar.OrderByAndOptionalOffsetAndLimit value;

    public OrderByAndOptionalOffsetAndLimit (openGql.grammar.OrderByAndOptionalOffsetAndLimit value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OrderByAndOptionalOffsetAndLimit)) {
        return false;
      }
      OrderByAndOptionalOffsetAndLimit o = (OrderByAndOptionalOffsetAndLimit) other;
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
    public int compareTo(OrderByAndPageStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      OrderByAndOptionalOffsetAndLimit o = (OrderByAndOptionalOffsetAndLimit) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class OffsetAndOptionalLimit extends openGql.grammar.OrderByAndPageStatement implements Serializable {
    public final openGql.grammar.OffsetAndOptionalLimit value;

    public OffsetAndOptionalLimit (openGql.grammar.OffsetAndOptionalLimit value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OffsetAndOptionalLimit)) {
        return false;
      }
      OffsetAndOptionalLimit o = (OffsetAndOptionalLimit) other;
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
    public int compareTo(OrderByAndPageStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      OffsetAndOptionalLimit o = (OffsetAndOptionalLimit) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class LimitOnly extends openGql.grammar.OrderByAndPageStatement implements Serializable {
    public final openGql.grammar.NonNegativeIntegerSpecification value;

    public LimitOnly (openGql.grammar.NonNegativeIntegerSpecification value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LimitOnly)) {
        return false;
      }
      LimitOnly o = (LimitOnly) other;
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
    public int compareTo(OrderByAndPageStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LimitOnly o = (LimitOnly) other;
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
