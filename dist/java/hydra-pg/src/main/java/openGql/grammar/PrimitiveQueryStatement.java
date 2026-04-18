// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class PrimitiveQueryStatement implements Serializable, Comparable<PrimitiveQueryStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PrimitiveQueryStatement");

  public static final hydra.core.Name MATCH = new hydra.core.Name("match");

  public static final hydra.core.Name LET = new hydra.core.Name("let");

  public static final hydra.core.Name FOR = new hydra.core.Name("for");

  public static final hydra.core.Name FILTER = new hydra.core.Name("filter");

  public static final hydra.core.Name ORDER_BY_AND_PAGE = new hydra.core.Name("orderByAndPage");

  private PrimitiveQueryStatement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Match instance) ;

    R visit(Let instance) ;

    R visit(For instance) ;

    R visit(Filter instance) ;

    R visit(OrderByAndPage instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PrimitiveQueryStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Match instance) {
      return otherwise(instance);
    }

    default R visit(Let instance) {
      return otherwise(instance);
    }

    default R visit(For instance) {
      return otherwise(instance);
    }

    default R visit(Filter instance) {
      return otherwise(instance);
    }

    default R visit(OrderByAndPage instance) {
      return otherwise(instance);
    }
  }

  public static final class Match extends openGql.grammar.PrimitiveQueryStatement implements Serializable {
    public final openGql.grammar.MatchStatement value;

    public Match (openGql.grammar.MatchStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Match)) {
        return false;
      }
      Match o = (Match) other;
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
    public int compareTo(PrimitiveQueryStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Match o = (Match) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Let extends openGql.grammar.PrimitiveQueryStatement implements Serializable {
    public final java.util.List<openGql.grammar.LetVariableDefinition> value;

    public Let (java.util.List<openGql.grammar.LetVariableDefinition> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Let)) {
        return false;
      }
      Let o = (Let) other;
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
    public int compareTo(PrimitiveQueryStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Let o = (Let) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class For extends openGql.grammar.PrimitiveQueryStatement implements Serializable {
    public final openGql.grammar.ForStatement value;

    public For (openGql.grammar.ForStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof For)) {
        return false;
      }
      For o = (For) other;
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
    public int compareTo(PrimitiveQueryStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      For o = (For) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Filter extends openGql.grammar.PrimitiveQueryStatement implements Serializable {
    public final openGql.grammar.FilterStatement value;

    public Filter (openGql.grammar.FilterStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Filter)) {
        return false;
      }
      Filter o = (Filter) other;
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
    public int compareTo(PrimitiveQueryStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Filter o = (Filter) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class OrderByAndPage extends openGql.grammar.PrimitiveQueryStatement implements Serializable {
    public final openGql.grammar.OrderByAndPageStatement value;

    public OrderByAndPage (openGql.grammar.OrderByAndPageStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OrderByAndPage)) {
        return false;
      }
      OrderByAndPage o = (OrderByAndPage) other;
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
    public int compareTo(PrimitiveQueryStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      OrderByAndPage o = (OrderByAndPage) other;
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
