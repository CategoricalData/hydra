// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class FilterStatement implements Serializable, Comparable<FilterStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.FilterStatement");

  public static final hydra.core.Name WHERE_CLAUSE = new hydra.core.Name("whereClause");

  public static final hydra.core.Name SEARCH_CONDITION = new hydra.core.Name("searchCondition");

  private FilterStatement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(WhereClause instance) ;

    R visit(SearchCondition instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(FilterStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(WhereClause instance) {
      return otherwise(instance);
    }

    default R visit(SearchCondition instance) {
      return otherwise(instance);
    }
  }

  public static final class WhereClause extends openGql.grammar.FilterStatement implements Serializable {
    public final openGql.grammar.ValueExpression value;

    public WhereClause (openGql.grammar.ValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WhereClause)) {
        return false;
      }
      WhereClause o = (WhereClause) other;
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
    public int compareTo(FilterStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      WhereClause o = (WhereClause) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class SearchCondition extends openGql.grammar.FilterStatement implements Serializable {
    public final openGql.grammar.ValueExpression value;

    public SearchCondition (openGql.grammar.ValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SearchCondition)) {
        return false;
      }
      SearchCondition o = (SearchCondition) other;
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
    public int compareTo(FilterStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SearchCondition o = (SearchCondition) other;
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
