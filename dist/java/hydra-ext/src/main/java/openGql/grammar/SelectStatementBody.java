// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SelectStatementBody implements Serializable, Comparable<SelectStatementBody> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SelectStatementBody");

  public static final hydra.core.Name GRAPH_MATCH_LIST = new hydra.core.Name("graphMatchList");

  public static final hydra.core.Name QUERY_SPECIFICATION = new hydra.core.Name("querySpecification");

  private SelectStatementBody () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(GraphMatchList instance) ;

    R visit(QuerySpecification instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SelectStatementBody instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(GraphMatchList instance) {
      return otherwise(instance);
    }

    default R visit(QuerySpecification instance) {
      return otherwise(instance);
    }
  }

  public static final class GraphMatchList extends openGql.grammar.SelectStatementBody implements Serializable {
    public final java.util.List<openGql.grammar.SelectGraphMatch> value;

    public GraphMatchList (java.util.List<openGql.grammar.SelectGraphMatch> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GraphMatchList)) {
        return false;
      }
      GraphMatchList o = (GraphMatchList) other;
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
    public int compareTo(SelectStatementBody other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      GraphMatchList o = (GraphMatchList) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class QuerySpecification extends openGql.grammar.SelectStatementBody implements Serializable {
    public final openGql.grammar.SelectQuerySpecification value;

    public QuerySpecification (openGql.grammar.SelectQuerySpecification value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof QuerySpecification)) {
        return false;
      }
      QuerySpecification o = (QuerySpecification) other;
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
    public int compareTo(SelectStatementBody other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      QuerySpecification o = (QuerySpecification) other;
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
