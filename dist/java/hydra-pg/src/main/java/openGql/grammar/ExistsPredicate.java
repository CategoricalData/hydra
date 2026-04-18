// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ExistsPredicate implements Serializable, Comparable<ExistsPredicate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ExistsPredicate");

  public static final hydra.core.Name GRAPH_PATTERN_BRACE = new hydra.core.Name("graphPatternBrace");

  public static final hydra.core.Name GRAPH_PATTERN_PAREN = new hydra.core.Name("graphPatternParen");

  public static final hydra.core.Name MATCH_BLOCK_BRACE = new hydra.core.Name("matchBlockBrace");

  public static final hydra.core.Name MATCH_BLOCK_PAREN = new hydra.core.Name("matchBlockParen");

  public static final hydra.core.Name NESTED_QUERY = new hydra.core.Name("nestedQuery");

  private ExistsPredicate () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(GraphPatternBrace instance) ;

    R visit(GraphPatternParen instance) ;

    R visit(MatchBlockBrace instance) ;

    R visit(MatchBlockParen instance) ;

    R visit(NestedQuery instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ExistsPredicate instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(GraphPatternBrace instance) {
      return otherwise(instance);
    }

    default R visit(GraphPatternParen instance) {
      return otherwise(instance);
    }

    default R visit(MatchBlockBrace instance) {
      return otherwise(instance);
    }

    default R visit(MatchBlockParen instance) {
      return otherwise(instance);
    }

    default R visit(NestedQuery instance) {
      return otherwise(instance);
    }
  }

  public static final class GraphPatternBrace extends openGql.grammar.ExistsPredicate implements Serializable {
    public final openGql.grammar.GraphPattern value;

    public GraphPatternBrace (openGql.grammar.GraphPattern value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GraphPatternBrace)) {
        return false;
      }
      GraphPatternBrace o = (GraphPatternBrace) other;
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
    public int compareTo(ExistsPredicate other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      GraphPatternBrace o = (GraphPatternBrace) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class GraphPatternParen extends openGql.grammar.ExistsPredicate implements Serializable {
    public final openGql.grammar.GraphPattern value;

    public GraphPatternParen (openGql.grammar.GraphPattern value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GraphPatternParen)) {
        return false;
      }
      GraphPatternParen o = (GraphPatternParen) other;
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
    public int compareTo(ExistsPredicate other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      GraphPatternParen o = (GraphPatternParen) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class MatchBlockBrace extends openGql.grammar.ExistsPredicate implements Serializable {
    public final java.util.List<openGql.grammar.MatchStatement> value;

    public MatchBlockBrace (java.util.List<openGql.grammar.MatchStatement> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MatchBlockBrace)) {
        return false;
      }
      MatchBlockBrace o = (MatchBlockBrace) other;
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
    public int compareTo(ExistsPredicate other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MatchBlockBrace o = (MatchBlockBrace) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class MatchBlockParen extends openGql.grammar.ExistsPredicate implements Serializable {
    public final java.util.List<openGql.grammar.MatchStatement> value;

    public MatchBlockParen (java.util.List<openGql.grammar.MatchStatement> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MatchBlockParen)) {
        return false;
      }
      MatchBlockParen o = (MatchBlockParen) other;
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
    public int compareTo(ExistsPredicate other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MatchBlockParen o = (MatchBlockParen) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NestedQuery extends openGql.grammar.ExistsPredicate implements Serializable {
    public final openGql.grammar.ProcedureBody value;

    public NestedQuery (openGql.grammar.ProcedureBody value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NestedQuery)) {
        return false;
      }
      NestedQuery o = (NestedQuery) other;
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
    public int compareTo(ExistsPredicate other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NestedQuery o = (NestedQuery) other;
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
