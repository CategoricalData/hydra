// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public abstract class Query implements Serializable, Comparable<Query> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.query.Query");

  public static final hydra.core.Name APPLICATION = new hydra.core.Name("application");

  public static final hydra.core.Name AGGREGATE = new hydra.core.Name("aggregate");

  public static final hydra.core.Name LET_QUERY = new hydra.core.Name("LetQuery");

  public static final hydra.core.Name MATCH = new hydra.core.Name("match");

  public static final hydra.core.Name SELECT = new hydra.core.Name("select");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  private Query () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Application instance) ;

    R visit(Aggregate instance) ;

    R visit(LetQuery instance) ;

    R visit(Match instance) ;

    R visit(Select instance) ;

    R visit(Value instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Query instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Application instance) {
      return otherwise(instance);
    }

    default R visit(Aggregate instance) {
      return otherwise(instance);
    }

    default R visit(LetQuery instance) {
      return otherwise(instance);
    }

    default R visit(Match instance) {
      return otherwise(instance);
    }

    default R visit(Select instance) {
      return otherwise(instance);
    }

    default R visit(Value instance) {
      return otherwise(instance);
    }
  }

  public static final class Application extends hydra.pg.query.Query implements Serializable {
    public final hydra.pg.query.ApplicationQuery value;

    public Application (hydra.pg.query.ApplicationQuery value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Application)) {
        return false;
      }
      Application o = (Application) other;
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
    public int compareTo(Query other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Application o = (Application) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Aggregate extends hydra.pg.query.Query implements Serializable {
    public final hydra.pg.query.AggregationQuery value;

    public Aggregate (hydra.pg.query.AggregationQuery value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Aggregate)) {
        return false;
      }
      Aggregate o = (Aggregate) other;
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
    public int compareTo(Query other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Aggregate o = (Aggregate) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class LetQuery extends hydra.pg.query.Query implements Serializable {
    public final hydra.pg.query.LetQuery value;

    public LetQuery (hydra.pg.query.LetQuery value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LetQuery)) {
        return false;
      }
      LetQuery o = (LetQuery) other;
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
    public int compareTo(Query other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LetQuery o = (LetQuery) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Match extends hydra.pg.query.Query implements Serializable {
    public final hydra.pg.query.MatchQuery value;

    public Match (hydra.pg.query.MatchQuery value) {
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
    public int compareTo(Query other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Match o = (Match) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Select extends hydra.pg.query.Query implements Serializable {
    public final hydra.pg.query.SelectQuery value;

    public Select (hydra.pg.query.SelectQuery value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Select)) {
        return false;
      }
      Select o = (Select) other;
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
    public int compareTo(Query other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Select o = (Select) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Value extends hydra.pg.query.Query implements Serializable {
    public final String value;

    public Value (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Value)) {
        return false;
      }
      Value o = (Value) other;
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
    public int compareTo(Query other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Value o = (Value) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
