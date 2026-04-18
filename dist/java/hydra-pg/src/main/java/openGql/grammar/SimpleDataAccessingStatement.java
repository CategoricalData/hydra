// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SimpleDataAccessingStatement implements Serializable, Comparable<SimpleDataAccessingStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SimpleDataAccessingStatement");

  public static final hydra.core.Name QUERY = new hydra.core.Name("query");

  public static final hydra.core.Name MODIFYING = new hydra.core.Name("modifying");

  private SimpleDataAccessingStatement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Query instance) ;

    R visit(Modifying instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SimpleDataAccessingStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Query instance) {
      return otherwise(instance);
    }

    default R visit(Modifying instance) {
      return otherwise(instance);
    }
  }

  public static final class Query extends openGql.grammar.SimpleDataAccessingStatement implements Serializable {
    public final openGql.grammar.SimpleQueryStatement value;

    public Query (openGql.grammar.SimpleQueryStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Query)) {
        return false;
      }
      Query o = (Query) other;
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
    public int compareTo(SimpleDataAccessingStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Query o = (Query) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Modifying extends openGql.grammar.SimpleDataAccessingStatement implements Serializable {
    public final openGql.grammar.SimpleDataModifyingStatement value;

    public Modifying (openGql.grammar.SimpleDataModifyingStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Modifying)) {
        return false;
      }
      Modifying o = (Modifying) other;
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
    public int compareTo(SimpleDataAccessingStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Modifying o = (Modifying) other;
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
