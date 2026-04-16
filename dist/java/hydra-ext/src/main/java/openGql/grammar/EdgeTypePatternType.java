// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class EdgeTypePatternType implements Serializable, Comparable<EdgeTypePatternType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EdgeTypePatternType");

  public static final hydra.core.Name DIRECTED = new hydra.core.Name("directed");

  public static final hydra.core.Name UNDIRECTED = new hydra.core.Name("undirected");

  private EdgeTypePatternType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Directed instance) ;

    R visit(Undirected instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EdgeTypePatternType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Directed instance) {
      return otherwise(instance);
    }

    default R visit(Undirected instance) {
      return otherwise(instance);
    }
  }

  public static final class Directed extends openGql.grammar.EdgeTypePatternType implements Serializable {
    public final openGql.grammar.EdgeTypePatternDirected value;

    public Directed (openGql.grammar.EdgeTypePatternDirected value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Directed)) {
        return false;
      }
      Directed o = (Directed) other;
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
    public int compareTo(EdgeTypePatternType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Directed o = (Directed) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Undirected extends openGql.grammar.EdgeTypePatternType implements Serializable {
    public final openGql.grammar.EdgeTypePatternUndirected value;

    public Undirected (openGql.grammar.EdgeTypePatternUndirected value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Undirected)) {
        return false;
      }
      Undirected o = (Undirected) other;
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
    public int compareTo(EdgeTypePatternType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Undirected o = (Undirected) other;
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
