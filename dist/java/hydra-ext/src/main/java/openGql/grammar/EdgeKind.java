// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class EdgeKind implements Serializable, Comparable<EdgeKind> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EdgeKind");

  public static final hydra.core.Name DIRECTED = new hydra.core.Name("directed");

  public static final hydra.core.Name UNDIRECTED = new hydra.core.Name("undirected");

  private EdgeKind () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Directed instance) ;

    R visit(Undirected instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EdgeKind instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Directed instance) {
      return otherwise(instance);
    }

    default R visit(Undirected instance) {
      return otherwise(instance);
    }
  }

  public static final class Directed extends openGql.grammar.EdgeKind implements Serializable {
    public Directed () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Directed)) {
        return false;
      }
      Directed o = (Directed) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(EdgeKind other) {
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

  public static final class Undirected extends openGql.grammar.EdgeKind implements Serializable {
    public Undirected () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Undirected)) {
        return false;
      }
      Undirected o = (Undirected) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(EdgeKind other) {
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
}
