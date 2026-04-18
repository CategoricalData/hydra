// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class EdgeSynonym implements Serializable, Comparable<EdgeSynonym> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EdgeSynonym");

  public static final hydra.core.Name EDGE = new hydra.core.Name("edge");

  public static final hydra.core.Name RELATIONSHIP = new hydra.core.Name("relationship");

  private EdgeSynonym () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Edge instance) ;

    R visit(Relationship instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EdgeSynonym instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Edge instance) {
      return otherwise(instance);
    }

    default R visit(Relationship instance) {
      return otherwise(instance);
    }
  }

  public static final class Edge extends openGql.grammar.EdgeSynonym implements Serializable {
    public Edge () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Edge)) {
        return false;
      }
      Edge o = (Edge) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(EdgeSynonym other) {
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

  public static final class Relationship extends openGql.grammar.EdgeSynonym implements Serializable {
    public Relationship () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Relationship)) {
        return false;
      }
      Relationship o = (Relationship) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(EdgeSynonym other) {
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
