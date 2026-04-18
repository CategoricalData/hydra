// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class EdgesSynonym implements Serializable, Comparable<EdgesSynonym> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EdgesSynonym");

  public static final hydra.core.Name EDGES = new hydra.core.Name("edges");

  public static final hydra.core.Name RELATIONSHIPS = new hydra.core.Name("relationships");

  private EdgesSynonym () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Edges instance) ;

    R visit(Relationships instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EdgesSynonym instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Edges instance) {
      return otherwise(instance);
    }

    default R visit(Relationships instance) {
      return otherwise(instance);
    }
  }

  public static final class Edges extends openGql.grammar.EdgesSynonym implements Serializable {
    public Edges () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Edges)) {
        return false;
      }
      Edges o = (Edges) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(EdgesSynonym other) {
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

  public static final class Relationships extends openGql.grammar.EdgesSynonym implements Serializable {
    public Relationships () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Relationships)) {
        return false;
      }
      Relationships o = (Relationships) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(EdgesSynonym other) {
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
