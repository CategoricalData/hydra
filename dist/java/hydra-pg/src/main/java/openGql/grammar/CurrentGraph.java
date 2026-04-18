// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class CurrentGraph implements Serializable, Comparable<CurrentGraph> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CurrentGraph");

  public static final hydra.core.Name GRAPH = new hydra.core.Name("graph");

  public static final hydra.core.Name PROPERTY_GRAPH = new hydra.core.Name("propertyGraph");

  private CurrentGraph () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Graph instance) ;

    R visit(PropertyGraph instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CurrentGraph instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Graph instance) {
      return otherwise(instance);
    }

    default R visit(PropertyGraph instance) {
      return otherwise(instance);
    }
  }

  public static final class Graph extends openGql.grammar.CurrentGraph implements Serializable {
    public Graph () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Graph)) {
        return false;
      }
      Graph o = (Graph) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CurrentGraph other) {
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

  public static final class PropertyGraph extends openGql.grammar.CurrentGraph implements Serializable {
    public PropertyGraph () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PropertyGraph)) {
        return false;
      }
      PropertyGraph o = (PropertyGraph) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CurrentGraph other) {
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
