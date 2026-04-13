// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class ShortestPathConstants implements Serializable, Comparable<ShortestPathConstants> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.ShortestPathConstants");

  public static final hydra.core.Name TARGET = new hydra.core.Name("target");

  public static final hydra.core.Name EDGES = new hydra.core.Name("edges");

  public static final hydra.core.Name DISTANCE = new hydra.core.Name("distance");

  public static final hydra.core.Name MAX_DISTANCE = new hydra.core.Name("maxDistance");

  public static final hydra.core.Name INCLUDE_EDGES = new hydra.core.Name("includeEdges");

  private ShortestPathConstants () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Target instance) ;

    R visit(Edges instance) ;

    R visit(Distance instance) ;

    R visit(MaxDistance instance) ;

    R visit(IncludeEdges instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ShortestPathConstants instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Target instance) {
      return otherwise(instance);
    }

    default R visit(Edges instance) {
      return otherwise(instance);
    }

    default R visit(Distance instance) {
      return otherwise(instance);
    }

    default R visit(MaxDistance instance) {
      return otherwise(instance);
    }

    default R visit(IncludeEdges instance) {
      return otherwise(instance);
    }
  }

  public static final class Target extends hydra.tinkerpop.gremlin.ShortestPathConstants implements Serializable {
    public Target () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Target)) {
        return false;
      }
      Target o = (Target) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ShortestPathConstants other) {
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

  public static final class Edges extends hydra.tinkerpop.gremlin.ShortestPathConstants implements Serializable {
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
    public int compareTo(ShortestPathConstants other) {
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

  public static final class Distance extends hydra.tinkerpop.gremlin.ShortestPathConstants implements Serializable {
    public Distance () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Distance)) {
        return false;
      }
      Distance o = (Distance) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ShortestPathConstants other) {
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

  public static final class MaxDistance extends hydra.tinkerpop.gremlin.ShortestPathConstants implements Serializable {
    public MaxDistance () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MaxDistance)) {
        return false;
      }
      MaxDistance o = (MaxDistance) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ShortestPathConstants other) {
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

  public static final class IncludeEdges extends hydra.tinkerpop.gremlin.ShortestPathConstants implements Serializable {
    public IncludeEdges () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IncludeEdges)) {
        return false;
      }
      IncludeEdges o = (IncludeEdges) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ShortestPathConstants other) {
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
