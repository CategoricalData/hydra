// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class ConnectedComponentConstants implements Serializable, Comparable<ConnectedComponentConstants> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.ConnectedComponentConstants");

  public static final hydra.core.Name COMPONENT = new hydra.core.Name("component");

  public static final hydra.core.Name EDGES = new hydra.core.Name("edges");

  public static final hydra.core.Name PROPERTY_NAME = new hydra.core.Name("propertyName");

  private ConnectedComponentConstants () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Component instance) ;

    R visit(Edges instance) ;

    R visit(PropertyName instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ConnectedComponentConstants instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Component instance) {
      return otherwise(instance);
    }

    default R visit(Edges instance) {
      return otherwise(instance);
    }

    default R visit(PropertyName instance) {
      return otherwise(instance);
    }
  }

  public static final class Component extends hydra.tinkerpop.gremlin.ConnectedComponentConstants implements Serializable {
    public Component () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Component)) {
        return false;
      }
      Component o = (Component) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ConnectedComponentConstants other) {
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

  public static final class Edges extends hydra.tinkerpop.gremlin.ConnectedComponentConstants implements Serializable {
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
    public int compareTo(ConnectedComponentConstants other) {
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

  public static final class PropertyName extends hydra.tinkerpop.gremlin.ConnectedComponentConstants implements Serializable {
    public PropertyName () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PropertyName)) {
        return false;
      }
      PropertyName o = (PropertyName) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ConnectedComponentConstants other) {
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
