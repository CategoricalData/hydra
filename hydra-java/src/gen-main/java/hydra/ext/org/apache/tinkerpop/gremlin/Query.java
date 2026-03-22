// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class Query implements Serializable, Comparable<Query> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.Query");

  public static final hydra.core.Name TRAVERSAL_SOURCE = new hydra.core.Name("traversalSource");

  public static final hydra.core.Name ROOT_TRAVERSAL = new hydra.core.Name("rootTraversal");

  public static final hydra.core.Name TO_STRING = new hydra.core.Name("toString");

  public static final hydra.core.Name EMPTY = new hydra.core.Name("empty");

  private Query () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(TraversalSource instance) ;

    R visit(RootTraversal instance) ;

    R visit(ToString instance) ;

    R visit(Empty instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Query instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(TraversalSource instance) {
      return otherwise(instance);
    }

    default R visit(RootTraversal instance) {
      return otherwise(instance);
    }

    default R visit(ToString instance) {
      return otherwise(instance);
    }

    default R visit(Empty instance) {
      return otherwise(instance);
    }
  }

  public static final class TraversalSource extends hydra.ext.org.apache.tinkerpop.gremlin.Query implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceQuery value;

    public TraversalSource (hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceQuery value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TraversalSource)) {
        return false;
      }
      TraversalSource o = (TraversalSource) other;
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
      TraversalSource o = (TraversalSource) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class RootTraversal extends hydra.ext.org.apache.tinkerpop.gremlin.Query implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.RootTraversalQuery value;

    public RootTraversal (hydra.ext.org.apache.tinkerpop.gremlin.RootTraversalQuery value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RootTraversal)) {
        return false;
      }
      RootTraversal o = (RootTraversal) other;
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
      RootTraversal o = (RootTraversal) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ToString extends hydra.ext.org.apache.tinkerpop.gremlin.Query implements Serializable {
    public ToString () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ToString)) {
        return false;
      }
      ToString o = (ToString) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Query other) {
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

  public static final class Empty extends hydra.ext.org.apache.tinkerpop.gremlin.Query implements Serializable {
    public Empty () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Empty)) {
        return false;
      }
      Empty o = (Empty) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Query other) {
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
