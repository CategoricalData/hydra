// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalDirection implements Serializable, Comparable<TraversalDirection> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.TraversalDirection");

  public static final hydra.core.Name IN = new hydra.core.Name("in");

  public static final hydra.core.Name OUT = new hydra.core.Name("out");

  public static final hydra.core.Name BOTH = new hydra.core.Name("both");

  private TraversalDirection () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(In instance) ;

    R visit(Out instance) ;

    R visit(Both instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalDirection instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(In instance) {
      return otherwise(instance);
    }

    default R visit(Out instance) {
      return otherwise(instance);
    }

    default R visit(Both instance) {
      return otherwise(instance);
    }
  }

  public static final class In extends hydra.tinkerpop.gremlin.TraversalDirection implements Serializable {
    public In () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof In)) {
        return false;
      }
      In o = (In) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalDirection other) {
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

  public static final class Out extends hydra.tinkerpop.gremlin.TraversalDirection implements Serializable {
    public Out () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Out)) {
        return false;
      }
      Out o = (Out) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalDirection other) {
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

  public static final class Both extends hydra.tinkerpop.gremlin.TraversalDirection implements Serializable {
    public Both () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Both)) {
        return false;
      }
      Both o = (Both) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalDirection other) {
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
