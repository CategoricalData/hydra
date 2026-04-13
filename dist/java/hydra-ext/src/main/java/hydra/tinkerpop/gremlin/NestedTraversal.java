// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class NestedTraversal implements Serializable, Comparable<NestedTraversal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.NestedTraversal");

  public static final hydra.core.Name ROOT = new hydra.core.Name("root");

  public static final hydra.core.Name CHAINED = new hydra.core.Name("chained");

  public static final hydra.core.Name ANONYMOUS = new hydra.core.Name("anonymous");

  private NestedTraversal () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Root instance) ;

    R visit(Chained instance) ;

    R visit(Anonymous instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NestedTraversal instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Root instance) {
      return otherwise(instance);
    }

    default R visit(Chained instance) {
      return otherwise(instance);
    }

    default R visit(Anonymous instance) {
      return otherwise(instance);
    }
  }

  public static final class Root extends hydra.tinkerpop.gremlin.NestedTraversal implements Serializable {
    public final hydra.tinkerpop.gremlin.RootTraversal value;

    public Root (hydra.tinkerpop.gremlin.RootTraversal value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Root)) {
        return false;
      }
      Root o = (Root) other;
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
    public int compareTo(NestedTraversal other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Root o = (Root) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Chained extends hydra.tinkerpop.gremlin.NestedTraversal implements Serializable {
    public final hydra.tinkerpop.gremlin.ChainedTraversal value;

    public Chained (hydra.tinkerpop.gremlin.ChainedTraversal value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Chained)) {
        return false;
      }
      Chained o = (Chained) other;
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
    public int compareTo(NestedTraversal other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Chained o = (Chained) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Anonymous extends hydra.tinkerpop.gremlin.NestedTraversal implements Serializable {
    public final hydra.tinkerpop.gremlin.ChainedTraversal value;

    public Anonymous (hydra.tinkerpop.gremlin.ChainedTraversal value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Anonymous)) {
        return false;
      }
      Anonymous o = (Anonymous) other;
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
    public int compareTo(NestedTraversal other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Anonymous o = (Anonymous) other;
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
