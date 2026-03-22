// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class OptionArgs implements Serializable, Comparable<OptionArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.OptionArgs");

  public static final hydra.core.Name PREDICATE_TRAVERSAL = new hydra.core.Name("predicateTraversal");

  public static final hydra.core.Name MERGE_MAP = new hydra.core.Name("mergeMap");

  public static final hydra.core.Name MERGE_TRAVERSAL = new hydra.core.Name("mergeTraversal");

  public static final hydra.core.Name OBJECT_TRAVERSAL = new hydra.core.Name("objectTraversal");

  public static final hydra.core.Name TRAVERSAL = new hydra.core.Name("traversal");

  private OptionArgs () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(PredicateTraversal instance) ;

    R visit(MergeMap instance) ;

    R visit(MergeTraversal instance) ;

    R visit(ObjectTraversal instance) ;

    R visit(Traversal instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OptionArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(PredicateTraversal instance) {
      return otherwise(instance);
    }

    default R visit(MergeMap instance) {
      return otherwise(instance);
    }

    default R visit(MergeTraversal instance) {
      return otherwise(instance);
    }

    default R visit(ObjectTraversal instance) {
      return otherwise(instance);
    }

    default R visit(Traversal instance) {
      return otherwise(instance);
    }
  }

  public static final class PredicateTraversal extends hydra.ext.org.apache.tinkerpop.gremlin.OptionArgs implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal value;

    public PredicateTraversal (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PredicateTraversal)) {
        return false;
      }
      PredicateTraversal o = (PredicateTraversal) other;
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
    public int compareTo(OptionArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PredicateTraversal o = (PredicateTraversal) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class MergeMap extends hydra.ext.org.apache.tinkerpop.gremlin.OptionArgs implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument value;

    public MergeMap (hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MergeMap)) {
        return false;
      }
      MergeMap o = (MergeMap) other;
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
    public int compareTo(OptionArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MergeMap o = (MergeMap) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class MergeTraversal extends hydra.ext.org.apache.tinkerpop.gremlin.OptionArgs implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal value;

    public MergeTraversal (hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MergeTraversal)) {
        return false;
      }
      MergeTraversal o = (MergeTraversal) other;
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
    public int compareTo(OptionArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MergeTraversal o = (MergeTraversal) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ObjectTraversal extends hydra.ext.org.apache.tinkerpop.gremlin.OptionArgs implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal value;

    public ObjectTraversal (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectTraversal)) {
        return false;
      }
      ObjectTraversal o = (ObjectTraversal) other;
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
    public int compareTo(OptionArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ObjectTraversal o = (ObjectTraversal) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Traversal extends hydra.ext.org.apache.tinkerpop.gremlin.OptionArgs implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value;

    public Traversal (hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Traversal)) {
        return false;
      }
      Traversal o = (Traversal) other;
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
    public int compareTo(OptionArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Traversal o = (Traversal) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
