// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class GenericLiteralMapNullableArgumentOrNestedTraversal implements Serializable, Comparable<GenericLiteralMapNullableArgumentOrNestedTraversal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal");

  public static final hydra.core.Name MAP = new hydra.core.Name("map");

  public static final hydra.core.Name TRAVERSAL = new hydra.core.Name("traversal");

  private GenericLiteralMapNullableArgumentOrNestedTraversal () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Map instance) ;

    R visit(Traversal instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GenericLiteralMapNullableArgumentOrNestedTraversal instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Map instance) {
      return otherwise(instance);
    }

    default R visit(Traversal instance) {
      return otherwise(instance);
    }
  }

  public static final class Map extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgument value;

    public Map (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgument value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) other;
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
    public int compareTo(GenericLiteralMapNullableArgumentOrNestedTraversal other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Map o = (Map) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Traversal extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal implements Serializable {
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
    public int compareTo(GenericLiteralMapNullableArgumentOrNestedTraversal other) {
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
