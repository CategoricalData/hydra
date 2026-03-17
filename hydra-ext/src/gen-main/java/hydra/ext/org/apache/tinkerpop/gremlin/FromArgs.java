// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class FromArgs implements Serializable, Comparable<FromArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.FromArgs");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  public static final hydra.core.Name VERTEX = new hydra.core.Name("vertex");

  public static final hydra.core.Name TRAVERSAL = new hydra.core.Name("traversal");

  private FromArgs () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(String_ instance) ;

    R visit(Vertex instance) ;

    R visit(Traversal instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(FromArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(String_ instance) {
      return otherwise(instance);
    }

    default R visit(Vertex instance) {
      return otherwise(instance);
    }

    default R visit(Traversal instance) {
      return otherwise(instance);
    }
  }

  public static final class String_ extends hydra.ext.org.apache.tinkerpop.gremlin.FromArgs implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value;

    public String_ (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) other;
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
    public int compareTo(FromArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      String_ o = (String_) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Vertex extends hydra.ext.org.apache.tinkerpop.gremlin.FromArgs implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StructureVertexArgument value;

    public Vertex (hydra.ext.org.apache.tinkerpop.gremlin.StructureVertexArgument value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Vertex)) {
        return false;
      }
      Vertex o = (Vertex) other;
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
    public int compareTo(FromArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Vertex o = (Vertex) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Traversal extends hydra.ext.org.apache.tinkerpop.gremlin.FromArgs implements Serializable {
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
    public int compareTo(FromArgs other) {
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
