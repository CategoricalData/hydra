// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class SelectArgs implements Serializable, Comparable<SelectArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.SelectArgs");

  public static final hydra.core.Name COLUMN = new hydra.core.Name("column");

  public static final hydra.core.Name POP_STRINGS = new hydra.core.Name("popStrings");

  public static final hydra.core.Name POP_TRAVERSAL = new hydra.core.Name("popTraversal");

  public static final hydra.core.Name STRINGS = new hydra.core.Name("strings");

  public static final hydra.core.Name TRAVERSAL = new hydra.core.Name("traversal");

  private SelectArgs () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Column instance) ;

    R visit(PopStrings instance) ;

    R visit(PopTraversal instance) ;

    R visit(Strings instance) ;

    R visit(Traversal instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SelectArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Column instance) {
      return otherwise(instance);
    }

    default R visit(PopStrings instance) {
      return otherwise(instance);
    }

    default R visit(PopTraversal instance) {
      return otherwise(instance);
    }

    default R visit(Strings instance) {
      return otherwise(instance);
    }

    default R visit(Traversal instance) {
      return otherwise(instance);
    }
  }

  public static final class Column extends hydra.tinkerpop.gremlin.SelectArgs implements Serializable {
    public final hydra.tinkerpop.gremlin.TraversalColumnArgument value;

    public Column (hydra.tinkerpop.gremlin.TraversalColumnArgument value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Column)) {
        return false;
      }
      Column o = (Column) other;
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
    public int compareTo(SelectArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Column o = (Column) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PopStrings extends hydra.tinkerpop.gremlin.SelectArgs implements Serializable {
    public final hydra.tinkerpop.gremlin.PopStringsArgument value;

    public PopStrings (hydra.tinkerpop.gremlin.PopStringsArgument value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PopStrings)) {
        return false;
      }
      PopStrings o = (PopStrings) other;
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
    public int compareTo(SelectArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PopStrings o = (PopStrings) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PopTraversal extends hydra.tinkerpop.gremlin.SelectArgs implements Serializable {
    public final hydra.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal value;

    public PopTraversal (hydra.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PopTraversal)) {
        return false;
      }
      PopTraversal o = (PopTraversal) other;
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
    public int compareTo(SelectArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PopTraversal o = (PopTraversal) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Strings extends hydra.tinkerpop.gremlin.SelectArgs implements Serializable {
    public final java.util.List<hydra.tinkerpop.gremlin.StringArgument> value;

    public Strings (java.util.List<hydra.tinkerpop.gremlin.StringArgument> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Strings)) {
        return false;
      }
      Strings o = (Strings) other;
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
    public int compareTo(SelectArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Strings o = (Strings) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Traversal extends hydra.tinkerpop.gremlin.SelectArgs implements Serializable {
    public final hydra.tinkerpop.gremlin.NestedTraversal value;

    public Traversal (hydra.tinkerpop.gremlin.NestedTraversal value) {
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
    public int compareTo(SelectArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Traversal o = (Traversal) other;
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
