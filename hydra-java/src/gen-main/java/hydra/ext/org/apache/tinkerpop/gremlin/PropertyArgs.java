// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class PropertyArgs implements Serializable, Comparable<PropertyArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.PropertyArgs");

  public static final hydra.core.Name CARDINALITY_OBJECTS = new hydra.core.Name("cardinalityObjects");

  public static final hydra.core.Name OBJECTS = new hydra.core.Name("objects");

  public static final hydra.core.Name OBJECT = new hydra.core.Name("object");

  public static final hydra.core.Name CARDINALITY_OBJECT = new hydra.core.Name("cardinalityObject");

  private PropertyArgs () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(CardinalityObjects instance) ;

    R visit(Objects instance) ;

    R visit(Object_ instance) ;

    R visit(CardinalityObject instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PropertyArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(CardinalityObjects instance) {
      return otherwise(instance);
    }

    default R visit(Objects instance) {
      return otherwise(instance);
    }

    default R visit(Object_ instance) {
      return otherwise(instance);
    }

    default R visit(CardinalityObject instance) {
      return otherwise(instance);
    }
  }

  public static final class CardinalityObjects extends hydra.ext.org.apache.tinkerpop.gremlin.PropertyArgs implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects value;

    public CardinalityObjects (hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CardinalityObjects)) {
        return false;
      }
      CardinalityObjects o = (CardinalityObjects) other;
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
    public int compareTo(PropertyArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CardinalityObjects o = (CardinalityObjects) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Objects extends hydra.ext.org.apache.tinkerpop.gremlin.PropertyArgs implements Serializable {
    public final hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value;

    public Objects (hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Objects)) {
        return false;
      }
      Objects o = (Objects) other;
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
    public int compareTo(PropertyArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Objects o = (Objects) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Object_ extends hydra.ext.org.apache.tinkerpop.gremlin.PropertyArgs implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgument value;

    public Object_ (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgument value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Object_)) {
        return false;
      }
      Object_ o = (Object_) other;
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
    public int compareTo(PropertyArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Object_ o = (Object_) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CardinalityObject extends hydra.ext.org.apache.tinkerpop.gremlin.PropertyArgs implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument value;

    public CardinalityObject (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CardinalityObject)) {
        return false;
      }
      CardinalityObject o = (CardinalityObject) other;
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
    public int compareTo(PropertyArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CardinalityObject o = (CardinalityObject) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
