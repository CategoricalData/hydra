// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A property in an object pattern
 */
public abstract class ObjectPatternProperty implements Serializable, Comparable<ObjectPatternProperty> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ObjectPatternProperty");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name REST = new hydra.core.Name("rest");

  private ObjectPatternProperty () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Property instance) ;

    R visit(Rest instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ObjectPatternProperty instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Property instance) {
      return otherwise(instance);
    }

    default R visit(Rest instance) {
      return otherwise(instance);
    }
  }

  public static final class Property extends hydra.javaScript.syntax.ObjectPatternProperty implements Serializable {
    public final hydra.javaScript.syntax.Property value;

    public Property (hydra.javaScript.syntax.Property value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) other;
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
    public int compareTo(ObjectPatternProperty other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Property o = (Property) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Rest extends hydra.javaScript.syntax.ObjectPatternProperty implements Serializable {
    public final hydra.javaScript.syntax.RestElement value;

    public Rest (hydra.javaScript.syntax.RestElement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Rest)) {
        return false;
      }
      Rest o = (Rest) other;
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
    public int compareTo(ObjectPatternProperty other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Rest o = (Rest) other;
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
