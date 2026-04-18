// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ElementBindingsOrElements implements Serializable, Comparable<ElementBindingsOrElements> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ElementBindingsOrElements");

  public static final hydra.core.Name ELEMENT_BINDINGS = new hydra.core.Name("elementBindings");

  public static final hydra.core.Name ELEMENTS = new hydra.core.Name("elements");

  private ElementBindingsOrElements () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ElementBindings instance) ;

    R visit(Elements_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ElementBindingsOrElements instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ElementBindings instance) {
      return otherwise(instance);
    }

    default R visit(Elements_ instance) {
      return otherwise(instance);
    }
  }

  public static final class ElementBindings extends openGql.grammar.ElementBindingsOrElements implements Serializable {
    public final Boolean value;

    public ElementBindings (Boolean value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ElementBindings)) {
        return false;
      }
      ElementBindings o = (ElementBindings) other;
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
    public int compareTo(ElementBindingsOrElements other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ElementBindings o = (ElementBindings) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Elements_ extends openGql.grammar.ElementBindingsOrElements implements Serializable {
    public Elements_ () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Elements_)) {
        return false;
      }
      Elements_ o = (Elements_) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ElementBindingsOrElements other) {
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
