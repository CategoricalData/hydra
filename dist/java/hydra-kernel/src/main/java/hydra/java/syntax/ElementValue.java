// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public abstract class ElementValue implements Serializable, Comparable<ElementValue> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.ElementValue");

  public static final hydra.core.Name CONDITIONAL_EXPRESSION = new hydra.core.Name("conditionalExpression");

  public static final hydra.core.Name ELEMENT_VALUE_ARRAY_INITIALIZER = new hydra.core.Name("elementValueArrayInitializer");

  public static final hydra.core.Name ANNOTATION = new hydra.core.Name("annotation");

  private ElementValue () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ConditionalExpression instance) ;

    R visit(ElementValueArrayInitializer instance) ;

    R visit(Annotation instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ElementValue instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ConditionalExpression instance) {
      return otherwise(instance);
    }

    default R visit(ElementValueArrayInitializer instance) {
      return otherwise(instance);
    }

    default R visit(Annotation instance) {
      return otherwise(instance);
    }
  }

  public static final class ConditionalExpression extends hydra.java.syntax.ElementValue implements Serializable {
    public final hydra.java.syntax.ConditionalExpression value;

    public ConditionalExpression (hydra.java.syntax.ConditionalExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ConditionalExpression)) {
        return false;
      }
      ConditionalExpression o = (ConditionalExpression) other;
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
    public int compareTo(ElementValue other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ConditionalExpression o = (ConditionalExpression) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ElementValueArrayInitializer extends hydra.java.syntax.ElementValue implements Serializable {
    public final hydra.java.syntax.ElementValueArrayInitializer value;

    public ElementValueArrayInitializer (hydra.java.syntax.ElementValueArrayInitializer value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ElementValueArrayInitializer)) {
        return false;
      }
      ElementValueArrayInitializer o = (ElementValueArrayInitializer) other;
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
    public int compareTo(ElementValue other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ElementValueArrayInitializer o = (ElementValueArrayInitializer) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Annotation extends hydra.java.syntax.ElementValue implements Serializable {
    public final hydra.java.syntax.Annotation value;

    public Annotation (hydra.java.syntax.Annotation value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Annotation)) {
        return false;
      }
      Annotation o = (Annotation) other;
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
    public int compareTo(ElementValue other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Annotation o = (Annotation) other;
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
