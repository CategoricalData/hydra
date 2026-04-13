// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public abstract class AnnotationTypeElementModifier implements Serializable, Comparable<AnnotationTypeElementModifier> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.AnnotationTypeElementModifier");

  public static final hydra.core.Name PUBLIC = new hydra.core.Name("public");

  public static final hydra.core.Name ABSTRACT = new hydra.core.Name("abstract");

  private AnnotationTypeElementModifier () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Public instance) ;

    R visit(Abstract instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AnnotationTypeElementModifier instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Public instance) {
      return otherwise(instance);
    }

    default R visit(Abstract instance) {
      return otherwise(instance);
    }
  }

  public static final class Public extends hydra.java.syntax.AnnotationTypeElementModifier implements Serializable {
    public final hydra.java.syntax.Annotation value;

    public Public (hydra.java.syntax.Annotation value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Public)) {
        return false;
      }
      Public o = (Public) other;
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
    public int compareTo(AnnotationTypeElementModifier other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Public o = (Public) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Abstract extends hydra.java.syntax.AnnotationTypeElementModifier implements Serializable {
    public Abstract () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Abstract)) {
        return false;
      }
      Abstract o = (Abstract) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AnnotationTypeElementModifier other) {
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
