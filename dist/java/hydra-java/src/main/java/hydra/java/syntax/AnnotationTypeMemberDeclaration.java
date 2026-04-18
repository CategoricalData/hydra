// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public abstract class AnnotationTypeMemberDeclaration implements Serializable, Comparable<AnnotationTypeMemberDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.AnnotationTypeMemberDeclaration");

  public static final hydra.core.Name ANNOTATION_TYPE = new hydra.core.Name("annotationType");

  public static final hydra.core.Name CONSTANT = new hydra.core.Name("constant");

  public static final hydra.core.Name CLASS = new hydra.core.Name("class");

  public static final hydra.core.Name INTERFACE = new hydra.core.Name("interface");

  private AnnotationTypeMemberDeclaration () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(AnnotationType instance) ;

    R visit(Constant instance) ;

    R visit(Class_ instance) ;

    R visit(Interface instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AnnotationTypeMemberDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(AnnotationType instance) {
      return otherwise(instance);
    }

    default R visit(Constant instance) {
      return otherwise(instance);
    }

    default R visit(Class_ instance) {
      return otherwise(instance);
    }

    default R visit(Interface instance) {
      return otherwise(instance);
    }
  }

  public static final class AnnotationType extends hydra.java.syntax.AnnotationTypeMemberDeclaration implements Serializable {
    public final hydra.java.syntax.AnnotationTypeElementDeclaration value;

    public AnnotationType (hydra.java.syntax.AnnotationTypeElementDeclaration value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnnotationType)) {
        return false;
      }
      AnnotationType o = (AnnotationType) other;
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
    public int compareTo(AnnotationTypeMemberDeclaration other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AnnotationType o = (AnnotationType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Constant extends hydra.java.syntax.AnnotationTypeMemberDeclaration implements Serializable {
    public final hydra.java.syntax.ConstantDeclaration value;

    public Constant (hydra.java.syntax.ConstantDeclaration value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Constant)) {
        return false;
      }
      Constant o = (Constant) other;
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
    public int compareTo(AnnotationTypeMemberDeclaration other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Constant o = (Constant) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Class_ extends hydra.java.syntax.AnnotationTypeMemberDeclaration implements Serializable {
    public final hydra.java.syntax.ClassDeclaration value;

    public Class_ (hydra.java.syntax.ClassDeclaration value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Class_)) {
        return false;
      }
      Class_ o = (Class_) other;
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
    public int compareTo(AnnotationTypeMemberDeclaration other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Class_ o = (Class_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Interface extends hydra.java.syntax.AnnotationTypeMemberDeclaration implements Serializable {
    public final hydra.java.syntax.InterfaceDeclaration value;

    public Interface (hydra.java.syntax.InterfaceDeclaration value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Interface)) {
        return false;
      }
      Interface o = (Interface) other;
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
    public int compareTo(AnnotationTypeMemberDeclaration other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Interface o = (Interface) other;
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
