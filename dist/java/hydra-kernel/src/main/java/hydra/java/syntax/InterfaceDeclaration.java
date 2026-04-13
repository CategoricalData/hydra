// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public abstract class InterfaceDeclaration implements Serializable, Comparable<InterfaceDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.InterfaceDeclaration");

  public static final hydra.core.Name NORMAL_INTERFACE = new hydra.core.Name("normalInterface");

  public static final hydra.core.Name ANNOTATION_TYPE = new hydra.core.Name("annotationType");

  private InterfaceDeclaration () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(NormalInterface instance) ;

    R visit(AnnotationType instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InterfaceDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(NormalInterface instance) {
      return otherwise(instance);
    }

    default R visit(AnnotationType instance) {
      return otherwise(instance);
    }
  }

  public static final class NormalInterface extends hydra.java.syntax.InterfaceDeclaration implements Serializable {
    public final hydra.java.syntax.NormalInterfaceDeclaration value;

    public NormalInterface (hydra.java.syntax.NormalInterfaceDeclaration value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NormalInterface)) {
        return false;
      }
      NormalInterface o = (NormalInterface) other;
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
    public int compareTo(InterfaceDeclaration other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NormalInterface o = (NormalInterface) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class AnnotationType extends hydra.java.syntax.InterfaceDeclaration implements Serializable {
    public final hydra.java.syntax.AnnotationTypeDeclaration value;

    public AnnotationType (hydra.java.syntax.AnnotationTypeDeclaration value) {
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
    public int compareTo(InterfaceDeclaration other) {
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
}
