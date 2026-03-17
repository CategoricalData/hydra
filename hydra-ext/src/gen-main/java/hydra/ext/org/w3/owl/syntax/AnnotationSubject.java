// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public abstract class AnnotationSubject implements Serializable, Comparable<AnnotationSubject> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.AnnotationSubject");

  public static final hydra.core.Name IRI = new hydra.core.Name("iri");

  public static final hydra.core.Name ANONYMOUS_INDIVIDUAL = new hydra.core.Name("anonymousIndividual");

  private AnnotationSubject () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Iri instance) ;

    R visit(AnonymousIndividual instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AnnotationSubject instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Iri instance) {
      return otherwise(instance);
    }

    default R visit(AnonymousIndividual instance) {
      return otherwise(instance);
    }
  }

  public static final class Iri extends hydra.ext.org.w3.owl.syntax.AnnotationSubject implements Serializable {
    public final hydra.ext.org.w3.rdf.syntax.Iri value;

    public Iri (hydra.ext.org.w3.rdf.syntax.Iri value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Iri)) {
        return false;
      }
      Iri o = (Iri) other;
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
    public int compareTo(AnnotationSubject other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Iri o = (Iri) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class AnonymousIndividual extends hydra.ext.org.w3.owl.syntax.AnnotationSubject implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.AnonymousIndividual value;

    public AnonymousIndividual (hydra.ext.org.w3.owl.syntax.AnonymousIndividual value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnonymousIndividual)) {
        return false;
      }
      AnonymousIndividual o = (AnonymousIndividual) other;
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
    public int compareTo(AnnotationSubject other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AnonymousIndividual o = (AnonymousIndividual) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
