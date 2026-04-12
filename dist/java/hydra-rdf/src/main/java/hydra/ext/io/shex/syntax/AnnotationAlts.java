// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class Annotation_Alts implements Serializable, Comparable<Annotation_Alts> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.Annotation_Alts");

  public static final hydra.core.Name IRI = new hydra.core.Name("Iri");

  public static final hydra.core.Name LITERAL = new hydra.core.Name("Literal");

  private Annotation_Alts () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Iri instance) ;

    R visit(Literal instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Annotation_Alts instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Iri instance) {
      return otherwise(instance);
    }

    default R visit(Literal instance) {
      return otherwise(instance);
    }
  }

  public static final class Iri extends hydra.ext.io.shex.syntax.Annotation_Alts implements Serializable {
    public final hydra.ext.io.shex.syntax.Iri value;

    public Iri (hydra.ext.io.shex.syntax.Iri value) {
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
    public int compareTo(Annotation_Alts other) {
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

  public static final class Literal extends hydra.ext.io.shex.syntax.Annotation_Alts implements Serializable {
    public final hydra.ext.io.shex.syntax.Literal value;

    public Literal (hydra.ext.io.shex.syntax.Literal value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) other;
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
    public int compareTo(Annotation_Alts other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Literal o = (Literal) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
