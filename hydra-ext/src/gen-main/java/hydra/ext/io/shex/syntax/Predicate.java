// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class Predicate implements Serializable, Comparable<Predicate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.Predicate");

  public static final hydra.core.Name IRI = new hydra.core.Name("Iri");

  public static final hydra.core.Name RDF_TYPE = new hydra.core.Name("RdfType");

  private Predicate () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Iri instance) ;

    R visit(RdfType instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Predicate instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Iri instance) {
      return otherwise(instance);
    }

    default R visit(RdfType instance) {
      return otherwise(instance);
    }
  }

  public static final class Iri extends hydra.ext.io.shex.syntax.Predicate implements Serializable {
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
    public int compareTo(Predicate other) {
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

  public static final class RdfType extends hydra.ext.io.shex.syntax.Predicate implements Serializable {
    public final hydra.ext.io.shex.syntax.RdfType value;

    public RdfType (hydra.ext.io.shex.syntax.RdfType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RdfType)) {
        return false;
      }
      RdfType o = (RdfType) other;
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
    public int compareTo(Predicate other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      RdfType o = (RdfType) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
