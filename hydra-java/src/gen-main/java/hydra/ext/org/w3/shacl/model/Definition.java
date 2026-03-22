// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.shacl.model;

import java.io.Serializable;

/**
 * An instance of a type like sh:Shape or sh:NodeShape, together with a unique IRI for that instance
 */
public class Definition<A> implements Serializable, Comparable<Definition<A>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.shacl.model.Definition");

  public static final hydra.core.Name IRI = new hydra.core.Name("iri");

  public static final hydra.core.Name TARGET = new hydra.core.Name("target");

  public final hydra.ext.org.w3.rdf.syntax.Iri iri;

  public final A target;

  public Definition (hydra.ext.org.w3.rdf.syntax.Iri iri, A target) {
    this.iri = iri;
    this.target = target;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Definition)) {
      return false;
    }
    Definition o = (Definition) other;
    return java.util.Objects.equals(
      this.iri,
      o.iri) && java.util.Objects.equals(
      this.target,
      o.target);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(iri) + 3 * java.util.Objects.hashCode(target);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Definition other) {
    int cmp = 0;
    cmp = ((Comparable) iri).compareTo(other.iri);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) target).compareTo(other.target);
  }

  public Definition withIri(hydra.ext.org.w3.rdf.syntax.Iri iri) {
    return new Definition(iri, target);
  }

  public Definition withTarget(A target) {
    return new Definition(iri, target);
  }
}
