// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shacl.model;

import java.io.Serializable;

/**
 * An instance of a type like sh:Shape or sh:NodeShape, together with a unique IRI for that instance
 */
public class Definition<A> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/shacl/model.Definition");
  
  public static final hydra.core.Name FIELD_NAME_IRI = new hydra.core.Name("iri");
  
  public static final hydra.core.Name FIELD_NAME_TARGET = new hydra.core.Name("target");
  
  public final hydra.langs.rdf.syntax.Iri iri;
  
  public final A target;
  
  public Definition (hydra.langs.rdf.syntax.Iri iri, A target) {
    java.util.Objects.requireNonNull((iri));
    java.util.Objects.requireNonNull((target));
    this.iri = iri;
    this.target = target;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Definition)) {
      return false;
    }
    Definition o = (Definition) (other);
    return iri.equals(o.iri) && target.equals(o.target);
  }
  
  @Override
  public int hashCode() {
    return 2 * iri.hashCode() + 3 * target.hashCode();
  }
  
  public Definition withIri(hydra.langs.rdf.syntax.Iri iri) {
    java.util.Objects.requireNonNull((iri));
    return new Definition(iri, target);
  }
  
  public Definition withTarget(A target) {
    java.util.Objects.requireNonNull((target));
    return new Definition(iri, target);
  }
}