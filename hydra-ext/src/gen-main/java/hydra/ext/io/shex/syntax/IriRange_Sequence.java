// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class IriRange_Sequence implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.IriRange_Sequence");
  
  public static final hydra.core.Name FIELD_NAME_IRI = new hydra.core.Name("iri");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE = new hydra.core.Name("sequence");
  
  public final hydra.ext.io.shex.syntax.Iri iri;
  
  public final hydra.util.Opt<java.util.List<hydra.ext.io.shex.syntax.Exclusion>> sequence;
  
  public IriRange_Sequence (hydra.ext.io.shex.syntax.Iri iri, hydra.util.Opt<java.util.List<hydra.ext.io.shex.syntax.Exclusion>> sequence) {
    java.util.Objects.requireNonNull((iri));
    java.util.Objects.requireNonNull((sequence));
    this.iri = iri;
    this.sequence = sequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IriRange_Sequence)) {
      return false;
    }
    IriRange_Sequence o = (IriRange_Sequence) (other);
    return iri.equals(o.iri) && sequence.equals(o.sequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * iri.hashCode() + 3 * sequence.hashCode();
  }
  
  public IriRange_Sequence withIri(hydra.ext.io.shex.syntax.Iri iri) {
    java.util.Objects.requireNonNull((iri));
    return new IriRange_Sequence(iri, sequence);
  }
  
  public IriRange_Sequence withSequence(hydra.util.Opt<java.util.List<hydra.ext.io.shex.syntax.Exclusion>> sequence) {
    java.util.Objects.requireNonNull((sequence));
    return new IriRange_Sequence(iri, sequence);
  }
}