// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public class IriRange_Sequence implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.IriRange.Sequence");
  
  public final hydra.langs.shex.syntax.Iri iri;
  
  public final hydra.util.Opt<java.util.List<hydra.langs.shex.syntax.Exclusion>> sequence;
  
  public IriRange_Sequence (hydra.langs.shex.syntax.Iri iri, hydra.util.Opt<java.util.List<hydra.langs.shex.syntax.Exclusion>> sequence) {
    if (iri == null) {
      throw new IllegalArgumentException("null value for 'iri' argument");
    }
    if (sequence == null) {
      throw new IllegalArgumentException("null value for 'sequence' argument");
    }
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
  
  public IriRange_Sequence withIri(hydra.langs.shex.syntax.Iri iri) {
    if (iri == null) {
      throw new IllegalArgumentException("null value for 'iri' argument");
    }
    return new IriRange_Sequence(iri, sequence);
  }
  
  public IriRange_Sequence withSequence(hydra.util.Opt<java.util.List<hydra.langs.shex.syntax.Exclusion>> sequence) {
    if (sequence == null) {
      throw new IllegalArgumentException("null value for 'sequence' argument");
    }
    return new IriRange_Sequence(iri, sequence);
  }
}