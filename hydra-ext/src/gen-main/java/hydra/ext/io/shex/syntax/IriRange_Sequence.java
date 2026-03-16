// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class IriRange_Sequence implements Serializable, Comparable<IriRange_Sequence> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.IriRange_Sequence");
  
  public static final hydra.core.Name IRI = new hydra.core.Name("Iri");
  
  public static final hydra.core.Name SEQUENCE = new hydra.core.Name("Sequence");
  
  public final hydra.ext.io.shex.syntax.Iri Iri;
  
  public final hydra.util.Maybe<hydra.util.ConsList<hydra.ext.io.shex.syntax.Exclusion>> Sequence;
  
  public IriRange_Sequence (hydra.ext.io.shex.syntax.Iri Iri, hydra.util.Maybe<hydra.util.ConsList<hydra.ext.io.shex.syntax.Exclusion>> Sequence) {
    this.Iri = Iri;
    this.Sequence = Sequence;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IriRange_Sequence)) {
      return false;
    }
    IriRange_Sequence o = (IriRange_Sequence) other;
    return java.util.Objects.equals(
      this.Iri,
      o.Iri) && java.util.Objects.equals(
      this.Sequence,
      o.Sequence);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(Iri) + 3 * java.util.Objects.hashCode(Sequence);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(IriRange_Sequence other) {
    int cmp = 0;
    cmp = ((Comparable) Iri).compareTo(other.Iri);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) Sequence).compareTo(other.Sequence);
  }
  
  public IriRange_Sequence withIri(hydra.ext.io.shex.syntax.Iri Iri) {
    return new IriRange_Sequence(Iri, Sequence);
  }
  
  public IriRange_Sequence withSequence(hydra.util.Maybe<hydra.util.ConsList<hydra.ext.io.shex.syntax.Exclusion>> Sequence) {
    return new IriRange_Sequence(Iri, Sequence);
  }
}
