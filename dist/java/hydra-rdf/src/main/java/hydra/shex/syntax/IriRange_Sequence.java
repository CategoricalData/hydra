// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public class IriRange_Sequence implements Serializable, Comparable<IriRange_Sequence> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.IriRange_Sequence");

  public static final hydra.core.Name IRI = new hydra.core.Name("Iri");

  public static final hydra.core.Name SEQUENCE = new hydra.core.Name("Sequence");

  public final hydra.shex.syntax.Iri Iri;

  public final hydra.util.Maybe<java.util.List<hydra.shex.syntax.Exclusion>> Sequence;

  public IriRange_Sequence (hydra.shex.syntax.Iri Iri, hydra.util.Maybe<java.util.List<hydra.shex.syntax.Exclusion>> Sequence) {
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
    cmp = hydra.util.Comparing.compare(
      Iri,
      other.Iri);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      Sequence,
      other.Sequence);
  }

  public IriRange_Sequence withIri(hydra.shex.syntax.Iri Iri) {
    return new IriRange_Sequence(Iri, Sequence);
  }

  public IriRange_Sequence withSequence(hydra.util.Maybe<java.util.List<hydra.shex.syntax.Exclusion>> Sequence) {
    return new IriRange_Sequence(Iri, Sequence);
  }
}
