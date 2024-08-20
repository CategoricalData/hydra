// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class NaturalArg implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.NaturalArg");
  
  public static final hydra.core.Name FIELD_NAME_NATURAL = new hydra.core.Name("natural");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public final hydra.ext.fr.inria.coq.syntax.Natural natural;
  
  public final hydra.ext.fr.inria.coq.syntax.Term term;
  
  public NaturalArg (hydra.ext.fr.inria.coq.syntax.Natural natural, hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((natural));
    java.util.Objects.requireNonNull((term));
    this.natural = natural;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NaturalArg)) {
      return false;
    }
    NaturalArg o = (NaturalArg) (other);
    return natural.equals(o.natural) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * natural.hashCode() + 3 * term.hashCode();
  }
  
  public NaturalArg withNatural(hydra.ext.fr.inria.coq.syntax.Natural natural) {
    java.util.Objects.requireNonNull((natural));
    return new NaturalArg(natural, term);
  }
  
  public NaturalArg withTerm(hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((term));
    return new NaturalArg(natural, term);
  }
}