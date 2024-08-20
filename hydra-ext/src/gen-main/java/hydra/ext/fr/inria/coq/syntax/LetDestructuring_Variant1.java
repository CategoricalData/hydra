// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class LetDestructuring_Variant1 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.LetDestructuring.Variant1");
  
  public static final hydra.core.Name FIELD_NAME_NAMES = new hydra.core.Name("names");
  
  public static final hydra.core.Name FIELD_NAME_RETURN_AS = new hydra.core.Name("returnAs");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public final java.util.List<hydra.ext.fr.inria.coq.syntax.Name> names;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.ReturnAs> returnAs;
  
  public final hydra.ext.fr.inria.coq.syntax.Term term;
  
  public LetDestructuring_Variant1 (java.util.List<hydra.ext.fr.inria.coq.syntax.Name> names, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.ReturnAs> returnAs, hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((names));
    java.util.Objects.requireNonNull((returnAs));
    java.util.Objects.requireNonNull((term));
    this.names = names;
    this.returnAs = returnAs;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetDestructuring_Variant1)) {
      return false;
    }
    LetDestructuring_Variant1 o = (LetDestructuring_Variant1) (other);
    return names.equals(o.names) && returnAs.equals(o.returnAs) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * names.hashCode() + 3 * returnAs.hashCode() + 5 * term.hashCode();
  }
  
  public LetDestructuring_Variant1 withNames(java.util.List<hydra.ext.fr.inria.coq.syntax.Name> names) {
    java.util.Objects.requireNonNull((names));
    return new LetDestructuring_Variant1(names, returnAs, term);
  }
  
  public LetDestructuring_Variant1 withReturnAs(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.ReturnAs> returnAs) {
    java.util.Objects.requireNonNull((returnAs));
    return new LetDestructuring_Variant1(names, returnAs, term);
  }
  
  public LetDestructuring_Variant1 withTerm(hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((term));
    return new LetDestructuring_Variant1(names, returnAs, term);
  }
}