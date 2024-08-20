// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

/**
 * Some constructions allow the binding of a variable to value. This is called a “let-binder”.
 */
public class LetBinder implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.LetBinder");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public final hydra.ext.fr.inria.coq.syntax.Name name;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Type> type;
  
  public final hydra.ext.fr.inria.coq.syntax.Term term;
  
  public LetBinder (hydra.ext.fr.inria.coq.syntax.Name name, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Type> type, hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((term));
    this.name = name;
    this.type = type;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetBinder)) {
      return false;
    }
    LetBinder o = (LetBinder) (other);
    return name.equals(o.name) && type.equals(o.type) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * type.hashCode() + 5 * term.hashCode();
  }
  
  public LetBinder withName(hydra.ext.fr.inria.coq.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new LetBinder(name, type, term);
  }
  
  public LetBinder withType(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Type> type) {
    java.util.Objects.requireNonNull((type));
    return new LetBinder(name, type, term);
  }
  
  public LetBinder withTerm(hydra.ext.fr.inria.coq.syntax.Term term) {
    java.util.Objects.requireNonNull((term));
    return new LetBinder(name, type, term);
  }
}