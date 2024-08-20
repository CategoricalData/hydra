// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

/**
 * A let-in definition
 */
public class Let implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.Let");
  
  public static final hydra.core.Name FIELD_NAME_BINDINGS = new hydra.core.Name("bindings");
  
  public static final hydra.core.Name FIELD_NAME_IN = new hydra.core.Name("in");
  
  public final hydra.ext.fr.inria.coq.syntax.LetBindings bindings;
  
  public final hydra.ext.fr.inria.coq.syntax.Term in;
  
  public Let (hydra.ext.fr.inria.coq.syntax.LetBindings bindings, hydra.ext.fr.inria.coq.syntax.Term in) {
    java.util.Objects.requireNonNull((bindings));
    java.util.Objects.requireNonNull((in));
    this.bindings = bindings;
    this.in = in;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Let)) {
      return false;
    }
    Let o = (Let) (other);
    return bindings.equals(o.bindings) && in.equals(o.in);
  }
  
  @Override
  public int hashCode() {
    return 2 * bindings.hashCode() + 3 * in.hashCode();
  }
  
  public Let withBindings(hydra.ext.fr.inria.coq.syntax.LetBindings bindings) {
    java.util.Objects.requireNonNull((bindings));
    return new Let(bindings, in);
  }
  
  public Let withIn(hydra.ext.fr.inria.coq.syntax.Term in) {
    java.util.Objects.requireNonNull((in));
    return new Let(bindings, in);
  }
}