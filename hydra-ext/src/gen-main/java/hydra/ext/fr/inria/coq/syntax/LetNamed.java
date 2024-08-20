// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class LetNamed implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.LetNamed");
  
  public static final hydra.core.Name FIELD_NAME_BINDER = new hydra.core.Name("binder");
  
  public static final hydra.core.Name FIELD_NAME_BINDERS = new hydra.core.Name("binders");
  
  public final hydra.ext.fr.inria.coq.syntax.LetBinder binder;
  
  public final java.util.List<hydra.ext.fr.inria.coq.syntax.Binder> binders;
  
  public LetNamed (hydra.ext.fr.inria.coq.syntax.LetBinder binder, java.util.List<hydra.ext.fr.inria.coq.syntax.Binder> binders) {
    java.util.Objects.requireNonNull((binder));
    java.util.Objects.requireNonNull((binders));
    this.binder = binder;
    this.binders = binders;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetNamed)) {
      return false;
    }
    LetNamed o = (LetNamed) (other);
    return binder.equals(o.binder) && binders.equals(o.binders);
  }
  
  @Override
  public int hashCode() {
    return 2 * binder.hashCode() + 3 * binders.hashCode();
  }
  
  public LetNamed withBinder(hydra.ext.fr.inria.coq.syntax.LetBinder binder) {
    java.util.Objects.requireNonNull((binder));
    return new LetNamed(binder, binders);
  }
  
  public LetNamed withBinders(java.util.List<hydra.ext.fr.inria.coq.syntax.Binder> binders) {
    java.util.Objects.requireNonNull((binders));
    return new LetNamed(binder, binders);
  }
}