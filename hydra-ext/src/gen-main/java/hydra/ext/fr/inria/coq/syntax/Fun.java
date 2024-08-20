// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class Fun implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.Fun");
  
  public static final hydra.core.Name FIELD_NAME_BINDERS = new hydra.core.Name("binders");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.fr.inria.coq.syntax.OpenBinders binders;
  
  public final hydra.ext.fr.inria.coq.syntax.Term body;
  
  public Fun (hydra.ext.fr.inria.coq.syntax.OpenBinders binders, hydra.ext.fr.inria.coq.syntax.Term body) {
    java.util.Objects.requireNonNull((binders));
    java.util.Objects.requireNonNull((body));
    this.binders = binders;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Fun)) {
      return false;
    }
    Fun o = (Fun) (other);
    return binders.equals(o.binders) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * binders.hashCode() + 3 * body.hashCode();
  }
  
  public Fun withBinders(hydra.ext.fr.inria.coq.syntax.OpenBinders binders) {
    java.util.Objects.requireNonNull((binders));
    return new Fun(binders, body);
  }
  
  public Fun withBody(hydra.ext.fr.inria.coq.syntax.Term body) {
    java.util.Objects.requireNonNull((body));
    return new Fun(binders, body);
  }
}