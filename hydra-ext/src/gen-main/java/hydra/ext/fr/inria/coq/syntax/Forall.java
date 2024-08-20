// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class Forall implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.Forall");
  
  public static final hydra.core.Name FIELD_NAME_BINDERS = new hydra.core.Name("binders");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public final hydra.ext.fr.inria.coq.syntax.OpenBinders binders;
  
  public final hydra.ext.fr.inria.coq.syntax.Type type;
  
  public Forall (hydra.ext.fr.inria.coq.syntax.OpenBinders binders, hydra.ext.fr.inria.coq.syntax.Type type) {
    java.util.Objects.requireNonNull((binders));
    java.util.Objects.requireNonNull((type));
    this.binders = binders;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Forall)) {
      return false;
    }
    Forall o = (Forall) (other);
    return binders.equals(o.binders) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * binders.hashCode() + 3 * type.hashCode();
  }
  
  public Forall withBinders(hydra.ext.fr.inria.coq.syntax.OpenBinders binders) {
    java.util.Objects.requireNonNull((binders));
    return new Forall(binders, type);
  }
  
  public Forall withType(hydra.ext.fr.inria.coq.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new Forall(binders, type);
  }
}