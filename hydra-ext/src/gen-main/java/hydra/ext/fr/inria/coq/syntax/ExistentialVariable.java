// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class ExistentialVariable implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.ExistentialVariable");
  
  public static final hydra.core.Name FIELD_NAME_IDENT = new hydra.core.Name("ident");
  
  public static final hydra.core.Name FIELD_NAME_VARIANT = new hydra.core.Name("variant");
  
  public final hydra.ext.fr.inria.coq.syntax.Ident ident;
  
  public final hydra.ext.fr.inria.coq.syntax.ExistentialVariableVariant variant;
  
  public ExistentialVariable (hydra.ext.fr.inria.coq.syntax.Ident ident, hydra.ext.fr.inria.coq.syntax.ExistentialVariableVariant variant) {
    java.util.Objects.requireNonNull((ident));
    java.util.Objects.requireNonNull((variant));
    this.ident = ident;
    this.variant = variant;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExistentialVariable)) {
      return false;
    }
    ExistentialVariable o = (ExistentialVariable) (other);
    return ident.equals(o.ident) && variant.equals(o.variant);
  }
  
  @Override
  public int hashCode() {
    return 2 * ident.hashCode() + 3 * variant.hashCode();
  }
  
  public ExistentialVariable withIdent(hydra.ext.fr.inria.coq.syntax.Ident ident) {
    java.util.Objects.requireNonNull((ident));
    return new ExistentialVariable(ident, variant);
  }
  
  public ExistentialVariable withVariant(hydra.ext.fr.inria.coq.syntax.ExistentialVariableVariant variant) {
    java.util.Objects.requireNonNull((variant));
    return new ExistentialVariable(ident, variant);
  }
}