// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

/**
 * A qualified identifier
 */
public class Qualid implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.Qualid");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_FIELD_IDS = new hydra.core.Name("fieldIds");
  
  public final hydra.ext.fr.inria.coq.syntax.Ident id;
  
  public final java.util.List<hydra.ext.fr.inria.coq.syntax.FieldIdent> fieldIds;
  
  public Qualid (hydra.ext.fr.inria.coq.syntax.Ident id, java.util.List<hydra.ext.fr.inria.coq.syntax.FieldIdent> fieldIds) {
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((fieldIds));
    this.id = id;
    this.fieldIds = fieldIds;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Qualid)) {
      return false;
    }
    Qualid o = (Qualid) (other);
    return id.equals(o.id) && fieldIds.equals(o.fieldIds);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * fieldIds.hashCode();
  }
  
  public Qualid withId(hydra.ext.fr.inria.coq.syntax.Ident id) {
    java.util.Objects.requireNonNull((id));
    return new Qualid(id, fieldIds);
  }
  
  public Qualid withFieldIds(java.util.List<hydra.ext.fr.inria.coq.syntax.FieldIdent> fieldIds) {
    java.util.Objects.requireNonNull((fieldIds));
    return new Qualid(id, fieldIds);
  }
}