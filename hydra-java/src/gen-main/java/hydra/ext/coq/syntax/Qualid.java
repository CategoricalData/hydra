package hydra.ext.coq.syntax;

/**
 * A qualified identifier
 */
public class Qualid {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Qualid");
  
  public final hydra.ext.coq.syntax.Ident id;
  
  public final java.util.List<hydra.ext.coq.syntax.FieldIdent> fieldIds;
  
  public Qualid (hydra.ext.coq.syntax.Ident id, java.util.List<hydra.ext.coq.syntax.FieldIdent> fieldIds) {
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
  
  public Qualid withId(hydra.ext.coq.syntax.Ident id) {
    return new Qualid(id, fieldIds);
  }
  
  public Qualid withFieldIds(java.util.List<hydra.ext.coq.syntax.FieldIdent> fieldIds) {
    return new Qualid(id, fieldIds);
  }
}