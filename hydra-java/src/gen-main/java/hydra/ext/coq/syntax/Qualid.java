package hydra.ext.coq.syntax;

/**
 * A qualified identifier
 */
public class Qualid {
  public final Ident id;
  
  public final java.util.List<FieldIdent> fieldIds;
  
  public Qualid (Ident id, java.util.List<FieldIdent> fieldIds) {
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
  
  public Qualid withId(Ident id) {
    return new Qualid(id, fieldIds);
  }
  
  public Qualid withFieldIds(java.util.List<FieldIdent> fieldIds) {
    return new Qualid(id, fieldIds);
  }
}