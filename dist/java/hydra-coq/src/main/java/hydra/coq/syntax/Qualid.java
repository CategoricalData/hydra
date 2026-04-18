// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * A qualified identifier
 */
public class Qualid implements Serializable, Comparable<Qualid> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Qualid");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name FIELD_IDS = new hydra.core.Name("fieldIds");

  public final hydra.coq.syntax.Ident id;

  public final java.util.List<hydra.coq.syntax.FieldIdent> fieldIds;

  public Qualid (hydra.coq.syntax.Ident id, java.util.List<hydra.coq.syntax.FieldIdent> fieldIds) {
    this.id = id;
    this.fieldIds = fieldIds;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Qualid)) {
      return false;
    }
    Qualid o = (Qualid) other;
    return java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.fieldIds,
      o.fieldIds);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(id) + 3 * java.util.Objects.hashCode(fieldIds);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Qualid other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      id,
      other.id);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      fieldIds,
      other.fieldIds);
  }

  public Qualid withId(hydra.coq.syntax.Ident id) {
    return new Qualid(id, fieldIds);
  }

  public Qualid withFieldIds(java.util.List<hydra.coq.syntax.FieldIdent> fieldIds) {
    return new Qualid(id, fieldIds);
  }
}
