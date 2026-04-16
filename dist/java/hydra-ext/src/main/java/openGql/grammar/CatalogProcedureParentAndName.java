// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class CatalogProcedureParentAndName implements Serializable, Comparable<CatalogProcedureParentAndName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CatalogProcedureParentAndName");

  public static final hydra.core.Name PARENT_REFERENCE = new hydra.core.Name("parentReference");

  public static final hydra.core.Name PROCEDURE_NAME = new hydra.core.Name("procedureName");

  public final hydra.util.Maybe<openGql.grammar.CatalogObjectParentReference> parentReference;

  public final String procedureName;

  public CatalogProcedureParentAndName (hydra.util.Maybe<openGql.grammar.CatalogObjectParentReference> parentReference, String procedureName) {
    this.parentReference = parentReference;
    this.procedureName = procedureName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CatalogProcedureParentAndName)) {
      return false;
    }
    CatalogProcedureParentAndName o = (CatalogProcedureParentAndName) other;
    return java.util.Objects.equals(
      this.parentReference,
      o.parentReference) && java.util.Objects.equals(
      this.procedureName,
      o.procedureName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(parentReference) + 3 * java.util.Objects.hashCode(procedureName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CatalogProcedureParentAndName other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      parentReference,
      other.parentReference);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      procedureName,
      other.procedureName);
  }

  public CatalogProcedureParentAndName withParentReference(hydra.util.Maybe<openGql.grammar.CatalogObjectParentReference> parentReference) {
    return new CatalogProcedureParentAndName(parentReference, procedureName);
  }

  public CatalogProcedureParentAndName withProcedureName(String procedureName) {
    return new CatalogProcedureParentAndName(parentReference, procedureName);
  }
}
