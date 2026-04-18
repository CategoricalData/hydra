// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ParentAndTableName implements Serializable, Comparable<ParentAndTableName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ParentAndTableName");

  public static final hydra.core.Name PARENT_REFERENCE = new hydra.core.Name("parentReference");

  public static final hydra.core.Name TABLE_NAME = new hydra.core.Name("tableName");

  public final openGql.grammar.CatalogObjectParentReference parentReference;

  public final openGql.grammar.BindingTableName tableName;

  public ParentAndTableName (openGql.grammar.CatalogObjectParentReference parentReference, openGql.grammar.BindingTableName tableName) {
    this.parentReference = parentReference;
    this.tableName = tableName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParentAndTableName)) {
      return false;
    }
    ParentAndTableName o = (ParentAndTableName) other;
    return java.util.Objects.equals(
      this.parentReference,
      o.parentReference) && java.util.Objects.equals(
      this.tableName,
      o.tableName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(parentReference) + 3 * java.util.Objects.hashCode(tableName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ParentAndTableName other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      parentReference,
      other.parentReference);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      tableName,
      other.tableName);
  }

  public ParentAndTableName withParentReference(openGql.grammar.CatalogObjectParentReference parentReference) {
    return new ParentAndTableName(parentReference, tableName);
  }

  public ParentAndTableName withTableName(openGql.grammar.BindingTableName tableName) {
    return new ParentAndTableName(parentReference, tableName);
  }
}
