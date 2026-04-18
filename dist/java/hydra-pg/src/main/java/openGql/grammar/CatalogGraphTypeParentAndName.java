// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class CatalogGraphTypeParentAndName implements Serializable, Comparable<CatalogGraphTypeParentAndName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CatalogGraphTypeParentAndName");

  public static final hydra.core.Name PARENT_REFERENCE = new hydra.core.Name("parentReference");

  public static final hydra.core.Name GRAPH_TYPE_NAME = new hydra.core.Name("graphTypeName");

  public final hydra.util.Maybe<openGql.grammar.CatalogObjectParentReference> parentReference;

  public final String graphTypeName;

  public CatalogGraphTypeParentAndName (hydra.util.Maybe<openGql.grammar.CatalogObjectParentReference> parentReference, String graphTypeName) {
    this.parentReference = parentReference;
    this.graphTypeName = graphTypeName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CatalogGraphTypeParentAndName)) {
      return false;
    }
    CatalogGraphTypeParentAndName o = (CatalogGraphTypeParentAndName) other;
    return java.util.Objects.equals(
      this.parentReference,
      o.parentReference) && java.util.Objects.equals(
      this.graphTypeName,
      o.graphTypeName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(parentReference) + 3 * java.util.Objects.hashCode(graphTypeName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CatalogGraphTypeParentAndName other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      parentReference,
      other.parentReference);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      graphTypeName,
      other.graphTypeName);
  }

  public CatalogGraphTypeParentAndName withParentReference(hydra.util.Maybe<openGql.grammar.CatalogObjectParentReference> parentReference) {
    return new CatalogGraphTypeParentAndName(parentReference, graphTypeName);
  }

  public CatalogGraphTypeParentAndName withGraphTypeName(String graphTypeName) {
    return new CatalogGraphTypeParentAndName(parentReference, graphTypeName);
  }
}
