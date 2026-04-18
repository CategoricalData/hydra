// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ParentAndGraphName implements Serializable, Comparable<ParentAndGraphName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ParentAndGraphName");

  public static final hydra.core.Name PARENT_REFERENCE = new hydra.core.Name("parentReference");

  public static final hydra.core.Name GRAPH_NAME = new hydra.core.Name("graphName");

  public final openGql.grammar.CatalogObjectParentReference parentReference;

  public final String graphName;

  public ParentAndGraphName (openGql.grammar.CatalogObjectParentReference parentReference, String graphName) {
    this.parentReference = parentReference;
    this.graphName = graphName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParentAndGraphName)) {
      return false;
    }
    ParentAndGraphName o = (ParentAndGraphName) other;
    return java.util.Objects.equals(
      this.parentReference,
      o.parentReference) && java.util.Objects.equals(
      this.graphName,
      o.graphName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(parentReference) + 3 * java.util.Objects.hashCode(graphName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ParentAndGraphName other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      parentReference,
      other.parentReference);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      graphName,
      other.graphName);
  }

  public ParentAndGraphName withParentReference(openGql.grammar.CatalogObjectParentReference parentReference) {
    return new ParentAndGraphName(parentReference, graphName);
  }

  public ParentAndGraphName withGraphName(String graphName) {
    return new ParentAndGraphName(parentReference, graphName);
  }
}
