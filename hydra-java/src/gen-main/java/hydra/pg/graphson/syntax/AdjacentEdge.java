// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public class AdjacentEdge implements Serializable, Comparable<AdjacentEdge> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.graphson.syntax.AdjacentEdge");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name VERTEX_ID = new hydra.core.Name("vertexId");

  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");

  public final hydra.pg.graphson.syntax.Value id;

  public final hydra.pg.graphson.syntax.Value vertexId;

  public final hydra.util.PersistentMap<hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.Value> properties;

  public AdjacentEdge (hydra.pg.graphson.syntax.Value id, hydra.pg.graphson.syntax.Value vertexId, hydra.util.PersistentMap<hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.Value> properties) {
    this.id = id;
    this.vertexId = vertexId;
    this.properties = properties;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AdjacentEdge)) {
      return false;
    }
    AdjacentEdge o = (AdjacentEdge) other;
    return java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.vertexId,
      o.vertexId) && java.util.Objects.equals(
      this.properties,
      o.properties);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(id) + 3 * java.util.Objects.hashCode(vertexId) + 5 * java.util.Objects.hashCode(properties);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AdjacentEdge other) {
    int cmp = 0;
    cmp = ((Comparable) id).compareTo(other.id);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) vertexId).compareTo(other.vertexId);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) properties).compareTo(other.properties);
  }

  public AdjacentEdge withId(hydra.pg.graphson.syntax.Value id) {
    return new AdjacentEdge(id, vertexId, properties);
  }

  public AdjacentEdge withVertexId(hydra.pg.graphson.syntax.Value vertexId) {
    return new AdjacentEdge(id, vertexId, properties);
  }

  public AdjacentEdge withProperties(hydra.util.PersistentMap<hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.Value> properties) {
    return new AdjacentEdge(id, vertexId, properties);
  }
}
