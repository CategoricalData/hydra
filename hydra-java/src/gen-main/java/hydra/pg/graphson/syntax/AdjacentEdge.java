// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public class AdjacentEdge implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.graphson.syntax.AdjacentEdge");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_VERTEX_ID = new hydra.core.Name("vertexId");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  public final hydra.pg.graphson.syntax.Value id;
  
  public final hydra.pg.graphson.syntax.Value vertexId;
  
  public final java.util.Map<hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.Value> properties;
  
  public AdjacentEdge (hydra.pg.graphson.syntax.Value id, hydra.pg.graphson.syntax.Value vertexId, java.util.Map<hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.Value> properties) {
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((vertexId));
    java.util.Objects.requireNonNull((properties));
    this.id = id;
    this.vertexId = vertexId;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AdjacentEdge)) {
      return false;
    }
    AdjacentEdge o = (AdjacentEdge) (other);
    return id.equals(o.id) && vertexId.equals(o.vertexId) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * vertexId.hashCode() + 5 * properties.hashCode();
  }
  
  public AdjacentEdge withId(hydra.pg.graphson.syntax.Value id) {
    java.util.Objects.requireNonNull((id));
    return new AdjacentEdge(id, vertexId, properties);
  }
  
  public AdjacentEdge withVertexId(hydra.pg.graphson.syntax.Value vertexId) {
    java.util.Objects.requireNonNull((vertexId));
    return new AdjacentEdge(id, vertexId, properties);
  }
  
  public AdjacentEdge withProperties(java.util.Map<hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.Value> properties) {
    java.util.Objects.requireNonNull((properties));
    return new AdjacentEdge(id, vertexId, properties);
  }
}