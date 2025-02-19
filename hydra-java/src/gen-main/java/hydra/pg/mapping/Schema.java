// Note: this is an automatically generated file. Do not edit.

package hydra.pg.mapping;

/**
 * A set of mappings which translates between Hydra terms and annotations, and application-specific property graph types
 */
public class Schema<S, T, V> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.mapping.Schema");
  
  public static final hydra.core.Name FIELD_NAME_VERTEX_ID_TYPES = new hydra.core.Name("vertexIdTypes");
  
  public static final hydra.core.Name FIELD_NAME_VERTEX_IDS = new hydra.core.Name("vertexIds");
  
  public static final hydra.core.Name FIELD_NAME_EDGE_ID_TYPES = new hydra.core.Name("edgeIdTypes");
  
  public static final hydra.core.Name FIELD_NAME_EDGE_IDS = new hydra.core.Name("edgeIds");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY_TYPES = new hydra.core.Name("propertyTypes");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY_VALUES = new hydra.core.Name("propertyValues");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT_VERTEX_ID = new hydra.core.Name("defaultVertexId");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT_EDGE_ID = new hydra.core.Name("defaultEdgeId");
  
  public final hydra.compute.Coder<S, S, hydra.core.Type, T> vertexIdTypes;
  
  public final hydra.compute.Coder<S, S, hydra.core.Term, V> vertexIds;
  
  public final hydra.compute.Coder<S, S, hydra.core.Type, T> edgeIdTypes;
  
  public final hydra.compute.Coder<S, S, hydra.core.Term, V> edgeIds;
  
  public final hydra.compute.Coder<S, S, hydra.core.Type, T> propertyTypes;
  
  public final hydra.compute.Coder<S, S, hydra.core.Term, V> propertyValues;
  
  public final hydra.pg.mapping.AnnotationSchema annotations;
  
  public final V defaultVertexId;
  
  public final V defaultEdgeId;
  
  public Schema (hydra.compute.Coder<S, S, hydra.core.Type, T> vertexIdTypes, hydra.compute.Coder<S, S, hydra.core.Term, V> vertexIds, hydra.compute.Coder<S, S, hydra.core.Type, T> edgeIdTypes, hydra.compute.Coder<S, S, hydra.core.Term, V> edgeIds, hydra.compute.Coder<S, S, hydra.core.Type, T> propertyTypes, hydra.compute.Coder<S, S, hydra.core.Term, V> propertyValues, hydra.pg.mapping.AnnotationSchema annotations, V defaultVertexId, V defaultEdgeId) {
    java.util.Objects.requireNonNull((vertexIdTypes));
    java.util.Objects.requireNonNull((vertexIds));
    java.util.Objects.requireNonNull((edgeIdTypes));
    java.util.Objects.requireNonNull((edgeIds));
    java.util.Objects.requireNonNull((propertyTypes));
    java.util.Objects.requireNonNull((propertyValues));
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((defaultVertexId));
    java.util.Objects.requireNonNull((defaultEdgeId));
    this.vertexIdTypes = vertexIdTypes;
    this.vertexIds = vertexIds;
    this.edgeIdTypes = edgeIdTypes;
    this.edgeIds = edgeIds;
    this.propertyTypes = propertyTypes;
    this.propertyValues = propertyValues;
    this.annotations = annotations;
    this.defaultVertexId = defaultVertexId;
    this.defaultEdgeId = defaultEdgeId;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Schema)) {
      return false;
    }
    Schema o = (Schema) (other);
    return vertexIdTypes.equals(o.vertexIdTypes) && vertexIds.equals(o.vertexIds) && edgeIdTypes.equals(o.edgeIdTypes) && edgeIds.equals(o.edgeIds) && propertyTypes.equals(o.propertyTypes) && propertyValues.equals(o.propertyValues) && annotations.equals(o.annotations) && defaultVertexId.equals(o.defaultVertexId) && defaultEdgeId.equals(o.defaultEdgeId);
  }
  
  @Override
  public int hashCode() {
    return 2 * vertexIdTypes.hashCode() + 3 * vertexIds.hashCode() + 5 * edgeIdTypes.hashCode() + 7 * edgeIds.hashCode() + 11 * propertyTypes.hashCode() + 13 * propertyValues.hashCode() + 17 * annotations.hashCode() + 19 * defaultVertexId.hashCode() + 23 * defaultEdgeId.hashCode();
  }
  
  public Schema withVertexIdTypes(hydra.compute.Coder<S, S, hydra.core.Type, T> vertexIdTypes) {
    java.util.Objects.requireNonNull((vertexIdTypes));
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
  
  public Schema withVertexIds(hydra.compute.Coder<S, S, hydra.core.Term, V> vertexIds) {
    java.util.Objects.requireNonNull((vertexIds));
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
  
  public Schema withEdgeIdTypes(hydra.compute.Coder<S, S, hydra.core.Type, T> edgeIdTypes) {
    java.util.Objects.requireNonNull((edgeIdTypes));
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
  
  public Schema withEdgeIds(hydra.compute.Coder<S, S, hydra.core.Term, V> edgeIds) {
    java.util.Objects.requireNonNull((edgeIds));
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
  
  public Schema withPropertyTypes(hydra.compute.Coder<S, S, hydra.core.Type, T> propertyTypes) {
    java.util.Objects.requireNonNull((propertyTypes));
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
  
  public Schema withPropertyValues(hydra.compute.Coder<S, S, hydra.core.Term, V> propertyValues) {
    java.util.Objects.requireNonNull((propertyValues));
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
  
  public Schema withAnnotations(hydra.pg.mapping.AnnotationSchema annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
  
  public Schema withDefaultVertexId(V defaultVertexId) {
    java.util.Objects.requireNonNull((defaultVertexId));
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
  
  public Schema withDefaultEdgeId(V defaultEdgeId) {
    java.util.Objects.requireNonNull((defaultEdgeId));
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
}