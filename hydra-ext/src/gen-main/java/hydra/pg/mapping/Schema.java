// Note: this is an automatically generated file. Do not edit.

package hydra.pg.mapping;

import java.io.Serializable;

/**
 * A set of mappings which translates between Hydra terms and annotations, and application-specific property graph types
 */
public class Schema<S, T, V> implements Serializable, Comparable<Schema<S, T, V>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.mapping.Schema");

  public static final hydra.core.Name VERTEX_ID_TYPES = new hydra.core.Name("vertexIdTypes");

  public static final hydra.core.Name VERTEX_IDS = new hydra.core.Name("vertexIds");

  public static final hydra.core.Name EDGE_ID_TYPES = new hydra.core.Name("edgeIdTypes");

  public static final hydra.core.Name EDGE_IDS = new hydra.core.Name("edgeIds");

  public static final hydra.core.Name PROPERTY_TYPES = new hydra.core.Name("propertyTypes");

  public static final hydra.core.Name PROPERTY_VALUES = new hydra.core.Name("propertyValues");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name DEFAULT_VERTEX_ID = new hydra.core.Name("defaultVertexId");

  public static final hydra.core.Name DEFAULT_EDGE_ID = new hydra.core.Name("defaultEdgeId");

  public final hydra.coders.Coder<hydra.core.Type, T> vertexIdTypes;

  public final hydra.coders.Coder<hydra.core.Term, V> vertexIds;

  public final hydra.coders.Coder<hydra.core.Type, T> edgeIdTypes;

  public final hydra.coders.Coder<hydra.core.Term, V> edgeIds;

  public final hydra.coders.Coder<hydra.core.Type, T> propertyTypes;

  public final hydra.coders.Coder<hydra.core.Term, V> propertyValues;

  public final hydra.pg.mapping.AnnotationSchema annotations;

  public final V defaultVertexId;

  public final V defaultEdgeId;

  public Schema (hydra.coders.Coder<hydra.core.Type, T> vertexIdTypes, hydra.coders.Coder<hydra.core.Term, V> vertexIds, hydra.coders.Coder<hydra.core.Type, T> edgeIdTypes, hydra.coders.Coder<hydra.core.Term, V> edgeIds, hydra.coders.Coder<hydra.core.Type, T> propertyTypes, hydra.coders.Coder<hydra.core.Term, V> propertyValues, hydra.pg.mapping.AnnotationSchema annotations, V defaultVertexId, V defaultEdgeId) {
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
    Schema o = (Schema) other;
    return java.util.Objects.equals(
      this.vertexIdTypes,
      o.vertexIdTypes) && java.util.Objects.equals(
      this.vertexIds,
      o.vertexIds) && java.util.Objects.equals(
      this.edgeIdTypes,
      o.edgeIdTypes) && java.util.Objects.equals(
      this.edgeIds,
      o.edgeIds) && java.util.Objects.equals(
      this.propertyTypes,
      o.propertyTypes) && java.util.Objects.equals(
      this.propertyValues,
      o.propertyValues) && java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.defaultVertexId,
      o.defaultVertexId) && java.util.Objects.equals(
      this.defaultEdgeId,
      o.defaultEdgeId);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(vertexIdTypes) + 3 * java.util.Objects.hashCode(vertexIds) + 5 * java.util.Objects.hashCode(edgeIdTypes) + 7 * java.util.Objects.hashCode(edgeIds) + 11 * java.util.Objects.hashCode(propertyTypes) + 13 * java.util.Objects.hashCode(propertyValues) + 17 * java.util.Objects.hashCode(annotations) + 19 * java.util.Objects.hashCode(defaultVertexId) + 23 * java.util.Objects.hashCode(defaultEdgeId);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Schema other) {
    int cmp = 0;
    cmp = ((Comparable) vertexIdTypes).compareTo(other.vertexIdTypes);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) vertexIds).compareTo(other.vertexIds);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) edgeIdTypes).compareTo(other.edgeIdTypes);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) edgeIds).compareTo(other.edgeIds);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) propertyTypes).compareTo(other.propertyTypes);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) propertyValues).compareTo(other.propertyValues);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) annotations).compareTo(other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) defaultVertexId).compareTo(other.defaultVertexId);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) defaultEdgeId).compareTo(other.defaultEdgeId);
  }

  public Schema withVertexIdTypes(hydra.coders.Coder<hydra.core.Type, T> vertexIdTypes) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }

  public Schema withVertexIds(hydra.coders.Coder<hydra.core.Term, V> vertexIds) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }

  public Schema withEdgeIdTypes(hydra.coders.Coder<hydra.core.Type, T> edgeIdTypes) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }

  public Schema withEdgeIds(hydra.coders.Coder<hydra.core.Term, V> edgeIds) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }

  public Schema withPropertyTypes(hydra.coders.Coder<hydra.core.Type, T> propertyTypes) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }

  public Schema withPropertyValues(hydra.coders.Coder<hydra.core.Term, V> propertyValues) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }

  public Schema withAnnotations(hydra.pg.mapping.AnnotationSchema annotations) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }

  public Schema withDefaultVertexId(V defaultVertexId) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }

  public Schema withDefaultEdgeId(V defaultEdgeId) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
}
