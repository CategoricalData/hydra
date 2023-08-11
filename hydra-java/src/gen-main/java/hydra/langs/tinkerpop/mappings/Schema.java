package hydra.langs.tinkerpop.mappings;

/**
 * A set of mappings which translates between Hydra terms and annotations, and application-specific property graph types
 */
public class Schema<S, A, Vt, Et, Pt, V, E, P> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/mappings.Schema");
  
  public final hydra.compute.Coder<S, S, hydra.core.Type<A>, Vt> vertexIdTypes;
  
  public final hydra.compute.Coder<S, S, hydra.core.Term<A>, V> vertexIds;
  
  public final hydra.compute.Coder<S, S, hydra.core.Type<A>, Et> edgeIdTypes;
  
  public final hydra.compute.Coder<S, S, hydra.core.Term<A>, E> edgeIds;
  
  public final hydra.compute.Coder<S, S, hydra.core.Type<A>, Pt> propertyTypes;
  
  public final hydra.compute.Coder<S, S, hydra.core.Term<A>, P> propertyValues;
  
  public final hydra.langs.tinkerpop.mappings.AnnotationSchema annotations;
  
  public final V defaultVertexId;
  
  public final E defaultEdgeId;
  
  public Schema (hydra.compute.Coder<S, S, hydra.core.Type<A>, Vt> vertexIdTypes, hydra.compute.Coder<S, S, hydra.core.Term<A>, V> vertexIds, hydra.compute.Coder<S, S, hydra.core.Type<A>, Et> edgeIdTypes, hydra.compute.Coder<S, S, hydra.core.Term<A>, E> edgeIds, hydra.compute.Coder<S, S, hydra.core.Type<A>, Pt> propertyTypes, hydra.compute.Coder<S, S, hydra.core.Term<A>, P> propertyValues, hydra.langs.tinkerpop.mappings.AnnotationSchema annotations, V defaultVertexId, E defaultEdgeId) {
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
  
  public Schema withVertexIdTypes(hydra.compute.Coder<S, S, hydra.core.Type<A>, Vt> vertexIdTypes) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
  
  public Schema withVertexIds(hydra.compute.Coder<S, S, hydra.core.Term<A>, V> vertexIds) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
  
  public Schema withEdgeIdTypes(hydra.compute.Coder<S, S, hydra.core.Type<A>, Et> edgeIdTypes) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
  
  public Schema withEdgeIds(hydra.compute.Coder<S, S, hydra.core.Term<A>, E> edgeIds) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
  
  public Schema withPropertyTypes(hydra.compute.Coder<S, S, hydra.core.Type<A>, Pt> propertyTypes) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
  
  public Schema withPropertyValues(hydra.compute.Coder<S, S, hydra.core.Term<A>, P> propertyValues) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
  
  public Schema withAnnotations(hydra.langs.tinkerpop.mappings.AnnotationSchema annotations) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
  
  public Schema withDefaultVertexId(V defaultVertexId) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
  
  public Schema withDefaultEdgeId(E defaultEdgeId) {
    return new Schema(vertexIdTypes, vertexIds, edgeIdTypes, edgeIds, propertyTypes, propertyValues, annotations, defaultVertexId, defaultEdgeId);
  }
}