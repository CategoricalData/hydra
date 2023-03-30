package hydra.langs.tinkerpop.mappings;

public class Schema<S, A, T, V, E, P> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/mappings.Schema");
  
  public final hydra.compute.Coder<S, S, hydra.core.Term<A>, V> vertexIds;
  
  public final hydra.compute.Coder<S, S, hydra.core.Term<A>, E> edgeIds;
  
  public final hydra.compute.Coder<S, S, hydra.core.Type<A>, T> propertyTypes;
  
  public final hydra.compute.Coder<S, S, hydra.core.Term<A>, P> propertyValues;
  
  public Schema (hydra.compute.Coder<S, S, hydra.core.Term<A>, V> vertexIds, hydra.compute.Coder<S, S, hydra.core.Term<A>, E> edgeIds, hydra.compute.Coder<S, S, hydra.core.Type<A>, T> propertyTypes, hydra.compute.Coder<S, S, hydra.core.Term<A>, P> propertyValues) {
    this.vertexIds = vertexIds;
    this.edgeIds = edgeIds;
    this.propertyTypes = propertyTypes;
    this.propertyValues = propertyValues;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Schema)) {
      return false;
    }
    Schema o = (Schema) (other);
    return vertexIds.equals(o.vertexIds) && edgeIds.equals(o.edgeIds) && propertyTypes.equals(o.propertyTypes) && propertyValues.equals(o.propertyValues);
  }
  
  @Override
  public int hashCode() {
    return 2 * vertexIds.hashCode() + 3 * edgeIds.hashCode() + 5 * propertyTypes.hashCode() + 7 * propertyValues.hashCode();
  }
  
  public Schema withVertexIds(hydra.compute.Coder<S, S, hydra.core.Term<A>, V> vertexIds) {
    return new Schema(vertexIds, edgeIds, propertyTypes, propertyValues);
  }
  
  public Schema withEdgeIds(hydra.compute.Coder<S, S, hydra.core.Term<A>, E> edgeIds) {
    return new Schema(vertexIds, edgeIds, propertyTypes, propertyValues);
  }
  
  public Schema withPropertyTypes(hydra.compute.Coder<S, S, hydra.core.Type<A>, T> propertyTypes) {
    return new Schema(vertexIds, edgeIds, propertyTypes, propertyValues);
  }
  
  public Schema withPropertyValues(hydra.compute.Coder<S, S, hydra.core.Term<A>, P> propertyValues) {
    return new Schema(vertexIds, edgeIds, propertyTypes, propertyValues);
  }
}