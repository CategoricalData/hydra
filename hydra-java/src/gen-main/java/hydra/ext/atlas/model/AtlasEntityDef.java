package hydra.ext.atlas.model;

/**
 * class that captures details of a entity-type.
 */
public class AtlasEntityDef {
  public final hydra.ext.atlas.model.AtlasStructDef asAtlasStructDef;
  
  public final java.util.Set<String> superTypes;
  
  /**
   * the value of this field is derived from 'superTypes' specified in all AtlasEntityDef
   */
  public final java.util.Set<String> subTypes;
  
  /**
   * the value of this field is derived from all the relationshipDefs this entityType is referenced in
   */
  public final java.util.List<hydra.ext.atlas.model.AtlasRelationshipAttributeDef> relationshipAttributeDefs;
  
  /**
   * the value of this field is derived from all the businessMetadataDefs this entityType is referenced in
   */
  public final java.util.Map<String, java.util.List<hydra.ext.atlas.model.AtlasAttributeDef>> businessAttributeDefs;
  
  public AtlasEntityDef (hydra.ext.atlas.model.AtlasStructDef asAtlasStructDef, java.util.Set<String> superTypes, java.util.Set<String> subTypes, java.util.List<hydra.ext.atlas.model.AtlasRelationshipAttributeDef> relationshipAttributeDefs, java.util.Map<String, java.util.List<hydra.ext.atlas.model.AtlasAttributeDef>> businessAttributeDefs) {
    this.asAtlasStructDef = asAtlasStructDef;
    this.superTypes = superTypes;
    this.subTypes = subTypes;
    this.relationshipAttributeDefs = relationshipAttributeDefs;
    this.businessAttributeDefs = businessAttributeDefs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AtlasEntityDef)) {
      return false;
    }
    AtlasEntityDef o = (AtlasEntityDef) (other);
    return asAtlasStructDef.equals(o.asAtlasStructDef) && superTypes.equals(o.superTypes) && subTypes.equals(o.subTypes) && relationshipAttributeDefs.equals(o.relationshipAttributeDefs) && businessAttributeDefs.equals(o.businessAttributeDefs);
  }
  
  @Override
  public int hashCode() {
    return 2 * asAtlasStructDef.hashCode() + 3 * superTypes.hashCode() + 5 * subTypes.hashCode() + 7 * relationshipAttributeDefs.hashCode() + 11 * businessAttributeDefs.hashCode();
  }
  
  public AtlasEntityDef withAsAtlasStructDef(hydra.ext.atlas.model.AtlasStructDef asAtlasStructDef) {
    return new AtlasEntityDef(asAtlasStructDef, superTypes, subTypes, relationshipAttributeDefs, businessAttributeDefs);
  }
  
  public AtlasEntityDef withSuperTypes(java.util.Set<String> superTypes) {
    return new AtlasEntityDef(asAtlasStructDef, superTypes, subTypes, relationshipAttributeDefs, businessAttributeDefs);
  }
  
  public AtlasEntityDef withSubTypes(java.util.Set<String> subTypes) {
    return new AtlasEntityDef(asAtlasStructDef, superTypes, subTypes, relationshipAttributeDefs, businessAttributeDefs);
  }
  
  public AtlasEntityDef withRelationshipAttributeDefs(java.util.List<hydra.ext.atlas.model.AtlasRelationshipAttributeDef> relationshipAttributeDefs) {
    return new AtlasEntityDef(asAtlasStructDef, superTypes, subTypes, relationshipAttributeDefs, businessAttributeDefs);
  }
  
  public AtlasEntityDef withBusinessAttributeDefs(java.util.Map<String, java.util.List<hydra.ext.atlas.model.AtlasAttributeDef>> businessAttributeDefs) {
    return new AtlasEntityDef(asAtlasStructDef, superTypes, subTypes, relationshipAttributeDefs, businessAttributeDefs);
  }
}