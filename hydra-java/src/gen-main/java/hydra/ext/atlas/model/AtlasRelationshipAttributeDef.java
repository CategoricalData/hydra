package hydra.ext.atlas.model;

/**
 * class that captures details of a struct-attribute.
 */
public class AtlasRelationshipAttributeDef {
  public final AtlasAttributeDef asAtlasAttributeDef;
  
  public final java.util.Optional<String> relationshipTypeName;
  
  public final Boolean isLegacyAttribute;
  
  public AtlasRelationshipAttributeDef (AtlasAttributeDef asAtlasAttributeDef, java.util.Optional<String> relationshipTypeName, Boolean isLegacyAttribute) {
    this.asAtlasAttributeDef = asAtlasAttributeDef;
    this.relationshipTypeName = relationshipTypeName;
    this.isLegacyAttribute = isLegacyAttribute;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AtlasRelationshipAttributeDef)) {
      return false;
    }
    AtlasRelationshipAttributeDef o = (AtlasRelationshipAttributeDef) (other);
    return asAtlasAttributeDef.equals(o.asAtlasAttributeDef) && relationshipTypeName.equals(o.relationshipTypeName) && isLegacyAttribute.equals(o.isLegacyAttribute);
  }
  
  @Override
  public int hashCode() {
    return 2 * asAtlasAttributeDef.hashCode() + 3 * relationshipTypeName.hashCode() + 5 * isLegacyAttribute.hashCode();
  }
  
  public AtlasRelationshipAttributeDef withAsAtlasAttributeDef(AtlasAttributeDef asAtlasAttributeDef) {
    return new AtlasRelationshipAttributeDef(asAtlasAttributeDef, relationshipTypeName, isLegacyAttribute);
  }
  
  public AtlasRelationshipAttributeDef withRelationshipTypeName(java.util.Optional<String> relationshipTypeName) {
    return new AtlasRelationshipAttributeDef(asAtlasAttributeDef, relationshipTypeName, isLegacyAttribute);
  }
  
  public AtlasRelationshipAttributeDef withIsLegacyAttribute(Boolean isLegacyAttribute) {
    return new AtlasRelationshipAttributeDef(asAtlasAttributeDef, relationshipTypeName, isLegacyAttribute);
  }
}