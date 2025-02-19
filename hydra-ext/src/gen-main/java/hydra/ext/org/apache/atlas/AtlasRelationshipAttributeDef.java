// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.atlas;

import java.io.Serializable;

/**
 * class that captures details of a struct-attribute.
 */
public class AtlasRelationshipAttributeDef implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.atlas.AtlasRelationshipAttributeDef");
  
  public static final hydra.core.Name FIELD_NAME_AS_ATLAS_ATTRIBUTE_DEF = new hydra.core.Name("asAtlasAttributeDef");
  
  public static final hydra.core.Name FIELD_NAME_RELATIONSHIP_TYPE_NAME = new hydra.core.Name("relationshipTypeName");
  
  public static final hydra.core.Name FIELD_NAME_IS_LEGACY_ATTRIBUTE = new hydra.core.Name("isLegacyAttribute");
  
  public final hydra.ext.org.apache.atlas.AtlasAttributeDef asAtlasAttributeDef;
  
  public final hydra.util.Opt<String> relationshipTypeName;
  
  public final Boolean isLegacyAttribute;
  
  public AtlasRelationshipAttributeDef (hydra.ext.org.apache.atlas.AtlasAttributeDef asAtlasAttributeDef, hydra.util.Opt<String> relationshipTypeName, Boolean isLegacyAttribute) {
    java.util.Objects.requireNonNull((asAtlasAttributeDef));
    java.util.Objects.requireNonNull((relationshipTypeName));
    java.util.Objects.requireNonNull((isLegacyAttribute));
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
  
  public AtlasRelationshipAttributeDef withAsAtlasAttributeDef(hydra.ext.org.apache.atlas.AtlasAttributeDef asAtlasAttributeDef) {
    java.util.Objects.requireNonNull((asAtlasAttributeDef));
    return new AtlasRelationshipAttributeDef(asAtlasAttributeDef, relationshipTypeName, isLegacyAttribute);
  }
  
  public AtlasRelationshipAttributeDef withRelationshipTypeName(hydra.util.Opt<String> relationshipTypeName) {
    java.util.Objects.requireNonNull((relationshipTypeName));
    return new AtlasRelationshipAttributeDef(asAtlasAttributeDef, relationshipTypeName, isLegacyAttribute);
  }
  
  public AtlasRelationshipAttributeDef withIsLegacyAttribute(Boolean isLegacyAttribute) {
    java.util.Objects.requireNonNull((isLegacyAttribute));
    return new AtlasRelationshipAttributeDef(asAtlasAttributeDef, relationshipTypeName, isLegacyAttribute);
  }
}