// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.atlas;

import java.io.Serializable;

/**
 * class that captures details of a entity-type.
 */
public class AtlasEntityDef implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/atlas.AtlasEntityDef");
  
  public static final hydra.core.Name FIELD_NAME_AS_ATLAS_STRUCT_DEF = new hydra.core.Name("asAtlasStructDef");
  
  public static final hydra.core.Name FIELD_NAME_SUPER_TYPES = new hydra.core.Name("superTypes");
  
  public static final hydra.core.Name FIELD_NAME_SUB_TYPES = new hydra.core.Name("subTypes");
  
  public static final hydra.core.Name FIELD_NAME_RELATIONSHIP_ATTRIBUTE_DEFS = new hydra.core.Name("relationshipAttributeDefs");
  
  public static final hydra.core.Name FIELD_NAME_BUSINESS_ATTRIBUTE_DEFS = new hydra.core.Name("businessAttributeDefs");
  
  public final hydra.ext.org.apache.atlas.AtlasStructDef asAtlasStructDef;
  
  public final java.util.Set<String> superTypes;
  
  /**
   * the value of this field is derived from 'superTypes' specified in all AtlasEntityDef
   */
  public final java.util.Set<String> subTypes;
  
  /**
   * the value of this field is derived from all the relationshipDefs this entityType is referenced in
   */
  public final java.util.List<hydra.ext.org.apache.atlas.AtlasRelationshipAttributeDef> relationshipAttributeDefs;
  
  /**
   * the value of this field is derived from all the businessMetadataDefs this entityType is referenced in
   */
  public final java.util.Map<String, java.util.List<hydra.ext.org.apache.atlas.AtlasAttributeDef>> businessAttributeDefs;
  
  public AtlasEntityDef (hydra.ext.org.apache.atlas.AtlasStructDef asAtlasStructDef, java.util.Set<String> superTypes, java.util.Set<String> subTypes, java.util.List<hydra.ext.org.apache.atlas.AtlasRelationshipAttributeDef> relationshipAttributeDefs, java.util.Map<String, java.util.List<hydra.ext.org.apache.atlas.AtlasAttributeDef>> businessAttributeDefs) {
    java.util.Objects.requireNonNull((asAtlasStructDef));
    java.util.Objects.requireNonNull((superTypes));
    java.util.Objects.requireNonNull((subTypes));
    java.util.Objects.requireNonNull((relationshipAttributeDefs));
    java.util.Objects.requireNonNull((businessAttributeDefs));
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
  
  public AtlasEntityDef withAsAtlasStructDef(hydra.ext.org.apache.atlas.AtlasStructDef asAtlasStructDef) {
    java.util.Objects.requireNonNull((asAtlasStructDef));
    return new AtlasEntityDef(asAtlasStructDef, superTypes, subTypes, relationshipAttributeDefs, businessAttributeDefs);
  }
  
  public AtlasEntityDef withSuperTypes(java.util.Set<String> superTypes) {
    java.util.Objects.requireNonNull((superTypes));
    return new AtlasEntityDef(asAtlasStructDef, superTypes, subTypes, relationshipAttributeDefs, businessAttributeDefs);
  }
  
  public AtlasEntityDef withSubTypes(java.util.Set<String> subTypes) {
    java.util.Objects.requireNonNull((subTypes));
    return new AtlasEntityDef(asAtlasStructDef, superTypes, subTypes, relationshipAttributeDefs, businessAttributeDefs);
  }
  
  public AtlasEntityDef withRelationshipAttributeDefs(java.util.List<hydra.ext.org.apache.atlas.AtlasRelationshipAttributeDef> relationshipAttributeDefs) {
    java.util.Objects.requireNonNull((relationshipAttributeDefs));
    return new AtlasEntityDef(asAtlasStructDef, superTypes, subTypes, relationshipAttributeDefs, businessAttributeDefs);
  }
  
  public AtlasEntityDef withBusinessAttributeDefs(java.util.Map<String, java.util.List<hydra.ext.org.apache.atlas.AtlasAttributeDef>> businessAttributeDefs) {
    java.util.Objects.requireNonNull((businessAttributeDefs));
    return new AtlasEntityDef(asAtlasStructDef, superTypes, subTypes, relationshipAttributeDefs, businessAttributeDefs);
  }
}