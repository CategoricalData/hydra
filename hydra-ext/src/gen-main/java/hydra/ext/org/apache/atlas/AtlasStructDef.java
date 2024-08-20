// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.atlas;

import java.io.Serializable;

/**
 * class that captures details of a struct-type.
 */
public class AtlasStructDef implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/atlas.AtlasStructDef");
  
  public static final hydra.core.Name FIELD_NAME_AS_ATLAS_BASE_TYPE_DEF = new hydra.core.Name("asAtlasBaseTypeDef");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTE_DEFS = new hydra.core.Name("attributeDefs");
  
  public final hydra.ext.org.apache.atlas.AtlasBaseTypeDef asAtlasBaseTypeDef;
  
  public final java.util.List<hydra.ext.org.apache.atlas.AtlasAttributeDef> attributeDefs;
  
  public AtlasStructDef (hydra.ext.org.apache.atlas.AtlasBaseTypeDef asAtlasBaseTypeDef, java.util.List<hydra.ext.org.apache.atlas.AtlasAttributeDef> attributeDefs) {
    java.util.Objects.requireNonNull((asAtlasBaseTypeDef));
    java.util.Objects.requireNonNull((attributeDefs));
    this.asAtlasBaseTypeDef = asAtlasBaseTypeDef;
    this.attributeDefs = attributeDefs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AtlasStructDef)) {
      return false;
    }
    AtlasStructDef o = (AtlasStructDef) (other);
    return asAtlasBaseTypeDef.equals(o.asAtlasBaseTypeDef) && attributeDefs.equals(o.attributeDefs);
  }
  
  @Override
  public int hashCode() {
    return 2 * asAtlasBaseTypeDef.hashCode() + 3 * attributeDefs.hashCode();
  }
  
  public AtlasStructDef withAsAtlasBaseTypeDef(hydra.ext.org.apache.atlas.AtlasBaseTypeDef asAtlasBaseTypeDef) {
    java.util.Objects.requireNonNull((asAtlasBaseTypeDef));
    return new AtlasStructDef(asAtlasBaseTypeDef, attributeDefs);
  }
  
  public AtlasStructDef withAttributeDefs(java.util.List<hydra.ext.org.apache.atlas.AtlasAttributeDef> attributeDefs) {
    java.util.Objects.requireNonNull((attributeDefs));
    return new AtlasStructDef(asAtlasBaseTypeDef, attributeDefs);
  }
}