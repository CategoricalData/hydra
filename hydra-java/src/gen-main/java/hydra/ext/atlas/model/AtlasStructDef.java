package hydra.ext.atlas.model;

/**
 * class that captures details of a struct-type.
 */
public class AtlasStructDef {
  public final hydra.ext.atlas.model.AtlasBaseTypeDef asAtlasBaseTypeDef;
  
  public final java.util.List<hydra.ext.atlas.model.AtlasAttributeDef> attributeDefs;
  
  public AtlasStructDef (hydra.ext.atlas.model.AtlasBaseTypeDef asAtlasBaseTypeDef, java.util.List<hydra.ext.atlas.model.AtlasAttributeDef> attributeDefs) {
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
  
  public AtlasStructDef withAsAtlasBaseTypeDef(hydra.ext.atlas.model.AtlasBaseTypeDef asAtlasBaseTypeDef) {
    return new AtlasStructDef(asAtlasBaseTypeDef, attributeDefs);
  }
  
  public AtlasStructDef withAttributeDefs(java.util.List<hydra.ext.atlas.model.AtlasAttributeDef> attributeDefs) {
    return new AtlasStructDef(asAtlasBaseTypeDef, attributeDefs);
  }
}