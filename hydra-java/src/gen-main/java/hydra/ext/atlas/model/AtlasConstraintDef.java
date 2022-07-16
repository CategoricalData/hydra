package hydra.ext.atlas.model;

/**
 * class that captures details of a constraint.
 */
public class AtlasConstraintDef {
  public final java.util.Optional<String> type;
  
  public final java.util.Map<String, String> params;
  
  public AtlasConstraintDef (java.util.Optional<String> type, java.util.Map<String, String> params) {
    this.type = type;
    this.params = params;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AtlasConstraintDef)) {
      return false;
    }
    AtlasConstraintDef o = (AtlasConstraintDef) (other);
    return type.equals(o.type) && params.equals(o.params);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * params.hashCode();
  }
  
  public AtlasConstraintDef withType(java.util.Optional<String> type) {
    return new AtlasConstraintDef(type, params);
  }
  
  public AtlasConstraintDef withParams(java.util.Map<String, String> params) {
    return new AtlasConstraintDef(type, params);
  }
}