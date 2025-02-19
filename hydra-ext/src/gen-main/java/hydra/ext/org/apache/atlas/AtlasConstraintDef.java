// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.atlas;

import java.io.Serializable;

/**
 * class that captures details of a constraint.
 */
public class AtlasConstraintDef implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.atlas.AtlasConstraintDef");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_PARAMS = new hydra.core.Name("params");
  
  public final hydra.util.Opt<String> type;
  
  public final java.util.Map<String, String> params;
  
  public AtlasConstraintDef (hydra.util.Opt<String> type, java.util.Map<String, String> params) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((params));
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
  
  public AtlasConstraintDef withType(hydra.util.Opt<String> type) {
    java.util.Objects.requireNonNull((type));
    return new AtlasConstraintDef(type, params);
  }
  
  public AtlasConstraintDef withParams(java.util.Map<String, String> params) {
    java.util.Objects.requireNonNull((params));
    return new AtlasConstraintDef(type, params);
  }
}