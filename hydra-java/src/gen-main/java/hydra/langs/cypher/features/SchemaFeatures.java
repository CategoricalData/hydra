// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for schema functions.
 */
public class SchemaFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/features.SchemaFeatures");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_VALUE_TYPE = new hydra.core.Name("valueType");
  
  /**
   * Whether to expect the type() function.
   */
  public final Boolean type;
  
  /**
   * Whether to expect the valueType() function.
   */
  public final Boolean valueType;
  
  public SchemaFeatures (Boolean type, Boolean valueType) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((valueType));
    this.type = type;
    this.valueType = valueType;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SchemaFeatures)) {
      return false;
    }
    SchemaFeatures o = (SchemaFeatures) (other);
    return type.equals(o.type) && valueType.equals(o.valueType);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * valueType.hashCode();
  }
  
  public SchemaFeatures withType(Boolean type) {
    java.util.Objects.requireNonNull((type));
    return new SchemaFeatures(type, valueType);
  }
  
  public SchemaFeatures withValueType(Boolean valueType) {
    java.util.Objects.requireNonNull((valueType));
    return new SchemaFeatures(type, valueType);
  }
}