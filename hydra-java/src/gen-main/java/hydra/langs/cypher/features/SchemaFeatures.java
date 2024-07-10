// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for schema functions.
 */
public class SchemaFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.SchemaFeatures");
  
  /**
   * Whether to expect the type() function.
   */
  public final Boolean type;
  
  /**
   * Whether to expect the valueType() function.
   */
  public final Boolean valueType;
  
  public SchemaFeatures (Boolean type, Boolean valueType) {
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    if (valueType == null) {
      throw new IllegalArgumentException("null value for 'valueType' argument");
    }
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
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    return new SchemaFeatures(type, valueType);
  }
  
  public SchemaFeatures withValueType(Boolean valueType) {
    if (valueType == null) {
      throw new IllegalArgumentException("null value for 'valueType' argument");
    }
    return new SchemaFeatures(type, valueType);
  }
}