// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Relationship patterns
 */
public class RelationshipPatternFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.features.RelationshipPatternFeatures");
  
  public static final hydra.core.Name FIELD_NAME_MULTIPLE_TYPES = new hydra.core.Name("multipleTypes");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE_RELATIONSHIP = new hydra.core.Name("variableRelationship");
  
  public static final hydra.core.Name FIELD_NAME_WILDCARD_TYPE = new hydra.core.Name("wildcardType");
  
  /**
   * Specifying a disjunction of multiple types in a relationship pattern
   */
  public final Boolean multipleTypes;
  
  /**
   * Binding a variable to a relationship in a relationship pattern (note: included by most if not all implementations).
   */
  public final Boolean variableRelationship;
  
  /**
   * Omitting types from a relationship pattern
   */
  public final Boolean wildcardType;
  
  public RelationshipPatternFeatures (Boolean multipleTypes, Boolean variableRelationship, Boolean wildcardType) {
    java.util.Objects.requireNonNull((multipleTypes));
    java.util.Objects.requireNonNull((variableRelationship));
    java.util.Objects.requireNonNull((wildcardType));
    this.multipleTypes = multipleTypes;
    this.variableRelationship = variableRelationship;
    this.wildcardType = wildcardType;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationshipPatternFeatures)) {
      return false;
    }
    RelationshipPatternFeatures o = (RelationshipPatternFeatures) (other);
    return multipleTypes.equals(o.multipleTypes) && variableRelationship.equals(o.variableRelationship) && wildcardType.equals(o.wildcardType);
  }
  
  @Override
  public int hashCode() {
    return 2 * multipleTypes.hashCode() + 3 * variableRelationship.hashCode() + 5 * wildcardType.hashCode();
  }
  
  public RelationshipPatternFeatures withMultipleTypes(Boolean multipleTypes) {
    java.util.Objects.requireNonNull((multipleTypes));
    return new RelationshipPatternFeatures(multipleTypes, variableRelationship, wildcardType);
  }
  
  public RelationshipPatternFeatures withVariableRelationship(Boolean variableRelationship) {
    java.util.Objects.requireNonNull((variableRelationship));
    return new RelationshipPatternFeatures(multipleTypes, variableRelationship, wildcardType);
  }
  
  public RelationshipPatternFeatures withWildcardType(Boolean wildcardType) {
    java.util.Objects.requireNonNull((wildcardType));
    return new RelationshipPatternFeatures(multipleTypes, variableRelationship, wildcardType);
  }
}