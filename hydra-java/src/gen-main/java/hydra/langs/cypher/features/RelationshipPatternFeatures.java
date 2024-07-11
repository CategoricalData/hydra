// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for relationship patterns.
 */
public class RelationshipPatternFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.RelationshipPatternFeatures");
  
  /**
   * Whether to expect specifying a disjunction of multiple types in a relationship pattern.
   */
  public final Boolean multipleTypes;
  
  /**
   * Whether to expect binding a variable to a relationship in a relationship pattern (note: included by most if not all implementations).
   */
  public final Boolean variableRelationship;
  
  /**
   * Whether to expect omitting types from a relationship pattern.
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