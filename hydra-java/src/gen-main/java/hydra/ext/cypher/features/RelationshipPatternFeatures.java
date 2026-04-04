// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Relationship patterns
 */
public class RelationshipPatternFeatures implements Serializable, Comparable<RelationshipPatternFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.RelationshipPatternFeatures");

  public static final hydra.core.Name MULTIPLE_TYPES = new hydra.core.Name("multipleTypes");

  public static final hydra.core.Name VARIABLE_RELATIONSHIP = new hydra.core.Name("variableRelationship");

  public static final hydra.core.Name WILDCARD_TYPE = new hydra.core.Name("wildcardType");

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
    this.multipleTypes = multipleTypes;
    this.variableRelationship = variableRelationship;
    this.wildcardType = wildcardType;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelationshipPatternFeatures)) {
      return false;
    }
    RelationshipPatternFeatures o = (RelationshipPatternFeatures) other;
    return java.util.Objects.equals(
      this.multipleTypes,
      o.multipleTypes) && java.util.Objects.equals(
      this.variableRelationship,
      o.variableRelationship) && java.util.Objects.equals(
      this.wildcardType,
      o.wildcardType);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(multipleTypes) + 3 * java.util.Objects.hashCode(variableRelationship) + 5 * java.util.Objects.hashCode(wildcardType);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RelationshipPatternFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      multipleTypes,
      other.multipleTypes);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      variableRelationship,
      other.variableRelationship);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      wildcardType,
      other.wildcardType);
  }

  public RelationshipPatternFeatures withMultipleTypes(Boolean multipleTypes) {
    return new RelationshipPatternFeatures(multipleTypes, variableRelationship, wildcardType);
  }

  public RelationshipPatternFeatures withVariableRelationship(Boolean variableRelationship) {
    return new RelationshipPatternFeatures(multipleTypes, variableRelationship, wildcardType);
  }

  public RelationshipPatternFeatures withWildcardType(Boolean wildcardType) {
    return new RelationshipPatternFeatures(multipleTypes, variableRelationship, wildcardType);
  }
}
