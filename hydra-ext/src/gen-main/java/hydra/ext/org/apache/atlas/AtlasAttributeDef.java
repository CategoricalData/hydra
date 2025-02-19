// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.atlas;

import java.io.Serializable;

/**
 * class that captures details of a struct-attribute.
 */
public class AtlasAttributeDef implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.atlas.AtlasAttributeDef");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_NAME = new hydra.core.Name("typeName");
  
  public static final hydra.core.Name FIELD_NAME_IS_OPTIONAL = new hydra.core.Name("isOptional");
  
  public static final hydra.core.Name FIELD_NAME_CARDINALITY = new hydra.core.Name("cardinality");
  
  public static final hydra.core.Name FIELD_NAME_VALUES_MIN_COUNT = new hydra.core.Name("valuesMinCount");
  
  public static final hydra.core.Name FIELD_NAME_VALUES_MAX_COUNT = new hydra.core.Name("valuesMaxCount");
  
  public static final hydra.core.Name FIELD_NAME_IS_UNIQUE = new hydra.core.Name("isUnique");
  
  public static final hydra.core.Name FIELD_NAME_IS_INDEXABLE = new hydra.core.Name("isIndexable");
  
  public static final hydra.core.Name FIELD_NAME_INCLUDE_IN_NOTIFICATION = new hydra.core.Name("includeInNotification");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT_VALUE = new hydra.core.Name("defaultValue");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_SEARCH_WEIGHT = new hydra.core.Name("searchWeight");
  
  public static final hydra.core.Name FIELD_NAME_INDEX_TYPE = new hydra.core.Name("indexType");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRAINTS = new hydra.core.Name("constraints");
  
  public static final hydra.core.Name FIELD_NAME_OPTIONS = new hydra.core.Name("options");
  
  public static final hydra.core.Name FIELD_NAME_DISPLAY_NAME = new hydra.core.Name("displayName");
  
  public final hydra.util.Opt<String> name;
  
  public final hydra.util.Opt<String> typeName;
  
  public final Boolean isOptional;
  
  public final hydra.util.Opt<hydra.ext.org.apache.atlas.AtlasAttributeDef_Cardinality> cardinality;
  
  public final Integer valuesMinCount;
  
  public final Integer valuesMaxCount;
  
  public final Boolean isUnique;
  
  public final Boolean isIndexable;
  
  public final Boolean includeInNotification;
  
  public final hydra.util.Opt<String> defaultValue;
  
  public final hydra.util.Opt<String> description;
  
  public final Integer searchWeight;
  
  public final hydra.util.Opt<hydra.ext.org.apache.atlas.AtlasAttributeDef_IndexType> indexType;
  
  public final java.util.List<hydra.ext.org.apache.atlas.AtlasConstraintDef> constraints;
  
  public final java.util.Map<String, String> options;
  
  public final hydra.util.Opt<String> displayName;
  
  public AtlasAttributeDef (hydra.util.Opt<String> name, hydra.util.Opt<String> typeName, Boolean isOptional, hydra.util.Opt<hydra.ext.org.apache.atlas.AtlasAttributeDef_Cardinality> cardinality, Integer valuesMinCount, Integer valuesMaxCount, Boolean isUnique, Boolean isIndexable, Boolean includeInNotification, hydra.util.Opt<String> defaultValue, hydra.util.Opt<String> description, Integer searchWeight, hydra.util.Opt<hydra.ext.org.apache.atlas.AtlasAttributeDef_IndexType> indexType, java.util.List<hydra.ext.org.apache.atlas.AtlasConstraintDef> constraints, java.util.Map<String, String> options, hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((typeName));
    java.util.Objects.requireNonNull((isOptional));
    java.util.Objects.requireNonNull((cardinality));
    java.util.Objects.requireNonNull((valuesMinCount));
    java.util.Objects.requireNonNull((valuesMaxCount));
    java.util.Objects.requireNonNull((isUnique));
    java.util.Objects.requireNonNull((isIndexable));
    java.util.Objects.requireNonNull((includeInNotification));
    java.util.Objects.requireNonNull((defaultValue));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((searchWeight));
    java.util.Objects.requireNonNull((indexType));
    java.util.Objects.requireNonNull((constraints));
    java.util.Objects.requireNonNull((options));
    java.util.Objects.requireNonNull((displayName));
    this.name = name;
    this.typeName = typeName;
    this.isOptional = isOptional;
    this.cardinality = cardinality;
    this.valuesMinCount = valuesMinCount;
    this.valuesMaxCount = valuesMaxCount;
    this.isUnique = isUnique;
    this.isIndexable = isIndexable;
    this.includeInNotification = includeInNotification;
    this.defaultValue = defaultValue;
    this.description = description;
    this.searchWeight = searchWeight;
    this.indexType = indexType;
    this.constraints = constraints;
    this.options = options;
    this.displayName = displayName;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AtlasAttributeDef)) {
      return false;
    }
    AtlasAttributeDef o = (AtlasAttributeDef) (other);
    return name.equals(o.name) && typeName.equals(o.typeName) && isOptional.equals(o.isOptional) && cardinality.equals(o.cardinality) && valuesMinCount.equals(o.valuesMinCount) && valuesMaxCount.equals(o.valuesMaxCount) && isUnique.equals(o.isUnique) && isIndexable.equals(o.isIndexable) && includeInNotification.equals(o.includeInNotification) && defaultValue.equals(o.defaultValue) && description.equals(o.description) && searchWeight.equals(o.searchWeight) && indexType.equals(o.indexType) && constraints.equals(o.constraints) && options.equals(o.options) && displayName.equals(o.displayName);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * typeName.hashCode() + 5 * isOptional.hashCode() + 7 * cardinality.hashCode() + 11 * valuesMinCount.hashCode() + 13 * valuesMaxCount.hashCode() + 17 * isUnique.hashCode() + 19 * isIndexable.hashCode() + 23 * includeInNotification.hashCode() + 29 * defaultValue.hashCode() + 31 * description.hashCode() + 37 * searchWeight.hashCode() + 41 * indexType.hashCode() + 43 * constraints.hashCode() + 47 * options.hashCode() + 53 * displayName.hashCode();
  }
  
  public AtlasAttributeDef withName(hydra.util.Opt<String> name) {
    java.util.Objects.requireNonNull((name));
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withTypeName(hydra.util.Opt<String> typeName) {
    java.util.Objects.requireNonNull((typeName));
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withIsOptional(Boolean isOptional) {
    java.util.Objects.requireNonNull((isOptional));
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withCardinality(hydra.util.Opt<hydra.ext.org.apache.atlas.AtlasAttributeDef_Cardinality> cardinality) {
    java.util.Objects.requireNonNull((cardinality));
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withValuesMinCount(Integer valuesMinCount) {
    java.util.Objects.requireNonNull((valuesMinCount));
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withValuesMaxCount(Integer valuesMaxCount) {
    java.util.Objects.requireNonNull((valuesMaxCount));
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withIsUnique(Boolean isUnique) {
    java.util.Objects.requireNonNull((isUnique));
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withIsIndexable(Boolean isIndexable) {
    java.util.Objects.requireNonNull((isIndexable));
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withIncludeInNotification(Boolean includeInNotification) {
    java.util.Objects.requireNonNull((includeInNotification));
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withDefaultValue(hydra.util.Opt<String> defaultValue) {
    java.util.Objects.requireNonNull((defaultValue));
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withSearchWeight(Integer searchWeight) {
    java.util.Objects.requireNonNull((searchWeight));
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withIndexType(hydra.util.Opt<hydra.ext.org.apache.atlas.AtlasAttributeDef_IndexType> indexType) {
    java.util.Objects.requireNonNull((indexType));
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withConstraints(java.util.List<hydra.ext.org.apache.atlas.AtlasConstraintDef> constraints) {
    java.util.Objects.requireNonNull((constraints));
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withOptions(java.util.Map<String, String> options) {
    java.util.Objects.requireNonNull((options));
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withDisplayName(hydra.util.Opt<String> displayName) {
    java.util.Objects.requireNonNull((displayName));
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
}