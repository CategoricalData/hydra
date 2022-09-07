package hydra.ext.atlas.model;

/**
 * class that captures details of a struct-attribute.
 */
public class AtlasAttributeDef {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/atlas/model.AtlasAttributeDef");
  
  public final java.util.Optional<String> name;
  
  public final java.util.Optional<String> typeName;
  
  public final Boolean isOptional;
  
  public final java.util.Optional<hydra.ext.atlas.model.AtlasAttributeDef_Cardinality> cardinality;
  
  public final Integer valuesMinCount;
  
  public final Integer valuesMaxCount;
  
  public final Boolean isUnique;
  
  public final Boolean isIndexable;
  
  public final Boolean includeInNotification;
  
  public final java.util.Optional<String> defaultValue;
  
  public final java.util.Optional<String> description;
  
  public final Integer searchWeight;
  
  public final java.util.Optional<hydra.ext.atlas.model.AtlasAttributeDef_IndexType> indexType;
  
  public final java.util.List<hydra.ext.atlas.model.AtlasConstraintDef> constraints;
  
  public final java.util.Map<String, String> options;
  
  public final java.util.Optional<String> displayName;
  
  public AtlasAttributeDef (java.util.Optional<String> name, java.util.Optional<String> typeName, Boolean isOptional, java.util.Optional<hydra.ext.atlas.model.AtlasAttributeDef_Cardinality> cardinality, Integer valuesMinCount, Integer valuesMaxCount, Boolean isUnique, Boolean isIndexable, Boolean includeInNotification, java.util.Optional<String> defaultValue, java.util.Optional<String> description, Integer searchWeight, java.util.Optional<hydra.ext.atlas.model.AtlasAttributeDef_IndexType> indexType, java.util.List<hydra.ext.atlas.model.AtlasConstraintDef> constraints, java.util.Map<String, String> options, java.util.Optional<String> displayName) {
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
  
  public AtlasAttributeDef withName(java.util.Optional<String> name) {
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withTypeName(java.util.Optional<String> typeName) {
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withIsOptional(Boolean isOptional) {
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withCardinality(java.util.Optional<hydra.ext.atlas.model.AtlasAttributeDef_Cardinality> cardinality) {
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withValuesMinCount(Integer valuesMinCount) {
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withValuesMaxCount(Integer valuesMaxCount) {
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withIsUnique(Boolean isUnique) {
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withIsIndexable(Boolean isIndexable) {
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withIncludeInNotification(Boolean includeInNotification) {
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withDefaultValue(java.util.Optional<String> defaultValue) {
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withDescription(java.util.Optional<String> description) {
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withSearchWeight(Integer searchWeight) {
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withIndexType(java.util.Optional<hydra.ext.atlas.model.AtlasAttributeDef_IndexType> indexType) {
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withConstraints(java.util.List<hydra.ext.atlas.model.AtlasConstraintDef> constraints) {
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withOptions(java.util.Map<String, String> options) {
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
  
  public AtlasAttributeDef withDisplayName(java.util.Optional<String> displayName) {
    return new AtlasAttributeDef(name, typeName, isOptional, cardinality, valuesMinCount, valuesMaxCount, isUnique, isIndexable, includeInNotification, defaultValue, description, searchWeight, indexType, constraints, options, displayName);
  }
}