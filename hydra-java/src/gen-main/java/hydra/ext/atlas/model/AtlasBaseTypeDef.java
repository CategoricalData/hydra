package hydra.ext.atlas.model;

/**
 * Base class that captures common-attributes for all Atlas types.
 */
public class AtlasBaseTypeDef {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/atlas/model.AtlasBaseTypeDef");
  
  public final java.util.Optional<hydra.ext.atlas.model.TypeCategory> category;
  
  public final java.util.Optional<String> guid;
  
  public final java.util.Optional<String> createdBy;
  
  public final java.util.Optional<String> updatedBy;
  
  public final java.util.Optional<hydra.ext.xml.schema.DateTime> createTime;
  
  public final java.util.Optional<hydra.ext.xml.schema.DateTime> updateTime;
  
  public final java.util.Optional<Long> version;
  
  public final java.util.Optional<String> name;
  
  public final java.util.Optional<String> description;
  
  public final java.util.Optional<String> typeVersion;
  
  public final java.util.Optional<String> serviceType;
  
  public final java.util.Map<String, String> options;
  
  public AtlasBaseTypeDef (java.util.Optional<hydra.ext.atlas.model.TypeCategory> category, java.util.Optional<String> guid, java.util.Optional<String> createdBy, java.util.Optional<String> updatedBy, java.util.Optional<hydra.ext.xml.schema.DateTime> createTime, java.util.Optional<hydra.ext.xml.schema.DateTime> updateTime, java.util.Optional<Long> version, java.util.Optional<String> name, java.util.Optional<String> description, java.util.Optional<String> typeVersion, java.util.Optional<String> serviceType, java.util.Map<String, String> options) {
    this.category = category;
    this.guid = guid;
    this.createdBy = createdBy;
    this.updatedBy = updatedBy;
    this.createTime = createTime;
    this.updateTime = updateTime;
    this.version = version;
    this.name = name;
    this.description = description;
    this.typeVersion = typeVersion;
    this.serviceType = serviceType;
    this.options = options;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AtlasBaseTypeDef)) {
      return false;
    }
    AtlasBaseTypeDef o = (AtlasBaseTypeDef) (other);
    return category.equals(o.category) && guid.equals(o.guid) && createdBy.equals(o.createdBy) && updatedBy.equals(o.updatedBy) && createTime.equals(o.createTime) && updateTime.equals(o.updateTime) && version.equals(o.version) && name.equals(o.name) && description.equals(o.description) && typeVersion.equals(o.typeVersion) && serviceType.equals(o.serviceType) && options.equals(o.options);
  }
  
  @Override
  public int hashCode() {
    return 2 * category.hashCode() + 3 * guid.hashCode() + 5 * createdBy.hashCode() + 7 * updatedBy.hashCode() + 11 * createTime.hashCode() + 13 * updateTime.hashCode() + 17 * version.hashCode() + 19 * name.hashCode() + 23 * description.hashCode() + 29 * typeVersion.hashCode() + 31 * serviceType.hashCode() + 37 * options.hashCode();
  }
  
  public AtlasBaseTypeDef withCategory(java.util.Optional<hydra.ext.atlas.model.TypeCategory> category) {
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withGuid(java.util.Optional<String> guid) {
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withCreatedBy(java.util.Optional<String> createdBy) {
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withUpdatedBy(java.util.Optional<String> updatedBy) {
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withCreateTime(java.util.Optional<hydra.ext.xml.schema.DateTime> createTime) {
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withUpdateTime(java.util.Optional<hydra.ext.xml.schema.DateTime> updateTime) {
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withVersion(java.util.Optional<Long> version) {
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withName(java.util.Optional<String> name) {
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withDescription(java.util.Optional<String> description) {
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withTypeVersion(java.util.Optional<String> typeVersion) {
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withServiceType(java.util.Optional<String> serviceType) {
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withOptions(java.util.Map<String, String> options) {
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
}