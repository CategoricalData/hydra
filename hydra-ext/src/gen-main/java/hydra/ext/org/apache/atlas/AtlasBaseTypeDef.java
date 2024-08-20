// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.atlas;

import java.io.Serializable;

/**
 * Base class that captures common-attributes for all Atlas types.
 */
public class AtlasBaseTypeDef implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/atlas.AtlasBaseTypeDef");
  
  public static final hydra.core.Name FIELD_NAME_CATEGORY = new hydra.core.Name("category");
  
  public static final hydra.core.Name FIELD_NAME_GUID = new hydra.core.Name("guid");
  
  public static final hydra.core.Name FIELD_NAME_CREATED_BY = new hydra.core.Name("createdBy");
  
  public static final hydra.core.Name FIELD_NAME_UPDATED_BY = new hydra.core.Name("updatedBy");
  
  public static final hydra.core.Name FIELD_NAME_CREATE_TIME = new hydra.core.Name("createTime");
  
  public static final hydra.core.Name FIELD_NAME_UPDATE_TIME = new hydra.core.Name("updateTime");
  
  public static final hydra.core.Name FIELD_NAME_VERSION = new hydra.core.Name("version");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_VERSION = new hydra.core.Name("typeVersion");
  
  public static final hydra.core.Name FIELD_NAME_SERVICE_TYPE = new hydra.core.Name("serviceType");
  
  public static final hydra.core.Name FIELD_NAME_OPTIONS = new hydra.core.Name("options");
  
  public final hydra.util.Opt<hydra.ext.org.apache.atlas.TypeCategory> category;
  
  public final hydra.util.Opt<String> guid;
  
  public final hydra.util.Opt<String> createdBy;
  
  public final hydra.util.Opt<String> updatedBy;
  
  public final hydra.util.Opt<hydra.ext.org.w3.xml.schema.DateTime> createTime;
  
  public final hydra.util.Opt<hydra.ext.org.w3.xml.schema.DateTime> updateTime;
  
  public final hydra.util.Opt<Long> version;
  
  public final hydra.util.Opt<String> name;
  
  public final hydra.util.Opt<String> description;
  
  public final hydra.util.Opt<String> typeVersion;
  
  public final hydra.util.Opt<String> serviceType;
  
  public final java.util.Map<String, String> options;
  
  public AtlasBaseTypeDef (hydra.util.Opt<hydra.ext.org.apache.atlas.TypeCategory> category, hydra.util.Opt<String> guid, hydra.util.Opt<String> createdBy, hydra.util.Opt<String> updatedBy, hydra.util.Opt<hydra.ext.org.w3.xml.schema.DateTime> createTime, hydra.util.Opt<hydra.ext.org.w3.xml.schema.DateTime> updateTime, hydra.util.Opt<Long> version, hydra.util.Opt<String> name, hydra.util.Opt<String> description, hydra.util.Opt<String> typeVersion, hydra.util.Opt<String> serviceType, java.util.Map<String, String> options) {
    java.util.Objects.requireNonNull((category));
    java.util.Objects.requireNonNull((guid));
    java.util.Objects.requireNonNull((createdBy));
    java.util.Objects.requireNonNull((updatedBy));
    java.util.Objects.requireNonNull((createTime));
    java.util.Objects.requireNonNull((updateTime));
    java.util.Objects.requireNonNull((version));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((typeVersion));
    java.util.Objects.requireNonNull((serviceType));
    java.util.Objects.requireNonNull((options));
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
  
  public AtlasBaseTypeDef withCategory(hydra.util.Opt<hydra.ext.org.apache.atlas.TypeCategory> category) {
    java.util.Objects.requireNonNull((category));
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withGuid(hydra.util.Opt<String> guid) {
    java.util.Objects.requireNonNull((guid));
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withCreatedBy(hydra.util.Opt<String> createdBy) {
    java.util.Objects.requireNonNull((createdBy));
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withUpdatedBy(hydra.util.Opt<String> updatedBy) {
    java.util.Objects.requireNonNull((updatedBy));
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withCreateTime(hydra.util.Opt<hydra.ext.org.w3.xml.schema.DateTime> createTime) {
    java.util.Objects.requireNonNull((createTime));
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withUpdateTime(hydra.util.Opt<hydra.ext.org.w3.xml.schema.DateTime> updateTime) {
    java.util.Objects.requireNonNull((updateTime));
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withVersion(hydra.util.Opt<Long> version) {
    java.util.Objects.requireNonNull((version));
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withName(hydra.util.Opt<String> name) {
    java.util.Objects.requireNonNull((name));
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withTypeVersion(hydra.util.Opt<String> typeVersion) {
    java.util.Objects.requireNonNull((typeVersion));
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withServiceType(hydra.util.Opt<String> serviceType) {
    java.util.Objects.requireNonNull((serviceType));
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
  
  public AtlasBaseTypeDef withOptions(java.util.Map<String, String> options) {
    java.util.Objects.requireNonNull((options));
    return new AtlasBaseTypeDef(category, guid, createdBy, updatedBy, createTime, updateTime, version, name, description, typeVersion, serviceType, options);
  }
}