// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.features;

import java.io.Serializable;

/**
 * Features that are related to Element objects.
 */
public class ElementFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.features.ElementFeatures");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_ADD_PROPERTY = new hydra.core.Name("supportsAddProperty");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_ANY_IDS = new hydra.core.Name("supportsAnyIds");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_CUSTOM_IDS = new hydra.core.Name("supportsCustomIds");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_NUMERIC_IDS = new hydra.core.Name("supportsNumericIds");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_REMOVE_PROPERTY = new hydra.core.Name("supportsRemoveProperty");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_STRING_IDS = new hydra.core.Name("supportsStringIds");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_USER_SUPPLIED_IDS = new hydra.core.Name("supportsUserSuppliedIds");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_UUID_IDS = new hydra.core.Name("supportsUuidIds");
  
  /**
   * Determines if an Element allows properties to be added.
   */
  public final Boolean supportsAddProperty;
  
  /**
   * Determines if an Element any Java object is a suitable identifier.
   */
  public final Boolean supportsAnyIds;
  
  /**
   * Determines if an Element has a specific custom object as their internal representation.
   */
  public final Boolean supportsCustomIds;
  
  /**
   * Determines if an Element has numeric identifiers as their internal representation.
   */
  public final Boolean supportsNumericIds;
  
  /**
   * Determines if an Element allows properties to be removed.
   */
  public final Boolean supportsRemoveProperty;
  
  /**
   * Determines if an Element has string identifiers as their internal representation.
   */
  public final Boolean supportsStringIds;
  
  /**
   * Determines if an Element can have a user defined identifier.
   */
  public final Boolean supportsUserSuppliedIds;
  
  /**
   * Determines if an Element has UUID identifiers as their internal representation.
   */
  public final Boolean supportsUuidIds;
  
  public ElementFeatures (Boolean supportsAddProperty, Boolean supportsAnyIds, Boolean supportsCustomIds, Boolean supportsNumericIds, Boolean supportsRemoveProperty, Boolean supportsStringIds, Boolean supportsUserSuppliedIds, Boolean supportsUuidIds) {
    java.util.Objects.requireNonNull((supportsAddProperty));
    java.util.Objects.requireNonNull((supportsAnyIds));
    java.util.Objects.requireNonNull((supportsCustomIds));
    java.util.Objects.requireNonNull((supportsNumericIds));
    java.util.Objects.requireNonNull((supportsRemoveProperty));
    java.util.Objects.requireNonNull((supportsStringIds));
    java.util.Objects.requireNonNull((supportsUserSuppliedIds));
    java.util.Objects.requireNonNull((supportsUuidIds));
    this.supportsAddProperty = supportsAddProperty;
    this.supportsAnyIds = supportsAnyIds;
    this.supportsCustomIds = supportsCustomIds;
    this.supportsNumericIds = supportsNumericIds;
    this.supportsRemoveProperty = supportsRemoveProperty;
    this.supportsStringIds = supportsStringIds;
    this.supportsUserSuppliedIds = supportsUserSuppliedIds;
    this.supportsUuidIds = supportsUuidIds;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ElementFeatures)) {
      return false;
    }
    ElementFeatures o = (ElementFeatures) (other);
    return supportsAddProperty.equals(o.supportsAddProperty) && supportsAnyIds.equals(o.supportsAnyIds) && supportsCustomIds.equals(o.supportsCustomIds) && supportsNumericIds.equals(o.supportsNumericIds) && supportsRemoveProperty.equals(o.supportsRemoveProperty) && supportsStringIds.equals(o.supportsStringIds) && supportsUserSuppliedIds.equals(o.supportsUserSuppliedIds) && supportsUuidIds.equals(o.supportsUuidIds);
  }
  
  @Override
  public int hashCode() {
    return 2 * supportsAddProperty.hashCode() + 3 * supportsAnyIds.hashCode() + 5 * supportsCustomIds.hashCode() + 7 * supportsNumericIds.hashCode() + 11 * supportsRemoveProperty.hashCode() + 13 * supportsStringIds.hashCode() + 17 * supportsUserSuppliedIds.hashCode() + 19 * supportsUuidIds.hashCode();
  }
  
  public ElementFeatures withSupportsAddProperty(Boolean supportsAddProperty) {
    java.util.Objects.requireNonNull((supportsAddProperty));
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }
  
  public ElementFeatures withSupportsAnyIds(Boolean supportsAnyIds) {
    java.util.Objects.requireNonNull((supportsAnyIds));
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }
  
  public ElementFeatures withSupportsCustomIds(Boolean supportsCustomIds) {
    java.util.Objects.requireNonNull((supportsCustomIds));
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }
  
  public ElementFeatures withSupportsNumericIds(Boolean supportsNumericIds) {
    java.util.Objects.requireNonNull((supportsNumericIds));
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }
  
  public ElementFeatures withSupportsRemoveProperty(Boolean supportsRemoveProperty) {
    java.util.Objects.requireNonNull((supportsRemoveProperty));
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }
  
  public ElementFeatures withSupportsStringIds(Boolean supportsStringIds) {
    java.util.Objects.requireNonNull((supportsStringIds));
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }
  
  public ElementFeatures withSupportsUserSuppliedIds(Boolean supportsUserSuppliedIds) {
    java.util.Objects.requireNonNull((supportsUserSuppliedIds));
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }
  
  public ElementFeatures withSupportsUuidIds(Boolean supportsUuidIds) {
    java.util.Objects.requireNonNull((supportsUuidIds));
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }
}