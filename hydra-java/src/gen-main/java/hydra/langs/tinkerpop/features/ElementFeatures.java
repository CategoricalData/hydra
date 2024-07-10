// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.features;

import java.io.Serializable;

/**
 * Features that are related to Element objects.
 */
public class ElementFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/features.ElementFeatures");
  
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
    if (supportsAddProperty == null) {
      throw new IllegalArgumentException("null value for 'supportsAddProperty' argument");
    }
    if (supportsAnyIds == null) {
      throw new IllegalArgumentException("null value for 'supportsAnyIds' argument");
    }
    if (supportsCustomIds == null) {
      throw new IllegalArgumentException("null value for 'supportsCustomIds' argument");
    }
    if (supportsNumericIds == null) {
      throw new IllegalArgumentException("null value for 'supportsNumericIds' argument");
    }
    if (supportsRemoveProperty == null) {
      throw new IllegalArgumentException("null value for 'supportsRemoveProperty' argument");
    }
    if (supportsStringIds == null) {
      throw new IllegalArgumentException("null value for 'supportsStringIds' argument");
    }
    if (supportsUserSuppliedIds == null) {
      throw new IllegalArgumentException("null value for 'supportsUserSuppliedIds' argument");
    }
    if (supportsUuidIds == null) {
      throw new IllegalArgumentException("null value for 'supportsUuidIds' argument");
    }
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
    if (supportsAddProperty == null) {
      throw new IllegalArgumentException("null value for 'supportsAddProperty' argument");
    }
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }
  
  public ElementFeatures withSupportsAnyIds(Boolean supportsAnyIds) {
    if (supportsAnyIds == null) {
      throw new IllegalArgumentException("null value for 'supportsAnyIds' argument");
    }
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }
  
  public ElementFeatures withSupportsCustomIds(Boolean supportsCustomIds) {
    if (supportsCustomIds == null) {
      throw new IllegalArgumentException("null value for 'supportsCustomIds' argument");
    }
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }
  
  public ElementFeatures withSupportsNumericIds(Boolean supportsNumericIds) {
    if (supportsNumericIds == null) {
      throw new IllegalArgumentException("null value for 'supportsNumericIds' argument");
    }
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }
  
  public ElementFeatures withSupportsRemoveProperty(Boolean supportsRemoveProperty) {
    if (supportsRemoveProperty == null) {
      throw new IllegalArgumentException("null value for 'supportsRemoveProperty' argument");
    }
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }
  
  public ElementFeatures withSupportsStringIds(Boolean supportsStringIds) {
    if (supportsStringIds == null) {
      throw new IllegalArgumentException("null value for 'supportsStringIds' argument");
    }
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }
  
  public ElementFeatures withSupportsUserSuppliedIds(Boolean supportsUserSuppliedIds) {
    if (supportsUserSuppliedIds == null) {
      throw new IllegalArgumentException("null value for 'supportsUserSuppliedIds' argument");
    }
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }
  
  public ElementFeatures withSupportsUuidIds(Boolean supportsUuidIds) {
    if (supportsUuidIds == null) {
      throw new IllegalArgumentException("null value for 'supportsUuidIds' argument");
    }
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }
}