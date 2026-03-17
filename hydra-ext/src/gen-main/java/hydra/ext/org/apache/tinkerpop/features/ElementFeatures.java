// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.features;

import java.io.Serializable;

/**
 * Features that are related to Element objects.
 */
public class ElementFeatures implements Serializable, Comparable<ElementFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.features.ElementFeatures");

  public static final hydra.core.Name SUPPORTS_ADD_PROPERTY = new hydra.core.Name("supportsAddProperty");

  public static final hydra.core.Name SUPPORTS_ANY_IDS = new hydra.core.Name("supportsAnyIds");

  public static final hydra.core.Name SUPPORTS_CUSTOM_IDS = new hydra.core.Name("supportsCustomIds");

  public static final hydra.core.Name SUPPORTS_NUMERIC_IDS = new hydra.core.Name("supportsNumericIds");

  public static final hydra.core.Name SUPPORTS_REMOVE_PROPERTY = new hydra.core.Name("supportsRemoveProperty");

  public static final hydra.core.Name SUPPORTS_STRING_IDS = new hydra.core.Name("supportsStringIds");

  public static final hydra.core.Name SUPPORTS_USER_SUPPLIED_IDS = new hydra.core.Name("supportsUserSuppliedIds");

  public static final hydra.core.Name SUPPORTS_UUID_IDS = new hydra.core.Name("supportsUuidIds");

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
    ElementFeatures o = (ElementFeatures) other;
    return java.util.Objects.equals(
      this.supportsAddProperty,
      o.supportsAddProperty) && java.util.Objects.equals(
      this.supportsAnyIds,
      o.supportsAnyIds) && java.util.Objects.equals(
      this.supportsCustomIds,
      o.supportsCustomIds) && java.util.Objects.equals(
      this.supportsNumericIds,
      o.supportsNumericIds) && java.util.Objects.equals(
      this.supportsRemoveProperty,
      o.supportsRemoveProperty) && java.util.Objects.equals(
      this.supportsStringIds,
      o.supportsStringIds) && java.util.Objects.equals(
      this.supportsUserSuppliedIds,
      o.supportsUserSuppliedIds) && java.util.Objects.equals(
      this.supportsUuidIds,
      o.supportsUuidIds);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(supportsAddProperty) + 3 * java.util.Objects.hashCode(supportsAnyIds) + 5 * java.util.Objects.hashCode(supportsCustomIds) + 7 * java.util.Objects.hashCode(supportsNumericIds) + 11 * java.util.Objects.hashCode(supportsRemoveProperty) + 13 * java.util.Objects.hashCode(supportsStringIds) + 17 * java.util.Objects.hashCode(supportsUserSuppliedIds) + 19 * java.util.Objects.hashCode(supportsUuidIds);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ElementFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) supportsAddProperty).compareTo(other.supportsAddProperty);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) supportsAnyIds).compareTo(other.supportsAnyIds);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) supportsCustomIds).compareTo(other.supportsCustomIds);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) supportsNumericIds).compareTo(other.supportsNumericIds);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) supportsRemoveProperty).compareTo(other.supportsRemoveProperty);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) supportsStringIds).compareTo(other.supportsStringIds);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) supportsUserSuppliedIds).compareTo(other.supportsUserSuppliedIds);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) supportsUuidIds).compareTo(other.supportsUuidIds);
  }

  public ElementFeatures withSupportsAddProperty(Boolean supportsAddProperty) {
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }

  public ElementFeatures withSupportsAnyIds(Boolean supportsAnyIds) {
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }

  public ElementFeatures withSupportsCustomIds(Boolean supportsCustomIds) {
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }

  public ElementFeatures withSupportsNumericIds(Boolean supportsNumericIds) {
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }

  public ElementFeatures withSupportsRemoveProperty(Boolean supportsRemoveProperty) {
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }

  public ElementFeatures withSupportsStringIds(Boolean supportsStringIds) {
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }

  public ElementFeatures withSupportsUserSuppliedIds(Boolean supportsUserSuppliedIds) {
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }

  public ElementFeatures withSupportsUuidIds(Boolean supportsUuidIds) {
    return new ElementFeatures(supportsAddProperty, supportsAnyIds, supportsCustomIds, supportsNumericIds, supportsRemoveProperty, supportsStringIds, supportsUserSuppliedIds, supportsUuidIds);
  }
}
