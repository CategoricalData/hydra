// Note: this is an automatically generated file. Do not edit.

package hydra.shacl.model;

import java.io.Serializable;

/**
 * A SHACL property shape. See https://www.w3.org/TR/shacl/#property-shapes
 */
public class PropertyShape implements Serializable, Comparable<PropertyShape> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shacl.model.PropertyShape");

  public static final hydra.core.Name COMMON = new hydra.core.Name("common");

  public static final hydra.core.Name CONSTRAINTS = new hydra.core.Name("constraints");

  public static final hydra.core.Name DEFAULT_VALUE = new hydra.core.Name("defaultValue");

  public static final hydra.core.Name DESCRIPTION = new hydra.core.Name("description");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name ORDER = new hydra.core.Name("order");

  public static final hydra.core.Name PATH = new hydra.core.Name("path");

  public final hydra.shacl.model.CommonProperties common;

  /**
   * Any property shape -specific constraint parameters
   */
  public final java.util.Set<hydra.shacl.model.PropertyShapeConstraint> constraints;

  /**
   * See https://www.w3.org/TR/shacl/#defaultValue
   */
  public final hydra.util.Maybe<hydra.rdf.syntax.Node> defaultValue;

  /**
   * See https://www.w3.org/TR/shacl/#name
   */
  public final hydra.rdf.syntax.LangStrings description;

  /**
   * See https://www.w3.org/TR/shacl/#name
   */
  public final hydra.rdf.syntax.LangStrings name;

  /**
   * See https://www.w3.org/TR/shacl/#order
   */
  public final hydra.util.Maybe<java.math.BigInteger> order;

  public final hydra.rdf.syntax.Iri path;

  public PropertyShape (hydra.shacl.model.CommonProperties common, java.util.Set<hydra.shacl.model.PropertyShapeConstraint> constraints, hydra.util.Maybe<hydra.rdf.syntax.Node> defaultValue, hydra.rdf.syntax.LangStrings description, hydra.rdf.syntax.LangStrings name, hydra.util.Maybe<java.math.BigInteger> order, hydra.rdf.syntax.Iri path) {
    this.common = common;
    this.constraints = constraints;
    this.defaultValue = defaultValue;
    this.description = description;
    this.name = name;
    this.order = order;
    this.path = path;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyShape)) {
      return false;
    }
    PropertyShape o = (PropertyShape) other;
    return java.util.Objects.equals(
      this.common,
      o.common) && java.util.Objects.equals(
      this.constraints,
      o.constraints) && java.util.Objects.equals(
      this.defaultValue,
      o.defaultValue) && java.util.Objects.equals(
      this.description,
      o.description) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.order,
      o.order) && java.util.Objects.equals(
      this.path,
      o.path);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(common) + 3 * java.util.Objects.hashCode(constraints) + 5 * java.util.Objects.hashCode(defaultValue) + 7 * java.util.Objects.hashCode(description) + 11 * java.util.Objects.hashCode(name) + 13 * java.util.Objects.hashCode(order) + 17 * java.util.Objects.hashCode(path);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PropertyShape other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      common,
      other.common);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      constraints,
      other.constraints);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      defaultValue,
      other.defaultValue);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      description,
      other.description);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      order,
      other.order);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      path,
      other.path);
  }

  public PropertyShape withCommon(hydra.shacl.model.CommonProperties common) {
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }

  public PropertyShape withConstraints(java.util.Set<hydra.shacl.model.PropertyShapeConstraint> constraints) {
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }

  public PropertyShape withDefaultValue(hydra.util.Maybe<hydra.rdf.syntax.Node> defaultValue) {
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }

  public PropertyShape withDescription(hydra.rdf.syntax.LangStrings description) {
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }

  public PropertyShape withName(hydra.rdf.syntax.LangStrings name) {
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }

  public PropertyShape withOrder(hydra.util.Maybe<java.math.BigInteger> order) {
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }

  public PropertyShape withPath(hydra.rdf.syntax.Iri path) {
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }
}
