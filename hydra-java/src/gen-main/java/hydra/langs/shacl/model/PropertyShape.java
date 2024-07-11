// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shacl.model;

import java.io.Serializable;

/**
 * A SHACL property shape. See https://www.w3.org/TR/shacl/#property-shapes
 */
public class PropertyShape implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shacl/model.PropertyShape");
  
  public final hydra.langs.shacl.model.CommonProperties common;
  
  /**
   * Any property shape -specific constraint parameters
   */
  public final java.util.Set<hydra.langs.shacl.model.PropertyShapeConstraint> constraints;
  
  /**
   * See https://www.w3.org/TR/shacl/#defaultValue
   */
  public final hydra.util.Opt<hydra.langs.rdf.syntax.Node> defaultValue;
  
  /**
   * See https://www.w3.org/TR/shacl/#name
   */
  public final hydra.langs.rdf.syntax.LangStrings description;
  
  /**
   * See https://www.w3.org/TR/shacl/#name
   */
  public final hydra.langs.rdf.syntax.LangStrings name;
  
  /**
   * See https://www.w3.org/TR/shacl/#order
   */
  public final hydra.util.Opt<java.math.BigInteger> order;
  
  public final hydra.langs.rdf.syntax.Iri path;
  
  public PropertyShape (hydra.langs.shacl.model.CommonProperties common, java.util.Set<hydra.langs.shacl.model.PropertyShapeConstraint> constraints, hydra.util.Opt<hydra.langs.rdf.syntax.Node> defaultValue, hydra.langs.rdf.syntax.LangStrings description, hydra.langs.rdf.syntax.LangStrings name, hydra.util.Opt<java.math.BigInteger> order, hydra.langs.rdf.syntax.Iri path) {
    if (common == null) {
      throw new IllegalArgumentException("null value for 'common' argument");
    }
    if (constraints == null) {
      throw new IllegalArgumentException("null value for 'constraints' argument");
    }
    if (defaultValue == null) {
      throw new IllegalArgumentException("null value for 'defaultValue' argument");
    }
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (order == null) {
      throw new IllegalArgumentException("null value for 'order' argument");
    }
    if (path == null) {
      throw new IllegalArgumentException("null value for 'path' argument");
    }
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
    PropertyShape o = (PropertyShape) (other);
    return common.equals(o.common) && constraints.equals(o.constraints) && defaultValue.equals(o.defaultValue) && description.equals(o.description) && name.equals(o.name) && order.equals(o.order) && path.equals(o.path);
  }
  
  @Override
  public int hashCode() {
    return 2 * common.hashCode() + 3 * constraints.hashCode() + 5 * defaultValue.hashCode() + 7 * description.hashCode() + 11 * name.hashCode() + 13 * order.hashCode() + 17 * path.hashCode();
  }
  
  public PropertyShape withCommon(hydra.langs.shacl.model.CommonProperties common) {
    if (common == null) {
      throw new IllegalArgumentException("null value for 'common' argument");
    }
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }
  
  public PropertyShape withConstraints(java.util.Set<hydra.langs.shacl.model.PropertyShapeConstraint> constraints) {
    if (constraints == null) {
      throw new IllegalArgumentException("null value for 'constraints' argument");
    }
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }
  
  public PropertyShape withDefaultValue(hydra.util.Opt<hydra.langs.rdf.syntax.Node> defaultValue) {
    if (defaultValue == null) {
      throw new IllegalArgumentException("null value for 'defaultValue' argument");
    }
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }
  
  public PropertyShape withDescription(hydra.langs.rdf.syntax.LangStrings description) {
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }
  
  public PropertyShape withName(hydra.langs.rdf.syntax.LangStrings name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }
  
  public PropertyShape withOrder(hydra.util.Opt<java.math.BigInteger> order) {
    if (order == null) {
      throw new IllegalArgumentException("null value for 'order' argument");
    }
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }
  
  public PropertyShape withPath(hydra.langs.rdf.syntax.Iri path) {
    if (path == null) {
      throw new IllegalArgumentException("null value for 'path' argument");
    }
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }
}