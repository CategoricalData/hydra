// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shacl.model;

import java.io.Serializable;

/**
 * A SHACL property shape. See https://www.w3.org/TR/shacl/#property-shapes
 */
public class PropertyShape implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shacl/model.PropertyShape");
  
  public static final hydra.core.Name FIELD_NAME_COMMON = new hydra.core.Name("common");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRAINTS = new hydra.core.Name("constraints");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT_VALUE = new hydra.core.Name("defaultValue");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ORDER = new hydra.core.Name("order");
  
  public static final hydra.core.Name FIELD_NAME_PATH = new hydra.core.Name("path");
  
  public final hydra.ext.shacl.model.CommonProperties common;
  
  /**
   * Any property shape -specific constraint parameters
   */
  public final java.util.Set<hydra.ext.shacl.model.PropertyShapeConstraint> constraints;
  
  /**
   * See https://www.w3.org/TR/shacl/#defaultValue
   */
  public final hydra.util.Opt<hydra.ext.rdf.syntax.Node> defaultValue;
  
  /**
   * See https://www.w3.org/TR/shacl/#name
   */
  public final hydra.ext.rdf.syntax.LangStrings description;
  
  /**
   * See https://www.w3.org/TR/shacl/#name
   */
  public final hydra.ext.rdf.syntax.LangStrings name;
  
  /**
   * See https://www.w3.org/TR/shacl/#order
   */
  public final hydra.util.Opt<java.math.BigInteger> order;
  
  public final hydra.ext.rdf.syntax.Iri path;
  
  public PropertyShape (hydra.ext.shacl.model.CommonProperties common, java.util.Set<hydra.ext.shacl.model.PropertyShapeConstraint> constraints, hydra.util.Opt<hydra.ext.rdf.syntax.Node> defaultValue, hydra.ext.rdf.syntax.LangStrings description, hydra.ext.rdf.syntax.LangStrings name, hydra.util.Opt<java.math.BigInteger> order, hydra.ext.rdf.syntax.Iri path) {
    java.util.Objects.requireNonNull((common));
    java.util.Objects.requireNonNull((constraints));
    java.util.Objects.requireNonNull((defaultValue));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((order));
    java.util.Objects.requireNonNull((path));
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
  
  public PropertyShape withCommon(hydra.ext.shacl.model.CommonProperties common) {
    java.util.Objects.requireNonNull((common));
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }
  
  public PropertyShape withConstraints(java.util.Set<hydra.ext.shacl.model.PropertyShapeConstraint> constraints) {
    java.util.Objects.requireNonNull((constraints));
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }
  
  public PropertyShape withDefaultValue(hydra.util.Opt<hydra.ext.rdf.syntax.Node> defaultValue) {
    java.util.Objects.requireNonNull((defaultValue));
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }
  
  public PropertyShape withDescription(hydra.ext.rdf.syntax.LangStrings description) {
    java.util.Objects.requireNonNull((description));
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }
  
  public PropertyShape withName(hydra.ext.rdf.syntax.LangStrings name) {
    java.util.Objects.requireNonNull((name));
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }
  
  public PropertyShape withOrder(hydra.util.Opt<java.math.BigInteger> order) {
    java.util.Objects.requireNonNull((order));
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }
  
  public PropertyShape withPath(hydra.ext.rdf.syntax.Iri path) {
    java.util.Objects.requireNonNull((path));
    return new PropertyShape(common, constraints, defaultValue, description, name, order, path);
  }
}
