// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class PropertyExistsPredicate implements Serializable, Comparable<PropertyExistsPredicate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PropertyExistsPredicate");

  public static final hydra.core.Name ELEMENT_VARIABLE_REFERENCE = new hydra.core.Name("elementVariableReference");

  public static final hydra.core.Name PROPERTY_NAME = new hydra.core.Name("propertyName");

  public final String elementVariableReference;

  public final String propertyName;

  public PropertyExistsPredicate (String elementVariableReference, String propertyName) {
    this.elementVariableReference = elementVariableReference;
    this.propertyName = propertyName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyExistsPredicate)) {
      return false;
    }
    PropertyExistsPredicate o = (PropertyExistsPredicate) other;
    return java.util.Objects.equals(
      this.elementVariableReference,
      o.elementVariableReference) && java.util.Objects.equals(
      this.propertyName,
      o.propertyName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(elementVariableReference) + 3 * java.util.Objects.hashCode(propertyName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PropertyExistsPredicate other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      elementVariableReference,
      other.elementVariableReference);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      propertyName,
      other.propertyName);
  }

  public PropertyExistsPredicate withElementVariableReference(String elementVariableReference) {
    return new PropertyExistsPredicate(elementVariableReference, propertyName);
  }

  public PropertyExistsPredicate withPropertyName(String propertyName) {
    return new PropertyExistsPredicate(elementVariableReference, propertyName);
  }
}
