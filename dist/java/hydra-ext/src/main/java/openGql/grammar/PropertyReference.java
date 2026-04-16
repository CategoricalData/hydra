// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class PropertyReference implements Serializable, Comparable<PropertyReference> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PropertyReference");

  public static final hydra.core.Name VALUE_EXPRESSION = new hydra.core.Name("valueExpression");

  public static final hydra.core.Name PROPERTY_NAME = new hydra.core.Name("propertyName");

  public final openGql.grammar.PrimaryValueExpression valueExpression;

  public final String propertyName;

  public PropertyReference (openGql.grammar.PrimaryValueExpression valueExpression, String propertyName) {
    this.valueExpression = valueExpression;
    this.propertyName = propertyName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyReference)) {
      return false;
    }
    PropertyReference o = (PropertyReference) other;
    return java.util.Objects.equals(
      this.valueExpression,
      o.valueExpression) && java.util.Objects.equals(
      this.propertyName,
      o.propertyName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(valueExpression) + 3 * java.util.Objects.hashCode(propertyName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PropertyReference other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      valueExpression,
      other.valueExpression);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      propertyName,
      other.propertyName);
  }

  public PropertyReference withValueExpression(openGql.grammar.PrimaryValueExpression valueExpression) {
    return new PropertyReference(valueExpression, propertyName);
  }

  public PropertyReference withPropertyName(String propertyName) {
    return new PropertyReference(valueExpression, propertyName);
  }
}
