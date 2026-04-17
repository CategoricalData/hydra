// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class RemovePropertyItem implements Serializable, Comparable<RemovePropertyItem> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.RemovePropertyItem");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public static final hydra.core.Name PROPERTY_NAME = new hydra.core.Name("propertyName");

  public final String variable;

  public final String propertyName;

  public RemovePropertyItem (String variable, String propertyName) {
    this.variable = variable;
    this.propertyName = propertyName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RemovePropertyItem)) {
      return false;
    }
    RemovePropertyItem o = (RemovePropertyItem) other;
    return java.util.Objects.equals(
      this.variable,
      o.variable) && java.util.Objects.equals(
      this.propertyName,
      o.propertyName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variable) + 3 * java.util.Objects.hashCode(propertyName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RemovePropertyItem other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      variable,
      other.variable);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      propertyName,
      other.propertyName);
  }

  public RemovePropertyItem withVariable(String variable) {
    return new RemovePropertyItem(variable, propertyName);
  }

  public RemovePropertyItem withPropertyName(String propertyName) {
    return new RemovePropertyItem(variable, propertyName);
  }
}
