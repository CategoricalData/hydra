// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class ObjectHasValue implements Serializable, Comparable<ObjectHasValue> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectHasValue");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name INDIVIDUAL = new hydra.core.Name("individual");

  public final hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property;

  public final hydra.ext.org.w3.owl.syntax.Individual individual;

  public ObjectHasValue (hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property, hydra.ext.org.w3.owl.syntax.Individual individual) {
    this.property = property;
    this.individual = individual;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectHasValue)) {
      return false;
    }
    ObjectHasValue o = (ObjectHasValue) other;
    return java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.individual,
      o.individual);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(property) + 3 * java.util.Objects.hashCode(individual);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ObjectHasValue other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      property,
      other.property);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      individual,
      other.individual);
  }

  public ObjectHasValue withProperty(hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property) {
    return new ObjectHasValue(property, individual);
  }

  public ObjectHasValue withIndividual(hydra.ext.org.w3.owl.syntax.Individual individual) {
    return new ObjectHasValue(property, individual);
  }
}
