// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class ObjectHasValue implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/w3/owl/syntax.ObjectHasValue");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_INDIVIDUAL = new hydra.core.Name("individual");
  
  public final hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property;
  
  public final hydra.ext.org.w3.owl.syntax.Individual individual;
  
  public ObjectHasValue (hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property, hydra.ext.org.w3.owl.syntax.Individual individual) {
    java.util.Objects.requireNonNull((property));
    java.util.Objects.requireNonNull((individual));
    this.property = property;
    this.individual = individual;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectHasValue)) {
      return false;
    }
    ObjectHasValue o = (ObjectHasValue) (other);
    return property.equals(o.property) && individual.equals(o.individual);
  }
  
  @Override
  public int hashCode() {
    return 2 * property.hashCode() + 3 * individual.hashCode();
  }
  
  public ObjectHasValue withProperty(hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property) {
    java.util.Objects.requireNonNull((property));
    return new ObjectHasValue(property, individual);
  }
  
  public ObjectHasValue withIndividual(hydra.ext.org.w3.owl.syntax.Individual individual) {
    java.util.Objects.requireNonNull((individual));
    return new ObjectHasValue(property, individual);
  }
}