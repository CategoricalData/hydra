// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphql.syntax;

import java.io.Serializable;

public class ObjectField implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphql.syntax.ObjectField");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.org.graphql.syntax.Name name;
  
  public final hydra.ext.org.graphql.syntax.Value value;
  
  public ObjectField (hydra.ext.org.graphql.syntax.Name name, hydra.ext.org.graphql.syntax.Value value) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((value));
    this.name = name;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectField)) {
      return false;
    }
    ObjectField o = (ObjectField) (other);
    return name.equals(o.name) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * value.hashCode();
  }
  
  public ObjectField withName(hydra.ext.org.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new ObjectField(name, value);
  }
  
  public ObjectField withValue(hydra.ext.org.graphql.syntax.Value value) {
    java.util.Objects.requireNonNull((value));
    return new ObjectField(name, value);
  }
}