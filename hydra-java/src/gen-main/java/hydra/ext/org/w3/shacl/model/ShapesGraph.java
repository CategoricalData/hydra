// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.shacl.model;

import java.io.Serializable;

/**
 * An RDF graph containing zero or more shapes that is passed into a SHACL validation process so that a data graph can be validated against the shapes
 */
public class ShapesGraph implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.shacl.model.ShapesGraph");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.Set<hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>> value;
  
  public ShapesGraph (java.util.Set<hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapesGraph)) {
      return false;
    }
    ShapesGraph o = (ShapesGraph) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}