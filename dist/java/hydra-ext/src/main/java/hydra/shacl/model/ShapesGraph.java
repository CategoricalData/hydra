// Note: this is an automatically generated file. Do not edit.

package hydra.shacl.model;

import java.io.Serializable;

/**
 * An RDF graph containing zero or more shapes that is passed into a SHACL validation process so that a data graph can be validated against the shapes
 */
public class ShapesGraph implements Serializable, Comparable<ShapesGraph> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shacl.model.ShapesGraph");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.Set<hydra.shacl.model.Definition<hydra.shacl.model.Shape>> value;

  public ShapesGraph (java.util.Set<hydra.shacl.model.Definition<hydra.shacl.model.Shape>> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShapesGraph)) {
      return false;
    }
    ShapesGraph o = (ShapesGraph) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ShapesGraph other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
