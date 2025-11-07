// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class PropertyExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.PropertyExpression");
  
  public static final hydra.core.Name FIELD_NAME_ATOM = new hydra.core.Name("atom");
  
  public static final hydra.core.Name FIELD_NAME_LOOKUPS = new hydra.core.Name("lookups");
  
  public final hydra.ext.cypher.openCypher.Atom atom;
  
  public final java.util.List<hydra.ext.cypher.openCypher.PropertyLookup> lookups;
  
  public PropertyExpression (hydra.ext.cypher.openCypher.Atom atom, java.util.List<hydra.ext.cypher.openCypher.PropertyLookup> lookups) {
    java.util.Objects.requireNonNull((atom));
    java.util.Objects.requireNonNull((lookups));
    this.atom = atom;
    this.lookups = lookups;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyExpression)) {
      return false;
    }
    PropertyExpression o = (PropertyExpression) (other);
    return atom.equals(o.atom) && lookups.equals(o.lookups);
  }
  
  @Override
  public int hashCode() {
    return 2 * atom.hashCode() + 3 * lookups.hashCode();
  }
  
  public PropertyExpression withAtom(hydra.ext.cypher.openCypher.Atom atom) {
    java.util.Objects.requireNonNull((atom));
    return new PropertyExpression(atom, lookups);
  }
  
  public PropertyExpression withLookups(java.util.List<hydra.ext.cypher.openCypher.PropertyLookup> lookups) {
    java.util.Objects.requireNonNull((lookups));
    return new PropertyExpression(atom, lookups);
  }
}
