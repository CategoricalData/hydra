// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class PropertyExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.PropertyExpression");
  
  public final hydra.langs.cypher.openCypher.Atom atom;
  
  public final java.util.List<hydra.langs.cypher.openCypher.PropertyLookup> lookups;
  
  public PropertyExpression (hydra.langs.cypher.openCypher.Atom atom, java.util.List<hydra.langs.cypher.openCypher.PropertyLookup> lookups) {
    if (atom == null) {
      throw new IllegalArgumentException("null value for 'atom' argument");
    }
    if (lookups == null) {
      throw new IllegalArgumentException("null value for 'lookups' argument");
    }
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
  
  public PropertyExpression withAtom(hydra.langs.cypher.openCypher.Atom atom) {
    if (atom == null) {
      throw new IllegalArgumentException("null value for 'atom' argument");
    }
    return new PropertyExpression(atom, lookups);
  }
  
  public PropertyExpression withLookups(java.util.List<hydra.langs.cypher.openCypher.PropertyLookup> lookups) {
    if (lookups == null) {
      throw new IllegalArgumentException("null value for 'lookups' argument");
    }
    return new PropertyExpression(atom, lookups);
  }
}