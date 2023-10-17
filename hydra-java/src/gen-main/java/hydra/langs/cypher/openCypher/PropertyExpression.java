package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class PropertyExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.PropertyExpression");
  
  public final hydra.langs.cypher.openCypher.Atom atom;
  
  public final java.util.List<hydra.langs.cypher.openCypher.PropertyKeyName> lookups;
  
  public PropertyExpression (hydra.langs.cypher.openCypher.Atom atom, java.util.List<hydra.langs.cypher.openCypher.PropertyKeyName> lookups) {
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
    return new PropertyExpression(atom, lookups);
  }
  
  public PropertyExpression withLookups(java.util.List<hydra.langs.cypher.openCypher.PropertyKeyName> lookups) {
    return new PropertyExpression(atom, lookups);
  }
}