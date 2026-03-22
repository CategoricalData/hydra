// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class PropertyExpression implements Serializable, Comparable<PropertyExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.PropertyExpression");

  public static final hydra.core.Name ATOM = new hydra.core.Name("atom");

  public static final hydra.core.Name LOOKUPS = new hydra.core.Name("lookups");

  public final hydra.ext.cypher.openCypher.Atom atom;

  public final hydra.util.ConsList<hydra.ext.cypher.openCypher.PropertyLookup> lookups;

  public PropertyExpression (hydra.ext.cypher.openCypher.Atom atom, hydra.util.ConsList<hydra.ext.cypher.openCypher.PropertyLookup> lookups) {
    this.atom = atom;
    this.lookups = lookups;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyExpression)) {
      return false;
    }
    PropertyExpression o = (PropertyExpression) other;
    return java.util.Objects.equals(
      this.atom,
      o.atom) && java.util.Objects.equals(
      this.lookups,
      o.lookups);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(atom) + 3 * java.util.Objects.hashCode(lookups);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PropertyExpression other) {
    int cmp = 0;
    cmp = ((Comparable) atom).compareTo(other.atom);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) lookups).compareTo(other.lookups);
  }

  public PropertyExpression withAtom(hydra.ext.cypher.openCypher.Atom atom) {
    return new PropertyExpression(atom, lookups);
  }

  public PropertyExpression withLookups(hydra.util.ConsList<hydra.ext.cypher.openCypher.PropertyLookup> lookups) {
    return new PropertyExpression(atom, lookups);
  }
}
