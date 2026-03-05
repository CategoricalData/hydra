// Note: this is an automatically generated file. Do not edit.

package hydra.ext.datalog.syntax;

import java.io.Serializable;

public class Rule implements Serializable, Comparable<Rule> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.datalog.syntax.Rule");
  
  public static final hydra.core.Name ATOM = new hydra.core.Name("Atom");
  
  public static final hydra.core.Name ATOM_LIST = new hydra.core.Name("AtomList");
  
  public final hydra.ext.datalog.syntax.Atom Atom;
  
  public final hydra.ext.datalog.syntax.AtomList AtomList;
  
  public Rule (hydra.ext.datalog.syntax.Atom Atom, hydra.ext.datalog.syntax.AtomList AtomList) {
    this.Atom = Atom;
    this.AtomList = AtomList;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Rule)) {
      return false;
    }
    Rule o = (Rule) other;
    return java.util.Objects.equals(
      this.Atom,
      o.Atom) && java.util.Objects.equals(
      this.AtomList,
      o.AtomList);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(Atom) + 3 * java.util.Objects.hashCode(AtomList);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Rule other) {
    int cmp = 0;
    cmp = ((Comparable) Atom).compareTo(other.Atom);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) AtomList).compareTo(other.AtomList);
  }
  
  public Rule withAtom(hydra.ext.datalog.syntax.Atom Atom) {
    return new Rule(Atom, AtomList);
  }
  
  public Rule withAtomList(hydra.ext.datalog.syntax.AtomList AtomList) {
    return new Rule(Atom, AtomList);
  }
}
