// Note: this is an automatically generated file. Do not edit.

package hydra.ext.datalog.syntax;

import java.io.Serializable;

public class AtomList_Multiple implements Serializable, Comparable<AtomList_Multiple> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.datalog.syntax.AtomList_Multiple");
  
  public static final hydra.core.Name ATOM = new hydra.core.Name("Atom");
  
  public static final hydra.core.Name ATOM_LIST = new hydra.core.Name("AtomList");
  
  public final hydra.ext.datalog.syntax.Atom Atom;
  
  public final hydra.ext.datalog.syntax.AtomList AtomList;
  
  public AtomList_Multiple (hydra.ext.datalog.syntax.Atom Atom, hydra.ext.datalog.syntax.AtomList AtomList) {
    this.Atom = Atom;
    this.AtomList = AtomList;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AtomList_Multiple)) {
      return false;
    }
    AtomList_Multiple o = (AtomList_Multiple) other;
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
  public int compareTo(AtomList_Multiple other) {
    int cmp = 0;
    cmp = ((Comparable) Atom).compareTo(other.Atom);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) AtomList).compareTo(other.AtomList);
  }
  
  public AtomList_Multiple withAtom(hydra.ext.datalog.syntax.Atom Atom) {
    return new AtomList_Multiple(Atom, AtomList);
  }
  
  public AtomList_Multiple withAtomList(hydra.ext.datalog.syntax.AtomList AtomList) {
    return new AtomList_Multiple(Atom, AtomList);
  }
}
