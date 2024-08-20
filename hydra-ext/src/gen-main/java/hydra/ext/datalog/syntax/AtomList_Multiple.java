// Note: this is an automatically generated file. Do not edit.

package hydra.ext.datalog.syntax;

import java.io.Serializable;

public class AtomList_Multiple implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/datalog/syntax.AtomList.Multiple");
  
  public static final hydra.core.Name FIELD_NAME_ATOM = new hydra.core.Name("atom");
  
  public static final hydra.core.Name FIELD_NAME_ATOM_LIST = new hydra.core.Name("atomList");
  
  public final hydra.ext.datalog.syntax.Atom atom;
  
  public final hydra.ext.datalog.syntax.AtomList atomList;
  
  public AtomList_Multiple (hydra.ext.datalog.syntax.Atom atom, hydra.ext.datalog.syntax.AtomList atomList) {
    java.util.Objects.requireNonNull((atom));
    java.util.Objects.requireNonNull((atomList));
    this.atom = atom;
    this.atomList = atomList;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AtomList_Multiple)) {
      return false;
    }
    AtomList_Multiple o = (AtomList_Multiple) (other);
    return atom.equals(o.atom) && atomList.equals(o.atomList);
  }
  
  @Override
  public int hashCode() {
    return 2 * atom.hashCode() + 3 * atomList.hashCode();
  }
  
  public AtomList_Multiple withAtom(hydra.ext.datalog.syntax.Atom atom) {
    java.util.Objects.requireNonNull((atom));
    return new AtomList_Multiple(atom, atomList);
  }
  
  public AtomList_Multiple withAtomList(hydra.ext.datalog.syntax.AtomList atomList) {
    java.util.Objects.requireNonNull((atomList));
    return new AtomList_Multiple(atom, atomList);
  }
}