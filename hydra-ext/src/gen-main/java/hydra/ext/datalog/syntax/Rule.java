// Note: this is an automatically generated file. Do not edit.

package hydra.ext.datalog.syntax;

import java.io.Serializable;

public class Rule implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.datalog.syntax.Rule");
  
  public static final hydra.core.Name FIELD_NAME_ATOM = new hydra.core.Name("atom");
  
  public static final hydra.core.Name FIELD_NAME_ATOM_LIST = new hydra.core.Name("atomList");
  
  public final hydra.ext.datalog.syntax.Atom atom;
  
  public final hydra.ext.datalog.syntax.AtomList atomList;
  
  public Rule (hydra.ext.datalog.syntax.Atom atom, hydra.ext.datalog.syntax.AtomList atomList) {
    java.util.Objects.requireNonNull((atom));
    java.util.Objects.requireNonNull((atomList));
    this.atom = atom;
    this.atomList = atomList;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Rule)) {
      return false;
    }
    Rule o = (Rule) (other);
    return atom.equals(o.atom) && atomList.equals(o.atomList);
  }
  
  @Override
  public int hashCode() {
    return 2 * atom.hashCode() + 3 * atomList.hashCode();
  }
  
  public Rule withAtom(hydra.ext.datalog.syntax.Atom atom) {
    java.util.Objects.requireNonNull((atom));
    return new Rule(atom, atomList);
  }
  
  public Rule withAtomList(hydra.ext.datalog.syntax.AtomList atomList) {
    java.util.Objects.requireNonNull((atomList));
    return new Rule(atom, atomList);
  }
}