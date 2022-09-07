package hydra.ext.datalog.syntax;

public class AtomList_Multiple {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/datalog/syntax.AtomList.Multiple");
  
  public final hydra.ext.datalog.syntax.Atom atom;
  
  public final hydra.ext.datalog.syntax.AtomList atomList;
  
  public AtomList_Multiple (hydra.ext.datalog.syntax.Atom atom, hydra.ext.datalog.syntax.AtomList atomList) {
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
    return new AtomList_Multiple(atom, atomList);
  }
  
  public AtomList_Multiple withAtomList(hydra.ext.datalog.syntax.AtomList atomList) {
    return new AtomList_Multiple(atom, atomList);
  }
}