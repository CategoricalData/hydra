package hydra.ext.datalog.syntax;

public class AtomList_Multiple {
  public final Atom atom;
  
  public final AtomList atomList;
  
  public AtomList_Multiple (Atom atom, AtomList atomList) {
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
  
  public AtomList_Multiple withAtom(Atom atom) {
    return new AtomList_Multiple(atom, atomList);
  }
  
  public AtomList_Multiple withAtomList(AtomList atomList) {
    return new AtomList_Multiple(atom, atomList);
  }
}