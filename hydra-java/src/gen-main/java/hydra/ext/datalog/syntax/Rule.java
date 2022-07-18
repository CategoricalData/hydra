package hydra.ext.datalog.syntax;

public class Rule {
  public final Atom atom;
  
  public final AtomList atomList;
  
  public Rule (Atom atom, AtomList atomList) {
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
  
  public Rule withAtom(Atom atom) {
    return new Rule(atom, atomList);
  }
  
  public Rule withAtomList(AtomList atomList) {
    return new Rule(atom, atomList);
  }
}