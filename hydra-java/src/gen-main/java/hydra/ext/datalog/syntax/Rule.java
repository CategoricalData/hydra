package hydra.ext.datalog.syntax;

public class Rule {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/datalog/syntax.Rule");
  
  public final hydra.ext.datalog.syntax.Atom atom;
  
  public final hydra.ext.datalog.syntax.AtomList atomList;
  
  public Rule (hydra.ext.datalog.syntax.Atom atom, hydra.ext.datalog.syntax.AtomList atomList) {
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
    return new Rule(atom, atomList);
  }
  
  public Rule withAtomList(hydra.ext.datalog.syntax.AtomList atomList) {
    return new Rule(atom, atomList);
  }
}