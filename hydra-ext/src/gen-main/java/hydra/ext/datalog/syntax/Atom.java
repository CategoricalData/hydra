// Note: this is an automatically generated file. Do not edit.

package hydra.ext.datalog.syntax;

import java.io.Serializable;

public class Atom implements Serializable, Comparable<Atom> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.datalog.syntax.Atom");
  
  public static final hydra.core.Name RELATION = new hydra.core.Name("Relation");
  
  public static final hydra.core.Name TERM_LIST = new hydra.core.Name("TermList");
  
  public final hydra.ext.datalog.syntax.Relation Relation;
  
  public final hydra.ext.datalog.syntax.TermList TermList;
  
  public Atom (hydra.ext.datalog.syntax.Relation Relation, hydra.ext.datalog.syntax.TermList TermList) {
    this.Relation = Relation;
    this.TermList = TermList;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Atom)) {
      return false;
    }
    Atom o = (Atom) other;
    return java.util.Objects.equals(
      this.Relation,
      o.Relation) && java.util.Objects.equals(
      this.TermList,
      o.TermList);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(Relation) + 3 * java.util.Objects.hashCode(TermList);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Atom other) {
    int cmp = 0;
    cmp = ((Comparable) Relation).compareTo(other.Relation);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) TermList).compareTo(other.TermList);
  }
  
  public Atom withRelation(hydra.ext.datalog.syntax.Relation Relation) {
    return new Atom(Relation, TermList);
  }
  
  public Atom withTermList(hydra.ext.datalog.syntax.TermList TermList) {
    return new Atom(Relation, TermList);
  }
}
