// Note: this is an automatically generated file. Do not edit.

package hydra.ext.datalog.syntax;

import java.io.Serializable;

public class Atom implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.datalog.syntax.Atom");
  
  public static final hydra.core.Name FIELD_NAME_RELATION = new hydra.core.Name("relation");
  
  public static final hydra.core.Name FIELD_NAME_TERM_LIST = new hydra.core.Name("termList");
  
  public final hydra.ext.datalog.syntax.Relation relation;
  
  public final hydra.ext.datalog.syntax.TermList termList;
  
  public Atom (hydra.ext.datalog.syntax.Relation relation, hydra.ext.datalog.syntax.TermList termList) {
    java.util.Objects.requireNonNull((relation));
    java.util.Objects.requireNonNull((termList));
    this.relation = relation;
    this.termList = termList;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Atom)) {
      return false;
    }
    Atom o = (Atom) (other);
    return relation.equals(o.relation) && termList.equals(o.termList);
  }
  
  @Override
  public int hashCode() {
    return 2 * relation.hashCode() + 3 * termList.hashCode();
  }
  
  public Atom withRelation(hydra.ext.datalog.syntax.Relation relation) {
    java.util.Objects.requireNonNull((relation));
    return new Atom(relation, termList);
  }
  
  public Atom withTermList(hydra.ext.datalog.syntax.TermList termList) {
    java.util.Objects.requireNonNull((termList));
    return new Atom(relation, termList);
  }
}