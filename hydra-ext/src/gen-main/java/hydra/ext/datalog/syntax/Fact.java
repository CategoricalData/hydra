// Note: this is an automatically generated file. Do not edit.

package hydra.ext.datalog.syntax;

import java.io.Serializable;

public class Fact implements Serializable, Comparable<Fact> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.datalog.syntax.Fact");
  
  public static final hydra.core.Name RELATION = new hydra.core.Name("Relation");
  
  public static final hydra.core.Name CONSTANT_LIST = new hydra.core.Name("ConstantList");
  
  public final hydra.ext.datalog.syntax.Relation Relation;
  
  public final hydra.ext.datalog.syntax.ConstantList ConstantList;
  
  public Fact (hydra.ext.datalog.syntax.Relation Relation, hydra.ext.datalog.syntax.ConstantList ConstantList) {
    this.Relation = Relation;
    this.ConstantList = ConstantList;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Fact)) {
      return false;
    }
    Fact o = (Fact) other;
    return java.util.Objects.equals(
      this.Relation,
      o.Relation) && java.util.Objects.equals(
      this.ConstantList,
      o.ConstantList);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(Relation) + 3 * java.util.Objects.hashCode(ConstantList);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Fact other) {
    int cmp = 0;
    cmp = ((Comparable) Relation).compareTo(other.Relation);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) ConstantList).compareTo(other.ConstantList);
  }
  
  public Fact withRelation(hydra.ext.datalog.syntax.Relation Relation) {
    return new Fact(Relation, ConstantList);
  }
  
  public Fact withConstantList(hydra.ext.datalog.syntax.ConstantList ConstantList) {
    return new Fact(Relation, ConstantList);
  }
}
