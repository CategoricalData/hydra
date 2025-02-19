// Note: this is an automatically generated file. Do not edit.

package hydra.ext.datalog.syntax;

import java.io.Serializable;

public class Fact implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.datalog.syntax.Fact");
  
  public static final hydra.core.Name FIELD_NAME_RELATION = new hydra.core.Name("relation");
  
  public static final hydra.core.Name FIELD_NAME_CONSTANT_LIST = new hydra.core.Name("constantList");
  
  public final hydra.ext.datalog.syntax.Relation relation;
  
  public final hydra.ext.datalog.syntax.ConstantList constantList;
  
  public Fact (hydra.ext.datalog.syntax.Relation relation, hydra.ext.datalog.syntax.ConstantList constantList) {
    java.util.Objects.requireNonNull((relation));
    java.util.Objects.requireNonNull((constantList));
    this.relation = relation;
    this.constantList = constantList;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Fact)) {
      return false;
    }
    Fact o = (Fact) (other);
    return relation.equals(o.relation) && constantList.equals(o.constantList);
  }
  
  @Override
  public int hashCode() {
    return 2 * relation.hashCode() + 3 * constantList.hashCode();
  }
  
  public Fact withRelation(hydra.ext.datalog.syntax.Relation relation) {
    java.util.Objects.requireNonNull((relation));
    return new Fact(relation, constantList);
  }
  
  public Fact withConstantList(hydra.ext.datalog.syntax.ConstantList constantList) {
    java.util.Objects.requireNonNull((constantList));
    return new Fact(relation, constantList);
  }
}