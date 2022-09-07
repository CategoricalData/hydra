package hydra.ext.datalog.syntax;

public class Fact {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/datalog/syntax.Fact");
  
  public final hydra.ext.datalog.syntax.Relation relation;
  
  public final hydra.ext.datalog.syntax.ConstantList constantList;
  
  public Fact (hydra.ext.datalog.syntax.Relation relation, hydra.ext.datalog.syntax.ConstantList constantList) {
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
    return new Fact(relation, constantList);
  }
  
  public Fact withConstantList(hydra.ext.datalog.syntax.ConstantList constantList) {
    return new Fact(relation, constantList);
  }
}