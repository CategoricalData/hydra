package hydra.ext.datalog.syntax;

public class Fact {
  public final Relation relation;
  
  public final ConstantList constantList;
  
  public Fact (Relation relation, ConstantList constantList) {
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
  
  public Fact withRelation(Relation relation) {
    return new Fact(relation, constantList);
  }
  
  public Fact withConstantList(ConstantList constantList) {
    return new Fact(relation, constantList);
  }
}