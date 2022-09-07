package hydra.core;

public class CaseStatement<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.CaseStatement");
  
  public final hydra.core.Name typeName;
  
  public final java.util.List<hydra.core.Field<M>> cases;
  
  public CaseStatement (hydra.core.Name typeName, java.util.List<hydra.core.Field<M>> cases) {
    this.typeName = typeName;
    this.cases = cases;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseStatement)) {
      return false;
    }
    CaseStatement o = (CaseStatement) (other);
    return typeName.equals(o.typeName) && cases.equals(o.cases);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * cases.hashCode();
  }
  
  public CaseStatement withTypeName(hydra.core.Name typeName) {
    return new CaseStatement(typeName, cases);
  }
  
  public CaseStatement withCases(java.util.List<hydra.core.Field<M>> cases) {
    return new CaseStatement(typeName, cases);
  }
}