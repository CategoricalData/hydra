package hydra.ext.java.syntax;

public class DoStatement {
  public final Statement body;
  
  public final java.util.Optional<Expression> conde;
  
  public DoStatement (Statement body, java.util.Optional<Expression> conde) {
    this.body = body;
    this.conde = conde;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DoStatement)) {
      return false;
    }
    DoStatement o = (DoStatement) (other);
    return body.equals(o.body) && conde.equals(o.conde);
  }
  
  @Override
  public int hashCode() {
    return 2 * body.hashCode() + 3 * conde.hashCode();
  }
  
  public DoStatement withBody(Statement body) {
    return new DoStatement(body, conde);
  }
  
  public DoStatement withConde(java.util.Optional<Expression> conde) {
    return new DoStatement(body, conde);
  }
}