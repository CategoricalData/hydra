package hydra.ext.java.syntax;

public class EmptyStatement {
  public EmptyStatement () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EmptyStatement)) {
      return false;
    }
    EmptyStatement o = (EmptyStatement) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}