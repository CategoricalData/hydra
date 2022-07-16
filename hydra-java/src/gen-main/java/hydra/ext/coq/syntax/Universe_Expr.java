package hydra.ext.coq.syntax;

public class Universe_Expr {
  public final UniverseName name;
  
  public final java.util.Optional<Natural> number;
  
  public Universe_Expr (UniverseName name, java.util.Optional<Natural> number) {
    this.name = name;
    this.number = number;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Universe_Expr)) {
      return false;
    }
    Universe_Expr o = (Universe_Expr) (other);
    return name.equals(o.name) && number.equals(o.number);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * number.hashCode();
  }
  
  public Universe_Expr withName(UniverseName name) {
    return new Universe_Expr(name, number);
  }
  
  public Universe_Expr withNumber(java.util.Optional<Natural> number) {
    return new Universe_Expr(name, number);
  }
}