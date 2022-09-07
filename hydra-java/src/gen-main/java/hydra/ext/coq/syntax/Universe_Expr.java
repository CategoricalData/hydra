package hydra.ext.coq.syntax;

public class Universe_Expr {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Universe.Expr");
  
  public final hydra.ext.coq.syntax.UniverseName name;
  
  public final java.util.Optional<hydra.ext.coq.syntax.Natural> number;
  
  public Universe_Expr (hydra.ext.coq.syntax.UniverseName name, java.util.Optional<hydra.ext.coq.syntax.Natural> number) {
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
  
  public Universe_Expr withName(hydra.ext.coq.syntax.UniverseName name) {
    return new Universe_Expr(name, number);
  }
  
  public Universe_Expr withNumber(java.util.Optional<hydra.ext.coq.syntax.Natural> number) {
    return new Universe_Expr(name, number);
  }
}