package hydra.ext.datalog.syntax;

public class Program {
  public final java.util.List<hydra.ext.datalog.syntax.Program_Elmt> value;
  
  public Program (java.util.List<hydra.ext.datalog.syntax.Program_Elmt> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Program)) {
      return false;
    }
    Program o = (Program) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}