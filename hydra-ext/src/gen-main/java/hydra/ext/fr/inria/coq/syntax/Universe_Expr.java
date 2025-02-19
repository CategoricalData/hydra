// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class Universe_Expr implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.Universe_Expr");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_NUMBER = new hydra.core.Name("number");
  
  public final hydra.ext.fr.inria.coq.syntax.UniverseName name;
  
  public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Natural> number;
  
  public Universe_Expr (hydra.ext.fr.inria.coq.syntax.UniverseName name, hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Natural> number) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((number));
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
  
  public Universe_Expr withName(hydra.ext.fr.inria.coq.syntax.UniverseName name) {
    java.util.Objects.requireNonNull((name));
    return new Universe_Expr(name, number);
  }
  
  public Universe_Expr withNumber(hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Natural> number) {
    java.util.Objects.requireNonNull((number));
    return new Universe_Expr(name, number);
  }
}