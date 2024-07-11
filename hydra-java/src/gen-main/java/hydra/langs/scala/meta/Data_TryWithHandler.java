// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Data_TryWithHandler implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Data.TryWithHandler");
  
  public final hydra.langs.scala.meta.Data expr;
  
  public final hydra.langs.scala.meta.Data catchp;
  
  public final hydra.util.Opt<hydra.langs.scala.meta.Data> finallyp;
  
  public Data_TryWithHandler (hydra.langs.scala.meta.Data expr, hydra.langs.scala.meta.Data catchp, hydra.util.Opt<hydra.langs.scala.meta.Data> finallyp) {
    java.util.Objects.requireNonNull((expr));
    java.util.Objects.requireNonNull((catchp));
    java.util.Objects.requireNonNull((finallyp));
    this.expr = expr;
    this.catchp = catchp;
    this.finallyp = finallyp;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_TryWithHandler)) {
      return false;
    }
    Data_TryWithHandler o = (Data_TryWithHandler) (other);
    return expr.equals(o.expr) && catchp.equals(o.catchp) && finallyp.equals(o.finallyp);
  }
  
  @Override
  public int hashCode() {
    return 2 * expr.hashCode() + 3 * catchp.hashCode() + 5 * finallyp.hashCode();
  }
  
  public Data_TryWithHandler withExpr(hydra.langs.scala.meta.Data expr) {
    java.util.Objects.requireNonNull((expr));
    return new Data_TryWithHandler(expr, catchp, finallyp);
  }
  
  public Data_TryWithHandler withCatchp(hydra.langs.scala.meta.Data catchp) {
    java.util.Objects.requireNonNull((catchp));
    return new Data_TryWithHandler(expr, catchp, finallyp);
  }
  
  public Data_TryWithHandler withFinallyp(hydra.util.Opt<hydra.langs.scala.meta.Data> finallyp) {
    java.util.Objects.requireNonNull((finallyp));
    return new Data_TryWithHandler(expr, catchp, finallyp);
  }
}