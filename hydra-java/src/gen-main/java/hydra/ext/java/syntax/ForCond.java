// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ForCond implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.ForCond");
  
  public static final hydra.core.Name FIELD_NAME_INIT = new hydra.core.Name("init");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public static final hydra.core.Name FIELD_NAME_UPDATE = new hydra.core.Name("update");
  
  public final hydra.util.Opt<hydra.ext.java.syntax.ForInit> init;
  
  public final hydra.util.Opt<hydra.ext.java.syntax.Expression> cond;
  
  public final hydra.util.Opt<hydra.ext.java.syntax.ForUpdate> update;
  
  public ForCond (hydra.util.Opt<hydra.ext.java.syntax.ForInit> init, hydra.util.Opt<hydra.ext.java.syntax.Expression> cond, hydra.util.Opt<hydra.ext.java.syntax.ForUpdate> update) {
    java.util.Objects.requireNonNull((init));
    java.util.Objects.requireNonNull((cond));
    java.util.Objects.requireNonNull((update));
    this.init = init;
    this.cond = cond;
    this.update = update;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForCond)) {
      return false;
    }
    ForCond o = (ForCond) (other);
    return init.equals(o.init) && cond.equals(o.cond) && update.equals(o.update);
  }
  
  @Override
  public int hashCode() {
    return 2 * init.hashCode() + 3 * cond.hashCode() + 5 * update.hashCode();
  }
  
  public ForCond withInit(hydra.util.Opt<hydra.ext.java.syntax.ForInit> init) {
    java.util.Objects.requireNonNull((init));
    return new ForCond(init, cond, update);
  }
  
  public ForCond withCond(hydra.util.Opt<hydra.ext.java.syntax.Expression> cond) {
    java.util.Objects.requireNonNull((cond));
    return new ForCond(init, cond, update);
  }
  
  public ForCond withUpdate(hydra.util.Opt<hydra.ext.java.syntax.ForUpdate> update) {
    java.util.Objects.requireNonNull((update));
    return new ForCond(init, cond, update);
  }
}
