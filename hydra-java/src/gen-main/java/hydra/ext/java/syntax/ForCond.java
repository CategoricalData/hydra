// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ForCond implements Serializable, Comparable<ForCond> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ForCond");
  
  public static final hydra.core.Name FIELD_NAME_INIT = new hydra.core.Name("init");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public static final hydra.core.Name FIELD_NAME_UPDATE = new hydra.core.Name("update");
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.ForInit> init;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.Expression> cond;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.ForUpdate> update;
  
  public ForCond (hydra.util.Maybe<hydra.ext.java.syntax.ForInit> init, hydra.util.Maybe<hydra.ext.java.syntax.Expression> cond, hydra.util.Maybe<hydra.ext.java.syntax.ForUpdate> update) {
    this.init = init;
    this.cond = cond;
    this.update = update;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForCond)) {
      return false;
    }
    ForCond o = (ForCond) other;
    return java.util.Objects.equals(
      this.init,
      o.init) && java.util.Objects.equals(
      this.cond,
      o.cond) && java.util.Objects.equals(
      this.update,
      o.update);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(init) + 3 * java.util.Objects.hashCode(cond) + 5 * java.util.Objects.hashCode(update);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ForCond other) {
    int cmp = 0;
    cmp = Integer.compare(
      init.hashCode(),
      other.init.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      cond.hashCode(),
      other.cond.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      update.hashCode(),
      other.update.hashCode());
  }
  
  public ForCond withInit(hydra.util.Maybe<hydra.ext.java.syntax.ForInit> init) {
    return new ForCond(init, cond, update);
  }
  
  public ForCond withCond(hydra.util.Maybe<hydra.ext.java.syntax.Expression> cond) {
    return new ForCond(init, cond, update);
  }
  
  public ForCond withUpdate(hydra.util.Maybe<hydra.ext.java.syntax.ForUpdate> update) {
    return new ForCond(init, cond, update);
  }
}
