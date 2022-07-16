package hydra.ext.java.syntax;

public class ForCond {
  public final java.util.Optional<ForInit> init;
  
  public final java.util.Optional<Expression> cond;
  
  public final java.util.Optional<ForUpdate> update;
  
  public ForCond (java.util.Optional<ForInit> init, java.util.Optional<Expression> cond, java.util.Optional<ForUpdate> update) {
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
  
  public ForCond withInit(java.util.Optional<ForInit> init) {
    return new ForCond(init, cond, update);
  }
  
  public ForCond withCond(java.util.Optional<Expression> cond) {
    return new ForCond(init, cond, update);
  }
  
  public ForCond withUpdate(java.util.Optional<ForUpdate> update) {
    return new ForCond(init, cond, update);
  }
}