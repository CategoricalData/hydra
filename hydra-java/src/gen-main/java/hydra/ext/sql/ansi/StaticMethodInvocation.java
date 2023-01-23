package hydra.ext.sql.ansi;

public class StaticMethodInvocation {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.StaticMethodInvocation");
  
  public StaticMethodInvocation () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StaticMethodInvocation)) {
      return false;
    }
    StaticMethodInvocation o = (StaticMethodInvocation) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}