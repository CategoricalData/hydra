package hydra.ext.haskell.ast;

public class Pattern_Application {
  public final Name name;
  
  public final java.util.List<Pattern> args;
  
  public Pattern_Application (Name name, java.util.List<Pattern> args) {
    this.name = name;
    this.args = args;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern_Application)) {
      return false;
    }
    Pattern_Application o = (Pattern_Application) (other);
    return name.equals(o.name) && args.equals(o.args);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * args.hashCode();
  }
  
  public Pattern_Application withName(Name name) {
    return new Pattern_Application(name, args);
  }
  
  public Pattern_Application withArgs(java.util.List<Pattern> args) {
    return new Pattern_Application(name, args);
  }
}