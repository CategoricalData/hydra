package hydra.langs.haskell.ast;

public class ModuleName {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/haskell/ast.ModuleName");
  
  public final String value;
  
  public ModuleName (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModuleName)) {
      return false;
    }
    ModuleName o = (ModuleName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}