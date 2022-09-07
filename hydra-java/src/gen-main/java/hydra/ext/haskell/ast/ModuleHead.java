package hydra.ext.haskell.ast;

public class ModuleHead {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.ModuleHead");
  
  public final hydra.ext.haskell.ast.ModuleName name;
  
  public final java.util.List<hydra.ext.haskell.ast.Export> exports;
  
  public ModuleHead (hydra.ext.haskell.ast.ModuleName name, java.util.List<hydra.ext.haskell.ast.Export> exports) {
    this.name = name;
    this.exports = exports;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModuleHead)) {
      return false;
    }
    ModuleHead o = (ModuleHead) (other);
    return name.equals(o.name) && exports.equals(o.exports);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * exports.hashCode();
  }
  
  public ModuleHead withName(hydra.ext.haskell.ast.ModuleName name) {
    return new ModuleHead(name, exports);
  }
  
  public ModuleHead withExports(java.util.List<hydra.ext.haskell.ast.Export> exports) {
    return new ModuleHead(name, exports);
  }
}