package hydra.ext.haskell.ast;

public class ModuleHead {
  public final ModuleName name;
  
  public final java.util.List<Export> exports;
  
  public ModuleHead (ModuleName name, java.util.List<Export> exports) {
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
  
  public ModuleHead withName(ModuleName name) {
    return new ModuleHead(name, exports);
  }
  
  public ModuleHead withExports(java.util.List<Export> exports) {
    return new ModuleHead(name, exports);
  }
}