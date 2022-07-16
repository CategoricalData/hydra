package hydra.ext.java.syntax;

public class ModularCompilationUnit {
  public final java.util.List<ImportDeclaration> imports;
  
  public final ModuleDeclaration module;
  
  public ModularCompilationUnit (java.util.List<ImportDeclaration> imports, ModuleDeclaration module) {
    this.imports = imports;
    this.module = module;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModularCompilationUnit)) {
      return false;
    }
    ModularCompilationUnit o = (ModularCompilationUnit) (other);
    return imports.equals(o.imports) && module.equals(o.module);
  }
  
  @Override
  public int hashCode() {
    return 2 * imports.hashCode() + 3 * module.hashCode();
  }
  
  public ModularCompilationUnit withImports(java.util.List<ImportDeclaration> imports) {
    return new ModularCompilationUnit(imports, module);
  }
  
  public ModularCompilationUnit withModule(ModuleDeclaration module) {
    return new ModularCompilationUnit(imports, module);
  }
}