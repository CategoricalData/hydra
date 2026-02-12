// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ModularCompilationUnit implements Serializable, Comparable<ModularCompilationUnit> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ModularCompilationUnit");
  
  public static final hydra.core.Name FIELD_NAME_IMPORTS = new hydra.core.Name("imports");
  
  public static final hydra.core.Name FIELD_NAME_MODULE = new hydra.core.Name("module");
  
  public final java.util.List<hydra.ext.java.syntax.ImportDeclaration> imports;
  
  public final hydra.ext.java.syntax.ModuleDeclaration module;
  
  public ModularCompilationUnit (java.util.List<hydra.ext.java.syntax.ImportDeclaration> imports, hydra.ext.java.syntax.ModuleDeclaration module) {
    this.imports = imports;
    this.module = module;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModularCompilationUnit)) {
      return false;
    }
    ModularCompilationUnit o = (ModularCompilationUnit) other;
    return java.util.Objects.equals(
      this.imports,
      o.imports) && java.util.Objects.equals(
      this.module,
      o.module);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(imports) + 3 * java.util.Objects.hashCode(module);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ModularCompilationUnit other) {
    int cmp = 0;
    cmp = Integer.compare(
      imports.hashCode(),
      other.imports.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) module).compareTo(other.module);
  }
  
  public ModularCompilationUnit withImports(java.util.List<hydra.ext.java.syntax.ImportDeclaration> imports) {
    return new ModularCompilationUnit(imports, module);
  }
  
  public ModularCompilationUnit withModule(hydra.ext.java.syntax.ModuleDeclaration module) {
    return new ModularCompilationUnit(imports, module);
  }
}
