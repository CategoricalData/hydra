// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class ModularCompilationUnit implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.ModularCompilationUnit");
  
  public static final hydra.core.Name FIELD_NAME_IMPORTS = new hydra.core.Name("imports");
  
  public static final hydra.core.Name FIELD_NAME_MODULE = new hydra.core.Name("module");
  
  public final java.util.List<hydra.langs.java.syntax.ImportDeclaration> imports;
  
  public final hydra.langs.java.syntax.ModuleDeclaration module;
  
  public ModularCompilationUnit (java.util.List<hydra.langs.java.syntax.ImportDeclaration> imports, hydra.langs.java.syntax.ModuleDeclaration module) {
    java.util.Objects.requireNonNull((imports));
    java.util.Objects.requireNonNull((module));
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
  
  public ModularCompilationUnit withImports(java.util.List<hydra.langs.java.syntax.ImportDeclaration> imports) {
    java.util.Objects.requireNonNull((imports));
    return new ModularCompilationUnit(imports, module);
  }
  
  public ModularCompilationUnit withModule(hydra.langs.java.syntax.ModuleDeclaration module) {
    java.util.Objects.requireNonNull((module));
    return new ModularCompilationUnit(imports, module);
  }
}