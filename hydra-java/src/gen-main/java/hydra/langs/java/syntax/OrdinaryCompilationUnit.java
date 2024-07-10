// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class OrdinaryCompilationUnit implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.OrdinaryCompilationUnit");
  
  public final java.util.Optional<hydra.langs.java.syntax.PackageDeclaration> package_;
  
  public final java.util.List<hydra.langs.java.syntax.ImportDeclaration> imports;
  
  public final java.util.List<hydra.langs.java.syntax.TypeDeclarationWithComments> types;
  
  public OrdinaryCompilationUnit (java.util.Optional<hydra.langs.java.syntax.PackageDeclaration> package_, java.util.List<hydra.langs.java.syntax.ImportDeclaration> imports, java.util.List<hydra.langs.java.syntax.TypeDeclarationWithComments> types) {
    if (package_ == null) {
      throw new IllegalArgumentException("null value for 'package' argument");
    }
    if (imports == null) {
      throw new IllegalArgumentException("null value for 'imports' argument");
    }
    if (types == null) {
      throw new IllegalArgumentException("null value for 'types' argument");
    }
    this.package_ = package_;
    this.imports = imports;
    this.types = types;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OrdinaryCompilationUnit)) {
      return false;
    }
    OrdinaryCompilationUnit o = (OrdinaryCompilationUnit) (other);
    return package_.equals(o.package_) && imports.equals(o.imports) && types.equals(o.types);
  }
  
  @Override
  public int hashCode() {
    return 2 * package_.hashCode() + 3 * imports.hashCode() + 5 * types.hashCode();
  }
  
  public OrdinaryCompilationUnit withPackage(java.util.Optional<hydra.langs.java.syntax.PackageDeclaration> package_) {
    if (package_ == null) {
      throw new IllegalArgumentException("null value for 'package' argument");
    }
    return new OrdinaryCompilationUnit(package_, imports, types);
  }
  
  public OrdinaryCompilationUnit withImports(java.util.List<hydra.langs.java.syntax.ImportDeclaration> imports) {
    if (imports == null) {
      throw new IllegalArgumentException("null value for 'imports' argument");
    }
    return new OrdinaryCompilationUnit(package_, imports, types);
  }
  
  public OrdinaryCompilationUnit withTypes(java.util.List<hydra.langs.java.syntax.TypeDeclarationWithComments> types) {
    if (types == null) {
      throw new IllegalArgumentException("null value for 'types' argument");
    }
    return new OrdinaryCompilationUnit(package_, imports, types);
  }
}