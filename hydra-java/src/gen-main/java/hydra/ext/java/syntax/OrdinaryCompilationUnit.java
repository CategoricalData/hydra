// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class OrdinaryCompilationUnit implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.OrdinaryCompilationUnit");
  
  public static final hydra.core.Name FIELD_NAME_PACKAGE = new hydra.core.Name("package");
  
  public static final hydra.core.Name FIELD_NAME_IMPORTS = new hydra.core.Name("imports");
  
  public static final hydra.core.Name FIELD_NAME_TYPES = new hydra.core.Name("types");
  
  public final hydra.util.Opt<hydra.ext.java.syntax.PackageDeclaration> package_;
  
  public final java.util.List<hydra.ext.java.syntax.ImportDeclaration> imports;
  
  public final java.util.List<hydra.ext.java.syntax.TypeDeclarationWithComments> types;
  
  public OrdinaryCompilationUnit (hydra.util.Opt<hydra.ext.java.syntax.PackageDeclaration> package_, java.util.List<hydra.ext.java.syntax.ImportDeclaration> imports, java.util.List<hydra.ext.java.syntax.TypeDeclarationWithComments> types) {
    java.util.Objects.requireNonNull((package_));
    java.util.Objects.requireNonNull((imports));
    java.util.Objects.requireNonNull((types));
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
  
  public OrdinaryCompilationUnit withPackage(hydra.util.Opt<hydra.ext.java.syntax.PackageDeclaration> package_) {
    java.util.Objects.requireNonNull((package_));
    return new OrdinaryCompilationUnit(package_, imports, types);
  }
  
  public OrdinaryCompilationUnit withImports(java.util.List<hydra.ext.java.syntax.ImportDeclaration> imports) {
    java.util.Objects.requireNonNull((imports));
    return new OrdinaryCompilationUnit(package_, imports, types);
  }
  
  public OrdinaryCompilationUnit withTypes(java.util.List<hydra.ext.java.syntax.TypeDeclarationWithComments> types) {
    java.util.Objects.requireNonNull((types));
    return new OrdinaryCompilationUnit(package_, imports, types);
  }
}