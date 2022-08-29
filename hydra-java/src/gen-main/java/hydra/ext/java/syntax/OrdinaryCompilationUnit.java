package hydra.ext.java.syntax;

public class OrdinaryCompilationUnit {
  public final java.util.Optional<hydra.ext.java.syntax.PackageDeclaration> package_;
  
  public final java.util.List<hydra.ext.java.syntax.ImportDeclaration> imports;
  
  public final java.util.List<hydra.ext.java.syntax.TypeDeclarationWithComments> types;
  
  public OrdinaryCompilationUnit (java.util.Optional<hydra.ext.java.syntax.PackageDeclaration> package_, java.util.List<hydra.ext.java.syntax.ImportDeclaration> imports, java.util.List<hydra.ext.java.syntax.TypeDeclarationWithComments> types) {
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
  
  public OrdinaryCompilationUnit withPackage(java.util.Optional<hydra.ext.java.syntax.PackageDeclaration> package_) {
    return new OrdinaryCompilationUnit(package_, imports, types);
  }
  
  public OrdinaryCompilationUnit withImports(java.util.List<hydra.ext.java.syntax.ImportDeclaration> imports) {
    return new OrdinaryCompilationUnit(package_, imports, types);
  }
  
  public OrdinaryCompilationUnit withTypes(java.util.List<hydra.ext.java.syntax.TypeDeclarationWithComments> types) {
    return new OrdinaryCompilationUnit(package_, imports, types);
  }
}