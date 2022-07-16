package hydra.ext.java.syntax;

public class OrdinaryCompilationUnit {
  public final java.util.Optional<PackageDeclaration> package_;
  
  public final java.util.List<ImportDeclaration> imports;
  
  public final java.util.List<TypeDeclarationWithComments> types;
  
  public OrdinaryCompilationUnit (java.util.Optional<PackageDeclaration> package_, java.util.List<ImportDeclaration> imports, java.util.List<TypeDeclarationWithComments> types) {
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
  
  public OrdinaryCompilationUnit withPackage(java.util.Optional<PackageDeclaration> package_) {
    return new OrdinaryCompilationUnit(package_, imports, types);
  }
  
  public OrdinaryCompilationUnit withImports(java.util.List<ImportDeclaration> imports) {
    return new OrdinaryCompilationUnit(package_, imports, types);
  }
  
  public OrdinaryCompilationUnit withTypes(java.util.List<TypeDeclarationWithComments> types) {
    return new OrdinaryCompilationUnit(package_, imports, types);
  }
}