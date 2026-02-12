// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class OrdinaryCompilationUnit implements Serializable, Comparable<OrdinaryCompilationUnit> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.OrdinaryCompilationUnit");
  
  public static final hydra.core.Name FIELD_NAME_PACKAGE = new hydra.core.Name("package");
  
  public static final hydra.core.Name FIELD_NAME_IMPORTS = new hydra.core.Name("imports");
  
  public static final hydra.core.Name FIELD_NAME_TYPES = new hydra.core.Name("types");
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.PackageDeclaration> package_;
  
  public final java.util.List<hydra.ext.java.syntax.ImportDeclaration> imports;
  
  public final java.util.List<hydra.ext.java.syntax.TypeDeclarationWithComments> types;
  
  public OrdinaryCompilationUnit (hydra.util.Maybe<hydra.ext.java.syntax.PackageDeclaration> package_, java.util.List<hydra.ext.java.syntax.ImportDeclaration> imports, java.util.List<hydra.ext.java.syntax.TypeDeclarationWithComments> types) {
    this.package_ = package_;
    this.imports = imports;
    this.types = types;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OrdinaryCompilationUnit)) {
      return false;
    }
    OrdinaryCompilationUnit o = (OrdinaryCompilationUnit) other;
    return java.util.Objects.equals(
      this.package_,
      o.package_) && java.util.Objects.equals(
      this.imports,
      o.imports) && java.util.Objects.equals(
      this.types,
      o.types);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(package_) + 3 * java.util.Objects.hashCode(imports) + 5 * java.util.Objects.hashCode(types);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OrdinaryCompilationUnit other) {
    int cmp = 0;
    cmp = Integer.compare(
      package_.hashCode(),
      other.package_.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      imports.hashCode(),
      other.imports.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      types.hashCode(),
      other.types.hashCode());
  }
  
  public OrdinaryCompilationUnit withPackage(hydra.util.Maybe<hydra.ext.java.syntax.PackageDeclaration> package_) {
    return new OrdinaryCompilationUnit(package_, imports, types);
  }
  
  public OrdinaryCompilationUnit withImports(java.util.List<hydra.ext.java.syntax.ImportDeclaration> imports) {
    return new OrdinaryCompilationUnit(package_, imports, types);
  }
  
  public OrdinaryCompilationUnit withTypes(java.util.List<hydra.ext.java.syntax.TypeDeclarationWithComments> types) {
    return new OrdinaryCompilationUnit(package_, imports, types);
  }
}
