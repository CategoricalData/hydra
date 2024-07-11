// Note: this is an automatically generated file. Do not edit.

package hydra.langs.pegasus.pdl;

import java.io.Serializable;

public class SchemaFile implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/pegasus/pdl.SchemaFile");
  
  public final hydra.langs.pegasus.pdl.Namespace namespace;
  
  public final hydra.util.Opt<hydra.langs.pegasus.pdl.Package_> package_;
  
  public final java.util.List<hydra.langs.pegasus.pdl.QualifiedName> imports;
  
  public final java.util.List<hydra.langs.pegasus.pdl.NamedSchema> schemas;
  
  public SchemaFile (hydra.langs.pegasus.pdl.Namespace namespace, hydra.util.Opt<hydra.langs.pegasus.pdl.Package_> package_, java.util.List<hydra.langs.pegasus.pdl.QualifiedName> imports, java.util.List<hydra.langs.pegasus.pdl.NamedSchema> schemas) {
    if (namespace == null) {
      throw new IllegalArgumentException("null value for 'namespace' argument");
    }
    if (package_ == null) {
      throw new IllegalArgumentException("null value for 'package' argument");
    }
    if (imports == null) {
      throw new IllegalArgumentException("null value for 'imports' argument");
    }
    if (schemas == null) {
      throw new IllegalArgumentException("null value for 'schemas' argument");
    }
    this.namespace = namespace;
    this.package_ = package_;
    this.imports = imports;
    this.schemas = schemas;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SchemaFile)) {
      return false;
    }
    SchemaFile o = (SchemaFile) (other);
    return namespace.equals(o.namespace) && package_.equals(o.package_) && imports.equals(o.imports) && schemas.equals(o.schemas);
  }
  
  @Override
  public int hashCode() {
    return 2 * namespace.hashCode() + 3 * package_.hashCode() + 5 * imports.hashCode() + 7 * schemas.hashCode();
  }
  
  public SchemaFile withNamespace(hydra.langs.pegasus.pdl.Namespace namespace) {
    if (namespace == null) {
      throw new IllegalArgumentException("null value for 'namespace' argument");
    }
    return new SchemaFile(namespace, package_, imports, schemas);
  }
  
  public SchemaFile withPackage(hydra.util.Opt<hydra.langs.pegasus.pdl.Package_> package_) {
    if (package_ == null) {
      throw new IllegalArgumentException("null value for 'package' argument");
    }
    return new SchemaFile(namespace, package_, imports, schemas);
  }
  
  public SchemaFile withImports(java.util.List<hydra.langs.pegasus.pdl.QualifiedName> imports) {
    if (imports == null) {
      throw new IllegalArgumentException("null value for 'imports' argument");
    }
    return new SchemaFile(namespace, package_, imports, schemas);
  }
  
  public SchemaFile withSchemas(java.util.List<hydra.langs.pegasus.pdl.NamedSchema> schemas) {
    if (schemas == null) {
      throw new IllegalArgumentException("null value for 'schemas' argument");
    }
    return new SchemaFile(namespace, package_, imports, schemas);
  }
}