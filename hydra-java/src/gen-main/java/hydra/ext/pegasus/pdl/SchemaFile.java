package hydra.ext.pegasus.pdl;

public class SchemaFile {
  public final hydra.ext.pegasus.pdl.Namespace namespace;
  
  public final java.util.Optional<hydra.ext.pegasus.pdl.Package_> package_;
  
  public final java.util.List<hydra.ext.pegasus.pdl.QualifiedName> imports;
  
  public final java.util.List<hydra.ext.pegasus.pdl.NamedSchema> schemas;
  
  public SchemaFile (hydra.ext.pegasus.pdl.Namespace namespace, java.util.Optional<hydra.ext.pegasus.pdl.Package_> package_, java.util.List<hydra.ext.pegasus.pdl.QualifiedName> imports, java.util.List<hydra.ext.pegasus.pdl.NamedSchema> schemas) {
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
  
  public SchemaFile withNamespace(hydra.ext.pegasus.pdl.Namespace namespace) {
    return new SchemaFile(namespace, package_, imports, schemas);
  }
  
  public SchemaFile withPackage(java.util.Optional<hydra.ext.pegasus.pdl.Package_> package_) {
    return new SchemaFile(namespace, package_, imports, schemas);
  }
  
  public SchemaFile withImports(java.util.List<hydra.ext.pegasus.pdl.QualifiedName> imports) {
    return new SchemaFile(namespace, package_, imports, schemas);
  }
  
  public SchemaFile withSchemas(java.util.List<hydra.ext.pegasus.pdl.NamedSchema> schemas) {
    return new SchemaFile(namespace, package_, imports, schemas);
  }
}