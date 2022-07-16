package hydra.ext.pegasus.pdl;

public class SchemaFile {
  public final Namespace namespace;
  
  public final java.util.Optional<Package_> package_;
  
  public final java.util.List<QualifiedName> imports;
  
  public final java.util.List<NamedSchema> schemas;
  
  public SchemaFile (Namespace namespace, java.util.Optional<Package_> package_, java.util.List<QualifiedName> imports, java.util.List<NamedSchema> schemas) {
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
  
  public SchemaFile withNamespace(Namespace namespace) {
    return new SchemaFile(namespace, package_, imports, schemas);
  }
  
  public SchemaFile withPackage(java.util.Optional<Package_> package_) {
    return new SchemaFile(namespace, package_, imports, schemas);
  }
  
  public SchemaFile withImports(java.util.List<QualifiedName> imports) {
    return new SchemaFile(namespace, package_, imports, schemas);
  }
  
  public SchemaFile withSchemas(java.util.List<NamedSchema> schemas) {
    return new SchemaFile(namespace, package_, imports, schemas);
  }
}