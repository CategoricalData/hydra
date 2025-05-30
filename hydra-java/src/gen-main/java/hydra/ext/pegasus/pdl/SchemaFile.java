// Note: this is an automatically generated file. Do not edit.

package hydra.ext.pegasus.pdl;

import java.io.Serializable;

public class SchemaFile implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.pegasus.pdl.SchemaFile");
  
  public static final hydra.core.Name FIELD_NAME_NAMESPACE = new hydra.core.Name("namespace");
  
  public static final hydra.core.Name FIELD_NAME_PACKAGE = new hydra.core.Name("package");
  
  public static final hydra.core.Name FIELD_NAME_IMPORTS = new hydra.core.Name("imports");
  
  public static final hydra.core.Name FIELD_NAME_SCHEMAS = new hydra.core.Name("schemas");
  
  public final hydra.ext.pegasus.pdl.Namespace namespace;
  
  public final hydra.util.Opt<hydra.ext.pegasus.pdl.Package_> package_;
  
  public final java.util.List<hydra.ext.pegasus.pdl.QualifiedName> imports;
  
  public final java.util.List<hydra.ext.pegasus.pdl.NamedSchema> schemas;
  
  public SchemaFile (hydra.ext.pegasus.pdl.Namespace namespace, hydra.util.Opt<hydra.ext.pegasus.pdl.Package_> package_, java.util.List<hydra.ext.pegasus.pdl.QualifiedName> imports, java.util.List<hydra.ext.pegasus.pdl.NamedSchema> schemas) {
    java.util.Objects.requireNonNull((namespace));
    java.util.Objects.requireNonNull((package_));
    java.util.Objects.requireNonNull((imports));
    java.util.Objects.requireNonNull((schemas));
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
    java.util.Objects.requireNonNull((namespace));
    return new SchemaFile(namespace, package_, imports, schemas);
  }
  
  public SchemaFile withPackage(hydra.util.Opt<hydra.ext.pegasus.pdl.Package_> package_) {
    java.util.Objects.requireNonNull((package_));
    return new SchemaFile(namespace, package_, imports, schemas);
  }
  
  public SchemaFile withImports(java.util.List<hydra.ext.pegasus.pdl.QualifiedName> imports) {
    java.util.Objects.requireNonNull((imports));
    return new SchemaFile(namespace, package_, imports, schemas);
  }
  
  public SchemaFile withSchemas(java.util.List<hydra.ext.pegasus.pdl.NamedSchema> schemas) {
    java.util.Objects.requireNonNull((schemas));
    return new SchemaFile(namespace, package_, imports, schemas);
  }
}