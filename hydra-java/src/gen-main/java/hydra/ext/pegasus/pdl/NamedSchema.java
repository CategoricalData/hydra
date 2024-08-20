// Note: this is an automatically generated file. Do not edit.

package hydra.ext.pegasus.pdl;

import java.io.Serializable;

public class NamedSchema implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/pegasus/pdl.NamedSchema");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIED_NAME = new hydra.core.Name("qualifiedName");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public final hydra.ext.pegasus.pdl.QualifiedName qualifiedName;
  
  public final hydra.ext.pegasus.pdl.NamedSchema_Type type;
  
  public final hydra.ext.pegasus.pdl.Annotations annotations;
  
  public NamedSchema (hydra.ext.pegasus.pdl.QualifiedName qualifiedName, hydra.ext.pegasus.pdl.NamedSchema_Type type, hydra.ext.pegasus.pdl.Annotations annotations) {
    java.util.Objects.requireNonNull((qualifiedName));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((annotations));
    this.qualifiedName = qualifiedName;
    this.type = type;
    this.annotations = annotations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NamedSchema)) {
      return false;
    }
    NamedSchema o = (NamedSchema) (other);
    return qualifiedName.equals(o.qualifiedName) && type.equals(o.type) && annotations.equals(o.annotations);
  }
  
  @Override
  public int hashCode() {
    return 2 * qualifiedName.hashCode() + 3 * type.hashCode() + 5 * annotations.hashCode();
  }
  
  public NamedSchema withQualifiedName(hydra.ext.pegasus.pdl.QualifiedName qualifiedName) {
    java.util.Objects.requireNonNull((qualifiedName));
    return new NamedSchema(qualifiedName, type, annotations);
  }
  
  public NamedSchema withType(hydra.ext.pegasus.pdl.NamedSchema_Type type) {
    java.util.Objects.requireNonNull((type));
    return new NamedSchema(qualifiedName, type, annotations);
  }
  
  public NamedSchema withAnnotations(hydra.ext.pegasus.pdl.Annotations annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new NamedSchema(qualifiedName, type, annotations);
  }
}
